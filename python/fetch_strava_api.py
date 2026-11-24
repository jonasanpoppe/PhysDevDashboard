"""
Fetch everything the Strava API offers about the authenticated athlete.

Saves API responses, unchanged, under data/landing/strava/ for ingestion by the R
targets pipeline:

  *_strava_activities.json  activity summaries, one new file per run
  details/{id}.json         detailed activity: calories, description, laps,
                            splits, best efforts and all segment efforts
  streams/{id}.json         per-second time series (HR, power, GPS, …)
  segments/{id}.json        each segment ridden or run, with your PR stats
  gear/{id}.json            bikes and shoes (latest snapshot)
  athlete/*.json            profile and totals (latest snapshot)

Time in HR zones isn't fetched: it is computed from the heartrate streams with
our own zone boundaries, consistent across Strava and Garmin.

Crash safety: every per-activity and per-segment response goes to its own file
(written atomically), and a run only requests what has no file yet, so after a
crash, reboot or Ctrl-C just run it again. Responses that have no data (404,
private segment, manual activity without streams) are saved as a small
{"_status": …} marker so they aren't requested again. A lock file prevents two
runs at once. Rate limits (per 15 min and per day) are read from the response
headers; the script waits until the window resets, up to midnight UTC for the
daily limit, so a first full load can take a few days unattended.

Setup (once):
  1. Create an API app at https://www.strava.com/settings/api with
     "Authorization Callback Domain" set to: localhost
  2. Put its Client ID and Client Secret in .env as STRAVA_CLIENT_ID and
     STRAVA_CLIENT_SECRET.
  3. uv run python/fetch_strava_api.py auth

Tokens are stored in ~/.strava/tokens.json (chmod 600) and refreshed
automatically; access tokens last six hours.

Usage:
  uv run python/fetch_strava_api.py auth          # one-time authorization
  uv run python/fetch_strava_api.py               # new activities + everything missing
  uv run python/fetch_strava_api.py --full        # re-list every activity (picks up edits)
  uv run python/fetch_strava_api.py --list-only   # only the activity summaries
"""

import argparse
import fcntl
import json
import os
import sys
import time
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.parse import parse_qs, urlencode, urlparse

import httpx
from dotenv import load_dotenv

REPO_DIR = Path(__file__).resolve().parent.parent
OUTPUT_DIR = REPO_DIR / "data" / "landing" / "strava"
DETAILS_DIR = OUTPUT_DIR / "details"
STREAMS_DIR = OUTPUT_DIR / "streams"
SEGMENTS_DIR = OUTPUT_DIR / "segments"
GEAR_DIR = OUTPUT_DIR / "gear"
ATHLETE_DIR = OUTPUT_DIR / "athlete"
LOCK_FILE = OUTPUT_DIR / ".fetch.lock"
TOKEN_FILE = Path.home() / ".strava" / "tokens.json"

AUTH_URL = "https://www.strava.com/oauth/authorize"
TOKEN_URL = "https://www.strava.com/oauth/token"
API_URL = "https://www.strava.com/api/v3"
REDIRECT_URI = "http://localhost/exchange_token"
# activity:read_all includes activities set to "Only you"
SCOPE = "read,activity:read_all"
PER_PAGE = 200  # Strava's maximum
STREAM_KEYS = ("time,distance,latlng,altitude,velocity_smooth,heartrate,"
               "cadence,watts,temp,moving,grade_smooth")
# Seconds to wait after network errors or 5xx responses before giving up
ERROR_WAITS = (10, 30, 60, 120, 300, 600, 900)


def log(msg: str) -> None:
    print(f"[{datetime.now():%Y-%m-%d %H:%M:%S}] {msg}", flush=True)


def write_json(path: Path, data, indent: int | None = 2) -> None:
    """Write atomically: a crash leaves either the old file or the new one."""
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(json.dumps(data, indent=indent))
    os.replace(tmp, path)


# ── Credentials and tokens ───────────────────────────────────────────────────

def client_credentials() -> tuple[str, str]:
    load_dotenv(REPO_DIR / ".env")
    client_id = os.getenv("STRAVA_CLIENT_ID")
    client_secret = os.getenv("STRAVA_CLIENT_SECRET")
    if not client_id or not client_secret:
        sys.exit("Set STRAVA_CLIENT_ID and STRAVA_CLIENT_SECRET in .env first.")
    return client_id, client_secret


def save_tokens(tokens: dict) -> None:
    """Keep only what's needed; the token response also includes the athlete."""
    keep = {k: tokens[k] for k in ("access_token", "refresh_token", "expires_at")}
    if tokens.get("scope"):
        keep["scope"] = tokens["scope"]
    TOKEN_FILE.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    # Create with 600 before writing, so the tokens are never readable by others
    TOKEN_FILE.touch(mode=0o600, exist_ok=True)
    TOKEN_FILE.chmod(0o600)
    TOKEN_FILE.write_text(json.dumps(keep, indent=2))


def authorize() -> None:
    """One-time authorization-code flow: browser consent, then code exchange."""
    client_id, client_secret = client_credentials()
    url = AUTH_URL + "?" + urlencode({
        "client_id": client_id,
        "response_type": "code",
        "redirect_uri": REDIRECT_URI,
        "approval_prompt": "force",
        "scope": SCOPE,
    })
    print("1. Open this URL in your browser and click Authorize:\n")
    print(f"   {url}\n")
    print("2. The browser then goes to a localhost page that fails to load.")
    print("   That's expected. Copy the full URL from the address bar.\n")
    redirected = input("Paste it here: ").strip()

    params = parse_qs(urlparse(redirected).query)
    if "error" in params:
        sys.exit(f"Authorization refused: {params['error'][0]}")
    if "code" not in params:
        sys.exit("No ?code= in that URL.")
    granted = params.get("scope", [""])[0]
    if "activity:read_all" not in granted:
        sys.exit("Activity access wasn't granted; tick the activity box and retry.")

    resp = httpx.post(TOKEN_URL, data={
        "client_id": client_id,
        "client_secret": client_secret,
        "code": params["code"][0],
        "grant_type": "authorization_code",
    })
    resp.raise_for_status()
    tokens = resp.json()
    tokens["scope"] = granted
    save_tokens(tokens)
    print(f"✓ Authorized; tokens saved to {TOKEN_FILE}")


def access_token(force_refresh: bool = False) -> tuple[str, int]:
    """Return a valid access token and its expiry, refreshing when needed."""
    if not TOKEN_FILE.exists():
        sys.exit("No tokens yet: run `uv run python/fetch_strava_api.py auth`.")
    tokens = json.loads(TOKEN_FILE.read_text())
    if not force_refresh and tokens["expires_at"] > time.time() + 300:
        return tokens["access_token"], tokens["expires_at"]

    client_id, client_secret = client_credentials()
    resp = httpx.post(TOKEN_URL, timeout=60, data={
        "client_id": client_id,
        "client_secret": client_secret,
        "grant_type": "refresh_token",
        "refresh_token": tokens["refresh_token"],
    })
    resp.raise_for_status()
    # Strava may rotate the refresh token, so always store the new one
    refreshed = resp.json() | {"scope": tokens.get("scope")}
    save_tokens(refreshed)
    log("✓ Access token refreshed")
    return refreshed["access_token"], refreshed["expires_at"]


# ── API client with rate limiting and retries ────────────────────────────────

def sleep_until(when: datetime, why: str) -> None:
    secs = (when - datetime.now(timezone.utc)).total_seconds()
    if secs > 0:
        log(f"⏸ {why}; resuming at {when.astimezone():%Y-%m-%d %H:%M} local "
            f"({secs / 60:.0f} min)")
        time.sleep(secs)


def next_quarter_hour() -> datetime:
    """Short-term limits reset at :00, :15, :30 and :45 past the hour."""
    now = datetime.now(timezone.utc)
    reset = now.replace(minute=now.minute // 15 * 15, second=0, microsecond=0)
    return reset + timedelta(minutes=15, seconds=5)


def next_midnight_utc() -> datetime:
    """Daily limits reset at midnight UTC."""
    now = datetime.now(timezone.utc)
    return (now + timedelta(days=1)).replace(hour=0, minute=1, second=0, microsecond=0)


class Strava:
    def __init__(self):
        self.client = httpx.Client(timeout=60)
        self.token, self.expires_at = access_token()
        self.calls = 0

    def close(self):
        self.client.close()

    def _limit_state(self, resp: httpx.Response) -> str | None:
        """'day' or '15min' if a limit is reached, from the usage headers.

        Strava sends overall limits (X-RateLimit-*) and, for GET requests, the
        stricter read limits (X-ReadRateLimit-*): "15-min,daily" pairs.
        """
        state = None
        for prefix in ("X-RateLimit", "X-ReadRateLimit"):
            limit = resp.headers.get(f"{prefix}-Limit")
            usage = resp.headers.get(f"{prefix}-Usage")
            if not (limit and usage):
                continue
            (l15, lday), (u15, uday) = (map(int, h.split(",")) for h in (limit, usage))
            if uday >= lday:
                return "day"
            if u15 >= l15:
                state = "15min"
        return state

    def _wait_for_limit(self, state: str) -> None:
        if state == "day":
            sleep_until(next_midnight_utc(), "daily rate limit reached")
        else:
            sleep_until(next_quarter_hour(), "15-minute rate limit reached")

    def get(self, path: str, params: dict | None = None) -> tuple[int, object]:
        """GET a path; return (status, JSON or None).

        Waits out rate limits, retries network errors and 5xx, refreshes the
        token on 401. Returns 4xx other than those to the caller, which decides
        whether "no data" is final.
        """
        errors = 0
        refreshed = False
        while True:
            if self.expires_at < time.time() + 300:
                self.token, self.expires_at = access_token()
            try:
                resp = self.client.get(
                    f"{API_URL}{path}", params=params,
                    headers={"Authorization": f"Bearer {self.token}"})
            except httpx.TransportError as e:
                resp = None
                problem = f"{type(e).__name__}: {e}"
            else:
                self.calls += 1
                problem = f"HTTP {resp.status_code}" if resp.status_code >= 500 else None

            if problem:
                if errors >= len(ERROR_WAITS):
                    raise RuntimeError(f"GET {path} kept failing ({problem})")
                log(f"⚠ {problem} on {path}; retrying in {ERROR_WAITS[errors]}s")
                time.sleep(ERROR_WAITS[errors])
                errors += 1
                continue

            if resp.status_code == 429:
                self._wait_for_limit(self._limit_state(resp) or "15min")
                continue
            if resp.status_code == 401:
                if refreshed:
                    raise RuntimeError(
                        f"GET {path}: unauthorized even after a token refresh; "
                        "run `auth` again")
                self.token, self.expires_at = access_token(force_refresh=True)
                refreshed = True
                continue

            # Pause before the next call rather than wasting it on a 429
            state = self._limit_state(resp)
            if resp.status_code >= 400:
                if state:
                    self._wait_for_limit(state)
                return resp.status_code, None
            data = resp.json()
            if state:
                self._wait_for_limit(state)
            return resp.status_code, data


# ── Activity summaries ───────────────────────────────────────────────────────

def activity_files() -> list[Path]:
    return sorted(OUTPUT_DIR.glob("*_strava_activities.json"))


def known_activities() -> dict[int, dict]:
    """All activities from earlier list fetches; later files win."""
    acts = {}
    for f in activity_files():
        for a in json.loads(f.read_text()):
            acts[a["id"]] = a
    return acts


def fetch_activity_list(api: Strava, full: bool) -> None:
    acts = known_activities()
    after = None
    if not full and acts:
        after = int(max(
            datetime.fromisoformat(a["start_date"].replace("Z", "+00:00"))
            for a in acts.values()).timestamp())
    log("Listing all activities" if after is None else
        f"Listing activities after {datetime.fromtimestamp(after, timezone.utc):%Y-%m-%d %H:%M} UTC")

    activities = []
    page = 1
    while True:
        params = {"per_page": PER_PAGE, "page": page}
        if after is not None:
            params["after"] = after
        status, batch = api.get("/athlete/activities", params)
        if status != 200:
            raise RuntimeError(f"Listing activities failed: HTTP {status}")
        log(f"  page {page}: {len(batch)} activities")
        activities.extend(batch)
        if len(batch) < PER_PAGE:
            break
        page += 1

    if not activities:
        log("✓ No new activities")
        return
    # One file per run, named by fetch time: files are never edited afterwards
    stamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H%M%SZ")
    out_file = OUTPUT_DIR / f"{stamp}_strava_activities.json"
    write_json(out_file, activities)
    log(f"✓ Wrote {len(activities)} activities → {out_file.name}")


# ── Per-item backfill ────────────────────────────────────────────────────────

def backfill(api: Strava, name: str, out_dir: Path, ids: list, path_for,
             params: dict | None = None, indent: int | None = 2) -> None:
    """Fetch each id that has no file in out_dir yet."""
    out_dir.mkdir(parents=True, exist_ok=True)
    todo = [i for i in ids if not (out_dir / f"{i}.json").exists()]
    log(f"{name}: {len(ids) - len(todo)} of {len(ids)} done, {len(todo)} to fetch")
    for n, item_id in enumerate(todo, 1):
        status, data = api.get(path_for(item_id), params)
        if data is None:
            # Only "this has no data" is final; anything else is a bug to see
            if status not in (403, 404):
                raise RuntimeError(f"{name} {item_id}: HTTP {status}")
            data = {"id": item_id, "_status": status}
        write_json(out_dir / f"{item_id}.json", data, indent=indent)
        if n % 25 == 0 or n == len(todo):
            log(f"  {name}: {n}/{len(todo)}")


def segment_ids() -> list[int]:
    """Every segment with an effort in a fetched activity detail."""
    ids = set()
    for f in DETAILS_DIR.glob("*.json"):
        for effort in json.loads(f.read_text()).get("segment_efforts") or []:
            ids.add(effort["segment"]["id"])
    return sorted(ids)


def fetch_snapshots(api: Strava, acts: dict[int, dict]) -> None:
    """Athlete profile and totals, and gear: small, refreshed each run."""
    ATHLETE_DIR.mkdir(parents=True, exist_ok=True)
    status, athlete = api.get("/athlete")
    if status != 200:
        raise RuntimeError(f"Fetching the athlete failed: HTTP {status}")
    write_json(ATHLETE_DIR / "athlete.json", athlete)
    status, stats = api.get(f"/athletes/{athlete['id']}/stats")
    write_json(ATHLETE_DIR / "stats.json",
               stats if stats is not None else {"_status": status})

    GEAR_DIR.mkdir(parents=True, exist_ok=True)
    gear = sorted({a["gear_id"] for a in acts.values() if a.get("gear_id")})
    for gear_id in gear:
        status, data = api.get(f"/gear/{gear_id}")
        write_json(GEAR_DIR / f"{gear_id}.json",
                   data if data is not None else {"id": gear_id, "_status": status})
    log(f"✓ Athlete profile, stats and {len(gear)} gear items")


def fetch_all(full: bool, list_only: bool) -> None:
    api = Strava()
    try:
        fetch_activity_list(api, full)
        if list_only:
            return
        acts = known_activities()
        # Newest first, so recent data is complete soonest
        ids = sorted(acts, key=lambda i: acts[i]["start_date"], reverse=True)

        fetch_snapshots(api, acts)
        backfill(api, "details", DETAILS_DIR, ids,
                 lambda i: f"/activities/{i}", {"include_all_efforts": "true"})
        backfill(api, "streams", STREAMS_DIR, ids,
                 lambda i: f"/activities/{i}/streams",
                 {"keys": STREAM_KEYS, "key_by_type": "true"}, indent=None)
        backfill(api, "segments", SEGMENTS_DIR, segment_ids(),
                 lambda i: f"/segments/{i}")
        log(f"✓ Done ({api.calls} API calls this run)")
    finally:
        api.close()


def main():
    parser = argparse.ArgumentParser(description="Fetch Strava data")
    parser.add_argument("command", nargs="?", choices=["auth", "fetch"], default="fetch")
    parser.add_argument("--full", action="store_true",
                        help="Re-list every activity, not just newer ones.")
    parser.add_argument("--list-only", action="store_true",
                        help="Only fetch activity summaries.")
    args = parser.parse_args()

    if args.command == "auth":
        authorize()
        return

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    with open(LOCK_FILE, "w") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            sys.exit("Another fetch is already running.")
        # Leftovers from a crash mid-write
        for stale in OUTPUT_DIR.rglob("*.json.tmp"):
            stale.unlink()
        fetch_all(args.full, args.list_only)


if __name__ == "__main__":
    main()
