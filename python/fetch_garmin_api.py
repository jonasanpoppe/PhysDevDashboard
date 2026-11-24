"""
Fetch daily wellness data from the Garmin Connect API: RHR, HRV, intensity
minutes, steps and other daily totals, SpO2, breathing rate, sleep and VO2max.

Saves daily JSON files to data/landing/garmin/ for ingestion by the R targets pipeline.
Tokens are cached in ~/.garminconnect/garmin_tokens.json after the first login
(credentials from GARMIN_EMAIL / GARMIN_PASSWORD, or prompted; MFA is prompted).

Usage:
  uv run python/fetch_garmin_api.py                          # last 30 days
  uv run python/fetch_garmin_api.py --start 2024-07-20       # from date to yesterday
  uv run python/fetch_garmin_api.py --start 2024-07-20 --end 2025-08-21  # specific range
"""

import argparse
import json
import os
import time
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from getpass import getpass

from garminconnect import Garmin, GarminConnectTooManyRequestsError

TOKEN_DIR = Path.home() / ".garminconnect"
OUTPUT_DIR = Path(__file__).resolve().parent.parent / "data" / "landing" / "garmin"
RETRY_WAITS = (60, 180, 600)  # seconds to back off after a rate-limit error


def call(fn, *args):
    """Call a Garmin endpoint, backing off and retrying when rate limited.

    Re-raises after the last retry so the day's field is reported, not silently
    left empty.
    """
    for wait in (*RETRY_WAITS, None):
        try:
            return fn(*args)
        except GarminConnectTooManyRequestsError:
            if wait is None:
                raise
            print(f"\n  ⏸ rate limited, waiting {wait}s …", end=" ", flush=True)
            time.sleep(wait)


def authenticate() -> Garmin:
    """Authenticate with Garmin Connect, reusing saved tokens when possible."""
    tokenstore = str(TOKEN_DIR)
    try:
        garmin = Garmin()
        garmin.login(tokenstore)
        print("✓ Authenticated with saved tokens")
        return garmin
    except Exception as e:
        print(f"No usable saved session ({e}) — logging in with credentials.")

    email = os.getenv("GARMIN_EMAIL") or input("Garmin email: ")
    password = os.getenv("GARMIN_PASSWORD") or getpass("Garmin password: ")
    garmin = Garmin(
        email=email,
        password=password,
        prompt_mfa=lambda: input("Garmin MFA code: "),
    )
    garmin.login()

    # Tokens grant account access: keep them private to this user
    TOKEN_DIR.mkdir(mode=0o700, parents=True, exist_ok=True)
    garmin.client.dump(tokenstore)
    (TOKEN_DIR / "garmin_tokens.json").chmod(0o600)
    print(f"✓ Logged in and tokens saved to {TOKEN_DIR}")
    return garmin


def ms_to_utc(ms):
    """Garmin GMT epoch milliseconds -> ISO 8601 UTC string."""
    if ms is None:
        return None
    return datetime.fromtimestamp(ms / 1000, tz=timezone.utc).isoformat()


def ms_to_local(ms):
    """Garmin "local" epoch milliseconds -> ISO 8601 local wall time, no offset.

    Garmin encodes local wall-clock time as if it were UTC, so decoding it as
    UTC gives the local time.
    """
    if ms is None:
        return None
    return datetime.fromtimestamp(ms / 1000, tz=timezone.utc).replace(tzinfo=None).isoformat()


def fetch_day(garmin: Garmin, d: date) -> dict:
    """Fetch all daily wellness metrics for a single date. Returns a flat dict."""
    cdate = d.isoformat()
    record = {"date": cdate, "rhr": None, "hrv": None,
              "im_moderate": None, "im_vigorous": None}

    # ── RHR ──────────────────────────────────────────────────────────────────
    try:
        rhr_resp = call(garmin.get_rhr_day, cdate)
        # Response shape varies; dig into allMetrics or direct fields
        if isinstance(rhr_resp, dict):
            metrics = rhr_resp.get("allMetrics", {}).get("metricsMap", {})
            rhr_entry = metrics.get("WELLNESS_RESTING_HEART_RATE", [{}])
            if rhr_entry and isinstance(rhr_entry, list) and len(rhr_entry) > 0:
                record["rhr"] = rhr_entry[0].get("value")
            # Fallback: direct key
            if record["rhr"] is None:
                record["rhr"] = rhr_resp.get("restingHeartRate")
    except Exception as e:
        print(f"  ⚠ RHR {cdate}: {e}")

    # ── HRV ──────────────────────────────────────────────────────────────────
    try:
        hrv_resp = call(garmin.get_hrv_data, cdate)
        if isinstance(hrv_resp, dict):
            summary = hrv_resp.get("hrvSummary", {})
            if summary:
                baseline = summary.get("baseline") or {}
                record["hrv"] = summary.get("weeklyAvg")
                record["hrv_last_night"] = summary.get("lastNight")
                record["hrv_last_night_avg"] = summary.get("lastNightAvg")
                record["hrv_last_night_5_min_high"] = summary.get("lastNight5MinHigh")
                # Balanced range of the personal baseline (Garmin's green band)
                record["hrv_baseline_low"] = baseline.get("balancedLow")
                record["hrv_baseline_high"] = baseline.get("balancedUpper")
                record["hrv_status"] = summary.get("status")
    except Exception as e:
        print(f"  ⚠ HRV {cdate}: {e}")

    # ── Intensity minutes ────────────────────────────────────────────────────
    # Garmin's "zone minutes": moderate and vigorous minutes for the day.
    # Garmin counts vigorous double toward the weekly goal.
    try:
        im_resp = call(garmin.get_intensity_minutes_data, cdate)
        if isinstance(im_resp, dict):
            record["im_moderate"] = im_resp.get("moderateMinutes")
            record["im_vigorous"] = im_resp.get("vigorousMinutes")
            record["im_week_goal"] = im_resp.get("weekGoal")
    except Exception as e:
        print(f"  ⚠ Intensity minutes {cdate}: {e}")

    # ── Daily summary: steps, distance, floors, calories, SpO2, breathing ───
    try:
        s = call(garmin.get_user_summary, cdate)
        if isinstance(s, dict):
            record["steps"] = s.get("totalSteps")
            record["step_goal"] = s.get("dailyStepGoal")
            record["distance_m"] = s.get("totalDistanceMeters")
            record["floors_ascended"] = s.get("floorsAscended")
            record["kcal_total"] = s.get("totalKilocalories")
            record["kcal_active"] = s.get("activeKilocalories")
            record["kcal_bmr"] = s.get("bmrKilocalories")
            # Whole day (24 h), not just sleep
            record["spo2_avg"] = s.get("averageSpo2")
            record["spo2_lowest"] = s.get("lowestSpo2")
            record["resp_waking_avg"] = s.get("avgWakingRespirationValue")
            record["resp_lowest"] = s.get("lowestRespirationValue")
            record["resp_highest"] = s.get("highestRespirationValue")
    except Exception as e:
        print(f"  ⚠ Summary {cdate}: {e}")

    # ── Sleep ────────────────────────────────────────────────────────────────
    # Garmin assigns a night to the date you wake up (same as our convention).
    try:
        sl = call(garmin.get_sleep_data, cdate)
        dto = (sl or {}).get("dailySleepDTO") or {}
        if dto.get("sleepTimeSeconds") is not None:
            scores = dto.get("sleepScores") or {}
            record["sleep_start_utc"] = ms_to_utc(dto.get("sleepStartTimestampGMT"))
            record["sleep_end_utc"] = ms_to_utc(dto.get("sleepEndTimestampGMT"))
            record["sleep_start_local"] = ms_to_local(dto.get("sleepStartTimestampLocal"))
            record["sleep_end_local"] = ms_to_local(dto.get("sleepEndTimestampLocal"))
            record["sleep_s"] = dto.get("sleepTimeSeconds")
            record["sleep_deep_s"] = dto.get("deepSleepSeconds")
            record["sleep_light_s"] = dto.get("lightSleepSeconds")
            record["sleep_rem_s"] = dto.get("remSleepSeconds")
            record["sleep_awake_s"] = dto.get("awakeSleepSeconds")
            record["sleep_unmeasurable_s"] = dto.get("unmeasurableSleepSeconds")
            record["nap_s"] = dto.get("napTimeSeconds")
            record["sleep_awake_count"] = dto.get("awakeCount")
            record["sleep_score"] = (scores.get("overall") or {}).get("value")
            record["sleep_avg_hr"] = dto.get("avgHeartRate")
            record["sleep_avg_stress"] = dto.get("avgSleepStress")
            record["sleep_spo2_avg"] = dto.get("averageSpO2Value")
            record["sleep_spo2_lowest"] = dto.get("lowestSpO2Value")
            record["sleep_resp_avg"] = dto.get("averageRespirationValue")
            record["sleep_resp_lowest"] = dto.get("lowestRespirationValue")
            record["sleep_resp_highest"] = dto.get("highestRespirationValue")
            record["sleep_skin_temp_dev_c"] = sl.get("avgSkinTempDeviationC")
            record["sleep_restless_moments"] = sl.get("restlessMomentsCount")
    except Exception as e:
        print(f"  ⚠ Sleep {cdate}: {e}")

    # ── VO2max ───────────────────────────────────────────────────────────────
    # Only present on days Garmin produced a new estimate (a qualifying run, or
    # a ride with a power meter), so most days are empty. Garmin model output.
    try:
        mm = call(garmin.get_max_metrics, cdate)
        entry = mm[0] if isinstance(mm, list) and mm else {}
        generic = entry.get("generic") or {}
        cycling = entry.get("cycling") or {}
        record["vo2max"] = generic.get("vo2MaxPreciseValue")
        record["vo2max_cycling"] = cycling.get("vo2MaxPreciseValue")
    except Exception as e:
        print(f"  ⚠ VO2max {cdate}: {e}")

    return record


def has_any_data(record: dict) -> bool:
    return any(v is not None for k, v in record.items()
               if k != "date" and not k.endswith("goal"))


def main():
    parser = argparse.ArgumentParser(description="Fetch Garmin RHR, HRV & intensity minutes")
    parser.add_argument("--start", type=str, default=None,
                        help="Start date (YYYY-MM-DD). Default: 30 days ago.")
    parser.add_argument("--end", type=str, default=None,
                        help="End date (YYYY-MM-DD). Default: yesterday.")
    parser.add_argument("--pause", type=float, default=1.0,
                        help="Seconds to wait between days. Default: 1.")
    args = parser.parse_args()

    end_date = datetime.strptime(args.end, "%Y-%m-%d").date() if args.end else date.today() - timedelta(days=1)
    start_date = datetime.strptime(args.start, "%Y-%m-%d").date() if args.start else end_date - timedelta(days=29)

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    garmin = authenticate()

    total_days = (end_date - start_date).days + 1
    print(f"Fetching {total_days} days: {start_date} → {end_date}")

    records = []
    for i in range(total_days):
        d = start_date + timedelta(days=i)
        print(f"  [{i+1}/{total_days}] {d}", end=" … ")
        record = fetch_day(garmin, d)
        records.append(record)
        has_data = has_any_data(record)
        print("✓" if has_data else "—", flush=True)
        time.sleep(args.pause)

    # Write single JSON file with all records
    out_file = OUTPUT_DIR / f"{start_date}_{end_date}_garmin_api.json"
    with open(out_file, "w") as f:
        json.dump(records, f, indent=2)

    filled = sum(1 for r in records if has_any_data(r))
    print(f"\n✓ Wrote {len(records)} records ({filled} with data) → {out_file}")


if __name__ == "__main__":
    main()
