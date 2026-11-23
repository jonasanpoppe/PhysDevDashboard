# Roadmap

A learning project: a personal health data pipeline and Shiny dashboard, built
with the best tool for each layer rather than the most familiar one.

Earlier Claude dashboards (a weight and strength tracker; strength and cycling
projections) are the design reference for which views matter. The Shiny app
replaces them.

## Principles

1. **Vertical slices.** Every milestone after the first ends with something new
   visible in the app. No layer is built ahead of a view that needs it.
2. **Claude drafts, you review, Claude double-checks.** Claude Code writes the
   first version and explains the non-obvious choices. You review it and change
   what you'd do differently. Claude then checks the result (tests, conventions,
   your edits), ideally in a fresh session that didn't write it. The learning
   note in each PR, in your own words, is what makes the review stick.
3. **Raw files are the truth; the database is disposable.** Every source lands
   as dated files that are never edited. The database is rebuilt from them on
   every build.
4. **One writer, one DuckDB version.** Python never writes the database; it only
   writes files. R runs the SQL build and serves the app.
5. **Public repo, private data.** No personal data, values or targets in code,
   tests, docs or commit messages. Personal goals live in `config/goals.yml`
   (gitignored).
6. **Keep the old tracker running** until the app covers each of its views.

## Architecture

```
APIs ──Python──▶ data/landing/<source>/<date>/   (dated files, never edited)
exports ────────▶ data/landing/<source>/<date>/
                          │
                          ▼  R runs sql/*.sql through DuckDB
                 raw ──▶ staging ──▶ marts        (db/health.duckdb, rebuilt every time)
                                       │
                                       ▼
                           R: projections + Shiny app
```

| Concern | Tool | Why |
|---|---|---|
| Fetching from APIs | Python (`httpx`, `stravalib`, `garminconnect`) | Mature API clients |
| Python env | `uv` | Lockfile-based, fast |
| R env | `renv` | Already in place |
| Loading files | DuckDB SQL (`read_xlsx`, `read_csv`, `read_json`, `read_parquet`) | Reads every landing format natively |
| Transformations | Plain SQL files, run in order from R | Language-neutral, testable |
| Analysis | R; `targets` for projections only | Existing strength |
| Dashboard | Shiny + `bslib`, one module per view | Scenario inputs, not just charts |
| Task runner | `just` | Owns the data flow: `fetch`, `build`, `test`, `app` |
| Secrets | `.env` with `chmod 600`, outside git | `keyring` usually has no backend in WSL |
| Backups | `restic` | Encrypted, deduplicated, restorable |
| CI | GitHub Actions on synthetic data | Real data never leaves the machine |
| Remote access (optional) | Docker + Tailscale | Phone access without public exposure |

### Environment

- Everything lives inside WSL2's Linux filesystem (`~/projects/...`), never
  under `/mnt/c`: repo, data, R (RStudio Server in WSL, or Positron/VS Code via
  Remote-WSL), Python and Claude Code. File locks across the Windows/WSL
  boundary are unreliable and I/O is slow.
- **DuckDB versions.** R is the only process that creates or writes the
  database, so R's duckdb version decides the file format. Every other reader
  (DuckDB CLI, Python) must be the same version or newer. Files created by
  DuckDB 2.0 cannot be read by 1.x. `just doctor` checks this.
- **Concurrency.** DuckDB allows one read-write process or several read-only
  ones, never both. `just build` writes `db/health.new.duckdb`, closes it
  cleanly (`dbDisconnect(con, shutdown = TRUE)`, no `.wal` left), then `mv`
  replaces `db/health.duckdb`, which is atomic on one filesystem. The app opens a
  read-only connection per query, so it never blocks a build.

## Data conventions

**Systems of record.** One per kind of data; everything else is deduplicated
against it (match on start time within a tolerance).

| Data | System of record | Notes |
|---|---|---|
| Intake, body weight, lifting sets | MacroFactor export | Strength sessions logged elsewhere are ignored |
| Rides | intervals.icu if it syncs directly from Garmin, otherwise Garmin | Never sum rides across sources |
| Segment efforts (benchmark climbs) | Strava | Check whether intervals.icu covers these too |
| Sleep, HRV, resting HR | Garmin, preferably via intervals.icu's sync over the unofficial library | |
| History before the Garmin era | Fitbit Takeout | One-off backfill |

**Measurements vs. model outputs.** Scale weight and intake are measurements.
MacroFactor's trend weight and expenditure are model outputs that get
recalculated after past edits and algorithm changes: store them with their
export date and never treat them as measurements. Diff two exports to see how
much past values move.

**What a day is.** One convention for every source: the local calendar date in
the home timezone (set in config). Sleep and overnight HRV belong to the date
you wake up. Store UTC timestamps next to the local date. Document each source's
native convention in `docs/sources.md` and convert in staging.

**No splicing.** Series from different devices or measurement windows stay
separate (Fitbit vs. Garmin; morning chest-strap HRV vs. overnight watch HRV).

**Estimated power** (no power meter) is directional only; heart rate is the
primary intensity metric.

## Milestones

Each milestone: one branch, one PR, and a short note on what you learned.

### 1. Foundations

*Learn: Linux filesystem and permissions, `uv`, `just`, `restic`.*

- Back up `data/` with `restic` before touching anything, and test a restore.
- Move the repo and data into WSL's filesystem; run Claude Code from there.
- Remove SEC-dashboard leftovers (`app.R`, `init_db.R`); fix or remove the
  template `DESCRIPTION` and `NAMESPACE`.
- `.gitignore`: add `*.html`, `*_files/`, `.env`, `config/goals.yml`.
- `config/goals.example.yml` with invented values (committed);
  `config/goals.yml` with real ones (ignored).
- `pyproject.toml` with `uv`; move `python/fetch_garmin_api.py` into the `uv` project and
  point it at `data/landing/garmin/`.
- `justfile` with `doctor` (tool versions, including DuckDB), `fetch`, `build`,
  `build-fixture`, `test`, `app`.
- Synthetic fixture: a script generates a fake MacroFactor export with the real
  sheet and column structure and invented values, in `fixtures/`.
- `CLAUDE.md` with the principles and data conventions above, so every Claude
  Code session follows them.

**Done when:** a fresh clone runs `just doctor && just build-fixture && just test`
green without any real data.

### 2. Slice: weight vs. plan

*Learn: SQL layering, loading files with DuckDB, GitHub Actions.*

- Drop each MacroFactor export, unchanged, into
  `data/landing/macrofactor/<export-date>/`.
- Load sheets with DuckDB's `read_xlsx`; fall back to Python only if the layout
  fights it.
- `sql/staging/` and `sql/marts/mart_weight.sql`: scale weight and intake as
  measurements; trend weight and expenditure as model outputs with export date.
- SQL data checks: expected columns present (catches export format changes), no
  duplicate dates, plausible ranges.
- Shiny module: weight against the planned rate and checkpoints from
  `config/goals.yml`.
- GitHub Actions runs `just test` on the fixture for every PR.

**Done when:** dropping a new export and running `just build` updates the view,
and CI is green.

### 3. Slice: strength (gate)

*Learn: SQL window functions, golden testing.*

- Copy the reference implementation (`mf_refresh.py`) into `reference/`. Freeze
  one real export in `data/golden/` (local only) and run the reference on it to
  produce the expected output.
- `seeds/exercises.csv`: map MacroFactor exercise names to canonical lifts, so a
  rename doesn't break a series.
- Set-level e1RM in SQL: Wathen (1994), 100 × load / (48.8 + 53.8 ×
  e^(−0.075 × (reps + RIR))), with load = bar + machine base weight, or
  bodyweight − assistance for assisted lifts. Standard and Failure sets only,
  RIR ≤ 3. Session bests with window functions. (The R version is
  `R/e1rm.R`.)
- Golden test (local only, skipped in CI) against the reference output;
  synthetic tests run everywhere.
- Shiny module: e1RM per canonical lift against targets from config.

**Done when:** the golden test matches to within rounding. If it doesn't, stop
and resolve the difference before anything else.

### 4. Slice: rides (first API)

*Learn: REST APIs, API-key auth, incremental pulls, timezones.*

- Confirm the system of record for rides (does intervals.icu sync directly from
  Garmin?).
- Python client with `httpx`, API key from `.env`. Each pull writes dated JSON
  to `data/landing/intervals/`, fetching only what is newer than the last file.
- Staging converts timestamps to UTC plus local date.
- Test: a ride present in two sources counts once.
- Shiny module: distance year-to-date against the annual target from config;
  best rides.

**Done when:** a new ride appears after `just fetch && just build`.

### 5. Slice: climbs (OAuth2)

*Learn: OAuth2 authorization-code flow, refresh tokens, rate limits.*

- Register a Strava API app; run the authorization-code flow once, then refresh
  tokens automatically.
- Store tokens in a file with `chmod 600`, outside the repo.
- Pull segment efforts for benchmark climbs only; activities stay with their
  system of record.
- Shiny module: climb times over time.

**Done when:** an expired token refreshes without manual steps.

If intervals.icu already covers segment efforts, this milestone is OAuth
practice rather than new data. Still worth doing; just know which it is.

### 6. Slice: recovery and history

*Learn: messy bulk exports, one-off backfills, Parquet.*

- Convert the Fitbit Takeout once to Parquet in `data/landing/fitbit/` with the
  existing R loaders; never rebuilt after that.
- Garmin wellness via intervals.icu if available, otherwise the existing script.
- Apply the day convention to sleep and HRV.
- Shiny module: recovery trends, with device eras as separate series.

**Done when:** the old loaders in `_targets.R` are gone.

### 7. Scenarios

*Learn: modelling inside an app, `targets` for analysis, `shinytest2`.*

- Projection models in R, orchestrated by `targets` (analysis only).
- Scenario inputs: rate of change, target date, adherence.
- `shinytest2` for at least one module.

**Done when:** the app answers "am I on track?" under adjustable assumptions,
and `mf_refresh.py` and the old Claude dashboards are no longer needed.

### 8. Remote access (optional)

*Learn: containers for deployment, private networking.*

- Dockerfile for the app; run it on an always-on machine at home.
- Tailscale for access from your phone; nothing exposed to the public internet.

**Done when:** the app opens on your phone over Tailscale only.

## Checklist for every PR

- No personal values in code, tests, docs or commit messages; fixtures are
  synthetic.
- No rendered outputs committed.
- Every new source has a system-of-record decision, a day convention, a fixture
  and a data check.
