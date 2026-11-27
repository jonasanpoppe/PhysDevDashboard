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
   tests, docs or commit messages. Personal config (`config/*.yml`,
   `config/*.csv`: goals, zones, bikes, climbs, overrides) is gitignored; each
   file has a committed `*.example.*` twin with invented values.
6. **Keep the old tracker running** until the app covers each of its views.

## Architecture

How it is built today (data flow, tools, layout, the database, the app,
testing, config) is in `docs/architecture.md`, the one description. The
direction: loading moves from R loaders to plain SQL run by DuckDB
(`sql/staging` → `sql/marts`), `targets` keeps the analysis (projections,
climb matching, models), and the app stays read-only. The "Interim loaders"
convention below says when.

## Data conventions

**Systems of record.** One per kind of data; everything else is deduplicated
against it (match on start time within a tolerance).

| Data | System of record | Notes |
|---|---|---|
| Intake, body weight, lifting sets | MacroFactor export | From its first day on, strength sessions logged elsewhere are ignored |
| Lifting sets before MacroFactor | Strong export, training templates | One-off backfills in MacroFactor's layout; they end where MacroFactor starts |
| Body weight before MacroFactor | Fitbit Takeout | Weigh-ins, used only before MacroFactor's first one, in a labelled series |
| Rides and runs (activities, streams) | Strava | Complete history; never sum rides across sources. intervals.icu is a settings source (HR zones), not a data source |
| Climb efforts | Own matching on Strava GPS streams | Strava segments seed the climb list once; reduced polylines are not enough |
| Sleep, HRV, resting HR | Garmin, via the own fetcher | |
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

**No silent splicing.** Series from different devices or measurement windows
stay separate (Fitbit vs. Garmin; wrist vs. chest-strap HR; morning
chest-strap HRV vs. overnight watch HRV). Each record carries its device era.
A spliced series is allowed only as a derived, clearly labelled series kept
beside the originals, and models filter on era, not on year.

**Heart-rate zones.** Two named sets, never mixed: `hr_zones` (descriptive,
from intervals.icu) and `plan_bands` (training-plan compliance). Both are
computed from the same per-activity HR histogram.

**Estimated power** (no power meter) is a physics model for comparing efforts
with each other: directional, with its uncertainty shown in the app. Measured
power and Strava's own estimate stay separate series; heart rate remains the
primary intensity metric.

**Table names.** Source tables carry their source as prefix (`strava_*`,
`garmin_*`), including analysis tables derived from a single source
(`strava_climb_efforts`, `strava_daily_load`).

**Interim loaders.** Until the climb module works end to end, sources are
loaded by R functions in `_targets.R`. After that, loading moves to the SQL
layers in one go; the analysis stays in `targets`.

## Milestones

Each milestone: one branch, one PR, and a short note on what you learned.

**Order.** Minimal M1 first, then the Vitals view from M6 (Garmin data is
complete), then climbs (M5). The Strava fetcher (M4/M5 data and OAuth) already
exists; those milestones now centre on modelling and views.

### 1. Foundations

*Learn: Linux filesystem and permissions, `uv`, `just`, `restic`.*

- Back up `data/` with `restic` before touching anything, and test a restore.
- Move the repo and data into WSL's filesystem; run Claude Code from there.
- Remove SEC-dashboard leftovers (`app.R`, `init_db.R`); fix or remove the
  template `DESCRIPTION` and `NAMESPACE`.
- `.gitignore`: add `*.html`, `*_files/`, `.env`, `config/goals.yml`.
- `config/goals.example.yml` with invented values (committed);
  `config/goals.yml` with real ones (ignored).
- `pyproject.toml` with `uv` for both fetchers (Garmin, Strava); move the
  existing data folders to `data/landing/<source>/` and point fetchers,
  `_targets.R` and the backup check at them.
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
- Lifting history from before MacroFactor (one-off backfills in its layout),
  stacked with one e1RM rule; corrections as overrides, never in raw files.
- Forecast per main lift: a plateau curve with ceiling = aptitude × an elite
  line (fat-free mass per cm), fitted on months spent training the lift;
  dates to targets with a p10–p90 band, and loads actually lifted ("on the
  bar") against thresholds. All in plain R functions with golden tests.

**Done when:** the golden test matches to within rounding. If it doesn't, stop
and resolve the difference before anything else.

### 4. Slice: rides

*Learn: REST APIs, incremental pulls, timezones.*

- Strava is the system of record. The fetcher writes one JSON file per
  activity to `data/landing/strava/`, fetching only what is new.
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
- Climb library in `config/climbs.csv`, seeded once from Strava segments and
  reviewed by hand.
- Own climb matching on full GPS streams; effort metrics and a status per
  effort (clean or with stops), with manual overrides.
- Power model per effort (estimate, see data conventions).
- Later: fill each climb's surface (cobbles, part cobbles, asphalt) by
  matching the climb library against an external climb database such as
  Climbfinder, instead of by hand. Check its terms of use before any
  automated access; until then the surface stays empty and the power model
  says which rolling resistance it assumed.
- Golden tests on known efforts (local only, skipped in CI).
- Shiny module: climb record.

**Done when:** an expired token refreshes without manual steps, and the
golden tests pass.

### 6. Slice: recovery and history

*Learn: messy bulk exports, one-off backfills, Parquet.*

- Convert the Fitbit Takeout once to Parquet in `data/landing/fitbit/` with the
  existing R loaders; never rebuilt after that.
- Garmin wellness via the existing fetcher.
- Apply the day convention to sleep and HRV.
- Shiny module (Vitals, built early, right after M1): resting HR, HRV against
  its baseline band, sleep, breathing rate, SpO2, skin temperature, with device
  eras as separate series and the device switch visible.

**Done when:** the old loaders in `_targets.R` are gone.

### 7. Scenarios

*Learn: modelling inside an app, `targets` for analysis, `shinytest2`.*

- Projection models in R, orchestrated by `targets` (analysis only).
- Scenario inputs: rate of change, target date, adherence.
- `shinytest2` for at least one module.
- Quests: named goals with stages and checkpoints, defined in config (a new
  quest is a config entry, not code). An overview of all quests plus one tab
  each, with each condition checked against what was actually done. Manual
  done flags first; automatic checks once the data is in the database.

**Done when:** the app answers "am I on track?" under adjustable assumptions,
and `mf_refresh.py` and the old Claude dashboards are no longer needed.

### 8. Remote access (optional)

*Learn: containers for deployment, private networking.*

- Dockerfile for the app; run it on an always-on machine at home.
- Tailscale for access from your phone; nothing exposed to the public internet.

**Done when:** the app opens on your phone over Tailscale only.

### 9. Finishing touches

*Learn: design systems, configurable apps.*

- Styling: one visual identity across tabs, figures and tables.
- Integration: modules share selections and dates where it helps.
- Customisability: what is shown, and how, set from config rather than code.

**Done when:** the app looks and behaves as one product, and changing what it
shows needs no code change.

## Checklist for every PR

- No personal values in code, tests, docs or commit messages; fixtures are
  synthetic.
- No rendered outputs committed.
- Every new source has a system-of-record decision, a day convention, a fixture
  and a data check.
