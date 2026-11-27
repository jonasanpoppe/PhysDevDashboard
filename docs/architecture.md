# Architecture

How the project is built **today**: the tools, how data flows from the APIs
to the app, and the conventions each layer follows. ROADMAP.md holds the
plan and the milestones; `docs/sources.md` holds the per-source details
(system of record, landing paths, day convention).

## Data flow

```
 Strava API ─┐                                      config/*.yml, *.csv
 Garmin API ─┼─ python/ (uv) ─▶ data/landing/<source>/      │
 exports ────┘   writes files     raw files, never edited    │
 (MacroFactor,                           │                   │
  Fitbit Takeout)                        ▼                   ▼
                              R loaders + marts, run by targets (_targets.R)
                                         │  the only writer
                                         ▼
                                   db/health.db (DuckDB)
                                         │  read-only, one connection per query
                                         ▼
                                  app/ (Shiny + bslib)
                                         │
                       localhost:3838 ───┴─── Tailscale (tailnet only)
```

Three rules hold the flow together:

1. **Raw files are the truth.** Python and manual exports only add files to
   `data/landing/`. Nothing edits them; the database can always be rebuilt.
2. **One writer.** Only the R pipeline writes `db/health.db`. Python never
   touches it; the app only reads it.
3. **Days are local dates; sleep belongs to the wake-up day.** Conversion
   happens in the loaders (see `docs/sources.md`).

## Tools

| Layer | Tool | Role here |
|---|---|---|
| Fetching | Python 3.11 via `uv` (`httpx`, `garminconnect`) | `python/fetch_*.py` write JSON to `data/landing/`. Dependencies in `pyproject.toml`, locked in `uv.lock` |
| Pipeline | R 4.6 + `targets` | `_targets.R` lists every step; only changed inputs are rebuilt |
| R packages | `renv` | Versions locked in `renv.lock`; `renv::snapshot()` after adding a package |
| Database | DuckDB (R package `duckdb`) | One file, `db/health.db`. R's duckdb version decides the file format; every other reader must be the same version or newer |
| App | Shiny + `bslib` | `app/`, one module per view |
| Charts | `ggplot2` with `R/theme_physdev.R` | Static for now |
| Tests | `testthat` | `tests/testthat/`, synthetic data only |
| Task runner | `just` | The entry point for everything; see below |
| Backups | `restic` to an S3-compatible bucket | Encrypted snapshots of `data/`, `config/`, `private/`, `.env` |
| Remote access | Tailscale (`tailscale serve`) | The app on the tailnet only; never `funnel` |

## Commands

`just --list` prints every recipe with what it does; the justfile is the one
place they are defined. The everyday ones: `doctor` (tools, secrets, personal
config and that the plan's config agrees with itself), `fetch`, `build`,
`test`, `app`, `backup`.

## Environment

Everything lives inside WSL2's Linux filesystem, never under `/mnt/c`: file
locks across the Windows/WSL boundary are unreliable and I/O is slow.

## Repository layout

```
python/            fetchers (write files only)
data/landing/      raw files per source (gitignored): garmin/ strava/ macrofactor/ fitbit/ garmin_export/
R/                 loaders, marts and plot functions; sourced by _targets.R
sql/               staging/ and marts/: loading in plain SQL, run by R/run_sql.R
_targets.R         the pipeline
db/                the database (gitignored, disposable)
app/               app.R, db.R, modules/<view>.R
config/            personal config (gitignored); *.example.* twins committed (see "Config")
tests/testthat/    tests
docs/              this file, sources.md
private/           private specs and plans (gitignored)
```

## The pipeline

`_targets.R` has five kinds of target, in this order:

1. **File targets** (`format = "file"`): the landing files and folders. A
   changed file makes its downstream targets rerun; folders that receive new
   files are re-listed on every run.
2. **Loaders** (`R/load_*.R`): read one source and write one table per kind
   of data, prefixed by source (`fitbit_*`, `garmin_*`, `strava_*`,
   `macrofactor_*`). Each returns the data frame as well, so marts can depend
   on it.
3. **Marts** (`R/build_*.R`): combine loader outputs into what a view needs,
   with data checks that stop the build on structural problems.
   `vitals_daily` is the first.
4. **Analysis**: climb matching, effort metrics, the fitness and strength
   models, the Quest Board, relics, roadmap and scenarios.
5. **The consistency check** (`view_checks`, last): numbers that two tabs
   show must agree (`R/consistency.R`); the build stops when they don't.

Loading through R functions is interim: loading is moving to plain SQL files
run by DuckDB (`sql/staging`, `sql/marts`), and `targets` keeps only the
analysis. `R/run_sql.R` runs one file; `{{name}}` placeholders take landing
paths as quoted literals, and a missing or unused name stops the run. The
`.sql` file and its landing files are file targets, so either change reruns
the step. First moved: the Fitbit calories
(`sql/staging/fitbit_calories_daily.sql`, ~80× faster than the R loader).

## The database

- One DuckDB file, rebuilt from `data/landing/` by `just build`. Deleting it
  loses nothing.
- DuckDB allows **one read-write process or several read-only ones**, never
  both. The pipeline is the writer; the app opens a read-only connection per
  query and closes it at once, so a running app never blocks a build.
- Tables are written in place. A table whose loader is removed stays in the
  file until dropped or until the file is rebuilt from scratch (planned: build
  into a new file, then swap it in atomically).

## The app

```
app/app.R             page_navbar; sources R/ helpers and modules (local = TRUE)
app/db.R              query(sql): read-only connection per query; PDD_DB overrides the path
app/modules/<view>.R  <view>_ui(id) / <view>_server(id)
R/plot_<view>.R       the chart as a plain function, testable without Shiny
```

- A module reads its mart once per session; a rebuild shows up on reload.
- Shared helpers (formatters, the session count) are in `R/common.R`; the
  colours in `R/theme_physdev.R`, which also writes the CSS colour tokens
  that `app/www/oath.css` uses.
- Files are sourced with `local = TRUE`: `runApp()` evaluates `app.R` in its
  own environment, and helpers sourced into the global one can't see it.
- Charts follow the data conventions: one y-axis per panel, device eras as
  separate series with fixed colours, derived values and model outputs
  labelled as such.

## Testing

- Everything in `tests/testthat/` runs on synthetic data shaped like the real
  files and tables, never on personal data.
- Loaders are tested on small files with the real headers; marts on small
  data frames; plots through `ggplot_build()`; the app through
  `shiny::testServer(shinyAppDir("app"))` against a temporary database.
- Golden tests on real data (`test-golden-*.R`, expected values in
  `private/`) run locally only and skip elsewhere, as does the check that the
  real database's tabs agree.
- `shinytest2` drives the app in a headless Chrome (`test-app-snapshots.R`):
  snapshots of the rendered pages in both looks (`tests/testthat/_snaps/`)
  and what only a browser runs. A changed page fails with a diff; accept it
  with `testthat::snapshot_accept()`. Skips without Chrome.

## Config

Personal settings live in `config/` (gitignored), each with an invented
`*.example.*` twin that documents every option. One home per fact:

- `goals.yml`: **what you aim for**: the weight goal (target, pause, rate,
  start), sessions a week, lift targets (the heavy single on the bar, per
  group's main lift in `lifts.csv`) and single ratios, km a year, the goal
  ride, the forecast's settings.
- `oath.yml`: **how it is shown and judged**: the Home copy, quests,
  relics, roadmap, deadlines, `adherence_met` (80% counts as met) and the
  scenario assumptions. Its numbers are filled from goals.yml
  (`R/plan_config.R`); text names them as `{weight_goal.target_kg}`.
- `athlete.yml`: zones, the climb matching and power model settings.

`just doctor` and the start of `just build` stop when a number is written in
both files and differs, a reference leads nowhere, or a plan number is typed
into text instead of named.

## Secrets and private data

- `.env` (chmod 600) holds API and backup credentials; tokens live in the
  home directory (`~/.strava`, `~/.garminconnect`).
- `config/` and `private/` are gitignored; each config file has a committed
  `*.example.*` twin with invented values.
- No personal values in code, tests, docs or commit messages.

## Adding things

**A source:** a fetcher or export drop into `data/landing/<source>/`; a
loader in `R/load_<source>_*.R` writing `<source>_*` tables; a file target and
a loader target in `_targets.R`; a section in `docs/sources.md` (system of
record, day convention); a test on a synthetic file.

**A view:** a mart in `R/build_<view>.R` with data checks; a plot function in
`R/plot_<view>.R`; a module in `app/modules/<view>.R` and a `nav_panel` in
`app/app.R`; tests for the mart, the plot and the app.
