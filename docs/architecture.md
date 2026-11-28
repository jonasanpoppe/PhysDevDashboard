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
                     SQL staging, R marts and analysis, run by targets (_targets.R)
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
   happens in staging (see `docs/sources.md`).

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
R/                 marts, analysis and plot functions; sourced by _targets.R
sql/               staging/, checks/ and marts/: plain SQL, run by R/run_sql.R
_targets.R         the pipeline
db/                the database (gitignored, disposable)
app/               app.R, db.R, modules/<view>.R
config/            personal config (gitignored); *.example.* twins committed (see "Config")
tests/testthat/    tests
docs/              this file, sources.md
demo/              the public demo: synthetic data generator, demo.db (see "The demo")
app.R              the demo's entry point (Posit Connect Cloud)
private/           private specs and plans (gitignored)
```

## The pipeline

`_targets.R` has six kinds of target, in this order:

1. **File targets** (`format = "file"`): the landing files and folders. A
   changed file makes its downstream targets rerun; folders that receive new
   files are re-listed on every run.
2. **Staging** (`sql/staging/*.sql`, run by `R/run_sql.R`): read one source
   and write one table per kind of data, prefixed by source (`fitbit_*`,
   `garmin_*`, `strava_*`). The target returns the table as well, so marts
   can depend on it. Two stay in R: MacroFactor (`R/load_macrofactor*.R`;
   DuckDB would need its `excel` extension) and the HR histograms
   (`R/strava_streams.R`, one branch per ride, only new rides).
3. **Data checks** (`sql/checks/`, `run_data_checks()` in `R/run_sql.R`),
   after staging: `expected_columns.sql` is the contract (the columns and
   types the marts read from each source table); `data_checks.sql` stops the
   build on a missing column, a changed type or a duplicate date or ID, and
   warns on values outside a plausible range (kept, listed in the table
   `data_checks`).
4. **Marts** (`sql/marts/*.sql`, or `R/build_*.R` where a mart needs R):
   combine staging outputs into what a view needs, with their own structural
   checks. `sql/marts/vitals_daily.sql` stops on overlapping device eras;
   `sql/marts/device_steps.sql` gives each day to one device (Garmin from
   its first day with steps).
5. **Analysis**: climb matching, effort metrics, the fitness and strength
   models, the Quest Board, relics, roadmap and scenarios.
6. **The consistency check** (`view_checks`, last): numbers that two tabs
   show must agree (`R/consistency.R`); the build stops when they don't.

Loading is plain SQL run by DuckDB (`sql/staging`, `sql/marts`); `targets`
keeps the analysis. `R/run_sql.R` runs one file; `{{name}}` placeholders take
landing paths as quoted literals, and a missing or unused name stops the run.
The `.sql` file and its landing files or folders are file targets, so either
change reruns the step. A staging file reads only the fields it lists, so a
new field in an export changes nothing until it is added. A glob that finds
no files stops the build.

DuckDB's `json` and `icu` extensions are not built into R's duckdb: they are
downloaded on first use into `~/.duckdb` (once per machine, once per CI run).

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
app/pages.R           app_pages: every page and the navbar tab that holds it
app/db.R              query(sql): read-only connection per query, answers cached until the database
                      file changes; plot_key() for the figures' cache; PDD_DB overrides the path
app/modules/<view>.R  <view>_ui(id) / <view>_server(id)
R/plot_<view>.R       the chart as a plain function, testable without Shiny
```

- A module reads its mart once per session; a rebuild shows up on reload.
  `query()` answers from a per-process cache keyed by the database file's
  modification time, so a new session doesn't reopen the database ~55
  times, and a rebuild empties it. Figures are cached app-wide with
  `bindCache(plot_key(input, "<output>"))`: the figure, its module's
  inputs, the look and the database's time.
- Phones: sidebars start closed (`sidebar_open`); a dropped connection
  reloads the page when it is visible again (`reconnect_js`), and the tab
  and page live in the URL (`?tab=body&page=weight`), so the reload opens
  where you were. Busy indicators mark a figure while it redraws.
- **Four navbar tabs by kind:** Oath (pills: Quest Board, Reliquary,
  Roadmap, Scenarios), Body (pills: Weight, Strength, Vitals), Engine (its
  own view selector in the sidebar) and Analytics. Home, the Quest Board,
  is the hub: a Today strip (level, the roadmap's current phase and next
  gate), the quest cards and the relic shelf, each linking to its full page.
  A card shows its number over time against the target (`quest_history`:
  `quest_now()` week by week, so the chart ends on the card's number;
  inline SVG from `R/plot_quests.R`), or one pip per stage; its notes fold
  under Details. From 1400 px wide Home fits one screen: the poster (title,
  Today, main quest, relics) left, the board and the rules right, three
  cards a row (four from 1800 px), charts sized by the screen's height.
  A link names a page (`link: {tab}` in `config/oath.yml`); `oath_follow()`
  opens its tab and pill through `app_pages`.
- Tab and pill names exist in both looks (`look_label()`); the corporate
  switch sets `body.pdd-corporate` in the browser to show the plain ones.
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

## The demo

A public demo of the app on invented data, deployed to Posit Connect Cloud
from the `demo` branch.

- `just build-demo` writes synthetic raw files for every source
  (`demo/make_demo_landing.R`: Fitbit, then Garmin; Strava rides past the
  example climbs, which sit at open sea; the lifting backfill, then MacroFactor) and the example configs into
  `demo/.build/`, and runs the real pipeline there. The result is
  `demo/demo.db`, committed on the `demo` branch only (with
  `manifest.json`). It never reads `data/`, `config/` or `db/`.
- The build is frozen on one day (`PDD_AS_OF`, read by `pdd_as_of()` in
  `R/common.R`), so the plan's year stays in progress.
- `app.R` at the root starts `app/` on `demo/demo.db`, locked to the
  corporate look (`PDD_LOOK`, no switch) and without relic art. `just app-demo` runs it locally.
- `manifest.json` (`just demo-manifest`) gives Connect Cloud the R version
  and packages from `renv.lock`; rerun it when the lockfile changes.

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
staging file `sql/staging/<source>_*.sql` writing `<source>_*` tables; a file
target, the `.sql` file target and a `run_sql()` target in `_targets.R`; a section in `docs/sources.md` (system of
record, day convention); a test on a synthetic file.

**A view:** a mart in `sql/marts/<view>.sql` (or `R/build_<view>.R`) with
its structural checks, and every source column it reads added to
`sql/checks/expected_columns.sql`; a plot function in
`R/plot_<view>.R`; a module in `app/modules/<view>.R`, a `nav_panel` in
`app/app.R` (a pill under the tab of its kind) and its line in `app/pages.R`; tests for the mart, the plot and the app.
