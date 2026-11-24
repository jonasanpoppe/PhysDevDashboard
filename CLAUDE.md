# PhysDevDashboard

Personal health data pipeline and Shiny dashboard. ROADMAP.md holds the
principles, architecture, data conventions and milestones; read it before
changing structure. This file is the short version for every session.

## Commands

```
just doctor   # tools, versions, secrets, personal config
just fetch    # Strava (incremental) + Garmin (last 30 days)
just build    # rebuild db/health.db from data/landing/ (targets)
just test     # R tests (testthat)
just backup   # restic snapshot of data/, config/, private/, .env
```

Python runs only through `uv` (`uv run python/<fetcher>.py`); dependencies
live in `pyproject.toml`. R uses `renv`.

## Layout

```
data/landing/<source>/   raw files, never edited (gitignored)
  garmin/ strava/ macrofactor/ fitbit/ garmin_export/
db/                      disposable database, rebuilt by `just build`
config/                  personal config, gitignored; *.example.* committed
python/                  fetchers: write files only, never the database
R/                       loaders and analysis, sourced by _targets.R
tests/testthat/          tests
private/                 private specs and reports, gitignored
```

## Rules

- **Public repo, private data.** No personal values, targets, names of
  personal routes or results in code, tests, docs or commit messages. New
  personal config goes in `config/` with an invented `*.example.*` twin.
  Fixtures are synthetic. Content from `private/` never leaks into tracked
  files.
- **Raw files are the truth.** Never edit `data/landing/`; the database is
  rebuilt from it. Back up (`just backup`) before moving or deleting data.
- **One writer.** Python writes files; R builds and reads the database.
- **Systems of record:** see ROADMAP.md. Rides and streams come from Strava;
  never sum the same ride across sources.
- **No silent splicing.** Device eras stay separate series; a spliced series
  is derived, labelled and kept beside the originals.
- **Days** are local calendar dates; sleep and overnight HRV belong to the
  wake-up day.
- **Model outputs are not measurements** (trend weight, expenditure,
  estimated power): store them as such and show their uncertainty.
- **Table names** carry their source prefix (`strava_*`, `garmin_*`).

## Working agreements

- Vertical slices: build only what a view needs.
- One branch and one PR per milestone. Propose commits; commit only when
  asked, never push unasked.
- Explain non-obvious choices; the owner reviews every change.
