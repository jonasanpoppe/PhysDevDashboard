# PhysDevDashboard

Personal health data pipeline and Shiny dashboard. This file is the short
version for every session; each topic has one home:

| Topic | The source |
|---|---|
| Commands | `just --list` (the justfile; `just doctor`, `build`, `test`, `app`, `backup`) |
| How it is built: data flow, layout, tools, testing | `docs/architecture.md` |
| Principles, data conventions, milestones, PR checklist | `ROADMAP.md` (read before changing structure) |
| Per source: system of record, landing path, day convention | `docs/sources.md` |
| The plan's numbers (targets, rates, km) | `config/goals.yml`; `config/oath.yml` names them |
| Colours | `R/theme_physdev.R` |

Python runs only through `uv` (`uv run python/<fetcher>.py`); dependencies
live in `pyproject.toml`. R uses `renv`.

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
- **One source per fact.** Write a fact in its home (the table above) and
  link to it elsewhere; don't copy it into a second doc or file.
- **One source for the plan's numbers.** Targets, rates, sessions a week,
  lift targets (the heavy single on the bar) and single ratios live in
  `config/goals.yml`; `oath.yml` says how they are shown and judged and is
  filled from goals.yml (`R/plan_config.R`). Two tabs showing the same
  number use the same function; `just build` stops when they disagree
  (`R/consistency.R`). Add a check there for every number a new view repeats.

## Working agreements

- Vertical slices: build only what a view needs.
- One branch and one PR per milestone. Propose commits; commit only when
  asked, never push unasked.
- Explain non-obvious choices; the owner reviews every change.
