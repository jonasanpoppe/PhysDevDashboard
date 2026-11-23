# private/

Personal analyses and tailored modules: Quarto (`.qmd`) or R Markdown
(`.Rmd`) files with content that doesn't belong in a public repo.

- Everything here except this README is **gitignored**, including rendered
  HTML. It never reaches GitHub.
- It **is** backed up: `private` is in `backup_paths` in the `justfile`, so
  `just backup` includes it.
- Read data from the pipeline, not from raw files, so a report always matches
  the latest build. Quarto renders from this folder, so point at the store one
  level up:

  ```r
  targets::tar_read(strava_segment_efforts, store = "../_targets")
  ```

  or query `../db/health.db` with a read-only DuckDB connection
  (`read_only = TRUE`) so a running build is never blocked.
- Personal values (zones, goals, benchmark climbs) come from `config/*.yml`
  rather than being typed into a report, so each lives in one place.
