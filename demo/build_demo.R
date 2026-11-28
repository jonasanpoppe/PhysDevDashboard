# `just build-demo`: the demo database, demo/demo.db, from synthetic raw files
# (demo/make_demo_landing.R) and the example configs, built by the real
# pipeline (_targets.R) in a scratch copy of the project: its own
# data/landing/, config/ and db/, with R/, sql/ and _targets.R linked in.
# Never reads data/, config/ or db/ of this checkout. The build is frozen on
# DEMO$as_of (PDD_AS_OF), so the Quest Board and Roadmap stay mid-year.
#
# Run from the project root. The app opens it with `just app-demo`, the cloud
# demo with app.R at the root.

suppressMessages({
  source("fixtures/macrofactor_schema.R")
  source("fixtures/make_macrofactor_fixture.R")
  library(dplyr)
  source("demo/make_demo_landing.R")
})

repo <- normalizePath(".")
work <- file.path(repo, "demo", ".build")   # kept after the build, for a look at a failure (gitignored)
unlink(work, recursive = TRUE)
dir.create(file.path(work, "db"), recursive = TRUE)
for (f in c("R", "sql", "_targets.R")) file.symlink(file.path(repo, f), file.path(work, f))

make_demo_landing(work, repo)

Sys.setenv(PDD_AS_OF = format(DEMO$as_of))
withr::with_dir(work, targets::tar_make(reporter = "balanced"))

out <- file.path(repo, "demo", "demo.db")
file.copy(file.path(work, "db", "health.db"), out, overwrite = TRUE)
message("✓ demo database: ", out, " (", round(file.size(out) / 1e6, 1), " MB, as of ", DEMO$as_of, ")")
