# `just demo-manifest`: manifest.json for Posit Connect Cloud, which reads
# the R version and the packages from it (taken from renv.lock). Rerun after
# a change to renv.lock. Lists only the files the demo app needs. rsconnect
# is a deploy tool, not a project dependency: it is installed into a
# temporary library, never into renv.
lib <- file.path(tempdir(), "rsconnect-lib")
dir.create(lib, showWarnings = FALSE)
if (!requireNamespace("rsconnect", quietly = TRUE, lib.loc = lib)) {
  install.packages("rsconnect", lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)
}
.libPaths(c(.libPaths(), lib))
files <- c("app.R", "DESCRIPTION", "renv.lock", "demo/demo.db",
           list.files("app", recursive = TRUE, full.names = TRUE),
           list.files("R", recursive = TRUE, full.names = TRUE))
rsconnect::writeManifest(appDir = ".", appFiles = files, appPrimaryDoc = "app.R",
                         appMode = "shiny", verbose = FALSE)
message("✓ manifest.json (", length(files), " files)")
