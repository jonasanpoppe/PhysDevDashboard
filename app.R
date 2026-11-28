# The public demo (Posit Connect Cloud deploys this file; locally: `just
# app-demo`). The app in app/ on demo/demo.db: synthetic data and the example
# configs only (demo/build_demo.R), locked to the corporate look. No relic
# art: the cards show their placeholders.
root <- normalizePath(".")
Sys.setenv(
  PDD_DB        = file.path(root, "demo", "demo.db"),
  PDD_LOOK      = "corporate",   # locked: no oath look in the demo
  PDD_RELIC_ART = file.path(root, "demo", "relics")
)
shiny::shinyAppDir(file.path(root, "app"))
