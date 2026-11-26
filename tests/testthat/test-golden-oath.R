# Golden test on real data (local only): the Quest Board's "Now" values
# match the plan doc on its baseline date. Expected values:
# private/golden_oath.yml (gitignored); config: config/oath.yml; data:
# db/health.db. Without any of them, the test skips.

root <- testthat::test_path("..", "..")
golden_path <- file.path(root, "private", "golden_oath.yml")
config_path <- file.path(root, "config", "oath.yml")
db_path <- file.path(root, "db", "health.db")

test_that("golden: Quest Board now-values on the plan's baseline date", {
  testthat::skip_if_not(file.exists(golden_path), "no private/golden_oath.yml")
  testthat::skip_if_not(file.exists(config_path), "no config/oath.yml")
  testthat::skip_if_not(file.exists(db_path), "no db/health.db (run just build)")
  source(file.path(root, "R", "quests.R"))
  g <- yaml::read_yaml(golden_path)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  b <- quest_board(yaml::read_yaml(config_path), quest_data(con), as_of = g$as_of)
  for (id in names(g$now)) {
    want <- g$now[[id]]
    got <- b$now_value[b$id == id]
    expect_equal(b$source[b$id == id], "live", info = id)
    if (!is.null(want$low)) {
      expect_gte(got, want$low)
      expect_lte(got, want$high)
    } else {
      expect_equal(got, want$value, tolerance = want$tolerance, scale = 1, info = id)
    }
  }
})
