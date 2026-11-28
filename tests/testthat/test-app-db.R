# app/db.R: queries cached until the database changes; figures' cache keys.

test_that("a query is answered from the cache until the database file changes", {
  dir <- withr::local_tempdir()
  db <- file.path(dir, "t.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), db); DBI::dbWriteTable(con, "t", data.frame(x = 1)); DBI::dbDisconnect(con, shutdown = TRUE)
  withr::local_envvar(PDD_DB = db)
  env <- new.env()
  env$project_root <- dir
  local(library(magrittr), envir = env)
  sys.source(testthat::test_path("..", "..", "app", "db.R"), envir = env)
  q <- function() env$query("SELECT x FROM t")$x
  expect_equal(q(), 1)
  # Change the data without changing the file's time: still the cached answer
  t0 <- file.mtime(db)
  con <- DBI::dbConnect(duckdb::duckdb(), db); DBI::dbExecute(con, "UPDATE t SET x = 2"); DBI::dbDisconnect(con, shutdown = TRUE)
  Sys.setFileTime(db, t0)
  expect_equal(q(), 1)
  # A rebuild changes the time: the cache is emptied
  Sys.setFileTime(db, t0 + 60)
  expect_equal(q(), 2)
  expect_true(env$db_has_table("t"))
  expect_false(env$db_has_table("nope"))
})

test_that("figures' cache keys name the figure, its inputs, the look and the database", {
  env <- new.env()
  env$project_root <- tempdir()
  env$ui_mode <- function() "oath"
  env$db_stamp_value <- "v1"
  sys.source(testthat::test_path("..", "..", "app", "db.R"), envir = env)
  env$db_stamp <- function() env$db_stamp_value
  shiny::testServer(function(input, output, session) {}, {
    session$setInputs(x = 1)
    a <- env$plot_key(input, "plot", session)
    b <- env$plot_key(input, "matrix", session)
    expect_false(identical(a, b))                     # two figures, one module: two keys
    session$setInputs(x = 2)
    expect_false(identical(a, env$plot_key(input, "plot", session)))
    env$db_stamp_value <- "v2"
    session$setInputs(x = 1)
    expect_false(identical(a, env$plot_key(input, "plot", session)))   # a rebuild: a new key
  })
})
