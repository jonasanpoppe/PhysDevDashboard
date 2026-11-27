library(dplyr)

# The SQL layer: plain .sql files under sql/ (staging/, marts/), run by R
# through DuckDB. R only runs them; the transformation lives in the file, so
# it reads the same in any SQL client.
#
# A file may use {{name}} placeholders for landing paths, filled from
# `params` as quoted SQL strings (single quotes doubled), so a path with a
# space or a quote stays one literal. Unfilled or unused placeholders stop the
# run: a typo never reaches DuckDB as an empty path.
#
# run_sql() runs one file (several statements are fine) and returns the
# tables it lists in `read_back`, so targets downstream can use them.

render_sql <- function(sql, params = list()) {
  used <- unique(regmatches(sql, gregexpr("\\{\\{[a-z_]+\\}\\}", sql))[[1]])
  used <- gsub("[{}]", "", used)
  missing <- setdiff(used, names(params))
  if (length(missing) > 0) stop("run_sql: no value for {{", paste(missing, collapse = "}}, {{"), "}}")
  unused <- setdiff(names(params), used)
  if (length(unused) > 0) stop("run_sql: parameter not used in the SQL: ", paste(unused, collapse = ", "))
  for (name in used) {
    value <- paste0("'", gsub("'", "''", params[[name]], fixed = TRUE), "'")
    sql <- gsub(paste0("{{", name, "}}"), value, sql, fixed = TRUE)
  }
  sql
}

# DuckDB's json and icu extensions are not built into R's duckdb: they are
# downloaded on first use. shared_home keeps them in ~/.duckdb, so that
# happens once per machine (and once per CI run), not once per R session
# (~2.5 s and a network call every build).
run_sql <- function(path, params = list(), read_back = character(), db_path = "db/health.db") {
  sql <- render_sql(paste(readLines(path, warn = FALSE), collapse = "\n"), params)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = TRUE), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, sql)
  out <- lapply(stats::setNames(read_back, read_back), \(t) tibble::as_tibble(DBI::dbReadTable(con, t)))
  for (t in read_back) message("✓ ", t, " written by ", basename(path), " (", nrow(out[[t]]), " rows)")
  if (length(read_back) == 1) invisible(out[[1]]) else invisible(out)
}
