# Database access for the app: read-only, one connection per query.
#
# DuckDB allows one read-write process or several read-only ones. Opening a
# read-only connection per query and closing it straight away means an open
# app never blocks `just build`. Queries are small, so the cost is negligible.

db_path <- function() {
  Sys.getenv("PDD_DB", file.path(project_root, "db", "health.db"))
}

query <- function(sql, params = NULL) {
  path <- db_path()
  if (!file.exists(path)) {
    stop("No database at ", path, ". Run `just build` first.")
  }
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbGetQuery(con, sql, params = params) %>% tibble::as_tibble()
}
