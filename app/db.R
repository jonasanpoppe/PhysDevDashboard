# Database access for the app: read-only, one connection per query, and
# each answer cached until the database changes.
#
# DuckDB allows one read-write process or several read-only ones. Opening a
# read-only connection per query and closing it straight away means an open
# app never blocks `just build`. Opening one costs ~50 ms, and a session
# asks ~55 queries, so the answers are kept per R process, keyed by the
# query and the database file's modification time: the data only changes
# on a rebuild, which changes the time and empties the cache.

db_path <- function() {
  Sys.getenv("PDD_DB", file.path(project_root, "db", "health.db"))
}

# The database's version: its modification time (a rebuild changes it).
# Also part of the figures' cache keys (bindCache in the modules).
db_stamp <- function() {
  path <- db_path()
  if (!file.exists(path)) stop("No database at ", path, ". Run `just build` first.")
  format(file.mtime(path), "%Y-%m-%d %H:%M:%OS6")
}

query_cache <- new.env(parent = emptyenv())

query <- function(sql, params = NULL) {
  stamp <- db_stamp()
  if (!identical(query_cache$.stamp, stamp)) {          # a rebuild: forget everything
    rm(list = ls(query_cache, all.names = TRUE), envir = query_cache)
    query_cache$.stamp <- stamp
  }
  key <- paste(sql, paste(unlist(params), collapse = "\r"), sep = "\n")
  hit <- query_cache[[key]]
  if (!is.null(hit)) return(hit)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), db_path(), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  out <- DBI::dbGetQuery(con, sql, params = params) %>% tibble::as_tibble()
  assign(key, out, envir = query_cache)
  out
}

# Cache key for a module's figure (bindCache): which figure (the module's
# namespace and the output's name: the app-wide cache is shared), everything
# the module's inputs hold, the look and the database's version.
# Over-inclusive on purpose: an input the figure doesn't use only costs a
# cache miss, never a stale figure.
plot_key <- function(input, output_id, session = getDefaultReactiveDomain()) {
  list(session$ns(output_id), reactiveValuesToList(input), ui_mode(), db_stamp())
}

# Whether a table exists (for tabs whose table a build may not have written yet)
db_has_table <- function(name) {
  nrow(query("SELECT 1 FROM information_schema.tables WHERE table_name = ?", params = list(name))) > 0
}
