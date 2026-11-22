library(readr)
library(dplyr)
library(janitor)
library(duckdb)

# MacroFactor food log export (CSV): one row per logged food item, with
# macros and ~50 micronutrient columns.
load_macrofactor_food_log <- function(
    path,
    db_path = "db/health.db"
) {

  con <- DBI::dbConnect(duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  food_df <- read_csv(
    path,
    col_types = cols(
      Date        = col_date(),
      Time        = col_character(),
      `Food Name` = col_character(),
      `Serving Size` = col_character(),
      .default    = col_double()
    ),
    show_col_types = FALSE
  ) |>
    clean_names() |>
    # Times look like "9:00 AM". readr parses %p in English whatever the
    # system locale, unlike strptime().
    mutate(time = parse_time(time, format = "%I:%M %p")) |>
    arrange(date, time)

  DBI::dbWriteTable(con, "macrofactor_food_log", food_df, overwrite = TRUE)
  message("✓ macrofactor_food_log written (", nrow(food_df), " rows)")

  invisible(food_df)
}
