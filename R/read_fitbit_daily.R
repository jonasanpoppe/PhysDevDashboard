library(readr)
library(dplyr)
library(janitor)

# Fitbit's daily_*.csv files label each day as "YYYY-MM-DDT00:00:00Z". The
# "Z" is not a real UTC midnight: the date part is the day itself. Reading the
# timestamp as text and keeping the date part avoids any timezone shift.
read_fitbit_daily <- function(path) {
  read_csv(
    path,
    col_types = cols(timestamp = col_character(), `data source` = col_character(),
                     .default = col_double()),
    show_col_types = FALSE
  ) %>%
    clean_names() %>%
    mutate(date = as.Date(substr(timestamp, 1, 10)), .before = 1) %>%
    select(-timestamp, -data_source)
}
