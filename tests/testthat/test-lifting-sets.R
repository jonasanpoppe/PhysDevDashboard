source(testthat::test_path("..", "..", "R", "run_sql.R"))
source(testthat::test_path("..", "..", "R", "e1rm.R"))
source(testthat::test_path("..", "..", "R", "load_macrofactor_workouts.R"))
source(testthat::test_path("..", "..", "R", "lifting_sets.R"))

# Invented sets in the backfill layout: MacroFactor's 12 columns + provenance.
history_header <- paste(
  "Date,Workout Duration,Workout,Exercise,Exercise Base Weight (kg),Set Type,Weight (kg),Reps,RIR,Duration,",
  "Distance short (m),Distance long (km),Source,Source Workout,Source Exercise,Exercise Match,RIR Basis,",
  "Date Confidence,Date Earliest,Date Latest,Program,Program Week,Source Ref",
  sep = ""
)
write_history <- function(path, rows) writeLines(c(history_header, rows), path)

stage_history <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  write_history(file.path(dir, "templates.csv"), c(
    "2030-01-01,,Plan W1 D1,Barbell Bench Press,,Failure Set,100,5,0,,,,drive_templates,t1,bench,exact,assumed 0 (AMRAP),medium,2029-12-30,2030-01-01,Some Plan,1,sheet!A2",
    "2030-01-01,,Plan W1 D1,Barbell Bench Press,,Standard Set,90,5,,,,,drive_templates,t1,bench,exact,missing,medium,2029-12-30,2030-01-01,Some Plan,1,sheet!A3"
  ))
  write_history(file.path(dir, "strong.csv"), c(
    "2030-02-01,3600,Evening Workout,Barbell Bench Press,,Standard Set,100,3,2.5,,,,strong,s1,Bench Press (Barbell),exact,RPE logged,exact,2030-02-01,2030-02-01,,,set 1",
    "2030-02-01,3600,Evening Workout,Assisted Pull-Up,,Standard Set,-20,8,2,,,,strong,s1,Assisted Pull Up,exact,RPE logged,exact,2030-02-01,2030-02-01,,,set 2",
    "2030-02-01,3600,Evening Workout,Barbell Bench Press,,Standard Set,95,4,2,,,,strong,s1,Bench Press (Barbell),exact,RPE logged,exact,2030-02-01,2030-02-01,,,set 3"
  ))
  db <- file.path(dir, "t.duckdb")
  run_sql(testthat::test_path("..", "..", "sql", "staging", "lifting_history_sets.sql"),
          params = list(drive_templates_csv = file.path(dir, "templates.csv"),
                        strong_csv = file.path(dir, "strong.csv")),
          read_back = c("drive_templates_workout_sets", "strong_workout_sets"), db_path = db)
}

test_that("staging keeps the sets, renames to the MacroFactor loader's columns, numbers sets in file order", {
  st <- suppressMessages(stage_history())
  s <- st$strong_workout_sets
  expect_equal(nrow(s), 3)
  expect_equal(nrow(st$drive_templates_workout_sets), 2)
  expect_true(all(c("date", "workout", "exercise", "set_type", "weight_kg", "reps", "rir",
                    "exercise_base_weight_kg", "set_number", "is_warmup", "source", "rir_basis") %in% names(s)))
  bench <- s[s$exercise == "Barbell Bench Press", ]
  expect_equal(bench$set_number, 1:2)        # the pull-up in between doesn't count
  expect_equal(bench$weight_kg, c(100, 95))
  expect_equal(bench$rir, c(2.5, 2))         # fractional RIR survives
  expect_true(is.na(st$drive_templates_workout_sets$rir[2]))
})

test_that("lifting_sets stacks the sources with one e1RM rule and bodyweight for assisted lifts", {
  st <- suppressMessages(stage_history())
  mf <- tibble::tibble(
    date = as.Date("2030-03-01"), workout = "Full Body", exercise = "Barbell Bench Press",
    exercise_base_weight_kg = NA_real_, set_type = "Standard Set", weight_kg = 100, reps = 5, rir = 1,
    set_number = 1L, is_warmup = FALSE, superset = NA_character_
  ) %>% add_set_e1rm()
  bw <- tibble::tibble(date = as.Date("2030-01-15"), bodyweight_kg = 100)

  out <- build_lifting_sets(mf, st, bodyweight = bw)
  expect_equal(sort(unique(out$source)), c("drive_templates", "macrofactor", "strong"))
  expect_equal(nrow(out), 6)
  expect_equal(out$date, sort(out$date))
  # No RIR: no e1RM. The AMRAP (failure set, RIR 0) gets one.
  tmpl <- out[out$source == "drive_templates", ]
  expect_equal(is.na(tmpl$e1rm_kg), c(FALSE, TRUE))
  expect_equal(tmpl$e1rm_kg[1], unname(e1rm_by("wathen", 100, 5, 0)))
  # Before the first weight: no bodyweight. After: assisted load = bodyweight - assistance.
  expect_true(all(is.na(tmpl$bodyweight_kg)))
  pull <- out[out$exercise == "Assisted Pull-Up", ]
  expect_equal(pull$load_kg, 80)
})

test_that("lifting_sets refuses a set that appears twice", {
  st <- suppressMessages(stage_history())
  st$strong_workout_sets <- rbind(st$strong_workout_sets, st$strong_workout_sets[1, ])
  no_mf <- tibble::tibble(date = as.Date(character()), workout = character(), exercise = character(),
                          set_number = integer())
  expect_error(build_lifting_sets(no_mf, st), "appear twice")
})
