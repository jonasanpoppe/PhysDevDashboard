# One source of truth (R/plan_config.R) and the check across tabs
# (R/consistency.R), on the example configs and invented conflicts.

source(testthat::test_path("..", "..", "R", "plan_config.R"))
source(testthat::test_path("..", "..", "R", "consistency.R"))

raw <- function() {
  root <- testthat::test_path("..", "..")
  lifts <- readr::read_csv(file.path(root, "config", "lifts.example.csv"), show_col_types = FALSE)
  list(oath = yaml::read_yaml(file.path(root, "config", "oath.example.yml")),
       goals = resolve_goals(yaml::read_yaml(file.path(root, "config", "goals.example.yml")), lifts),
       lifts = lifts)
}

test_that("the example configs agree, and oath.yml is filled from goals.yml", {
  r <- raw()
  expect_equal(nrow(check_plan_config(r$oath, r$goals, r$lifts)), 0)
  o <- oath_from_config(r$oath, r$goals, r$lifts)
  q <- setNames(o$quests, vapply(o$quests, `[[`, "", "id"))
  expect_equal(q$bodyweight$target, r$goals$weight_goal$target_kg)
  expect_equal(q$bodyweight$milestones[[1]]$value, r$goals$weight_goal$pause_at_kg)
  expect_equal(q$bodyweight$milestones[[1]]$note, "a week's pause")              # the copy stays in oath.yml
  expect_equal(q$judo$target, 2)
  expect_equal(q$judo$met_at, 2 * 0.8)                                          # adherence_met
  expect_equal(q$bench$target, 100)
  expect_equal(q$bench$single_ratio, 0.96)
  expect_equal(q$bench$forecast, list(lift = "bench"))
  expect_equal(q$bench$deadline, r$goals$strength$target_date)
  expect_equal(q$riding$target, 2000)
  expect_equal(q$riding$extra$goal_ride, list(km = 100, gain_m = 800, month = 6L))
  rel <- setNames(o$relics, vapply(o$relics, `[[`, "", "id"))
  expect_equal(rel$I$rule$kg, 78)                                               # goal: pause
  expect_equal(rel$I$rule$rate_kg_week, 0.4)
  expect_equal(rel$I$rule$since, r$goals$weight_goal$start_date)
  expect_equal(rel$II$rule[c("km", "gain_m", "year")], list(km = 100, gain_m = 800, year = 2030L))
  expect_equal(o$scenarios$rate_kg_week, 0.4)
  expect_null(attr(o, "conflicts"))
})

test_that("copy: {paths} filled from goals.yml; unknown paths and typed-in numbers flagged", {
  r <- raw()
  o <- oath_from_config(r$oath, r$goals, r$lifts)
  rel <- setNames(o$relics, vapply(o$relics, `[[`, "", "id"))
  expect_equal(rel$I$unlock, "78 kg, reached at 0.4 kg a week")
  expect_equal(o$copy$main_sub, "Two sessions a week, met at 1.6. Everything else gives way to it.")
  expect_match(o$copy$footer, "{level}", fixed = TRUE)                           # filled by the app
  v <- copy_values(r$goals, r$oath)
  expect_equal(fill_copy("{riding.annual_km.2030} km, {strength.targets_kg.Deadlift and hinge} kg", v),
               "2,000 km, 170\u2013175 kg")
  r$oath$copy$rules <- "{weight_goal.nope} kg"
  expect_match(check_plan_config(r$oath, r$goals, r$lifts)$problem, "\\{weight_goal.nope\\} is not a path")
  r <- raw()
  r$oath$copy$main_quest <- "At 75 kg by summer, then 2,000 km"
  p <- check_plan_config(r$oath, r$goals, r$lifts)
  expect_equal(nrow(p), 1)                                                     # one row per text
  expect_match(p$problem, "typed into the text")
  r$oath$copy$main_quest <- "At 750 kg or 75kgs, 2,0000 km"                    # no false hits
  expect_equal(nrow(check_plan_config(r$oath, r$goals, r$lifts)), 0)
})

test_that("a number written in both files and differing is a conflict; equal is fine", {
  r <- raw()
  r$oath$quests[[2]]$target <- 74                                               # bodyweight: goals.yml says 75
  p <- check_plan_config(r$oath, r$goals, r$lifts)
  expect_equal(nrow(p), 1)
  expect_match(p$problem, "target is 74 in oath.yml but 75 in goals.yml")
  expect_error(oath_from_config(r$oath, r$goals, r$lifts), "quest bodyweight: target is 74")
  r$oath$quests[[2]]$target <- 75
  expect_equal(nrow(check_plan_config(r$oath, r$goals, r$lifts)), 0)
  # A relic's own kg against its goal
  r$oath$relics[[1]]$rule$kg <- 77
  expect_match(check_plan_config(r$oath, r$goals, r$lifts)$problem, "kg is 77 in oath.yml but 78")
})

test_that("lift quests: on the bar, a main lift with a target and a forecast", {
  r <- raw()
  r$oath$quests[[4]]$kind <- "hold"
  expect_match(check_plan_config(r$oath, r$goals, r$lifts)$problem, "kind hold")
  r <- raw()
  r$oath$quests[[3]]$exercise <- "Barbell Romanian Deadlift"                     # in lifts.csv, not the main lift
  expect_match(check_plan_config(r$oath, r$goals, r$lifts)$problem, "not the main lift", all = FALSE)
  r$oath$quests[[3]]$exercise <- "Zercher Squat"
  expect_match(check_plan_config(r$oath, r$goals, r$lifts)$problem, "not in lifts.csv", all = FALSE)
})

test_that("single ratios and forecasts: the forecast's lifts, variants, else 1", {
  g <- list(strength = list(single_ratio = list("Front Squat" = 0.97)),
            strength_forecast = list(exercises = list(squat = "Barbell Back Squat"), single_ratio = list(squat = 0.98),
                                     variants = list("Front Squat" = list(lift = "squat", ratio = 0.85))))
  expect_equal(single_ratio_for("Barbell Back Squat", g), 0.98)
  expect_equal(single_ratio_for("Front Squat", g), 0.97)
  expect_equal(single_ratio_for("Leg Press", g), 1)
  expect_equal(forecast_for("Barbell Back Squat", g), list(lift = "squat"))
  expect_equal(forecast_for("Front Squat", g), list(lift = "squat", variant_ratio = 0.85))
  expect_null(forecast_for("Leg Press", g))
})

test_that("resolve_goals: the forecast's targets are the main lifts' targets on the bar, as e1RM", {
  lifts <- tibble::tibble(exercise = c("Barbell Back Squat", "Front Squat", "Barbell Bench Press"),
                          group = c("Squat", "Squat", "Bench"), main = c(FALSE, TRUE, TRUE))
  g <- list(strength = list(targets_kg = list(Squat = 120, Bench = list(100, 105))),
            strength_forecast = list(exercises = list(squat = "Barbell Back Squat", bench = "Barbell Bench Press"),
                                     single_ratio = list(squat = 0.98, bench = 0.96)))
  t <- resolve_goals(g, lifts)$strength_forecast$targets_kg
  expect_equal(t$bench, round(100 / 0.96, 1))                                   # a range counts by its low end
  expect_null(t$squat)                                                          # back squat isn't the Squat main lift
})

test_that("check_views: the synthetic app database agrees; a drifted number is caught", {
  dir <- withr::local_tempdir()
  fake <- write_fake_app_db(dir)
  con <- DBI::dbConnect(duckdb::duckdb(), fake$db)
  v <- check_views(con)
  expect_gt(nrow(v), 5)
  expect_true(all(v$ok), info = paste(v$check[!v$ok], collapse = ", "))
  # The Weight tab counting BJJ to another day than Home
  ws <- DBI::dbReadTable(con, "weight_status")
  ws$bjj_per_week <- ws$bjj_per_week + 0.25
  DBI::dbWriteTable(con, "weight_status", ws, overwrite = TRUE)
  bad <- check_views(con) %>% dplyr::filter(!ok)
  expect_equal(bad$check, "BJJ a week, last 28 days")
  expect_equal(bad$a_value, format(ws$bjj_per_week))                            # values, not names
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_error(assert_views(fake$db), "BJJ a week, last 28 days: Weight tile")
})

test_that("real data: the tabs agree (local only)", {
  db <- testthat::test_path("..", "..", "db", "health.db")
  testthat::skip_if_not(file.exists(db), "no db/health.db (run just build)")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  v <- check_views(con)
  expect_true(all(v$ok), info = paste(v$check[!v$ok], collapse = ", "))
})
