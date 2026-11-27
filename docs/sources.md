# Sources

One section per source: what it is the system of record for, where its raw
files land, and how its native dates map onto the project's day convention.
Every new source gets a section here (see the PR checklist in ROADMAP.md).

## The day convention

- A **day** is the local calendar date in `home_timezone`
  (`config/athlete.yml`). UTC timestamps are kept next to the local date.
- **Sleep and overnight measurements** (sleep, overnight HRV, sleeping
  breathing rate, SpO2, skin temperature) belong to the day you **wake up**.
- When a source already gives one value per night with a wake-up date, that
  date is used as is.
- When a source gives samples during the night, the wake-up day is the local
  date of `local time + 12 h`: everything from noon to noon belongs to the
  morning it ends in.
- A timestamp that *is* a wake-up time is dated by its own local date (adding
  12 h would push a late morning into the next day).
- Nights whose sleep ends after 12:00 local are not moved; the table
  `vitals_late_wakes` lists them for review.

## Device eras

Each record carries the device era it was measured in (`fitbit`, `garmin`;
`fitbit_wrist`, `chest_strap` for heart rate during rides). Eras are set by
source and device, never by date, and stay separate series. A spliced series
is allowed only as a derived, labelled series beside the originals.

## MacroFactor (export)

- **System of record for:** intake, body weight, lifting sets.
- **Landing:** `data/landing/macrofactor/MacroFactor-<YYYYMMDDhhmmss>.<ext>`,
  dropped unchanged. Three kinds share one name pattern: summary `.xlsx`, food
  log `.csv`, workout log `.csv` (told apart by header).
- **Dates:** local calendar dates as logged in the app; no conversion.
- **Model outputs:** trend weight and expenditure are recalculated by
  MacroFactor after edits; store them with the export date.
- **Quirks:** the workout CSV is double-encoded UTF-8 (BOM and text), handled
  in `load_macrofactor_workouts()`.

## Lifting history (one-off backfills)

- **System of record for:** lifting sets before MacroFactor's first day, from
  two sources: training templates (spreadsheets) and the Strong app.
- **Landing:** `data/landing/backfill/`, frozen:
  `drive_templates_workout_log.csv` and `strong_workout_log.csv`. Both are
  already converted to MacroFactor's workout-log layout (its 12 columns, names
  and units), followed by provenance columns: source workout and exercise
  name, how the MacroFactor exercise name was chosen, where the RIR came from
  (`RIR Basis`) and how sure the date is (`Date Confidence`, with a window).
- **Conversion rules** (done before landing): effort as RIR = 10 − RPE; AMRAP
  and rep-out sets are failure sets at RIR 0, marked as assumed; assistance
  weights negative, as MacroFactor stores them; template slots that don't say
  which option was done keep a name ending in "(template slot)", so they never
  enter a real lift's series.
- **Loading:** `sql/staging/lifting_history_sets.sql` gives
  `drive_templates_workout_sets` and `strong_workout_sets`; R stacks them with
  `macrofactor_workout_sets` into `lifting_sets` (column `source`), with one
  bodyweight join and one e1RM rule. A duplicate set stops the build.
- **Frozen:** the two CSVs were built once, outside the pipeline, from the
  template sheets and a Strong export, with a hand-made map from source
  exercise names to MacroFactor names; each row's provenance columns say
  where it came from. They are never rebuilt: corrections go in overrides.
- **Dumbbells:** MacroFactor stores the weight of both dumbbells, Strong and
  the templates one (the same curl reads about twice as heavy right after the
  switch). Staging doubles two-dumbbell exercises; `source_weight_kg` keeps the
  logged value. One-dumbbell exercises stay as logged.
- **Overrides:** `config/lifting_overrides.csv` corrects a logged RIR, rep
  count or weight for one set (for example an RPE typo), with the value the
  set must have now, so a stale override stops the build. The raw files stay
  untouched; `override_note` marks the changed sets.

## Strava (API, own fetcher)

- **System of record for:** rides and runs (activities, laps, streams).
- **Landing:** `data/landing/strava/`: activity summaries per fetch, details,
  segments and streams one JSON file per activity.
- **Dates:** `start_date` is UTC; `start_date_local` is local wall time with a
  misleading `Z`. `date` is the date part of `start_date_local`.
- **IDs** are 64-bit integers, stored as text and read exactly (segment effort
  IDs are above 2^53, where a double rounds them).

## Garmin Connect (API, own fetcher)

- **System of record for:** sleep, HRV, resting HR, breathing rate, SpO2, skin
  temperature, from the switch to Garmin onwards.
- **Landing:** `data/landing/garmin/<from>_<to>_garmin_api.json`, one record
  per calendar date; overlapping ranges are merged in staging (per field,
  the first non-missing value in file name order).
- **Dates:** Garmin's `calendarDate`. Garmin assigns a night to the date you
  wake up, which matches the convention. Sleep start and end are stored as
  UTC and local.
- **Model outputs:** HRV baseline band, HRV status, sleep score, VO2max.

## Garmin Connect (one-off export)

- **Role:** none; not loaded. The raw files stay in landing.
- **Landing:** `data/landing/garmin_export/DI_CONNECT/`.
- **Dates:** `calendarDate`, same convention as the API.
- **Why it was dropped:** its `healthStatusData` files are Garmin's Health
  Status: overnight averages of HRV, heart rate, SpO2, skin temperature and
  breathing rate, each with Health Status's own baseline and status. On the
  days both exist, every value matches the API exactly (HRV = last-night
  average, HR = average sleeping HR, not resting HR). The baselines are a
  different Garmin model from the HRV status band.

## Fitbit (Google Takeout, one-off backfill)

- **System of record for:** sleep, HRV, resting HR, breathing rate, SpO2, skin
  temperature, before the switch to Garmin; body weight before MacroFactor's
  first weigh-in.
- **Landing:** `data/landing/fitbit/Takeout/Fitbit/`, frozen.
- **Weight:** `Physical Activity_GoogleData/weight.csv` (grams) and
  `body_fat_*.csv`, read by `sql/staging/fitbit_weight.sql` into
  `fitbit_weight`. Timestamps are real UTC instants; the day is the local date
  in `home_timezone`. Body fat was typed in, so it is an estimate. The derived
  `bodyweight_for_ratios` (R/bodyweight.R) uses these weigh-ins only before
  MacroFactor starts, labelled `fitbit scale` next to `macrofactor trend`, and
  a weight older than 45 days is not used.
- **Dates:** two kinds of file, with different conventions:

  | Files | Timestamp | Wake-up day |
  |---|---|---|
  | `Physical Activity_GoogleData/daily_*.csv` (resting HR, HRV, breathing rate, SpO2, skin temperature) | Date label `YYYY-MM-DDT00:00:00Z`; the `Z` is not a real UTC midnight | The date part. Checked against the per-sample HRV and per-sleep breathing-rate files: they line up with no offset |
  | `heart_rate_variability_*.csv`, `oxygen_saturation_*.csv` (per sample) | Real UTC instants | Local date of `local time + 12 h` |
  | `respiratory_rate_sleep_summary_*.csv` (per sleep, naps included) | Wake-up time, UTC | Local date of the wake-up time |
  | `Global Export Data/sleep-*.json` | `dateOfSleep` | `dateOfSleep` as is |

- **Prefer the daily files** for daily values: they hold one value per night
  for the main sleep. The per-sample and per-sleep files are for detail only;
  averaging them by UTC date mixes two half-nights, and the per-sleep files
  include naps.
- **Quirks:** the per-sleep breathing-rate columns are named "milli breaths
  per minute" but hold breaths per minute. Daily HRV is 0 on nights that were
  recorded but had no usable HRV (non-REM heart rate is normal on those
  rows); staging reads 0 as missing. Skin temperature is an absolute
  nightly value, with Fitbit's 30-day baseline beside it; Garmin gives a
  deviation instead.
