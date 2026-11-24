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
- Nights whose sleep ends after 12:00 local are not moved; a data check lists
  them for review.

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

## Strava (API, own fetcher)

- **System of record for:** rides and runs (activities, laps, streams).
- **Landing:** `data/landing/strava/`: activity summaries per fetch, details,
  segments and streams one JSON file per activity.
- **Dates:** `start_date` is UTC; `start_date_local` is local wall time with a
  misleading `Z`. `date` is the date part of `start_date_local`.

## Garmin Connect (API, own fetcher)

- **System of record for:** sleep, HRV, resting HR, breathing rate, SpO2, skin
  temperature, from the switch to Garmin onwards.
- **Landing:** `data/landing/garmin/<from>_<to>_garmin_api.json`, one record
  per calendar date; overlapping ranges are merged in the loader.
- **Dates:** Garmin's `calendarDate`. Garmin assigns a night to the date you
  wake up, which matches the convention. Sleep start and end are stored as
  UTC and local.
- **Model outputs:** HRV baseline band, HRV status, sleep score, VO2max.

## Garmin Connect (one-off export)

- **Role:** cross-check for the API data (resting HR, HRV) where both exist;
  to be dropped once they agree.
- **Landing:** `data/landing/garmin_export/DI_CONNECT/`.
- **Dates:** `calendarDate`, same convention as the API.

## Fitbit (Google Takeout, one-off backfill)

- **System of record for:** sleep, HRV, resting HR, breathing rate, SpO2, skin
  temperature, before the switch to Garmin.
- **Landing:** `data/landing/fitbit/Takeout/Fitbit/`, frozen.
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
  per minute" but hold breaths per minute. Skin temperature is an absolute
  nightly value, with Fitbit's 30-day baseline beside it; Garmin gives a
  deviation instead.
