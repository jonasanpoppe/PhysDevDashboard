# Task runner for PhysDevDashboard. See ROADMAP.md.
# Recipes: doctor, fetch, build, test, app, backup. Still to come: build-fixture.
#
# Backups: an encrypted restic repository in a Backblaze B2 bucket, reached
# through B2's S3-compatible API (restic's recommended route for B2).
# Configure in .env (gitignored, chmod 600), with absolute paths:
#
#   RESTIC_REPOSITORY=s3:https://s3.<region>.backblazeb2.com/<bucket>/pha
#   RESTIC_PASSWORD_FILE=/home/<you>/.config/restic/pha.pass
#   AWS_ACCESS_KEY_ID=<B2 keyID, scoped to this bucket>
#   AWS_SECRET_ACCESS_KEY=<B2 applicationKey>
#   AWS_DEFAULT_REGION=<region>
#
# The bucket needs a lifecycle rule that deletes hidden files (restic's S3
# backend only hides what prune removes), or storage grows forever.
#
# The password file is chmod 600, lives outside the repo, and its contents are
# also kept in a password manager. Without it the backup cannot be opened.

set dotenv-load

# Raw data, personal config, private reports and secrets. db/ is rebuilt
# from data/, so it is never backed up.
backup_paths := "data config private CLAUDE.local.md .env"

default:
    @just --list

# Check tools, versions, secrets and personal config
doctor:
    #!/usr/bin/env bash
    set -uo pipefail
    fail=0
    ok()   { printf '  ok    %s\n' "$*"; }
    warn() { printf '  warn  %s\n' "$*"; }
    bad()  { printf '  FAIL  %s\n' "$*"; fail=1; }
    echo "Tools"
    for t in R Rscript uv restic; do
        command -v "$t" >/dev/null && ok "$t" || bad "$t not found"
    done
    r_duckdb=$(Rscript -e 'cat(as.character(packageVersion("duckdb")))' 2>/dev/null | tail -n1) \
        && ok "R duckdb $r_duckdb (decides the database file format)" \
        || bad "R package duckdb not installed"
    for p in targets testthat; do
        Rscript -e "quit(status = !requireNamespace('$p', quietly = TRUE))" 2>/dev/null \
            && ok "R $p" || bad "R package $p not installed"
    done
    # Every other DuckDB reader must be the same version as R's or newer
    if command -v duckdb >/dev/null; then
        cli=$(duckdb --version | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
        if [[ "$(printf '%s\n' "$r_duckdb" "$cli" | sort -V | head -1)" == "$r_duckdb" ]]; then
            ok "duckdb CLI $cli"
        else
            bad "duckdb CLI $cli is older than R's $r_duckdb: it can't read the database"
        fi
    else
        warn "duckdb CLI not installed (optional)"
    fi
    uv sync --quiet && ok "Python env in sync (uv.lock)" || bad "uv sync failed"
    echo "Secrets"
    if [[ -f .env ]]; then
        [[ "$(stat -c %a .env)" == 600 ]] && ok ".env (600)" || bad ".env should be chmod 600"
    else
        bad ".env missing"
    fi
    [[ -f ~/.strava/tokens.json ]] && ok "Strava tokens" \
        || warn "no Strava tokens: uv run python/fetch_strava_api.py auth"
    [[ -d ~/.garminconnect ]] && ok "Garmin tokens" \
        || warn "no Garmin tokens: the first fetch will ask for a login"
    echo "Personal config"
    for ex in config/*.example.*; do
        real="${ex/.example/}"
        [[ -f "$real" ]] && ok "$real" || warn "$real missing: copy $ex and fill it in"
    done
    exit $fail

# Fetch new data: Strava (incremental) and Garmin (last 30 days)
fetch: fetch-strava fetch-garmin

fetch-strava:
    uv run python/fetch_strava_api.py

fetch-garmin:
    uv run python/fetch_garmin_api.py

# Rebuild the database from data/landing/ (R loaders in _targets.R for now)
build:
    Rscript -e 'targets::tar_make()'

# Start the Shiny app on http://localhost:3838 (reads db/ read-only)
app:
    Rscript -e 'shiny::runApp("app", port = 3838, launch.browser = FALSE)'

# Run the R tests
test:
    Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

# Create the restic repository (run once)
backup-init:
    restic init

# Snapshot raw data, goals and secrets, then apply the retention policy
backup:
    #!/usr/bin/env bash
    set -euo pipefail
    paths=()
    for p in {{ backup_paths }}; do
        if [[ -e "$p" ]]; then
            paths+=("$PWD/$p")   # absolute, so restore-test knows where files land
        else
            echo "skip: $p (not found)"
        fi
    done
    restic backup --tag pha "${paths[@]}"
    restic forget --tag pha --keep-last 20 --keep-monthly 24 --prune

# List snapshots
snapshots:
    restic snapshots --tag pha

# Restore data/ from the latest snapshot to a temp dir and compare with the live copy
restore-test:
    #!/usr/bin/env bash
    set -euo pipefail
    tmp="$(mktemp -d)"
    trap 'rm -rf "$tmp"' EXIT
    restic restore latest --tag pha --target "$tmp" --include "$PWD/data"
    if diff -r "$PWD/data" "$tmp$PWD/data"; then
        echo "restore-test: OK, data/ restored identically"
    else
        echo "restore-test: FAILED, see differences above (run 'just backup' first if data/ changed since)" >&2
        exit 1
    fi
    # Once 'just build' exists: build from "$tmp$PWD/data" and compare mart row counts.

# Verify repository structure and re-read a random 5% of stored data
backup-check:
    restic check --read-data-subset=5%
