# Task runner for PhysDevDashboard. See ROADMAP.md.
# Milestone 1 adds doctor, fetch, build, build-fixture, test and app.
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
backup_paths := "data config/goals.yml config/athlete.yml private CLAUDE.local.md .env"

default:
    @just --list

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
