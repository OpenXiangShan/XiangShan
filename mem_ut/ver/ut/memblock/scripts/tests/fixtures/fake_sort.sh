#!/usr/bin/env bash
set -euo pipefail

for arg in "$@"; do
    if [[ "$arg" == *'.live_intervals.'* ]]; then
        count_file="$FAKE_EDA_STATE_DIR/peak_recompute.count"
        count=0; [[ ! -f "$count_file" ]] || count="$(cat "$count_file")"
        printf '%s\n' "$((count + 1))" >"$count_file"
        break
    fi
done
exec /usr/bin/sort "$@"
