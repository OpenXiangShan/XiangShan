#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat >&2 <<'EOF'
Usage: copy_simulator_logs.sh [--direct] <remote-directory> [destination-directory]

Recursively copy simulator_err.txt and simulator_out.txt from a remote report
directory, preserving their relative directory structure. Use --direct only
when the supplied path is a single result directory containing the two files.

Examples:
  copy_simulator_logs.sh node040:/nfs/home/cirunner/perf-report/cr260902-5d3934132-KunminghuV2Config
  copy_simulator_logs.sh node040:/path/to/one-result ./logs
  copy_simulator_logs.sh --direct node040:/path/to/one-result ./logs
EOF
}

recursive=true
if [[ ${1:-} == '--direct' ]]; then
  recursive=false
  shift
fi

if (( $# < 1 || $# > 2 )); then
  usage
  exit 2
fi

remote_dir=${1%/}

if [[ "$remote_dir" != *:* ]]; then
  printf 'error: expected a remote directory such as host:/path/to/report\n' >&2
  exit 2
fi

remote_path=${remote_dir#*:}
report_name=${remote_path##*/}

if [[ -z "$report_name" ]]; then
  printf 'error: the remote path must name a report directory\n' >&2
  exit 2
fi

if [[ "$recursive" == true ]]; then
  destination=${2:-"./$report_name"}
else
  destination=${2:-.}
fi

if [[ -e "$destination" && ! -d "$destination" ]]; then
  printf 'error: destination exists but is not a directory: %s\n' "$destination" >&2
  exit 2
fi

mkdir -p -- "$destination"

if [[ "$recursive" == false ]]; then
  scp -- \
    "$remote_dir/simulator_err.txt" \
    "$remote_dir/simulator_out.txt" \
    "$destination/"
else
  # Directory entries are included only so rsync can descend into them. All
  # other files are excluded, and -m removes directories that contain no logs.
  rsync -rtm --human-readable --info=progress2 \
    --include='*/' \
    --include='simulator_err.txt' \
    --include='simulator_out.txt' \
    --exclude='*' \
    -- "$remote_dir/" "$destination/"
fi

printf 'Copied simulator logs to %s\n' "$destination"
