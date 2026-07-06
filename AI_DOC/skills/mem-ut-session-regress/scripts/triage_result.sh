#!/bin/bash

set -euo pipefail

if [[ $# -lt 3 ]]; then
    echo "Usage: triage_result.sh <result-json> <compile-log> <run-log>" >&2
    exit 1
fi

RESULT_JSON="$1"
COMPILE_LOG="$2"
RUN_LOG="$3"

status="$(sed -n 's/.*"status": "\([^"]*\)".*/\1/p' "$RESULT_JSON" | head -n 1)"
stage="$(sed -n 's/.*"stage": "\([^"]*\)".*/\1/p' "$RESULT_JSON" | head -n 1)"

summarize_log() {
    local log_file="$1"
    if [[ ! -f "$log_file" ]]; then
        echo "log-missing"
        return
    fi
    rg -n "command not found|license|Syntax error|Cannot find cell|Cross-module reference|Invalid \\$root|UVM_FATAL|UVM_ERROR|timeout|Quit count reached|TEST CASE PASSED|TEST PASSED|PASS|FAIL" "$log_file" | head -n 10 || true
}

echo "stage=$stage"
echo "status=$status"
echo "compile_log=$COMPILE_LOG"
echo "run_log=$RUN_LOG"
echo "compile_summary:"
summarize_log "$COMPILE_LOG"
echo "run_summary:"
summarize_log "$RUN_LOG"
