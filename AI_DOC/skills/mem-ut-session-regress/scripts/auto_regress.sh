#!/bin/bash

set -euo pipefail

usage() {
    cat <<'EOF'
Usage:
  auto_regress.sh <testcase> [mode]

Example:
  auto_regress.sh tc_smoke smoke
EOF
}

if [[ $# -lt 1 ]]; then
    usage
    exit 1
fi

TC="$1"
MODE="${2:-smoke}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIM_DIR="${MEM_UT_SIM_DIR:-/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/sim}"
RESULT_DIR="$SIM_DIR/$MODE/result"
RESULT_JSON="$RESULT_DIR/${TC}.json"
STATUS_POLL_SEC="${STATUS_POLL_SEC:-15}"
TAIL_LINES="${TAIL_LINES:-60}"
MAX_RETRIES="${MAX_RETRIES:-2}"

mkdir -p "$RESULT_DIR"
cd "$SIM_DIR"

write_result() {
    local stage="$1"
    local status="$2"
    local detail="$3"
    cat > "$RESULT_JSON" <<EOF
{
  "testcase": "$TC",
  "mode": "$MODE",
  "stage": "$stage",
  "status": "$status",
  "detail": "$detail"
}
EOF
}

latest_log() {
    ls -1t "$SIM_DIR/$MODE/log/${TC}"*.log 2>/dev/null | head -n 1 || true
}

log_has_terminal_state() {
    local log_file="$1"
    [[ -n "$log_file" ]] || return 1
    rg -n "TEST CASE PASSED|TEST PASSED|PASS|FAIL|UVM_FATAL|UVM_ERROR|timeout" "$log_file" >/dev/null 2>&1
}

classify_log() {
    local log_file="$1"
    [[ -n "$log_file" ]] || { echo "UNKNOWN"; return; }
    if rg -n 'command not found|module: command not found|license|Cannot find cell|Invalid \$root|Cross-module reference|Syntax error|UVM_ERROR|UVM_FATAL|timeout|Quit count reached' "$log_file" >/dev/null 2>&1; then
        if rg -n "command not found|module: command not found|license" "$log_file" >/dev/null 2>&1; then
            echo "ENV_FAIL"
        elif rg -n "Cannot write to .*work.lib\\+\\+|is in use of another process|locked by another process" "$log_file" >/dev/null 2>&1; then
            echo "LOCK_FAIL"
        elif rg -n 'Syntax error|Cannot find cell|Invalid \$root|Cross-module reference' "$log_file" >/dev/null 2>&1; then
            echo "COMPILE_FAIL"
        elif rg -n "UVM_ERROR|UVM_FATAL|Quit count reached|timeout" "$log_file" >/dev/null 2>&1; then
            echo "RUN_FAIL"
        else
            echo "FAIL"
        fi
        return
    fi
    echo "UNKNOWN"
}

cleanup_mode() {
    local mode_dir="$SIM_DIR/$MODE"
    mkdir -p "$mode_dir"
    find "$mode_dir" -mindepth 1 -maxdepth 1 \
        ! -name result \
        -exec rm -rf {} +
}

watch_run() {
    while true; do
        make eda_status "tc=$TC" "mode=$MODE" >/tmp/auto_regress_status.txt 2>&1 || true
        local status_out
        status_out="$(cat /tmp/auto_regress_status.txt 2>/dev/null || true)"
        local log_file
        log_file="$(latest_log)"
        if [[ -n "$log_file" ]]; then
            echo "--- tail: $log_file ---"
            tail -n "$TAIL_LINES" "$log_file" || true
            if log_has_terminal_state "$log_file"; then
                if rg -n "TEST CASE PASSED|TEST PASSED|PASS" "$log_file" >/dev/null 2>&1; then
                    write_result "run" "PASS" "$log_file"
                    return 0
                fi
                write_result "run" "$(classify_log "$log_file")" "$log_file"
                return 1
            fi
        fi
        if grep -q "EXITED" <<<"$status_out"; then
            write_result "run" "UNKNOWN_EXIT" "$status_out"
            return 1
        fi
        sleep "$STATUS_POLL_SEC"
    done
}

attempt=0
while (( attempt <= MAX_RETRIES )); do
    echo "[auto] attempt=$attempt tc=$TC mode=$MODE"
    cleanup_mode

    echo "[auto] compile tc=$TC mode=$MODE"
    if make eda_compile "tc=$TC" "mode=$MODE"; then
        write_result "compile" "PASS" "compile finished"
    else
        compile_log="$SIM_DIR/$MODE/log/vcs_compile_rtl.log"
        compile_status="$(classify_log "$compile_log")"
        write_result "compile" "$compile_status" "$compile_log"
        if [[ "$compile_status" == "LOCK_FAIL" ]] && (( attempt < MAX_RETRIES )); then
            attempt=$((attempt + 1))
            continue
        fi
        exit 1
    fi

    echo "[auto] run tc=$TC mode=$MODE"
    make eda_run_bg "tc=$TC" "mode=$MODE"
    if watch_run; then
        exit 0
    fi

    result_status="$(sed -n 's/.*\"status\": \"\\([^\"]*\\)\".*/\\1/p' "$RESULT_JSON" | head -n 1)"
    if [[ "$result_status" == "LOCK_FAIL" ]] && (( attempt < MAX_RETRIES )); then
        attempt=$((attempt + 1))
        continue
    fi
    exit 1
done
