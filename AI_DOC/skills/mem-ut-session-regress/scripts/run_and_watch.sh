#!/bin/bash

set -euo pipefail

usage() {
    cat <<'EOF'
Usage:
  run_and_watch.sh <testcase> [mode]

Example:
  run_and_watch.sh tc_smoke smoke
EOF
}

if [[ $# -lt 1 ]]; then
    usage
    exit 1
fi

TC="$1"
MODE="${2:-smoke}"
SIM_DIR="${MEM_UT_SIM_DIR:-/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/sim}"
STATUS_POLL_SEC="${STATUS_POLL_SEC:-15}"
TAIL_LINES="${TAIL_LINES:-40}"

cd "$SIM_DIR"

echo "[1/3] compile: tc=$TC mode=$MODE"
make eda_compile "tc=$TC" "mode=$MODE"

echo "[2/3] run: tc=$TC mode=$MODE"
make eda_run_bg "tc=$TC" "mode=$MODE"

echo "[3/3] watch: tc=$TC mode=$MODE"
while true; do
    if make eda_status "tc=$TC" "mode=$MODE"; then
        :
    else
        echo "status query failed"
    fi

    LOG_GLOB="$SIM_DIR/$MODE/log/${TC}_*_${MODE:+rtl}.log"
    FOUND_LOG="$(ls -1t $SIM_DIR/$MODE/log/${TC}_*.log 2>/dev/null | head -n 1 || true)"
    if [[ -n "$FOUND_LOG" ]]; then
        echo "--- tail: $FOUND_LOG ---"
        tail -n "$TAIL_LINES" "$FOUND_LOG" || true
        if rg -n "UVM_FATAL|UVM_ERROR|TEST CASE PASSED|TEST PASSED|PASS|FAIL|timeout" "$FOUND_LOG" >/dev/null 2>&1; then
            echo "Detected terminal keywords in log: $FOUND_LOG"
            exit 0
        fi
    fi

    sleep "$STATUS_POLL_SEC"
done
