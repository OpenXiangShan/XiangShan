#!/bin/bash

set -euo pipefail

usage() {
    cat <<'EOF'
Usage:
  auto_resume_regress.sh <testcase> [mode]

Environment:
  CODEX_TMUX_SESSION Optional tmux session name for the active Codex conversation
  CODEX_PROJECT_DIR  Project directory opened inside tmux Codex session
  SESSION_ID         Optional codex session id to resume explicitly
  RESUME_WITH_LAST   Default 1. If SESSION_ID is empty, use codex resume --last
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
COMPILE_LOG="$SIM_DIR/$MODE/log/vcs_compile_rtl.log"
RUN_LOG_GLOB="$SIM_DIR/$MODE/log/${TC}_*.log"
TRIAGE_TXT="$RESULT_DIR/${TC}.triage.txt"
CODEX_TMUX_SESSION="${CODEX_TMUX_SESSION:-codex_memblock}"
CODEX_PROJECT_DIR="${CODEX_PROJECT_DIR:-/nfs/home/lixiangrui/work/memblock_ut/XiangShan}"
SESSION_ID="${SESSION_ID:-}"
RESUME_WITH_LAST="${RESUME_WITH_LAST:-1}"

mkdir -p "$RESULT_DIR"

cd "$SIM_DIR"

echo "[resume-auto] start tc=$TC mode=$MODE"
bash "$SCRIPT_DIR/start_codex_tmux.sh"
bash "$SCRIPT_DIR/auto_regress.sh" "$TC" "$MODE" || true

RUN_LOG="$(ls -1t $RUN_LOG_GLOB 2>/dev/null | head -n 1 || true)"
bash "$SCRIPT_DIR/triage_result.sh" "$RESULT_JSON" "$COMPILE_LOG" "$RUN_LOG" > "$TRIAGE_TXT"
exec env \
    CODEX_TMUX_SESSION="$CODEX_TMUX_SESSION" \
    CODEX_PROJECT_DIR="$CODEX_PROJECT_DIR" \
    MEM_UT_SIM_DIR="$SIM_DIR" \
    SESSION_ID="$SESSION_ID" \
    RESUME_WITH_LAST="$RESUME_WITH_LAST" \
    RESULT_JSON="$RESULT_JSON" \
    COMPILE_LOG="$COMPILE_LOG" \
    RUN_LOG="$RUN_LOG" \
    TRIAGE_TXT="$TRIAGE_TXT" \
    bash "$SCRIPT_DIR/resume_from_result.sh" "$TC" "$MODE"
