#!/bin/bash

set -euo pipefail

usage() {
    cat <<'EOF'
Usage:
  resume_from_result.sh <testcase> [mode]

Environment:
  CODEX_TMUX_SESSION Optional tmux session to inject prompt into
  SESSION_ID         Optional codex session id to resume explicitly
  RESUME_WITH_LAST   Default 1. If SESSION_ID is empty, use codex resume --last
  RESULT_JSON        Optional explicit result json path
  COMPILE_LOG        Optional explicit compile log path
  RUN_LOG            Optional explicit run log path
  TRIAGE_TXT         Optional explicit triage txt path
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
CODEX_TMUX_SESSION="${CODEX_TMUX_SESSION:-codex_memblock}"
SESSION_ID="${SESSION_ID:-}"
RESUME_WITH_LAST="${RESUME_WITH_LAST:-1}"

RESULT_JSON="${RESULT_JSON:-$RESULT_DIR/${TC}.json}"
COMPILE_LOG="${COMPILE_LOG:-$SIM_DIR/$MODE/log/vcs_compile_rtl.log}"
RUN_LOG="${RUN_LOG:-$(ls -1t "$SIM_DIR/$MODE/log/${TC}"*.log 2>/dev/null | head -n 1 || true)}"
TRIAGE_TXT="${TRIAGE_TXT:-$RESULT_DIR/${TC}.triage.txt}"

if [[ ! -f "$RESULT_JSON" ]]; then
    echo "result json not found: $RESULT_JSON" >&2
    exit 1
fi

if [[ ! -f "$TRIAGE_TXT" ]]; then
    echo "triage txt not found: $TRIAGE_TXT" >&2
    exit 1
fi

PROMPT="仿真任务已结束。请继续当前任务并基于结果自动判断下一步。
testcase: $TC
mode: $MODE
result_json: $RESULT_JSON
compile_log: $COMPILE_LOG
run_log: $RUN_LOG
triage_txt: $TRIAGE_TXT
请检查该任务是否已经完成；若失败，请继续修复环境、用例或验证逻辑直到成功；若确认为 DUT 逻辑问题，请输出分析文档。"

if tmux has-session -t "$CODEX_TMUX_SESSION" 2>/dev/null; then
    exec env \
        CODEX_TMUX_SESSION="$CODEX_TMUX_SESSION" \
        RESULT_JSON="$RESULT_JSON" \
        COMPILE_LOG="$COMPILE_LOG" \
        RUN_LOG="$RUN_LOG" \
        TRIAGE_TXT="$TRIAGE_TXT" \
        MEM_UT_SIM_DIR="$SIM_DIR" \
        bash "$SCRIPT_DIR/notify_current_session.sh" "$TC" "$MODE"
fi

if [[ -n "$SESSION_ID" ]]; then
    exec codex resume "$SESSION_ID" "$PROMPT"
fi

if [[ "$RESUME_WITH_LAST" == "1" ]]; then
    exec codex resume --last "$PROMPT"
fi

echo "No SESSION_ID provided and RESUME_WITH_LAST is disabled." >&2
echo "$PROMPT" >&2
exit 1
