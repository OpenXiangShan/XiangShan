#!/bin/bash

set -euo pipefail

usage() {
    cat <<'EOF'
Usage:
  notify_current_session.sh <testcase> [mode]

Environment:
  CODEX_TMUX_SESSION   tmux target session name, default: codex_memblock
  RESULT_JSON          explicit result json path
  COMPILE_LOG          explicit compile log path
  RUN_LOG              explicit run log path
  TRIAGE_TXT           explicit triage txt path
EOF
}

if [[ $# -lt 1 ]]; then
    usage
    exit 1
fi

TC="$1"
MODE="${2:-smoke}"
SIM_DIR="${MEM_UT_SIM_DIR:-/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/sim}"
RESULT_DIR="$SIM_DIR/$MODE/result"
TMUX_SESSION_NAME="${CODEX_TMUX_SESSION:-codex_memblock}"

RESULT_JSON="${RESULT_JSON:-$RESULT_DIR/${TC}.json}"
COMPILE_LOG="${COMPILE_LOG:-$SIM_DIR/$MODE/log/vcs_compile_rtl.log}"
RUN_LOG="${RUN_LOG:-$(ls -1t "$SIM_DIR/$MODE/log/${TC}"*.log 2>/dev/null | head -n 1 || true)}"
TRIAGE_TXT="${TRIAGE_TXT:-$RESULT_DIR/${TC}.triage.txt}"

if ! tmux has-session -t "$TMUX_SESSION_NAME" 2>/dev/null; then
    echo "tmux session not found: $TMUX_SESSION_NAME" >&2
    exit 1
fi

PROMPT=$(cat <<EOF
仿真任务已结束，请继续当前任务。
testcase: $TC
mode: $MODE
result_json: $RESULT_JSON
compile_log: $COMPILE_LOG
run_log: $RUN_LOG
triage_txt: $TRIAGE_TXT
请检查任务是否完成；若失败，请继续修复环境、用例或验证逻辑直到成功；若确认为 DUT 逻辑问题，请输出分析文档。
EOF
)

tmux send-keys -t "$TMUX_SESSION_NAME" Escape
sleep 0.1
tmux set-buffer -- "$PROMPT"
tmux paste-buffer -t "$TMUX_SESSION_NAME"
sleep 0.2
tmux send-keys -t "$TMUX_SESSION_NAME" Enter
sleep 0.1
tmux send-keys -t "$TMUX_SESSION_NAME" Enter
echo "sent prompt to tmux session: $TMUX_SESSION_NAME"
