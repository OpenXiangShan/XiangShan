#!/bin/bash

set -euo pipefail

TMUX_SESSION_NAME="${CODEX_TMUX_SESSION:-codex_memblock}"
PROJECT_DIR="${CODEX_PROJECT_DIR:-/nfs/home/lixiangrui/work/memblock_ut/XiangShan}"
CODEX_RESUME_CMD="${CODEX_RESUME_CMD:-codex resume --last --dangerously-bypass-approvals-and-sandbox -C /nfs/home/lixiangrui/work/memblock_ut/XiangShan}"

tmux_running() {
    tmux has-session -t "$TMUX_SESSION_NAME" 2>/dev/null
}

if tmux_running; then
    echo "tmux session already exists: $TMUX_SESSION_NAME"
    exit 0
fi

tmux new-session -d -s "$TMUX_SESSION_NAME"
tmux send-keys -t "$TMUX_SESSION_NAME" "cd $PROJECT_DIR" C-m
tmux send-keys -t "$TMUX_SESSION_NAME" "$CODEX_RESUME_CMD" C-m

echo "started tmux session: $TMUX_SESSION_NAME"
