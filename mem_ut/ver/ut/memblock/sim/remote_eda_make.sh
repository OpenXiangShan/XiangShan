#!/bin/bash

set -euo pipefail

usage() {
    cat <<'EOF'
Usage:
  remote_eda_make.sh sync  <make-target>
  remote_eda_make.sh async <make-target>
  remote_eda_make.sh status
  remote_eda_make.sh tail
  remote_eda_make.sh kill

Environment:
  REMOTE_HOST       Remote host, default: eda01
  REMOTE_WORKDIR    Shared sim directory on remote host
  REMOTE_ENTRY      Remote entry script path on shared filesystem
  REMOTE_PID_FILE   PID file for async run
  REMOTE_LAUNCH_LOG Launcher log for async run
  REMOTE_BOOTSTRAP  Optional shell fragment to setup remote EDA env
EOF
}

if [[ $# -lt 1 ]]; then
    usage
    exit 1
fi

ACTION="$1"
TARGET="${2:-}"

REMOTE_HOST="${REMOTE_HOST:-eda01}"
REMOTE_WORKDIR="${REMOTE_WORKDIR:-$(pwd)}"
REMOTE_ENTRY="${REMOTE_ENTRY:-$REMOTE_WORKDIR/eda01_entry.sh}"
REMOTE_PID_FILE="${REMOTE_PID_FILE:-$(pwd)/.eda_remote.pid}"
REMOTE_LAUNCH_LOG="${REMOTE_LAUNCH_LOG:-$(pwd)/.eda_remote.launch.log}"
REMOTE_BOOTSTRAP="${REMOTE_BOOTSTRAP:-}"

SSH_OPTS=(
    -o BatchMode=yes
    -o ControlMaster=no
)

FORWARD_VARS=(
    seed
    tc
    ts
    pl
    mode
    wave
    timing
    ccov
    fcov
    udr
    udf
    cfg
    plus_file
    plus_arg
    PLUS_ARG
    note
    timeout_ns
    simv_by_tc
    xprop
    partcmp_op
    initreg
    regr_ini
    time_mem_chk
    gui_on
    SIM_TOOLS
    MEMBLOCK_XS_HOME
    MEMBLOCK_PROJECT
)

build_make_assignments() {
    local -a result=()
    local name value quoted
    for name in "${FORWARD_VARS[@]}"; do
        value="${!name-}"
        if [[ ("$name" == "MEMBLOCK_XS_HOME" || "$name" == "MEMBLOCK_PROJECT") && -z "$value" ]]; then
            continue
        fi
        quoted="$(printf "%q" "$value")"
        result+=("${name}=${quoted}")
    done
    printf '%s\n' "${result[@]}"
}

remote_entry_cmd() {
    local make_target="$1"
    local -a lines cmd
    mapfile -t lines < <(build_make_assignments)
    cmd="$(printf "%q %q" "$REMOTE_ENTRY" "$make_target")"
    local item
    for item in "${lines[@]}"; do
        cmd+=" $(printf "%q" "$item")"
    done
    if [[ -n "$REMOTE_BOOTSTRAP" ]]; then
        printf '%s && %s' "$REMOTE_BOOTSTRAP" "$cmd"
        return
    fi
    printf '%s' "$cmd"
}

ssh_run() {
    local remote_cmd="$1"
    ssh "${SSH_OPTS[@]}" "$REMOTE_HOST" "$remote_cmd"
}

require_target() {
    if [[ -z "$TARGET" ]]; then
        echo "missing make target" >&2
        usage >&2
        exit 1
    fi
}

case "$ACTION" in
    sync)
        require_target
        ssh_run "$(remote_entry_cmd "$TARGET")"
        ;;
    async)
        require_target
        mkdir -p "$(dirname "$REMOTE_PID_FILE")"
        mkdir -p "$(dirname "$REMOTE_LAUNCH_LOG")"
        : >"$REMOTE_LAUNCH_LOG"
        ssh_run "cd $(printf '%q' "$REMOTE_WORKDIR") && nohup bash -c $(printf '%q' "$(remote_entry_cmd "$TARGET")") > $(printf '%q' "$REMOTE_LAUNCH_LOG") 2>&1 < /dev/null & echo \$! > $(printf '%q' "$REMOTE_PID_FILE") && cat $(printf '%q' "$REMOTE_PID_FILE")"
        ;;
    status)
        if [[ ! -f "$REMOTE_PID_FILE" ]]; then
            echo "no remote pid file: $REMOTE_PID_FILE"
            exit 1
        fi
        REMOTE_PID="$(cat "$REMOTE_PID_FILE")"
        ssh_run "if ps -p $(printf '%q' "$REMOTE_PID") >/dev/null 2>&1; then echo RUNNING pid=$(printf '%q' "$REMOTE_PID"); else echo EXITED pid=$(printf '%q' "$REMOTE_PID"); fi"
        ;;
    tail)
        if [[ ! -f "$REMOTE_LAUNCH_LOG" ]]; then
            echo "no launcher log: $REMOTE_LAUNCH_LOG"
            exit 1
        fi
        tail -n 50 "$REMOTE_LAUNCH_LOG"
        ;;
    kill)
        if [[ ! -f "$REMOTE_PID_FILE" ]]; then
            echo "no remote pid file: $REMOTE_PID_FILE"
            exit 1
        fi
        REMOTE_PID="$(cat "$REMOTE_PID_FILE")"
        ssh_run "kill $(printf '%q' "$REMOTE_PID")"
        ;;
    *)
        usage >&2
        exit 1
        ;;
esac
