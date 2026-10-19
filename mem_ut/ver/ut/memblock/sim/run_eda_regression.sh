#!/usr/bin/env bash
# Remote compile-once, multi-seed memblock regression controller.
#
# Common examples:
#   ./run_eda_regression.sh --tc basicTest --cfg default --count 100
#   ./run_eda_regression.sh --tc basicTest --cfg default --count 50 \
#     --compile --coverage --jobs 10
#   ./run_eda_regression.sh --tc basicTest --cfg default --seed 123456
#   ./run_eda_regression.sh --tc basicTest --cfg default --seeds 11,22,33
#   ./run_eda_regression.sh --tc basicTest --cfg default --seed 123456 --debug
#
# Run with --help for the full parameter list and defaults.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_PATH="$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
DEFAULT_REMOTE_HOST="eda01"
FALLBACK_REMOTE_HOST="172.28.10.101"

log_info() {
    printf '[EDA-REG] %s\n' "$*"
}

log_warn() {
    printf '[EDA-REG][WARN] %s\n' "$*" >&2
}

log_error() {
    printf '[EDA-REG][ERROR] %s\n' "$*" >&2
}

quote_cmd() {
    local out="" item
    for item in "$@"; do
        printf -v out '%s %q' "$out" "$item"
    done
    printf '%s' "${out# }"
}

atomic_write() {
    local path="$1"
    local tmp
    tmp="${path}.tmp.$$.$RANDOM"
    cat >"$tmp"
    mv -f "$tmp" "$path"
}

is_positive_integer() {
    [[ "$1" =~ ^[1-9][0-9]*$ ]]
}

valid_name() {
    [[ "$1" =~ ^[A-Za-z0-9_.-]+$ ]]
}

read_key() {
    local file="$1" key="$2"
    awk -F= -v key="$key" '$1 == key {sub(/^[^=]*=/, ""); print; exit}' "$file" 2>/dev/null
}

process_group_has_token() {
    local pgid="$1" token="$2"
    local current_uid row_uid row_pid row_pgid
    current_uid="$(id -u)"
    while read -r row_uid row_pid row_pgid; do
        [[ "$row_uid" == "$current_uid" ]] || continue
        [[ "$row_pgid" == "$pgid" ]] || continue
        [[ -r "/proc/$row_pid/environ" ]] || continue
        if tr '\0' '\n' <"/proc/$row_pid/environ" 2>/dev/null \
            | grep -Fqx "EDA_REGRESSION_RUN_TOKEN=$token"; then
            printf '%s\n' "$row_pid"
            return 0
        fi
    done < <(ps -e -o uid=,pid=,pgid= 2>/dev/null)
    return 1
}

process_group_has_live_member() {
    local pgid="$1" current_uid row_uid row_pgid stat
    current_uid="$(id -u)"
    while read -r row_uid row_pgid stat; do
        [[ "$row_uid" == "$current_uid" ]] || continue
        [[ "$row_pgid" == "$pgid" ]] || continue
        [[ "$stat" == Z* ]] && continue
        return 0
    done < <(ps -e -o uid=,pgid=,stat= 2>/dev/null)
    return 1
}

remote_discover_token() {
    local token="$1"
    local current_uid row_uid pid pgid sid
    current_uid="$(id -u)"
    declare -A seen=()
    while read -r row_uid pid pgid sid; do
        [[ "$row_uid" == "$current_uid" ]] || continue
        [[ "$pid" =~ ^[0-9]+$ && "$pgid" =~ ^[0-9]+$ && "$sid" =~ ^[0-9]+$ ]] || continue
        [[ -r "/proc/$pid/environ" ]] || continue
        if tr '\0' '\n' <"/proc/$pid/environ" 2>/dev/null \
            | grep -Fqx "EDA_REGRESSION_RUN_TOKEN=$token"; then
            if [[ -z "${seen[$pgid]+x}" ]]; then
                printf '%s\t%s\t%s\n' "$pid" "$pgid" "$sid"
                seen[$pgid]=1
            fi
        fi
    done < <(ps -e -o uid=,pid=,pgid=,sid= 2>/dev/null)
}

remote_probe_file() {
    local request_file="$1"
    local seed token pgid
    while IFS=$'\t' read -r seed token pgid; do
        [[ -n "$seed" ]] || continue
        if ! kill -0 -- "-$pgid" 2>/dev/null || ! process_group_has_live_member "$pgid"; then
            printf '%s\tEXITED\n' "$seed"
        elif process_group_has_token "$pgid" "$token" >/dev/null; then
            printf '%s\tALIVE\n' "$seed"
        else
            printf '%s\tIDENTITY_MISMATCH\n' "$seed"
        fi
    done <"$request_file"
}

remote_signal_file() {
    local request_file="$1" signal_name="$2"
    local seed token pgid
    while IFS=$'\t' read -r seed token pgid; do
        [[ -n "$seed" ]] || continue
        if ! kill -0 -- "-$pgid" 2>/dev/null || ! process_group_has_live_member "$pgid"; then
            printf '%s\tEXITED\n' "$seed"
        elif process_group_has_token "$pgid" "$token" >/dev/null; then
            if kill "-$signal_name" -- "-$pgid" 2>/dev/null; then
                printf '%s\tSIGNALED\n' "$seed"
            else
                printf '%s\tSIGNAL_FAILED\n' "$seed"
            fi
        else
            printf '%s\tIDENTITY_MISMATCH\n' "$seed"
        fi
    done <"$request_file"
}

remote_worker() {
    local ack_file="" rc_file="" token="" entry="$SCRIPT_DIR/eda01_entry.sh"
    local seed="" tc="" ts="" cfg="" mode="" pl="" wave="" ccov=""
    local note="" timeout_ns="" plus_file="../seq/plus_cfg" bootstrap=""
    local rc_written=0 worker_rc=125

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --ack) ack_file="$2"; shift 2 ;;
            --rc) rc_file="$2"; shift 2 ;;
            --token) token="$2"; shift 2 ;;
            --entry) entry="$2"; shift 2 ;;
            --seed) seed="$2"; shift 2 ;;
            --tc) tc="$2"; shift 2 ;;
            --ts) ts="$2"; shift 2 ;;
            --cfg) cfg="$2"; shift 2 ;;
            --mode) mode="$2"; shift 2 ;;
            --verbosity) pl="$2"; shift 2 ;;
            --wave) wave="$2"; shift 2 ;;
            --coverage) ccov="$2"; shift 2 ;;
            --note) note="$2"; shift 2 ;;
            --timeout-ns) timeout_ns="$2"; shift 2 ;;
            --plus-file) plus_file="$2"; shift 2 ;;
            --bootstrap) bootstrap="$2"; shift 2 ;;
            *) log_error "unknown remote worker argument: $1"; exit 2 ;;
        esac
    done

    if [[ -z "$ack_file" || -z "$rc_file" || -z "$token" || -z "$seed" \
        || -z "$tc" || -z "$ts" || -z "$cfg" || -z "$mode" ]]; then
        log_error "remote worker is missing required arguments"
        exit 2
    fi
    if [[ "${EDA_REGRESSION_RUN_TOKEN:-}" != "$token" ]]; then
        log_error "remote worker token is not present in the initial environment"
        exit 2
    fi

    mkdir -p "$(dirname "$ack_file")" "$(dirname "$rc_file")"
    local pid="$$" pgid sid
    pgid="$(ps -o pgid= -p "$$" | tr -d ' ')"
    sid="$(ps -o sid= -p "$$" | tr -d ' ')"
    if [[ "$pid" != "$pgid" || "$pid" != "$sid" ]]; then
        log_error "remote worker is not a process-group/session leader: pid=$pid pgid=$pgid sid=$sid"
        exit 2
    fi

    {
        printf 'token=%s\n' "$token"
        printf 'pid=%s\n' "$pid"
        printf 'pgid=%s\n' "$pgid"
        printf 'sid=%s\n' "$sid"
        printf 'seed=%s\n' "$seed"
        printf 'started_at=%s\n' "$(date '+%F %T %Z')"
    } | atomic_write "$ack_file"

    write_worker_rc() {
        [[ "$rc_written" -eq 0 ]] || return
        rc_written=1
        {
            printf 'rc=%s\n' "$worker_rc"
            printf 'ended_at=%s\n' "$(date '+%F %T %Z')"
        } | atomic_write "$rc_file"
    }
    trap write_worker_rc EXIT

    local effective_entry="${EDA_REGRESSION_REMOTE_ENTRY:-$entry}"
    local -a entry_args=(
        "$effective_entry" batch_run
        "seed=$seed" "tc=$tc" "ts=$ts" "cfg=$cfg" "mode=$mode"
        "pl=$pl" "wave=$wave" "ccov=$ccov" "fcov=off"
        "note=$note" "timeout_ns=$timeout_ns" "plus_file=$plus_file"
    )
    if [[ -n "$bootstrap" ]]; then
        bash -c "$bootstrap && exec \"\$@\"" bash "${entry_args[@]}"
    else
        "${entry_args[@]}"
    fi
    worker_rc=$?
    exit "$worker_rc"
}

if [[ "${1:-}" == "--remote-worker" ]]; then
    shift
    remote_worker "$@"
fi
if [[ "${1:-}" == "--remote-discover" ]]; then
    remote_discover_token "${2:?missing token}"
    exit 0
fi
if [[ "${1:-}" == "--remote-probe-file" ]]; then
    remote_probe_file "${2:?missing request file}"
    exit 0
fi
if [[ "${1:-}" == "--remote-signal-file" ]]; then
    remote_signal_file "${2:?missing request file}" "${3:?missing signal}"
    exit 0
fi

usage() {
    cat <<'EOF'
Usage:
  run_eda_regression.sh --tc <name> [options]

Required:
  --tc <name>                 Testcase name.

Test selection:
  --cfg <name>                Plus cfg name without .cfg (default: default).
  --ts <name>                 Virtual sequence (default: virtual_base_sequence).
  --mode <name>               Build/run mode (default: base_fun).
  --count <n>                 Generated-seed run count (default: 1).
  --seed <n>                  Explicit seed; may be repeated.
  --seeds <list>              Comma/space-separated explicit seeds.

Execution:
  --jobs <n>                  Maximum parallel simulations (default: 20).
  --compile / --no-compile    Compile once before simulation (default: no).
  --wave / --no-wave          Enable FSDB waveform (default: no).
  --coverage / --no-coverage  Enable VCS code coverage (default: no).
  --verbosity <level>         UVM_NONE/LOW/MEDIUM/HIGH/FULL/DEBUG
                              (default: UVM_LOW).
  --debug                     Defaults wave=on and verbosity=UVM_DEBUG unless
                              those options were explicitly set.
  --timeout-ns <n>            UVM timeout (default: 100000000).

Monitoring and records:
  --monitor / --no-monitor    Early-stop monitoring (default: on).
  --error-limit <n>           Per-seed combined UVM_ERROR/UVM_FATAL threshold
                              (default: 1).
  --record / --no-record      Dynamic progress/result files (default: on).

Infrastructure:
  --remote-host <host>        Remote EDA host (default: eda01; automatic
                              fallback to 172.28.10.101 for the default host).
  --output-dir <path>         Result directory (default: .eda_regression/run_id).
  --poll-interval <sec>       Controller poll interval (default: 1).
  --launch-timeout <sec>      Worker ACK timeout (default: 30).
  --probe-interval <sec>      Remote liveness probe interval (default: 5).
  --ssh-retries <n>           Probe/kill SSH retry count (default: 3).
  --dry-run                   Validate parameters and print seeds/config only.
  -h, --help                  Show this help.

Explicit seed mode overrides --count; duplicate seeds are removed while
preserving their first occurrence.
EOF
}

TC=""
CFG="default"
TS="virtual_base_sequence"
MODE="base_fun"
COUNT=1
JOBS=20
DO_COMPILE=0
WAVE_ON=0
COVERAGE_ON=0
VERBOSITY="UVM_LOW"
MONITOR_ON=1
ERROR_LIMIT=1
RECORD_ON=1
DEBUG_ON=0
TIMEOUT_NS=100000000
REMOTE_HOST="$DEFAULT_REMOTE_HOST"
OUTPUT_DIR=""
POLL_INTERVAL=1
LAUNCH_TIMEOUT=30
PROBE_INTERVAL=5
SSH_RETRIES=3
DRY_RUN=0
WAVE_EXPLICIT=0
VERBOSITY_EXPLICIT=0
COUNT_EXPLICIT=0
declare -a RAW_SEEDS=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        --tc) TC="${2:-}"; shift 2 ;;
        --cfg) CFG="${2:-}"; shift 2 ;;
        --ts) TS="${2:-}"; shift 2 ;;
        --mode) MODE="${2:-}"; shift 2 ;;
        --count) COUNT="${2:-}"; COUNT_EXPLICIT=1; shift 2 ;;
        --seed) RAW_SEEDS+=("${2:-}"); shift 2 ;;
        --seeds)
            seed_text="${2:-}"
            seed_text="${seed_text//,/ }"
            read -r -a seed_parts <<<"$seed_text"
            RAW_SEEDS+=("${seed_parts[@]}")
            shift 2
            ;;
        --jobs) JOBS="${2:-}"; shift 2 ;;
        --compile) DO_COMPILE=1; shift ;;
        --no-compile) DO_COMPILE=0; shift ;;
        --wave) WAVE_ON=1; WAVE_EXPLICIT=1; shift ;;
        --no-wave) WAVE_ON=0; WAVE_EXPLICIT=1; shift ;;
        --coverage) COVERAGE_ON=1; shift ;;
        --no-coverage) COVERAGE_ON=0; shift ;;
        --verbosity) VERBOSITY="${2:-}"; VERBOSITY_EXPLICIT=1; shift 2 ;;
        --monitor) MONITOR_ON=1; shift ;;
        --no-monitor) MONITOR_ON=0; shift ;;
        --error-limit) ERROR_LIMIT="${2:-}"; shift 2 ;;
        --record) RECORD_ON=1; shift ;;
        --no-record) RECORD_ON=0; shift ;;
        --debug) DEBUG_ON=1; shift ;;
        --timeout-ns) TIMEOUT_NS="${2:-}"; shift 2 ;;
        --remote-host) REMOTE_HOST="${2:-}"; shift 2 ;;
        --output-dir) OUTPUT_DIR="${2:-}"; shift 2 ;;
        --poll-interval) POLL_INTERVAL="${2:-}"; shift 2 ;;
        --launch-timeout) LAUNCH_TIMEOUT="${2:-}"; shift 2 ;;
        --probe-interval) PROBE_INTERVAL="${2:-}"; shift 2 ;;
        --ssh-retries) SSH_RETRIES="${2:-}"; shift 2 ;;
        --dry-run) DRY_RUN=1; shift ;;
        -h|--help) usage; exit 0 ;;
        *) log_error "unknown argument: $1"; usage >&2; exit 2 ;;
    esac
done

if [[ "$DEBUG_ON" -eq 1 ]]; then
    [[ "$WAVE_EXPLICIT" -eq 1 ]] || WAVE_ON=1
    [[ "$VERBOSITY_EXPLICIT" -eq 1 ]] || VERBOSITY="UVM_DEBUG"
fi

if [[ -z "$TC" ]]; then
    log_error "--tc is required"
    exit 2
fi
for value in "$TC" "$CFG" "$TS" "$MODE"; do
    if ! valid_name "$value"; then
        log_error "invalid name '$value'; allowed characters: A-Z a-z 0-9 _ . -"
        exit 2
    fi
done
for pair in \
    "count:$COUNT" "jobs:$JOBS" "error-limit:$ERROR_LIMIT" \
    "timeout-ns:$TIMEOUT_NS" "poll-interval:$POLL_INTERVAL" \
    "launch-timeout:$LAUNCH_TIMEOUT" "probe-interval:$PROBE_INTERVAL" \
    "ssh-retries:$SSH_RETRIES"; do
    name="${pair%%:*}"
    value="${pair#*:}"
    if ! is_positive_integer "$value"; then
        log_error "--$name must be a positive integer, got '$value'"
        exit 2
    fi
done
case "$VERBOSITY" in
    UVM_NONE|UVM_LOW|UVM_MEDIUM|UVM_HIGH|UVM_FULL|UVM_DEBUG) ;;
    *) log_error "invalid --verbosity '$VERBOSITY'"; exit 2 ;;
esac

declare -a SEEDS=()
declare -A SEED_SEEN=()
if [[ "${#RAW_SEEDS[@]}" -gt 0 ]]; then
    [[ "$COUNT_EXPLICIT" -eq 0 ]] || log_warn "--count ignored because explicit seeds were provided"
    for seed in "${RAW_SEEDS[@]}"; do
        if ! [[ "$seed" =~ ^[0-9]+$ ]] || (( seed < 1 || seed > 2147483646 )); then
            log_error "seed must be in 1..2147483646, got '$seed'"
            exit 2
        fi
        if [[ -z "${SEED_SEEN[$seed]+x}" ]]; then
            SEEDS+=("$seed")
            SEED_SEEN[$seed]=1
        fi
    done
else
    while [[ "${#SEEDS[@]}" -lt "$COUNT" ]]; do
        raw_random="$(od -An -N4 -tu4 /dev/urandom | tr -d ' ')"
        seed=$((raw_random % 2147483646 + 1))
        if [[ -z "${SEED_SEEN[$seed]+x}" ]]; then
            SEEDS+=("$seed")
            SEED_SEEN[$seed]=1
        fi
    done
fi

RUN_ID="${TC}_${CFG}_$(date '+%Y%m%d_%H%M%S')_$$_$RANDOM"
NOTE="eda_reg_$(date '+%Y%m%d_%H%M%S')_$$_$RANDOM"
if [[ -z "$OUTPUT_DIR" ]]; then
    OUTPUT_DIR="$SCRIPT_DIR/.eda_regression/$RUN_ID"
elif [[ "$OUTPUT_DIR" != /* ]]; then
    OUTPUT_DIR="$PWD/$OUTPUT_DIR"
fi
if [[ -d "$OUTPUT_DIR" ]] && find "$OUTPUT_DIR" -mindepth 1 -maxdepth 1 -print -quit | grep -q .; then
    log_error "output directory is not empty: $OUTPUT_DIR"
    exit 2
fi
mkdir -p "$OUTPUT_DIR"
OUTPUT_DIR="$(cd "$OUTPUT_DIR" && pwd)"
STATUS_DIR="$OUTPUT_DIR/status"
PID_DIR="$OUTPUT_DIR/pids"
LAUNCHER_DIR="$OUTPUT_DIR/launcher"
MONITOR_DIR="$OUTPUT_DIR/monitor"
mkdir -p "$STATUS_DIR" "$PID_DIR" "$LAUNCHER_DIR" "$MONITOR_DIR"

printf '%s\n' "${SEEDS[@]}" >"$OUTPUT_DIR/seeds.txt"
{
    printf 'run_id=%s\n' "$RUN_ID"
    printf 'tc=%s\n' "$TC"
    printf 'cfg=%s\n' "$CFG"
    printf 'ts=%s\n' "$TS"
    printf 'mode=%s\n' "$MODE"
    printf 'planned=%s\n' "${#SEEDS[@]}"
    printf 'jobs=%s\n' "$JOBS"
    printf 'compile=%s\n' "$DO_COMPILE"
    printf 'wave=%s\n' "$WAVE_ON"
    printf 'coverage=%s\n' "$COVERAGE_ON"
    printf 'verbosity=%s\n' "$VERBOSITY"
    printf 'monitor=%s\n' "$MONITOR_ON"
    printf 'error_limit=%s\n' "$ERROR_LIMIT"
    printf 'record=%s\n' "$RECORD_ON"
    printf 'timeout_ns=%s\n' "$TIMEOUT_NS"
    printf 'requested_remote_host=%s\n' "$REMOTE_HOST"
    printf 'note=%s\n' "$NOTE"
} >"$OUTPUT_DIR/config.txt"

if [[ "$DRY_RUN" -eq 1 ]]; then
    log_info "dry-run configuration:"
    cat "$OUTPUT_DIR/config.txt"
    log_info "seeds: ${SEEDS[*]}"
    log_info "result directory: $OUTPUT_DIR"
    exit 0
fi

SSH_OPTS=(-o BatchMode=yes -o ControlMaster=no -o ConnectTimeout=10)
ssh_once() {
    local remote_cmd="$1"
    ssh "${SSH_OPTS[@]}" "$FINAL_REMOTE_HOST" "$remote_cmd"
}

resolve_remote_host() {
    FINAL_REMOTE_HOST="$REMOTE_HOST"
    if ssh "${SSH_OPTS[@]}" "$FINAL_REMOTE_HOST" true >/dev/null 2>&1; then
        return 0
    fi
    if [[ "$REMOTE_HOST" == "$DEFAULT_REMOTE_HOST" ]] \
        && ssh "${SSH_OPTS[@]}" "$FALLBACK_REMOTE_HOST" true >/dev/null 2>&1; then
        FINAL_REMOTE_HOST="$FALLBACK_REMOTE_HOST"
        log_warn "remote host eda01 is unavailable; using $FALLBACK_REMOTE_HOST"
        return 0
    fi
    log_error "cannot connect to remote host '$REMOTE_HOST'"
    return 1
}

FINAL_REMOTE_HOST=""
if ! resolve_remote_host; then
    exit 3
fi
printf 'final_remote_host=%s\n' "$FINAL_REMOTE_HOST" >>"$OUTPUT_DIR/config.txt"

REMOTE_BOOTSTRAP="$(make -s -C "$SCRIPT_DIR" eda_print_bootstrap)"
if [[ -z "$REMOTE_BOOTSTRAP" ]]; then
    log_error "Makefile returned an empty REMOTE_BOOTSTRAP"
    exit 3
fi

LOCK_ROOT="$SCRIPT_DIR/.eda_regression/locks"
mkdir -p "$LOCK_ROOT"
LOCK_FILE="$LOCK_ROOT/$MODE.lock"
LOCK_INFO="$LOCK_ROOT/$MODE.owner"
QUARANTINE_FILE="$LOCK_ROOT/$MODE.quarantine"
if [[ -f "$QUARANTINE_FILE" ]]; then
    log_error "mode '$MODE' is quarantined: $QUARANTINE_FILE"
    cat "$QUARANTINE_FILE" >&2
    exit 4
fi
exec {LOCK_FD}>"$LOCK_FILE"
if ! flock -n "$LOCK_FD"; then
    log_error "mode '$MODE' is already in use"
    [[ -f "$LOCK_INFO" ]] && cat "$LOCK_INFO" >&2
    exit 4
fi
{
    printf 'host=%s\n' "$(hostname)"
    printf 'pid=%s\n' "$$"
    printf 'tc=%s\n' "$TC"
    printf 'cfg=%s\n' "$CFG"
    printf 'run_id=%s\n' "$RUN_ID"
    printf 'started_at=%s\n' "$(date '+%F %T %Z')"
} | atomic_write "$LOCK_INFO"

WAVE_VALUE="off"
CCOV_VALUE="off"
[[ "$WAVE_ON" -eq 0 ]] || WAVE_VALUE="on"
[[ "$COVERAGE_ON" -eq 0 ]] || CCOV_VALUE="on"
SIMV_PATH="$SCRIPT_DIR/$MODE/exec/simv"
FINGERPRINT_PATH="$SCRIPT_DIR/$MODE/exec/.eda_compile_fingerprint"
SIMV_BASELINE=""

simv_identity() {
    stat -Lc '%d:%i:%s:%Y:%Z' "$SIMV_PATH" 2>/dev/null
}

write_fingerprint() {
    local hash identity
    hash="$(sha256sum "$SIMV_PATH" | awk '{print $1}')"
    identity="$(simv_identity)"
    {
        printf 'mode=%s\n' "$MODE"
        printf 'timing=rtl\n'
        printf 'wave=%s\n' "$WAVE_VALUE"
        printf 'ccov=%s\n' "$CCOV_VALUE"
        printf 'simv=%s\n' "$SIMV_PATH"
        printf 'sha256=%s\n' "$hash"
        printf 'identity=%s\n' "$identity"
        printf 'git_commit=%s\n' "$(git -C "$SCRIPT_DIR" rev-parse HEAD 2>/dev/null || echo unknown)"
        printf 'created_at=%s\n' "$(date '+%F %T %Z')"
    } | atomic_write "$FINGERPRINT_PATH"
}

verify_fingerprint() {
    local fp_wave fp_ccov fp_hash fp_identity current_hash current_identity
    if [[ ! -f "$FINGERPRINT_PATH" ]]; then
        if [[ "$WAVE_ON" -eq 1 || "$COVERAGE_ON" -eq 1 ]]; then
            log_error "wave/coverage requires a matching compile fingerprint: $FINGERPRINT_PATH"
            return 1
        fi
        log_warn "using legacy simv without fingerprint; wave and coverage are disabled"
        return 0
    fi
    fp_wave="$(read_key "$FINGERPRINT_PATH" wave)"
    fp_ccov="$(read_key "$FINGERPRINT_PATH" ccov)"
    fp_hash="$(read_key "$FINGERPRINT_PATH" sha256)"
    fp_identity="$(read_key "$FINGERPRINT_PATH" identity)"
    if [[ "$WAVE_ON" -eq 1 && "$fp_wave" != "on" ]]; then
        log_error "existing simv was not fingerprinted with wave=on"
        return 1
    fi
    if [[ "$COVERAGE_ON" -eq 1 && "$fp_ccov" != "on" ]]; then
        log_error "existing simv was not fingerprinted with ccov=on"
        return 1
    fi
    current_identity="$(simv_identity)"
    current_hash="$(sha256sum "$SIMV_PATH" | awk '{print $1}')"
    if [[ "$current_identity" != "$fp_identity" || "$current_hash" != "$fp_hash" ]]; then
        log_error "simv does not match its compile fingerprint"
        return 1
    fi
}

if [[ "$DO_COMPILE" -eq 1 ]]; then
    rm -f "$FINGERPRINT_PATH"
    log_info "compile once on $FINAL_REMOTE_HOST (mode=$MODE wave=$WAVE_VALUE ccov=$CCOV_VALUE)"
    if ! make -C "$SCRIPT_DIR" eda_compile \
        REMOTE_HOST="$FINAL_REMOTE_HOST" \
        tc="$TC" ts="$TS" cfg="$CFG" mode="$MODE" seed="${SEEDS[0]}" \
        pl="$VERBOSITY" wave="$WAVE_VALUE" ccov="$CCOV_VALUE" fcov=off \
        note="${NOTE}_compile" timeout_ns="$TIMEOUT_NS" \
        >"$OUTPUT_DIR/compile.log" 2>&1; then
        log_error "compile failed; see $OUTPUT_DIR/compile.log"
        printf 'result=COMPILE_FAIL\n' >"$OUTPUT_DIR/summary.log"
        rm -f "$LOCK_INFO"
        exit 5
    fi
    if [[ ! -x "$SIMV_PATH" ]]; then
        log_error "compile succeeded but simv is missing: $SIMV_PATH"
        rm -f "$LOCK_INFO"
        exit 5
    fi
    write_fingerprint
else
    if [[ ! -x "$SIMV_PATH" ]]; then
        log_error "simv is missing; use --compile or select a compiled --mode: $SIMV_PATH"
        rm -f "$LOCK_INFO"
        exit 5
    fi
    if ! verify_fingerprint; then
        rm -f "$LOCK_INFO"
        exit 5
    fi
fi
SIMV_BASELINE="$(simv_identity)"

declare -A STATE=()
declare -A RC=()
declare -A LOG_PATH=()
declare -A CLASSIFY_LOG_PATH=()
declare -A TOKEN=()
declare -A PGID=()
declare -A WATCHER_PID=()
declare -A UVM_COUNT=()
declare -A FAILURE_REASON=()
STOP_REQUESTED=0
STOP_REASON=""
CLEANUP_DONE=0
REMOTE_UNKNOWN_SEEN=0
LAST_PROGRESS=""
LAST_PROBE_AT=0
STOPPING=0

for seed in "${SEEDS[@]}"; do
    STATE[$seed]="PENDING"
    printf 'PENDING\n' >"$STATUS_DIR/$seed.state"
done

set_state() {
    local seed="$1" state="$2" reason="${3:-}"
    STATE[$seed]="$state"
    [[ -z "$reason" ]] || FAILURE_REASON[$seed]="$reason"
    {
        printf 'state=%s\n' "$state"
        printf 'reason=%s\n' "$reason"
        printf 'updated_at=%s\n' "$(date '+%F %T %Z')"
    } | atomic_write "$STATUS_DIR/$seed.state"
}

is_terminal_state() {
    case "$1" in
        PASS|FAIL|INFRA|INCOMPLETE|ABORTED|REMOTE_UNKNOWN) return 0 ;;
        *) return 1 ;;
    esac
}

count_uvm_reports() {
    local log="$1"
    [[ -f "$log" ]] || { printf '0\n'; return; }
    grep -Ec '^[[:space:]]*UVM_(ERROR|FATAL)[[:space:]]+@' "$log" 2>/dev/null || true
}

prepare_classification_log() {
    local seed="$1" log snapshot cmd
    log="${LOG_PATH[$seed]}"
    if [[ -f "$log" ]]; then
        CLASSIFY_LOG_PATH[$seed]="$log"
        return 0
    fi
    snapshot="$STATUS_DIR/$seed.log.snapshot"
    cmd="cat $(printf '%q' "$log")"
    if ssh_retry "$cmd" >"$snapshot.tmp" 2>/dev/null; then
        mv -f "$snapshot.tmp" "$snapshot"
        CLASSIFY_LOG_PATH[$seed]="$snapshot"
        return 0
    fi
    rm -f "$snapshot.tmp"
    CLASSIFY_LOG_PATH[$seed]="$log"
    return 1
}

classify_seed() {
    local seed="$1" log rc
    log="${CLASSIFY_LOG_PATH[$seed]:-${LOG_PATH[$seed]}}"
    rc="${RC[$seed]:-125}"
    local actual_count error_summary fatal_summary
    if [[ ! -f "$log" ]]; then
        printf 'INFRA\n'
        return
    fi
    if grep -qiE 'License checkout failed|Unable to checkout|SNPS_VCS_INTERNAL_ERROR|Cannot find.*simv|No space left on device|Stale file handle' "$log"; then
        printf 'INFRA\n'
        return
    fi
    actual_count="$(count_uvm_reports "$log")"
    error_summary="$(awk '/^[[:space:]]*UVM_ERROR[[:space:]]*:/ {v=$NF} END {print v+0}' "$log")"
    fatal_summary="$(awk '/^[[:space:]]*UVM_FATAL[[:space:]]*:/ {v=$NF} END {print v+0}' "$log")"
    UVM_COUNT[$seed]="$actual_count"
    if (( actual_count > 0 || error_summary > 0 || fatal_summary > 0 )); then
        printf 'FAIL\n'
        return
    fi
    if [[ "$rc" -eq 0 ]] \
        && grep -qE 'TEST CASE PASSED|TEST_PASS' "$log" \
        && grep -qE '\$finish at simulation time|UVM Report Summary' "$log"; then
        printf 'PASS\n'
        return
    fi
    if [[ "$rc" -ne 0 ]]; then
        printf 'FAIL\n'
        return
    fi
    printf 'INCOMPLETE\n'
}

write_progress() {
    local final="${1:-0}"
    local planned="${#SEEDS[@]}" running=0 pass=0 fail=0 infra=0 incomplete=0
    local aborted=0 remote_unknown=0 pending=0 completed settled remaining seed state
    local -a success=() failed=() not_run=()
    for seed in "${SEEDS[@]}"; do
        state="${STATE[$seed]}"
        case "$state" in
            RUNNING) running=$((running + 1)) ;;
            PASS) pass=$((pass + 1)); success+=("$seed") ;;
            FAIL) fail=$((fail + 1)); failed+=("$seed") ;;
            INFRA) infra=$((infra + 1)); failed+=("$seed") ;;
            INCOMPLETE) incomplete=$((incomplete + 1)); failed+=("$seed") ;;
            ABORTED) aborted=$((aborted + 1)); failed+=("$seed") ;;
            REMOTE_UNKNOWN) remote_unknown=$((remote_unknown + 1)); failed+=("$seed") ;;
            PENDING) pending=$((pending + 1)); [[ "$final" -eq 0 ]] || not_run+=("$seed") ;;
        esac
    done
    completed=$((pass + fail))
    settled=$((pass + fail + infra + incomplete + aborted + remote_unknown))
    remaining=$((planned - completed))
    local progress="planned=$planned completed=$completed remaining=$remaining running=$running pass=$pass fail=$fail infra=$infra incomplete=$incomplete aborted=$aborted remote_unknown=$remote_unknown not_run=${#not_run[@]}"
    if [[ "$progress" != "$LAST_PROGRESS" || "$final" -eq 1 ]]; then
        log_info "$progress"
        LAST_PROGRESS="$progress"
    fi
    if [[ "$RECORD_ON" -eq 1 ]]; then
        {
            printf 'run_id=%s\n' "$RUN_ID"
            printf 'testcase=%s\n' "$TC"
            printf 'cfg=%s\n' "$CFG"
            printf 'mode=%s\n' "$MODE"
            printf '%s\n' "$progress"
            printf 'settled=%s\n' "$settled"
            printf 'success_seeds=%s\n' "${success[*]:-}"
            printf 'failed_seeds=%s\n' "${failed[*]:-}"
            printf 'updated_at=%s\n' "$(date '+%F %T %Z')"
        } | atomic_write "$OUTPUT_DIR/progress.log"
        printf '%s\n' "${success[@]}" >"$OUTPUT_DIR/success_seeds.txt"
        printf '%s\n' "${failed[@]}" >"$OUTPUT_DIR/failed_seeds.txt"
        [[ "$final" -eq 0 ]] || printf '%s\n' "${not_run[@]}" >"$OUTPUT_DIR/not_run_seeds.txt"
        {
            printf 'seed\tstate\trc\tuvm_count\tlog\treason\n'
            for seed in "${SEEDS[@]}"; do
                printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
                    "$seed" "${STATE[$seed]}" "${RC[$seed]:-}" "${UVM_COUNT[$seed]:-}" \
                    "${LOG_PATH[$seed]:-}" "${FAILURE_REASON[$seed]:-}"
            done
        } | atomic_write "$OUTPUT_DIR/results.tsv"
    fi
    if [[ "$final" -eq 1 ]]; then
        {
            printf 'run_id=%s\n' "$RUN_ID"
            printf 'testcase=%s\n' "$TC"
            printf 'cfg=%s\n' "$CFG"
            printf 'mode=%s\n' "$MODE"
            printf '%s\n' "$progress"
            printf 'settled=%s\n' "$settled"
            printf 'stop_reason=%s\n' "$STOP_REASON"
            printf 'result_dir=%s\n' "$OUTPUT_DIR"
            printf 'updated_at=%s\n' "$(date '+%F %T %Z')"
        } | atomic_write "$OUTPUT_DIR/summary.log"
    fi
}

create_quarantine() {
    local seed="$1" reason="$2"
    REMOTE_UNKNOWN_SEEN=1
    {
        printf 'mode=%s\n' "$MODE"
        printf 'host=%s\n' "$FINAL_REMOTE_HOST"
        printf 'run_id=%s\n' "$RUN_ID"
        printf 'seed=%s\n' "$seed"
        printf 'token=%s\n' "${TOKEN[$seed]:-unknown}"
        printf 'pgid=%s\n' "${PGID[$seed]:-unknown}"
        printf 'reason=%s\n' "$reason"
        printf 'created_at=%s\n' "$(date '+%F %T %Z')"
        printf 'action=Confirm remote processes are gone, then remove this file manually.\n'
    } | atomic_write "$QUARANTINE_FILE"
}

stop_watcher() {
    local seed="$1" watcher
    watcher="${WATCHER_PID[$seed]:-}"
    if [[ -n "$watcher" ]]; then
        kill "$watcher" 2>/dev/null || true
        wait "$watcher" 2>/dev/null || true
        unset 'WATCHER_PID[$seed]'
    fi
}

start_watcher() {
    local seed="$1" log="$2" trigger
    trigger="$MONITOR_DIR/$seed.trigger"
    rm -f "$trigger"
    (
        local count=0 line tmp tail_pid
        local fifo="$MONITOR_DIR/$seed.fifo.$$.$RANDOM"
        mkfifo "$fifo"
        tail -n +1 --retry -F "$log" >"$fifo" 2>/dev/null &
        tail_pid=$!
        watcher_cleanup() {
            kill "$tail_pid" 2>/dev/null || true
            wait "$tail_pid" 2>/dev/null || true
            rm -f "$fifo"
        }
        trap watcher_cleanup EXIT INT TERM
        while IFS= read -r line; do
            if [[ "$line" =~ ^[[:space:]]*UVM_(ERROR|FATAL)[[:space:]]+@ ]]; then
                count=$((count + 1))
                if (( count >= ERROR_LIMIT )); then
                    tmp="${trigger}.tmp.$$.$RANDOM"
                    {
                        printf 'seed=%s\n' "$seed"
                        printf 'count=%s\n' "$count"
                        printf 'line=%s\n' "$line"
                        printf 'triggered_at=%s\n' "$(date '+%F %T %Z')"
                    } >"$tmp"
                    mv -f "$tmp" "$trigger"
                    break
                fi
            fi
        done <"$fifo"
    ) &
    WATCHER_PID[$seed]=$!
}

ssh_retry() {
    local remote_cmd="$1" attempt
    for ((attempt = 1; attempt <= SSH_RETRIES; attempt++)); do
        if ssh_once "$remote_cmd"; then
            return 0
        fi
        (( attempt == SSH_RETRIES )) || sleep 1
    done
    return 1
}

discover_after_ack_timeout() {
    local seed="$1" token="$2" output rc unique_pgids=0 first_pid="" first_pgid="" first_sid=""
    local cmd
    cmd="$(quote_cmd "$SCRIPT_PATH" --remote-discover "$token")"
    output="$(ssh_retry "$cmd")"
    rc=$?
    if [[ "$rc" -ne 0 ]]; then
        set_state "$seed" REMOTE_UNKNOWN "ACK timeout and remote token discovery failed"
        create_quarantine "$seed" "ACK timeout and remote token discovery failed"
        return 2
    fi
    declare -A seen_pgids=()
    local pid pgid sid
    while IFS=$'\t' read -r pid pgid sid; do
        [[ -n "$pgid" ]] || continue
        if [[ -z "${seen_pgids[$pgid]+x}" ]]; then
            seen_pgids[$pgid]=1
            unique_pgids=$((unique_pgids + 1))
            first_pid="$pid"
            first_pgid="$pgid"
            first_sid="$sid"
        fi
    done <<<"$output"
    if [[ "$unique_pgids" -eq 0 ]]; then
        set_state "$seed" INFRA "worker ACK timeout; no remote token process found"
        return 1
    fi
    if [[ "$unique_pgids" -ne 1 ]]; then
        set_state "$seed" REMOTE_UNKNOWN "worker ACK timeout; multiple token process groups found"
        create_quarantine "$seed" "multiple token process groups found after ACK timeout"
        return 2
    fi
    PGID[$seed]="$first_pgid"
    {
        printf 'token=%s\n' "$token"
        printf 'pid=%s\n' "$first_pgid"
        printf 'pgid=%s\n' "$first_pgid"
        printf 'sid=%s\n' "$first_sid"
        printf 'seed=%s\n' "$seed"
        printf 'recovered_from_discovery=1\n'
    } | atomic_write "$STATUS_DIR/$seed.ack"
    printf '%s\n' "$first_pgid" >"$PID_DIR/$seed.pid"
    log_warn "seed $seed ACK was recovered from token discovery (pgid=$first_pgid, first_pid=$first_pid)"
    return 0
}

recover_ack_over_ssh() {
    local ack="$1" recovered cmd
    recovered="${ack}.recovered.$$.$RANDOM"
    cmd="cat $(printf '%q' "$ack")"
    if ssh_retry "$cmd" >"$recovered" 2>/dev/null && grep -q '^token=' "$recovered"; then
        mv -f "$recovered" "$ack"
        return 0
    fi
    rm -f "$recovered"
    return 1
}

recover_rc_over_ssh() {
    local seed="$1" rc_file="$STATUS_DIR/$seed.rc" recovered cmd
    [[ -f "$rc_file" ]] && return 0
    recovered="${rc_file}.recovered.$$.$RANDOM"
    cmd="cat $(printf '%q' "$rc_file")"
    if ssh_retry "$cmd" >"$recovered" 2>/dev/null && grep -q '^rc=' "$recovered"; then
        mv -f "$recovered" "$rc_file"
        return 0
    fi
    rm -f "$recovered"
    return 1
}

validate_ack() {
    local seed="$1" expected_token="$2" ack
    ack="$STATUS_DIR/$seed.ack"
    local token pid pgid sid
    token="$(read_key "$ack" token)"
    pid="$(read_key "$ack" pid)"
    pgid="$(read_key "$ack" pgid)"
    sid="$(read_key "$ack" sid)"
    if [[ "$token" != "$expected_token" || ! "$pid" =~ ^[0-9]+$ \
        || "$pid" != "$pgid" || "$pid" != "$sid" ]]; then
        return 1
    fi
    PGID[$seed]="$pgid"
    printf '%s\n' "$pgid" >"$PID_DIR/$seed.pid"
    return 0
}

check_simv_unchanged() {
    local current
    current="$(simv_identity)"
    [[ -n "$current" && "$current" == "$SIMV_BASELINE" ]]
}

launch_seed() {
    local seed="$1"
    if ! check_simv_unchanged; then
        set_state "$seed" INFRA "simv changed after batch baseline was established"
        STOP_REQUESTED=1
        STOP_REASON="simv changed during batch"
        return 1
    fi
    local token="${RUN_ID}_${seed}_$RANDOM$RANDOM"
    local ack="$STATUS_DIR/$seed.ack" rc_file="$STATUS_DIR/$seed.rc"
    local launcher="$LAUNCHER_DIR/$seed.log"
    local log="$SCRIPT_DIR/$MODE/log/tc=${TC}_ts=${TS}_cfg=${CFG}_seed=${seed}_rtl_${NOTE}.log"
    local entry="$SCRIPT_DIR/eda01_entry.sh"
    local -a worker_args=(
        "$SCRIPT_PATH" --remote-worker
        --ack "$ack" --rc "$rc_file" --token "$token" --entry "$entry"
        --seed "$seed" --tc "$TC" --ts "$TS" --cfg "$CFG" --mode "$MODE"
        --verbosity "$VERBOSITY" --wave "$WAVE_VALUE" --coverage "$CCOV_VALUE"
        --note "$NOTE" --timeout-ns "$TIMEOUT_NS" --plus-file ../seq/plus_cfg
        --bootstrap "$REMOTE_BOOTSTRAP"
    )
    local worker_cmd env_cmd remote_cmd launch_rc deadline
    worker_cmd="$(quote_cmd "${worker_args[@]}")"
    env_cmd="$(quote_cmd "EDA_REGRESSION_RUN_TOKEN=$token")"
    if [[ -n "${EDA_REGRESSION_REMOTE_ENTRY:-}" ]]; then
        env_cmd+=" $(quote_cmd "EDA_REGRESSION_REMOTE_ENTRY=$EDA_REGRESSION_REMOTE_ENTRY")"
    fi
    remote_cmd="exec nohup env $env_cmd setsid $worker_cmd > $(printf '%q' "$launcher") 2>&1 < /dev/null"

    TOKEN[$seed]="$token"
    LOG_PATH[$seed]="$log"
    rm -f "$ack" "$rc_file" "$launcher" "$MONITOR_DIR/$seed.trigger"
    if [[ -e "$log" ]]; then
        set_state "$seed" INFRA "unique simulation log already exists: $log"
        return 1
    fi
    set_state "$seed" RUNNING
    ssh "${SSH_OPTS[@]}" -f -n "$FINAL_REMOTE_HOST" "$remote_cmd" >/dev/null 2>&1
    launch_rc=$?
    [[ "$launch_rc" -eq 0 ]] || log_warn "launch SSH returned rc=$launch_rc for seed $seed; waiting for token ACK"

    deadline=$((SECONDS + LAUNCH_TIMEOUT))
    while [[ ! -f "$ack" && "$SECONDS" -lt "$deadline" ]]; do
        sleep 1
    done
    if [[ ! -f "$ack" ]]; then
        recover_ack_over_ssh "$ack" || true
    fi
    if [[ ! -f "$ack" ]]; then
        discover_after_ack_timeout "$seed" "$token"
        case $? in
            0) ;;
            *) return 1 ;;
        esac
    fi
    if ! validate_ack "$seed" "$token"; then
        set_state "$seed" REMOTE_UNKNOWN "invalid worker ACK"
        create_quarantine "$seed" "invalid worker ACK"
        return 1
    fi
    if [[ "$MONITOR_ON" -eq 1 ]]; then
        start_watcher "$seed" "$log"
    fi
    log_info "launched seed=$seed pgid=${PGID[$seed]} log=$log"
    return 0
}

retire_completed_workers() {
    local seed rc_file rc_value state actual_count
    for seed in "${SEEDS[@]}"; do
        [[ "${STATE[$seed]}" == "RUNNING" ]] || continue
        rc_file="$STATUS_DIR/$seed.rc"
        [[ -f "$rc_file" ]] || continue
        rc_value="$(read_key "$rc_file" rc)"
        [[ "$rc_value" =~ ^[0-9]+$ ]] || rc_value=125
        RC[$seed]="$rc_value"
        prepare_classification_log "$seed" || true
        if [[ "$MONITOR_ON" -eq 1 ]]; then
            actual_count="$(count_uvm_reports "${CLASSIFY_LOG_PATH[$seed]:-${LOG_PATH[$seed]}}")"
            UVM_COUNT[$seed]="$actual_count"
            if (( actual_count >= ERROR_LIMIT )); then
                STOP_REQUESTED=1
                STOP_REASON="seed $seed reached UVM error limit $ERROR_LIMIT"
            fi
        fi
        stop_watcher "$seed"
        state="$(classify_seed "$seed")"
        set_state "$seed" "$state" "worker rc=$rc_value"
        unset 'PGID[$seed]'
        log_info "completed seed=$seed state=$state rc=$rc_value"
    done
}

build_active_request() {
    local request="$1" seed
    : >"$request"
    for seed in "${SEEDS[@]}"; do
        [[ "${STATE[$seed]}" == "RUNNING" ]] || continue
        [[ -n "${PGID[$seed]:-}" ]] || continue
        printf '%s\t%s\t%s\n' "$seed" "${TOKEN[$seed]}" "${PGID[$seed]}" >>"$request"
    done
}

probe_active_workers() {
    local request="$OUTPUT_DIR/probe_request.tsv" output="$OUTPUT_DIR/probe_output.tsv"
    local cmd attempt seed status
    build_active_request "$request"
    [[ -s "$request" ]] || return 0
    cmd="$(quote_cmd "$SCRIPT_PATH" --remote-probe-file "$request")"
    for ((attempt = 1; attempt <= SSH_RETRIES; attempt++)); do
        if ssh_once "$cmd" >"$output.tmp" 2>/dev/null; then
            mv -f "$output.tmp" "$output"
            while IFS=$'\t' read -r seed status; do
                [[ "${STATE[$seed]:-}" == "RUNNING" ]] || continue
                case "$status" in
                    ALIVE) ;;
                    EXITED)
                        stop_watcher "$seed"
                        if recover_rc_over_ssh "$seed"; then
                            unset 'PGID[$seed]'
                        elif [[ "$STOPPING" -eq 1 ]]; then
                            set_state "$seed" ABORTED "$STOP_REASON"
                            unset 'PGID[$seed]'
                        else
                            set_state "$seed" INFRA "remote process group exited without rc"
                            unset 'PGID[$seed]'
                        fi
                        ;;
                    IDENTITY_MISMATCH)
                        stop_watcher "$seed"
                        set_state "$seed" REMOTE_UNKNOWN "remote process group token mismatch"
                        create_quarantine "$seed" "remote process group token mismatch"
                        unset 'PGID[$seed]'
                        STOP_REQUESTED=1
                        STOP_REASON="remote worker identity mismatch"
                        ;;
                esac
            done <"$output"
            return 0
        fi
        (( attempt == SSH_RETRIES )) || sleep 1
    done
    for seed in "${SEEDS[@]}"; do
        [[ "${STATE[$seed]}" == "RUNNING" ]] || continue
        stop_watcher "$seed"
        set_state "$seed" REMOTE_UNKNOWN "remote probe failed after $SSH_RETRIES attempts"
        create_quarantine "$seed" "remote probe failed after $SSH_RETRIES attempts"
        unset 'PGID[$seed]'
    done
    STOP_REQUESTED=1
    STOP_REASON="remote probe failed"
    return 1
}

has_monitor_trigger() {
    local seed
    [[ "$MONITOR_ON" -eq 1 ]] || return 1
    for seed in "${SEEDS[@]}"; do
        if [[ -f "$MONITOR_DIR/$seed.trigger" ]]; then
            STOP_REASON="seed $seed reached UVM error limit $ERROR_LIMIT"
            return 0
        fi
    done
    return 1
}

signal_active_groups() {
    local signal_name="$1" request output cmd attempt seed status
    request="$OUTPUT_DIR/signal_${signal_name}.tsv"
    output="$OUTPUT_DIR/signal_${signal_name}.out"
    build_active_request "$request"
    [[ -s "$request" ]] || return 0
    cmd="$(quote_cmd "$SCRIPT_PATH" --remote-signal-file "$request" "$signal_name")"
    for ((attempt = 1; attempt <= SSH_RETRIES; attempt++)); do
        if ssh_once "$cmd" >"$output.tmp" 2>/dev/null; then
            mv -f "$output.tmp" "$output"
            while IFS=$'\t' read -r seed status; do
                case "$status" in
                    IDENTITY_MISMATCH|SIGNAL_FAILED)
                        [[ "${STATE[$seed]:-}" == "RUNNING" ]] || continue
                        set_state "$seed" REMOTE_UNKNOWN "remote $signal_name failed: $status"
                        create_quarantine "$seed" "remote $signal_name failed: $status"
                        unset 'PGID[$seed]'
                        ;;
                esac
            done <"$output"
            return 0
        fi
        (( attempt == SSH_RETRIES )) || sleep 1
    done
    for seed in "${SEEDS[@]}"; do
        [[ "${STATE[$seed]}" == "RUNNING" ]] || continue
        set_state "$seed" REMOTE_UNKNOWN "remote $signal_name SSH failed"
        create_quarantine "$seed" "remote $signal_name SSH failed"
        unset 'PGID[$seed]'
    done
    return 1
}

stop_all_workers() {
    local seed
    STOPPING=1
    signal_active_groups TERM || true
    sleep 2
    probe_active_workers || true
    signal_active_groups KILL || true
    sleep 1
    probe_active_workers || true
    for seed in "${SEEDS[@]}"; do
        stop_watcher "$seed"
        if [[ "${STATE[$seed]}" == "RUNNING" ]]; then
            if [[ -n "${PGID[$seed]:-}" ]]; then
                set_state "$seed" REMOTE_UNKNOWN "could not confirm remote process group exit"
                create_quarantine "$seed" "could not confirm remote process group exit"
                unset 'PGID[$seed]'
            else
                set_state "$seed" ABORTED "$STOP_REASON"
            fi
        fi
    done
}

cleanup_controller() {
    local exit_rc=$?
    [[ "$CLEANUP_DONE" -eq 0 ]] || return
    CLEANUP_DONE=1
    local seed
    for seed in "${SEEDS[@]}"; do
        stop_watcher "$seed"
    done
    local active_found=0
    for seed in "${SEEDS[@]}"; do
        [[ "${STATE[$seed]:-}" == "RUNNING" ]] || continue
        active_found=1
        break
    done
    if [[ "$active_found" -eq 1 ]]; then
        STOP_REASON="${STOP_REASON:-controller exit rc=$exit_rc}"
        stop_all_workers
    fi
    rm -f "$LOCK_INFO"
    flock -u "$LOCK_FD" 2>/dev/null || true
}
trap cleanup_controller EXIT
trap 'STOP_REQUESTED=1; STOP_REASON="user interrupt"' INT TERM

pending_exists() {
    local seed
    for seed in "${SEEDS[@]}"; do
        [[ "${STATE[$seed]}" == "PENDING" ]] && return 0
    done
    return 1
}

running_exists() {
    local seed
    for seed in "${SEEDS[@]}"; do
        [[ "${STATE[$seed]}" == "RUNNING" ]] && return 0
    done
    return 1
}

running_count() {
    local count=0 seed
    for seed in "${SEEDS[@]}"; do
        [[ "${STATE[$seed]}" == "RUNNING" ]] && count=$((count + 1))
    done
    printf '%s\n' "$count"
}

write_progress 0
while running_exists || { [[ "$STOP_REQUESTED" -eq 0 ]] && pending_exists; }; do
    if has_monitor_trigger; then
        STOP_REQUESTED=1
    fi

    retire_completed_workers

    now=$SECONDS
    if (( now - LAST_PROBE_AT >= PROBE_INTERVAL )); then
        probe_active_workers || true
        LAST_PROBE_AT=$now
    fi

    if has_monitor_trigger; then
        STOP_REQUESTED=1
    fi
    if [[ "$REMOTE_UNKNOWN_SEEN" -eq 1 ]]; then
        STOP_REQUESTED=1
        [[ -n "$STOP_REASON" ]] || STOP_REASON="remote worker status is unknown"
    fi

    if [[ "$STOP_REQUESTED" -eq 1 ]]; then
        stop_all_workers
    else
        while [[ "$STOP_REQUESTED" -eq 0 ]] && (( $(running_count) < JOBS )) && pending_exists; do
            if has_monitor_trigger; then
                STOP_REQUESTED=1
                break
            fi
            next_seed=""
            for seed in "${SEEDS[@]}"; do
                if [[ "${STATE[$seed]}" == "PENDING" ]]; then
                    next_seed="$seed"
                    break
                fi
            done
            [[ -n "$next_seed" ]] || break
            launch_seed "$next_seed" || true
            if [[ "$REMOTE_UNKNOWN_SEEN" -eq 1 ]]; then
                STOP_REQUESTED=1
                STOP_REASON="remote launch status is unknown"
                break
            fi
        done
    fi

    write_progress 0
    if [[ "$STOP_REQUESTED" -eq 1 ]] && ! running_exists; then
        break
    fi
    sleep "$POLL_INTERVAL"
done

retire_completed_workers
if ! check_simv_unchanged; then
    log_warn "simv changed before final batch check"
    [[ -n "$STOP_REASON" ]] || STOP_REASON="simv changed during batch"
    SIMV_CHANGED=1
else
    SIMV_CHANGED=0
fi
write_progress 1

all_pass=1
[[ "$SIMV_CHANGED" -eq 0 ]] || all_pass=0
for seed in "${SEEDS[@]}"; do
    if [[ "${STATE[$seed]}" != "PASS" ]]; then
        all_pass=0
        break
    fi
done

CLEANUP_DONE=1
rm -f "$LOCK_INFO"
flock -u "$LOCK_FD" 2>/dev/null || true
trap - EXIT

log_info "summary: $OUTPUT_DIR/summary.log"
if [[ "$all_pass" -eq 1 ]]; then
    exit 0
fi
exit 1
