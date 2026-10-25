#!/usr/bin/env bash
# Remote compile-once, multi-seed memblock regression controller.
#
# Common examples:
#   ./run_eda_regression.sh --tc basicTest --cfg default --count 100
#   ./run_eda_regression.sh --tc basicTest --cfg default --count 50 \
#     --compile --coverage --jobs 10
#   ./run_eda_regression.sh --tc basicTest --cfg default --seed 123456
#   ./run_eda_regression.sh --tc basicTest --cfg default --seeds 11,22,33
#   ./run_eda_regression.sh --tc basicTest --cfg default \
#     --plusarg +MEMBLOCK_MAIN_TRANS_NUM=1000
#   ./run_eda_regression.sh --tc basicTest --cfg default --seed 123456 --debug
#
# Explicit seed mode ignores --count for scheduling, but every supplied option
# is still validated. Run with --help for all parameters and defaults.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_PATH="$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
SIM_DIR="$(cd "$SCRIPT_DIR/../sim" && pwd)"
DEFAULT_REMOTE_HOST="eda01"
FALLBACK_REMOTE_HOST="172.28.10.101"
JOB_PROTOCOL="EDA_REGRESSION_JOB_V1"
RPC_PROTOCOL="EDA_REGRESSION_RPC_V1"

log_info() { printf '[EDA-REG] %s\n' "$*"; }
log_warn() { printf '[EDA-REG][WARN] %s\n' "$*" >&2; }
log_error() { printf '[EDA-REG][ERROR] %s\n' "$*" >&2; }

quote_cmd() {
    local out="" item
    for item in "$@"; do
        printf -v out '%s %q' "$out" "$item"
    done
    printf '%s' "${out# }"
}

atomic_write() {
    local path="$1" tmp
    tmp="${path}.tmp.$$.$RANDOM"
    if ! cat >"$tmp"; then
        rm -f "$tmp"
        return 1
    fi
    if ! mv -fT "$tmp" "$path"; then
        rm -f "$tmp"
        return 1
    fi
}

read_key() {
    local file="$1" key="$2"
    awk -F= -v key="$key" '$1 == key {sub(/^[^=]*=/, ""); print; exit}' "$file" 2>/dev/null
}

valid_name() { [[ "$1" =~ ^[A-Za-z0-9_.-]+$ ]]; }
valid_job_id() { [[ "$1" =~ ^(compile|seed:[1-9][0-9]*)$ ]]; }
valid_token() { [[ "$1" =~ ^[A-Za-z0-9_.:-]+$ ]]; }

require_option_value() {
    local option="$1" remaining="$2" value="${3-}"
    if [[ "$remaining" -lt 2 || -z "$value" || "$value" == --* ]]; then
        log_error "$option requires a non-empty value"
        return 2
    fi
}

close_inherited_lock_fd() {
    if [[ -n "${LOCK_FD:-}" ]]; then
        eval "exec ${LOCK_FD}>&-" 2>/dev/null || true
    fi
}

process_group_has_token() {
    local pgid="$1" token="$2" current_uid row_uid row_pid row_pgid
    current_uid="$(id -u)"
    while read -r row_uid row_pid row_pgid; do
        [[ "$row_uid" == "$current_uid" && "$row_pgid" == "$pgid" ]] || continue
        [[ -r "/proc/$row_pid/environ" ]] || continue
        if tr '\0' '\n' <"/proc/$row_pid/environ" 2>/dev/null \
            | grep -Fqx "EDA_REGRESSION_RUN_TOKEN=$token"; then
            return 0
        fi
    done < <(ps -e -o uid=,pid=,pgid= 2>/dev/null)
    return 1
}

process_group_has_live_member() {
    local pgid="$1" current_uid row_uid row_pgid stat
    current_uid="$(id -u)"
    while read -r row_uid row_pgid stat; do
        [[ "$row_uid" == "$current_uid" && "$row_pgid" == "$pgid" ]] || continue
        [[ "$stat" == Z* ]] && continue
        return 0
    done < <(ps -e -o uid=,pgid=,stat= 2>/dev/null)
    return 1
}

remote_discover_token() {
    local token="$1" current_uid row_uid pid pgid sid
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

remote_rpc() {
    local request tmp_items tmp_response line protocol request_id op count expected_hash end_id
    local actual_hash item_count=0 job_id token pgid extra status signal_name=""
    request="$(mktemp)" || exit 2
    tmp_items="$(mktemp)" || { rm -f "$request"; exit 2; }
    tmp_response="$(mktemp)" || { rm -f "$request" "$tmp_items"; exit 2; }
    trap "rm -f $(printf '%q' "$request") $(printf '%q' "$tmp_items") $(printf '%q' "$tmp_response")" EXIT
    cat >"$request"

    exec 3<"$request"
    IFS= read -r line <&3 || exit 2
    [[ "$line" == "PROTO $RPC_PROTOCOL" ]] || exit 2
    protocol="$line"
    IFS= read -r line <&3 || exit 2
    [[ "$line" == REQUEST_ID\ * ]] || exit 2
    request_id="${line#REQUEST_ID }"
    valid_token "$request_id" || exit 2
    IFS= read -r line <&3 || exit 2
    [[ "$line" == OP\ * ]] || exit 2
    op="${line#OP }"
    case "$op" in
        PROBE) ;;
        SIGNAL_TERM) signal_name="TERM" ;;
        SIGNAL_KILL) signal_name="KILL" ;;
        *) exit 2 ;;
    esac
    IFS= read -r line <&3 || exit 2
    [[ "$line" == COUNT\ * ]] || exit 2
    count="${line#COUNT }"
    [[ "$count" =~ ^[0-9]+$ ]] || exit 2
    IFS= read -r line <&3 || exit 2
    [[ "$line" == HASH\ * ]] || exit 2
    expected_hash="${line#HASH }"
    [[ "$expected_hash" =~ ^[0-9a-f]{64}$ ]] || exit 2

    declare -A seen=()
    while IFS= read -r line <&3; do
        if [[ "$line" == END\ * ]]; then
            end_id="${line#END }"
            break
        fi
        IFS=$'\t' read -r protocol job_id token pgid extra <<<"$line"
        [[ "$protocol" == "ITEM" && -z "${extra:-}" ]] || exit 2
        valid_job_id "$job_id" && valid_token "$token" && [[ "$pgid" =~ ^[1-9][0-9]*$ ]] || exit 2
        [[ -z "${seen[$job_id]+x}" ]] || exit 2
        seen[$job_id]=1
        printf 'ITEM\t%s\t%s\t%s\n' "$job_id" "$token" "$pgid" >>"$tmp_items"
        item_count=$((item_count + 1))
    done
    [[ "${end_id:-}" == "$request_id" ]] || exit 2
    IFS= read -r line <&3 && exit 2
    exec 3<&-
    [[ "$item_count" -eq "$count" ]] || exit 2
    actual_hash="$(sha256sum "$tmp_items" | awk '{print $1}')"
    [[ "$actual_hash" == "$expected_hash" ]] || exit 2

    while IFS=$'\t' read -r protocol job_id token pgid; do
        if ! kill -0 -- "-$pgid" 2>/dev/null || ! process_group_has_live_member "$pgid"; then
            status="EXITED"
        elif ! process_group_has_token "$pgid" "$token"; then
            status="IDENTITY_MISMATCH"
        elif [[ "$op" == "PROBE" ]]; then
            status="ALIVE"
        elif kill "-$signal_name" -- "-$pgid" 2>/dev/null; then
            status="SIGNALED"
        else
            status="SIGNAL_FAILED"
        fi
        printf 'ITEM\t%s\t%s\n' "$job_id" "$status" >>"$tmp_response"
    done <"$tmp_items"

    actual_hash="$(sha256sum "$tmp_response" | awk '{print $1}')"
    printf 'PROTO %s\n' "$RPC_PROTOCOL"
    printf 'REQUEST_ID %s\n' "$request_id"
    printf 'OP %s\n' "$op"
    printf 'COUNT %s\n' "$count"
    printf 'HASH %s\n' "$actual_hash"
    cat "$tmp_response"
    printf 'END %s\n' "$request_id"
}

validate_remote_plusarg() {
    local token="$1" body key value
    [[ "$token" == +*=* && "$token" != *"="*"="* ]] || return 1
    body="${token#+}"
    key="${body%%=*}"
    value="${body#*=}"
    [[ "$key" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]] || return 1
    [[ "$value" =~ ^[A-Za-z0-9_./,:@%+-]+$ ]] || return 1
}

remote_worker() {
    local ack_file="" rc_file="" token="" job_id="" operation=""
    local entry="$SCRIPT_DIR/eda01_entry.sh" seed="" tc="" ts="" cfg="" cfg_hash="" mode=""
    local pl="" wave="" ccov="" note="" timeout_ns="" plus_file="../seq/plus_cfg"
    local bootstrap="" worker_rc=125 rc_written=0 arg plus_string="" plusarg_count=0 i
    local -a plusargs=()

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --ack|--rc|--token|--job-id|--operation|--entry|--seed|--tc|--ts|--cfg|--cfg-hash|--mode|--verbosity|--wave|--coverage|--note|--timeout-ns|--plus-file|--plusarg|--bootstrap)
                require_option_value "$1" "$#" "${2-}" || exit 2
                ;;
        esac
        case "$1" in
            --ack) ack_file="${2:-}"; shift 2 ;;
            --rc) rc_file="${2:-}"; shift 2 ;;
            --token) token="${2:-}"; shift 2 ;;
            --job-id) job_id="${2:-}"; shift 2 ;;
            --operation) operation="${2:-}"; shift 2 ;;
            --entry) entry="${2:-}"; shift 2 ;;
            --seed) seed="${2:-}"; shift 2 ;;
            --tc) tc="${2:-}"; shift 2 ;;
            --ts) ts="${2:-}"; shift 2 ;;
            --cfg) cfg="${2:-}"; shift 2 ;;
            --cfg-hash) cfg_hash="${2:-}"; shift 2 ;;
            --mode) mode="${2:-}"; shift 2 ;;
            --verbosity) pl="${2:-}"; shift 2 ;;
            --wave) wave="${2:-}"; shift 2 ;;
            --coverage) ccov="${2:-}"; shift 2 ;;
            --note) note="${2:-}"; shift 2 ;;
            --timeout-ns) timeout_ns="${2:-}"; shift 2 ;;
            --plus-file) plus_file="${2:-}"; shift 2 ;;
            --plusarg) plusargs[$plusarg_count]="${2:-}"; plusarg_count=$((plusarg_count + 1)); shift 2 ;;
            --bootstrap) bootstrap="${2:-}"; shift 2 ;;
            *) log_error "unknown remote worker argument: $1"; exit 2 ;;
        esac
    done

    if [[ -z "$ack_file" || -z "$rc_file" || -z "$token" || -z "$job_id" \
        || -z "$operation" || -z "$seed" || -z "$tc" || -z "$ts" \
        || -z "$cfg" || -z "$cfg_hash" || -z "$mode" ]]; then
        log_error "remote worker is missing required arguments"
        exit 2
    fi
    valid_job_id "$job_id" && valid_token "$token" || exit 2
    [[ "$operation" == "compile" || "$operation" == "batch_run" ]] || exit 2
    [[ "${EDA_REGRESSION_RUN_TOKEN:-}" == "$token" ]] || exit 2
    [[ "$cfg_hash" =~ ^[0-9a-f]{64}$ ]] || { log_error "invalid cfg hash"; exit 2; }
    declare -A remote_plusarg_seen=()
    local remote_body remote_key
    for ((i = 0; i < plusarg_count; i++)); do
        arg="${plusargs[$i]}"
        validate_remote_plusarg "$arg" || { log_error "invalid remote plusarg: $arg"; exit 2; }
        remote_body="${arg#+}"; remote_key="${remote_body%%=*}"
        case "$remote_key" in
            ntb_random_seed|UVM_TESTNAME|UVM_VERBOSITY|UVM_TIMEOUT|TEST_MODE|VSEQ_MAIN|wave_type|gen_wave)
                log_error "remote plusarg key is reserved: $remote_key"; exit 2 ;;
        esac
        [[ -z "${remote_plusarg_seen[$remote_key]+x}" ]] || { log_error "duplicate remote plusarg key: $remote_key"; exit 2; }
        remote_plusarg_seen[$remote_key]=1
        plus_string+="${plus_string:+ }$arg"
    done

    mkdir -p "$(dirname "$ack_file")" "$(dirname "$rc_file")"
    local pid="$$" pgid sid
    pgid="$(ps -o pgid= -p "$$" | tr -d ' ')"
    sid="$(ps -o sid= -p "$$" | tr -d ' ')"
    [[ "$pid" == "$pgid" && "$pid" == "$sid" ]] || exit 2
    {
        printf 'protocol=%s\n' "$JOB_PROTOCOL"
        printf 'job_id=%s\n' "$job_id"
        printf 'token=%s\n' "$token"
        printf 'pid=%s\n' "$pid"
        printf 'pgid=%s\n' "$pgid"
        printf 'sid=%s\n' "$sid"
        printf 'started_at=%s\n' "$(date '+%F %T %Z')"
        printf 'started_epoch_ns=%s\n' "$(date +%s%N)"
        printf 'end=ACK_END\n'
    } | atomic_write "$ack_file" || exit 2

    write_worker_rc() {
        [[ "$rc_written" -eq 0 ]] || return
        rc_written=1
        {
            printf 'protocol=%s\n' "$JOB_PROTOCOL"
            printf 'job_id=%s\n' "$job_id"
            printf 'token=%s\n' "$token"
            printf 'pgid=%s\n' "$pgid"
            printf 'rc=%s\n' "$worker_rc"
            printf 'ended_at=%s\n' "$(date '+%F %T %Z')"
            printf 'ended_epoch_ns=%s\n' "$(date +%s%N)"
            printf 'end=RC_END\n'
        } | atomic_write "$rc_file" || true
    }
    trap write_worker_rc EXIT

    if [[ ! -f "$plus_file/$cfg.cfg" || ! -r "$plus_file/$cfg.cfg" ]]; then
        log_error "remote cfg snapshot is missing: $plus_file/$cfg.cfg"
        worker_rc=2
        exit "$worker_rc"
    fi
    if [[ "$(sha256sum "$plus_file/$cfg.cfg" | awk '{print $1}')" != "$cfg_hash" ]]; then
        log_error "remote cfg snapshot hash mismatch: $plus_file/$cfg.cfg"
        worker_rc=2
        exit "$worker_rc"
    fi

    local effective_entry="${EDA_REGRESSION_REMOTE_ENTRY:-$entry}"
    local -a entry_args=(
        "$effective_entry" "$operation"
        "seed=$seed" "tc=$tc" "ts=$ts" "cfg=$cfg" "mode=$mode"
        "pl=$pl" "wave=$wave" "ccov=$ccov" "fcov=off"
        "note=$note" "timeout_ns=$timeout_ns" "plus_file=$plus_file"
    )
    if [[ "$operation" == "batch_run" ]]; then
        entry_args+=("plus_arg=$plus_string")
    fi
    if [[ -n "$bootstrap" ]]; then
        bash -c "$bootstrap && exec \"\$@\"" bash "${entry_args[@]}"
    else
        "${entry_args[@]}"
    fi
    worker_rc=$?
    exit "$worker_rc"
}

case "${1:-}" in
    --remote-worker) shift; remote_worker "$@" ;;
    --remote-discover) remote_discover_token "${2:?missing token}"; exit 0 ;;
    --remote-rpc) remote_rpc; exit $? ;;
esac

usage() {
    cat <<'EOF'
Usage:
  run_eda_regression.sh --tc <name> [options]

Required:
  --tc <name>                 Testcase name.

Test selection:
  --cfg <name>                Plus cfg name without .cfg (default: default).
  --plus-file <dir>           Plus cfg directory (default: seq/plus_cfg).
                              Relative paths resolve from the caller's cwd.
  --ts <name>                 Virtual sequence (default: virtual_base_sequence).
  --mode <name>               Build/run mode (default: base_fun).
  --count <n>                 Generated-seed count (default: 1).
  --seed <n>                  Explicit seed; may be repeated.
  --seeds <list>              Comma/space-separated explicit seeds.
  --plusarg +KEY=VALUE        Runtime plusarg; may be repeated. VALUE allows
                              letters, digits, _ . / , : @ % + - only.

Execution:
  --jobs <n>                  Maximum parallel simulations (default: 20).
  --compile / --no-compile    Compile once before simulation (default: no).
  --wave / --no-wave          Enable FSDB waveform (default: no).
  --coverage / --no-coverage  Enable VCS code coverage (default: no).
  --verbosity <level>         UVM_NONE/LOW/MEDIUM/HIGH/FULL/DEBUG
                              (default: UVM_LOW).
  --debug                     Defaults wave=on and verbosity=UVM_DEBUG unless
                              explicitly overridden.
  --timeout-ns <n>            UVM timeout (default: 100000000).

Monitoring and records:
  --monitor / --no-monitor    Early-stop monitoring (default: on).
  --error-limit <n>           Per-seed UVM_ERROR/UVM_FATAL threshold (default: 1).
  --record / --no-record      Dynamic progress/result files (default: on).
                              Safety event/commit and summary files are retained.

Infrastructure:
  --remote-host <host>        Remote EDA host (default: eda01; fallback to
                              172.28.10.101 for the default host).
  --output-dir <path>         Empty result directory shared with eda01.
  --poll-interval <sec>       Controller poll interval (default: 1).
  --launch-timeout <sec>      Worker ACK timeout (default: 30).
  --probe-interval <sec>      Remote liveness probe interval (default: 5).
  --ssh-retries <n>           Remote operation retry count (default: 3).
  --dry-run                   Validate and record configuration only; shared
                              path validation is marked DEFERRED.
  -h, --help                  Show this help.

Explicit seeds determine the simulation count; duplicate normalized seeds are
removed while preserving first occurrence.
EOF
}

normalize_bounded_decimal() {
    local name="$1" raw="$2" min="$3" max="$4" normalized max_digits
    [[ "$raw" =~ ^[0-9]+$ ]] || { log_error "--$name must be decimal, got '$raw'"; return 2; }
    max_digits="${#max}"
    (( ${#raw} <= max_digits + 8 )) || { log_error "--$name is too large"; return 2; }
    normalized="${raw#"${raw%%[!0]*}"}"
    [[ -n "$normalized" ]] || normalized="0"
    (( ${#normalized} <= max_digits )) || { log_error "--$name exceeds $max"; return 2; }
    local value=$((10#$normalized))
    (( value >= min && value <= max )) || {
        log_error "--$name must be in $min..$max, got '$raw'"
        return 2
    }
    printf '%s\n' "$value"
}

normalize_plus_file_dir() {
    local raw_dir="$1" candidate canonical_dir
    if [[ -z "$raw_dir" || "$raw_dir" =~ [[:cntrl:]] ]]; then
        log_error "--plus-file requires a non-empty directory without control characters"
        return 2
    fi
    if [[ "$raw_dir" == /* ]]; then candidate="$raw_dir"; else candidate="$PWD/$raw_dir"; fi
    [[ -d "$candidate" && -r "$candidate" && -x "$candidate" ]] || {
        log_error "plus-file directory is not readable and searchable: $candidate"
        return 2
    }
    canonical_dir="$(cd -- "$candidate" && pwd -P)" || {
        log_error "failed to canonicalize plus-file directory: $candidate"
        return 2
    }
    if [[ -z "$canonical_dir" || "$canonical_dir" =~ [[:cntrl:]] ]]; then
        log_error "canonical plus-file directory contains control characters"
        return 2
    fi
    [[ -d "$canonical_dir" && -r "$canonical_dir" && -x "$canonical_dir" ]] || {
        log_error "canonical plus-file directory is not readable and searchable: $canonical_dir"
        return 2
    }
    printf '%s\n' "$canonical_dir"
}

resolve_cfg_file() {
    local canonical_dir="$1" cfg_name="$2" candidate resolved_file resolved_parent
    candidate="$canonical_dir/$cfg_name.cfg"
    [[ -f "$candidate" && -r "$candidate" ]] || {
        log_error "cfg file is not a readable regular file: $candidate"
        return 2
    }
    resolved_file="$(realpath -e -- "$candidate")" || {
        log_error "failed to resolve cfg file: $candidate"
        return 2
    }
    if [[ -z "$resolved_file" || "$resolved_file" =~ [[:cntrl:]] ]]; then
        log_error "resolved cfg path contains control characters"
        return 2
    fi
    [[ -f "$resolved_file" && -r "$resolved_file" ]] || {
        log_error "resolved cfg is not a readable regular file: $resolved_file"
        return 2
    }
    resolved_parent="$(dirname -- "$resolved_file")"
    [[ "$resolved_parent" == "$canonical_dir" ]] || {
        log_error "cfg file escapes plus-file directory: $candidate -> $resolved_file"
        return 2
    }
    printf '%s\n' "$resolved_file"
}

validate_cfg_plusargs() {
    local file="$1" line trimmed body key value line_no=0
    declare -A cfg_seen=()
    while IFS= read -r line || [[ -n "$line" ]]; do
        ((line_no += 1))
        trimmed="${line#"${line%%[![:space:]]*}"}"
        trimmed="${trimmed%"${trimmed##*[![:space:]]}"}"
        [[ -z "$trimmed" || "$trimmed" == \#* || "$trimmed" == + ]] && continue
        [[ "$trimmed" == +*=* && "$trimmed" != *"="*"="* ]] || {
            log_error "invalid cfg plusarg at $file:$line_no; expected exactly +KEY=VALUE"
            return 2
        }
        body="${trimmed#+}"
        key="${body%%=*}"
        value="${body#*=}"
        [[ "$key" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]] || {
            log_error "invalid cfg plusarg key '$key' at $file:$line_no"
            return 2
        }
        [[ "$value" =~ ^[A-Za-z0-9_./,:@%+-]+$ ]] || {
            log_error "invalid cfg plusarg value for '$key' at $file:$line_no"
            return 2
        }
        [[ -z "${cfg_seen[$key]+x}" ]] || {
            log_error "duplicate cfg plusarg key '$key' at $file:$line_no"
            return 2
        }
        cfg_seen[$key]=1
    done <"$file"
}

declare -A PLUSARG_SEEN=()
declare -a USER_PLUSARGS=()
validate_plusarg() {
    local token="$1" body key value
    [[ "$token" == +*=* && "$token" != *"="*"="* ]] || {
        log_error "invalid --plusarg '$token'; expected exactly +KEY=VALUE"
        return 2
    }
    body="${token#+}"
    key="${body%%=*}"
    value="${body#*=}"
    [[ "$key" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]] || { log_error "invalid plusarg key '$key'"; return 2; }
    [[ "$value" =~ ^[A-Za-z0-9_./,:@%+-]+$ ]] || { log_error "invalid plusarg value for '$key'"; return 2; }
    case "$key" in
        ntb_random_seed|UVM_TESTNAME|UVM_VERBOSITY|UVM_TIMEOUT|TEST_MODE|VSEQ_MAIN|wave_type|gen_wave)
            log_error "plusarg key '$key' is controlled by a formal option"
            return 2
            ;;
    esac
    [[ -z "${PLUSARG_SEEN[$key]+x}" ]] || { log_error "duplicate plusarg key '$key'"; return 2; }
    PLUSARG_SEEN[$key]=1
    USER_PLUSARGS+=("$token")
}

TC=""; CFG="default"; TS="virtual_base_sequence"; MODE="base_fun"
COUNT="1"; JOBS="20"; ERROR_LIMIT="1"; TIMEOUT_NS="100000000"
POLL_INTERVAL="1"; LAUNCH_TIMEOUT="30"; PROBE_INTERVAL="5"; SSH_RETRIES="3"
DO_COMPILE=0; WAVE_ON=0; COVERAGE_ON=0; MONITOR_ON=1; RECORD_ON=1
DEBUG_ON=0; DRY_RUN=0; VERBOSITY="UVM_LOW"; REMOTE_HOST="$DEFAULT_REMOTE_HOST"
OUTPUT_DIR=""; WAVE_EXPLICIT=0; VERBOSITY_EXPLICIT=0; COUNT_EXPLICIT=0
PLUS_FILE_INPUT="$SIM_DIR/../seq/plus_cfg"
EXPLICIT_SEED_MODE=0
declare -a RAW_SEEDS=()

while [[ $# -gt 0 ]]; do
    option="$1"
    case "$option" in
        --tc|--cfg|--plus-file|--ts|--mode|--count|--seed|--seeds|--jobs|--plusarg|--verbosity|--error-limit|--timeout-ns|--remote-host|--output-dir|--poll-interval|--launch-timeout|--probe-interval|--ssh-retries)
            require_option_value "$option" "$#" "${2-}" || { usage >&2; exit 2; }
            value="$2"
            case "$option" in
                --tc) TC="$value" ;;
                --cfg) CFG="$value" ;;
                --plus-file) PLUS_FILE_INPUT="$value" ;;
                --ts) TS="$value" ;;
                --mode) MODE="$value" ;;
                --count) COUNT="$value"; COUNT_EXPLICIT=1 ;;
                --seed) RAW_SEEDS+=("$value"); EXPLICIT_SEED_MODE=1 ;;
                --seeds)
                    EXPLICIT_SEED_MODE=1
                    seed_text="${value//,/ }"
                    read -r -a seed_parts <<<"$seed_text"
                    RAW_SEEDS+=("${seed_parts[@]}")
                    ;;
                --jobs) JOBS="$value" ;;
                --plusarg) validate_plusarg "$value" || exit 2 ;;
                --verbosity) VERBOSITY="$value"; VERBOSITY_EXPLICIT=1 ;;
                --error-limit) ERROR_LIMIT="$value" ;;
                --timeout-ns) TIMEOUT_NS="$value" ;;
                --remote-host) REMOTE_HOST="$value" ;;
                --output-dir) OUTPUT_DIR="$value" ;;
                --poll-interval) POLL_INTERVAL="$value" ;;
                --launch-timeout) LAUNCH_TIMEOUT="$value" ;;
                --probe-interval) PROBE_INTERVAL="$value" ;;
                --ssh-retries) SSH_RETRIES="$value" ;;
            esac
            shift 2
            ;;
        --compile) DO_COMPILE=1; shift ;;
        --no-compile) DO_COMPILE=0; shift ;;
        --wave) WAVE_ON=1; WAVE_EXPLICIT=1; shift ;;
        --no-wave) WAVE_ON=0; WAVE_EXPLICIT=1; shift ;;
        --coverage) COVERAGE_ON=1; shift ;;
        --no-coverage) COVERAGE_ON=0; shift ;;
        --monitor) MONITOR_ON=1; shift ;;
        --no-monitor) MONITOR_ON=0; shift ;;
        --record) RECORD_ON=1; shift ;;
        --no-record) RECORD_ON=0; shift ;;
        --debug) DEBUG_ON=1; shift ;;
        --dry-run) DRY_RUN=1; shift ;;
        -h|--help) usage; exit 0 ;;
        *) log_error "unknown argument: $1"; usage >&2; exit 2 ;;
    esac
done

[[ "$DEBUG_ON" -eq 0 || "$WAVE_EXPLICIT" -eq 1 ]] || WAVE_ON=1
[[ "$DEBUG_ON" -eq 0 || "$VERBOSITY_EXPLICIT" -eq 1 ]] || VERBOSITY="UVM_DEBUG"
[[ -n "$TC" ]] || { log_error "--tc is required"; exit 2; }
for value in "$TC" "$CFG" "$TS" "$MODE"; do
    valid_name "$value" || { log_error "invalid name '$value'"; exit 2; }
done
case "$VERBOSITY" in
    UVM_NONE|UVM_LOW|UVM_MEDIUM|UVM_HIGH|UVM_FULL|UVM_DEBUG) ;;
    *) log_error "invalid --verbosity '$VERBOSITY'"; exit 2 ;;
esac

COUNT="$(normalize_bounded_decimal count "$COUNT" 1 100000)" || exit 2
JOBS="$(normalize_bounded_decimal jobs "$JOBS" 1 1000)" || exit 2
ERROR_LIMIT="$(normalize_bounded_decimal error-limit "$ERROR_LIMIT" 1 1000000)" || exit 2
TIMEOUT_NS="$(normalize_bounded_decimal timeout-ns "$TIMEOUT_NS" 1 999999999999999999)" || exit 2
POLL_INTERVAL="$(normalize_bounded_decimal poll-interval "$POLL_INTERVAL" 1 3600)" || exit 2
LAUNCH_TIMEOUT="$(normalize_bounded_decimal launch-timeout "$LAUNCH_TIMEOUT" 1 3600)" || exit 2
PROBE_INTERVAL="$(normalize_bounded_decimal probe-interval "$PROBE_INTERVAL" 1 3600)" || exit 2
SSH_RETRIES="$(normalize_bounded_decimal ssh-retries "$SSH_RETRIES" 1 100)" || exit 2

PLUS_FILE="$(normalize_plus_file_dir "$PLUS_FILE_INPUT")" || exit 2
CFG_FILE="$(resolve_cfg_file "$PLUS_FILE" "$CFG")" || exit 2

declare -a SEEDS=()
declare -A SEED_SEEN=()
if [[ "$EXPLICIT_SEED_MODE" -eq 1 && "${#RAW_SEEDS[@]}" -eq 0 ]]; then
    log_error "explicit seed mode requires at least one seed"
    exit 2
fi
if [[ "${#RAW_SEEDS[@]}" -gt 0 ]]; then
    [[ "$COUNT_EXPLICIT" -eq 0 ]] || log_warn "--count ignored because explicit seeds were provided"
    for raw_seed in "${RAW_SEEDS[@]}"; do
        seed="$(normalize_bounded_decimal seed "$raw_seed" 1 2147483646)" || exit 2
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

USER_PLUSARGS_STRING=""
for arg in "${USER_PLUSARGS[@]}"; do USER_PLUSARGS_STRING+="${USER_PLUSARGS_STRING:+ }$arg"; done

RUN_ID="${TC}_${CFG}_$(date '+%Y%m%d_%H%M%S')_$$_$RANDOM"
NOTE="eda_reg_$(date '+%Y%m%d_%H%M%S')_$$_$RANDOM"
if [[ -z "$OUTPUT_DIR" ]]; then
    OUTPUT_DIR="$SIM_DIR/.eda_regression/$RUN_ID"
elif [[ "$OUTPUT_DIR" != /* ]]; then
    OUTPUT_DIR="$PWD/$OUTPUT_DIR"
fi
if [[ "$OUTPUT_DIR" == *$'\n'* || "$OUTPUT_DIR" == *$'\r'* || "$OUTPUT_DIR" == *$'\t'* \
    || "$OUTPUT_DIR" == *"'"* || "$OUTPUT_DIR" == *'"'* || "$OUTPUT_DIR" == *'$'* \
    || "$OUTPUT_DIR" == *'`'* || "$OUTPUT_DIR" == *';'* || "$OUTPUT_DIR" == *'&'* \
    || "$OUTPUT_DIR" == *'|'* || "$OUTPUT_DIR" == *'<'* || "$OUTPUT_DIR" == *'>'* \
    || "$OUTPUT_DIR" == *'('* || "$OUTPUT_DIR" == *')'* || "$OUTPUT_DIR" == *'\\'* ]]; then
    log_error "output directory contains unsupported shell-sensitive characters: $OUTPUT_DIR"
    exit 2
fi
if [[ -e "$OUTPUT_DIR" && ! -d "$OUTPUT_DIR" ]]; then
    log_error "output path exists but is not a directory: $OUTPUT_DIR"
    exit 2
fi
if [[ -d "$OUTPUT_DIR" ]] && find "$OUTPUT_DIR" -mindepth 1 -maxdepth 1 -print -quit | grep -q .; then
    log_error "output directory is not empty: $OUTPUT_DIR"
    exit 2
fi
mkdir -p "$OUTPUT_DIR" || exit 2
OUTPUT_DIR="$(cd "$OUTPUT_DIR" && pwd)" || exit 2
[[ -w "$OUTPUT_DIR" ]] || { log_error "output directory is not writable: $OUTPUT_DIR"; exit 2; }

STATUS_DIR="$OUTPUT_DIR/status"; PID_DIR="$OUTPUT_DIR/pids"
LAUNCHER_DIR="$OUTPUT_DIR/launcher"; MONITOR_DIR="$OUTPUT_DIR/monitor"
mkdir -p "$STATUS_DIR" "$PID_DIR" "$LAUNCHER_DIR" "$MONITOR_DIR" || exit 2
FROZEN_PLUS_FILE="$OUTPUT_DIR/frozen_plus_cfg"
mkdir -p "$FROZEN_PLUS_FILE" || exit 2
cp -- "$CFG_FILE" "$FROZEN_PLUS_FILE/$CFG.cfg" || exit 2
chmod 0444 "$FROZEN_PLUS_FILE/$CFG.cfg" || exit 2
validate_cfg_plusargs "$FROZEN_PLUS_FILE/$CFG.cfg" || exit 2
CFG_HASH="$(sha256sum "$FROZEN_PLUS_FILE/$CFG.cfg" | awk '{print $1}')"
if ! EFFECTIVE_PLUSARGS="$(make -s -C "$SIM_DIR" eda_print_effective_plusargs \
    cfg="$CFG" plus_file="$FROZEN_PLUS_FILE" plus_arg="$USER_PLUSARGS_STRING" | paste -sd' ' -)"; then
    log_error "failed to resolve effective plusargs through Makefile"
    exit 2
fi
printf '%s\n' "${SEEDS[@]}" >"$OUTPUT_DIR/seeds.txt"
printf '%s\n' "${USER_PLUSARGS[@]}" >"$OUTPUT_DIR/user_plusargs.txt"
printf '%s\n' "$EFFECTIVE_PLUSARGS" >"$OUTPUT_DIR/effective_plusargs.txt"

WAVE_VALUE="off"; CCOV_VALUE="off"
[[ "$WAVE_ON" -eq 0 ]] || WAVE_VALUE="on"
[[ "$COVERAGE_ON" -eq 0 ]] || CCOV_VALUE="on"
SHARED_PATH_VALIDATION="DEFERRED"

write_config() {
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
        printf 'final_remote_host=%s\n' "${FINAL_REMOTE_HOST:-DEFERRED}"
        printf 'shared_path_validation=%s\n' "$SHARED_PATH_VALIDATION"
        printf 'note=%s\n' "$NOTE"
        printf 'user_plusargs=%s\n' "$USER_PLUSARGS_STRING"
        printf 'effective_plusargs=%s\n' "$EFFECTIVE_PLUSARGS"
        printf 'source_plus_file=%s\n' "$PLUS_FILE"
        printf 'source_cfg=%s\n' "$CFG_FILE"
        printf 'frozen_cfg=%s\n' "$FROZEN_PLUS_FILE/$CFG.cfg"
        printf 'cfg_sha256=%s\n' "$CFG_HASH"
    } | atomic_write "$OUTPUT_DIR/config.txt"
}
write_config || exit 2
if [[ "$DRY_RUN" -eq 1 ]]; then
    log_info "dry-run configuration:"
    cat "$OUTPUT_DIR/config.txt"
    log_info "seeds: ${SEEDS[*]}"
    log_info "result directory: $OUTPUT_DIR"
    exit 0
fi

SSH_OPTS=(-o BatchMode=yes -o ControlMaster=no -o ConnectTimeout=10)
RC_RECOVERY_READ_TIMEOUT=5
FINAL_REMOTE_HOST=""
resolve_remote_host() {
    FINAL_REMOTE_HOST="$REMOTE_HOST"
    if ssh "${SSH_OPTS[@]}" "$FINAL_REMOTE_HOST" true >/dev/null 2>&1; then return 0; fi
    if [[ "$REMOTE_HOST" == "$DEFAULT_REMOTE_HOST" ]] \
        && ssh "${SSH_OPTS[@]}" "$FALLBACK_REMOTE_HOST" true >/dev/null 2>&1; then
        FINAL_REMOTE_HOST="$FALLBACK_REMOTE_HOST"
        log_warn "remote host eda01 is unavailable; using $FALLBACK_REMOTE_HOST"
        return 0
    fi
    return 1
}
resolve_remote_host || { log_error "cannot connect to remote host '$REMOTE_HOST'"; exit 3; }

ssh_once() {
    local remote_cmd="$1" host="${2:-$FINAL_REMOTE_HOST}"
    (
        close_inherited_lock_fd
        exec ssh "${SSH_OPTS[@]}" "$host" "$remote_cmd"
    )
}
ssh_once_bounded() {
    local timeout_sec="$1" remote_cmd="$2" host="${3:-$FINAL_REMOTE_HOST}"
    (
        close_inherited_lock_fd
        exec timeout --signal=TERM --kill-after=1 "$timeout_sec" \
            ssh "${SSH_OPTS[@]}" "$host" "$remote_cmd"
    )
}
ssh_retry() {
    local remote_cmd="$1" host="${2:-$FINAL_REMOTE_HOST}" attempt
    for ((attempt = 1; attempt <= SSH_RETRIES; attempt++)); do
        ssh_once "$remote_cmd" "$host" && return 0
        (( attempt == SSH_RETRIES )) || sleep 1
    done
    return 1
}

validate_shared_output_dir() {
    local nonce_a response_file confirm_file remote_script remote_cmd nonce_b
    nonce_a="$(od -An -N16 -tx1 /dev/urandom | tr -d ' \n')"
    response_file="$OUTPUT_DIR/.share_response"
    confirm_file="$OUTPUT_DIR/.share_confirm"
    printf '%s\n' "$nonce_a" | atomic_write "$OUTPUT_DIR/.share_request" || return 1
    remote_script='set -eu; dir=$1; expected=$2; actual=$(cat "$dir/.share_request"); [ "$actual" = "$expected" ]; nonce_b=$(od -An -N16 -tx1 /dev/urandom | tr -d " \\n"); tmp="$dir/.share_response.tmp.$$"; printf "%s %s\\n" "$expected" "$nonce_b" >"$tmp"; mv -fT "$tmp" "$dir/.share_response"'
    remote_cmd="$(quote_cmd bash -c "$remote_script" bash "$OUTPUT_DIR" "$nonce_a")"
    ssh_retry "$remote_cmd" >/dev/null 2>&1 || return 1
    read -r response_a nonce_b <"$response_file" || return 1
    [[ "$response_a" == "$nonce_a" && "$nonce_b" =~ ^[0-9a-f]+$ ]] || return 1
    printf '%s\n' "$nonce_b" | atomic_write "$confirm_file" || return 1
    remote_script='set -eu; dir=$1; expected=$2; actual=$(cat "$dir/.share_confirm"); [ "$actual" = "$expected" ]'
    remote_cmd="$(quote_cmd bash -c "$remote_script" bash "$OUTPUT_DIR" "$nonce_b")"
    ssh_retry "$remote_cmd" >/dev/null 2>&1 || return 1
    rm -f "$OUTPUT_DIR/.share_request" "$response_file" "$confirm_file"
}
validate_shared_output_dir || { log_error "output directory is not proven shared and writable on eda01: $OUTPUT_DIR"; exit 3; }
SHARED_PATH_VALIDATION="PASS"
write_config || exit 3

REMOTE_BOOTSTRAP="${EDA_REGRESSION_REMOTE_BOOTSTRAP:-$(make -s -C "$SIM_DIR" eda_print_bootstrap)}"
[[ -n "$REMOTE_BOOTSTRAP" ]] || { log_error "Makefile returned empty REMOTE_BOOTSTRAP"; exit 3; }

LOCK_ROOT="$SIM_DIR/.eda_regression/locks"
mkdir -p "$LOCK_ROOT"
LOCK_FILE="$LOCK_ROOT/$MODE.lock"; LOCK_INFO="$LOCK_ROOT/$MODE.owner"
OWNER_FILE="$LOCK_ROOT/$MODE.owners.tsv"; QUARANTINE_FILE="$LOCK_ROOT/$MODE.quarantine.tsv"
exec {LOCK_FD}>"$LOCK_FILE"
if ! flock -n "$LOCK_FD"; then
    log_error "mode '$MODE' is already in use"
    [[ -f "$LOCK_INFO" ]] && cat "$LOCK_INFO" >&2
    exit 4
fi
LOCK_HELD=1

controller_exit_trap() {
    local rc=$?
    if declare -F cleanup_controller >/dev/null 2>&1 && [[ "${CLEANUP_DONE:-0}" -eq 0 ]]; then
        [[ -n "${STOP_REASON:-}" ]] || STOP_REASON="controller exit rc=$rc"
        cleanup_controller
    else
        [[ -n "${LOCK_INFO:-}" ]] && rm -f "$LOCK_INFO"
        if [[ "${LOCK_HELD:-0}" -eq 1 ]]; then flock -u "$LOCK_FD" 2>/dev/null || true; LOCK_HELD=0; fi
    fi
    return "$rc"
}
controller_signal_trap() {
    local sig="$1" rc=130
    [[ "$sig" == TERM ]] && rc=143
    if [[ "${CLEANUP_RUNNING:-0}" -eq 1 ]]; then
        STOP_REASON="additional user signal $sig during cleanup"
        return 0
    fi
    STOP_REASON="user signal $sig"
    if declare -F cleanup_controller >/dev/null 2>&1; then cleanup_controller; fi
    trap - EXIT INT TERM
    if [[ "${LOCK_HELD:-0}" -eq 1 ]]; then flock -u "$LOCK_FD" 2>/dev/null || true; LOCK_HELD=0; fi
    exit "$rc"
}
trap controller_exit_trap EXIT
trap 'controller_signal_trap INT' INT
trap 'controller_signal_trap TERM' TERM

declare -a JOB_ORDER=()
declare -A JOB_KNOWN=() JOB_SEED=() JOB_OPERATION=() TOKEN=() PGID=()
declare -A REMOTE_PHASE=() SIGNAL_DISPOSITION=() OWNER_ACTIVE=() OWNER_CREATED=()
declare -A JOB_HOST=() JOB_RUN_ID=()
declare -A ACK_FILE=() RC_FILE=() LOG_PATH=() FINAL_LOG=() RC=()
declare -A START_EPOCH_NS=() END_EPOCH_NS=()
declare -A RESULT_STATE=() FAILURE_REASON=() UVM_COUNT=() WATCHER_PID=()
declare -A UNKNOWN_REASON=() TRIGGER_HANDLED=() TRIGGER_COUNT=() TRIGGER_LINE=() TRIGGER_TIME=()
declare -A FINALIZATION_FAILURE=()
declare -A RPC_STATUS=()
STOP_REQUESTED=0; STOP_REASON=""; RECORD_INFRA_FAILURE=0; MONITOR_COMMIT="NONE"
CLEANUP_DONE=0; CLEANUP_RUNNING=0; LAST_PROGRESS=""; LAST_PROBE_AT=0
SIMV_CHANGED=0; FINAL_RESULT="FAIL"; PHASE="SETUP"; INITIAL_STATE_FAILURE=0
PEAK_ACTIVE=0; PEAK_LIVE=0

register_job() {
    local job="$1"
    if [[ -z "${JOB_KNOWN[$job]+x}" ]]; then JOB_ORDER+=("$job"); JOB_KNOWN[$job]=1; fi
}

write_owner_manifest() {
    local tmp="$OWNER_FILE.tmp.$$.$RANDOM" job
    {
        printf 'job_id\ttoken\tpgid\tremote_phase\trun_id\thost\tcreated_epoch\tupdated_at\n'
        for job in "${JOB_ORDER[@]}"; do
            [[ "${OWNER_ACTIVE[$job]:-0}" -eq 1 ]] || continue
            printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
                "$job" "${TOKEN[$job]}" "${PGID[$job]:-UNKNOWN}" "${REMOTE_PHASE[$job]}" \
                "${JOB_RUN_ID[$job]:-$RUN_ID}" "${JOB_HOST[$job]:-$FINAL_REMOTE_HOST}" "${OWNER_CREATED[$job]}" "$(date '+%F_%T_%Z')"
        done
    } >"$tmp" || { rm -f "$tmp"; return 1; }
    mv -fT "$tmp" "$OWNER_FILE" || { rm -f "$tmp"; return 1; }
}

write_quarantine() {
    local tmp="$QUARANTINE_FILE.tmp.$$.$RANDOM" job wrote=0
    {
        printf 'mode\trun_id\thost\tjob_id\ttoken\tpgid\tresult_state\tremote_phase\treason\tupdated_at\n'
        for job in "${JOB_ORDER[@]}"; do
            [[ "${REMOTE_PHASE[$job]:-}" == "REMOTE_UNKNOWN" ]] || continue
            printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
                "$MODE" "${JOB_RUN_ID[$job]:-$RUN_ID}" "${JOB_HOST[$job]:-$FINAL_REMOTE_HOST}" "$job" "${TOKEN[$job]:-unknown}" \
                "${PGID[$job]:-UNKNOWN}" "${RESULT_STATE[$job]:-PENDING}" \
                "${REMOTE_PHASE[$job]}" "${UNKNOWN_REASON[$job]:-unknown}" "$(date '+%F %T %Z')"
            wrote=1
        done
    } >"$tmp" || { rm -f "$tmp"; return 1; }
    if [[ "$wrote" -eq 1 ]]; then mv -fT "$tmp" "$QUARANTINE_FILE"; else rm -f "$tmp" "$QUARANTINE_FILE"; fi
}

mark_remote_unknown() {
    local job="$1" reason="$2"
    REMOTE_PHASE[$job]="REMOTE_UNKNOWN"
    UNKNOWN_REASON[$job]="$reason"
    STOP_REQUESTED=1
    [[ -n "$STOP_REASON" ]] || STOP_REASON="$reason"
    write_owner_manifest || RECORD_INFRA_FAILURE=1
}

mark_finalization_failure() {
    local job="$1" reason="$2"
    FINALIZATION_FAILURE[$job]="$reason"
    RECORD_INFRA_FAILURE=1
    STOP_REQUESTED=1
    [[ -n "$STOP_REASON" ]] || STOP_REASON="$reason"
    log_error "$job local finalization failed: $reason"
}

run_remote_rpc() {
    local op="$1"; shift
    local -a jobs=("$@")
    local request="$OUTPUT_DIR/rpc_request.$$.${RANDOM}" response="$OUTPUT_DIR/rpc_response.$$.${RANDOM}"
    local items="$request.items" response_items="$response.items" request_id count hash line
    local protocol job token pgid status extra response_id response_op response_count response_hash end_id rpc_host=""
    RPC_STATUS=()
    declare -A pending_status=()
    : >"$items"
    for job in "${jobs[@]}"; do
        if [[ -z "$rpc_host" ]]; then rpc_host="${JOB_HOST[$job]:-$FINAL_REMOTE_HOST}"; fi
        [[ "${JOB_HOST[$job]:-$FINAL_REMOTE_HOST}" == "$rpc_host" ]] || { rm -f "$items"; return 1; }
        [[ "${PGID[$job]:-}" =~ ^[1-9][0-9]*$ ]] || { rm -f "$items"; return 1; }
        printf 'ITEM\t%s\t%s\t%s\n' "$job" "${TOKEN[$job]}" "${PGID[$job]}" >>"$items"
    done
    count="${#jobs[@]}"
    request_id="${RUN_ID}_${RANDOM}_${RANDOM}"
    hash="$(sha256sum "$items" | awk '{print $1}')"
    {
        printf 'PROTO %s\n' "$RPC_PROTOCOL"
        printf 'REQUEST_ID %s\n' "$request_id"
        printf 'OP %s\n' "$op"
        printf 'COUNT %s\n' "$count"
        printf 'HASH %s\n' "$hash"
        cat "$items"
        printf 'END %s\n' "$request_id"
    } >"$request"
    local cmd
    cmd="$(quote_cmd "$SCRIPT_PATH" --remote-rpc)"
    if ! (close_inherited_lock_fd; exec ssh "${SSH_OPTS[@]}" "$rpc_host" "$cmd") <"$request" >"$response" 2>/dev/null; then
        rm -f "$request" "$items" "$response"
        return 1
    fi
    exec 3<"$response"
    IFS= read -r line <&3 || return 1; [[ "$line" == "PROTO $RPC_PROTOCOL" ]] || return 1
    IFS= read -r line <&3 || return 1; response_id="${line#REQUEST_ID }"; [[ "$line" == REQUEST_ID\ * && "$response_id" == "$request_id" ]] || return 1
    IFS= read -r line <&3 || return 1; response_op="${line#OP }"; [[ "$line" == OP\ * && "$response_op" == "$op" ]] || return 1
    IFS= read -r line <&3 || return 1; response_count="${line#COUNT }"; [[ "$line" == COUNT\ * && "$response_count" == "$count" ]] || return 1
    IFS= read -r line <&3 || return 1; response_hash="${line#HASH }"; [[ "$line" == HASH\ * && "$response_hash" =~ ^[0-9a-f]{64}$ ]] || return 1
    : >"$response_items"
    declare -A requested=() seen=()
    for job in "${jobs[@]}"; do requested[$job]=1; done
    local got=0
    while IFS= read -r line <&3; do
        if [[ "$line" == END\ * ]]; then end_id="${line#END }"; break; fi
        IFS=$'\t' read -r protocol job status extra <<<"$line"
        [[ "$protocol" == "ITEM" && -z "${extra:-}" && -n "${requested[$job]+x}" && -z "${seen[$job]+x}" ]] || return 1
        case "$status" in ALIVE|EXITED|SIGNALED|IDENTITY_MISMATCH|SIGNAL_FAILED) ;; *) return 1 ;; esac
        seen[$job]=1; pending_status[$job]="$status"; got=$((got + 1))
        printf 'ITEM\t%s\t%s\n' "$job" "$status" >>"$response_items"
    done
    IFS= read -r line <&3 && return 1
    exec 3<&-
    [[ "${end_id:-}" == "$request_id" && "$got" -eq "$count" ]] || return 1
    hash="$(sha256sum "$response_items" | awk '{print $1}')"
    [[ "$hash" == "$response_hash" ]] || return 1
    for job in "${jobs[@]}"; do RPC_STATUS[$job]="${pending_status[$job]}"; done
    rm -f "$request" "$items" "$response" "$response_items"
}

discover_job_token() {
    local job="$1" output cmd pid pgid sid unique=0 found_pgid=""
    cmd="$(quote_cmd "$SCRIPT_PATH" --remote-discover "${TOKEN[$job]}")"
    output="$(ssh_retry "$cmd" "${JOB_HOST[$job]:-$FINAL_REMOTE_HOST}")" || return 2
    declare -A seen=()
    while IFS=$'\t' read -r pid pgid sid; do
        [[ "$pgid" =~ ^[1-9][0-9]*$ ]] || continue
        if [[ -z "${seen[$pgid]+x}" ]]; then seen[$pgid]=1; found_pgid="$pgid"; unique=$((unique + 1)); fi
    done <<<"$output"
    [[ "$unique" -eq 1 ]] || return 1
    PGID[$job]="$found_pgid"
    REMOTE_PHASE[$job]="OWNED"
    write_owner_manifest || return 2
}

audit_stale_owners() {
    [[ ! -s "$QUARANTINE_FILE" ]] || { log_error "mode '$MODE' is quarantined: $QUARANTINE_FILE"; cat "$QUARANTINE_FILE" >&2; return 1; }
    [[ -s "$OWNER_FILE" ]] || return 0
    local header job token pgid phase old_run old_host created updated
    quarantine_malformed_owner() {
        local reason="$1"
        log_error "$reason"
        {
            printf 'mode\trun_id\thost\tjob_id\ttoken\tpgid\tresult_state\tremote_phase\treason\tupdated_at\n'
            printf '%s\tunknown\tunknown\tunknown\tunknown\tUNKNOWN\tPENDING\tREMOTE_UNKNOWN\t%s\t%s\n' \
                "$MODE" "$reason" "$(date '+%F %T %Z')"
        } | atomic_write "$QUARANTINE_FILE" || true
        return 1
    }
    header=$'job_id\ttoken\tpgid\tremote_phase\trun_id\thost\tcreated_epoch\tupdated_at'
    if [[ "$(head -n1 "$OWNER_FILE")" != "$header" ]] || \
        ! awk -F '\t' 'NR > 1 && NF != 8 {bad=1} END {exit bad}' "$OWNER_FILE"; then
        quarantine_malformed_owner "malformed owner manifest: $OWNER_FILE"
        return 1
    fi
    declare -A stale_seen=()
    while IFS=$'\t' read -r job token pgid phase old_run old_host created updated; do
        [[ "$job" == "job_id" || -z "$job" ]] && continue
        valid_job_id "$job" && valid_token "$token" || { quarantine_malformed_owner "invalid stale owner identity"; return 1; }
        [[ -z "${stale_seen[$job]+x}" ]] || { quarantine_malformed_owner "duplicate stale owner job: $job"; return 1; }
        stale_seen[$job]=1
        [[ "$pgid" == UNKNOWN || "$pgid" =~ ^[1-9][0-9]*$ ]] || { quarantine_malformed_owner "invalid stale owner pgid: $job"; return 1; }
        case "$phase" in LAUNCHING|OWNED|RC_SEEN|EXIT_CONFIRMED|REMOTE_UNKNOWN) ;; *) quarantine_malformed_owner "invalid stale owner phase: $job"; return 1 ;; esac
        valid_name "$old_run" || { quarantine_malformed_owner "invalid stale owner run id: $job"; return 1; }
        [[ "$old_host" =~ ^[A-Za-z0-9_.:-]+$ && "$created" =~ ^[0-9]+$ && -n "$updated" ]] || {
            quarantine_malformed_owner "invalid stale owner metadata: $job"; return 1;
        }
        register_job "$job"
        TOKEN[$job]="$token"; PGID[$job]="$pgid"; REMOTE_PHASE[$job]="$phase"
        JOB_HOST[$job]="$old_host"; JOB_RUN_ID[$job]="$old_run"
        OWNER_ACTIVE[$job]=1; OWNER_CREATED[$job]="${created:-0}"; RESULT_STATE[$job]="PENDING"
        if [[ "$phase" == "EXIT_CONFIRMED" ]]; then
            OWNER_ACTIVE[$job]=0
            continue
        fi
        if [[ "$pgid" == "UNKNOWN" ]]; then
            if ! discover_job_token "$job"; then
                mark_remote_unknown "$job" "stale LAUNCHING owner has no confirmed PGID; delayed launch cannot be excluded"
            fi
        fi
    done <"$OWNER_FILE"
    local -a probe_jobs=()
    for job in "${JOB_ORDER[@]}"; do
        case "${REMOTE_PHASE[$job]}" in
            LAUNCHING|OWNED|RC_SEEN)
                [[ "${PGID[$job]:-}" =~ ^[1-9][0-9]*$ ]] && probe_jobs+=("$job")
                ;;
        esac
    done
    if [[ "${#probe_jobs[@]}" -gt 0 ]]; then
        if run_remote_rpc PROBE "${probe_jobs[@]}"; then
            for job in "${probe_jobs[@]}"; do
                case "${RPC_STATUS[$job]}" in
                    EXITED) OWNER_ACTIVE[$job]=0 ;;
                    *) mark_remote_unknown "$job" "stale owner is not proven exited: ${RPC_STATUS[$job]}" ;;
                esac
            done
        else
            for job in "${probe_jobs[@]}"; do mark_remote_unknown "$job" "stale owner probe failed"; done
        fi
    fi
    local unknown=0
    for job in "${JOB_ORDER[@]}"; do [[ "${REMOTE_PHASE[$job]:-}" == "REMOTE_UNKNOWN" ]] && unknown=1; done
    if [[ "$unknown" -eq 1 ]]; then write_quarantine; return 1; fi
    JOB_ORDER=(); JOB_KNOWN=(); TOKEN=(); PGID=(); REMOTE_PHASE=(); OWNER_ACTIVE=(); OWNER_CREATED=(); RESULT_STATE=(); JOB_HOST=(); JOB_RUN_ID=()
    : >"$OWNER_FILE"
}

if ! audit_stale_owners; then
    log_error "stale remote owner audit failed; mode '$MODE' is quarantined"
    flock -u "$LOCK_FD" || true
    exit 4
fi

{
    printf 'host=%s\n' "$(hostname)"
    printf 'pid=%s\n' "$$"
    printf 'run_id=%s\n' "$RUN_ID"
    printf 'mode=%s\n' "$MODE"
    printf 'started_at=%s\n' "$(date '+%F %T %Z')"
} | atomic_write "$LOCK_INFO" || exit 4

for seed in "${SEEDS[@]}"; do
    job="seed:$seed"; register_job "$job"; JOB_SEED[$job]="$seed"
    RESULT_STATE[$job]="PENDING"; REMOTE_PHASE[$job]="PENDING"; SIGNAL_DISPOSITION[$job]="NEVER"
    printf 'state=PENDING\nreason=\n' | atomic_write "$STATUS_DIR/$seed.state" || INITIAL_STATE_FAILURE=1
done

set_seed_state() {
    local job="$1" state="$2" reason="${3:-}" seed
    seed="${JOB_SEED[$job]}"
    RESULT_STATE[$job]="$state"
    [[ -z "$reason" ]] || FAILURE_REASON[$job]="$reason"
    {
        printf 'state=%s\n' "$state"
        printf 'reason=%s\n' "$reason"
        printf 'updated_at=%s\n' "$(date '+%F %T %Z')"
    } | atomic_write "$STATUS_DIR/$seed.state"
}

count_uvm_reports() {
    local log="$1"
    [[ -f "$log" ]] || { printf '0\n'; return; }
    grep -Ec '^[[:space:]]*UVM_(ERROR|FATAL)[[:space:]]+@' "$log" 2>/dev/null || true
}

write_monitor_events() {
    local job seed
    {
        printf 'seed\tcount\ttriggered_at\tline\n'
        for job in "${JOB_ORDER[@]}"; do
            [[ -n "${TRIGGER_COUNT[$job]:-}" ]] || continue
            seed="${JOB_SEED[$job]}"
            printf '%s\t%s\t%s\t%s\n' "$seed" "${TRIGGER_COUNT[$job]}" "${TRIGGER_TIME[$job]}" "${TRIGGER_LINE[$job]}"
        done
    } | atomic_write "$OUTPUT_DIR/monitor_events.tsv"
}

write_progress_records() {
    local final="${1:-0}" planned="${#SEEDS[@]}" running=0 pass=0 fail=0 infra=0 incomplete=0 aborted=0 pending=0 unknown=0
    local job state completed settled remaining progress
    local -a success=() failed=() not_run=()
    for job in "${JOB_ORDER[@]}"; do
        [[ "${REMOTE_PHASE[$job]:-}" == REMOTE_UNKNOWN ]] && unknown=$((unknown + 1))
        [[ "$job" == seed:* ]] || continue
        state="${RESULT_STATE[$job]}"
        case "$state" in
            PASS) pass=$((pass + 1)); success+=("${JOB_SEED[$job]}") ;;
            FAIL) fail=$((fail + 1)); failed+=("${JOB_SEED[$job]}") ;;
            INFRA) infra=$((infra + 1)); failed+=("${JOB_SEED[$job]}") ;;
            INCOMPLETE) incomplete=$((incomplete + 1)); failed+=("${JOB_SEED[$job]}") ;;
            ABORTED) aborted=$((aborted + 1)); failed+=("${JOB_SEED[$job]}") ;;
            PENDING)
                pending=$((pending + 1))
                if [[ "$final" -eq 1 && "${REMOTE_PHASE[$job]:-}" == PENDING ]]; then
                    not_run+=("${JOB_SEED[$job]}")
                fi
                ;;
        esac
        case "${REMOTE_PHASE[$job]:-}" in LAUNCHING|OWNED|RC_SEEN) running=$((running + 1));; esac
    done
    completed=$((pass + fail)); settled=$((pass + fail + infra + incomplete + aborted)); remaining=$((planned - completed))
    progress="planned=$planned completed=$completed remaining=$remaining running=$running pass=$pass fail=$fail infra=$infra incomplete=$incomplete aborted=$aborted remote_unknown_jobs=$unknown not_run=${#not_run[@]}"
    if [[ "$progress" != "$LAST_PROGRESS" || "$final" -eq 1 ]]; then log_info "$progress"; LAST_PROGRESS="$progress"; fi
    [[ "$RECORD_ON" -eq 1 ]] || return 0
    {
        printf 'run_id=%s\ntestcase=%s\ncfg=%s\nmode=%s\n' "$RUN_ID" "$TC" "$CFG" "$MODE"
        printf '%s\nsettled=%s\n' "$progress" "$settled"
        printf 'success_seeds=%s\nfailed_seeds=%s\nupdated_at=%s\n' "${success[*]:-}" "${failed[*]:-}" "$(date '+%F %T %Z')"
    } | atomic_write "$OUTPUT_DIR/progress.log" || return 1
    printf '%s\n' "${success[@]}" | atomic_write "$OUTPUT_DIR/success_seeds.txt" || return 1
    printf '%s\n' "${failed[@]}" | atomic_write "$OUTPUT_DIR/failed_seeds.txt" || return 1
    [[ "$final" -eq 0 ]] || printf '%s\n' "${not_run[@]}" | atomic_write "$OUTPUT_DIR/not_run_seeds.txt" || return 1
    {
        printf 'seed\tresult_state\tremote_phase\trc\tuvm_count\tlog\treason\n'
        for job in "${JOB_ORDER[@]}"; do
            [[ "$job" == seed:* ]] || continue
            printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "${JOB_SEED[$job]}" "${RESULT_STATE[$job]}" \
                "${REMOTE_PHASE[$job]}" "${RC[$job]:-}" "${UVM_COUNT[$job]:-}" \
                "${FINAL_LOG[$job]:-${LOG_PATH[$job]:-}}" "${FAILURE_REASON[$job]:-}"
        done
    } | atomic_write "$OUTPUT_DIR/results.tsv"
}

start_watcher() {
    local job="$1" seed log trigger controller_pid
    seed="${JOB_SEED[$job]}"; log="${LOG_PATH[$job]}"; trigger="$MONITOR_DIR/$seed.trigger"
    controller_pid="$BASHPID"
    rm -f "$trigger"
    (
        close_inherited_lock_fd
        local count=0 line tmp tail_pid fifo="$MONITOR_DIR/$seed.fifo.$$.$RANDOM"
        mkfifo "$fifo" || exit 1
        tail --pid="$controller_pid" -n +1 --retry -F "$log" >"$fifo" 2>/dev/null & tail_pid=$!
        trap 'kill "$tail_pid" 2>/dev/null || true; wait "$tail_pid" 2>/dev/null || true; rm -f "$fifo"' EXIT INT TERM
        while IFS= read -r line; do
            if [[ "$line" =~ ^[[:space:]]*UVM_(ERROR|FATAL)[[:space:]]+@ ]]; then
                count=$((count + 1))
                if (( count >= ERROR_LIMIT )); then
                    tmp="$trigger.tmp.$$.$RANDOM"
                    {
                        printf 'seed=%s\n' "$seed"
                        printf 'token=%s\n' "${TOKEN[$job]}"
                        printf 'count=%s\n' "$count"
                        printf 'line=%s\n' "$line"
                        printf 'triggered_at=%s\n' "$(date '+%F %T %Z')"
                    } >"$tmp" && mv -fT "$tmp" "$trigger"
                    break
                fi
            fi
        done <"$fifo"
    ) &
    WATCHER_PID[$job]=$!
}

stop_watcher() {
    local job="$1" pid
    pid="${WATCHER_PID[$job]:-}"
    [[ -n "$pid" ]] || return 0
    kill "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
    unset 'WATCHER_PID[$job]'
}

handle_monitor_triggers() {
    [[ "$MONITOR_ON" -eq 1 ]] || return 0
    local job seed trigger trigger_seed trigger_token count line triggered_at found=0 commit_failed=0
    for job in "${JOB_ORDER[@]}"; do
        [[ "$job" == seed:* ]] || continue
        seed="${JOB_SEED[$job]}"; trigger="$MONITOR_DIR/$seed.trigger"
        [[ -f "$trigger" && -z "${TRIGGER_HANDLED[$job]+x}" ]] || continue
        trigger_seed="$(read_key "$trigger" seed)"; trigger_token="$(read_key "$trigger" token)"
        count="$(read_key "$trigger" count)"; line="$(read_key "$trigger" line)"; triggered_at="$(read_key "$trigger" triggered_at)"
        if [[ "$trigger_seed" != "$seed" || "$trigger_token" != "${TOKEN[$job]}" || ! "$count" =~ ^[1-9][0-9]*$ ]]; then
            RECORD_INFRA_FAILURE=1; STOP_REASON="invalid monitor trigger for seed $seed"; STOP_REQUESTED=1; continue
        fi
        TRIGGER_HANDLED[$job]=1; TRIGGER_COUNT[$job]="$count"; TRIGGER_LINE[$job]="$line"; TRIGGER_TIME[$job]="$triggered_at"
        UVM_COUNT[$job]="$count"
        if ! set_seed_state "$job" FAIL "UVM error/fatal threshold $ERROR_LIMIT reached"; then
            RECORD_INFRA_FAILURE=1
            commit_failed=1
        fi
        found=1
    done
    [[ "$found" -eq 1 ]] || return 0
    write_monitor_events || commit_failed=1
    if [[ "$RECORD_ON" -eq 1 ]]; then write_progress_records 0 || commit_failed=1; fi
    if [[ "$commit_failed" -eq 0 ]]; then
        {
            printf 'run_id=%s\ncommitted_at=%s\ncommitted_epoch_ns=%s\n' \
                "$RUN_ID" "$(date '+%F %T %Z')" "$(date +%s%N)"
        } | atomic_write "$OUTPUT_DIR/monitor_failure.committed" || commit_failed=1
    fi
    if [[ "$commit_failed" -ne 0 ]]; then
        RECORD_INFRA_FAILURE=1; MONITOR_COMMIT="FAILED"; STOP_REASON="monitor failure record commit failed"
        printf 'run_id=%s\nfailed_at=%s\n' "$RUN_ID" "$(date '+%F %T %Z')" \
            | atomic_write "$OUTPUT_DIR/monitor_failure.uncommitted" || true
        STOP_REQUESTED=1
        return 1
    fi
    MONITOR_COMMIT="COMMITTED"
    for job in "${JOB_ORDER[@]}"; do
        [[ -n "${TRIGGER_COUNT[$job]:-}" ]] || continue
        log_error "seed ${JOB_SEED[$job]} reached UVM error/fatal threshold: count=${TRIGGER_COUNT[$job]} line=${TRIGGER_LINE[$job]}"
    done
    STOP_REASON="monitor error threshold reached"
    STOP_REQUESTED=1
}

recompute_peak_live_from_intervals() {
    local job events="$OUTPUT_DIR/.live_intervals.$$.$RANDOM" peak
    : >"$events"
    for job in "${JOB_ORDER[@]}"; do
        [[ "$job" == seed:* ]] || continue
        [[ "${START_EPOCH_NS[$job]:-}" =~ ^[0-9]+$ && "${END_EPOCH_NS[$job]:-}" =~ ^[0-9]+$ ]] || continue
        printf '%s\t1\n%s\t-1\n' "${START_EPOCH_NS[$job]}" "${END_EPOCH_NS[$job]}" >>"$events"
    done
    peak="$(sort -n -k1,1 -k2,2r "$events" | awk '{live += $2; if (live > peak) peak=live} END {print peak+0}')"
    rm -f "$events"
    (( peak > PEAK_LIVE )) && PEAK_LIVE="$peak"
    return 0
}

validate_ack_file() {
    local job="$1" file protocol job_id token pid pgid sid started_epoch_ns end
    file="${ACK_FILE[$job]}"
    protocol="$(read_key "$file" protocol)"; job_id="$(read_key "$file" job_id)"; token="$(read_key "$file" token)"
    pid="$(read_key "$file" pid)"; pgid="$(read_key "$file" pgid)"; sid="$(read_key "$file" sid)"
    started_epoch_ns="$(read_key "$file" started_epoch_ns)"; end="$(read_key "$file" end)"
    [[ "$protocol" == "$JOB_PROTOCOL" && "$job_id" == "$job" && "$token" == "${TOKEN[$job]}" \
        && "$pid" =~ ^[1-9][0-9]*$ && "$pid" == "$pgid" && "$pid" == "$sid" \
        && "$started_epoch_ns" =~ ^[0-9]+$ && "$end" == "ACK_END" ]] || return 1
    START_EPOCH_NS[$job]="$started_epoch_ns"
    PGID[$job]="$pgid"; REMOTE_PHASE[$job]="OWNED"
    printf '%s\n' "$pgid" | atomic_write "$PID_DIR/${job//:/_}.pid" || return 1
    write_owner_manifest
}

recover_file_over_ssh() {
    local path="$1" tmp cmd
    tmp="$path.recovered.$$.$RANDOM"
    cmd="$(quote_cmd cat "$path")"
    if ssh_retry "$cmd" >"$tmp" 2>/dev/null; then mv -fT "$tmp" "$path"; return 0; fi
    rm -f "$tmp"; return 1
}

validate_matching_rc_file() {
    local job="$1" file="$2" protocol job_id token pgid rc ended_epoch_ns end
    [[ -f "$file" && -r "$file" ]] || return 1
    protocol="$(read_key "$file" protocol)"; job_id="$(read_key "$file" job_id)"; token="$(read_key "$file" token)"
    pgid="$(read_key "$file" pgid)"; rc="$(read_key "$file" rc)"
    ended_epoch_ns="$(read_key "$file" ended_epoch_ns)"; end="$(read_key "$file" end)"
    [[ "$protocol" == "$JOB_PROTOCOL" && "$job_id" == "$job" && "$token" == "${TOKEN[$job]}" \
        && "$pgid" == "${PGID[$job]}" && "$rc" =~ ^[0-9]+$ \
        && "$ended_epoch_ns" =~ ^[0-9]+$ && "$end" == "RC_END" ]]
}

consume_matching_rc_file() {
    local job="$1" file="$2" rc ended_epoch_ns
    validate_matching_rc_file "$job" "$file" || return 1
    rc="$(read_key "$file" rc)"; ended_epoch_ns="$(read_key "$file" ended_epoch_ns)"
    RC[$job]="$rc"; END_EPOCH_NS[$job]="$ended_epoch_ns"
    if [[ "${REMOTE_PHASE[$job]}" == "OWNED" ]]; then
        REMOTE_PHASE[$job]="RC_SEEN"
    fi
}

recover_matching_rc_with_retry() {
    local job="$1" attempts="${2:-$SSH_RETRIES}" context="${3:-terminal}" file tmp cmd attempt
    local before_phase ssh_ok
    file="${RC_FILE[$job]}"
    for ((attempt = 1; attempt <= attempts; attempt++)); do
        handle_monitor_triggers || true
        before_phase="${REMOTE_PHASE[$job]}"
        if consume_matching_rc_file "$job" "$file"; then
            recompute_peak_live_from_intervals
            if [[ "$before_phase" == OWNED ]]; then write_owner_manifest || RECORD_INFRA_FAILURE=1; fi
            log_info "$job recovered matching RC context=$context attempt=$attempt source=shared"
            return 0
        fi
        tmp="$file.recovered.$$.$RANDOM"
        cmd="$(quote_cmd cat "$file")"
        ssh_ok=0
        if ssh_once_bounded "$RC_RECOVERY_READ_TIMEOUT" "$cmd" "${JOB_HOST[$job]:-$FINAL_REMOTE_HOST}" \
            >"$tmp" 2>/dev/null; then
            ssh_ok=1
        fi
        handle_monitor_triggers || true
        before_phase="${REMOTE_PHASE[$job]}"
        if [[ "$ssh_ok" -eq 1 ]]; then
            if validate_matching_rc_file "$job" "$tmp"; then
                if mv -fT "$tmp" "$file" && consume_matching_rc_file "$job" "$file"; then
                    recompute_peak_live_from_intervals
                    if [[ "$before_phase" == OWNED ]]; then write_owner_manifest || RECORD_INFRA_FAILURE=1; fi
                    log_info "$job recovered matching RC context=$context attempt=$attempt source=ssh"
                    return 0
                fi
            else
                log_warn "$job rejected non-matching RC candidate context=$context attempt=$attempt phase=${REMOTE_PHASE[$job]}"
            fi
        fi
        rm -f "$tmp"
        handle_monitor_triggers || true
        if (( attempt < attempts )); then sleep 1; fi
    done
    return 1
}

observe_rc() {
    local job file before_phase old_end interval_changed=0 phase_changed=0
    for job in "${JOB_ORDER[@]}"; do
        case "${REMOTE_PHASE[$job]:-}" in OWNED|RC_SEEN|EXIT_CONFIRMED) ;; *) continue ;; esac
        file="${RC_FILE[$job]:-}"; [[ -n "$file" ]] || continue
        before_phase="${REMOTE_PHASE[$job]}"; old_end="${END_EPOCH_NS[$job]:-}"
        if consume_matching_rc_file "$job" "$file"; then
            [[ "${END_EPOCH_NS[$job]}" == "$old_end" ]] || interval_changed=1
            [[ "$before_phase" != OWNED ]] || phase_changed=1
        fi
    done
    [[ "$interval_changed" -eq 0 ]] || recompute_peak_live_from_intervals
    [[ "$phase_changed" -eq 0 ]] || write_owner_manifest || RECORD_INFRA_FAILURE=1
}

create_managed_job() {
    local job="$1" operation="$2" seed="$3" log_path="$4" token safe ack rc_file launcher entry remote_cmd worker_cmd env_cmd
    register_job "$job"
    safe="${job//:/_}"; token="${RUN_ID}_${safe}_${RANDOM}${RANDOM}"
    ack="$STATUS_DIR/$safe.ack"; rc_file="$STATUS_DIR/$safe.rc"; launcher="$LAUNCHER_DIR/$safe.log"
    TOKEN[$job]="$token"; PGID[$job]="UNKNOWN"; REMOTE_PHASE[$job]="LAUNCHING"; SIGNAL_DISPOSITION[$job]="NEVER"
    OWNER_ACTIVE[$job]=1; OWNER_CREATED[$job]="$(date +%s)"; JOB_OPERATION[$job]="$operation"; JOB_SEED[$job]="$seed"
    JOB_HOST[$job]="$FINAL_REMOTE_HOST"; JOB_RUN_ID[$job]="$RUN_ID"
    ACK_FILE[$job]="$ack"; RC_FILE[$job]="$rc_file"; LOG_PATH[$job]="$log_path"
    rm -f "$ack" "$rc_file" "$launcher"
    if ! write_owner_manifest; then
        log_error "cannot persist LAUNCHING owner for $job"
        REMOTE_PHASE[$job]="REMOTE_UNKNOWN"
        UNKNOWN_REASON[$job]="initial owner manifest publish failed before SSH launch"
        RECORD_INFRA_FAILURE=1; STOP_REQUESTED=1; STOP_REASON="owner manifest publish failed"
        write_quarantine || true
        return 1
    fi

    local -a worker_args=(
        "$SCRIPT_PATH" --remote-worker --ack "$ack" --rc "$rc_file" --token "$token"
        --job-id "$job" --operation "$operation" --entry "$SCRIPT_DIR/eda01_entry.sh"
        --seed "$seed" --tc "$TC" --ts "$TS" --cfg "$CFG" --cfg-hash "$CFG_HASH" --mode "$MODE"
        --verbosity "$VERBOSITY" --wave "$WAVE_VALUE" --coverage "$CCOV_VALUE"
        --note "$NOTE" --timeout-ns "$TIMEOUT_NS" --plus-file "$FROZEN_PLUS_FILE"
        --bootstrap "$REMOTE_BOOTSTRAP"
    )
    if [[ "$operation" == "batch_run" ]]; then
        for arg in "${USER_PLUSARGS[@]}"; do worker_args+=(--plusarg "$arg"); done
    fi
    worker_cmd="$(quote_cmd "${worker_args[@]}")"; env_cmd="$(quote_cmd "EDA_REGRESSION_RUN_TOKEN=$token")"
    if [[ -n "${EDA_REGRESSION_REMOTE_ENTRY:-}" ]]; then
        env_cmd+=" $(quote_cmd "EDA_REGRESSION_REMOTE_ENTRY=$EDA_REGRESSION_REMOTE_ENTRY")"
    fi
    remote_cmd="exec nohup env $env_cmd setsid $worker_cmd > $(printf '%q' "$launcher") 2>&1 < /dev/null &"
    (close_inherited_lock_fd; exec ssh "${SSH_OPTS[@]}" "$FINAL_REMOTE_HOST" "$remote_cmd") >/dev/null 2>&1 \
        || log_warn "launch SSH was not confirmed for $job; waiting for ACK/token discovery"

    local deadline=$((SECONDS + LAUNCH_TIMEOUT))
    while [[ ! -f "$ack" && "$SECONDS" -lt "$deadline" ]]; do
        [[ "$operation" == "batch_run" ]] && handle_monitor_triggers || true
        [[ "$STOP_REQUESTED" -eq 0 ]] || break
        recover_file_over_ssh "$ack" && break
        sleep 1
    done
    [[ -f "$ack" ]] || recover_file_over_ssh "$ack" || true
    if [[ -f "$ack" ]] && validate_ack_file "$job"; then return 0; fi
    if discover_job_token "$job"; then
        log_warn "$job ACK recovered by token discovery (pgid=${PGID[$job]})"
        return 0
    fi
    mark_remote_unknown "$job" "$job launch/ACK state cannot be proven; delayed launch remains possible"
    return 1
}

fetch_stable_remote_log() {
    local job="$1" path snapshot
    path="${LOG_PATH[$job]}"; snapshot="$STATUS_DIR/${job//:/_}.final.log"
    local attempt before after tmp cmd bytes expected
    for ((attempt = 1; attempt <= SSH_RETRIES; attempt++)); do
        cmd="$(quote_cmd stat -Lc '%d:%i:%s:%Y' "$path")"
        before="$(ssh_once "$cmd" 2>/dev/null)" || { sleep 1; continue; }
        tmp="$snapshot.tmp.$$.$RANDOM"; cmd="$(quote_cmd cat "$path")"
        ssh_once "$cmd" >"$tmp" 2>/dev/null || { rm -f "$tmp"; sleep 1; continue; }
        after="$(ssh_once "$(quote_cmd stat -Lc '%d:%i:%s:%Y' "$path")" 2>/dev/null)" || { rm -f "$tmp"; sleep 1; continue; }
        bytes="$(wc -c <"$tmp" | tr -d ' ')"; expected="${before#*:*:}"; expected="${expected%%:*}"
        if [[ "$before" == "$after" && "$bytes" == "$expected" ]]; then
            mv -fT "$tmp" "$snapshot" || return 1
            FINAL_LOG[$job]="$snapshot"
            return 0
        fi
        rm -f "$tmp"; sleep 1
    done
    return 1
}

classify_seed_job() {
    local job="$1" log rc actual error_summary fatal_summary complete_pass
    log="${FINAL_LOG[$job]}"; rc="${RC[$job]:-}"
    actual="$(count_uvm_reports "$log")"; UVM_COUNT[$job]="$actual"
    error_summary="$(awk '/^[[:space:]]*UVM_ERROR[[:space:]]*:/ {v=$NF} END {print v+0}' "$log")"
    fatal_summary="$(awk '/^[[:space:]]*UVM_FATAL[[:space:]]*:/ {v=$NF} END {print v+0}' "$log")"
    complete_pass=0
    if grep -qE 'TEST CASE PASSED|TEST_PASS' "$log" && grep -qE '\$finish at simulation time|UVM Report Summary' "$log"; then complete_pass=1; fi
    if [[ "${RESULT_STATE[$job]}" == "FAIL" && -n "${TRIGGER_COUNT[$job]:-}" ]]; then return 0; fi
    if grep -qiE 'License checkout failed|Unable to checkout|SNPS_VCS_INTERNAL_ERROR|Cannot find.*simv|No space left on device|Stale file handle' "$log"; then
        set_seed_state "$job" INFRA "simulation infrastructure error"
    elif (( actual > 0 || error_summary > 0 || fatal_summary > 0 )); then
        set_seed_state "$job" FAIL "UVM errors/fatals in final log"
    elif [[ -z "$rc" ]]; then
        set_seed_state "$job" INFRA "remote process exited without recoverable RC"
    elif [[ "${SIGNAL_DISPOSITION[$job]}" != "NEVER" ]]; then
        if [[ "$complete_pass" -eq 1 ]]; then set_seed_state "$job" PASS "complete PASS log after signal"; else set_seed_state "$job" ABORTED "$STOP_REASON"; fi
    elif [[ "$rc" -eq 0 && "$complete_pass" -eq 1 ]]; then
        set_seed_state "$job" PASS "worker rc=0"
    elif [[ "$rc" -ne 0 ]]; then
        set_seed_state "$job" FAIL "worker rc=$rc"
    else
        set_seed_state "$job" INCOMPLETE "simulation ended without complete PASS markers"
    fi
}

maybe_publish_final_trigger() {
    local job="$1" seed count trigger line
    seed="${JOB_SEED[$job]}"; count="${UVM_COUNT[$job]:-0}"; trigger="$MONITOR_DIR/$seed.trigger"
    [[ "$MONITOR_ON" -eq 1 && "$count" -ge "$ERROR_LIMIT" && -z "${TRIGGER_HANDLED[$job]+x}" ]] || return 0
    line="$(grep -m1 -E '^[[:space:]]*UVM_(ERROR|FATAL)[[:space:]]+@' "${FINAL_LOG[$job]}" 2>/dev/null || echo 'final log error threshold')"
    {
        printf 'seed=%s\ntoken=%s\ncount=%s\nline=%s\ntriggered_at=%s\n' \
            "$seed" "${TOKEN[$job]}" "$count" "$line" "$(date '+%F %T %Z')"
    } | atomic_write "$trigger"
}

finalize_exited_job() {
    local job="$1" persist_failed=0 marker
    [[ "${REMOTE_PHASE[$job]}" == "EXIT_CONFIRMED" ]] || return 1
    stop_watcher "$job"
    if [[ -z "${RC[$job]:-}" ]]; then recover_matching_rc_with_retry "$job" 1 finalization || true; fi
    if ! fetch_stable_remote_log "$job"; then
        if [[ "$job" == seed:* && "${RESULT_STATE[$job]}" != "FAIL" ]]; then
            set_seed_state "$job" INFRA "stable remote log snapshot failed" || persist_failed=1
        elif [[ "$job" == compile ]]; then
            RESULT_STATE[$job]="INFRA"
            FAILURE_REASON[$job]="stable compile log snapshot failed"
        fi
    elif [[ "$job" == seed:* ]]; then
        classify_seed_job "$job" || persist_failed=1
        maybe_publish_final_trigger "$job" || persist_failed=1
    elif [[ "$job" == compile ]]; then
        if [[ -z "${RC[$job]:-}" ]]; then
            RESULT_STATE[$job]="INFRA"
            FAILURE_REASON[$job]="remote process exited without recoverable RC"
        elif [[ "${RC[$job]}" -eq 0 ]]; then
            RESULT_STATE[$job]="PASS"
        else
            RESULT_STATE[$job]="FAIL"
        fi
    fi
    if [[ "$job" == compile ]]; then
        log_info "finalized $job result=${RESULT_STATE[$job]:-compile} rc=${RC[$job]:-unknown}; awaiting fingerprint commit"
        return 0
    fi
    if [[ "$RECORD_ON" -eq 1 ]]; then write_progress_records 0 || persist_failed=1; fi
    marker="$STATUS_DIR/${job//:/_}.finalized"
    {
        printf 'job_id=%s\nresult_state=%s\nremote_phase=%s\ncommitted_at=%s\n' \
            "$job" "${RESULT_STATE[$job]}" "${REMOTE_PHASE[$job]}" "$(date '+%F %T %Z')"
    } | atomic_write "$marker" || persist_failed=1
    if [[ "$persist_failed" -ne 0 ]]; then
        mark_finalization_failure "$job" "seed finalization records were not fully committed"
        return 1
    fi
    OWNER_ACTIVE[$job]=0
    if ! write_owner_manifest; then
        OWNER_ACTIVE[$job]=1
        mark_finalization_failure "$job" "owner removal publish failed"
        return 1
    fi
    log_info "finalized $job result=${RESULT_STATE[$job]} rc=${RC[$job]:-unknown}"
}

settle_terminal_rpc_status() {
    local job="$1" status="$2" source="$3"
    case "$status" in EXITED|IDENTITY_MISMATCH) ;; *) return 2 ;; esac
    if [[ -z "${RC[$job]:-}" ]]; then
        log_info "$job terminal status=$status source=$source has no matching RC; entering bounded recovery"
        recover_matching_rc_with_retry "$job" "$SSH_RETRIES" "$source:$status" || true
    fi
    if [[ -n "${RC[$job]:-}" || "$status" == "EXITED" ]]; then
        REMOTE_PHASE[$job]="EXIT_CONFIRMED"
        write_owner_manifest || RECORD_INFRA_FAILURE=1
        finalize_exited_job "$job" || true
        return 0
    fi
    mark_remote_unknown "$job" "$source returned IDENTITY_MISMATCH without a matching RC after bounded recovery"
    return 1
}

probe_active_jobs() {
    local -a jobs=() job
    for job in "${JOB_ORDER[@]}"; do
        [[ "${OWNER_ACTIVE[$job]:-0}" -eq 1 && "${PGID[$job]:-}" =~ ^[1-9][0-9]*$ ]] || continue
        case "${REMOTE_PHASE[$job]}" in OWNED|RC_SEEN) jobs+=("$job");; esac
    done
    [[ "${#jobs[@]}" -gt 0 ]] || return 0
    if ! run_remote_rpc PROBE "${jobs[@]}"; then
        for job in "${jobs[@]}"; do mark_remote_unknown "$job" "remote probe response incomplete or SSH failed"; done
        return 1
    fi
    for job in "${jobs[@]}"; do
        case "${RPC_STATUS[$job]}" in
            ALIVE) : ;;
            EXITED|IDENTITY_MISMATCH) settle_terminal_rpc_status "$job" "${RPC_STATUS[$job]}" "remote probe" || true ;;
            *) mark_remote_unknown "$job" "remote probe returned ${RPC_STATUS[$job]}" ;;
        esac
    done
}

signal_active_jobs() {
    local op="$1" disposition="$2"; shift 2
    local -a jobs=("$@")
    local job
    [[ "${#jobs[@]}" -gt 0 ]] || return 0
    if ! run_remote_rpc "$op" "${jobs[@]}"; then
        for job in "${jobs[@]}"; do mark_remote_unknown "$job" "$op response incomplete or SSH failed"; done
        return 1
    fi
    {
        printf 'operation=%s\nsent_at=%s\nsent_epoch_ns=%s\n' \
            "$op" "$(date '+%F %T %Z')" "$(date +%s%N)"
        for job in "${jobs[@]}"; do printf 'job=%s status=%s\n' "$job" "${RPC_STATUS[$job]:-UNKNOWN}"; done
    } | atomic_write "$OUTPUT_DIR/${op,,}.sent" || RECORD_INFRA_FAILURE=1
    for job in "${jobs[@]}"; do
        case "${RPC_STATUS[$job]}" in
            SIGNALED) SIGNAL_DISPOSITION[$job]="$disposition" ;;
            EXITED|IDENTITY_MISMATCH) settle_terminal_rpc_status "$job" "${RPC_STATUS[$job]}" "$op" || true ;;
            *) mark_remote_unknown "$job" "$op returned ${RPC_STATUS[$job]}" ;;
        esac
    done
}

active_jobs() {
    local job
    for job in "${JOB_ORDER[@]}"; do
        [[ "${OWNER_ACTIVE[$job]:-0}" -eq 1 ]] || continue
        case "${REMOTE_PHASE[$job]}" in LAUNCHING|OWNED|RC_SEEN) printf '%s\n' "$job";; esac
    done
}
active_seed_count() { active_jobs | awk '$0 ~ /^seed:/ {n++} END {print n+0}'; }
pending_seed_exists() { local job; for job in "${JOB_ORDER[@]}"; do [[ "$job" == seed:* && "${RESULT_STATE[$job]}" == PENDING && "${REMOTE_PHASE[$job]}" == PENDING ]] && return 0; done; return 1; }

SIMV_PATH="$SIM_DIR/$MODE/exec/simv"
FINGERPRINT_PATH="$SIM_DIR/$MODE/exec/.eda_compile_fingerprint"
simv_exists() {
    ssh_retry "$(quote_cmd test -x "$SIMV_PATH")" >/dev/null 2>&1
}
simv_identity() {
    ssh_retry "$(quote_cmd stat -Lc '%d:%i:%s:%Y:%Z' "$SIMV_PATH")" 2>/dev/null
}
simv_hash() {
    ssh_retry "$(quote_cmd sha256sum "$SIMV_PATH")" 2>/dev/null | awk '{print $1}'
}
write_fingerprint() {
    local hash identity mock_meta
    mkdir -p "$(dirname "$FINGERPRINT_PATH")" || return 1
    if [[ -n "${EDA_REGRESSION_REMOTE_ENTRY:-}" ]]; then
        mock_meta="$SIM_DIR/$MODE/exec/mock_capabilities"
        [[ -f "$mock_meta" ]] || return 1
        [[ "$(read_key "$mock_meta" cfg)" == "$CFG" ]] || return 1
        [[ "$(read_key "$mock_meta" cfg_sha256)" == "$CFG_HASH" ]] || return 1
        [[ "$(read_key "$mock_meta" pl)" == "$VERBOSITY" ]] || return 1
        [[ "$(read_key "$mock_meta" wave)" == "$WAVE_VALUE" ]] || return 1
        [[ "$(read_key "$mock_meta" ccov)" == "$CCOV_VALUE" ]] || return 1
    fi
    hash="$(simv_hash)" || return 1
    identity="$(simv_identity)" || return 1
    {
        printf 'mode=%s\ntiming=rtl\nwave=%s\nccov=%s\n' "$MODE" "$WAVE_VALUE" "$CCOV_VALUE"
        printf 'simv=%s\nsha256=%s\nidentity=%s\n' "$SIMV_PATH" "$hash" "$identity"
        printf 'git_commit=%s\ncreated_at=%s\n' "$(git -C "$SIM_DIR" rev-parse HEAD 2>/dev/null || echo unknown)" "$(date '+%F %T %Z')"
    } | atomic_write "$FINGERPRINT_PATH"
}
verify_fingerprint() {
    local fp_wave fp_ccov fp_hash fp_identity current_hash current_identity
    if [[ ! -f "$FINGERPRINT_PATH" ]]; then
        [[ "$WAVE_ON" -eq 0 && "$COVERAGE_ON" -eq 0 ]] || return 1
        log_warn "using legacy simv without fingerprint; wave and coverage are disabled"
        return 0
    fi
    fp_wave="$(read_key "$FINGERPRINT_PATH" wave)"; fp_ccov="$(read_key "$FINGERPRINT_PATH" ccov)"
    fp_hash="$(read_key "$FINGERPRINT_PATH" sha256)"; fp_identity="$(read_key "$FINGERPRINT_PATH" identity)"
    [[ "$WAVE_ON" -eq 0 || "$fp_wave" == on ]] || return 1
    [[ "$COVERAGE_ON" -eq 0 || "$fp_ccov" == on ]] || return 1
    current_identity="$(simv_identity)"; current_hash="$(simv_hash)"
    [[ "$current_identity" == "$fp_identity" && "$current_hash" == "$fp_hash" ]]
}

wait_for_simv_visibility() {
    local deadline=$((SECONDS + LAUNCH_TIMEOUT))
    while (( SECONDS < deadline )); do
        simv_exists && return 0
        sleep 1
    done
    log_error "simv is not executable on $FINAL_REMOTE_HOST after ${LAUNCH_TIMEOUT}s: $SIMV_PATH"
    return 1
}

write_summary() {
    local result="$1" pass=0 fail=0 infra=0 incomplete=0 aborted=0 unknown=0 pending=0
    local completed=0 settled=0 remaining=0 not_run=0 job state
    for job in "${JOB_ORDER[@]}"; do
        [[ "${REMOTE_PHASE[$job]:-}" == REMOTE_UNKNOWN ]] && unknown=$((unknown+1))
        [[ "$job" == seed:* ]] || continue
        state="${RESULT_STATE[$job]}"
        case "$state" in
            PASS) pass=$((pass+1)) ;;
            FAIL) fail=$((fail+1)) ;;
            INFRA) infra=$((infra+1)) ;;
            INCOMPLETE) incomplete=$((incomplete+1)) ;;
            ABORTED) aborted=$((aborted+1)) ;;
            PENDING)
                pending=$((pending+1))
                [[ "${REMOTE_PHASE[$job]:-}" != PENDING ]] || not_run=$((not_run+1))
                ;;
        esac
    done
    completed=$((pass + fail))
    settled=$((pass + fail + infra + incomplete + aborted))
    remaining=$((${#SEEDS[@]} - completed))
    {
        printf 'result=%s\nrun_id=%s\ntestcase=%s\ncfg=%s\nmode=%s\n' "$result" "$RUN_ID" "$TC" "$CFG" "$MODE"
        printf 'planned=%s\ncompleted=%s\nsettled=%s\nremaining=%s\nnot_run=%s\n' \
            "${#SEEDS[@]}" "$completed" "$settled" "$remaining" "$not_run"
        printf 'pass=%s\nfail=%s\ninfra=%s\nincomplete=%s\naborted=%s\npending=%s\nremote_unknown_jobs=%s\n' \
            "$pass" "$fail" "$infra" "$incomplete" "$aborted" "$pending" "$unknown"
        printf 'peak_active=%s\n' "$PEAK_ACTIVE"
        printf 'peak_live=%s\n' "$PEAK_LIVE"
        printf 'monitor_commit=%s\nrecord_infra_failure=%s\nsimv_changed=%s\nstop_reason=%s\nresult_dir=%s\nupdated_at=%s\n' \
            "$MONITOR_COMMIT" "$RECORD_INFRA_FAILURE" "$SIMV_CHANGED" "$STOP_REASON" "$OUTPUT_DIR" "$(date '+%F %T %Z')"
    } | atomic_write "$OUTPUT_DIR/summary.log"
}

compute_batch_result() {
    local job
    [[ "$RECORD_INFRA_FAILURE" -eq 0 && "$SIMV_CHANGED" -eq 0 && ! -s "$QUARANTINE_FILE" ]] || return 1
    for job in "${JOB_ORDER[@]}"; do
        [[ "${OWNER_ACTIVE[$job]:-0}" -eq 0 ]] || return 1
        [[ "$job" != seed:* || "${RESULT_STATE[$job]}" == PASS ]] || return 1
        [[ "${REMOTE_PHASE[$job]:-}" != REMOTE_UNKNOWN ]] || return 1
    done
}

cleanup_controller() {
    [[ "$CLEANUP_DONE" -eq 0 && "$CLEANUP_RUNNING" -eq 0 ]] || return 0
    CLEANUP_RUNNING=1; STOP_REQUESTED=1
    local job
    # A watcher may have published a trigger just before an external signal.
    # Commit it before stopping watchers or sending TERM/KILL to remote jobs.
    handle_monitor_triggers || true
    for job in "${JOB_ORDER[@]}"; do stop_watcher "$job"; done
    for job in "${JOB_ORDER[@]}"; do
        [[ "${OWNER_ACTIVE[$job]:-0}" -eq 1 && "${REMOTE_PHASE[$job]:-}" == LAUNCHING && "${PGID[$job]:-}" == UNKNOWN ]] || continue
        discover_job_token "$job" || mark_remote_unknown "$job" "cleanup cannot prove LAUNCHING job identity; delayed launch remains possible"
    done
    observe_rc; probe_active_jobs || true
    handle_monitor_triggers || true
    for job in "${JOB_ORDER[@]}"; do
        [[ "${OWNER_ACTIVE[$job]:-0}" -eq 1 && "${REMOTE_PHASE[$job]:-}" == EXIT_CONFIRMED ]] || continue
        finalize_exited_job "$job" || true
    done
    mapfile -t alive < <(active_jobs)
    signal_active_jobs SIGNAL_TERM TERM_SENT "${alive[@]}" || true
    sleep 2; observe_rc; probe_active_jobs || true
    mapfile -t alive < <(active_jobs)
    signal_active_jobs SIGNAL_KILL KILL_SENT "${alive[@]}" || true
    sleep 1; observe_rc; probe_active_jobs || true
    for job in "${JOB_ORDER[@]}"; do
        [[ "${OWNER_ACTIVE[$job]:-0}" -eq 1 && "${REMOTE_PHASE[$job]:-}" == EXIT_CONFIRMED ]] || continue
        finalize_exited_job "$job" || true
    done
    for job in "${JOB_ORDER[@]}"; do
        if [[ "${OWNER_ACTIVE[$job]:-0}" -eq 1 && "${REMOTE_PHASE[$job]:-}" != REMOTE_UNKNOWN \
            && "${REMOTE_PHASE[$job]:-}" != EXIT_CONFIRMED ]]; then
            mark_remote_unknown "$job" "cleanup could not confirm remote exit"
        fi
    done
    write_quarantine || RECORD_INFRA_FAILURE=1
    write_owner_manifest || RECORD_INFRA_FAILURE=1
    if [[ -n "${SIMV_BASELINE:-}" && "$(simv_identity)" != "$SIMV_BASELINE" ]]; then SIMV_CHANGED=1; fi
    write_progress_records 1 || RECORD_INFRA_FAILURE=1
    if compute_batch_result; then FINAL_RESULT="PASS"; else FINAL_RESULT="FAIL"; fi
    if ! write_summary "$FINAL_RESULT"; then
        RECORD_INFRA_FAILURE=1
        FINAL_RESULT="FAIL"
        write_summary FAIL || {
            log_error "failed to publish summary.log; writing summary.failure"
            {
                printf 'result=FAIL\nreason=summary.log publish failed\n'
                printf 'run_id=%s\nmode=%s\nresult_dir=%s\nupdated_at=%s\n' \
                    "$RUN_ID" "$MODE" "$OUTPUT_DIR" "$(date '+%F %T %Z')"
            } | atomic_write "$OUTPUT_DIR/summary.failure" || true
        }
    fi
    rm -f "$LOCK_INFO"
    if [[ "$LOCK_HELD" -eq 1 ]]; then flock -u "$LOCK_FD" 2>/dev/null || true; LOCK_HELD=0; fi
    CLEANUP_DONE=1; CLEANUP_RUNNING=0
}

if [[ "$INITIAL_STATE_FAILURE" -ne 0 ]]; then
    RECORD_INFRA_FAILURE=1
    STOP_REASON="initial seed state publish failed"
    cleanup_controller
    trap - EXIT INT TERM
    exit 3
fi

PHASE="COMPILE"
if [[ "$DO_COMPILE" -eq 1 ]]; then
    rm -f "$FINGERPRINT_PATH"
    compile_job="compile"; register_job "$compile_job"; RESULT_STATE[$compile_job]="PENDING"
    log_info "compile once on $FINAL_REMOTE_HOST (mode=$MODE wave=$WAVE_VALUE ccov=$CCOV_VALUE)"
    if create_managed_job "$compile_job" compile "${SEEDS[0]}" "$LAUNCHER_DIR/compile.log"; then
        while [[ "${REMOTE_PHASE[$compile_job]}" != EXIT_CONFIRMED \
            && "${REMOTE_PHASE[$compile_job]}" != REMOTE_UNKNOWN && "$STOP_REQUESTED" -eq 0 ]]; do
            observe_rc; probe_active_jobs || true
            [[ "${OWNER_ACTIVE[$compile_job]}" -eq 0 ]] || sleep "$POLL_INTERVAL"
        done
    fi
    wait_for_simv_visibility || true
    compile_gate_failed=0
    [[ "${OWNER_ACTIVE[$compile_job]:-0}" -eq 1 ]] || compile_gate_failed=1
    [[ "${REMOTE_PHASE[$compile_job]}" != REMOTE_UNKNOWN ]] || compile_gate_failed=1
    [[ "${RESULT_STATE[$compile_job]:-INFRA}" == PASS ]] || compile_gate_failed=1
    [[ "${RC[$compile_job]:-125}" -eq 0 ]] || compile_gate_failed=1
    simv_exists || compile_gate_failed=1
    if simv_exists; then simv_gate_status=executable; else simv_gate_status=missing; fi
    log_info "compile gate owner=${OWNER_ACTIVE[$compile_job]:-unset} phase=${REMOTE_PHASE[$compile_job]:-unset} result=${RESULT_STATE[$compile_job]:-unset} rc=${RC[$compile_job]:-unset} simv=$simv_gate_status"
    if [[ "$compile_gate_failed" -ne 0 ]]; then
        STOP_REASON="compile managed job failed"
        cleanup_controller
        trap - EXIT INT TERM
        exit 5
    fi
    if write_fingerprint && {
        printf 'job_id=compile\nresult_state=PASS\nremote_phase=EXIT_CONFIRMED\nfingerprint=%s\ncommitted_at=%s\n' \
            "$FINGERPRINT_PATH" "$(date '+%F %T %Z')"
    } | atomic_write "$STATUS_DIR/compile.finalized"; then
        OWNER_ACTIVE[$compile_job]=0
        if ! write_owner_manifest; then
            OWNER_ACTIVE[$compile_job]=1
            mark_finalization_failure "$compile_job" "compile owner removal publish failed"
        fi
    else
        mark_finalization_failure "$compile_job" "compile fingerprint/finalization commit failed"
    fi
    if [[ "${OWNER_ACTIVE[$compile_job]}" -ne 0 || "$RECORD_INFRA_FAILURE" -ne 0 ]]; then
        STOP_REASON="compile fingerprint failed"
        cleanup_controller
        trap - EXIT INT TERM
        exit 5
    fi
else
    simv_exists || { log_error "simv is missing on $FINAL_REMOTE_HOST; use --compile: $SIMV_PATH"; STOP_REASON="simv missing"; cleanup_controller; trap - EXIT INT TERM; exit 5; }
    verify_fingerprint || { log_error "simv fingerprint does not satisfy requested wave/coverage"; STOP_REASON="fingerprint mismatch"; cleanup_controller; trap - EXIT INT TERM; exit 5; }
fi
SIMV_BASELINE="$(simv_identity)"

PHASE="SIMULATION"
write_progress_records 0 || RECORD_INFRA_FAILURE=1
while :; do
    handle_monitor_triggers || true
    observe_rc
    now=$SECONDS
    if (( now - LAST_PROBE_AT >= PROBE_INTERVAL )); then probe_active_jobs || true; LAST_PROBE_AT=$now; fi
    handle_monitor_triggers || true
    [[ "$RECORD_INFRA_FAILURE" -eq 0 ]] || { STOP_REQUESTED=1; [[ -n "$STOP_REASON" ]] || STOP_REASON="record infrastructure failure"; }
    [[ "$STOP_REQUESTED" -eq 0 ]] || break

    while (( $(active_seed_count) < JOBS )) && pending_seed_exists; do
        handle_monitor_triggers || true
        [[ "$STOP_REQUESTED" -eq 0 ]] || break
        if [[ "$(simv_identity)" != "$SIMV_BASELINE" ]]; then
            SIMV_CHANGED=1; STOP_REASON="simv changed during batch"; STOP_REQUESTED=1; break
        fi
        next_job=""
        for job in "${JOB_ORDER[@]}"; do
            if [[ "$job" == seed:* && "${RESULT_STATE[$job]}" == PENDING && "${REMOTE_PHASE[$job]}" == PENDING ]]; then next_job="$job"; break; fi
        done
        [[ -n "$next_job" ]] || break
        seed="${JOB_SEED[$next_job]}"
        log="$SIM_DIR/$MODE/log/tc=${TC}_ts=${TS}_cfg=${CFG}_seed=${seed}_rtl_${NOTE}.log"
        if [[ -e "$log" ]]; then set_seed_state "$next_job" INFRA "unique simulation log already exists"; REMOTE_PHASE[$next_job]="EXIT_CONFIRMED"; continue; fi
        if create_managed_job "$next_job" batch_run "$seed" "$log"; then
            [[ "$MONITOR_ON" -eq 0 ]] || start_watcher "$next_job"
            log_info "launched seed=$seed pgid=${PGID[$next_job]} log=$log"
            current_active="$(active_seed_count)"
            (( current_active <= JOBS )) || { RECORD_INFRA_FAILURE=1; STOP_REASON="scheduler exceeded jobs limit"; STOP_REQUESTED=1; }
            (( current_active <= PEAK_ACTIVE )) || PEAK_ACTIVE="$current_active"
        fi
        [[ "$STOP_REQUESTED" -eq 0 ]] || break
    done
    write_progress_records 0 || { RECORD_INFRA_FAILURE=1; STOP_REASON="progress record write failed"; STOP_REQUESTED=1; }
    handle_monitor_triggers || true
    if ! pending_seed_exists && [[ "$(active_seed_count)" -eq 0 ]]; then break; fi
    sleep "$POLL_INTERVAL"
done

cleanup_controller
trap - EXIT INT TERM
log_info "summary: $OUTPUT_DIR/summary.log"
[[ "$FINAL_RESULT" == PASS ]] && exit 0
exit 1
