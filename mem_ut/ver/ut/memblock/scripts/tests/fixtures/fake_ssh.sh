#!/usr/bin/env bash
set -euo pipefail

while [[ $# -gt 0 ]]; do
    case "$1" in
        -o) shift 2 ;;
        --) shift; break ;;
        -*) shift ;;
        *) host="$1"; shift; break ;;
    esac
done
[[ -n "${host:-}" && $# -gt 0 ]] || exit 2
remote_cmd="$*"

trace() { printf '%s\n' "$*" >>"$FAKE_EDA_STATE_DIR/ssh.trace"; }
safe_job() { printf '%s\n' "${1//:/_}"; }
rc_path_for_job() { printf '%s/status/%s.rc\n' "$FAKE_EDA_OUTPUT_DIR" "$(safe_job "$1")"; }

write_matching_rc() {
    local job="$1" token="$2" pgid="$3" path
    path="$(rc_path_for_job "$job")"
    {
        printf 'protocol=EDA_REGRESSION_JOB_V1\n'
        printf 'job_id=%s\n' "$job"
        printf 'token=%s\n' "$token"
        printf 'pgid=%s\n' "$pgid"
        printf 'rc=0\n'
        printf 'ended_at=fake\n'
        printf 'ended_epoch_ns=%s\n' "$(date +%s%N)"
        printf 'end=RC_END\n'
    } >"$path.tmp.fake"
    /bin/mv -fT "$path.tmp.fake" "$path"
}

emit_rpc() {
    local request items response request_id op count expected_hash line end_id hash
    local protocol job token pgid extra status path scenario="$FAKE_EDA_SCENARIO"
    request="$(mktemp "$FAKE_EDA_STATE_DIR/rpc.request.XXXXXX")"
    items="$request.items"; response="$request.response"
    cat >"$request"
    exec 3<"$request"
    IFS= read -r line <&3; [[ "$line" == 'PROTO EDA_REGRESSION_RPC_V1' ]]
    IFS= read -r line <&3; request_id="${line#REQUEST_ID }"
    IFS= read -r line <&3; op="${line#OP }"
    IFS= read -r line <&3; count="${line#COUNT }"
    IFS= read -r line <&3; expected_hash="${line#HASH }"
    : >"$items"; : >"$response"
    while IFS= read -r line <&3; do
        if [[ "$line" == END\ * ]]; then end_id="${line#END }"; break; fi
        IFS=$'\t' read -r protocol job token pgid extra <<<"$line"
        [[ "$protocol" == ITEM && -z "${extra:-}" ]]
        printf 'ITEM\t%s\t%s\t%s\n' "$job" "$token" "$pgid" >>"$items"
        printf '%s\t%s\t%s\n' "$token" "$pgid" "$op" >"$FAKE_EDA_STATE_DIR/$(safe_job "$job").meta"
        path="$(rc_path_for_job "$job")"
        case "$scenario:$op" in
            delayed_pass:PROBE|jobs4_delayed:PROBE)
                if [[ -f "$path" ]]; then status=EXITED; else status=IDENTITY_MISMATCH; fi
                ;;
            observe_batch:PROBE) status=EXITED ;;
            seed_no_rc:PROBE|compile_no_rc:PROBE) status=EXITED ;;
            invalid_rc_sequence:PROBE|timeout_monitor:PROBE)
                if [[ "$job" == seed:601 || "$scenario" == invalid_rc_sequence ]]; then status=IDENTITY_MISMATCH; else status=ALIVE; fi
                ;;
            signal_term_exit:PROBE|signal_kill_recover:PROBE) status=ALIVE ;;
            signal_term_exit:SIGNAL_TERM)
                kill -TERM -- "-$pgid" 2>/dev/null || true
                status=EXITED
                ;;
            signal_kill_recover:SIGNAL_TERM) status=SIGNALED ;;
            signal_kill_recover:SIGNAL_KILL)
                write_matching_rc "$job" "$token" "$pgid"
                kill -KILL -- "-$pgid" 2>/dev/null || true
                status=IDENTITY_MISMATCH
                ;;
            external_signal_no_rc_complete:SIGNAL_TERM|external_signal_no_rc_incomplete:SIGNAL_TERM|timeout_monitor:SIGNAL_TERM)
                kill -TERM -- "-$pgid" 2>/dev/null || true
                status=SIGNALED
                ;;
            external_signal_no_rc_complete:PROBE|external_signal_no_rc_incomplete:PROBE)
                if kill -0 -- "-$pgid" 2>/dev/null; then status=ALIVE; else status=EXITED; fi
                ;;
            *) status=ALIVE ;;
        esac
        trace "RPC scenario=$scenario op=$op job=$job status=$status rc_present=$([[ -f "$path" ]] && echo 1 || echo 0)"
        printf 'ITEM\t%s\t%s\n' "$job" "$status" >>"$response"
    done
    exec 3<&-
    [[ "$end_id" == "$request_id" && "$(sha256sum "$items" | awk '{print $1}')" == "$expected_hash" ]]
    hash="$(sha256sum "$response" | awk '{print $1}')"
    printf 'PROTO EDA_REGRESSION_RPC_V1\n'
    printf 'REQUEST_ID %s\n' "$request_id"
    printf 'OP %s\n' "$op"
    printf 'COUNT %s\n' "$count"
    printf 'HASH %s\n' "$hash"
    cat "$response"
    printf 'END %s\n' "$request_id"
    rm -f "$request" "$items" "$response"
}

emit_recovery_candidate() {
    local path="$1" base job meta token pgid op attempt scenario="$FAKE_EDA_SCENARIO"
    base="$(basename "$path" .rc)"; job="${base/_/:}"
    meta="$FAKE_EDA_STATE_DIR/$base.meta"
    [[ -f "$meta" ]] || exit 1
    IFS=$'\t' read -r token pgid op <"$meta"
    attempt_file="$FAKE_EDA_STATE_DIR/$base.recovery_attempt"
    attempt=0; [[ ! -f "$attempt_file" ]] || attempt="$(cat "$attempt_file")"
    attempt=$((attempt + 1)); printf '%s\n' "$attempt" >"$attempt_file"
    if [[ "$scenario" == timeout_monitor ]]; then
        trace "RC_CANDIDATE job=$job attempt=$attempt kind=stalled"
        sleep 30
        exit 1
    fi
    if [[ "$scenario" == jobs4_delayed ]]; then
        trace "RC_CANDIDATE job=$job attempt=$attempt kind=valid_jobs4"
        printf 'protocol=EDA_REGRESSION_JOB_V1\njob_id=%s\ntoken=%s\npgid=%s\nrc=0\nended_at=fake\nended_epoch_ns=%s\nend=RC_END\n' \
            "$job" "$token" "$pgid" "$(date +%s%N)"
        return 0
    fi
    if [[ "$scenario" != invalid_rc_sequence ]]; then
        exec /bin/cat "$path"
    fi
    case "$attempt" in
        1) trace "RC_CANDIDATE job=$job attempt=$attempt kind=ssh_failure canonical=$([[ -e "$path" ]] && echo present || echo absent)"; exit 1 ;;
        2) token="wrong_${token}"; kind=wrong_token ;;
        3) job="seed:2147483646"; kind=wrong_job ;;
        4) pgid=$((pgid + 1)); kind=wrong_pgid ;;
        5)
            trace "RC_CANDIDATE job=$job attempt=$attempt kind=truncated canonical=$([[ -e "$path" ]] && echo present || echo absent)"
            printf 'protocol=EDA_REGRESSION_JOB_V1\njob_id=%s\ntoken=%s\n' "$job" "$token"
            exit 0
            ;;
        *) kind=valid ;;
    esac
    trace "RC_CANDIDATE job=$job attempt=$attempt kind=$kind canonical=$([[ -e "$path" ]] && echo present || echo absent)"
    printf 'protocol=EDA_REGRESSION_JOB_V1\njob_id=%s\ntoken=%s\npgid=%s\nrc=0\nended_at=fake\nended_epoch_ns=%s\nend=RC_END\n' \
        "$job" "$token" "$pgid" "$(date +%s%N)"
}

if [[ "$FAKE_EDA_SCENARIO" == token_mismatch && "$remote_cmd" == *'--remote-rpc'* ]]; then
    request="$(mktemp "$FAKE_EDA_STATE_DIR/real_rpc.request.XXXXXX")"
    response="$request.response"
    cat >"$request"
    bash -c "$remote_cmd" <"$request" >"$response"
    while IFS=$'\t' read -r tag job status; do
        [[ "$tag" == ITEM ]] || continue
        trace "RPC scenario=token_mismatch op=PROBE job=$job status=$status source=real_rpc"
    done <"$response"
    cat "$response"
    rm -f "$request" "$response"
elif [[ "$remote_cmd" == *'--remote-rpc'* ]]; then
    emit_rpc
elif [[ "$FAKE_EDA_SCENARIO" =~ ^(invalid_rc_sequence|jobs4_delayed|timeout_monitor)$ && "$remote_cmd" == cat\ *status/*\.rc ]]; then
    eval "set -- $remote_cmd"
    emit_recovery_candidate "$2"
else
    exec bash -c "$remote_cmd"
fi
