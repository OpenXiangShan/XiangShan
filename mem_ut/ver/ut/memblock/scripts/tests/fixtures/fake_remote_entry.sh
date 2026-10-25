#!/usr/bin/env bash
set -euo pipefail

operation="$1"; shift
declare -A arg=()
for item in "$@"; do arg["${item%%=*}"]="${item#*=}"; done
mode="${arg[mode]}"; seed="${arg[seed]}"; tc="${arg[tc]}"; ts="${arg[ts]}"; cfg="${arg[cfg]}"; note="${arg[note]}"
mkdir -p "$FAKE_EDA_STATE_DIR"
printf '%s mode=%s seed=%s\n' "$operation" "$mode" "$seed" >>"$FAKE_EDA_STATE_DIR/entry.trace"

if [[ "$operation" == compile ]]; then
    exec_dir="$FAKE_SIM_DIR/$mode/exec"
    mkdir -p "$exec_dir"
    printf '#!/usr/bin/env bash\nexit 0\n' >"$exec_dir/simv"
    chmod +x "$exec_dir/simv"
    {
        printf 'cfg=%s\n' "$cfg"
        printf 'cfg_sha256=%s\n' "$(sha256sum "${arg[plus_file]}/$cfg.cfg" | awk '{print $1}')"
        printf 'pl=%s\nwave=%s\nccov=%s\n' "${arg[pl]}" "${arg[wave]}" "${arg[ccov]}"
    } >"$exec_dir/mock_capabilities"
    exit 0
fi

log_dir="$FAKE_SIM_DIR/$mode/log"
log="$log_dir/tc=${tc}_ts=${ts}_cfg=${cfg}_seed=${seed}_rtl_${note}.log"
mkdir -p "$log_dir"

case "$FAKE_EDA_SCENARIO" in
    jobs4_delayed|observe_batch)
        barrier="$FAKE_EDA_STATE_DIR/barrier"
        mkdir -p "$barrier"
        : >"$barrier/$seed.ready"
        deadline=$((SECONDS + 20))
        while [[ "$(find "$barrier" -name '*.ready' | wc -l)" -lt 4 && "$SECONDS" -lt "$deadline" ]]; do sleep 0.1; done
        [[ "$(find "$barrier" -name '*.ready' | wc -l)" -eq 4 ]] || exit 9
        ;;
    signal_term_exit|signal_kill_recover)
        printf 'UVM_ERROR @ 1: fake monitor stop\n' >"$log"
        while :; do sleep 1; done
        ;;
    token_mismatch)
        env -u EDA_REGRESSION_RUN_TOKEN bash -c 'printf "%s\n" "$$" >"$1/tokenless.pid"; while :; do sleep 1; done' \
            bash "$FAKE_EDA_STATE_DIR" >/dev/null 2>&1 &
        exit 0
        ;;
    external_signal_no_rc_complete)
        {
            printf 'TEST CASE PASSED\nUVM Report Summary\nUVM_ERROR : 0\nUVM_FATAL : 0\n'
            printf '$finish at simulation time 100\n'
        } >"$log"
        while :; do sleep 1; done
        ;;
    external_signal_no_rc_incomplete)
        printf 'simulation still running\n' >"$log"
        while :; do sleep 1; done
        ;;
    timeout_monitor)
        barrier="$FAKE_EDA_STATE_DIR/timeout_barrier"
        mkdir -p "$barrier"; : >"$barrier/$seed.ready"
        deadline=$((SECONDS + 20))
        while [[ "$(find "$barrier" -name '*.ready' | wc -l)" -lt 2 && "$SECONDS" -lt "$deadline" ]]; do sleep 0.1; done
        [[ "$(find "$barrier" -name '*.ready' | wc -l)" -eq 2 ]] || exit 9
        if [[ "$seed" == 601 ]]; then
            printf 'TEST CASE PASSED\nUVM Report Summary\nUVM_ERROR : 0\nUVM_FATAL : 0\n$finish at simulation time 100\n' >"$log"
            exit 0
        fi
        sleep 2
        printf 'UVM_ERROR @ 1: timeout monitor trigger\n' >"$log"
        while :; do sleep 1; done
        ;;
esac

{
    printf 'TEST CASE PASSED\n'
    printf 'UVM Report Summary\n'
    printf 'UVM_ERROR : 0\nUVM_FATAL : 0\n'
    printf '$finish at simulation time 100\n'
} >"$log"
