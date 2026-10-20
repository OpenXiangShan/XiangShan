#!/usr/bin/env bash

set -uo pipefail

operation="${1:-}"
shift || true

declare -A arg=()
for item in "$@"; do
    key="${item%%=*}"
    value="${item#*=}"
    arg[$key]="$value"
done

sim_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mode="${arg[mode]:-base_fun}"
seed="${arg[seed]:-1}"
tc="${arg[tc]:-mockPass}"
ts="${arg[ts]:-virtual_base_sequence}"
cfg="${arg[cfg]:-default}"
plus_file="${arg[plus_file]:-}"
note="${arg[note]:-mock}"

case "$operation" in
    compile)
        compile_sleep_file="$sim_dir/.eda_regression/mock_controls/${mode}.compile_sleep"
        compile_fail_file="$sim_dir/.eda_regression/mock_controls/${mode}.compile_fail"
        if [[ -f "$compile_sleep_file" ]]; then
            read -r compile_sleep <"$compile_sleep_file"
            sleep "$compile_sleep"
        fi
        if [[ -f "$compile_fail_file" ]]; then
            printf 'MOCK COMPILE FAILED\n' >&2
            exit 1
        fi
        mkdir -p "$sim_dir/$mode/exec"
        printf '#!/usr/bin/env bash\nexit 0\n' >"$sim_dir/$mode/exec/simv"
        chmod +x "$sim_dir/$mode/exec/simv"
        {
            printf 'cfg=%s\n' "$cfg"
            printf 'cfg_sha256=%s\n' "$(sha256sum "$plus_file/$cfg.cfg" | awk '{print $1}')"
            printf 'pl=%s\nwave=%s\nccov=%s\n' "${arg[pl]:-}" "${arg[wave]:-}" "${arg[ccov]:-}"
        } >"$sim_dir/$mode/exec/mock_capabilities"
        printf 'MOCK COMPILE PASSED\n'
        ;;
    batch_run)
        mkdir -p "$sim_dir/$mode/log"
        log="$sim_dir/$mode/log/tc=${tc}_ts=${ts}_cfg=${cfg}_seed=${seed}_rtl_${note}.log"
        fail_seed=""
        fail_delay=0
        fail_hold=30
        sleep_seconds=0
        plus_arg="${arg[plus_arg]:-}"
        effective_plusargs="$(make -s -C "$sim_dir" eda_print_effective_plusargs \
            cfg="$cfg" plus_file="$plus_file" plus_arg="$plus_arg" | paste -sd' ' -)" || exit 2
        plus_arg="$effective_plusargs"
        for token in $plus_arg; do
            case "$token" in
                +MOCK_FAIL_SEED=*) fail_seed="${token#*=}" ;;
                +MOCK_FAIL_DELAY=*) fail_delay="${token#*=}" ;;
                +MOCK_FAIL_HOLD=*) fail_hold="${token#*=}" ;;
                +MOCK_SLEEP=*) sleep_seconds="${token#*=}" ;;
            esac
        done
        {
            printf 'MOCK_SEED=%s\n' "$seed"
            printf 'MOCK_PLUSARGS=%s\n' "$plus_arg"
            printf 'MOCK_CFG=%s\n' "$cfg"
            printf 'MOCK_PL=%s\n' "${arg[pl]:-}"
            printf 'MOCK_WAVE=%s\n' "${arg[wave]:-}"
            printf 'MOCK_CCOV=%s\n' "${arg[ccov]:-}"
        } >"$log"
        if [[ "$seed" == "$fail_seed" ]]; then
            sleep "$fail_delay"
            printf 'UVM_ERROR @ 100: mock failure for seed %s\n' "$seed" >>"$log"
            sleep "$fail_hold"
            exit 1
        fi
        sleep "$sleep_seconds"
        {
            printf 'UVM Report Summary\n'
            printf 'UVM_ERROR : 0\n'
            printf 'UVM_FATAL : 0\n'
            printf 'TEST CASE PASSED\n'
            printf '$finish at simulation time 100\n'
        } >>"$log"
        ;;
    *)
        printf 'unknown mock operation: %s\n' "$operation" >&2
        exit 2
        ;;
esac
