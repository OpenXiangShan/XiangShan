#!/usr/bin/env bash
set -euo pipefail

args=("$@")
count=${#args[@]}
src="${args[$((count - 2))]}"; dst="${args[$((count - 1))]}"

if [[ -n "${EDA_REGRESSION_RUN_TOKEN:-}" && "$dst" == *.rc ]]; then
    base="$(basename "$dst")"
    case "$FAKE_EDA_SCENARIO" in
        delayed_pass|jobs4_delayed|signal_term_exit)
            delay="${FAKE_EDA_RC_DELAY:-2}"
            [[ "$FAKE_EDA_SCENARIO" != jobs4_delayed ]] || delay=8
            nohup bash -c 'sleep "$1"; /bin/mv -fT "$2" "$3"' bash "$delay" "$src" "$dst" \
                >/dev/null 2>&1 &
            exit 0
            ;;
        seed_no_rc|token_mismatch|invalid_rc_sequence|external_signal_no_rc_complete|external_signal_no_rc_incomplete|timeout_monitor)
            if [[ "$base" == seed_*.rc ]]; then rm -f "$src"; exit 0; fi
            ;;
        compile_no_rc)
            if [[ "$base" == compile.rc ]]; then rm -f "$src"; exit 0; fi
            ;;
    esac
fi

exec /bin/mv "$@"
