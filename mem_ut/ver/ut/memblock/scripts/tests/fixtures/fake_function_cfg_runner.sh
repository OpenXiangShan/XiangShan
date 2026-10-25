#!/usr/bin/env bash
set -euo pipefail

: "${FAKE_FUNCTION_CFG_TRACE:?missing FAKE_FUNCTION_CFG_TRACE}"

cfg=""; plus_file=""; output_dir=""; compile_mode=""; trans_num="ABSENT"; check_trigger="ABSENT"
while [[ $# -gt 0 ]]; do
    case "$1" in
        --cfg) cfg="$2"; shift 2 ;;
        --plus-file) plus_file="$2"; shift 2 ;;
        --output-dir) output_dir="$2"; shift 2 ;;
        --plusarg)
            case "$2" in
                +MEMBLOCK_MAIN_TRANS_NUM=*) trans_num="${2#*=}" ;;
                +MEMBLOCK_CHECK_TRIGGER_EN=*) check_trigger="${2#*=}" ;;
            esac
            shift 2
            ;;
        --compile|--no-compile) compile_mode="$1"; shift ;;
        --tc|--ts|--mode|--count|--jobs|--verbosity|--timeout-ns) shift 2 ;;
        --coverage|--no-coverage|--wave|--no-wave) shift ;;
        *) printf 'unexpected fake runner argument: %s\n' "$1" >&2; exit 2 ;;
    esac
done

[[ -n "$cfg" && -n "$plus_file" && -n "$output_dir" && -n "$compile_mode" ]] || exit 2
printf '%s\t%s\t%s\t%s\t%s\n' "$cfg" "$plus_file" "$compile_mode" "$trans_num" "$check_trigger" >>"$FAKE_FUNCTION_CFG_TRACE"
mkdir -p "$output_dir"
printf 'result=PASS\n' >"$output_dir/summary.log"
