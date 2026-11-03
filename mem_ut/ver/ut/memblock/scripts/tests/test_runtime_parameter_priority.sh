#!/usr/bin/env bash
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_DIR="$(cd "$TEST_DIR/.." && pwd)"
SIM_DIR="$(cd "$SCRIPT_DIR/../sim" && pwd)"
ROOT="$(mktemp -d "$SIM_DIR/.runtime_priority_test.XXXXXX")"
trap 'rm -rf "$ROOT"' EXIT
mkdir -p "$ROOT/cfg" "$ROOT/bin"
fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
value() { sed -n "s/^$2=//p" "$1"; }
expect_arg() {
    local file="$1" key="$2" expected="$3" count
    count="$(tr ' ' '\n' <"$file" | grep -c "^+$key=" || true)"
    [[ "$count" == 1 ]] || fail "$file $key count=$count"
    tr ' ' '\n' <"$file" | grep -qxF "+$key=$expected" || fail "$file expected +$key=$expected"
}
printf '+MEMBLOCK_MAIN_TRANS_NUM=2\n+MEMBLOCK_CHECK_TRIGGER_EN=1\n+OTHER=old\n+UVM_VERBOSITY=UVM_HIGH\n+UVM_TIMEOUT=98765\n+VSEQ_MAIN=cfg_sequence\n' >"$ROOT/cfg/preset.cfg"
printf '+OTHER=old\n' >"$ROOT/cfg/fallback.cfg"

dry() {
    local name="$1" cfg="$2"; shift 2
    "$SCRIPT_DIR/run_eda_regression.sh" --tc basicTest --mode priority_test --seed 17 \
        --cfg "$cfg" --plus-file "$ROOT/cfg" --output-dir "$ROOT/$name" --dry-run "$@" \
        >"$ROOT/$name.log" 2>&1 || { cat "$ROOT/$name.log" >&2; fail "$name dry-run"; }
}
dry cfg preset
[[ "$(value "$ROOT/cfg/config.txt" verbosity)" == UVM_HIGH ]] || fail 'runner default masked cfg verbosity'
[[ "$(value "$ROOT/cfg/config.txt" timeout_ns)" == 98765 ]] || fail 'runner default masked cfg timeout'
[[ "$(value "$ROOT/cfg/config.txt" ts)" == cfg_sequence ]] || fail 'runner default masked cfg VSEQ'
dry single preset --plusarg +MEMBLOCK_CHECK_TRIGGER_EN=0
expect_arg "$ROOT/single/effective_plusargs.txt" MEMBLOCK_MAIN_TRANS_NUM 2
expect_arg "$ROOT/single/effective_plusargs.txt" MEMBLOCK_CHECK_TRIGGER_EN 0
expect_arg "$ROOT/single/effective_plusargs.txt" OTHER old
dry multi preset --plusarg +OTHER=new --plusarg +MEMBLOCK_MAIN_TRANS_NUM=31 \
    --verbosity UVM_NONE --timeout-ns 222 --ts explicit_sequence
expect_arg "$ROOT/multi/effective_plusargs.txt" OTHER new
expect_arg "$ROOT/multi/effective_plusargs.txt" UVM_VERBOSITY UVM_NONE
expect_arg "$ROOT/multi/effective_plusargs.txt" UVM_TIMEOUT 222
expect_arg "$ROOT/multi/effective_plusargs.txt" VSEQ_MAIN explicit_sequence
dry fallback fallback
expect_arg "$ROOT/fallback/effective_plusargs.txt" UVM_VERBOSITY UVM_LOW
expect_arg "$ROOT/fallback/effective_plusargs.txt" UVM_TIMEOUT 100000000
expect_arg "$ROOT/fallback/effective_plusargs.txt" VSEQ_MAIN virtual_base_sequence
dry debug preset --debug
expect_arg "$ROOT/debug/effective_plusargs.txt" UVM_VERBOSITY UVM_DEBUG
dry debug_explicit preset --verbosity UVM_NONE --no-wave --debug
expect_arg "$ROOT/debug_explicit/effective_plusargs.txt" UVM_VERBOSITY UVM_NONE
[[ "$(value "$ROOT/debug_explicit/config.txt" wave)" == 0 ]] || fail 'debug masked explicit no-wave'

for tools in vcs xrun; do
    make -s -C "$SIM_DIR" eda_print_effective_plusargs SIM_TOOLS="$tools" \
        cfg=preset plus_file="$ROOT/cfg" >"$ROOT/$tools.effective"
    expect_arg "$ROOT/$tools.effective" UVM_VERBOSITY UVM_HIGH
    expect_arg "$ROOT/$tools.effective" UVM_TIMEOUT 98765
    expect_arg "$ROOT/$tools.effective" VSEQ_MAIN cfg_sequence
    make -s -C "$SIM_DIR" eda_print_effective_plusargs SIM_TOOLS="$tools" \
        cfg=fallback plus_file="$ROOT/cfg" >"$ROOT/$tools.fallback"
    expect_arg "$ROOT/$tools.fallback" UVM_VERBOSITY UVM_MEDIUM
    for kind in cfg explicit; do
        args=()
        if [[ "$kind" == explicit ]]; then
            args+=(pl=UVM_NONE timeout_ns=333 ts=make_sequence 'plus_arg=+OTHER=new +MEMBLOCK_CHECK_TRIGGER_EN=0')
        fi
        make -n -C "$SIM_DIR" batch_run SIM_TOOLS="$tools" cfg=preset plus_file="$ROOT/cfg" \
            tc=basicTest seed=17 mode=priority_test wave=off "${args[@]}" >"$ROOT/$tools.$kind.command"
        if [[ "$kind" == explicit ]]; then
            expect_arg "$ROOT/$tools.$kind.command" UVM_VERBOSITY UVM_NONE
            expect_arg "$ROOT/$tools.$kind.command" UVM_TIMEOUT 333
            expect_arg "$ROOT/$tools.$kind.command" VSEQ_MAIN make_sequence
            expect_arg "$ROOT/$tools.$kind.command" OTHER new
        else
            expect_arg "$ROOT/$tools.$kind.command" UVM_VERBOSITY UVM_HIGH
            expect_arg "$ROOT/$tools.$kind.command" UVM_TIMEOUT 98765
            expect_arg "$ROOT/$tools.$kind.command" VSEQ_MAIN cfg_sequence
        fi
    done
done

reject() {
    local name="$1"; shift
    local rc=0
    "$SCRIPT_DIR/run_eda_regression.sh" --tc basicTest --cfg preset --plus-file "$ROOT/cfg" \
        --seed 17 --dry-run --output-dir "$ROOT/reject_$name" "$@" >"$ROOT/reject_$name.log" 2>&1 || rc=$?
    [[ "$rc" == 2 ]] || fail "$name expected rc=2 got $rc"
}
reject duplicate --plusarg +OTHER=one --plusarg +OTHER=two
reject malformed --plusarg +OTHER=
reject illegal --plusarg '+BAD-KEY=1'
for key in UVM_VERBOSITY UVM_TIMEOUT VSEQ_MAIN UVM_TESTNAME ntb_random_seed TEST_MODE wave_type gen_wave; do
    reject "reserved_$key" --plusarg "+$key=other"
done
reject invalid_verbosity --verbosity UVM_INVALID
reject invalid_timeout --timeout-ns 0
reject duplicate_verbosity --verbosity UVM_HIGH --verbosity UVM_LOW
reject duplicate_ts --ts one --ts two
printf 'PASS: runtime parameter priority, effective records and VCS/XRUN commands\n'
