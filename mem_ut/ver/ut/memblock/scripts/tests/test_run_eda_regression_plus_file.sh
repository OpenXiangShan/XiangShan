#!/usr/bin/env bash
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_DIR="$(cd "$TEST_DIR/.." && pwd)"
SIM_DIR="$(cd "$SCRIPT_DIR/../sim" && pwd)"
SEQ_DIR="$(cd "$SCRIPT_DIR/../seq" && pwd)"
RUNNER="$SCRIPT_DIR/run_eda_regression.sh"
DEFAULT_DIR="$SEQ_DIR/plus_cfg"
SCALAR_DIR="$SEQ_DIR/csr_scalar_stress/cfg"
ROOT="$(mktemp -d "$SIM_DIR/.plus_file_test.XXXXXX")"
trap 'rm -rf "$ROOT"' EXIT

fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
read_config() { awk -F= -v key="$2" '$1 == key {sub(/^[^=]*=/, ""); print; exit}' "$1"; }
assert_eq() { [[ "$1" == "$2" ]] || fail "expected '$2', got '$1'"; }
assert_contains() { grep -Fq -- "$2" "$1" || fail "$1 missing: $2"; }

run_dry() {
    local name="$1" cfg="$2" plus_dir="${3-}" plusarg="${4-}" out
    out="$ROOT/$name"
    local -a args=(--tc basicTest --cfg "$cfg" --seed 1 --dry-run --output-dir "$out")
    [[ -z "$plus_dir" ]] || args+=(--plus-file "$plus_dir")
    [[ -z "$plusarg" ]] || args+=(--plusarg "$plusarg")
    "$RUNNER" "${args[@]}" >"$ROOT/$name.log" 2>&1
    printf '%s\n' "$out"
}

default_out="$(run_dry default default '' +MEMBLOCK_MAIN_TRANS_NUM=4321)"
assert_eq "$(read_config "$default_out/config.txt" source_plus_file)" "$(realpath "$DEFAULT_DIR")"
assert_eq "$(read_config "$default_out/config.txt" source_cfg)" "$(realpath "$DEFAULT_DIR/default.cfg")"
[[ -r "$default_out/frozen_plus_cfg/default.cfg" ]] || fail 'default frozen cfg missing'
assert_contains "$default_out/effective_plusargs.txt" '+MEMBLOCK_MAIN_TRANS_NUM=4321'
assert_eq "$(read_config "$default_out/config.txt" cfg_sha256)" \
    "$(sha256sum "$default_out/frozen_plus_cfg/default.cfg" | awk '{print $1}')"

declare -A expected=(
    [tc_csr_scalar_load_10k]=10010
    [tc_csr_scalar_store_10k]=10012
    [tc_csr_scalar_mixed_10k]=10016
    [tc_csr_scalar_store_sta_std_10k]=10025
)
for cfg in tc_csr_scalar_load_10k tc_csr_scalar_store_10k tc_csr_scalar_mixed_10k tc_csr_scalar_store_sta_std_10k; do
    before="$(sha256sum "$SCALAR_DIR/$cfg.cfg" | awk '{print $1}')"
    out="$(run_dry "$cfg" "$cfg" "$SCALAR_DIR")"
    assert_eq "$(read_config "$out/config.txt" source_plus_file)" "$(realpath "$SCALAR_DIR")"
    assert_eq "$(read_config "$out/config.txt" source_cfg)" "$(realpath "$SCALAR_DIR/$cfg.cfg")"
    assert_eq "$(read_config "$out/config.txt" frozen_cfg)" "$out/frozen_plus_cfg/$cfg.cfg"
    assert_eq "$(read_config "$out/config.txt" cfg_sha256)" \
        "$(sha256sum "$out/frozen_plus_cfg/$cfg.cfg" | awk '{print $1}')"
    assert_contains "$out/effective_plusargs.txt" "+MEMBLOCK_MAIN_TRANS_NUM=${expected[$cfg]}"
    assert_contains "$out/effective_plusargs.txt" '+MEMBLOCK_MAIN_MEM_RANGES_EN=1'
    assert_contains "$out/effective_plusargs.txt" '+MEMBLOCK_PMA_PMP_MODEL_EN=1'
    assert_contains "$out/effective_plusargs.txt" '+MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=0'
    [[ "$(grep -o '+MEMBLOCK_MAIN_TRANS_NUM=[^ ]*' "$out/effective_plusargs.txt" | wc -l)" -eq 1 ]] \
        || fail "$cfg has duplicate effective transaction count"
    assert_eq "$(sha256sum "$SCALAR_DIR/$cfg.cfg" | awk '{print $1}')" "$before"
done

relative="mem_ut/ver/ut/memblock/seq/csr_scalar_stress/cfg"
relative_out="$(run_dry relative tc_csr_scalar_load_10k "$relative")"
assert_eq "$(read_config "$relative_out/config.txt" source_plus_file)" "$(realpath "$SCALAR_DIR")"

expect_reject() {
    local name="$1" expected_text="$2" plus_dir="$3" cfg="${4:-default}" rc
    set +e
    "$RUNNER" --tc basicTest --cfg "$cfg" --seed 1 --dry-run \
        --plus-file "$plus_dir" --output-dir "$ROOT/reject_$name" >"$ROOT/reject_$name.log" 2>&1
    rc=$?
    set -e
    [[ "$rc" -eq 2 ]] || { cat "$ROOT/reject_$name.log" >&2; fail "$name expected rc=2 got rc=$rc"; }
    assert_contains "$ROOT/reject_$name.log" "$expected_text"
}

expect_reject missing 'plus-file directory is not readable and searchable' "$ROOT/missing"
printf 'not a directory\n' >"$ROOT/plain_file"
expect_reject regular_file 'plus-file directory is not readable and searchable' "$ROOT/plain_file"
expect_reject control 'without control characters' "$SCALAR_DIR"$'\n'

newline_dir="$ROOT/real"$'\n'"dir"
mkdir -p "$newline_dir"
ln -s "$newline_dir" "$ROOT/canonical_control"
expect_reject canonical_control 'canonical plus-file directory contains control characters' "$ROOT/canonical_control"

mkdir -p "$ROOT/escape_dir"
ln -s "$SCALAR_DIR/tc_csr_scalar_load_10k.cfg" "$ROOT/escape_dir/tc_csr_scalar_load_10k.cfg"
expect_reject symlink_escape 'cfg file escapes plus-file directory' "$ROOT/escape_dir" tc_csr_scalar_load_10k

mkdir -p "$ROOT/resolved_control"
control_target="$ROOT/resolved_control/target"$'\n'"name.cfg"
printf '+SAFE=1\n' >"$control_target"
ln -s "$(basename "$control_target")" "$ROOT/resolved_control/default.cfg"
expect_reject resolved_control 'resolved cfg path contains control characters' "$ROOT/resolved_control"

mkdir -p "$ROOT/invalid_cfg"
marker="$ROOT/cfg_injection_marker"
printf '+SAFE=1\n+EVIL=$$(touch>%s)\n' "$marker" >"$ROOT/invalid_cfg/injection.cfg"
expect_reject cfg_injection "invalid cfg plusarg value for 'EVIL'" "$ROOT/invalid_cfg" injection
[[ ! -e "$marker" ]] || fail 'cfg command injection executed'

printf '+SAFE=1\n+SAFE=2\n' >"$ROOT/invalid_cfg/duplicate.cfg"
expect_reject cfg_duplicate "duplicate cfg plusarg key 'SAFE'" "$ROOT/invalid_cfg" duplicate

printf '+VSEQ_MAIN=memblock_dispatch_real_smoke_vseq\n' >"$ROOT/invalid_cfg/formal_override.cfg"
"$RUNNER" --tc basicTest --cfg formal_override --seed 1 --dry-run \
    --plus-file "$ROOT/invalid_cfg" --output-dir "$ROOT/formal_override" \
    >"$ROOT/formal_override.log" 2>&1 || fail 'existing cfg formal override rejected'
grep -F 'VSEQ_MAIN=memblock_dispatch_real_smoke_vseq' "$ROOT/formal_override/config.txt" >/dev/null ||
    fail 'existing cfg formal override missing from effective plusargs'

printf '+SAFE=1\nnot_a_plusarg\n' >"$ROOT/invalid_cfg/malformed.cfg"
expect_reject cfg_malformed 'expected exactly +KEY=VALUE' "$ROOT/invalid_cfg" malformed

validated_cfgs=0
for cfg_dir in "$DEFAULT_DIR" "$SCALAR_DIR"; do
    while IFS= read -r cfg_file; do
        cfg="$(basename "$cfg_file" .cfg)"
        validated_cfgs=$((validated_cfgs + 1))
        "$RUNNER" --tc basicTest --cfg "$cfg" --seed 1 --dry-run \
            --plus-file "$cfg_dir" --output-dir "$ROOT/compat_$validated_cfgs" \
            >"$ROOT/compat_$validated_cfgs.log" 2>&1 ||
            fail "existing cfg rejected: $cfg_file"
    done < <(find "$cfg_dir" -maxdepth 1 -type f -name '*.cfg' -print | sort)
done
[[ "$validated_cfgs" -gt 0 ]] || fail 'no existing cfg files validated'

set +e
"$RUNNER" --tc basicTest --cfg '../tc_csr_scalar_load_10k' --seed 1 --dry-run \
    --plus-file "$SCALAR_DIR" --output-dir "$ROOT/reject_cfg_name" >"$ROOT/reject_cfg_name.log" 2>&1
rc=$?
set -e
[[ "$rc" -eq 2 ]] || fail "invalid cfg name expected rc=2 got rc=$rc"
assert_contains "$ROOT/reject_cfg_name.log" "invalid name '../tc_csr_scalar_load_10k'"

printf 'PASS: run_eda_regression plus-file dry-run matrix (%d existing cfg files)\n' "$validated_cfgs"
