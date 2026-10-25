#!/usr/bin/env bash
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_DIR="$(cd "$TEST_DIR/.." && pwd)"
SIM_DIR="$(cd "$SCRIPT_DIR/../sim" && pwd)"
MATRIX="$SCRIPT_DIR/run_function_cfg_cov_20261022.sh"
FAKE_RUNNER="$TEST_DIR/fixtures/fake_function_cfg_runner.sh"
ROOT="$(mktemp -d "$SIM_DIR/.function_cfg_matrix_test.XXXXXX")"
trap 'rm -rf "$ROOT"' EXIT

fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
TRACE="$ROOT/trace.tsv"
: >"$TRACE"
FAKE_FUNCTION_CFG_TRACE="$TRACE" \
FUNCTION_CFG_RUNNER="$FAKE_RUNNER" \
FUNCTION_CFG_RESULT_ROOT="$ROOT/results" \
TRANS_NUM=4321 COUNT=1 JOBS=1 \
    "$MATRIX" >"$ROOT/matrix.log" 2>&1

[[ "$(wc -l <"$TRACE")" -eq 30 ]] || fail 'matrix did not invoke 30 cases'
[[ "$(awk -F '\t' '$3 == "--compile" {n++} END {print n+0}' "$TRACE")" -eq 1 ]] || fail 'compile count mismatch'
[[ "$(awk -F '\t' 'NR == 1 && $3 == "--compile" {print 1}' "$TRACE")" == 1 ]] || fail 'first case did not compile'
[[ "$(awk -F '\t' 'NR > 1 && $3 != "--no-compile" {n++} END {print n+0}' "$TRACE")" -eq 0 ]] || fail 'non-first case compile mode mismatch'
[[ "$(awk -F '\t' 'NR <= 26 && $4 == "4321" {n++} END {print n+0}' "$TRACE")" -eq 26 ]] || fail 'default TRANS_NUM routing mismatch'
[[ "$(awk -F '\t' 'NR > 26 && $4 == "ABSENT" {n++} END {print n+0}' "$TRACE")" -eq 4 ]] || fail 'scalar TRANS_NUM override was not removed'
[[ "$(awk -F '\t' '$5 == "0" {n++} END {print n+0}' "$TRACE")" -eq 30 ]] || fail 'CHECK_TRIGGER routing mismatch'
[[ "$(awk -F '\t' 'NR <= 26 && $2 ~ /\/seq\/plus_cfg$/ {n++} END {print n+0}' "$TRACE")" -eq 26 ]] || fail 'default plus-file routing mismatch'
[[ "$(awk -F '\t' 'NR > 26 && $2 ~ /\/seq\/csr_scalar_stress\/cfg$/ {n++} END {print n+0}' "$TRACE")" -eq 4 ]] || fail 'scalar plus-file routing mismatch'

cat >"$ROOT/invalid_cases" <<'EOF'
default|basicTest|virtual_base_sequence|unknown_profile
EOF
: >"$ROOT/invalid_trace.tsv"
set +e
FAKE_FUNCTION_CFG_TRACE="$ROOT/invalid_trace.tsv" \
FUNCTION_CFG_RUNNER="$FAKE_RUNNER" \
FUNCTION_CFG_RESULT_ROOT="$ROOT/invalid_results" \
FUNCTION_CFG_CASES_FILE="$ROOT/invalid_cases" \
    "$MATRIX" >"$ROOT/invalid.log" 2>&1
rc=$?
set -e
[[ "$rc" -eq 2 ]] || fail "unknown profile expected rc=2 got rc=$rc"
[[ ! -s "$ROOT/invalid_trace.tsv" ]] || fail 'unknown profile invoked runner'
grep -Fq 'unknown functional cfg profile' "$ROOT/invalid.log" || fail 'unknown profile error missing'

cat >"$ROOT/traversal_cases" <<'EOF'
../escape|basicTest|virtual_base_sequence|default
EOF
set +e
FAKE_FUNCTION_CFG_TRACE="$ROOT/traversal_trace.tsv" \
FUNCTION_CFG_RUNNER="$FAKE_RUNNER" \
FUNCTION_CFG_RESULT_ROOT="$ROOT/traversal_results" \
FUNCTION_CFG_CASES_FILE="$ROOT/traversal_cases" \
    "$MATRIX" >"$ROOT/traversal.log" 2>&1
rc=$?
set -e
[[ "$rc" -eq 2 ]] || fail "traversal case expected rc=2 got rc=$rc"
[[ ! -e "$ROOT/escape" ]] || fail 'traversal case created output outside result root'
grep -Fq 'invalid CASE name: ../escape' "$ROOT/traversal.log" || fail 'traversal error missing'

cat >"$ROOT/dotdot_cases" <<'EOF'
..|basicTest|virtual_base_sequence|default
EOF
set +e
FAKE_FUNCTION_CFG_TRACE="$ROOT/dotdot_trace.tsv" \
FUNCTION_CFG_RUNNER="$FAKE_RUNNER" \
FUNCTION_CFG_RESULT_ROOT="$ROOT/dotdot_results" \
FUNCTION_CFG_CASES_FILE="$ROOT/dotdot_cases" \
    "$MATRIX" >"$ROOT/dotdot.log" 2>&1
rc=$?
set -e
[[ "$rc" -eq 2 ]] || fail "dotdot case expected rc=2 got rc=$rc"
[[ ! -e "$ROOT/summary.log" ]] || fail 'dotdot case wrote summary outside result root'
grep -Fq 'invalid CASE name: ..' "$ROOT/dotdot.log" || fail 'dotdot error missing'

printf 'PASS: functional cfg matrix profile routing\n'
