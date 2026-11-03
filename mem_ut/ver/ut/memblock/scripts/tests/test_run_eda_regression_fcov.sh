#!/usr/bin/env bash
# 验证功能覆盖率总开关透传，以及旧二进制与本地模块配置的复用边界。
set -euo pipefail
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_DIR="$(cd "$TEST_DIR/.." && pwd)"
SIM_DIR="$(cd "$SCRIPT_DIR/../sim" && pwd)"
ROOT="$(mktemp -d "$SIM_DIR/.fcov_runner_test.XXXXXX")"
MODE="fake_fcov_$$"
mkdir -p "$ROOT/bin"
ln -s "$TEST_DIR/fixtures/fake_ssh.sh" "$ROOT/bin/ssh"
trap 'rm -rf "$ROOT" "$SIM_DIR/$MODE"' EXIT
fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
run_case() {
    local name="$1" expected="$2" rc
    shift 2
    mkdir -p "$ROOT/$name/state"
    set +e
    PATH="$ROOT/bin:$PATH" EDA_REGRESSION_REMOTE_ENTRY="$TEST_DIR/fixtures/fake_remote_entry.sh" \
    EDA_REGRESSION_REMOTE_BOOTSTRAP=: FAKE_EDA_SCENARIO=delayed_pass \
    FAKE_EDA_OUTPUT_DIR="$ROOT/$name/out" FAKE_EDA_STATE_DIR="$ROOT/$name/state" FAKE_SIM_DIR="$SIM_DIR" \
        "$SCRIPT_DIR/run_eda_regression.sh" --tc basicTest --mode "$MODE" --seed 11 --jobs 1 \
        --remote-host fakehost --output-dir "$ROOT/$name/out" --poll-interval 1 --probe-interval 1 \
        "$@" >"$ROOT/$name.log" 2>&1
    rc=$?
    set -e
    [[ "$rc" -eq "$expected" ]] || { cat "$ROOT/$name.log" >&2; fail "$name expected=$expected actual=$rc"; }
    if [[ "$expected" -eq 5 ]] && [[ -e "$ROOT/$name/state/entry.trace" ]]; then
        fail "$name launched a worker despite fingerprint mismatch"
    fi
}
FP="$SIM_DIR/$MODE/exec/.eda_compile_fingerprint"
run_case enabled 0 --compile --fcov
grep -qx 'fcov=on' "$FP" || fail 'functional coverage missing from fingerprint'
grep -qx 'fcov=on' "$SIM_DIR/$MODE/exec/mock_capabilities" || fail 'functional coverage not forwarded'
run_case reuse 0 --no-compile --fcov
run_case enabled_binary_disabled_request 5 --no-compile --no-fcov
cp "$FP" "$ROOT/good_fingerprint"
sed -i 's/^user_compile_cfg_sha256=.*/user_compile_cfg_sha256=stale/' "$FP"
run_case stale_modules 5 --no-compile --fcov
cp "$ROOT/good_fingerprint" "$FP"
run_case disabled 0 --compile --no-fcov
grep -qx 'fcov=off' "$SIM_DIR/$MODE/exec/mock_capabilities" || fail 'disable not forwarded'
run_case disabled_binary_enabled_request 5 --no-compile --fcov
sed -i '/^fcov=/d; /^user_compile_cfg_sha256=/d' "$FP"
run_case old_off_fingerprint 0 --no-compile --no-fcov
rm "$FP"
run_case legacy_enabled_request 5 --no-compile --fcov
printf 'PASS: functional coverage forwarding, exact enable match, local configuration hash, legacy compatibility\n'
