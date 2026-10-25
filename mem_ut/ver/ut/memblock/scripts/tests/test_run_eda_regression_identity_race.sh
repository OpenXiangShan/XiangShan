#!/usr/bin/env bash
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_DIR="$(cd "$TEST_DIR/.." && pwd)"
SIM_DIR="$(cd "$SCRIPT_DIR/../sim" && pwd)"
RUNNER="$SCRIPT_DIR/run_eda_regression.sh"
FIXTURES="$TEST_DIR/fixtures"
ROOT="$(mktemp -d "$SIM_DIR/.identity_race_test.XXXXXX")"
FAKE_BIN="$ROOT/bin"
mkdir -p "$FAKE_BIN"
ln -s "$FIXTURES/fake_ssh.sh" "$FAKE_BIN/ssh"
ln -s "$FIXTURES/fake_mv.sh" "$FAKE_BIN/mv"
ln -s "$FIXTURES/fake_sort.sh" "$FAKE_BIN/sort"

cleanup() {
    local pid
    while read -r pid; do kill -KILL "$pid" 2>/dev/null || true; done < <(pgrep -f "$ROOT" || true)
    rm -rf "$ROOT"
}
trap cleanup EXIT

fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
assert_file_value() {
    local file="$1" key="$2" expected="$3" actual
    actual="$(awk -F= -v key="$key" '$1 == key {sub(/^[^=]*=/, ""); print; exit}' "$file")"
    [[ "$actual" == "$expected" ]] || fail "$file $key expected=$expected actual=$actual"
}
assert_contains() { grep -Fq -- "$2" "$1" || fail "$1 missing: $2"; }
assert_not_exists() { [[ ! -e "$1" ]] || fail "unexpected path: $1"; }

prepare_simv() {
    local mode="$1"
    mkdir -p "$SIM_DIR/$mode/exec"
    printf '#!/usr/bin/env bash\nexit 0\n' >"$SIM_DIR/$mode/exec/simv"
    chmod +x "$SIM_DIR/$mode/exec/simv"
}

run_case() {
    local name="$1" scenario="$2" expected_rc="$3" mode="$4" output state rc
    output="$ROOT/$name/out"; state="$ROOT/$name/state"
    shift 4
    mkdir -p "$state"
    set +e
    PATH="$FAKE_BIN:$PATH" \
    EDA_REGRESSION_REMOTE_ENTRY="$FIXTURES/fake_remote_entry.sh" \
    EDA_REGRESSION_REMOTE_BOOTSTRAP=: \
    FAKE_EDA_SCENARIO="$scenario" FAKE_EDA_OUTPUT_DIR="$output" FAKE_EDA_STATE_DIR="$state" \
    FAKE_SIM_DIR="$SIM_DIR" FAKE_EDA_RC_DELAY=2 \
        "$RUNNER" --tc basicTest --cfg default --mode "$mode" --remote-host fakehost \
        --output-dir "$output" --poll-interval 1 --probe-interval 1 --no-wave --no-coverage \
        "$@" >"$ROOT/$name/controller.log" 2>&1
    rc=$?
    set -e
    [[ "$rc" -eq "$expected_rc" ]] || {
        cat "$ROOT/$name/controller.log" >&2
        fail "$name exit expected=$expected_rc actual=$rc"
    }
}

run_external_signal_case() {
    local name="$1" scenario="$2" mode="$3" seed="$4" output state pid deadline rc
    output="$ROOT/$name/out"; state="$ROOT/$name/state"
    mkdir -p "$state"
    set +e
    PATH="$FAKE_BIN:$PATH" \
    EDA_REGRESSION_REMOTE_ENTRY="$FIXTURES/fake_remote_entry.sh" EDA_REGRESSION_REMOTE_BOOTSTRAP=: \
    FAKE_EDA_SCENARIO="$scenario" FAKE_EDA_OUTPUT_DIR="$output" FAKE_EDA_STATE_DIR="$state" \
    FAKE_SIM_DIR="$SIM_DIR" \
        "$RUNNER" --tc basicTest --cfg default --mode "$mode" --remote-host fakehost \
        --output-dir "$output" --poll-interval 1 --probe-interval 1 --no-wave --no-coverage \
        --no-compile --seed "$seed" --jobs 1 --no-monitor >"$ROOT/$name/controller.log" 2>&1 &
    pid=$!
    set -e
    deadline=$((SECONDS + 20))
    while [[ ! -f "$state/entry.trace" ]] || ! grep -q '^batch_run ' "$state/entry.trace"; do
        (( SECONDS < deadline )) || { kill -KILL "$pid" 2>/dev/null || true; fail "$name did not launch seed"; }
        sleep 0.1
    done
    kill -TERM "$pid"
    set +e; wait "$pid"; rc=$?; set -e
    [[ "$rc" -eq 143 ]] || { cat "$ROOT/$name/controller.log" >&2; fail "$name exit expected=143 actual=$rc"; }
}

case_delayed_pass() {
    local name=delayed_pass mode=fake_identity_delayed_$$ out
    out="$ROOT/$name/out"
    prepare_simv "$mode"
    run_case "$name" delayed_pass 0 "$mode" --no-compile --seed 101 --jobs 1 --no-monitor
    assert_file_value "$out/summary.log" pass 1
    assert_file_value "$out/summary.log" remote_unknown_jobs 0
    assert_contains "$ROOT/$name/controller.log" 'status=IDENTITY_MISMATCH source=remote probe has no matching RC'
    assert_contains "$ROOT/$name/state/ssh.trace" 'status=IDENTITY_MISMATCH'
    assert_not_exists "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv"
    rm -rf "$SIM_DIR/$mode"
}

case_seed_no_rc() {
    local name=seed_no_rc mode=fake_identity_no_rc_$$ out
    out="$ROOT/$name/out"
    prepare_simv "$mode"
    run_case "$name" seed_no_rc 1 "$mode" --no-compile --seed 102 --jobs 1 --no-monitor
    assert_file_value "$out/summary.log" infra 1
    assert_file_value "$out/summary.log" remote_unknown_jobs 0
    assert_contains "$out/results.tsv" $'102\tINFRA\tEXIT_CONFIRMED'
    assert_not_exists "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv"
    rm -rf "$SIM_DIR/$mode"
}

case_token_mismatch() {
    local name=token_mismatch mode=fake_identity_mismatch_$$ out
    out="$ROOT/$name/out"
    prepare_simv "$mode"
    run_case "$name" token_mismatch 1 "$mode" --no-compile --seed 103 --jobs 1 --no-monitor
    assert_file_value "$out/summary.log" remote_unknown_jobs 1
    [[ -s "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv" ]] || fail 'missing mismatch quarantine'
    assert_contains "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv" 'IDENTITY_MISMATCH without a matching RC after bounded recovery'
    assert_contains "$ROOT/$name/state/ssh.trace" 'status=IDENTITY_MISMATCH source=real_rpc'
    tokenless_pid="$(cat "$ROOT/$name/state/tokenless.pid")"
    kill -0 "$tokenless_pid" 2>/dev/null || fail 'tokenless live member was not preserved for mismatch test'
    kill -KILL "$tokenless_pid" 2>/dev/null || true
    rm -f "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv" "$SIM_DIR/.eda_regression/locks/$mode.owners.tsv"
    rm -rf "$SIM_DIR/$mode"
}

case_jobs4() {
    local name=jobs4 mode=fake_identity_jobs4_$$ out seed
    out="$ROOT/$name/out"
    prepare_simv "$mode"
    run_case "$name" jobs4_delayed 0 "$mode" --no-compile --seeds 201,202,203,204 --jobs 4 --no-monitor
    assert_file_value "$out/summary.log" pass 4
    assert_file_value "$out/summary.log" peak_active 4
    assert_file_value "$out/summary.log" remote_unknown_jobs 0
    for seed in 201 202 203 204; do
        assert_contains "$ROOT/$name/state/ssh.trace" "job=seed:$seed status=IDENTITY_MISMATCH"
    done
    assert_not_exists "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv"
    rm -rf "$SIM_DIR/$mode"
}

case_observe_batch_recompute() {
    local name=observe_batch mode=fake_identity_observe_$$ out
    out="$ROOT/$name/out"
    prepare_simv "$mode"
    run_case "$name" observe_batch 0 "$mode" --no-compile --seeds 211,212,213,214 --jobs 4 --no-monitor
    assert_file_value "$out/summary.log" pass 4
    [[ "$(cat "$ROOT/$name/state/peak_recompute.count")" == 1 ]] || fail 'observe_rc did not batch peak recomputation'
    rm -rf "$SIM_DIR/$mode"
}

case_compile_no_rc() {
    local name=compile_no_rc mode=fake_identity_compile_$$ out
    out="$ROOT/$name/out"
    run_case "$name" compile_no_rc 5 "$mode" --compile --seed 301 --jobs 1 --no-monitor
    assert_file_value "$out/summary.log" remote_unknown_jobs 0
    assert_contains "$ROOT/$name/state/entry.trace" 'compile mode='
    assert_contains "$ROOT/$name/controller.log" 'finalized compile result=INFRA rc=unknown'
    if grep -q '^batch_run ' "$ROOT/$name/state/entry.trace"; then fail 'seed launched after compile INFRA'; fi
    assert_not_exists "$SIM_DIR/$mode/exec/.eda_compile_fingerprint"
    assert_not_exists "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv"
    rm -rf "$SIM_DIR/$mode"
}

case_external_signal_missing_rc() {
    local name mode out
    for name in external_complete external_incomplete; do
        mode="fake_identity_${name}_$$"; out="$ROOT/$name/out"
        prepare_simv "$mode"
        if [[ "$name" == external_complete ]]; then
            run_external_signal_case "$name" external_signal_no_rc_complete "$mode" 411
        else
            run_external_signal_case "$name" external_signal_no_rc_incomplete "$mode" 412
        fi
        assert_file_value "$out/summary.log" infra 1
        assert_file_value "$out/summary.log" remote_unknown_jobs 0
        assert_contains "$out/results.tsv" $'\tINFRA\tEXIT_CONFIRMED\t'
        assert_not_exists "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv"
        rm -rf "$SIM_DIR/$mode"
    done
}

case_signal_term() {
    local name=signal_term mode=fake_identity_term_$$ out
    out="$ROOT/$name/out"
    prepare_simv "$mode"
    run_case "$name" signal_term_exit 1 "$mode" --no-compile --seed 401 --jobs 1 --monitor --error-limit 1
    assert_contains "$ROOT/$name/controller.log" 'status=EXITED source=SIGNAL_TERM has no matching RC'
    assert_contains "$ROOT/$name/state/ssh.trace" 'op=SIGNAL_TERM job=seed:401 status=EXITED'
    assert_file_value "$out/summary.log" remote_unknown_jobs 0
    assert_not_exists "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv"
    rm -rf "$SIM_DIR/$mode"
}

case_signal_kill() {
    local name=signal_kill mode=fake_identity_kill_$$ out
    out="$ROOT/$name/out"
    prepare_simv "$mode"
    run_case "$name" signal_kill_recover 1 "$mode" --no-compile --seed 402 --jobs 1 --monitor --error-limit 1
    assert_contains "$ROOT/$name/controller.log" 'status=IDENTITY_MISMATCH source=SIGNAL_KILL'
    assert_contains "$ROOT/$name/state/ssh.trace" 'op=SIGNAL_TERM job=seed:402 status=SIGNALED'
    assert_contains "$ROOT/$name/state/ssh.trace" 'op=SIGNAL_KILL job=seed:402 status=IDENTITY_MISMATCH'
    assert_file_value "$out/summary.log" remote_unknown_jobs 0
    assert_not_exists "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv"
    rm -rf "$SIM_DIR/$mode"
}

case_invalid_rc_sequence() {
    local name=invalid_rc mode=fake_identity_invalid_$$ out rc_file
    out="$ROOT/$name/out"; rc_file="$out/status/seed_501.rc"
    prepare_simv "$mode"
    run_case "$name" invalid_rc_sequence 0 "$mode" --no-compile --seed 501 --jobs 1 --no-monitor --ssh-retries 6
    assert_file_value "$out/summary.log" pass 1
    assert_contains "$ROOT/$name/state/ssh.trace" 'kind=ssh_failure'
    assert_contains "$ROOT/$name/state/ssh.trace" 'kind=wrong_token'
    assert_contains "$ROOT/$name/state/ssh.trace" 'kind=wrong_job'
    assert_contains "$ROOT/$name/state/ssh.trace" 'kind=wrong_pgid'
    assert_contains "$ROOT/$name/state/ssh.trace" 'kind=truncated'
    assert_contains "$ROOT/$name/state/ssh.trace" 'kind=valid'
    for kind in wrong_token wrong_job wrong_pgid truncated; do
        assert_contains "$ROOT/$name/state/ssh.trace" "kind=$kind canonical=absent"
    done
    for attempt in 2 3 4 5; do
        assert_contains "$ROOT/$name/controller.log" "attempt=$attempt phase=OWNED"
    done
    assert_file_value "$rc_file" token "$(awk -F= '$1 == "token" {print $2}' "$out/status/seed_501.ack")"
    assert_not_exists "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv"
    rm -rf "$SIM_DIR/$mode"
}

case_timeout_monitor_commit() {
    local name=timeout_monitor mode=fake_identity_timeout_$$ out start elapsed committed sent
    out="$ROOT/$name/out"
    prepare_simv "$mode"
    start=$SECONDS
    run_case "$name" timeout_monitor 1 "$mode" --no-compile --seeds 601,602 --jobs 2 --monitor --error-limit 1
    elapsed=$((SECONDS - start))
    (( elapsed < 30 )) || fail "bounded recovery exceeded 30 seconds: $elapsed"
    assert_contains "$ROOT/$name/state/ssh.trace" 'kind=stalled'
    [[ -s "$out/monitor_failure.committed" ]] || fail 'monitor failure was not committed during recovery'
    [[ -s "$out/signal_term.sent" ]] || fail 'TERM record missing after monitor commit'
    committed="$(awk -F= '$1 == "committed_epoch_ns" {print $2}' "$out/monitor_failure.committed")"
    sent="$(awk -F= '$1 == "sent_epoch_ns" {print $2}' "$out/signal_term.sent")"
    [[ "$committed" =~ ^[0-9]+$ && "$sent" =~ ^[0-9]+$ && "$committed" -lt "$sent" ]] \
        || fail 'monitor commit did not precede signal send'
    rm -f "$SIM_DIR/.eda_regression/locks/$mode.quarantine.tsv" "$SIM_DIR/.eda_regression/locks/$mode.owners.tsv"
    rm -rf "$SIM_DIR/$mode"
}

case_delayed_pass
case_seed_no_rc
case_token_mismatch
case_jobs4
case_observe_batch_recompute
case_compile_no_rc
case_external_signal_missing_rc
case_signal_term
case_signal_kill
case_invalid_rc_sequence
case_timeout_monitor_commit
printf 'PASS: run_eda_regression identity race fake matrix\n'
