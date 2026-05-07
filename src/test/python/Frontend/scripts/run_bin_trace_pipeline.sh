#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FRONTEND_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${FRONTEND_DIR}/../../../.." && pwd)"
PIPELINE_STAGE="init"
PIPELINE_REASON="not_started"

print_exit_reason() {
  local status=$?
  if [[ "${status}" -eq 0 ]]; then
    echo "[frontend] exit: code=${status} stage=${PIPELINE_STAGE} reason=${PIPELINE_REASON}"
  else
    echo "[frontend][error] exit: code=${status} stage=${PIPELINE_STAGE} reason=${PIPELINE_REASON}" >&2
  fi
}

trap print_exit_reason EXIT

usage() {
  cat <<'EOF'
Usage:
  ./scripts/run_bin_trace_pipeline.sh <bin_path> [trace_jsonl_path] [nemu_log_path] [pytest_target]

Environment variables:
  TB_NEMU_EXEC         NEMU executable path (default: <repo>/ready-to-run/riscv64-nemu-interpreter)
  TB_NEMU_MAX_INSTR    Pass -I to NEMU when > 0 (default: 0)
  TB_TRACE_LIMIT       Limit converted trace entries when > 0 (default: 0)
  TB_TRACE_START_INDEX  Start golden comparison from this jsonl index (default: 0)
  TB_RUN_DUT           Run DUT pytest after trace generation (default: 1)
  TB_TRACE_STAGNANT_CYCLES_LIMIT
                       Early-stop DUT run when golden cursor is stagnant for this many cycles (default: 20000)
  TB_TRACE_MAX_CYCLES  Optional DUT cycle budget for test_bin_trace; 0 means run until
                       golden completion (default: 0)
  TB_PYTEST_TIMEOUT_SECS
                       Hard timeout in seconds for DUT pytest stage (default: 900)
  TB_PYTEST_TARGET     Pytest target path/nodeid
  TB_BASE_ADDR         Program load base addr for DUT test (default: 0x80000000)
  TB_RESET_VECTOR      DUT reset/start PC for bin run (default: TB_BASE_ADDR)
  PYTHON               Python executable (default: python3)

Note:
  Python/runtime environment must be activated before running this script.
EOF
}

if [[ $# -lt 1 || $# -gt 4 ]]; then
  PIPELINE_REASON="invalid_arguments"
  usage
  exit 1
fi

BIN_PATH="$1"
BIN_BASENAME="$(basename "${BIN_PATH}")"
BIN_STEM="${BIN_BASENAME%.*}"

TRACE_PATH="${2:-${REPO_DIR}/NEMU/logs/${BIN_STEM}.trace.jsonl}"
NEMU_LOG_PATH="${3:-${REPO_DIR}/NEMU/logs/${BIN_STEM}.nemu.log}"
NEMU_EXEC="${TB_NEMU_EXEC:-${REPO_DIR}/ready-to-run/riscv64-nemu-interpreter}"
NEMU_MAX_INSTR="${TB_NEMU_MAX_INSTR:-0}"
TRACE_LIMIT="${TB_TRACE_LIMIT:-0}"
TRACE_START_INDEX="${TB_TRACE_START_INDEX:-0}"
RUN_DUT="${TB_RUN_DUT:-1}"
PYTEST_TARGET_DEFAULT="${FRONTEND_DIR}/tests/test_bin_trace_dut.py::test_bin_trace"
PYTEST_TARGET="${4:-${TB_PYTEST_TARGET:-${PYTEST_TARGET_DEFAULT}}}"
BASE_ADDR="${TB_BASE_ADDR:-0x80000000}"
RESET_VECTOR="${TB_RESET_VECTOR:-${BASE_ADDR}}"
TRACE_STAGNANT_CYCLES_LIMIT="${TB_TRACE_STAGNANT_CYCLES_LIMIT:-20000}"
TRACE_MAX_CYCLES="${TB_TRACE_MAX_CYCLES:-0}"
PYTEST_TIMEOUT_SECS="${TB_PYTEST_TIMEOUT_SECS:-900}"
PYTHON_BIN="${PYTHON:-python3}"
PYTEST_DISABLE_RERUNFAILURES="${TB_PYTEST_DISABLE_RERUNFAILURES:-1}"

if [[ "${BIN_STEM}" == "microbench" ]]; then
  NEMU_MAX_INSTR=0
  TRACE_LIMIT=0
  echo "[frontend] microbench detected: using run-to-completion bin-trace flow"
fi

echo "[frontend] repo: ${REPO_DIR}"
echo "[frontend] package: ${FRONTEND_DIR}"
echo "[frontend] bin: ${BIN_PATH}"
echo "[frontend] trace: ${TRACE_PATH}"
echo "[frontend] nemu_log: ${NEMU_LOG_PATH}"
echo "[frontend] nemu_exec: ${NEMU_EXEC}"
echo "[frontend] python: ${PYTHON_BIN}"
echo "[frontend] run_dut: ${RUN_DUT}"
echo "[frontend] pytest_target: ${PYTEST_TARGET}"
echo "[frontend] base_addr: ${BASE_ADDR}"
echo "[frontend] reset_vector: ${RESET_VECTOR}"
echo "[frontend] trace_stagnant_cycles_limit: ${TRACE_STAGNANT_CYCLES_LIMIT}"
echo "[frontend] trace_max_cycles: ${TRACE_MAX_CYCLES}"
echo "[frontend] trace_start_index: ${TRACE_START_INDEX}"
echo "[frontend] pytest_timeout_secs: ${PYTEST_TIMEOUT_SECS}"
echo "[frontend] pytest_disable_rerunfailures: ${PYTEST_DISABLE_RERUNFAILURES}"

cd "${REPO_DIR}"

if [[ ! -f "${BIN_PATH}" ]]; then
  PIPELINE_STAGE="validate_inputs"
  PIPELINE_REASON="bin_not_found"
  echo "[frontend][error] bin not found: ${BIN_PATH}" >&2
  exit 2
fi

if [[ ! -f "${NEMU_EXEC}" ]]; then
  PIPELINE_STAGE="validate_inputs"
  PIPELINE_REASON="nemu_exec_not_found"
  echo "[frontend][error] NEMU executable not found: ${NEMU_EXEC}" >&2
  exit 2
fi

if ! [[ "${PYTEST_TIMEOUT_SECS}" =~ ^[0-9]+$ ]] || [[ "${PYTEST_TIMEOUT_SECS}" -le 0 ]]; then
  PIPELINE_STAGE="validate_inputs"
  PIPELINE_REASON="invalid_pytest_timeout"
  echo "[frontend][error] TB_PYTEST_TIMEOUT_SECS must be a positive integer, got: ${PYTEST_TIMEOUT_SECS}" >&2
  exit 2
fi

if ! [[ "${TRACE_STAGNANT_CYCLES_LIMIT}" =~ ^[0-9]+$ ]] || [[ "${TRACE_STAGNANT_CYCLES_LIMIT}" -le 0 ]]; then
  PIPELINE_STAGE="validate_inputs"
  PIPELINE_REASON="invalid_stagnant_cycles_limit"
  echo "[frontend][error] TB_TRACE_STAGNANT_CYCLES_LIMIT must be a positive integer, got: ${TRACE_STAGNANT_CYCLES_LIMIT}" >&2
  exit 2
fi

if ! [[ "${TRACE_START_INDEX}" =~ ^[0-9]+$ ]]; then
  PIPELINE_STAGE="validate_inputs"
  PIPELINE_REASON="invalid_trace_start_index"
  echo "[frontend][error] TB_TRACE_START_INDEX must be a non-negative integer, got: ${TRACE_START_INDEX}" >&2
  exit 2
fi

mkdir -p "$(dirname "${TRACE_PATH}")" "$(dirname "${NEMU_LOG_PATH}")"
export PYTHONPATH="${FRONTEND_DIR}:${REPO_DIR}/build-frontend/pylib:${PYTHONPATH:-}"

PIPELINE_STAGE="trace_generation"
PIPELINE_REASON="trace_generation_failed"
"${PYTHON_BIN}" "${FRONTEND_DIR}/tools/nemu_bin_to_golden_trace.py" \
  "${BIN_PATH}" "${TRACE_PATH}" \
  --nemu-exec "${NEMU_EXEC}" \
  --nemu-log "${NEMU_LOG_PATH}" \
  --nemu-max-instr "${NEMU_MAX_INSTR}" \
  --trace-limit "${TRACE_LIMIT}"

PIPELINE_REASON="trace_generated"
echo "[frontend] trace done: ${TRACE_PATH}"

if [[ "${RUN_DUT}" != "0" ]]; then
  if [[ ! -f "${FRONTEND_DIR}/tests/test_bin_trace_dut.py" ]]; then
    PIPELINE_STAGE="validate_inputs"
    PIPELINE_REASON="missing_pytest_entry"
    echo "[frontend][error] missing pytest entry test file: ${FRONTEND_DIR}/tests/test_bin_trace_dut.py" >&2
    exit 2
  fi
  PYTEST_CMD=("${PYTHON_BIN}" -m pytest -v)
  if [[ "${PYTEST_DISABLE_RERUNFAILURES}" != "0" ]]; then
    PYTEST_CMD+=(-p no:rerunfailures)
  fi
  PYTEST_CMD+=("${PYTEST_TARGET}")
  PIPELINE_STAGE="dut_pytest"
  PIPELINE_REASON="dut_pytest_failed"
  if timeout --foreground "${PYTEST_TIMEOUT_SECS}" env \
    TB_ENABLE_DUT_TESTS=1 \
    TB_BIN_TRACE_PIPELINE=1 \
    TB_BIN_PATH="${BIN_PATH}" \
    TB_TRACE_PATH="${TRACE_PATH}" \
    TB_BASE_ADDR="${BASE_ADDR}" \
    TB_RESET_VECTOR="${RESET_VECTOR}" \
    TB_TRACE_STAGNANT_CYCLES_LIMIT="${TRACE_STAGNANT_CYCLES_LIMIT}" \
    TB_TRACE_MAX_CYCLES="${TRACE_MAX_CYCLES}" \
    TB_TRACE_START_INDEX="${TRACE_START_INDEX}" \
    "${PYTEST_CMD[@]}"; then
    PIPELINE_REASON="dut_pytest_passed"
  else
    status=$?
    if [[ "${status}" -eq 124 ]]; then
      PIPELINE_REASON="dut_pytest_timeout"
      echo "[frontend][error] DUT pytest timed out after ${PYTEST_TIMEOUT_SECS}s" >&2
    elif [[ "${status}" -gt 128 ]]; then
      PIPELINE_REASON="dut_pytest_terminated_by_signal"
    else
      PIPELINE_REASON="dut_pytest_assertion_or_error"
    fi
    exit "${status}"
  fi
else
  PIPELINE_STAGE="trace_generation"
  PIPELINE_REASON="dut_skipped"
fi

PIPELINE_STAGE="done"
PIPELINE_REASON="completed"
echo "[frontend] done"
