#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

usage() {
  cat <<'EOF'
Usage:
  ./run_bin_trace_pipeline.sh <bin_path> [trace_jsonl_path] [nemu_log_path] [pytest_target]

Environment variables:
  TB_NEMU_EXEC         NEMU executable path (default: <repo>/NEMU/build/riscv64-nemu-interpreter)
  TB_NEMU_MAX_INSTR    Pass -I to NEMU when > 0 (default: 0)
  TB_TRACE_LIMIT       Limit converted trace entries when > 0 (default: 0)
  TB_RUN_DUT           Run DUT pytest after trace generation (default: 1)
  TB_PYTEST_TARGET     Pytest target path/nodeid
  TB_BASE_ADDR         Program load base addr for DUT test (default: 0x80000000)
  TB_STEP_CYCLES       Extra step cycles in DUT test (default: 0)
  PYTHON               Python executable (default: python3)

Note:
  Python/runtime environment must be activated before running this script.
EOF
}

if [[ $# -lt 1 || $# -gt 4 ]]; then
  usage
  exit 1
fi

BIN_PATH="$1"
BIN_BASENAME="$(basename "${BIN_PATH}")"
BIN_STEM="${BIN_BASENAME%.*}"

TRACE_PATH="${2:-${REPO_DIR}/NEMU/logs/${BIN_STEM}.trace.jsonl}"
NEMU_LOG_PATH="${3:-${REPO_DIR}/NEMU/logs/${BIN_STEM}.nemu.log}"
NEMU_EXEC="${TB_NEMU_EXEC:-${REPO_DIR}/NEMU/build/riscv64-nemu-interpreter}"
NEMU_MAX_INSTR="${TB_NEMU_MAX_INSTR:-0}"
TRACE_LIMIT="${TB_TRACE_LIMIT:-0}"
RUN_DUT="${TB_RUN_DUT:-1}"
RUN_TO_TRACE_COMPLETION="${TB_RUN_TO_TRACE_COMPLETION:-0}"
PYTEST_TARGET_DEFAULT="${SCRIPT_DIR}/tests/test_bin_trace_dut.py::test_bin_trace"
PYTEST_TARGET="${4:-${TB_PYTEST_TARGET:-${PYTEST_TARGET_DEFAULT}}}"
BASE_ADDR="${TB_BASE_ADDR:-0x80000000}"
STEP_CYCLES="${TB_STEP_CYCLES:-0}"
PYTHON_BIN="${PYTHON:-python3}"

if [[ "${BIN_STEM}" == "microbench" ]]; then
  NEMU_MAX_INSTR=0
  TRACE_LIMIT=0
  STEP_CYCLES=0
  RUN_TO_TRACE_COMPLETION=1
  echo "[frontend] microbench detected: forcing unrestricted runtime"
fi

echo "[frontend] repo: ${REPO_DIR}"
echo "[frontend] package: ${SCRIPT_DIR}"
echo "[frontend] bin: ${BIN_PATH}"
echo "[frontend] trace: ${TRACE_PATH}"
echo "[frontend] nemu_log: ${NEMU_LOG_PATH}"
echo "[frontend] nemu_exec: ${NEMU_EXEC}"
echo "[frontend] python: ${PYTHON_BIN}"
echo "[frontend] run_dut: ${RUN_DUT}"
echo "[frontend] run_to_trace_completion: ${RUN_TO_TRACE_COMPLETION}"
echo "[frontend] pytest_target: ${PYTEST_TARGET}"
echo "[frontend] base_addr: ${BASE_ADDR}"
echo "[frontend] step_cycles: ${STEP_CYCLES}"

cd "${REPO_DIR}"

if [[ ! -f "${BIN_PATH}" ]]; then
  echo "[frontend][error] bin not found: ${BIN_PATH}" >&2
  exit 2
fi

if [[ ! -f "${NEMU_EXEC}" ]]; then
  echo "[frontend][error] NEMU executable not found: ${NEMU_EXEC}" >&2
  exit 2
fi

mkdir -p "$(dirname "${TRACE_PATH}")" "$(dirname "${NEMU_LOG_PATH}")"
export PYTHONPATH="${SCRIPT_DIR}:${REPO_DIR}/build-frontend/pylib:${PYTHONPATH:-}"

"${PYTHON_BIN}" "${SCRIPT_DIR}/nemu_bin_to_golden_trace.py" \
  "${BIN_PATH}" "${TRACE_PATH}" \
  --nemu-exec "${NEMU_EXEC}" \
  --nemu-log "${NEMU_LOG_PATH}" \
  --nemu-max-instr "${NEMU_MAX_INSTR}" \
  --trace-limit "${TRACE_LIMIT}"

echo "[frontend] trace done: ${TRACE_PATH}"

if [[ "${RUN_DUT}" != "0" ]]; then
  if [[ ! -f "${SCRIPT_DIR}/tests/test_bin_trace_dut.py" ]]; then
    echo "[frontend][error] missing pytest entry test file: ${SCRIPT_DIR}/tests/test_bin_trace_dut.py" >&2
    exit 2
  fi
  TB_ENABLE_DUT_TESTS=1 \
  TB_BIN_TRACE_PIPELINE=1 \
  TB_BIN_PATH="${BIN_PATH}" \
  TB_TRACE_PATH="${TRACE_PATH}" \
  TB_BASE_ADDR="${BASE_ADDR}" \
  TB_STEP_CYCLES="${STEP_CYCLES}" \
  TB_RUN_TO_TRACE_COMPLETION="${RUN_TO_TRACE_COMPLETION}" \
  "${PYTHON_BIN}" -m pytest -v "${PYTEST_TARGET}"
fi

echo "[frontend] done"
