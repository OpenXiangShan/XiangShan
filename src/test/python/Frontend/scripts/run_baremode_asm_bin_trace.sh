#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FRONTEND_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${FRONTEND_DIR}/../../../.." && pwd)"

usage() {
  cat <<'EOF'
Usage:
  src/test/python/Frontend/scripts/run_baremode_asm_bin_trace.sh [case.S]

Purpose:
  Compile a normal-path baremode assembly testcase into a bin, generate/reuse
  the NEMU golden trace, run the existing bin-trace DUT regression, and leave
  waveform/log/.dat/funcov artifacts in the normal Frontend data directories.

Defaults:
  case.S       src/test/python/Frontend/tests/asm_cases/fe_baremode_python_pilot_mix.S
  LINK_ADDR    0x80000000
  NEMU_MEM_BASE 0x80000000
  TB_BASE_ADDR 0x80000000
  TB_RESET_VECTOR 0x80000000

Environment:
  NEMU_EXEC or TB_NEMU_EXEC
               NEMU executable path. If unset, this script tries
               ready-to-run/riscv64-nemu-interpreter.
  TB_RUN_DUT   Passed to run_bin_trace_pipeline.sh; default 1.
  TB_LOG_LEVEL, TB_TRACE_TARGET_CURSOR, TB_TRACE_MAX_CYCLES,
  TB_PYTEST_TIMEOUT_SECS, TB_COVERAGE_DIR, TB_WAVEFORM_DIR
               Passed through to the existing bin-trace pipeline/fixtures.

After a successful run, coverage can be inspected with the existing commands:
  python src/test/python/Frontend/scripts/report_raw_code_coverage.py --data-dir src/test/python/Frontend/data/<YYYYMMDD>
  src/test/python/Frontend/scripts/gen_coverage_html.sh src/test/python/Frontend/data/<YYYYMMDD>
EOF
}

if [[ $# -gt 1 ]]; then
  usage
  exit 1
fi

DEFAULT_ASM="${FRONTEND_DIR}/tests/asm_cases/fe_baremode_python_pilot_mix.S"
ASM_PATH="${1:-${DEFAULT_ASM}}"
if [[ ! -f "${ASM_PATH}" ]]; then
  echo "[frontend][error] asm not found: ${ASM_PATH}" >&2
  exit 2
fi

CASE_NAME="$(basename "${ASM_PATH}")"
CASE_STEM="${CASE_NAME%.*}"
BIN_PATH="${FRONTEND_DIR}/tests/asm_cases/generated/${CASE_STEM}.bin"
TRACE_PATH="${REPO_DIR}/NEMU/logs/${CASE_STEM}.trace.jsonl"
NEMU_LOG_PATH="${REPO_DIR}/NEMU/logs/${CASE_STEM}.nemu.log"

LINK_ADDR="${LINK_ADDR:-0x80000000}"
NEMU_MEM_BASE="${NEMU_MEM_BASE:-0x80000000}"
TB_BASE_ADDR="${TB_BASE_ADDR:-${NEMU_MEM_BASE}}"
TB_RESET_VECTOR="${TB_RESET_VECTOR:-${LINK_ADDR}}"
TB_RUN_DUT="${TB_RUN_DUT:-1}"

resolve_nemu_exec() {
  local explicit="${NEMU_EXEC:-${TB_NEMU_EXEC:-}}"
  if [[ -n "${explicit}" ]]; then
    echo "${explicit}"
    return 0
  fi

  local candidate
  for candidate in \
    "${REPO_DIR}/ready-to-run/riscv64-nemu-interpreter"; do
    if [[ -f "${candidate}" ]]; then
      echo "${candidate}"
      return 0
    fi
  done

  return 1
}

NEMU_EXEC="$(resolve_nemu_exec)" || {
  echo "[frontend][error] NEMU executable not found. Set NEMU_EXEC or TB_NEMU_EXEC." >&2
  exit 2
}

if [[ ! -x "${NEMU_EXEC}" ]]; then
  echo "[frontend][error] NEMU path exists but is not executable: ${NEMU_EXEC}" >&2
  echo "[frontend][error] Fix the executable bit or point NEMU_EXEC/TB_NEMU_EXEC at an executable NEMU." >&2
  exit 2
fi

echo "[frontend] repo: ${REPO_DIR}"
echo "[frontend] asm: ${ASM_PATH}"
echo "[frontend] bin: ${BIN_PATH}"
echo "[frontend] trace: ${TRACE_PATH}"
echo "[frontend] nemu_log: ${NEMU_LOG_PATH}"
echo "[frontend] nemu_exec: ${NEMU_EXEC}"
echo "[frontend] link_addr: ${LINK_ADDR}"
echo "[frontend] nemu_mem_base: ${NEMU_MEM_BASE}"
echo "[frontend] tb_base_addr: ${TB_BASE_ADDR}"
echo "[frontend] tb_reset_vector: ${TB_RESET_VECTOR}"
echo "[frontend] tb_run_dut: ${TB_RUN_DUT}"

cd "${REPO_DIR}"

LINK_ADDR="${LINK_ADDR}" \
NEMU_MEM_BASE="${NEMU_MEM_BASE}" \
NEMU_EXEC="${NEMU_EXEC}" \
"${FRONTEND_DIR}/scripts/asm_to_jsonl.sh" "${ASM_PATH}" "${BIN_PATH}" "${TRACE_PATH}"

TB_SKIP_NEMU=1 \
TB_RUN_DUT="${TB_RUN_DUT}" \
TB_NEMU_EXEC="${NEMU_EXEC}" \
TB_BASE_ADDR="${TB_BASE_ADDR}" \
TB_RESET_VECTOR="${TB_RESET_VECTOR}" \
"${FRONTEND_DIR}/scripts/run_bin_trace_pipeline.sh" "${BIN_PATH}" "${TRACE_PATH}" "${NEMU_LOG_PATH}"

date_dir="${FRONTEND_DIR}/data/$(date +%Y%m%d)"
echo "[frontend] artifacts_dir=${date_dir}"
echo "[frontend] raw_coverage_command=python ${FRONTEND_DIR}/scripts/report_raw_code_coverage.py --data-dir ${date_dir}"
echo "[frontend] html_coverage_command=${FRONTEND_DIR}/scripts/gen_coverage_html.sh ${date_dir}"
