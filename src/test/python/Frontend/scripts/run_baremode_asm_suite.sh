#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FRONTEND_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${FRONTEND_DIR}/../../../.." && pwd)"

DEFAULT_CASES=(
  "${FRONTEND_DIR}/tests/asm_cases/fe_baremode_seq_icache_basic.S"
  "${FRONTEND_DIR}/tests/asm_cases/fe_baremode_direct_jmp.S"
  "${FRONTEND_DIR}/tests/asm_cases/fe_baremode_cond_nt.S"
  "${FRONTEND_DIR}/tests/asm_cases/fe_jal_forward_jump_observes_target_pc.S"
  "${FRONTEND_DIR}/tests/asm_cases/fe_jal_resolve_drains_pending_queue.S"
  "${FRONTEND_DIR}/tests/asm_cases/fe_multi_branch_random_positions.S"
  "${FRONTEND_DIR}/tests/asm_cases/fe_multi_cfi_per_ftq_entry.S"
  "${FRONTEND_DIR}/tests/asm_cases/fe_multi_branch_dense_loop.S"
  "${FRONTEND_DIR}/tests/asm_cases/fe_large_loop_multi_segment.S"
)

usage() {
  cat <<'EOF'
Usage:
  src/test/python/Frontend/scripts/run_baremode_asm_suite.sh [case.S ...]
  src/test/python/Frontend/scripts/run_baremode_asm_suite.sh --list

Purpose:
  Run the curated normal-path baremode assembly bin-trace suite:
    .S -> bin -> NEMU golden trace -> DUT bin-trace -> raw coverage summary

Environment:
  NEMU_EXEC or TB_NEMU_EXEC
               NEMU executable path.
  TB_RUN_DUT   Passed to run_baremode_asm_bin_trace.sh; default 1.
  TB_LOG_LEVEL Default WARNING for this suite unless already set.
  TB_PYTEST_TIMEOUT_SECS
               Default 1200 unless already set.
  TB_COVERAGE_DIR
               Default: <repo>/src/test/python/Frontend/data/<YYYYMMDD>/asm_bin_trace_suite
  TB_WAVEFORM_DIR
               Default: <repo>/src/test/python/Frontend/data/<YYYYMMDD>/asm_bin_trace_suite/waveforms
  ASM_SUITE_CONTINUE_ON_FAIL
               Continue running later cases after a failure when set to 1.
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

if [[ "${1:-}" == "--list" ]]; then
  printf '%s\n' "${DEFAULT_CASES[@]}"
  exit 0
fi

DATE_STAMP="$(date +%Y%m%d)"
SUITE_DIR="${FRONTEND_DIR}/data/${DATE_STAMP}/asm_bin_trace_suite"
TB_COVERAGE_DIR="${TB_COVERAGE_DIR:-${SUITE_DIR}}"
TB_WAVEFORM_DIR="${TB_WAVEFORM_DIR:-${SUITE_DIR}/waveforms}"
TB_LOG_LEVEL="${TB_LOG_LEVEL:-WARNING}"
TB_PYTEST_TIMEOUT_SECS="${TB_PYTEST_TIMEOUT_SECS:-1200}"
ASM_SUITE_CONTINUE_ON_FAIL="${ASM_SUITE_CONTINUE_ON_FAIL:-0}"

mkdir -p "${TB_COVERAGE_DIR}" "${TB_WAVEFORM_DIR}"

if [[ $# -gt 0 ]]; then
  CASES=("$@")
else
  CASES=("${DEFAULT_CASES[@]}")
fi

echo "[frontend-suite] repo: ${REPO_DIR}"
echo "[frontend-suite] coverage_dir: ${TB_COVERAGE_DIR}"
echo "[frontend-suite] waveform_dir: ${TB_WAVEFORM_DIR}"
echo "[frontend-suite] case_count: ${#CASES[@]}"

failed=()
for case_path in "${CASES[@]}"; do
  if [[ ! -f "${case_path}" ]]; then
    echo "[frontend-suite][error] asm not found: ${case_path}" >&2
    failed+=("${case_path}:not_found")
    if [[ "${ASM_SUITE_CONTINUE_ON_FAIL}" != "1" ]]; then
      exit 2
    fi
    continue
  fi

  echo
  echo "[frontend-suite] RUN ${case_path}"
  if TB_COVERAGE_DIR="${TB_COVERAGE_DIR}" \
    TB_WAVEFORM_DIR="${TB_WAVEFORM_DIR}" \
    TB_LOG_LEVEL="${TB_LOG_LEVEL}" \
    TB_PYTEST_TIMEOUT_SECS="${TB_PYTEST_TIMEOUT_SECS}" \
    "${SCRIPT_DIR}/run_baremode_asm_bin_trace.sh" "${case_path}"; then
    echo "[frontend-suite] PASS ${case_path}"
  else
    status=$?
    echo "[frontend-suite][error] FAIL status=${status} case=${case_path}" >&2
    failed+=("${case_path}:status_${status}")
    if [[ "${ASM_SUITE_CONTINUE_ON_FAIL}" != "1" ]]; then
      exit "${status}"
    fi
  fi
done

echo
echo "[frontend-suite] trace entry counts:"
for case_path in "${CASES[@]}"; do
  stem="$(basename "${case_path}")"
  stem="${stem%.*}"
  trace_path="${REPO_DIR}/NEMU/logs/${stem}.trace.jsonl"
  if [[ -f "${trace_path}" ]]; then
    printf '  %-48s %s\n' "${stem}" "$(wc -l < "${trace_path}")"
  else
    printf '  %-48s %s\n' "${stem}" "missing"
  fi
done

if [[ "${#failed[@]}" -gt 0 ]]; then
  echo "[frontend-suite][error] failed cases: ${failed[*]}" >&2
  exit 1
fi

echo
echo "[frontend-suite] raw coverage summary:"
"${PYTHON:-python3}" "${FRONTEND_DIR}/scripts/report_raw_code_coverage.py" --data-dir "${TB_COVERAGE_DIR}"

echo "[frontend-suite] done"
echo "[frontend-suite] coverage_dir=${TB_COVERAGE_DIR}"
