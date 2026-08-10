#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FRONTEND_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${FRONTEND_DIR}/../../../.." && pwd)"

mapfile -d '' -t DEFAULT_CASES < <(
  find "${FRONTEND_DIR}/tests/asm_cases" -type f -name '*.S' -print0 | sort -z
)

if [[ "${#DEFAULT_CASES[@]}" -eq 0 ]]; then
  echo "[frontend-suite][error] no assembly cases found under ${FRONTEND_DIR}/tests/asm_cases" >&2
  exit 2
fi

usage() {
  cat <<'EOF'
Usage:
  src/test/python/Frontend/scripts/run_baremode_asm_suite.sh [case.S ...]
  src/test/python/Frontend/scripts/run_baremode_asm_suite.sh --list

Purpose:
  Run the curated active functional-coverage assembly bin-trace suite:
    .S -> bin -> NEMU golden trace -> DUT bin-trace -> funcov audit/merge
    -> raw code-coverage summary

Environment:
  NEMU_EXEC or TB_NEMU_EXEC
               NEMU executable path.
  TB_RUN_DUT   Passed to run_baremode_asm_bin_trace.sh; default 1.
  TB_LOG_LEVEL Default INFO for this suite unless already set.
  TB_PYTEST_TIMEOUT_SECS
               Default 1200 unless already set.
  TB_RUN_ID    Optional suite ID prefix. Each case appends its assembly stem.
  TB_SUITE_ARTIFACT_DIR
               Root for dated suite directories; defaults to
               <repo>/src/test/python/Frontend/data/runs.
  TB_SUITE_DATE
               Optional suite date in YYYYMMDD format; defaults to the
               invocation date.
  TB_SUITE_TIME
               Optional suite start time in HHMMSS format; defaults to the
               invocation time.
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

SUITE_ID_DEFAULT="frontend_asm_suite_$(date +%Y%m%d_%H%M%S)_$$"
SUITE_ID="${TB_RUN_ID:-${SUITE_ID_DEFAULT}}"
SUITE_RUNS_ROOT="${TB_SUITE_ARTIFACT_DIR:-${FRONTEND_DIR}/data/runs}"
SUITE_DATE="${TB_SUITE_DATE:-$(date +%Y%m%d)}"
SUITE_TIME="${TB_SUITE_TIME:-$(date +%H%M%S)}"
TB_LOG_LEVEL="${TB_LOG_LEVEL:-INFO}"
TB_PYTEST_TIMEOUT_SECS="${TB_PYTEST_TIMEOUT_SECS:-1200}"
TB_RUN_DUT="${TB_RUN_DUT:-1}"
ASM_SUITE_CONTINUE_ON_FAIL="${ASM_SUITE_CONTINUE_ON_FAIL:-0}"

if ! [[ "${SUITE_ID}" =~ ^[A-Za-z0-9_.=-]+$ ]]; then
  echo "[frontend-suite][error] TB_RUN_ID suite prefix contains unsupported characters: ${SUITE_ID}" >&2
  exit 2
fi

if ! [[ "${SUITE_DATE}" =~ ^[0-9]{8}$ ]]; then
  echo "[frontend-suite][error] TB_SUITE_DATE must use YYYYMMDD: ${SUITE_DATE}" >&2
  exit 2
fi

if ! [[ "${SUITE_TIME}" =~ ^[0-9]{6}$ ]]; then
  echo "[frontend-suite][error] TB_SUITE_TIME must use HHMMSS: ${SUITE_TIME}" >&2
  exit 2
fi

mkdir -p "${SUITE_RUNS_ROOT}"
SUITE_RUNS_ROOT="$(cd "${SUITE_RUNS_ROOT}" && pwd -P)"
SUITE_ARTIFACT_DIR="${SUITE_RUNS_ROOT}/suites/${SUITE_DATE}/${SUITE_TIME}_${SUITE_ID}"
if [[ -e "${SUITE_ARTIFACT_DIR}" ]]; then
  echo "[frontend-suite][error] refusing to reuse existing suite root: ${SUITE_ARTIFACT_DIR}" >&2
  exit 2
fi
mkdir -p "${SUITE_ARTIFACT_DIR}/cases"

if [[ $# -gt 0 ]]; then
  CASES=("$@")
else
  CASES=("${DEFAULT_CASES[@]}")
fi

echo "[frontend-suite] repo: ${REPO_DIR}"
echo "[frontend-suite] suite_id: ${SUITE_ID}"
echo "[frontend-suite] runs_root: ${SUITE_RUNS_ROOT}"
echo "[frontend-suite] suite_dir: ${SUITE_ARTIFACT_DIR}"
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
  case_name="$(basename "${case_path}")"
  case_stem="${case_name%.*}"
  case_run_id="${SUITE_ID}_${case_stem}"
  case_artifact_dir="${SUITE_ARTIFACT_DIR}/cases/${case_stem}"
  printf -v case_run_command '%q ' "${SCRIPT_DIR}/run_baremode_asm_bin_trace.sh" "${case_path}"
  case_run_command="${case_run_command% }"
  if TB_RUN_ID="${case_run_id}" \
    TB_ARTIFACT_DIR="${case_artifact_dir}" \
    TB_COVERAGE_DIR="${case_artifact_dir}/coverage" \
    TB_WAVEFORM_DIR="${case_artifact_dir}/waveforms" \
    TB_FUNCOV_DIR="${case_artifact_dir}/funcov" \
    TB_CASE_LOG_DIR="${case_artifact_dir}/logs" \
    TB_RUN_COMMAND="${case_run_command}" \
    TB_RUN_DUT="${TB_RUN_DUT}" \
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
  trace_path="${SUITE_ARTIFACT_DIR}/cases/${stem}/inputs/${stem}.trace.jsonl"
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
if [[ "${TB_RUN_DUT}" != "0" ]]; then
  suite_report_dir="${SUITE_ARTIFACT_DIR}/report"
  funcov_artifacts=()
  for case_path in "${CASES[@]}"; do
    stem="$(basename "${case_path}")"
    stem="${stem%.*}"
    funcov_path="${SUITE_ARTIFACT_DIR}/cases/${stem}/funcov/${stem}_test_bin_trace.funcov.json"
    if [[ ! -f "${funcov_path}" ]]; then
      echo "[frontend-suite][error] missing functional coverage artifact: ${funcov_path}" >&2
      exit 2
    fi
    funcov_artifacts+=("${funcov_path}")
  done
  mkdir -p "${suite_report_dir}/funcov"
  backannotate_args=()
  merge_args=()
  for funcov_path in "${funcov_artifacts[@]}"; do
    backannotate_args+=(--artifact "${funcov_path}")
    merge_args+=(--artifact "${funcov_path}")
  done

  # The canonical testpoint sheet currently has complete one-to-one mappings
  # only for BIN-4 (IFU) and BIN-5 (2-fetch).  Earlier BIN ranges remain
  # historical/incomplete model rows, so auditing all prefixes would reject a
  # valid assembly regression before it reaches the observed-coverage report.
  for funcov_bin_prefix in BIN-4 BIN-5; do
    echo "[frontend-suite] functional coverage gate audit (${funcov_bin_prefix}):"
    "${PYTHON:-python3}" "${FRONTEND_DIR}/tools/backannotate_funcov.py" \
      --pilot "${FRONTEND_DIR}/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv" \
      --testpoints "${FRONTEND_DIR}/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv" \
      --bin-prefix "${funcov_bin_prefix}" \
      --check \
      --audit-json "${suite_report_dir}/funcov/backannotation_audit_${funcov_bin_prefix}.json" \
      "${backannotate_args[@]}"
  done

  echo "[frontend-suite] observed functional coverage aggregate (diagnostic only):"
  "${PYTHON:-python3}" "${FRONTEND_DIR}/tools/merge_funcov.py" \
    --output-dir "${suite_report_dir}/funcov" \
    --artifact-tag "${SUITE_ID}_observed" \
    --run-id "${SUITE_ID}" \
    "${merge_args[@]}"

  echo "[frontend-suite] raw coverage summary:"
  "${PYTHON:-python3}" "${FRONTEND_DIR}/scripts/report_raw_code_coverage.py" \
    --data-dir "${SUITE_ARTIFACT_DIR}/cases" \
    --glob "*/coverage/*.dat" \
    --run-id "${SUITE_ID}" \
    --json-output "${suite_report_dir}/code_coverage_summary.json"
else
  echo "[frontend-suite] raw coverage summary skipped: TB_RUN_DUT=0"
fi

echo "[frontend-suite] done"
echo "[frontend-suite] suite_id=${SUITE_ID}"
echo "[frontend-suite] suite_dir=${SUITE_ARTIFACT_DIR}"
