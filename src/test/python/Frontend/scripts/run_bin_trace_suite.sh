#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FRONTEND_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${FRONTEND_DIR}/../../../.." && pwd)"

usage() {
  cat <<'EOF'
Usage:
  src/test/python/Frontend/scripts/run_bin_trace_suite.sh [bin_path ...]
  src/test/python/Frontend/scripts/run_bin_trace_suite.sh --list-file <path>
  src/test/python/Frontend/scripts/run_bin_trace_suite.sh --list [bin_path ...]
  src/test/python/Frontend/scripts/run_bin_trace_suite.sh --list --list-file <path>

Purpose:
  Run an explicitly selected ready-to-run bin-trace regression list.

Environment:
  TB_RUN_DUT   Passed to run_bin_trace_pipeline.sh; default 1.
  TB_LOG_LEVEL Default INFO for this suite unless already set.
  TB_PYTEST_TIMEOUT_SECS
               Default 1200 unless already set.
  TB_RUN_ID    Optional suite ID prefix. Each case appends its bin stem.
  TB_SUITE_ARTIFACT_DIR
               Root for dated suite directories; defaults to
               <repo>/src/test/python/Frontend/data/runs.
  TB_SUITE_DATE
               Optional suite date in YYYYMMDD format; defaults to the
               invocation date.
  TB_SUITE_TIME
               Optional suite start time in HHMMSS format; defaults to the
               invocation time.
  TB_BIN_TRACE_SUITE_CONTINUE_ON_FAIL
               Continue running later bins after a failure when set to 1.
EOF
}

trim_whitespace() {
  local value="$1"
  value="${value#"${value%%[![:space:]]*}"}"
  value="${value%"${value##*[![:space:]]}"}"
  printf '%s' "${value}"
}

resolve_repo_path() {
  local path="$1"
  if [[ "${path}" = /* ]]; then
    printf '%s' "${path}"
  else
    printf '%s/%s' "${REPO_DIR}" "${path}"
  fi
}

read_case_list() {
  local list_path="$1"
  local raw line
  if [[ ! -f "${list_path}" ]]; then
    echo "[frontend-bin-suite][error] list file not found: ${list_path}" >&2
    exit 2
  fi
  while IFS= read -r raw || [[ -n "${raw}" ]]; do
    line="${raw%%#*}"
    line="$(trim_whitespace "${line}")"
    if [[ -n "${line}" ]]; then
      CASES+=("${line}")
    fi
  done < "${list_path}"
}

LIST_FILE=""
LIST_ONLY=0
CASES=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --help|-h)
      usage
      exit 0
      ;;
    --list)
      LIST_ONLY=1
      shift
      ;;
    --list-file)
      if [[ $# -lt 2 ]]; then
        echo "[frontend-bin-suite][error] --list-file requires a path" >&2
        exit 2
      fi
      LIST_FILE="$2"
      shift 2
      ;;
    --)
      shift
      while [[ $# -gt 0 ]]; do
        CASES+=("$1")
        shift
      done
      ;;
    -*)
      echo "[frontend-bin-suite][error] unknown option: $1" >&2
      usage
      exit 2
      ;;
    *)
      CASES+=("$1")
      shift
      ;;
  esac
done

if [[ "${#CASES[@]}" -eq 0 && -n "${LIST_FILE}" ]]; then
  if [[ "${LIST_FILE}" != /* ]]; then
    LIST_FILE="${REPO_DIR}/${LIST_FILE}"
  fi
  read_case_list "${LIST_FILE}"
fi

if [[ "${LIST_ONLY}" == "1" ]]; then
  printf '%s\n' "${CASES[@]}"
  exit 0
fi

if [[ "${#CASES[@]}" -eq 0 ]]; then
  echo "[frontend-bin-suite][error] no bin-trace cases selected; pass bin paths or --list-file <path>" >&2
  exit 2
fi

SUITE_ID_DEFAULT="frontend_bin_trace_suite_$(date +%Y%m%d_%H%M%S)_$$"
SUITE_ID="${TB_RUN_ID:-${SUITE_ID_DEFAULT}}"
SUITE_RUNS_ROOT="${TB_SUITE_ARTIFACT_DIR:-${FRONTEND_DIR}/data/runs}"
SUITE_DATE="${TB_SUITE_DATE:-$(date +%Y%m%d)}"
SUITE_TIME="${TB_SUITE_TIME:-$(date +%H%M%S)}"
TB_LOG_LEVEL="${TB_LOG_LEVEL:-INFO}"
TB_PYTEST_TIMEOUT_SECS="${TB_PYTEST_TIMEOUT_SECS:-1200}"
TB_RUN_DUT="${TB_RUN_DUT:-1}"
TB_BIN_TRACE_SUITE_CONTINUE_ON_FAIL="${TB_BIN_TRACE_SUITE_CONTINUE_ON_FAIL:-0}"

if ! [[ "${SUITE_ID}" =~ ^[A-Za-z0-9_.=-]+$ ]]; then
  echo "[frontend-bin-suite][error] TB_RUN_ID suite prefix contains unsupported characters: ${SUITE_ID}" >&2
  exit 2
fi

if ! [[ "${SUITE_DATE}" =~ ^[0-9]{8}$ ]]; then
  echo "[frontend-bin-suite][error] TB_SUITE_DATE must use YYYYMMDD: ${SUITE_DATE}" >&2
  exit 2
fi

if ! [[ "${SUITE_TIME}" =~ ^[0-9]{6}$ ]]; then
  echo "[frontend-bin-suite][error] TB_SUITE_TIME must use HHMMSS: ${SUITE_TIME}" >&2
  exit 2
fi

mkdir -p "${SUITE_RUNS_ROOT}"
SUITE_RUNS_ROOT="$(cd "${SUITE_RUNS_ROOT}" && pwd -P)"
SUITE_ARTIFACT_DIR="${SUITE_RUNS_ROOT}/suites/${SUITE_DATE}/${SUITE_TIME}_${SUITE_ID}"
if [[ -e "${SUITE_ARTIFACT_DIR}" ]]; then
  echo "[frontend-bin-suite][error] refusing to reuse existing suite root: ${SUITE_ARTIFACT_DIR}" >&2
  exit 2
fi
mkdir -p "${SUITE_ARTIFACT_DIR}/cases"

echo "[frontend-bin-suite] repo: ${REPO_DIR}"
echo "[frontend-bin-suite] suite_id: ${SUITE_ID}"
echo "[frontend-bin-suite] runs_root: ${SUITE_RUNS_ROOT}"
echo "[frontend-bin-suite] suite_dir: ${SUITE_ARTIFACT_DIR}"
echo "[frontend-bin-suite] case_count: ${#CASES[@]}"

failed=()
resolved_cases=()
for case_entry in "${CASES[@]}"; do
  case_path="$(resolve_repo_path "${case_entry}")"
  resolved_cases+=("${case_path}")
  if [[ ! -f "${case_path}" ]]; then
    echo "[frontend-bin-suite][error] bin not found: ${case_entry}" >&2
    failed+=("${case_entry}:not_found")
    if [[ "${TB_BIN_TRACE_SUITE_CONTINUE_ON_FAIL}" != "1" ]]; then
      exit 2
    fi
    continue
  fi

  echo
  echo "[frontend-bin-suite] RUN ${case_entry}"
  case_name="$(basename "${case_path}")"
  case_stem="${case_name%.*}"
  case_run_id="${SUITE_ID}_${case_stem}"
  case_artifact_dir="${SUITE_ARTIFACT_DIR}/cases/${case_stem}"
  printf -v case_run_command '%q ' "${SCRIPT_DIR}/run_bin_trace_pipeline.sh" "${case_path}"
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
    "${SCRIPT_DIR}/run_bin_trace_pipeline.sh" "${case_path}"; then
    echo "[frontend-bin-suite] PASS ${case_entry}"
  else
    status=$?
    echo "[frontend-bin-suite][error] FAIL status=${status} case=${case_entry}" >&2
    failed+=("${case_entry}:status_${status}")
    if [[ "${TB_BIN_TRACE_SUITE_CONTINUE_ON_FAIL}" != "1" ]]; then
      exit "${status}"
    fi
  fi
done

echo
echo "[frontend-bin-suite] trace entry counts:"
for case_path in "${resolved_cases[@]}"; do
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
  echo "[frontend-bin-suite][error] failed cases: ${failed[*]}" >&2
  exit 1
fi

echo "[frontend-bin-suite] done"
echo "[frontend-bin-suite] suite_id=${SUITE_ID}"
echo "[frontend-bin-suite] suite_dir=${SUITE_ARTIFACT_DIR}"
