#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  ./scripts/gen_coverage_html.sh [input.dat ... | input_dir] [output_dir]

Notes:
  - With no input, the script collects all .dat files under src/test/python/Frontend/data/.
  - A single directory input collects all .dat files directly under that directory.
  - Multiple .dat inputs are merged before generating HTML.
  - Output defaults to <input_stem>.genhtml/ for a single .dat input, or
    coverage.genhtml/ next to the input directory / default data directory.
  - The script will generate merged.info in the output directory.
  - genhtml is run with --ignore-errors range because the .dat file and the
    current build-frontend RTL sources may not be from the exact same build.
EOF
}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEFAULT_DATA_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)/data"

shopt -s nullglob

declare -a INPUTS=()
OUTPUT_DIR=""

if [[ $# -eq 0 ]]; then
  INPUTS=("${DEFAULT_DATA_DIR}"/*.dat)
  OUTPUT_DIR="${DEFAULT_DATA_DIR}/coverage.genhtml"
elif [[ $# -eq 1 ]]; then
  if [[ -d "$1" ]]; then
    INPUTS=("$1"/*.dat)
    OUTPUT_DIR="${1%/}/coverage.genhtml"
  else
    INPUTS=("$1")
  fi
else
  last_arg="${!#}"
  if [[ -d "${last_arg}" ]]; then
    OUTPUT_DIR="${last_arg}"
    for ((i = 1; i < $#; i++)); do
      INPUTS+=("${!i}")
    done
  else
    INPUTS=("$@")
  fi
fi

if [[ ${#INPUTS[@]} -eq 0 ]]; then
  echo "[frontend][error] no .dat files found" >&2
  exit 2
fi

declare -a DAT_FILES=()
for input in "${INPUTS[@]}"; do
  if [[ -d "${input}" ]]; then
    echo "[frontend][error] directory inputs must be passed alone: ${input}" >&2
    exit 2
  fi
  if [[ ! -f "${input}" ]]; then
    echo "[frontend][error] input dat not found: ${input}" >&2
    exit 2
  fi
  if [[ "${input}" != *.dat ]]; then
    echo "[frontend][error] input is not a .dat file: ${input}" >&2
    exit 2
  fi
  if [[ "${input}" != /* ]]; then
    input="$(pwd)/${input}"
  fi
  DAT_FILES+=("${input}")
done

if [[ -z "${OUTPUT_DIR}" ]]; then
  if [[ ${#DAT_FILES[@]} -eq 1 ]]; then
    INPUT_DIR="$(dirname "${DAT_FILES[0]}")"
    INPUT_NAME="$(basename "${DAT_FILES[0]}")"
    INPUT_STEM="${INPUT_NAME%.*}"
    OUTPUT_DIR="${INPUT_DIR}/${INPUT_STEM}.genhtml"
  else
    first_dir="$(dirname "${DAT_FILES[0]}")"
    OUTPUT_DIR="${first_dir}/coverage.genhtml"
  fi
fi

if [[ "${OUTPUT_DIR}" != /* ]]; then
  OUTPUT_DIR="$(pwd)/${OUTPUT_DIR}"
fi

MERGED_INFO="${OUTPUT_DIR}/merged.info"

if ! command -v verilator_coverage >/dev/null 2>&1; then
  echo "[frontend][error] verilator_coverage not found in PATH" >&2
  exit 2
fi

if ! command -v genhtml >/dev/null 2>&1; then
  echo "[frontend][error] genhtml not found in PATH" >&2
  exit 2
fi

mkdir -p "${OUTPUT_DIR}"

echo "[frontend] dat_count: ${#DAT_FILES[@]}"
printf '[frontend] dat: %s\n' "${DAT_FILES[@]}"
echo "[frontend] merged_info: ${MERGED_INFO}"
echo "[frontend] html_dir: ${OUTPUT_DIR}"

verilator_coverage -write-info "${MERGED_INFO}" "${DAT_FILES[@]}"
genhtml "${MERGED_INFO}" -o "${OUTPUT_DIR}" --ignore-errors range

echo "[frontend] index: ${OUTPUT_DIR}/index.html"
