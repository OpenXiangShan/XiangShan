#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  ./gen_coverage_html.sh <input.dat> [output_dir]

Notes:
  - Output defaults to <input_stem>.genhtml/ next to the input .dat file.
  - The script will generate merged.info in the output directory.
  - genhtml is run with --ignore-errors range because the .dat file and the
    current build-frontend RTL sources may not be from the exact same build.
EOF
}

if [[ $# -lt 1 || $# -gt 2 ]]; then
  usage
  exit 1
fi

INPUT_DAT="$1"
if [[ ! -f "${INPUT_DAT}" ]]; then
  echo "[frontend][error] input dat not found: ${INPUT_DAT}" >&2
  exit 2
fi

if [[ "${INPUT_DAT}" != /* ]]; then
  INPUT_DAT="$(pwd)/${INPUT_DAT}"
fi

INPUT_DIR="$(dirname "${INPUT_DAT}")"
INPUT_NAME="$(basename "${INPUT_DAT}")"
INPUT_STEM="${INPUT_NAME%.*}"
OUTPUT_DIR="${2:-${INPUT_DIR}/${INPUT_STEM}.genhtml}"

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

echo "[frontend] dat: ${INPUT_DAT}"
echo "[frontend] merged_info: ${MERGED_INFO}"
echo "[frontend] html_dir: ${OUTPUT_DIR}"

verilator_coverage -write-info "${MERGED_INFO}" "${INPUT_DAT}"
genhtml "${MERGED_INFO}" -o "${OUTPUT_DIR}" --ignore-errors range

echo "[frontend] index: ${OUTPUT_DIR}/index.html"
