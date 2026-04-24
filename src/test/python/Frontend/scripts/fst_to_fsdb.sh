#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${VERDI_HOME:-}" ]]; then
  echo "[frontend][error] VERDI_HOME is not set" >&2
  exit 2
fi

VCD2FSDB="${VERDI_HOME}/bin/vcd2fsdb"
LD_LIBRARY_PATH_OVERRIDE_VALUE="/usr/lib/x86_64-linux-gnu:/lib/x86_64-linux-gnu"

usage() {
  cat <<'EOF'
Usage:
  ./scripts/fst_to_fsdb.sh <input.fst> [output.fsdb]

Examples:
  ./scripts/fst_to_fsdb.sh src/test/python/Frontend/data/microbench_test_bin_trace.fst
  ./scripts/fst_to_fsdb.sh in.fst out.fsdb

Notes:
  - Output defaults to <input_basename>.fsdb next to the input file.
  - The intermediate VCD is kept in a temporary directory and removed automatically.
EOF
}

if [[ $# -lt 1 || $# -gt 2 ]]; then
  usage
  exit 1
fi

if ! command -v fst2vcd >/dev/null 2>&1; then
  echo "[frontend][error] fst2vcd not found in PATH" >&2
  exit 2
fi

if [[ ! -x "${VCD2FSDB}" ]]; then
  echo "[frontend][error] vcd2fsdb not found under VERDI_HOME/bin" >&2
  exit 2
fi

INPUT_FST="$1"

if [[ ! -f "${INPUT_FST}" ]]; then
  echo "[frontend][error] input fst not found: ${INPUT_FST}" >&2
  exit 2
fi

INPUT_DIR="$(dirname "${INPUT_FST}")"
INPUT_NAME="$(basename "${INPUT_FST}")"
INPUT_STEM="${INPUT_NAME%.*}"
OUTPUT_FSDB="${2:-${INPUT_DIR}/${INPUT_STEM}.fsdb}"

if [[ "${OUTPUT_FSDB}" != /* ]]; then
  OUTPUT_FSDB="$(pwd)/${OUTPUT_FSDB}"
fi

mkdir -p "$(dirname "${OUTPUT_FSDB}")"

WORK_DIR="$(mktemp -d "${TMPDIR:-/tmp}/frontend-fst-to-fsdb.XXXXXX")"
TMP_VCD="${WORK_DIR}/${INPUT_STEM}.vcd"
trap 'rm -rf "${WORK_DIR}"' EXIT

echo "[frontend] fst: ${INPUT_FST}"
echo "[frontend] fsdb: ${OUTPUT_FSDB}"
echo "[frontend] tmp_vcd: ${TMP_VCD}"

fst2vcd "${INPUT_FST}" -o "${TMP_VCD}"

(
  cd "${WORK_DIR}"
  # Prefer the host libstdc++ first so Verdi's wrapper can start on this machine.
  LD_LIBRARY_PATH_OVERRIDE="${LD_LIBRARY_PATH_OVERRIDE_VALUE}" \
    bash "${VCD2FSDB}" "${TMP_VCD}" -o "${OUTPUT_FSDB}"
)

echo "[frontend] done"
