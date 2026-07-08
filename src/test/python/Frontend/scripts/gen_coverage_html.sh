#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  ./scripts/gen_coverage_html.sh [--ignore-file FILE] [--omit-file FILE] [input.dat ... | input_dir] [output_dir]

Notes:
  - With no input, the script collects all .dat files under src/test/python/Frontend/data/.
  - A single directory input collects all .dat files directly under that directory.
  - Multiple .dat inputs are merged before generating HTML.
  - Output defaults to <input_stem>.genhtml/ for a single .dat input, or
    coverage.genhtml/ next to the input directory / default data directory.
  - The script will generate merged.info in the output directory.
  - If no --ignore-file is passed, src/test/python/Frontend/Frontend.ignore is
    used when present. Each non-empty, non-comment line is passed to genhtml as
    --exclude PATTERN.
  - If no --omit-file is passed, src/test/python/Frontend/Frontend.omit is used
    when present. Each non-empty, non-comment line is matched against source
    text and the matching lines are removed from the lcov data before genhtml.
  - genhtml is run with --ignore-errors range and --filter missing because the
    .dat file and the current build-frontend RTL sources may not be from the
    exact same build.
EOF
}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEFAULT_DATA_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)/data"
DEFAULT_IGNORE_FILE="$(cd "${SCRIPT_DIR}/.." && pwd)/Frontend.ignore"
DEFAULT_OMIT_FILE="$(cd "${SCRIPT_DIR}/.." && pwd)/Frontend.omit"

shopt -s nullglob

declare -a INPUTS=()
OUTPUT_DIR=""
IGNORE_FILE="${TB_LINE_COVERAGE_IGNORE:-${DEFAULT_IGNORE_FILE}}"
OMIT_FILE="${TB_LINE_COVERAGE_OMIT:-${DEFAULT_OMIT_FILE}}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --ignore-file)
      if [[ $# -lt 2 ]]; then
        echo "[frontend][error] --ignore-file requires a path" >&2
        exit 2
      fi
      IGNORE_FILE="$2"
      shift 2
      ;;
    --omit-file)
      if [[ $# -lt 2 ]]; then
        echo "[frontend][error] --omit-file requires a path" >&2
        exit 2
      fi
      OMIT_FILE="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      break
      ;;
    -*)
      echo "[frontend][error] unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
    *)
      break
      ;;
  esac
done

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
FILTERED_INFO="${OUTPUT_DIR}/merged.filtered.info"
declare -a GENHTML_EXCLUDES=()
declare -a GENHTML_OMITS=()

if [[ -n "${IGNORE_FILE}" && -f "${IGNORE_FILE}" ]]; then
  while IFS= read -r raw_line || [[ -n "${raw_line}" ]]; do
    line="${raw_line%%#*}"
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    if [[ -z "${line}" ]]; then
      continue
    fi
    if [[ "${line}" == include\ * ]]; then
      echo "[frontend][warn] skip include directive for genhtml exclude: ${line}" >&2
      continue
    fi
    if [[ "${line}" == *:* ]]; then
      echo "[frontend][warn] skip line-range waive for genhtml: ${line}" >&2
      continue
    fi
    GENHTML_EXCLUDES+=(--exclude "${line}")
  done < "${IGNORE_FILE}"
fi

if [[ -n "${OMIT_FILE}" && -f "${OMIT_FILE}" ]]; then
  while IFS= read -r raw_line || [[ -n "${raw_line}" ]]; do
    line="${raw_line%%#*}"
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    if [[ -z "${line}" ]]; then
      continue
    fi
    GENHTML_OMITS+=("${line}")
  done < "${OMIT_FILE}"
fi

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
if [[ -n "${IGNORE_FILE}" && -f "${IGNORE_FILE}" ]]; then
  echo "[frontend] ignore_file: ${IGNORE_FILE}"
fi
if [[ -n "${OMIT_FILE}" && -f "${OMIT_FILE}" ]]; then
  echo "[frontend] omit_file: ${OMIT_FILE}"
fi
if [[ ${#GENHTML_EXCLUDES[@]} -gt 0 ]]; then
  echo "[frontend] genhtml_exclude_count: $((${#GENHTML_EXCLUDES[@]} / 2))"
fi
if [[ ${#GENHTML_OMITS[@]} -gt 0 ]]; then
  echo "[frontend] source_omit_count: ${#GENHTML_OMITS[@]}"
fi

verilator_coverage -write-info "${MERGED_INFO}" "${DAT_FILES[@]}"
INFO_FOR_GENHTML="${MERGED_INFO}"
if [[ ${#GENHTML_OMITS[@]} -gt 0 ]]; then
  python - "${MERGED_INFO}" "${FILTERED_INFO}" "${GENHTML_OMITS[@]}" <<'PY'
from __future__ import annotations

import re
import sys
from pathlib import Path

merged_info = Path(sys.argv[1])
filtered_info = Path(sys.argv[2])
patterns = [re.compile(pattern) for pattern in sys.argv[3:]]


def omitted_lines(source: str) -> set[int]:
    path = Path(source)
    if not path.is_file():
        return set()
    return {
        lineno
        for lineno, text in enumerate(path.read_text(errors="ignore").splitlines(), 1)
        if any(pattern.search(text) for pattern in patterns)
    }


output: list[str] = []
record: list[str] = []
source = ""
omit: set[int] = set()
total_omitted = 0


def finish_record() -> None:
    global total_omitted
    if not record:
        return
    line_entries = []
    branch_entries = []
    other_entries = []
    omitted_in_record = 0
    for line in record:
        if line.startswith("DA:"):
            lineno = int(line[3:].split(",", 1)[0])
            if lineno in omit:
                omitted_in_record += 1
                continue
            line_entries.append(line)
        elif line.startswith("BRDA:"):
            lineno = int(line[5:].split(",", 1)[0])
            if lineno in omit:
                continue
            branch_entries.append(line)
        elif line.startswith("FNDA:"):
            parts = line[5:].split(",", 1)
            if len(parts) == 2 and parts[0].isdigit() and int(parts[0]) in omit:
                continue
            other_entries.append(line)
        elif line.startswith(("LF:", "LH:", "BRF:", "BRH:")):
            continue
        else:
            other_entries.append(line)

    total_omitted += omitted_in_record
    output.extend(other_entries)
    output.extend(line_entries)
    output.append(f"LF:{len(line_entries)}")
    output.append(f"LH:{sum(1 for line in line_entries if int(line.split(',', 1)[1]) > 0)}")
    output.extend(branch_entries)
    if branch_entries:
        output.append(f"BRF:{len(branch_entries)}")
        output.append(f"BRH:{sum(1 for line in branch_entries if line.rsplit(',', 1)[1] not in {'0', '-'})}")
    output.append("end_of_record")


for raw in merged_info.read_text(errors="ignore").splitlines():
    if raw.startswith("SF:"):
        finish_record()
        record = [raw]
        source = raw[3:]
        omit = omitted_lines(source)
        continue
    if raw == "end_of_record":
        finish_record()
        record = []
        source = ""
        omit = set()
        continue
    if record:
        record.append(raw)
    else:
        output.append(raw)

finish_record()
filtered_info.write_text("\n".join(output) + "\n", encoding="utf-8")
print(f"[frontend] omitted_source_lines: {total_omitted}")
PY
  INFO_FOR_GENHTML="${FILTERED_INFO}"
fi
genhtml "${INFO_FOR_GENHTML}" -o "${OUTPUT_DIR}" --ignore-errors range --filter missing "${GENHTML_EXCLUDES[@]}"

echo "[frontend] index: ${OUTPUT_DIR}/index.html"
