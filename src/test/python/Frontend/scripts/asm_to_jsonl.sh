#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FRONTEND_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${FRONTEND_DIR}/../../../.." && pwd)"

usage() {
  cat <<'EOF'
Usage:
  src/test/python/Frontend/scripts/asm_to_jsonl.sh <case.S> [bin_path] [trace_jsonl_path]

Environment variables:
  NEMU_EXEC       NEMU executable path
                  default: <repo>/ready-to-run/riscv64-nemu-interpreter
  RISCV_GCC       RISC-V gcc command. If unset, auto-detects common toolchain names.
  RISCV_OBJCOPY   RISC-V objcopy command. If unset, auto-detects common toolchain names.
  LINK_ADDR       Link/load address for the assembly text
                  default: 0x10001000
  NEMU_MEM_BASE   Base address of the NEMU binary memory image
                  default: 0x10000000
  NEMU_MAX_INSTR  Pass -I to NEMU when > 0
                  default: 0
  TRACE_LIMIT     Limit converted trace entries when > 0
                  default: 0
  NEMU_LOG_PATH   Raw NEMU log path
                  default: <repo>/NEMU/logs/<case>.nemu.log
  PYTHON          Python command used to call the existing converter
                  default: python from the caller's activated environment
  KEEP_ELF        Keep ELF/MAP/raw bin under ready-to-run when set to 1
                  default: 0

Outputs:
  bin_path defaults to <repo>/src/test/python/Frontend/tests/asm_cases/generated/<case>.bin.
  trace_jsonl_path defaults to <repo>/NEMU/logs/<case>.trace.jsonl.
  NEMU_LOG_PATH defaults to <repo>/NEMU/logs/<case>.nemu.log.

The final .bin is already padded for NEMU. No separate raw or _padded bin is
left behind unless KEEP_ELF=1 is set for debugging artifacts.
EOF
}

if [[ $# -lt 1 || $# -gt 3 ]]; then
  usage
  exit 1
fi

ASM_PATH="$1"
if [[ ! -f "${ASM_PATH}" ]]; then
  echo "[asm-to-jsonl][error] .S not found: ${ASM_PATH}" >&2
  exit 2
fi

ASM_ABS="$(cd "$(dirname "${ASM_PATH}")" && pwd)/$(basename "${ASM_PATH}")"
CASE_NAME="$(basename "${ASM_PATH}")"
CASE_STEM="${CASE_NAME%.*}"

NEMU_EXEC="${NEMU_EXEC:-${REPO_DIR}/ready-to-run/riscv64-nemu-interpreter}"
LINK_ADDR="${LINK_ADDR:-0x10001000}"
NEMU_MEM_BASE="${NEMU_MEM_BASE:-0x10000000}"
NEMU_MAX_INSTR="${NEMU_MAX_INSTR:-0}"
TRACE_LIMIT="${TRACE_LIMIT:-0}"
KEEP_ELF="${KEEP_ELF:-0}"

BIN_PATH="${2:-${FRONTEND_DIR}/tests/asm_cases/generated/${CASE_STEM}.bin}"
TRACE_JSONL_PATH="${3:-${REPO_DIR}/NEMU/logs/${CASE_STEM}.trace.jsonl}"
NEMU_LOG_PATH="${NEMU_LOG_PATH:-${REPO_DIR}/NEMU/logs/${CASE_STEM}.nemu.log}"

if [[ ! -x "${NEMU_EXEC}" ]]; then
  echo "[asm-to-jsonl][error] NEMU executable not found or not executable: ${NEMU_EXEC}" >&2
  exit 2
fi

find_tool() {
  local env_value="$1"
  shift
  if [[ -n "${env_value}" ]]; then
    if command -v "${env_value}" >/dev/null 2>&1; then
      command -v "${env_value}"
      return 0
    fi
    return 1
  fi
  local candidate
  for candidate in "$@"; do
    if command -v "${candidate}" >/dev/null 2>&1; then
      command -v "${candidate}"
      return 0
    fi
  done
  return 1
}

RISCV_GCC="$(find_tool "${RISCV_GCC:-}" \
  riscv64-unknown-elf-gcc \
  riscv64-linux-gnu-gcc \
  riscv64-unknown-linux-gnu-gcc)" || {
  echo "[asm-to-jsonl][error] RISC-V gcc not found. Tried RISCV_GCC, riscv64-unknown-elf-gcc, riscv64-linux-gnu-gcc, riscv64-unknown-linux-gnu-gcc" >&2
  exit 2
}

RISCV_OBJCOPY="$(find_tool "${RISCV_OBJCOPY:-}" \
  riscv64-unknown-elf-objcopy \
  riscv64-linux-gnu-objcopy \
  riscv64-unknown-linux-gnu-objcopy)" || {
  echo "[asm-to-jsonl][error] RISC-V objcopy not found. Tried RISCV_OBJCOPY, riscv64-unknown-elf-objcopy, riscv64-linux-gnu-objcopy, riscv64-unknown-linux-gnu-objcopy" >&2
  exit 2
}

PYTHON_BIN="$(find_tool "${PYTHON:-}" python)" || {
  echo "[asm-to-jsonl][error] Python not found. Tried PYTHON and python from the activated environment" >&2
  exit 2
}

if ! command -v "${RISCV_GCC}" >/dev/null 2>&1; then
  echo "[asm-to-jsonl][error] RISC-V gcc not found: ${RISCV_GCC}" >&2
  exit 2
fi

if ! command -v "${RISCV_OBJCOPY}" >/dev/null 2>&1; then
  echo "[asm-to-jsonl][error] RISC-V objcopy not found: ${RISCV_OBJCOPY}" >&2
  exit 2
fi

pad_size=$((LINK_ADDR - NEMU_MEM_BASE))
if [[ "${pad_size}" -lt 0 ]]; then
  echo "[asm-to-jsonl][error] LINK_ADDR must be >= NEMU_MEM_BASE: LINK_ADDR=${LINK_ADDR} NEMU_MEM_BASE=${NEMU_MEM_BASE}" >&2
  exit 2
fi

mkdir -p "$(dirname "${BIN_PATH}")"
mkdir -p "$(dirname "${NEMU_LOG_PATH}")"
mkdir -p "$(dirname "${TRACE_JSONL_PATH}")"

WORK_DIR="$(mktemp -d "${TMPDIR:-/tmp}/asm-to-jsonl.${CASE_STEM}.XXXXXX")"
cleanup() {
  if [[ "${KEEP_ELF}" != "1" ]]; then
    rm -rf "${WORK_DIR}"
  fi
}
trap cleanup EXIT

ELF_PATH="${WORK_DIR}/${CASE_STEM}.elf"
MAP_PATH="${WORK_DIR}/${CASE_STEM}.map"
RAW_BIN_PATH="${WORK_DIR}/${CASE_STEM}.raw.bin"
PAD_PATH="${WORK_DIR}/${CASE_STEM}.pad.bin"

echo "[asm-to-jsonl] repo: ${REPO_DIR}"
echo "[asm-to-jsonl] asm: ${ASM_ABS}"
echo "[asm-to-jsonl] link_addr: ${LINK_ADDR}"
echo "[asm-to-jsonl] nemu_mem_base: ${NEMU_MEM_BASE}"
echo "[asm-to-jsonl] pad_size: ${pad_size}"
echo "[asm-to-jsonl] bin: ${BIN_PATH}"
echo "[asm-to-jsonl] nemu_exec: ${NEMU_EXEC}"
echo "[asm-to-jsonl] nemu_log: ${NEMU_LOG_PATH}"
echo "[asm-to-jsonl] trace_jsonl: ${TRACE_JSONL_PATH}"
echo "[asm-to-jsonl] python: ${PYTHON_BIN}"

"${RISCV_GCC}" \
  -march=rv64gc_zicsr_zifencei \
  -mabi=lp64 \
  -static \
  -no-pie \
  -nostdlib \
  -nostartfiles \
  -Wl,--build-id=none \
  -Wl,-Ttext="${LINK_ADDR}" \
  -Wl,-e,_start \
  -Wl,-Map="${MAP_PATH}" \
  -o "${ELF_PATH}" \
  "${ASM_ABS}"

objcopy_adjust=$((-LINK_ADDR))
"${RISCV_OBJCOPY}" -O binary -j .text --change-addresses="${objcopy_adjust}" "${ELF_PATH}" "${RAW_BIN_PATH}"

if [[ "${pad_size}" -gt 0 ]]; then
  truncate -s "${pad_size}" "${PAD_PATH}"
  cat "${PAD_PATH}" "${RAW_BIN_PATH}" > "${BIN_PATH}"
else
  cp "${RAW_BIN_PATH}" "${BIN_PATH}"
fi

if [[ "${KEEP_ELF}" == "1" ]]; then
  cp "${ELF_PATH}" "${REPO_DIR}/ready-to-run/${CASE_STEM}.elf"
  cp "${MAP_PATH}" "${REPO_DIR}/ready-to-run/${CASE_STEM}.map"
  cp "${RAW_BIN_PATH}" "${REPO_DIR}/ready-to-run/${CASE_STEM}.raw.bin"
  echo "[asm-to-jsonl] kept debug artifacts under ready-to-run/"
fi

PYTHONPATH="${FRONTEND_DIR}:${REPO_DIR}/build-frontend/pylib:${PYTHONPATH:-}" \
"${PYTHON_BIN}" "${FRONTEND_DIR}/tools/nemu_bin_to_golden_trace.py" \
  "${BIN_PATH}" "${TRACE_JSONL_PATH}" \
  --nemu-exec "${NEMU_EXEC}" \
  --nemu-log "${NEMU_LOG_PATH}" \
  --nemu-max-instr "${NEMU_MAX_INSTR}" \
  --trace-limit "${TRACE_LIMIT}"

echo "[asm-to-jsonl] done"
echo "[asm-to-jsonl] bin=${BIN_PATH}"
echo "[asm-to-jsonl] log=${NEMU_LOG_PATH}"
echo "[asm-to-jsonl] trace=${TRACE_JSONL_PATH}"
