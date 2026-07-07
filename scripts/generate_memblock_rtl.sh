#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
XS_HOME="$(cd -- "${SCRIPT_DIR}/.." && pwd)"

TARGET_BUILD_DIR="${TARGET_BUILD_DIR:-build_memblock}"
TARGET_RTL_DIR="${TARGET_RTL_DIR:-${TARGET_BUILD_DIR}/rtl}"
REFERENCE_RTL_DIR="${REFERENCE_RTL_DIR:-build/rtl}"
CONFIG="${CONFIG:-DefaultConfig}"
CHISEL_TARGET="${CHISEL_TARGET:-systemverilog}"
JVM_XMX="${JVM_XMX:-40G}"
JVM_XSS="${JVM_XSS:-256m}"
CHECK_REFERENCE="${CHECK_REFERENCE:-1}"
FORCE_REGENERATE="${FORCE_REGENERATE:-1}"

export MEMBLOCK_XS_HOME="${MEMBLOCK_XS_HOME:-${XS_HOME}}"
export MEMBLOCK_PROJECT="${MEMBLOCK_PROJECT:-$(dirname "${XS_HOME}")}"
export NOOP_HOME="${NOOP_HOME:-${XS_HOME}}"

cd "${XS_HOME}"

if (( $# > 0 )); then
  echo "error: command-line passthrough is not supported by the V2 memblock RTL flow" >&2
  echo "hint: override Makefile variables with environment variables, for example CONFIG=DefaultConfig" >&2
  exit 1
fi

missing_submodules=()
while read -r status path _; do
  if [[ "${status}" == -* || ! -e "${path}/.git" ]]; then
    missing_submodules+=("${path}")
  fi
done < <(git submodule status --recursive)

if (( ${#missing_submodules[@]} > 0 )); then
  echo "error: required submodules are not initialized:" >&2
  printf '  %s\n' "${missing_submodules[@]}" >&2
  echo "hint: run git submodule update --init --recursive before generating V2 RTL" >&2
  exit 1
fi

echo "[memblock-rtl-v2] XS_HOME=${XS_HOME}"
echo "[memblock-rtl-v2] MEMBLOCK_XS_HOME=${MEMBLOCK_XS_HOME}"
echo "[memblock-rtl-v2] MEMBLOCK_PROJECT=${MEMBLOCK_PROJECT}"
echo "[memblock-rtl-v2] NOOP_HOME=${NOOP_HOME}"
echo "[memblock-rtl-v2] target_build=${TARGET_BUILD_DIR}"
echo "[memblock-rtl-v2] target_rtl=${TARGET_RTL_DIR}"
echo "[memblock-rtl-v2] reference_rtl=${REFERENCE_RTL_DIR}"
echo "[memblock-rtl-v2] config=${CONFIG}"
echo "[memblock-rtl-v2] force_regenerate=${FORCE_REGENERATE}"

if [[ "${FORCE_REGENERATE}" != 0 ]]; then
  rm -rf "${TARGET_RTL_DIR}"
fi

make_cmd=(make)
if [[ "${FORCE_REGENERATE}" != 0 ]]; then
  make_cmd+=(-B)
fi
make_cmd+=(verilog)

make_vars=(
  "BUILD_DIR=${TARGET_BUILD_DIR}"
  "CONFIG=${CONFIG}"
  "CHISEL_TARGET=${CHISEL_TARGET}"
  "JVM_XMX=${JVM_XMX}"
  "JVM_XSS=${JVM_XSS}"
)

for var in NUM_CORES ISSUE FPGA PLDM RELEASE FIRTOOL DFX SRAM_WITH_CTL ENABLE_NS \
           CHI_ADDR_WIDTH L2_CACHE_SIZE L3_CACHE_SIZE HART_ID_BITS DISABLE_XMR \
           YAML_CONFIG DUMP_CSR; do
  if [[ -n "${!var:-}" ]]; then
    make_vars+=("${var}=${!var}")
  fi
done

echo "[memblock-rtl-v2] generating RTL with whole-core top.TopMain into ${TARGET_BUILD_DIR}"
"${make_cmd[@]}" "${make_vars[@]}"

for required in "${TARGET_RTL_DIR}/filelist.f" "${TARGET_RTL_DIR}/MemBlock.sv"; do
  if [[ ! -s "${required}" ]]; then
    echo "error: expected generated file is missing or empty: ${required}" >&2
    exit 1
  fi
done

mapfile -t ext_mem_models < <(
  find "${TARGET_RTL_DIR}" -maxdepth 1 -type f -name '*_ext.v' -printf '%f\n' | sort
)
if (( ${#ext_mem_models[@]} == 0 )); then
  echo "error: generated ext memory model list is empty in ${TARGET_RTL_DIR}" >&2
  exit 1
fi

filelist_tmp="$(mktemp)"
grep -vE '(^|/)[^/]+_ext\.v$' "${TARGET_RTL_DIR}/filelist.f" > "${filelist_tmp}"
printf '%s\n' "${ext_mem_models[@]}" >> "${filelist_tmp}"
mv "${filelist_tmp}" "${TARGET_RTL_DIR}/filelist.f"

if [[ -e "${TARGET_RTL_DIR}/MemBlockTop.sv" ]]; then
  echo "error: stale standalone MemBlockTop.sv exists in ${TARGET_RTL_DIR}" >&2
  echo "hint: V2 must generate build_memblock through top.TopMain, not top.MemBlockTopMain" >&2
  exit 1
fi

if [[ "${CHECK_REFERENCE}" != 0 ]]; then
  if [[ ! -s "${REFERENCE_RTL_DIR}/MemBlock.sv" ]]; then
    echo "error: reference MemBlock RTL is missing: ${REFERENCE_RTL_DIR}/MemBlock.sv" >&2
    echo "hint: generate whole-core RTL first, or set CHECK_REFERENCE=0 for generation-only" >&2
    exit 1
  fi
  if ! cmp -s "${REFERENCE_RTL_DIR}/MemBlock.sv" "${TARGET_RTL_DIR}/MemBlock.sv"; then
    echo "error: generated ${TARGET_RTL_DIR}/MemBlock.sv differs from ${REFERENCE_RTL_DIR}/MemBlock.sv" >&2
    exit 1
  fi
fi

echo "[memblock-rtl-v2] generated:"
wc -l "${TARGET_RTL_DIR}/filelist.f"
ls -lh "${TARGET_RTL_DIR}/MemBlock.sv"
