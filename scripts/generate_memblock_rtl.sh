#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
XS_HOME="$(cd -- "${SCRIPT_DIR}/.." && pwd)"

MILL_BIN="${MILL:-mill}"
JVM_XMX="${JVM_XMX:-40G}"
JVM_XSS="${JVM_XSS:-256m}"
CONFIG="${CONFIG:-KunminghuV2Config}"
CHISEL_TARGET="${CHISEL_TARGET:-systemverilog}"
TARGET_DIR="${TARGET_DIR:-build_memblock/rtl}"
FIRTOOL_OPT="${FIRTOOL_OPT:--O=release --disable-annotation-unknown --lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none}"

export MEMBLOCK_PROJECT="${MEMBLOCK_PROJECT:-$(dirname "${XS_HOME}")}"

cd "${XS_HOME}"

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
echo "[memblock-rtl-v2] MEMBLOCK_PROJECT=${MEMBLOCK_PROJECT}"
echo "[memblock-rtl-v2] config=${CONFIG}"
echo "[memblock-rtl-v2] target=${TARGET_DIR}"

mill_args=(
  -i
  "-Djvm-xmx=${JVM_XMX}"
  "-Djvm-xss=${JVM_XSS}"
  xiangshan.runMain
  top.MemBlockTopMain
  --target-dir "${TARGET_DIR}"
  --config "${CONFIG}"
  --target "${CHISEL_TARGET}"
  --firtool-opt "${FIRTOOL_OPT}"
  --split-verilog
  --dump-fir
  --full-stacktrace
)

if [[ -n "${MEMBLOCK_FIRTOOL:-${FIRTOOL:-}}" ]]; then
  FIRTOOL_BIN="${MEMBLOCK_FIRTOOL:-${FIRTOOL:-}}"
  if [[ ! -x "${FIRTOOL_BIN}" ]]; then
    echo "error: firtool is not executable: ${FIRTOOL_BIN}" >&2
    exit 1
  fi
  mill_args+=(--firtool-binary-path "${FIRTOOL_BIN}")
  echo "[memblock-rtl-v2] firtool=${FIRTOOL_BIN}"
else
  echo "[memblock-rtl-v2] firtool=build.sc resolver default"
fi

"${MILL_BIN}" "${mill_args[@]}" "$@"

for arg in "$@"; do
  case "${arg}" in
    --version|--help|--xs-help)
      exit 0
      ;;
  esac
done

for required in "${TARGET_DIR}/filelist.f" "${TARGET_DIR}/MemBlock.sv" "${TARGET_DIR}/MemBlockTop.sv"; do
  if [[ ! -s "${required}" ]]; then
    echo "error: expected generated file is missing or empty: ${required}" >&2
    exit 1
  fi
done

echo "[memblock-rtl-v2] generated:"
wc -l "${TARGET_DIR}/filelist.f"
ls -lh "${TARGET_DIR}/MemBlock.sv" "${TARGET_DIR}/MemBlockTop.sv"
