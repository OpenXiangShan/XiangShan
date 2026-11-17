#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MEMBLOCK_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SIM_DIR="$MEMBLOCK_DIR/sim"
MEMBLOCK_XS_ROOT="$(cd "$MEMBLOCK_DIR/../../../.." && pwd)"
MEMBLOCK_PROJECT_PARENT="$(dirname "$MEMBLOCK_XS_ROOT")"

TARGET="${1:-}"
if [[ -z "$TARGET" ]]; then
    echo "usage: $0 <make-target> [make assignments...]" >&2
    exit 1
fi
shift

cd "$SIM_DIR"
export MEMBLOCK_XS_HOME="${MEMBLOCK_XS_HOME:-$MEMBLOCK_XS_ROOT}"
# 外置 RTL 与测试框架分别寻址；命令行 make 赋值仍可覆盖该缺省值。
export MEMBLOCK_RTL_HOME="${MEMBLOCK_RTL_HOME:-$MEMBLOCK_XS_HOME}"
export MEMBLOCK_PROJECT="${MEMBLOCK_PROJECT:-$MEMBLOCK_PROJECT_PARENT}"
exec make "$TARGET" "$@"
