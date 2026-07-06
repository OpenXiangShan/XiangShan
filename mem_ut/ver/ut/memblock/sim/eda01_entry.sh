#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIM_DIR="$SCRIPT_DIR"
MEMBLOCK_XS_ROOT="$(cd "$SIM_DIR/../../../../../" && pwd)"
MEMBLOCK_PROJECT_PARENT="$(dirname "$MEMBLOCK_XS_ROOT")"

TARGET="${1:-}"
if [[ -z "$TARGET" ]]; then
    echo "usage: $0 <make-target> [make assignments...]" >&2
    exit 1
fi
shift

cd "$SIM_DIR"
export MEMBLOCK_XS_HOME="${MEMBLOCK_XS_HOME:-$MEMBLOCK_XS_ROOT}"
export MEMBLOCK_PROJECT="${MEMBLOCK_PROJECT:-$MEMBLOCK_PROJECT_PARENT}"
exec make "$TARGET" "$@"
