#!/bin/bash

set -euo pipefail

SRC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DST_ROOT="${CODEX_HOME:-$HOME/.codex}/skills"
DST_DIR="$DST_ROOT/mem-ut-session-regress"

mkdir -p "$DST_ROOT"
rm -rf "$DST_DIR"
cp -r "$SRC_DIR" "$DST_DIR"

echo "Installed skill to: $DST_DIR"
