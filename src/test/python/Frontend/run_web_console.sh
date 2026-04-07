#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

HOST="${TB_WEB_HOST:-172.19.20.29}"
PORT="${TB_WEB_PORT:-9900}"
LOG_LEVEL="${TB_ENV_LOG_LEVEL:-INFO}"

cd "${SCRIPT_DIR}"

echo "[frontend] starting web console on http://${HOST}:${PORT}"
echo "[frontend] TB_ENV_LOG_LEVEL=${LOG_LEVEL}"
export TB_ENV_LOG_LEVEL="${LOG_LEVEL}"
export PYTHONPATH="${SCRIPT_DIR}:${REPO_DIR}/build-frontend/pylib:${PYTHONPATH:-}"

python3 -m uvicorn webui.server:app --host "${HOST}" --port "${PORT}"
