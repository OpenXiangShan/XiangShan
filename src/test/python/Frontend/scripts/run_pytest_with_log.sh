#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FRONTEND_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${FRONTEND_DIR}/../../../.." && pwd)"

LOG_DIR="${TB_REG_LOG_DIR:-${FRONTEND_DIR}/logs}"
mkdir -p "${LOG_DIR}"

if [[ -n "${TB_REG_LOG_FILE:-}" ]]; then
  LOG_FILE="${TB_REG_LOG_FILE}"
else
  TS="$(date +%Y%m%d_%H%M%S)"
  LOG_FILE="${LOG_DIR}/pytest_${TS}.log"
fi

mkdir -p "$(dirname "${LOG_FILE}")"

if [[ $# -eq 0 ]]; then
  set -- "${FRONTEND_DIR}/tests"
fi

CLI_LEVEL="${TB_LOG_CLI_LEVEL:-${TB_ENV_LOG_LEVEL:-INFO}}"
PYTEST_DISABLE_RERUNFAILURES="${TB_PYTEST_DISABLE_RERUNFAILURES:-1}"

echo "[frontend] saving regression log to: ${LOG_FILE}"
echo "[frontend] TB_ENV_LOG_LEVEL=${TB_ENV_LOG_LEVEL:-INFO}"
if [[ "${PYTEST_DISABLE_RERUNFAILURES}" != "0" ]]; then
  echo "[frontend] running: pytest -p no:rerunfailures -s -o log_cli=true --log-cli-level=${CLI_LEVEL} $*"
else
  echo "[frontend] running: pytest -s -o log_cli=true --log-cli-level=${CLI_LEVEL} $*"
fi

cd "${REPO_DIR}"
export PYTHONPATH="${FRONTEND_DIR}:${REPO_DIR}/build-frontend/pylib:${PYTHONPATH:-}"
PYTEST_CMD=(pytest -s -o log_cli=true --log-cli-level="${CLI_LEVEL}")
if [[ "${PYTEST_DISABLE_RERUNFAILURES}" != "0" ]]; then
  PYTEST_CMD+=(-p no:rerunfailures)
fi
PYTEST_CMD+=("$@")
"${PYTEST_CMD[@]}" 2>&1 | tee "${LOG_FILE}"
