#!/usr/bin/env bash

frontend_pylib_path() {
  local repo_dir="$1"
  local override="${TB_FRONTEND_PYLIB:-}"
  local sim="${TB_FRONTEND_SIM:-verilator}"

  if [[ -n "${override}" ]]; then
    if [[ "${override}" == "~" ]]; then
      printf '%s\n' "${HOME}"
    elif [[ "${override}" == "~/"* ]]; then
      printf '%s\n' "${HOME}/${override#~/}"
    else
      printf '%s\n' "${override}"
    fi
    return 0
  fi

  case "${sim,,}" in
    verilator|vcs) printf '%s/build-frontend/pylib-%s\n' "${repo_dir}" "${sim,,}" ;;
    *)
      echo "[frontend][error] TB_FRONTEND_SIM must be one of: verilator vcs" >&2
      return 2
      ;;
  esac
}
