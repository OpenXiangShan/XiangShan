#!/usr/bin/env bash

frontend_pylib_path() {
  local repo_dir="$1"
  local sim="${TB_FRONTEND_SIM:-verilator}"

  case "${sim,,}" in
    verilator|vcs) printf '%s/build-frontend/pylib-%s\n' "${repo_dir}" "${sim,,}" ;;
    *)
      echo "[frontend][error] TB_FRONTEND_SIM must be one of: verilator vcs" >&2
      return 2
      ;;
  esac
}

frontend_artifacts_root_path() {
  local repo_dir="$1"
  printf '%s/build-frontend/artifacts\n' "${repo_dir}"
}
