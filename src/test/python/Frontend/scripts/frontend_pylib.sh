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

frontend_configure_vcs_preload() {
  local repo_dir="$1"
  local sim="${TB_FRONTEND_SIM:-verilator}"
  local dut_lib

  [[ "${sim,,}" == "vcs" ]] || return 0

  dut_lib="$(frontend_pylib_path "${repo_dir}")/Frontend/libUTFrontend.so"
  if [[ ! -f "${dut_lib}" ]]; then
    echo "[frontend][error] completed VCS DUT library not found: ${dut_lib}" >&2
    echo "[frontend][error] rerun make frontend-vcs before starting a VCS DUT test" >&2
    return 2
  fi
  case ":${LD_PRELOAD:-}:" in
    *":${dut_lib}:"*) ;;
    *) export LD_PRELOAD="${dut_lib}${LD_PRELOAD:+:${LD_PRELOAD}}" ;;
  esac
}
