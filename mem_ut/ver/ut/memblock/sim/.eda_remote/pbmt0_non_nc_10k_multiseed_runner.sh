#!/usr/bin/env bash
# Temporary remote regression runner for the strict PBMT=00/non-NC campaign.
# This is deliberately outside source directories.  It consumes the already
# compiled simv and stops at the first seed that fails the full acceptance set.

set -u -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIM_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
MEMBLOCK_XS_ROOT="$(cd "${SIM_DIR}/../../../../../" && pwd)"

export MEMBLOCK_XS_HOME="${MEMBLOCK_XS_HOME:-${MEMBLOCK_XS_ROOT}}"
export MEMBLOCK_PROJECT="${MEMBLOCK_PROJECT:-$(dirname "${MEMBLOCK_XS_HOME}")}" 

source /usr/share/Modules/init/bash >/dev/null 2>&1 || true
module load synopsys/vcs/Q-2020.03-SP2 license
module load synopsys/verdi/R-2020.12-SP1 license

MODE="pbmt0_non_nc_10k_multiseed_20260906"
TC="basicTest"
TS="memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq"
CFG="tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k"
PLUS_ARG="+MEMBLOCK_MAIN_TRANS_NUM=10000 +MEMBLOCK_CHECK_TRIGGER_EN=0"
START_SEED="${START_SEED:-710001}"
END_SEED="${END_SEED:-710100}"

seed_log() {
    local seed="$1"
    printf '%s/%s/log/tc=%s_ts=%s_cfg=%s_seed=%s_rtl.log' \
        "${SIM_DIR}" "${MODE}" "${TC}" "${TS}" "${CFG}" "${seed}"
}

# Abstract responsibility: accept one completed simulation log only if it
# proves a complete 10k run and a clean responder-aware terminal drain.
validate_seed() {
    local seed="$1"
    local log main_count compare_count max_uid err_line fatal_line

    log="$(seed_log "${seed}")"
    [[ -f "${log}" ]] || return 1

    main_count="$(grep -c '\[RM_LS_TRACE_MAIN\] node=MAIN_TABLE' "${log}" || true)"
    compare_count="$(grep -E -c '\[RM_LS_COMPARE\] uid .* committed ROB/data/exception compare PASS' "${log}" || true)"
    max_uid="$(grep -Eo '\[RM_LS_TRACE_MAIN\] node=MAIN_TABLE uid=[0-9]+' "${log}" |
        sed -E 's/.*uid=//' | sort -n | tail -1 || true)"
    err_line="$(grep -E '^UVM_ERROR[[:space:]]*:[[:space:]]*[0-9]+$' "${log}" | tail -1 || true)"
    fatal_line="$(grep -E '^UVM_FATAL[[:space:]]*:[[:space:]]*[0-9]+$' "${log}" | tail -1 || true)"

    [[ "${main_count}" == "10000" ]] || return 1
    [[ "${compare_count}" == "10000" ]] || return 1
    [[ "${max_uid}" == "9999" ]] || return 1
    [[ "${err_line}" =~ ^UVM_ERROR[[:space:]]*:[[:space:]]*0$ ]] || return 1
    [[ "${fatal_line}" =~ ^UVM_FATAL[[:space:]]*:[[:space:]]*0$ ]] || return 1
    grep -q 'global stop committed after 1000\.000ns quiet memory-responder window' "${log}" || return 1
    grep -q 'DCache responder published terminal idle and stopped' "${log}" || return 1
    grep -q 'SBuffer responder published terminal idle and stopped' "${log}" || return 1
    grep -q 'real dispatch smoke virtual sequence completed' "${log}" || return 1
    ! grep -q 'RM_LS_COMPARE.*FAIL' "${log}" || return 1
}

# Abstract responsibility: run exactly one previously unverified seed without
# recompiling, then validate its completion before the next seed is admitted.
run_one_seed() {
    local seed="$1"
    local runner_log

    if validate_seed "${seed}"; then
        printf 'SKIP_PASS seed=%s (existing complete log)\n' "${seed}"
        return 0
    fi

    runner_log="/tmp/${MODE}_seed_${seed}.make.log"
    printf 'RUN seed=%s\n' "${seed}"
    if ! make -C "${SIM_DIR}" batch_run \
        tc="${TC}" ts="${TS}" cfg="${CFG}" mode="${MODE}" seed="${seed}" \
        plus_arg="${PLUS_ARG}" wave=off pl=UVM_LOW timeout_ns=100000000 \
        >"${runner_log}" 2>&1; then
        printf 'FAIL seed=%s reason=make_exit_nonzero runner_log=%s\n' \
            "${seed}" "${runner_log}" >&2
        return 1
    fi

    if ! validate_seed "${seed}"; then
        printf 'FAIL seed=%s reason=post_run_acceptance_check log=%s\n' \
            "${seed}" "$(seed_log "${seed}")" >&2
        return 1
    fi

    printf 'PASS seed=%s\n' "${seed}"
}

for seed in $(seq "${START_SEED}" "${END_SEED}"); do
    run_one_seed "${seed}" || exit 1
done

printf 'COMPLETE strict_pbmt0_non_nc_10k_multiseed passed=100\n'
