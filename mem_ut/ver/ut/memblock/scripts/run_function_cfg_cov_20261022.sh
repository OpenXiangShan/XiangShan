#!/usr/bin/env bash
# Run all functional plus cfg presets with one shared VCS code-coverage mode.
# Each preset gets ten random seeds. The common plusargs intentionally override
# the preset request count and trigger metadata check without editing source cfgs.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIM_DIR="$(cd "$SCRIPT_DIR/../sim" && pwd)"
RUNNER="$SCRIPT_DIR/run_eda_regression.sh"
MODE="${MODE:-functional_cfg_cov_20261022}"
RESULT_ROOT="$SIM_DIR/${MODE}_results"
COUNT="${COUNT:-10}"
JOBS="${JOBS:-20}"
TRANS_NUM="${TRANS_NUM:-1000}"
TIMEOUT_NS="${TIMEOUT_NS:-1000000000}"

if [[ ! -x "$RUNNER" ]]; then
    echo "missing regression runner: $RUNNER" >&2
    exit 2
fi

mkdir -p "$RESULT_ROOT"

# cfg | testcase | virtual sequence
CASES=(
    'default|tc_dispatch_real_smoke|virtual_base_sequence'
    'tc_dispatch_real_smoke|tc_dispatch_real_smoke|virtual_base_sequence'
    'tc_dispatch_real_store_smoke|tc_dispatch_real_store_smoke|virtual_base_sequence'
    'tc_dispatch_real_store_wb_smoke|tc_dispatch_real_store_wb_smoke|virtual_base_sequence'
    'tc_dispatch_real_store_sta_wb_smoke|tc_dispatch_real_store_sta_wb_smoke|virtual_base_sequence'
    'tc_dispatch_real_multi_store_wb_smoke|tc_dispatch_real_multi_store_wb_smoke|virtual_base_sequence'
    'tc_dispatch_real_mixed_wb_smoke|tc_dispatch_real_mixed_wb_smoke|virtual_base_sequence'
    'tc_dispatch_real_mixed_sta_wb_smoke|tc_dispatch_real_mixed_sta_wb_smoke|virtual_base_sequence'
    'tc_dispatch_real_smoke_flushsb|tc_dispatch_real_smoke|virtual_base_sequence'
    'tc_dispatch_real_cancel_reconcile_smoke|basicTest|memblock_dispatch_real_cancel_reconcile_vseq'
    'tc_dispatch_real_l2cache_model|tc_dispatch_real_smoke|virtual_base_sequence'
    'tc_dispatch_real_mmu_sv39_smoke|basicTest|memblock_dispatch_real_smoke_vseq'
    'tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k|basicTest|memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq'
    'tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16|basicTest|memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq'
    'tc_dispatch_real_mmu_sv39_strict_nc_100k|basicTest|memblock_dispatch_real_smoke_vseq'
    'tc_l2tlb_fault_2pct_no_trigger|basicTest|memblock_dispatch_real_smoke_vseq'
    'tc_l2tlb_pbmt_response_fault|basicTest|memblock_l2tlb_pbmt_response_fault_vseq'
    'tc_l2tlb_pbmt_csr_gate_nc|tc_l2tlb_pbmt_csr_gate|virtual_base_sequence'
    'tc_l2tlb_pbmt_csr_gate_io|tc_l2tlb_pbmt_csr_gate|virtual_base_sequence'
    'tc_l2tlb_ppn_reuse_smoke|tc_l2tlb_ppn_reuse_smoke|virtual_base_sequence'
    'tc_memblock_csr_random_config|basicTest|memblock_csr_random_config_vseq'
    'tc_sfence_stress_1k|basicTest|memblock_dispatch_real_smoke_vseq'
    'tc_sfence_stress_10k|basicTest|memblock_dispatch_real_smoke_vseq'
    'csr_sfence_check_store_rob_control|basicTest|memblock_dispatch_real_smoke_vseq'
    'csr_sfence_check_store_manual_control|basicTest|memblock_dispatch_manual_control_vseq'
    'tc_boundary_addr_reuse_store_cross8_gate|tc_boundary_addr_reuse_gate|virtual_base_sequence'
    'tc_csr_scalar_load_10k|basicTest|memblock_csr_scalar_load_10k_vseq'
    'tc_csr_scalar_store_10k|basicTest|memblock_csr_scalar_store_10k_vseq'
    'tc_csr_scalar_mixed_10k|basicTest|memblock_csr_scalar_mixed_10k_vseq'
    'tc_csr_scalar_store_sta_std_10k|basicTest|memblock_csr_scalar_store_sta_std_10k_vseq'
)

SUMMARY="$RESULT_ROOT/summary.tsv"
printf 'cfg\ttc\tts\tresult\trc\tresult_dir\n' >"$SUMMARY"

compile=1
overall_rc=0
for item in "${CASES[@]}"; do
    IFS='|' read -r cfg tc ts <<<"$item"
    out="$RESULT_ROOT/$cfg"
    mkdir -p "$out"

    args=(
        --tc "$tc"
        --cfg "$cfg"
        --ts "$ts"
        --mode "$MODE"
        --count "$COUNT"
        --jobs "$JOBS"
        --coverage
        --no-wave
        --verbosity UVM_LOW
        --timeout-ns "$TIMEOUT_NS"
        --plusarg "+MEMBLOCK_MAIN_TRANS_NUM=$TRANS_NUM"
        --plusarg '+MEMBLOCK_CHECK_TRIGGER_EN=0'
        --output-dir "$out"
    )
    if [[ "$cfg" == "default" &&
          "$tc" == "tc_dispatch_real_smoke" &&
          "$ts" == "virtual_base_sequence" ]]; then
        args+=(--plusarg '+MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1')
    fi
    if (( compile )); then
        args+=(--compile)
        compile=0
    else
        args+=(--no-compile)
    fi

    echo "[FUNCTION-CFG] start cfg=$cfg tc=$tc ts=$ts mode=$MODE count=$COUNT jobs=$JOBS"
    set +e
    "$RUNNER" "${args[@]}"
    rc=$?
    set -e

    result="UNKNOWN"
    if [[ -r "$out/summary.log" ]]; then
        result="$(awk -F= '$1 == "result" {print $2; exit}' "$out/summary.log")"
        [[ -n "$result" ]] || result="UNKNOWN"
    fi
    printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$cfg" "$tc" "$ts" "$result" "$rc" "$out" >>"$SUMMARY"
    echo "[FUNCTION-CFG] done cfg=$cfg result=$result rc=$rc"
    (( rc == 0 )) || overall_rc=1
done

echo "[FUNCTION-CFG] summary=$SUMMARY"
exit "$overall_rc"
