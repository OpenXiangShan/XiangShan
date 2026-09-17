#!/usr/bin/env bash
# Frontend reproducible randomized DUT regression runner.
#
# Run from the repository root after activating the Frontend Python environment:
#   source /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate
#   src/test/python/Frontend/scripts/run_random_regression_suite.sh smoke
#   src/test/python/Frontend/scripts/run_random_regression_suite.sh nightly
#
# With no positional argument, the runner uses smoke mode. Smoke runs one seed;
# nightly runs the configured seed matrix. Both modes enable DUT tests and use
# Verilator unless TB_FRONTEND_SIM selects another available DUT package.
#
# Reproduce or override a run:
#   TB_SEED=0x20260910 \
#     src/test/python/Frontend/scripts/run_random_regression_suite.sh smoke
#   TB_RANDOM_NIGHTLY_SEEDS="0x20260910 0x20260911" \
#     src/test/python/Frontend/scripts/run_random_regression_suite.sh nightly
#
# Environment:
#   TB_FRONTEND_SIM                DUT package: verilator (default) or vcs.
#   TB_SEED                        Smoke seed (default: 0x20260909).
#   TB_TRANSLATION_RANDOM_COUNT    Translation ordinals per seed (default: 6).
#   TB_RANDOM_NIGHTLY_SEEDS        Space-separated nightly seed matrix
#                                  (default: 0x20260909 through 0x2026090b).
#   TB_PYTEST_DISABLE_RERUNFAILURES
#                                  Set to 0 only when reruns are intentional.
#
# The runner delegates logging and artifacts to run_pytest_with_log.sh, keeps
# running the remaining nightly seeds after a failure, and returns nonzero when
# any selected seed fails. Use --help to print the same command summary.
set -u -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FRONTEND_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${FRONTEND_DIR}/../../../.." && pwd)"

usage() {
  cat <<'EOF'
Usage:
  src/test/python/Frontend/scripts/run_random_regression_suite.sh [smoke|nightly]

The default mode is smoke. The runner enables DUT tests and selects Verilator
unless TB_FRONTEND_SIM specifies another available DUT package.

Examples:
  src/test/python/Frontend/scripts/run_random_regression_suite.sh smoke
  src/test/python/Frontend/scripts/run_random_regression_suite.sh nightly
  TB_SEED=0x20260910 \
    src/test/python/Frontend/scripts/run_random_regression_suite.sh smoke
  TB_RANDOM_NIGHTLY_SEEDS="0x20260910 0x20260911" \
    src/test/python/Frontend/scripts/run_random_regression_suite.sh nightly

Environment:
  TB_FRONTEND_SIM                DUT package (default: verilator).
  TB_SEED                        Smoke seed (default: 0x20260909).
  TB_TRANSLATION_RANDOM_COUNT    Translation ordinals per seed (default: 6).
  TB_RANDOM_NIGHTLY_SEEDS        Space-separated nightly seeds
                                 (default: "0x20260909 0x2026090a 0x2026090b").
  TB_PYTEST_DISABLE_RERUNFAILURES  Set to 0 only when reruns are intentional.

Activate /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate before running.
Logs and artifacts are produced by run_pytest_with_log.sh. Nightly continues
after a failed seed and returns nonzero if any seed fails.
EOF
}

mode="${1:-smoke}"
if [[ "${mode}" == "-h" || "${mode}" == "--help" ]]; then
  usage
  exit 0
fi
if [[ "${mode}" != "smoke" && "${mode}" != "nightly" ]]; then
  echo "[frontend-random-suite][error] mode must be smoke or nightly: ${mode}" >&2
  usage >&2
  exit 2
fi

translation_case="${FRONTEND_DIR}/tests/py/zhaoxinran/translation/test_translation_random_regression_dut.py"
translation_normal_case="${FRONTEND_DIR}/tests/py/zhaoxinran/translation/test_address_translation_normal.py"
translation_timing_case="${FRONTEND_DIR}/tests/py/zhaoxinran/translation/test_address_translation_ptw_timing.py"
translation_fault_case="${FRONTEND_DIR}/tests/py/zhaoxinran/translation/test_address_translation_fault.py"
translation_gstage_case="${FRONTEND_DIR}/tests/py/zhaoxinran/translation/test_address_translation_gstage_provenance.py"
translation_permission_case="${FRONTEND_DIR}/tests/py/zhaoxinran/translation/test_instruction_fetch_permission_boundary.py"
translation_context_case="${FRONTEND_DIR}/tests/py/zhaoxinran/translation/test_address_translation_context_switch.py"
translation_pte_case="${FRONTEND_DIR}/tests/py/zhaoxinran/translation/test_address_translation_pte_permission.py"
branch_case="${FRONTEND_DIR}/tests/py/zhaoxinran/test_multi_branch.py"
mmio_case="${FRONTEND_DIR}/tests/py/zhaoxinran/mmio/test_mmio_fetch_boundary.py"
mmio_flow_case="${FRONTEND_DIR}/tests/py/zhaoxinran/mmio/test_mmio_fetch_flow_control.py"
mmio_flush_case="${FRONTEND_DIR}/tests/py/zhaoxinran/mmio/test_mmio_fetch_flush.py"
mmio_handoff_case="${FRONTEND_DIR}/tests/py/zhaoxinran/mmio/test_mmio_fetch_response_handoff.py"
mmio_page_state_case="${FRONTEND_DIR}/tests/py/zhaoxinran/mmio/test_mmio_fetch_page_state.py"
mmio_translation_fault_case="${FRONTEND_DIR}/tests/py/zhaoxinran/mmio/test_mmio_fetch_translation_faults.py"
mmio_attributes_case="${FRONTEND_DIR}/tests/py/zhaoxinran/mmio/test_mmio_fetch_attributes.py"
mmio_control_flow_case="${FRONTEND_DIR}/tests/py/zhaoxinran/mmio/test_mmio_fetch_control_flow.py"
mmio_state_edges_case="${FRONTEND_DIR}/tests/py/zhaoxinran/mmio/test_mmio_fetch_state_edges.py"
exception_paths_case="${FRONTEND_DIR}/tests/py/zhaoxinran/test_frontend_exception_paths.py"
rfr_case="${FRONTEND_DIR}/tests/py/zhaoxinran/test_redirect_flush_recovery_scenarios.py"
nc_case="${FRONTEND_DIR}/tests/py/zhaoxinran/uncache/test_nc_fetch_paths.py"
uncache_boundary_case="${FRONTEND_DIR}/tests/py/zhaoxinran/uncache/test_instr_uncache_port_boundaries.py"

smoke_filter='translation_constrained_random_stream_dut or multi_branch_random_positions or multi_cfi_per_ftq_entry or multi_branch_dense_loop or large_loop_multi_segment or mmio_rvi_non_tail_8b_offsets_deliver_instruction or mmio_rvc_at_8b_tail_advances_by_2b_without_second_beat or mmio_cross_8b_clean_rvi_requests_next_beat_and_delivers or mmio_cross_8b_first_beat_fault_reports_without_resend or mmio_cross_8b_second_beat_response_modes or nc_tl_a_backpressure_holds_payload_until_fire or nc_d_response_fault_reports_exception or nc_8b_tail_delivery_uses_correct_second_beat_policy or nc_cfi_instruction_is_delivered_with_control_flow_type or nc_mixed_beat_types_preserve_delivery_order_and_pc_progress or nc_page_tail_delivery_uses_correct_next_page_policy'
nightly_filter="${smoke_filter} or address_translation_normal or ptw_timing_by_translation_stage or address_translation_fault or cacheable_cross_page_second_page_translation_fault or address_translation_gstage_provenance or address_translation_pte_permission or instruction_fetch_permission_boundary or pmp_lock_mode or satp_asid_switch or tlb_csr_change or satp_switch_records_late or all_stage_context_change or sfence_scope_after_refill or sfence_stage_after_refill or unmatched_sfence_during_ptw_wait or mmio_wait_last_commit or mmio_empty_release or mmio_backend_can_accept or mmio_send_req_ibuffer_stall or mmio_flush_cancels or mmio_d_response_coincides or mmio_response_uses_reserved or mmio_backend_redirect_wins or mmio_page_tail_rvi_preserves_half or mmio_cross_page_second_page_translation_fault or mmio_page_tail_first_page_pmp or mmio_cross_page_second_page_pmp or pbmt_nc_non_mmio_enters_uncache or pmp_mmio_with_pbmt_nc or mmio_jalr_return or mmio_send_req_a_fire or mmio_send_req_with_ibuffer_ready or cacheable_cross64_refill_fault_delivery or cacheable_illegal_rvc or cacheable_execute_pc_trigger or uncache_a_ready_backpressure or uncache_response_fault_reports or uncache_wfi_blocks or uncache_wfi_during_a_ready or uncache_pending_response_flushed or uncache_flushed_fault_response or uncache_consecutive_redirects or uncache_redirect_to_mmio_while_icache_response_pending or uncache_mmio_and_icache_pending_redirects or sv39_same_page_sfence_retranslates_changed_attribute or sv39_redirect_transitions_to_changed_attribute or uncache_cacheable_non_mmio_uses_icache_path or uncache_sv39_revisit_uses_existing_translation_refill or uncache_sv39_sector_lane or uncache_translation_sequence_refills_after_sfence or uncache_sv39_all_stage_response_fault_priority or uncache_sv39_execute_denied or uncache_sv39_pmp_execute_denied or uncache_sv39_cross_page_rvi_uses_second_page_pma_path"
nightly_filter+=" or mmio_adjacent_rvc_rvi or mmio_single_beat_d_response_fault or mmio_branch_instruction or mmio_jal_instruction or mmio_tl_a_backpressure or mmio_redirect_drops_a_ready or mmio_redirect_cancels_wait_last_commit or mmio_page_tail_rvi_rechecks or mmio_page_tail_rvc_delivers or mmio_page_tail_first_beat_fault"
nightly_filter+=" or nc_page_tail_denied_response or nc_cross_page_second_page_translation_fault_has_exact_pc or nc_pmp_execute_denied or nc_pending_backend_can_accept or nc_response_and_redirect_same_cycle"
nightly_filter+=" or uncache_csr_changed_before_ptw_response or uncache_resend_first_beat or uncache_non_crossing_rvi_offsets or uncache_resend_second_beat_fault or uncache_page_tail_fault_does_not_create_half or uncache_cross_page_half_is_flushed or uncache_page_tail_rvc_does_not_fetch or uncache_mmio_commit_order or uncache_wfi_during_mmio_commit_gate"
nightly_filter+=" or uncache_pbmt_nc_non_mmio or uncache_pbmt_io_waits_commit or uncache_pbmt_nc_after_ibuffer_backpressure or uncache_pbmt_nc_mmio_pma_second_fetch or uncache_pbmt_nc_real_bin"
nightly_filter+=" or uncache_pbmt_nc_pending_redirect_to_cacheable or uncache_cacheable_pending_redirect_to_pbmt_nc or uncache_page_tail_rvi_need_resend"
nightly_filter+=" or jal_forward_jump_observes_target_pc or jal_resolve_drains_pending_queue"
nightly_filter+=" or rfr_s02 or rfr_s03 or rfr_s04 or rfr_s07"

if [[ "${mode}" == "smoke" ]]; then
  seeds=("${TB_SEED:-0x20260909}")
  pytest_filter="${smoke_filter}"
else
  read -r -a seeds <<< "${TB_RANDOM_NIGHTLY_SEEDS:-0x20260909 0x2026090a 0x2026090b}"
  pytest_filter="${nightly_filter}"
fi

translation_count="${TB_TRANSLATION_RANDOM_COUNT:-6}"
failed=0
for seed in "${seeds[@]}"; do
  echo "[frontend-random-suite] mode=${mode} seed=${seed} translation_count=${translation_count}"
  if ! TB_FRONTEND_SIM="${TB_FRONTEND_SIM:-verilator}" \
    TB_ENABLE_DUT_TESTS=1 \
    TB_SEED="${seed}" \
    TB_TRANSLATION_RANDOM_COUNT="${translation_count}" \
    "${SCRIPT_DIR}/run_pytest_with_log.sh" \
    "${translation_case}" "${translation_normal_case}" "${translation_timing_case}" \
    "${translation_fault_case}" "${translation_gstage_case}" "${translation_permission_case}" \
    "${translation_context_case}" "${translation_pte_case}" \
    "${branch_case}" "${mmio_case}" "${mmio_flow_case}" "${mmio_flush_case}" \
    "${mmio_handoff_case}" "${mmio_page_state_case}" "${mmio_translation_fault_case}" \
    "${mmio_attributes_case}" "${mmio_control_flow_case}" "${mmio_state_edges_case}" \
    "${exception_paths_case}" "${rfr_case}" \
    "${nc_case}" "${uncache_boundary_case}" \
    -k "${pytest_filter}"; then
    failed=1
    echo "[frontend-random-suite][error] failed seed=${seed}" >&2
  fi
done

if [[ "${failed}" -ne 0 ]]; then
  echo "[frontend-random-suite][error] regression failed" >&2
  exit 1
fi
echo "[frontend-random-suite] regression passed for ${#seeds[@]} seed(s)"
