#!/usr/bin/env python3

from __future__ import annotations

import re
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
REPO_ROOT = MEMBLOCK_ROOT.parents[1]


class MemBlockEnvironmentContractTest(unittest.TestCase):
    def test_hypervisor_load_store_contract_is_registered(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()

        for contract in (
            "hlvb = 0x10",
            "hlvxwu = 0x1e",
            "hsvb = 0x10",
            "hsvd = 0x13",
            "constexpr unsigned scalar_store_bytes(StoreOp op)",
            "static_cast<unsigned>(op) & 3U",
            "op == StoreOp::cbo_zero",
            "reference_hlvx_permitted",
            "set_hypervisor_access_permissions",
            "hypervisor-contracts",
            "hlvx-pmp-execute-denied",
            "translation_mode_pairs=",
            "mode_family_cases=",
            "pbmt_combinations=",
            "pbmt_family_cases=",
            "misaligned_family_cases=",
            "spvp=1 vsum=1 vmxr=1 hlvx=1 hsv=1 pmp_x=1",
        ):
            self.assertIn(contract, environment + driver + makefile)

    def test_pointer_masking_contract_is_registered(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()

        for contract in (
            "enum class PointerMaskingMode",
            "reference_pointer_mask",
            "set_pointer_masking",
            "pointer-masking-contracts",
            "m-bare-pmlen7",
            "sv48-pmlen16-high",
            "nested-vu-senvcfg",
            "hlv-u-hstatus",
            "hlvx-exempt",
            "allowed_additional_exception_mask",
            "mxr_exempt=1",
            "scalar_load=1 scalar_store=1 fp_load=1",
            "vector_load=1 vector_store=1 atomic=1 cbo_zero=1",
            "hlv=1 hlvx_exempt=1 hsv=1 mxr_exempt=1",
        ):
            self.assertIn(contract, environment + driver + makefile)

    def test_uncache_outstanding_contract_is_registered(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()

        for contract in (
            "set_uncache_write_outstanding",
            "force_next_uncache_response_delay",
            "uncache_max_outstanding_requests",
            "run_until_uncache_drained",
            "uncache-outstanding",
            "disabled_load_max=",
            "enabled_load_max=",
            "disabled_store_max=",
            "enabled_store_max=",
            "delayed_first_response=4096",
        ):
            self.assertIn(contract, environment + driver + makefile)

    def test_direct_sbuffer_flush_contract_is_registered(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()

        for contract in (
            "pulse_sbuffer_flush",
            "io_ooo_to_mem_flushSb",
            "sbuffer-flush",
            "phase=pre-flush-state",
            "phase=flush-duration",
            "combined_empty=1 cacheable_readback=1 nc_readback=1",
        ):
            self.assertIn(contract, environment + driver + makefile)

    def test_sbuffer_timeout_contract_is_registered(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()

        for contract in (
            "set_sbuffer_timeout",
            "io_ooo_to_mem_csrCtrl_sbuffer_timeout",
            "SBuffer timeout exceeds the 22-bit CSR field",
            "sbuffer-timeout",
            "phase=high-timeout-hold",
            "phase=low-timeout-evict",
            "high_readback=1 low_readback=1",
        ):
            self.assertIn(contract, environment + driver + makefile)

    def test_mbmc_bitmap_contract_is_registered(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()

        for contract in (
            "reference_bitmap_word_address",
            "reference_bitmap_deny_mask",
            "set_mbmc",
            "pulse_mbmc_bitmap_clear",
            "ptw_requests_covering_since",
            "mbmc-contracts",
            "bme_gate=1 cmode_gate=1 bitmap_allow=1",
            "cache_hit=1 bclear_refetch=1",
            "load_access_fault=1 store_access_fault=1",
        ):
            self.assertIn(contract, environment + driver + makefile)

    def test_pmp_contract_matches_platform_grain(self) -> None:
        parameters = (REPO_ROOT / "src/main/scala/xiangshan/PMParameters.scala").read_text()
        pmp = (REPO_ROOT / "src/main/scala/xiangshan/backend/fu/PMP.scala").read_text()
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        plan = (MEMBLOCK_ROOT / "docs/VERIFICATION_PLAN.md").read_text()

        self.assertIn("PlatformGrain: Int = log2Ceil(4*1024)", parameters)
        self.assertIn("if (CoarserGrain) { cfgVec(i).a :=", pmp)
        for contract in (
            "configure_pmp",
            "pmp-contracts",
            "pmp_na4_read_write",
            "pmp_locked_napot_deny",
            "machine-unlocked-bypass",
            "machine-locked-rewrite-rejected",
            "atomic_denied=1",
        ):
            self.assertIn(contract, environment + driver + makefile)
        self.assertIn("NA4 unselectable", plan)

    def test_queue_capacity_constants_match_xiangshan_parameters(self) -> None:
        parameters = (
            REPO_ROOT / "src/main/scala/xiangshan/Parameters.scala"
        ).read_text()
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()

        for scala_name, cpp_name in (
            ("VirtualLoadQueueSize", "kVirtualLoadQueueEntries"),
            ("StoreQueueSize", "kStoreQueueEntries"),
            ("RobSize", "kRobEntries"),
        ):
            scala_match = re.search(rf"{scala_name}: Int = (\d+)", parameters)
            cpp_match = re.search(rf"{cpp_name} = (\d+)", environment)
            self.assertIsNotNone(scala_match, scala_name)
            self.assertIsNotNone(cpp_match, cpp_name)
            self.assertEqual(int(cpp_match.group(1)), int(scala_match.group(1)))

        config = (
            MEMBLOCK_ROOT / "config/memblock.json"
        ).read_text()
        configured_store_entries = re.search(
            r'"queue\.store_entries"\s*:\s*(\d+)', config
        )
        scala_store_entries = re.search(r"StoreQueueSize: Int = (\d+)", parameters)
        self.assertIsNotNone(configured_store_entries)
        self.assertIsNotNone(scala_store_entries)
        self.assertEqual(
            int(configured_store_entries.group(1)),
            int(scala_store_entries.group(1)),
        )

    def test_vector_fu_type_constants_match_scala_one_hot_order(self) -> None:
        fu_type = (
            REPO_ROOT / "src/main/scala/xiangshan/backend/fu/FuType.scala"
        ).read_text()
        names = re.findall(r'val\s+(\w+)\s*=\s*addType\(name\s*=\s*"[^"]+"\)', fu_type)
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()

        for scala_name, cpp_name in (
            ("vldu", "kFuTypeVectorLoad"),
            ("vstu", "kFuTypeVectorStore"),
        ):
            match = re.search(
                rf"{cpp_name}\s*=\s*std::uint64_t\{{1\}}\s*<<\s*(\d+)",
                environment,
            )
            self.assertIsNotNone(match, cpp_name)
            self.assertEqual(int(match.group(1)), names.index(scala_name))

    def test_software_prefetch_encodings_match_lsu_op_type(self) -> None:
        package = (REPO_ROOT / "src/main/scala/xiangshan/package.scala").read_text()
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()

        for scala_name, cpp_name in (
            ("prefetch_i", "instruction"),
            ("prefetch_r", "read"),
            ("prefetch_w", "write"),
        ):
            scala_match = re.search(rf"def\s+{scala_name}\s*=\s*\"b([01]+)\"", package)
            cpp_match = re.search(rf"{cpp_name}\s*=\s*0x([0-9a-f]+)", environment)
            self.assertIsNotNone(scala_match, scala_name)
            self.assertIsNotNone(cpp_match, cpp_name)
            self.assertEqual(
                int(scala_match.group(1), 2), int(cpp_match.group(1), 16)
            )

    def test_cbo_zero_encoding_and_contract_are_registered(self) -> None:
        package = (REPO_ROOT / "src/main/scala/xiangshan/package.scala").read_text()
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        self.assertRegex(package, r'def\s+cbo_zero\s*=\s*"b0111"\.U')
        self.assertRegex(environment, r'cbo_zero\s*=\s*7')
        for contract in (
            "run_cbo_zero_contracts",
            "run_until_uncache_requests",
            "cbo_zero_line=1",
            "cbo-zero-contracts",
        ):
            self.assertIn(contract, main + environment + makefile)

    def test_l2_tlb_boundary_contract_is_registered(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        for contract in (
            "L2TlbResponse",
            "issue_l2_tlb_request",
            "io_l2_tlb_req_req_valid",
            "io_l2_tlb_req_resp_valid",
            "pulse_l2_hint",
            "io_l2_hint_valid",
            "l2-tlb-contracts",
            "l1_miss_response=1",
        ):
            self.assertIn(contract, environment + main + makefile)

    def test_uncache_store_bus_error_outputs_are_checked(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "struct BusErrorStats",
            "io_dcacheError_ecc_error_valid",
            "io_uncacheError_ecc_error_valid",
            "last_uncache_address",
            "run_store_error",
            "store_denied=1 store_corrupt=1",
            "expected_error_address",
            "errors_after.uncache_reports != errors_before.uncache_reports + 1",
        ):
            self.assertIn(contract, environment + main)

    def test_topdown_outputs_have_semantic_contracts(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        for contract in (
            "struct TopDownStats",
            "top-down L2/L3 miss output violated one-cycle delay",
            "io_topDownInfo_toBackend_replayAllocate",
            "io_topDownInfo_toBackend_sqFull",
            "io_topDownInfo_toBackend_sbFull",
            "io_topDownInfo_toBackend_l1Miss",
            "enqueue_store_pressure",
            "run_topdown_contracts",
            "MEMBLOCK_TOPDOWN_CONTRACTS_PASS",
            "topdown-contracts",
        ):
            self.assertIn(contract, environment + main + makefile + benchmark)

    def test_ifetch_ptw_bridge_covers_concurrent_dtlb_walk(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        memblock = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/MemBlock.scala"
        ).read_text()
        repeater = (
            REPO_ROOT / "src/main/scala/xiangshan/cache/mmu/Repeater.scala"
        ).read_text()
        for contract in (
            "start_ifetch_ptw_request",
            "complete_ifetch_ptw_request",
            "ifetch_ptw_pending_",
            "pending_ifetch_ptw_requests",
            "IFU-DTLB-concurrent",
            "ifu_dtlb_source_overlap=1",
            "ptw_max_outstanding_requests() < 2",
            "IFU-duplicate",
            "duplicate_requests=2 duplicate_walk_requests=3",
            "NestedFaultKind",
            "nested_vs_fault=4 nested_g_leaf_fault=4",
            "nested_implicit_g_fault=4",
            "confirm_ifetch_ptw_flushed",
            "ifu_stage1_context_race=1",
            "ifu_sfence_global_race=1 ifu_sfence_selective_race=1",
            "ifu_nested_context_race=1",
            "ifu_hfence_vvma_race=2 ifu_hfence_gvma_race=2",
            "reference.faulting_guest_physical_address",
            "ifetch-ptw-bridge",
        ):
            self.assertIn(contract, environment + main + makefile)
        self.assertIn("PTWRepeaterNB(passReady = false", memblock)
        self.assertIn("req_in.ready := !sent", repeater)

    def test_scalar_load_feedback_is_observed_on_every_lane(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        generator = (MEMBLOCK_ROOT / "scripts/generate_cpp.py").read_text()
        for contract in (
            "sample_scalar_load_wakeup",
            "sample_scalar_load_cancel",
            "ScalarLoadFeedbackStats",
            "last_wakeup",
            "ld2_cancels",
            "run_load_feedback",
            "unbalanced_early_wakeup",
            "warm_feedback_or_residency_mismatch",
            "page-fault-classification",
            "fault_wakeups != fault_cancels",
            "wait_for_mmio_request",
            "mmio_cancels=",
            "nc_cancels=",
            "uncache_environment.tilelink_requests() != dcache_before",
            "forwarding-pre-data-classification",
            "forwarding_wakeups != forwarding_cancels + 1",
            "forwarding_cancels=",
            "pmp-classification",
            "pmp_wakeups != pmp_cancels",
            "pmp_environment.uncache_requests() != pmp_uncache_before",
            "pmp_cancels=",
            "denied_feedback.first != denied_feedback.second",
            "corrupt_wakeups != corrupt_cancels",
            "feedback_wakeups != feedback_cancels",
            "load-feedback",
        ):
            self.assertIn(contract, environment + main + makefile + generator)

    def test_store_and_vector_slow_feedback_fields_are_checked(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        for contract in (
            "IqSlowFeedbackStats",
            "StoreSlowFeedbackSample",
            "VectorStoreSlowFeedbackSample",
            "issue_vector_batch_same_cycle",
            "run_iq_slow_feedback",
            "sta-same-cycle-miss",
            "sta-same-cycle-hit",
            "vstu-same-cycle-hit",
            "sample.replay_mask != 0",
            "iq-slow-feedback",
        ):
            self.assertIn(contract, environment + main + makefile)

    def test_memory_violation_is_sampled_and_checked(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        generator = (MEMBLOCK_ROOT / "scripts/generate_cpp.py").read_text()
        for contract in (
            "sample_memory_violation",
            "MemoryViolationStats",
            "run_memory_violation",
            "run_rar_violation",
            "configure_ldld_violation_check",
            "run_until_sbuffer_empty",
            "bank-conflict-classification",
            "bank_conflict_cancels < 2",
            "bank_conflict_wakeups != bank_conflict_cancels",
            "speculative-load",
            "redirect-check",
            "non_overlap=1",
            "candidates=",
            "rob_wrap=1",
            "issue_store_address_batch",
            "require_same_cycle",
            "vector_candidate=1",
            "concurrent_sources=2",
            "concurrent_older.size()",
            "concurrent_rob=",
            "concurrent_loads.back()",
            "rob_offset = 159 + index",
            "rob_pointer_value(rob_offset)",
            "rob_pointer_flag(rob_offset)",
            "memory-violation",
            "rar-violation",
        ):
            self.assertIn(contract, environment + main + makefile + generator)

    def test_ifetch_prefetch_is_sampled_and_discriminated(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        generator = (MEMBLOCK_ROOT / "scripts/generate_cpp.py").read_text()
        for contract in (
            "sample_ifetch_prefetch",
            "IfetchPrefetchStats",
            "run_ifetch_prefetch",
            "instruction-check",
            "data-prefetch-check",
            "enqueue_prefetch_batch",
            "issue_prefetch_batch_same_cycle",
            "concurrent-translation-bypass",
            "concurrent.ptw_requests() != concurrent_ptw_before",
            "concurrent_lanes=3 translation_bypass=3",
            "run_mixed_prefetches",
            "mixed_unmapped=3 mixed_mapped=3 mapped_individual=2",
            "mapped_warmup_ptw=",
            "mapped_data_dcache=",
            "phase=mixed-mapped-individual-data",
            "phase=memory-type-warmup-check",
            "memory_type_uncache_before != 4",
            "dcache_before + (index < 2 ? 1 : 0)",
            "memory_type_prefetches=4",
            "nc_prefetch_dcache=2 nc_prefetch_uncache=0",
            "io_prefetch_dcache=0 io_prefetch_uncache=0",
            "ifetch_prefetches=",
            "ifetch-prefetch",
        ):
            self.assertIn(contract, environment + main + makefile + generator)

    def test_hardware_prefetch_outputs_have_a_stride_oracle(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        generator = (MEMBLOCK_ROOT / "scripts/generate_cpp.py").read_text()
        for contract in (
            "sample_hardware_prefetch_outputs",
            "HardwarePrefetchStats",
            "configure_stride_prefetch",
            "l1D_pf_active_threshold.ImmSet",
            "std::uint64_t{12}",
            "l1D_pf_active_stride.ImmSet",
            "std::uint64_t{30}",
            "expect_l2_prefetch_control",
            "io_outer_l2PfCtrl_l2_pf_delay_latency",
            "run_hardware_prefetch",
            "stride_source = 12",
            "l2_depth = stride << 5",
            "stream_source = 11",
            "stream_l2_depth_lines = 640",
            "stream_l2_width_lines = 4",
            "stream-stride-priority",
            "stride_suppressed=1 l2_control_defaults=1",
            "hardware-prefetch",
        ):
            self.assertIn(contract, environment + main + makefile + generator)

    def test_exception_priority_uses_rob_age_not_queue_order(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "select_store_exception_address",
            "load_priority",
            "store_priority",
            "rob_pointer_value(160)",
            "selector_cross=1",
            "load_oldest=0x",
            "store_oldest=0x",
            "same_rob_uop_priority=2",
            "younger_uop.vuop_idx = 1",
            "vector_element_address(younger_uop, 0)",
            "different_cause_faults",
            "cross_cause_priority=2",
            "scalar_vector_priority=2",
            "phase=older-vector-replacement",
            "phase=older-scalar-replacement",
        ):
            self.assertIn(contract, environment + main)

    def test_vector_address_oracle_covers_ordinary_multi_uop_modes(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "static_cast<std::uint64_t>(transaction.vuop_idx) * 16U",
            "const std::int64_t elements_per_uop = 16 / element_bytes",
            "transaction.stride *",
            "const unsigned global_element = element_base + element",
            "transaction.mask_bits >> global_element",
            "multi_uop_loads",
            "second_indices",
            "const auto &first_issue = mode == 2 ? older : younger",
            "multi_uop_modes=3 multi_uop_writebacks=6",
        ):
            self.assertIn(contract, environment + main)

    def test_dcache_coherence_tracks_concurrent_probe_sources(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        for contract in (
            "expected.base == response.address",
            "probe_request_count_ - probe_response_count_",
            "probe_sources_seen_",
            "dcache_max_probe_outstanding",
            "phase=overlap-probe-accept",
            "overlap.writebacks() != writebacks_before",
            "overlap_probe_sources=",
            "overlap_probe_depth=",
            "dcache-coherence",
        ):
            self.assertIn(contract, environment + main + makefile)

    def test_frontend_bridge_has_semantic_transaction_coverage(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        for contract in (
            "exercise_frontend_bridges",
            "auto_inner_frontendBridge_icache_out_a_bits_user_reqSource",
            "auto_inner_frontendBridge_icache_out_a_bits_mask",
            "auto_inner_frontendBridge_instr_uncache_in_d_bits_corrupt",
            "auto_inner_frontendBridge_icachectrl_in_d_bits_denied",
            "icache_input - icache_completed_requests < 16",
            "instr_input == instr_responses",
            "ctrl_input - ctrl_responses < 32",
            "source_credit_stalls",
            "frontend bridge run missed a required stall class",
            "MEMBLOCK_FRONTEND_BRIDGE_PASS",
            "frontend-bridge",
        ):
            self.assertIn(contract, environment + main + makefile)

    def test_mixed_environment_has_combined_drain_and_queue_accounting(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()

        for contract in (
            "run_until_all_complete",
            "run_until_queues_retired",
            "account_lq_cancellation",
            "commit_vector_store",
            "expect_prefetch",
        ):
            self.assertIn(contract, environment)

        for gate in (
            "simultaneous_scalar_vector",
            "scalar_to_vector",
            "vector_to_scalar",
            "dirty_pressure",
            "redirect_recovery",
            "prefetch_ops",
            "backpressure_complete",
        ):
            self.assertIn(gate, main)

        for contract in (
            "dcache_request_stalls",
            "dcache_response_delays",
            "ptw_request_stalls",
            "ptw_response_delays",
            "uncache_request_stalls",
            "uncache_response_delays",
        ):
            self.assertIn(contract, environment)

    def test_atomic_contract_has_old_value_visibility_and_reservation_checks(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        for contract in (
            "enum class AtomicOp",
            "bool issue_atomic",
            "kFuTypeAtomic",
            "amoadd_d",
            "amoadd_w",
            "amocas_w",
            "amocas_d",
            "lr_d",
            "lr_w",
            "sc_d",
            "sc_w",
        ):
            self.assertIn(contract, environment)
        for contract in (
            "run_atomic_contracts",
            "amo_d_variants=",
            "amo_w_variants=",
            "amocas_variants=4",
            "lr_sc=1",
            "misaligned_d_offsets=7",
            "misaligned_w_offsets=3",
            "misaligned=10",
            "kExceptionStoreAddressMisaligned",
            "atomic-contracts",
            "run_atomic_dchannel_errors",
            "atomic-dchannel-errors",
            "std::array<AtomicErrorCase, 22>",
            '" denied_cases="',
            '" corrupt_cases="',
            '" readbacks="',
            '" denied_line_hits="',
            '" corrupt_line_hits="',
            '" sc_denied_hit_checks="',
            '" sc_corrupt_hit_checks="',
            '" clean_recoveries="',
            '"-clean-recovery-atomic reason="',
            '"-clean-recovery-readback reason="',
            "SC cannot issue a D-channel request on a miss",
            "Both denied and corrupt refills are installed as poisoned lines",
        ):
            self.assertIn(contract, main + makefile)
        self.assertIn("bool check_data_on_exception = false", environment)
        self.assertIn("it->second.check_data_on_exception", environment)

    def test_scoreboards_reject_duplicate_identity_and_store_halves(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()

        for diagnostic in (
            "duplicate outstanding scalar load ROB value",
            "duplicate outstanding scalar store ROB value",
            "duplicate outstanding vector memory uop",
            "duplicate store-address writeback",
            "duplicate store-data writeback",
        ):
            self.assertIn(diagnostic, environment)

    def test_store_writeback_is_gated_by_issue_handshake_epoch(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        for contract in (
            "address_issued",
            "data_issued",
            "address_issue_cycle",
            "data_issue_cycle",
            "sample_cycle < it->second.address_issue_cycle",
            "sample_cycle < it->second.data_issue_cycle",
            "mark_address_issued",
            "mark_data_issued",
        ):
            self.assertIn(contract, environment)

    def test_store_and_vector_scoreboards_check_metadata_sidebands(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "expected_debug_is_mmio",
            "expected_debug_is_ncio",
            "writeback.flush_pipe != it->second.flush_pipe",
            "mismatched store-address writeback",
            "std::optional<std::uint8_t> trigger",
            "writeback.vec_wen != expected.vec_wen",
            "writeback.v0_wen != expected.v0_wen",
            "writeback.vl_wen != expected.vl_wen",
            "mismatched vector memory metadata",
        ):
            self.assertIn(contract, environment)
        for contract in (
            "wait_for_mmio_store_request",
            "commit_stores_through(mmio_store, 1)",
            "store_mmio_valid",
            "stores=1",
        ):
            self.assertIn(contract, main + environment)

    def test_scalar_load_optional_metadata_is_checked_only_when_constrained(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        load_scoreboard = environment.split("class LoadScoreboard", 1)[1].split(
            "class StoreScoreboard", 1
        )[0]
        self.assertIn("std::optional<bool> debug_is_mmio", load_scoreboard)
        self.assertIn("optional_mismatch(", load_scoreboard)
        self.assertNotIn("expected_debug_is_ncio.value_or(false)", load_scoreboard)
        self.assertNotIn("expected_debug_is_mmio.value_or(false)", load_scoreboard)

    def test_translation_faults_cover_stage_one_and_gstage_pte_encodings(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "reference_pte_encoding_fault",
            "pte_reserved",
            "pbmt == 3U",
            "reference_pte_address_at_level",
            "set_page_based_memory_types",
        ):
            self.assertIn(contract, environment)
        for contract in (
            "std::array<PteEncodingCase, 26>",
            "leaf-pbmt-disabled",
            "l0-nonleaf",
            "nonleaf-u",
            "nonleaf-pbmt",
            "leaf-napot-encoding",
            "stage1_pte_encoding_cases=",
            "gstage_pte_encoding_cases=",
            "kExceptionLoadGuestPageFault",
            "nested_reference.is_for_vs_nonleaf_pte",
        ):
            self.assertIn(contract, main)

        for contract in (
            "kExceptionStorePageFault",
            "kExceptionStoreGuestPageFault",
            "stage1_store_pte_encoding_cases=",
            "gstage_store_pte_encoding_cases=",
            "faulting store reached memory or unbalanced SQ",
        ):
            self.assertIn(contract, main)

    def test_mixed_commit_boundary_does_not_auto_commit_next_rob(self) -> None:
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        self.assertIn("rob_offset - 1", main)
        self.assertIn("Keep the commit boundary at the last uop", main)

    def test_vector_store_commit_uses_its_enqueue_time_sq_target(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        self.assertIn(
            "vector_store_sq_targets_[vector_store_key(transaction)]", environment
        )
        self.assertIn("sq_allocated_ - sq_canceled_", environment)
        self.assertIn("const std::uint64_t target = target_it->second", environment)
        self.assertIn("if (sq_dequeued_ < target", environment)

    def test_reference_memory_is_separate_from_bus_backing_memory(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        for contract in (
            "SparseMemory bus_memory_;",
            "SparseMemory memory_;",
            "memory_(&bus_memory_)",
            "memory_agent_(bus_memory_, memory_)",
            "ptw_agent_(bus_memory_)",
            "uncache_agent_(bus_memory_)",
            "memory_.write_reference_byte(",
            "reference_memory_.read_byte(",
            "bus_expected_load",
        ):
            self.assertIn(contract, environment)

    def test_uncache_store_order_uses_bus_backing_oracle(self) -> None:
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        self.assertIn("int run_store_rdata_order", main)
        self.assertIn("environment.bus_expected_load(older.address", main)
        self.assertIn("environment.bus_expected_load(younger.address", main)

    def test_reset_recovery_reasserts_reset_and_is_a_registered_target(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        self.assertIn("dut_.reset.ImmSet(std::uint64_t{1});", environment)
        self.assertIn("int run_reset_recovery", main)
        self.assertIn('options.test == "reset-recovery"', main)
        self.assertIn("reset-recovery", makefile)
        self.assertIn("account_lq_cancellation", main)
        self.assertEqual(environment.count("void reset_link_state()"), 3)
        for contract in (
            "memory_agent_.reset_link_state();",
            "ptw_agent_.reset_link_state();",
            "uncache_agent_.reset_link_state();",
            '" outstanding_resets=3"',
            '" dcache=1 ptw=1 uncache=1"',
        ):
            self.assertIn(contract, environment + main)

    def test_wfi_safety_drains_each_memory_manager_before_safe(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        for contract in (
            "force_next_dcache_response_delay",
            "force_next_ptw_response_delay",
            "force_next_uncache_response_delay",
            "require_wfi_unsafe",
            "run_until_wfi_safe",
            "wfi-safety",
            "idle_safe=1 dcache_safe=1 ptw_safe=1 uncache_safe=1",
            "response_delay=",
            "unsafe_window=",
        ):
            self.assertIn(contract, environment + main + makefile + benchmark)

    def test_mmio_contract_has_pbmt_io_mapping_and_three_cycle_boundary_test(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        load_unit = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala"
        ).read_text()
        pma = (
            REPO_ROOT / "src/main/scala/xiangshan/backend/fu/PMA.scala"
        ).read_text()
        new_csr = (
            REPO_ROOT / "src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala"
        ).read_text()
        for contract in (
            "pte_pbmt_io",
            "pulse_pending_load",
            "wait_for_mmio_store_request",
            "set_debug_mode",
            "io_ooo_to_mem_tlbCsr_priv_debug",
            "expected_debug_is_mmio",
            "expected_debug_is_ncio",
        ):
            self.assertIn(contract, environment)
        for contract in (
            "s2_mmio_req.valid := RegNextN(io.lsq.uncache.fire, 2",
            "val s3_mmio_req     = RegNext(s2_mmio_req)",
        ):
            self.assertIn(contract, load_unit)
        for contract in (
            "int run_mmio_contracts",
            "dcache-bypass",
            "expected_debug_is_mmio = true",
            "commit_stores_through(mmio_store, 1)",
            "pma_physical_base = 0x35000000ULL",
            "pma_denied_count",
            "phase=pma-debug-denied",
            "debug_physical_base = 0x38020000ULL",
            "phase=pma-debug-load",
            "phase=pma-debug-store",
            "pma_debug_loads=",
            "pma_debug_stores=",
            "mmio-contracts",
        ):
            self.assertIn(contract, main + makefile)
        self.assertIn("io.tlb.debug := debugMode", new_csr)
        self.assertIn(
            "Mux(addr >= debugStart.U && addr <= debugEnd.U, debug, true.B)",
            pma,
        )

    def test_fp_loads_cover_data_paths_and_exception_suppression(self) -> None:
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        fp_loads = main[
            main.index("int run_fp_loads") : main.index("int run_trigger_contracts")
        ]
        load_unit = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala"
        ).read_text()
        for contract in (
            "mmio_fp=5 mmio_flh=1 mmio_flw=2 mmio_fld=2",
            '" mmio_faults="',
            '" page_faults=1"',
            '" pmp_faults=1 permission_faults=1 guest_faults=1"',
            '" misaligned_cacheable=3 misaligned_nc_faults=1"',
            "kExceptionLoadAccessFault",
            "kExceptionHardwareError",
            "kExceptionLoadPageFault",
            "kExceptionLoadGuestPageFault",
            "kExceptionLoadAddressMisaligned",
            "configure_pmp",
            "exception_gpaddr",
            "exception_is_for_vs_nonleaf_pte",
            "target.tilelink_requests() == dcache_before",
            "target.uncache_requests() == uncache_before",
            "mmio_dcache_requests=",
        ):
            self.assertIn(contract, fp_loads)
        self.assertIn(
            "io.ldout.bits.uop.fpWen := s3_fpWen && "
            "!io.ldout.bits.uop.exceptionVec.asUInt.orR",
            load_unit,
        )

    def test_mixed_stimulus_drives_every_lsq_dispatch_lane_in_one_cycle(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        generated = (MEMBLOCK_ROOT / "cpp/generated_port_defaults.hpp").read_text()

        self.assertIn("kLsqEnqueueLanes = 6", generated)
        self.assertIn("enqueue_load_batch", environment)
        self.assertIn("generated::drive_lsq_enqueue(dut_, lane, enqueue)", environment)
        self.assertIn("width <= memblock::generated::kLsqEnqueueLanes", main)
        self.assertIn("dispatch_widths=", main)
        self.assertIn("dispatch_lanes=", main)

    def test_mixed_vector_aliasing_uses_address_oracle_and_nonoverlap_stores(self) -> None:
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "vector_element_address(store, element)",
            "forwarded[address + byte]",
            "std::shuffle(offset_slots.begin(), offset_slots.end(), random)",
            "vec_load_stride=",
            "vec_store_stride=",
            "constexpr std::array<std::int64_t, 3> load_strides{{-4, 0, 4}}",
        ):
            self.assertIn(contract, main)

    def test_vector_oracle_allows_only_spec_permitted_agnostic_data(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        for contract in (
            "matches_load_data",
            "tail_agnostic",
            "mask_agnostic",
            "!preserved && !all_ones",
        ):
            self.assertIn(contract, environment)

    def test_vector_fault_only_first_contract_is_registered(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        vfof = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VfofBuffer.scala"
        ).read_text()
        for contract in (
            "kVectorLoadFaultOnlyFirst",
            "issue.is_vleff = transaction.is_vleff",
            "issue.last_uop = transaction.last_uop",
            "issue.vl_wen = transaction.vl_wen",
            "mismatched vector FOF fix-VL writeback",
            "vector_fof_fix_writebacks",
            "run_vector_fault_only_first",
            "original_vl=2 final_vl=1",
            "vector-fof",
        ):
            self.assertIn(contract, environment + main + makefile + benchmark)
        self.assertIn("entries.hasException", vfof)
        self.assertIn("io.uopWriteback.bits.data             := entries.vl", vfof)

    def test_vector_segment_contract_is_registered(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        segment = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VSegmentUnit.scala"
        ).read_text()
        for contract in (
            "kFuTypeVectorSegmentLoad",
            "kFuTypeVectorSegmentStore",
            "std::unordered_multimap<RobIdentity, Expected",
            "run_vector_segment",
            "segment_load_writebacks=2",
            "segment_store_writebacks=2",
            "const std::uint64_t field_offset = transaction.segment",
            "VectorAddressingMode::indexed_ordered",
            "addressed_modes=3",
            "addressed_load_writebacks",
            "addressed_store_writebacks",
            "segment_lsq_allocations=0",
            "vector-segment",
        ):
            self.assertIn(contract, environment + main + makefile + benchmark)
        self.assertIn("class VSegmentUnit", segment)
        self.assertIn("io.uopwriteback.valid", segment)

    def test_vector_segment_fof_contract_is_registered(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        segment = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VSegmentUnit.scala"
        ).read_text()
        for contract in (
            "run_vector_segment_fault_only_first",
            "MEMBLOCK_VECTOR_SEGMENT_FOF_PASS",
            "fields=2 original_vl=2 final_vl=1",
            "fix_vl_writebacks=",
            "vector-segment-fof",
            ".fault_only_first = true",
            ".is_vleff = false",
            "first_element_faults=2",
            "first_fault_fix_vl=2",
            "first_fault.exception_vaddr() != first_fault_virtual",
        ):
            self.assertIn(contract, environment + main + makefile + benchmark)
        self.assertIn("fofBufferValid", segment)
        self.assertIn("instMicroOp.exceptionVl.bits := segmentIdx", segment)

    def test_translation_plan_matches_xiangshan_mode_contract(self) -> None:
        parameters = (REPO_ROOT / "src/main/scala/xiangshan/Parameters.scala").read_text()
        mmu_constants = (REPO_ROOT / "src/main/scala/xiangshan/cache/mmu/MMUConst.scala").read_text()
        plan = (MEMBLOCK_ROOT / "docs/VERIFICATION_PLAN.md").read_text()

        self.assertIn("HasHExtension: Boolean = true", parameters)
        self.assertIn("EnableSv48: Boolean = true", parameters)
        for mode in ("def Sv39 =", "def Sv48 =", "def Sv39x4 =", "def Sv48x4 ="):
            self.assertIn(mode, mmu_constants)
        for pair in (
            "Sv39 -> Sv39x4",
            "Sv39 -> Sv48x4",
            "Sv48 -> Sv39x4",
            "Sv48 -> Sv48x4",
        ):
            self.assertIn(pair, plan)
        for contract in (
            "Translation Specification Scope",
            "Translation Closure Phases",
            "16-KiB root",
            "50-bit GPA",
            "SFENCE.VMA",
            "HFENCE.GVMA",
        ):
            self.assertIn(contract, plan)

        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        for contract in (
            "reference_page_walk",
            "reference_sv48_walk",
            "map_sv48_4k",
            "map_sv48x4_4k",
            "activate_two_stage_modes",
            "issue_sfence",
            "translation-matrix",
            "translation-fence",
            "translation-fence-selective",
            "translation-fence-sv48",
            "translation-fence-sv48-selective",
            "translation-fence-sv39-sv48x4",
            "translation-fence-sv39-sv48x4-selective",
            "translation-fence-sv48-sv39x4",
            "translation-fence-sv48-sv39x4-selective",
            "translation-fence-all",
            "translation-inflight-context-all",
            "translation-context",
            "translation-bare",
            "translation-faults",
            "translation-permissions",
            "translation-pbmt",
            "translation-superpages",
            "hfence-vvma",
            "fp-loads",
        ):
            self.assertIn(contract, environment + main + makefile)

        scala_ptw_modes = {
            name: int(bits, 2)
            for name, bits in re.findall(
                r'def\s+(noS2xlate|onlyStage1|onlyStage2|allStage)\s*=\s*"b([01]+)"',
                mmu_constants,
            )
        }
        cpp_ptw_modes = {
            name: int(value)
            for name, value in re.findall(
                r"(no_stage_two|only_stage_one|only_stage_two|all_stages)\s*=\s*(\d+)",
                environment,
            )
        }
        self.assertEqual(
            cpp_ptw_modes,
            {
                "no_stage_two": scala_ptw_modes["noS2xlate"],
                "only_stage_one": scala_ptw_modes["onlyStage1"],
                "only_stage_two": scala_ptw_modes["onlyStage2"],
                "all_stages": scala_ptw_modes["allStage"],
            },
        )
        for contract in (
            "map_sv39_2m",
            "map_sv39_1g",
            "map_sv48_2m",
            "map_sv48_1g",
            "map_sv48_512g",
            "map_sv39x4_2m",
            "map_sv39x4_1g",
            "map_sv48x4_2m",
            "map_sv48x4_1g",
            "map_sv48x4_512g",
            "ReferencePageMode::bare",
            "activate_bare",
        ):
            self.assertIn(contract, environment)
        self.assertIn("readonly-store-execution", main)
        self.assertIn("kExceptionStorePageFault", main)
        self.assertIn("account_sq_cancellation(1)", main)

    def test_translation_fence_covers_same_id_root_reuse(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for helper in (
            "update_stage_one_context",
            "update_vs_context",
            "update_g_context",
        ):
            self.assertIn(helper, environment)
            self.assertIn(helper, main)
        for phase in (
            "same-asid-refill",
            "same-vs-asid-refill",
            "same-vmid-refill",
            "same_id_reuses=3",
        ):
            self.assertIn(phase, main)

    def test_translation_fence_covers_outstanding_ptw_response(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "issue_sfence_with_redirect",
            "ResponseLatencyProfile::spec",
            "target_pte, ptw_before_walk, 4096, 12",
            "target_pte, first_ptw_request, 8192, 12",
            '"-old-pte-request reason="',
            '"-refill reason="',
            "run_outstanding_sfence(outstanding_selective)",
            "run_outstanding_hfence(true, outstanding_selective)",
            "run_outstanding_hfence(false, outstanding_selective)",
            "run_until_ptw_request_covering",
            "request_covering_address_has_min_delay_since",
            "run_fully_nested_hfence(true, outstanding_selective)",
            "run_fully_nested_hfence(false, outstanding_selective)",
            "outstanding_scope=",
            "outstanding_vs_mode=",
            "outstanding_g_mode=",
            "outstanding_stage1=",
            "outstanding_vs=",
            "outstanding_g=",
            "outstanding_nested_vs=",
            "outstanding_nested_g=",
        ):
            self.assertIn(contract, environment + main)

    def test_translation_fence_covers_inflight_context_switches(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "force_next_ptw_response_delay",
            "update_two_stage_context",
            "run_inflight_context_switch(false, false)",
            "run_inflight_context_switch(true, false)",
            "run_inflight_context_switch(false, true)",
            "run_inflight_context_switch(true, true)",
            "run_inflight_virtualization_switch(true)",
            "run_inflight_virtualization_switch(false)",
            '"-old-request reason="',
            '"-switch reason="',
            '"-new-request reason="',
            "inflight_context_cases=",
            "inflight_mode_cases=",
            "inflight_v_cases=",
        ):
            self.assertIn(contract, environment + main)

    def test_translation_faults_cover_both_canonicality_directions(self) -> None:
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "sv39-high",
            "sv48-high",
            "sv39-upper-zero-sign-one",
            "sv39-upper-one-sign-zero",
            "sv48-upper-zero-sign-one",
            "sv48-upper-one-sign-zero",
            "canonical_boundary_cases=",
        ):
            self.assertIn(contract, main)

    def test_translation_permission_matrix_uses_top_level_csr_controls(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "ReferencePtePermissions",
            "ReferencePbmt",
            "reference_load_permitted",
            "reference_store_permitted",
            "reference_two_stage_pbmt",
            "set_translation_permissions",
            "set_page_based_memory_types",
            "io_ooo_to_mem_tlbCsr_priv_dmode",
            "io_ooo_to_mem_tlbCsr_priv_mxr",
            "io_ooo_to_mem_tlbCsr_priv_sum",
            "io_ooo_to_mem_tlbCsr_priv_vmxr",
            "io_ooo_to_mem_tlbCsr_priv_vsum",
        ):
            self.assertIn(contract, environment)
        for contract in (
            "std::array<StageOneLoadCase, 14>",
            "std::array<StageOneStoreCase, 10>",
            "std::array<TwoStageLoadCase, 8>",
            "std::array<TwoStageStoreCase, 22>",
            "sv39-s-user-sum0",
            "sv48-xonly-mxr1",
            "sv39-store-dirty0",
            "sv48-store-accessed0",
            "vs-xonly-vmxr1",
            "g-accessed0",
            "sv39-sv48x4-store-valid",
            "sv48-sv39x4-store-valid",
            "sv39x4-store-g-dirty0",
            "sv48x4-store-g-dirty0",
            "sv39x4-store-g-user0",
            "sv48x4-store-g-user0",
            "stage1_load_cases=",
            "stage1_store_cases=",
            "two_stage_load_cases=",
            "two_stage_store_cases=",
            "MEMBLOCK_TRANSLATION_PBMT_PASS",
            "final_pma=",
            "final_nc=",
            "final_io=",
        ):
            self.assertIn(contract, main)

    def test_translation_context_covers_host_guest_and_virtuality_switches(self) -> None:
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "phase=satp-asid",
            "phase=vsatp-asid",
            "phase=hgatp-vmid",
            "phase=virt-transition",
            "activate_sv39(root_a, 17)",
            "activate_two_stage(vs_root_b, g_root, 22, 31)",
            "activate_two_stage(vs_root, g_root_b, 25, 36)",
            '" contexts=5"',
            '" accesses="',
        ):
            self.assertIn(contract, main)

    def test_stress_driver_requires_real_burst_overlap_and_combo_gates(self) -> None:
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "int run_random_stress",
            "struct StressRandom",
            "next_shape",
            "next_payload",
            "next_schedule",
            "stress_max_outstanding",
            "stress_combinations",
            "issue-dependency-deadlock",
            "run_until_all_complete(8192)",
            "random-stress",
        ):
            self.assertIn(contract, driver)

    def test_stress_vector_forwarding_excludes_repeated_address_stores(self) -> None:
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        self.assertIn("const std::array<std::int64_t, 5> store_strides", driver)
        self.assertIn("const std::array<std::int64_t, 6> load_strides", driver)
        self.assertIn("if (transaction.store)", driver)

    def test_stress_scalar_forwarding_counter_tracks_dependent_load(self) -> None:
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        independent = driver.index("case Kind::scalar_load:")
        dependent = driver.index("case Kind::extra_load:")
        self.assertNotIn(
            "++coverage.scalar_forwarding", driver[independent:dependent]
        )
        self.assertIn(
            "++coverage.scalar_forwarding", driver[dependent:dependent + 500]
        )

    def test_stress_combinations_are_derived_from_generated_features(self) -> None:
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for feature in (
            "masked_vector",
            "unmasked_vector",
            "strided_vector",
            "indexed_vector",
            "scalar_misaligned",
            "scalar_forwarding",
        ):
            self.assertIn(feature, driver)
        self.assertIn("group_count == 2 && masked_vector", driver)
        self.assertIn("scalar_misaligned && strided_vector", driver)
        self.assertIn("indexed_vector && scalar_forwarding", driver)

    def test_random_mixed_has_one_configurable_constraint_interface(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        runner = (MEMBLOCK_ROOT / "scripts/run_regression.py").read_text()

        for contract in (
            "struct RandomConstraints",
            'name == "coverage"',
            'name == "spec"',
            'name == "corner"',
            'argument == "--constraints"',
            'argument == "--constraint"',
            "operation_weights",
            "locality_weights",
            "atomic_family_weights",
            "atomic_width_weights",
            "hypervisor_family_weights",
            "translation_weights",
            "stage1_mode_weights",
            "vs_mode_weights",
            "g_mode_weights",
            "fence_kind_weights",
            "fence_scope_weights",
            "concurrent_actions_per_mille",
            "special_concurrent_per_mille",
            "translation_switches_per_mille",
            "tlb_flushes_per_mille",
            "misaligned_per_mille",
            "vector_corner_per_mille",
            "vector_segment_stores_per_mille",
            "probes_per_mille",
            "probe_to_b_per_mille",
            "probe_need_data_per_mille",
            "nc_stores_per_mille",
            "mmio_stores_per_mille",
            "stride_stream_per_mille",
            "ConstraintCoverage",
            "target_ops=",
            "actual_ops=",
            "target_hypervisor_family=",
            "actual_hypervisor_family=",
            "actual_vector_segment_direction=",
            "actual_vector_segment_eew=",
            "actual_vector_segment_nf=",
            "actual_atomic_family=",
            "actual_atomic_width=",
            "actual_nc_direction=",
            "actual_mmio_direction=",
            "actual_special_concurrent=",
            "actual_translation=",
            "actual_stage1_mode=",
            "actual_vs_mode=",
            "actual_g_mode=",
            "actual_nested_pairs=",
            "actual_fences=",
            "actual_translation_switch=",
            "actual_translation_walk_reuse=",
            "target_probe=",
            "target_probe_to_b=",
            "target_probe_need_data=",
            "target_stride_stream=",
            "l2_stride_prefetches=",
            "backend_load_feedback_frozen",
            "raw_load_wakeups=",
            "raw_load_cancels=",
            "actual_probe_sequences=",
            "actual_probe_caps=",
            "actual_probe_need_data=",
            '"hypervisor-hlv"',
            '"hypervisor-hlvx"',
            '"hypervisor-hsv"',
            "run_cycles(constrained_completion_timeout / 2)",
        ):
            self.assertIn(contract, driver)
        for key in (
            "scalar-load",
            "scalar-store",
            "vector-load",
            "vector-store",
            "vector-segment",
            "prefetch",
            "atomic",
            "nc",
            "mmio",
            "stride-stream",
            "atomic-amo",
            "atomic-lrsc",
            "atomic-cas",
            "atomic-w",
            "atomic-d",
            "locality-hot",
            "locality-warm",
            "locality-cold",
            "concurrent",
            "special-concurrent",
            "translation-bare",
            "translation-stage1",
            "translation-nested",
            "stage1-sv39",
            "stage1-sv48",
            "vs-sv39",
            "vs-sv48",
            "g-sv39x4",
            "g-sv48x4",
            "translation-switch",
            "fence-sfence",
            "fence-hfence-vvma",
            "fence-hfence-gvma",
            "fence-global",
            "fence-selective",
            "tlb-flush",
            "misaligned",
            "vector-corner",
            "vector-segment-store",
            "probe",
            "probe-to-b",
            "probe-need-data",
            "nc-store",
            "mmio-store",
            "latency",
            "dcache-latency",
            "ptw-latency",
            "uncache-latency",
        ):
            self.assertIn('"' + key + '"', driver)

        for contract in (
            "enum class ResponseLatencyProfile",
            "struct ResponseLatencyProfiles",
            "struct ResponseLatencyStats",
            "percentile < 7410",
            "percentile < 8853",
            "percentile < 9359",
            "100 + static_cast<unsigned>",
            "run_until_store_complete_with_replay",
            "run_until_vector_complete_with_replays",
            "record_atomic_result",
            "const std::vector<VectorMemoryTransaction> &transactions",
        ):
            self.assertIn(contract, environment)
        self.assertIn(
            "environment.record_atomic_result(", driver
        )
        self.assertIn("struct TranslationContext", driver)
        self.assertIn("reference_two_stage_walk", driver)
        self.assertIn("environment.set_page_based_memory_types(true, true)", driver)
        self.assertIn("stage1_weight + nested_weight", driver)
        self.assertIn("translation_coverage_closed", driver)
        self.assertIn("CONSTRAINT_ARGS", makefile)
        self.assertIn("LONG_CONSTRAINT_ARGS", makefile)
        self.assertIn("constraint_profile", runner)
        self.assertIn("constraint_overrides", runner)

    def test_random_mixed_preserves_atomic_serialization_contract(self) -> None:
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        memblock = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/MemBlock.scala"
        ).read_text()
        concurrent_tail = driver[
            driver.index('phase = "seeded-mixed-tail"'):
            driver.index("const std::array<memblock::AtomicOp, 9>")
        ]

        self.assertIn("atomics insts (LR/SC/AMO) will block the pipeline", memblock)
        self.assertIn("uses_concurrent_special_operations", concurrent_tail)
        self.assertIn("RandomConstraints::noncacheable + index", concurrent_tail)
        self.assertNotIn("issue_atomic", concurrent_tail)

    def test_vector_cross_16_misalignment_advances_rob_head(self) -> None:
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        self.assertIn("requires_misaligned_head", driver)
        self.assertIn("((address & 0xfU) + element_bytes) > 16U", driver)
        self.assertIn("requires_store_pending", driver)
        self.assertIn("(address & (element_bytes - 1)) != 0", driver)
        self.assertIn("requires_store_pending))", driver)
        self.assertIn(".address = base + (index == 3 ? 0x1800 : 0x1803)", driver)
        self.assertIn("loads[index].index[8] = index == 4 ? 0xa0 : 0xa8", driver)

    def test_random_mixed_window_honors_scalar_misalignment_contract(self) -> None:
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        for contract in (
            "constrained_window_address",
            "constraints.misaligned_per_mille",
            "scalar_store_crosses_page",
            "(scalar_store.address & 0xfffU) + scalar_store_bytes > 0x1000U",
            "environment.set_rob_head(\n                     scalar_store.rob",
            "scalar_store, constrained_completion_timeout,\n                    scalar_store_crosses_page",
        ):
            self.assertIn(contract, driver)

        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        for contract in (
            "bool hold_pending_store = false",
            "const auto clear_pending_store",
            "io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1})",
        ):
            self.assertIn(contract, environment)

        for contract in (
            "phase=translated-scalar-store",
            "run_until_store_tlb_misses",
            "translated_store, 8192, true",
            "translated_scalar_cross_page=1",
        ):
            self.assertIn(contract, driver)

    def test_scalar_misaligned_keeps_rar_pressure_behind_pending_splits(
        self,
    ) -> None:
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        scenario = driver[
            driver.index("int run_scalar_misaligned"):
            driver.index("int run_misaligned_stores")
        ]
        for contract in (
            "pressure_load_count = 60",
            "pending_split",
            "pressure.enqueue_load(transaction)",
            "pressure.issue_load_batch(batch, 256, true)",
            "pressure.set_rob_head(transaction.rob, transaction.rob_flag)",
            "pressure.memory_violation_stats().count != pressure_violations_before",
            "pressure.writebacks() != pressure_load_count + pending_split.size()",
            '" rar_pressure_loads="',
        ):
            self.assertIn(contract, scenario)

    def test_random_mixed_drops_directed_release_snapshots_before_random_tail(
        self,
    ) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        directed_end = driver.index('phase = "seeded-mixed-tail"')
        dirty_pressure = driver.index('phase = "dcache-dirty-pressure"')
        directed_region = driver[dirty_pressure:directed_end]

        self.assertIn("void clear_release_line_expectations()", environment)
        self.assertIn(
            "environment.clear_release_line_expectations();", directed_region
        )

    def test_random_mmio_store_replays_until_tlb_hit(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        driver = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        self.assertIn("bool issue_store_address_until_tlb_hit(", environment)
        self.assertIn("store_tlb_feedbacks_ == feedbacks_before", environment)
        self.assertIn("store_tlb_misses_ == misses_before", environment)
        self.assertIn(
            "environment.issue_store_address_until_tlb_hit(", driver
        )

    def test_make_targets_forward_make_variable_seed_and_transaction_counts(self) -> None:
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        self.assertIn("--seed $(or $(SEED),1)", makefile)
        self.assertIn("--transactions $(or $(TRANSACTIONS),16384)", makefile)
        self.assertIn(
            "--transactions $(or $(STRESS_TRANSACTIONS),16384)", makefile
        )
        stress_rule = makefile[
            makefile.index("stress-regression:"):makefile.index("verify-stress-results:")
        ]
        self.assertIn(
            "--transactions $(or $(STRESS_TRANSACTIONS),16384)", stress_rule
        )
        self.assertIn(
            "--mixed-transactions $(or $(STRESS_TRANSACTIONS),16384)", stress_rule
        )
        self.assertIn("--rtl-metadata $(FROZEN_RTL_METADATA)", makefile)
        self.assertNotIn("$${SEED:-1}", makefile)
        self.assertNotIn("$${TRANSACTIONS:-", makefile)

        final_acceptance = makefile[
            makefile.index("final-acceptance:"):makefile.index("unit:")
        ]
        self.assertIn("$(MAKE) benchmark-tests", final_acceptance)
        self.assertIn("$(MAKE) endurance-regression", final_acceptance)
        self.assertIn("$(MAKE) verify-endurance-results", final_acceptance)


if __name__ == "__main__":
    unittest.main()
