#!/usr/bin/env python3

from __future__ import annotations

import re
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
REPO_ROOT = MEMBLOCK_ROOT.parents[1]


class MemBlockEnvironmentContractTest(unittest.TestCase):
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
            "denied_amo=1 corrupt_amo=1 corrupt_hit_amo=1",
            "check_data_on_exception = true",
        ):
            self.assertIn(contract, main + makefile)
        self.assertIn("bool check_data_on_exception = false", environment)
        self.assertIn("it->second.check_data_on_exception", environment)

    def test_scoreboards_reject_duplicate_identity_and_store_halves(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()

        for diagnostic in (
            "duplicate outstanding scalar load ROB value",
            "duplicate outstanding scalar store ROB value",
            "duplicate outstanding vector memory ROB value",
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

    def test_mixed_commit_boundary_does_not_auto_commit_next_rob(self) -> None:
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        self.assertIn("rob_offset - 1", main)
        self.assertIn("Keep the commit boundary at the last uop", main)

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

    def test_mmio_contract_has_pbmt_io_mapping_and_three_cycle_boundary_test(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        load_unit = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala"
        ).read_text()
        for contract in (
            "pte_pbmt_io",
            "pulse_pending_load",
            "wait_for_mmio_store_request",
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
            "mmio-contracts",
        ):
            self.assertIn(contract, main + makefile)

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
            "translation-context",
            "translation-bare",
            "translation-faults",
            "translation-permissions",
            "translation-superpages",
            "hfence-vvma",
            "fp-loads",
        ):
            self.assertIn(contract, environment + main + makefile)
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

    def test_make_targets_forward_make_variable_seed_and_transaction_counts(self) -> None:
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        self.assertIn("--seed $(or $(SEED),1)", makefile)
        self.assertIn("--transactions $(or $(TRANSACTIONS),16384)", makefile)
        self.assertNotIn("$${SEED:-1}", makefile)
        self.assertNotIn("$${TRANSACTIONS:-", makefile)


if __name__ == "__main__":
    unittest.main()
