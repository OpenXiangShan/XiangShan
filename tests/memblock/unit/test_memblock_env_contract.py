#!/usr/bin/env python3

from __future__ import annotations

import re
import sys
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
REPO_ROOT = MEMBLOCK_ROOT.parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))
import scenario_catalog


def read_makefile() -> str:
    return (MEMBLOCK_ROOT / "Makefile").read_text() + "\n" + (
        MEMBLOCK_ROOT / "cpp/scenarios/generated_targets.mk"
    ).read_text()


def read_cpp_source(name: str) -> str:
    """Read a C++ source with local implementation fragments expanded."""
    root = MEMBLOCK_ROOT / "cpp"
    if name == "memblock_env.hpp":
        # Inspect implementations through their real compilation boundary.
        paths = [root / name, root / "environment/implementation.hpp"]
        paths += sorted((root / "environment").glob("*.hpp"))
        paths += sorted((root / "environment").glob("*.inc"))
        paths += sorted((root / "environment").glob("*.cpp"))
    elif name == "memblock_main.cpp":
        paths = [root / name, root / "scenarios/shared.hpp", root / "scenarios/catalog.json"]
        paths += sorted((root / "random").glob("*.inc"))
        paths += sorted((root / "scenarios").glob("*.inc"))
        paths += sorted((root / "scenarios").glob("mixed_*.cpp"))
        paths += sorted((root / "scenarios/mixed").glob("*.hpp"))
    else:
        paths = [root / name]
    return "\n".join(path.read_text() for path in paths)


def read_cpp_definition(path: str, qualified_name: str) -> str:
    """Select one production definition without rearranging or rewriting it."""
    source = read_cpp_source(path)
    start = source.index(qualified_name + "(")
    opening = source.index("{", start)
    depth = 0
    # Ignore braces inside comments and quoted text while locating the end.
    tokens = re.finditer(r'//[^\n]*|/\*[\s\S]*?\*/|"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'|[{}]', source[opening:])
    for token in tokens:
        if token.group() == "{":
            depth += 1
        elif token.group() == "}":
            depth -= 1
            if depth == 0:
                return source[start:opening + token.end()]
    raise AssertionError(f"unterminated C++ definition: {path}: {qualified_name}")



class MemBlockEnvironmentContractTest(unittest.TestCase):
    def test_page_mapping_reuses_walk_structure_instead_of_xor_keys(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")

        for contract in (
            "ReferencePageMode::sv39, false, 0",
            "ReferencePageMode::sv48, false, 0",
            "ReferencePageMode::sv39, true, 0",
            "ReferencePageMode::sv48, true, 0",
        ):
            self.assertIn(contract, environment)
        for obsolete_cache in (
            "sv39_l1_tables_",
            "sv39_l0_tables_",
            "sv48_l2_tables_",
            "sv48_l1_tables_",
            "sv48_l0_tables_",
            "gstage_l1_tables_",
            "gstage_l0_tables_",
            "gstage_sv48_l2_tables_",
            "gstage_sv48_l1_tables_",
            "gstage_sv48_l0_tables_",
        ):
            self.assertNotIn(obsolete_cache, environment)

    def test_single_load_covers_refill_order_merge_and_partial_progress(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        miss_queue = (
            REPO_ROOT
            / "src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala"
        ).read_text()

        for contract in (
            "scalar_immediate_is_valid",
            "scalar_issue_base_address",
            "immediate_loads=",
            "immediate_min=-2048 immediate_max=2047",
            "keyword_refill_count_",
            "nonkeyword_refill_count_",
            "dcache_keyword_refills()",
            "dcache_nonkeyword_refills()",
            "phase=refill-order",
            "nonkeyword_refills=",
            "keyword_refills=",
            "issue_load_batch(merged_loads, 128, true)",
            "phase=same-line-merge",
            "merged_loads=",
            "merge_refills=",
            "force_next_dcache_interbeat_delay(128)",
            "dcache_grant_data_beats()",
            "phase=partial-refill-early",
            "phase=partial-refill-drain",
            "partial_writeback_cycle=",
        ):
            self.assertIn(contract, environment + driver)
        self.assertIn(
            "miss_req_pipe_reg_bits.vaddr(5).asBool", miss_queue
        )
        self.assertIn("refill_count ^ isKeyword", miss_queue)

    def test_hypervisor_load_store_contract_is_registered(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        tlb = (
            REPO_ROOT / "src/main/scala/xiangshan/cache/mmu/TLB.scala"
        ).read_text()

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
            "physical_pmp_cases=",
            "locked_physical_pmp_cases=",
            "physical_pma_cases=",
            "physical_pma_boundary_cases=",
            "ReferencePrivilegeMode::machine",
            "pmp_napot_read_execute",
            "pmp_locked_napot_read",
            "pmp_locked_napot_read_write_execute",
            "required_pmp_permission = hlvx ? 0x5U : 0x1U",
            "pma_device_physical = 0x35000000ULL",
            "pma-device-hlvx",
            "pma_device_boundary_page = 0x7ffff000ULL",
            "pma_ddr_boundary_page = 0x80000000ULL",
            "pma-boundary-device-hlvx",
            "pma-boundary-ddr-hlvx",
            "spvp=1 vsum=1 vmxr=1 hlvx=1 hsv=1 pmp_x=1",
            "machine_spvp_pmp=1 locked_spvp_pmp=1",
        ):
            self.assertIn(contract, environment + driver + makefile)
        self.assertIn(
            "Mux(req_out(i).isPrefetch, mode_tmp, mode(i))", tlb
        )

    def test_pointer_masking_contract_is_registered(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()

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
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()

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
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()

        for contract in (
            "pulse_sbuffer_flush",
            "io_ooo_to_mem_flushSb",
            "sbuffer-flush",
            "phase=pre-flush-state",
            "phase=flush-duration",
            "combined_empty=1 cacheable_readback=1 nc_readback=1",
        ):
            self.assertIn(contract, environment + driver + makefile)

    def test_l2_flush_control_has_a_continuous_timing_oracle(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        memblock = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/MemBlock.scala"
        ).read_text()

        for contract in (
            "drive_l2_flush",
            "outer_l2_flush_enabled",
            "backend_l2_flush_done",
            "L2 flush enable did not pass through combinationally",
            "L2 flush completion violated one-cycle delay",
            "l2-flush-contracts",
            "combinations=4",
            "enable_transitions=",
            "done_transitions=",
            "delay_checks=",
        ):
            self.assertIn(contract, environment + driver + makefile + benchmark)
        self.assertIn(
            "x.l2FlushDone       := RegNext(io.l2_flush_done)", memblock
        )
        self.assertIn(
            "io.outer_l2_flush_en := io.ooo_to_mem.csrCtrl.flush_l2_enable",
            memblock,
        )

    def test_top_control_bypasses_have_continuous_timing_oracles(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        memblock = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/MemBlock.scala"
        ).read_text()

        for contract in (
            "drive_top_controls",
            "backend hart ID did not pass through combinationally",
            "power-down enable did not pass through combinationally",
            "reset vector violated one-cycle delay",
            "CPU halt violated one-cycle delay",
            "CPU critical error violated one-cycle delay",
            "drive_top_bridge_stimulus",
            "MSI acknowledgement did not pass through combinationally",
            "frontend reset bypass did not pass through combinationally",
            "I-cache BEU metadata violated one-cycle delay",
            "MSI information violated valid-gated one-cycle delay",
            "CLINT time violated valid-gated one-cycle delay",
            "external interrupts violated one-cycle mapping",
            "hardware-counter perf event violated one-cycle delay",
            "L2 prefetch control violated two-cycle delay",
            "top-control-contracts",
            "boolean_combinations=8",
            "hart_patterns=",
            "reset_vector_patterns=",
            "bridge_patterns=256",
            "interrupt_combinations=256",
            "l2_prefetch_combinations=32",
            "perf_event_shared_lanes=",
        ):
            self.assertIn(contract, environment + driver + makefile + benchmark)
        for contract in (
            "x.hartId            := io.hartId",
            "io.inner_reset_vector := RegNext(io.outer_reset_vector)",
            "io.outer_cpu_halt := RegNext",
            "io.outer_power_down_en := io.ooo_to_mem.csrCtrl.power_down_enable",
            "io.outer_cpu_critical_error := RegNext",
            "x.msiInfo           := DelayNWithValid",
            "x.clintTime         := DelayNWithValid",
            "x.externalInterrupt.msip  := RegNext",
            "x.externalInterrupt.nmi.nmi_31 := RegNext",
            "io.outer_msi_ack := io.ooo_to_mem.backendToTopBypass.msiAck",
            "io.outer_beu_errors_icache := RegNext",
            "io.inner_hc_perfEvents <> RegNext",
            "io.outer_l2PfCtrl := DelayN",
            "io.resetInFrontendBypass.toL2Top := io.resetInFrontendBypass.fromFrontend",
        ):
            self.assertIn(contract, memblock)

    def test_trace_bridge_has_valid_gated_timing_oracles(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        memblock = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/MemBlock.scala"
        ).read_text()

        for contract in (
            "drive_trace_bridge_stimulus",
            "trace encoder feedback violated one-cycle delay",
            "trace mstatus violated one-cycle delay",
            "trace privilege violated group-0 valid hold contract",
            "trace trap metadata violated trap-only hold contract",
            "trace group unconditional fields violated one-cycle delay",
            "trace group payload violated valid hold contract",
            "trace-bridge-contracts",
            "valid_combinations=8",
            "itypes=16",
            "ftq_offsets=16",
            "encoder_combinations=4",
        ):
            self.assertIn(contract, environment + driver + makefile + benchmark)
        for contract in (
            "traceFromBackend.fromEncoder := RegNext",
            "traceToL2Top.toEncoder.trap  := RegEnable",
            "traceToL2Top.toEncoder.priv := RegEnable",
            "traceToL2Top.toEncoder.mstatus := RegNext",
            "groups(i).valid := RegNext",
            "bits.iretire := RegNext",
            "bits.itype := RegNext",
            "bits.ilastsize := RegEnable",
            "bits.iaddr := RegEnable",
            "<< instOffsetBits",
        ):
            self.assertIn(contract, memblock)

    def test_dft_bridge_exhausts_combinational_input_space(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        memblock = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/MemBlock.scala"
        ).read_text()

        for contract in (
            "check_dft_bridge_space",
            "pattern < 1024",
            "DFT bridge mismatch pattern=0x",
            "dft-bridge-contracts",
            "inputs=10",
            "frontend_outputs=10",
            "backend_outputs=4",
        ):
            self.assertIn(contract, environment + driver + makefile + benchmark)
        for contract in (
            "sig.ram_hold := dft.ram_hold",
            "sig.ram_bypass := dft.ram_bypass",
            "sig.cgen := dft.cgen",
            "io.dft_frnt.zip(sigFromSrams)",
            "io.dft_reset_frnt.zip(io.dft_reset)",
            "io.dft_bcknd.zip(sigFromSrams)",
            "io.dft_reset_bcknd.zip(io.dft_reset)",
        ):
            self.assertIn(contract, memblock)

    def test_frontend_bridge_reset_recovery_covers_requests_and_response(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        for contract in (
            "check_frontend_bridge_reset_recovery",
            '"pre-reset-request"',
            '"request-reset"',
            '"response-reset"',
            '"post-reset-survivor"',
            '"name": "frontend-reset-recovery"',
            "frontend-reset-recovery",
            '" paths=3 canceled_requests="',
            '" canceled_responses="',
            '" survivor_requests="',
        ):
            self.assertIn(contract, environment + driver + makefile + benchmark)

    def test_sbuffer_timeout_contract_is_registered(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()

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
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()

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
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
        environment = read_cpp_source("memblock_env.hpp")

        for scala_name, cpp_name in (
            ("VirtualLoadQueueSize", "kVirtualLoadQueueEntries"),
            ("StoreQueueSize", "kStoreQueueEntries"),
            ("RobSize", "kRobEntries"),
            ("RabSize", "kRabEntries"),
            ("VTypeBufferSize", "kVTypeBufferEntries"),
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

        configured_probe_entries = re.search(
            r'"queue\.dcache_probe_entries"\s*:\s*(\d+)', config
        )
        configured_miss_entries = re.search(
            r'"queue\.dcache_miss_entries"\s*:\s*(\d+)', config
        )
        scala_probe_entries = re.search(r"nProbeEntries\s*=\s*(\d+)", parameters)
        scala_miss_entries = re.search(r"nMissEntries\s*=\s*(\d+)", parameters)
        cpp_probe_entries = re.search(r"kDcacheProbeEntries = (\d+)", environment)
        cpp_miss_entries = re.search(r"kDcacheMissEntries = (\d+)", environment)
        configured_probe_source_bits = re.search(
            r'"tilelink\.dcache_probe_source_bits"\s*:\s*(\d+)', config
        )
        cpp_probe_source_bits = re.search(
            r"kDcacheProbeSourceBits = (\d+)", environment
        )
        self.assertIsNotNone(configured_probe_entries)
        self.assertIsNotNone(configured_miss_entries)
        self.assertIsNotNone(scala_probe_entries)
        self.assertIsNotNone(scala_miss_entries)
        self.assertIsNotNone(cpp_probe_entries)
        self.assertIsNotNone(cpp_miss_entries)
        self.assertIsNotNone(configured_probe_source_bits)
        self.assertIsNotNone(cpp_probe_source_bits)
        self.assertEqual(
            int(configured_probe_entries.group(1)),
            int(scala_probe_entries.group(1)),
        )
        self.assertEqual(
            int(cpp_probe_entries.group(1)),
            int(scala_probe_entries.group(1)),
        )
        self.assertEqual(
            int(configured_miss_entries.group(1)),
            int(scala_miss_entries.group(1)),
        )
        self.assertEqual(
            int(cpp_miss_entries.group(1)),
            int(scala_miss_entries.group(1)),
        )
        self.assertEqual(
            int(configured_probe_source_bits.group(1)),
            int(cpp_probe_source_bits.group(1)),
        )

    def test_vector_fu_type_constants_match_scala_one_hot_order(self) -> None:
        fu_type = (
            REPO_ROOT / "src/main/scala/xiangshan/backend/fu/FuType.scala"
        ).read_text()
        names = re.findall(r'val\s+(\w+)\s*=\s*addType\(name\s*=\s*"[^"]+"\)', fu_type)
        environment = read_cpp_source("memblock_env.hpp")

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
        environment = read_cpp_source("memblock_env.hpp")

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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        self.assertRegex(package, r'def\s+cbo_zero\s*=\s*"b0111"\.U')
        self.assertRegex(environment, r'cbo_zero\s*=\s*7')
        for contract in (
            "run_cbo_zero_contracts",
            "run_until_uncache_requests",
            "cbo_zero_line=1",
            "cbo-zero-contracts",
        ):
            self.assertIn(contract, main + environment + makefile)

    def test_cmo_encodings_manager_model_and_oracles_are_registered(self) -> None:
        package = (REPO_ROOT / "src/main/scala/xiangshan/package.scala").read_text()
        tilelink = (
            REPO_ROOT / "rocket-chip/src/main/scala/tilelink/Bundles.scala"
        ).read_text()
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        for name, bits, opcode in (
            ("clean", "1100", 12),
            ("flush", "1101", 13),
            ("inval", "1110", 14),
        ):
            self.assertRegex(package, rf'def\s+cbo_{name}\s*=\s*"b{bits}"\.U')
            self.assertRegex(environment, rf'cbo_{name}\s*=\s*0x0?{opcode:x}')
        for name, opcode in (
            ("CBOClean", 12),
            ("CBOFlush", 13),
            ("CBOInval", 14),
            ("CBOAck", 8),
        ):
            self.assertRegex(tilelink, rf'def\s+{name}\s*=\s*{opcode}\.U')
        for contract in (
            "kDcacheCmoSource = kDcacheMissEntries + 1",
            "wait_for_cmo_store_request",
            "force_next_cmo_response_delay",
            "inject_dcache_cmo_response_error_at",
            "request.opcode >= 12 && request.opcode <= 14",
            "malformed DCache CMO request identity",
            "CBOClean -> CBOAck",
            "response.opcode == 8",
            "expected_output_flush_pipe",
            "run_cmo_contracts",
            "probe-before-ack",
            "line_state_cases=",
            "dirty_probe_data=",
            "automatic_sbuffer_drains=",
            "retained_hits=",
            "invalidation_refills=",
            "flushed_younger_loads=",
            "concurrent_cycles=",
            "denied_cases=",
            "corrupt_cases=",
            "cmo-contracts",
        ):
            self.assertIn(contract, environment + main + makefile + benchmark)
        cmo_scenario = main.split("int run_cmo_contracts", 1)[1].split(
            "int run_wfi_safety", 1
        )[0]
        self.assertNotIn("pulse_sbuffer_flush", cmo_scenario)
        self.assertIn("environment.sbuffer_empty()", cmo_scenario)

    def test_l2_tlb_boundary_contract_is_registered(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        for contract in (
            "L2TlbResponse",
            "issue_l2_tlb_request",
            "io_l2_tlb_req_req_valid",
            "io_l2_tlb_req_resp_valid",
            "pulse_l2_hint",
            "io_l2_hint_valid",
            "l2-tlb-contracts",
            "l1_miss_response=1",
            "l1_hit_response=1",
            "phase=l1-hit-response",
            "phase=pmp-denied",
            "RefilledResponseExpectation",
            "pbmt_nc=1 pbmt_io=1 pf_ld=1 gpf_ld=1 af_ld=1",
            "pmp_allow=1 pmp_deny=1 pmp_mmio=1",
            "ptw_a_requests=",
            "leaveHitMux",
        ):
            self.assertIn(contract, environment + main + makefile)

    def test_uncache_store_bus_error_outputs_are_checked(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        for contract in (
            "struct BusErrorStats",
            "io_dcacheError_ecc_error_valid",
            "io_uncacheError_ecc_error_valid",
            "last_uncache_address",
            "run_store_error",
            "store_denied=1 store_corrupt=0",
            "cannot inject corrupt on a data-less Uncache AccessAck",
            "expected_error_address",
            "errors_after.uncache_reports != errors_before.uncache_reports + 1",
        ):
            self.assertIn(contract, environment + main)

    def test_physical_dcache_ecc_contract_is_registered(self) -> None:
        main = read_cpp_source("memblock_main.cpp")
        parameters = (
            REPO_ROOT / "src/main/scala/xiangshan/Parameters.scala"
        ).read_text()
        ctrl_unit = (
            REPO_ROOT / "src/main/scala/xiangshan/cache/dcache/CtrlUnit.scala"
        ).read_text()
        wrapper = (
            REPO_ROOT
            / "src/main/scala/xiangshan/cache/dcache/DCacheWrapper.scala"
        ).read_text()
        data_array = (
            REPO_ROOT
            / "src/main/scala/xiangshan/cache/dcache/data/BankedDataArray.scala"
        ).read_text()
        load_pipe = (
            REPO_ROOT
            / "src/main/scala/xiangshan/cache/dcache/loadpipe/LoadPipe.scala"
        ).read_text()

        for contract in (
            "dcache_ctrl_base = 0x38022000ULL",
            "dcache_ctrl_mask_bank0",
            "tag_error_once = 0x11",
            "data_error_once = 0x19",
            "dcache_banks = 8",
            "data_double_error_masks",
            "data_control_for_bank",
            "concurrent_error_mask = 1ULL << 23",
            "issue_load_batch(concurrent_loads, 512, true)",
            "both_load_lanes_woke",
            "concurrent_ecc_cases",
            "dcache_grants_drained()",
            "run_clean_hit",
            "run_tag_ecc_error",
            "run_data_ecc_error",
            "expected_data_xor",
            "reports != 1",
            "errors_after.last_dcache_address != address",
            "phase=ecc-queue-conservation",
            "physical_ecc_reports != data_ecc_cases + tag_ecc_cases",
            '<< " tag_ecc=" << tag_ecc_cases',
            '<< " data_ecc=" << data_ecc_cases',
        ):
            self.assertIn(contract, main)
        for contract in (
            "val DCacheBanks = 8 // hardcoded",
            "EnableAccurateLoadError: Boolean = false",
            "newCtrlReg.ese := Mux(ctrlRegBundle.persist, ctrlRegBundle.ese, false.B)",
            "bankedDataArray.io.pseudo_error <> ctrlUnit.io_pseudoError(1)",
            "val pseudo_data_toggle_mask = io.pseudo_error.bits.map",
            "raw_data := getDataFromEncWord(data_bank.io.r.data) ^ pseudo_data_toggle_mask",
            "io.error.bits.report_to_beu := (s3_tag_error || s3_data_error) && s3_valid",
        ):
            self.assertIn(
                contract,
                parameters + ctrl_unit + wrapper + data_array + load_pipe,
            )

    def test_topdown_outputs_have_semantic_contracts(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
            "GlobalPtePlacement",
            "s1_global_leaf=3 s1_global_nonleaf=3",
            "gstage_g_leaf=2 gstage_g_nonleaf=2",
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

    def test_ifetch_ptw_bridge_covers_all_sector_indices(self) -> None:
        main = read_cpp_source("memblock_main.cpp")
        for contract in (
            "sector_request_order",
            "sector_valid_masks",
            "response.s1_ppn_low[sibling]",
            "response.s1_valididx == valid_mask",
            "sector_ptw_before + 3",
            "sector_indices=8 sector_ppn_low=8 sector_valid_masks=3",
        ):
            self.assertIn(contract, main)

    def test_scalar_load_feedback_is_observed_on_every_lane(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        generator = (MEMBLOCK_ROOT / "scripts/generate_cpp.py").read_text()
        for contract in (
            "sample_memory_violation",
            "cancel_pending_for_redirect",
            "MemoryViolationStats",
            "run_memory_violation",
            "run_rar_violation",
            "configure_ldld_violation_check",
            "run_until_sbuffer_empty",
            "bank-conflict-terminal",
            "bank_writebacks_before",
            "bank_dequeues_before",
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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
            "immediate_cases=5 immediate_min=-2048 immediate_max=2047",
            "ifetch_prefetches=",
            "ifetch-prefetch",
        ):
            self.assertIn(contract, environment + main + makefile + generator)

    def test_scalar_store_immediates_and_error_responses_preserve_memory(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        for contract in (
            "scalar store immediate exceeds signed 12-bit range",
            "immediate_stores=",
            "corrupt || denied",
            "dcache_last_request_address",
            "cannot inject corrupt on a data-less DCache Grant",
            "cannot inject corrupt on a data-less Uncache AccessAck",
            "if (!denied)",
            "bus_data_before",
        ):
            self.assertIn(contract, environment + main)

    def test_hardware_prefetch_outputs_are_characterized_without_an_algorithm_oracle(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        generator = (MEMBLOCK_ROOT / "scripts/generate_cpp.py").read_text()
        for contract in (
            "sample_hardware_prefetch_outputs",
            "HardwarePrefetchStats",
            "configure_stride_prefetch",
            "l1_d_pf_active_threshold.set",
            "std::uint64_t{12}",
            "l1_d_pf_active_stride.set",
            "std::uint64_t{30}",
            "expect_l2_prefetch_control",
            "io_outer_l2PfCtrl_l2_pf_delay_latency",
            "run_hardware_prefetch",
            "stride_source = 12",
            "stream_source = 11",
            "implementation_characterization=1 disable_controls=1",
            "hardware-prefetch",
        ):
            self.assertIn(contract, environment + main + makefile + generator)
        adapter = read_cpp_source("environment/dut_adapter.cpp")
        self.assertIn(
            "control.prefetch.l1_d_pf_active_threshold = DrivenPort(&impl_->dut."
            "io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold, 4)", adapter
        )
        self.assertIn(
            "control.prefetch.l1_d_pf_active_stride = DrivenPort(&impl_->dut."
            "io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride, 6)", adapter
        )

    def test_exception_priority_uses_rob_age_not_queue_order(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
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
            "run_store_vector_selector",
            "phase=store-vector-retain-first",
            "phase=store-vector-select-second",
            "store_vector_priority=2",
        ):
            self.assertIn(contract, environment + main)

    def test_vector_address_oracle_covers_ordinary_multi_uop_modes(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
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

    def test_vector_unit_stride_lmul_emul_matrix_matches_rtl_contract(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        rename = (
            REPO_ROOT / "src/main/scala/xiangshan/backend/rename/Rename.scala"
        ).read_text()
        split = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VSplit.scala"
        ).read_text()

        configurations = []
        uops = 0
        for eew in range(4):
            for vsew in range(4):
                for lmul_log2 in range(-3, 4):
                    emul_log2 = eew - vsew + lmul_log2
                    if lmul_log2 < vsew - 3 or not -3 <= emul_log2 <= 3:
                        continue
                    configurations.append((eew, vsew, lmul_log2, emul_log2))
                    uops += 1 << max(emul_log2, 0)
        self.assertEqual(len(configurations), 78)
        self.assertEqual(uops, 202)

        for contract in (
            "kVectorUnitStrideMaxFlows = 2",
            "std::optional<std::uint8_t> vsew",
            "vector_vsew(transaction)",
            "writeback.vsew != expected.vsew",
            "writeback.veew != expected.eew",
            "writeback.vlmul != expected.vlmul",
            "issue.vsew = vector_vsew(transaction)",
            "make_vector_memory_issue(vector)",
            "lmul_configurations != 78",
            "lmul_load_uops != 404",
            "lmul_store_uops != 202",
            "lmul_matrix.lq_allocated() != 808",
            "lmul_matrix.sq_allocated() != 404",
            '<< " lmul_configurations="',
        ):
            self.assertIn(contract, environment + main)
        self.assertIn("VecMemUnitStrideMaxFlowNum.U", rename)
        self.assertIn(
            "EewLog2(s0_eew) - s0_sew + s0_lmul", split
        )

    def test_vector_strided_lmul_emul_matrix_matches_rtl_contract(self) -> None:
        main = read_cpp_source("memblock_main.cpp")
        common = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VecCommon.scala"
        ).read_text()

        configurations = 0
        uops = 0
        flows = 0
        for eew in range(4):
            for vsew in range(4):
                for lmul_log2 in range(-3, 4):
                    emul_log2 = eew - vsew + lmul_log2
                    if lmul_log2 < vsew - 3 or not -3 <= emul_log2 <= 3:
                        continue
                    uop_count = 1 << max(emul_log2, 0)
                    bytes_per_uop = 16 >> max(-emul_log2, 0)
                    configurations += 1
                    uops += uop_count
                    flows += uop_count * (bytes_per_uop >> eew)
        self.assertEqual(configurations, 78)
        self.assertEqual(uops, 202)
        self.assertEqual(flows, 1152)

        for contract in (
            "run_matrix_load",
            "run_matrix_store",
            "kVirtualLoadQueueEntries -",
            "kLqEnqueueHeadroom",
            "kStoreQueueEntries -",
            "kSqEnqueueHeadroom",
            "strided_configurations != 78",
            "strided_load_uops != 404",
            "strided_store_uops != 202",
            "strided_positive != 39",
            "strided_negative != 39",
            "strided_matrix.lq_allocated() != 2304",
            "strided_matrix.sq_allocated() != 1152",
            '<< " strided_configurations="',
        ):
            self.assertIn(contract, main)
        self.assertIn(
            '(MulDataSize(emul) >> eew).asUInt, // strided', common
        )

    def test_vector_indexed_lmul_emul_matrix_matches_rtl_contract(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        common = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VecCommon.scala"
        ).read_text()
        split = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VSplit.scala"
        ).read_text()

        configurations = 0
        uops = 0
        flows = 0
        special = 0
        for eew in range(4):
            for vsew in range(4):
                for lmul_log2 in range(-3, 4):
                    emul_log2 = eew - vsew + lmul_log2
                    if lmul_log2 < vsew - 3 or not -3 <= emul_log2 <= 3:
                        continue
                    uop_count = 1 << max(lmul_log2, emul_log2, 0)
                    if emul_log2 > lmul_log2:
                        bytes_per_uop = 16 >> max(-emul_log2, 0)
                        flow_num = bytes_per_uop >> eew
                        special += 1
                    else:
                        bytes_per_uop = 16 >> max(-lmul_log2, 0)
                        flow_num = bytes_per_uop >> vsew
                    configurations += 1
                    uops += uop_count
                    flows += uop_count * flow_num
        self.assertEqual(configurations, 78)
        self.assertEqual(uops, 254)
        self.assertEqual(flows, 1152)
        self.assertEqual(special, 28)

        for contract in (
            "vector_data_eew",
            "vector_emul_log2",
            "vector_is_special_indexed",
            "vector_indexed_vd_index",
            "vector_indexed_split_offset",
            "vector_writeback_elements",
            "make_indexed_lmul_uops",
            "std::max({lmul_log2, emul_log2, 0})",
            "indexed_lmul_configurations != 156",
            "indexed_lmul_unordered != 78",
            "indexed_lmul_ordered != 78",
            "indexed_lmul_special != 56",
            "indexed_lmul_load_uops != 1016",
            "indexed_lmul_store_uops != 508",
            "indexed_lmul_matrix.lq_allocated() != 4608",
            "indexed_lmul_matrix.sq_allocated() != 2304",
            '<< " indexed_lmul_configurations="',
        ):
            self.assertIn(contract, environment + main)
        for contract in (
            "Mux(emul.asSInt > lmul.asSInt",
            "(MulDataSize(emul) >> eew).asUInt",
            "(MulDataSize(lmul) >> sew(1,0)).asUInt",
            "object GenVdIdxInField",
        ):
            self.assertIn(contract, common)
        for contract in (
            "val indexedSplitOffset = Mux(isSpecialIndexed",
            "isIndexed(s0_mop) && s0_lmul.asSInt > s0_emul.asSInt",
        ):
            self.assertIn(contract, split)

    def test_vector_whole_register_matrix_matches_rtl_contract(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        package = (REPO_ROOT / "src/main/scala/xiangshan/package.scala").read_text()
        split = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VSplit.scala"
        ).read_text()
        common = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VecCommon.scala"
        ).read_text()
        for contract in (
            "kVectorLoadWholeRegister = 0x088",
            "kVectorStoreWholeRegister = 0x108",
            "bool whole_register = false",
            "vector_effective_vl",
            "(registers * 16U) >> transaction.eew",
            "whole_nfs{{0, 1, 3, 7}}",
            ".whole_register = true",
            ".vl = 1",
            "whole_cases != 16",
            "whole_load_uops != 120",
            "whole_store_uops != 60",
            '<< " whole_lq_allocated="',
            '<< " whole_sq_allocated="',
        ):
            self.assertIn(contract, environment + main)
        for contract in (
            'def vlr       = "b01_00_01000".U',
            'def vsr       = "b10_00_01000".U',
            "GenUSWholeRegVL",
            "GenUSWholeEmul",
            "override def us_whole_reg",
        ):
            self.assertIn(contract, package + split + common)

    def test_vector_indexed_matrix_covers_modes_eews_and_queue_wraps(self) -> None:
        main = read_cpp_source("memblock_main.cpp")
        for contract in (
            "indexed_modes",
            "VectorAddressingMode::indexed_unordered",
            "VectorAddressingMode::indexed_ordered",
            "fill_indices",
            "alias_last",
            "case_base + 0xfc0",
            "case_base + 0x1fc0",
            "indexed_cases != 8",
            "indexed_load_uops != 16",
            "indexed_store_uops != 8",
            "indexed_matrix.lq_allocated() != 120",
            "indexed_matrix.sq_allocated() != 60",
            '<< " indexed_lq_allocated="',
            '<< " indexed_sq_allocated="',
        ):
            self.assertIn(contract, main)

    def test_dcache_coherence_tracks_concurrent_probe_sources(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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

    def test_dcache_probe_ack_requires_accepted_b_provenance(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        for contract in (
            "bool b_accepted = false",
            "accepted a B Probe without a ",
            "matching expectation",
            "accepted the same B Probe source twice",
            "response->b_accepted = true",
            "expected.base == response.address &&",
            "expected.b_accepted",
            "DCache ProbeAck completed before its B Probe was accepted",
        ):
            self.assertIn(contract, environment)

    def test_dcache_errors_cross_per_beat_corruption_and_mshr_isolation(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        for contract in (
            "enum class DcacheCorruptBeat",
            "inject_response_error_at",
            "selected_corrupt_beat",
            "DCache TileLink A source reused before D completion",
            "dcache_max_outstanding_requests",
            "dcache_responses_idle",
            "force_next_dcache_interbeat_delay(256)",
            "phase=multibeat-overlap-window",
            "multibeat_cases != 6",
            "multibeat_denied_cases != 2",
            "multibeat_corrupt_first_cases != 2",
            "multibeat_corrupt_last_cases != 2",
            "multibeat_keyword_cases != 3",
            "multibeat_nonkeyword_cases != 3",
            "multibeat_concurrent_mshr_cases != 6",
            "multibeat_poisoned_hit_cases != 6",
            "multibeat_healthy_hit_cases != 6",
            "VectorLateErrorCase",
            "phase=vector-late-error-primer",
            "requests_before_vector",
            "vector_late_errors=",
            "vector_late_denied=",
            "vector_late_corrupt_last=",
        ):
            self.assertIn(contract, environment + main)

    def test_hardware_prefetch_records_sms_pht_characterization(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        for contract in (
            "configure_sms_pht_prefetch(false)",
            "configure_sms_pht_prefetch(true)",
            "l2_addresses_by_source",
            "sms_training_offsets",
            '" sms_pht_l2="',
            "sms_offset_bitmap",
        ):
            self.assertIn(contract, environment + main)

    def test_memory_trigger_has_control_match_chain_and_store_matrix(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        segment_rtl = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VSegmentUnit.scala"
        ).read_text()
        vector_transaction = environment.split(
            "struct VectorMemoryTransaction", 1
        )[1].split("inline std::uint16_t vector_fu_op_type", 1)[0]
        self.assertIn("bool check_data = true;", vector_transaction)
        self.assertIn(
            "segmentTrigger.io.fromLoadStore.vaddr                 := tlbReqVaddr",
            segment_rtl,
        )
        self.assertNotIn(
            "segmentTrigger.io.fromLoadStore.vaddr                 := "
            "Mux(isMisalignReg, misalignVaddr, latchVaddr)",
            segment_rtl,
        )
        for contract in (
            "MemoryTriggerConfig",
            "kTriggerMatchGreaterOrEqual",
            "kTriggerMatchLessThan",
            "config.enable_mask",
            "config.trigger_can_raise_breakpoint",
            "config.debug_mode",
            ".check_data = !debug_action",
            "load data checking may only be disabled for DebugMode trigger",
            "vector data checking may only be disabled for DebugMode trigger",
            '"equal-hit"',
            '"breakpoint-gate"',
            '"debug-mode-suppression"',
            '"select-suppression"',
            '"greater-equal-hit"',
            '"less-than-hit"',
            '"chain-hit"',
            '"chain-miss"',
            '"slot-two-hit"',
            '"slot-three-hit"',
            '"debug-action"',
            '"misaligned-store-hit"',
            "phase=vector-load-hit",
            "phase=vector-store-hit",
            "phase=post-vector-store-fault-address",
            "scalar_store_fault_address",
            "vector_store.rob, vector_store.rob_flag, true, true",
            '"vector-segment-indexed-load-breakpoint"',
            '"vector-segment-strided-store-breakpoint"',
            '"vector-unit-eew32-debug-action"',
            '"vector-indexed-eew64-store-debug-action"',
            '" cases=25 actions=2 match_types=3 enabled_slots=4"',
            '" debug_action_loads="',
            '" scalar_store_breakpoints="',
            '" vector_cases=8 vector_loads=4 vector_stores=4"',
            '" vector_debug_actions="',
            '" vector_widths=4 vector_addressing_modes=4 chain_cases=2"',
        ):
            self.assertIn(contract, environment + main)

    def test_frontend_bridge_has_semantic_transaction_coverage(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")

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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
            "std::array<memblock::AtomicOp, 12> doubleword_atomic_ops",
            "std::array<memblock::AtomicOp, 12> word_atomic_ops",
            "op == memblock::AtomicOp::lr_w",
            "kExceptionLoadAddressMisaligned",
            "misaligned_d_cases=",
            "misaligned_w_cases=",
            "misaligned_rob_wraps != 1",
            '<< " misaligned_rob_wraps="',
            "pma_atomic_address = 0x7ffffff8ULL",
            "phase=pma-atomic-denied",
            "pma_atomic_denied=1",
            "pma_atomic_cycles=",
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
        environment = read_cpp_source("memblock_env.hpp")

        for diagnostic in (
            "duplicate outstanding scalar load ROB value",
            "duplicate outstanding scalar store ROB value",
            "duplicate outstanding vector memory uop",
            "duplicate store-address writeback",
            "duplicate store-data writeback",
            "expected_address_writeback_lane",
            "expected_data_writeback_lane",
            "issue_address_lane",
            "issue_data_lane",
            "writeback_address_lane",
            "writeback_data_lane",
            "wrong store-address writeback lane",
            "wrong store-data writeback lane",
            "scalar store-address issue lane differs from expectation",
            "scalar store-data issue lane differs from expectation",
        ):
            self.assertIn(diagnostic, environment)

    def test_store_writeback_is_gated_by_issue_handshake_epoch(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
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
        scoreboards = read_cpp_source("environment/scoreboards.inc")
        load_scoreboard = scoreboards.split("class LoadScoreboard {", 1)[1].split(
            "class StoreScoreboard {", 1
        )[0]
        self.assertIn("std::optional<bool> debug_is_mmio", load_scoreboard)
        self.assertIn("optional_mismatch(", load_scoreboard)
        self.assertNotIn("expected_debug_is_ncio.value_or(false)", load_scoreboard)
        self.assertNotIn("expected_debug_is_mmio.value_or(false)", load_scoreboard)

    def test_translation_faults_cover_stage_one_and_gstage_pte_encodings(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
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

    def test_translation_oracle_models_legal_svnapot(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        for contract in (
            "reference_pte_is_napot",
            "input_address & napot_offset_mask",
            "map_reference_napot64k",
            "map_sv39_napot64k",
            "map_sv48_napot64k",
            "map_sv39x4_napot64k",
            "map_sv48x4_napot64k",
            "for (unsigned page = 0; page < 16; ++page)",
            "(physical_base >> 12) | 8U",
        ):
            self.assertIn(contract, environment)
        for contract in (
            "napot_stage1_subpages=",
            "napot_gstage_subpages=",
            "napot_loads=",
            "napot_stores=",
            "napot_readbacks=",
            "run_napot_case(memblock::ReferencePageMode::sv39, false",
            "run_napot_case(memblock::ReferencePageMode::sv48, true",
            "std::array<NestedNapotPair, 4>",
            "std::array<NestedNapotTopology, 3>",
            '{true, false, "VS-NAPOT"}',
            '{false, true, "G-NAPOT"}',
            '{true, true, "VS-G-NAPOT"}',
            "napot_nested_mode_cases=",
            "napot_nested_both_subpages=",
        ):
            self.assertIn(contract, main)

    def test_ptw_manager_errors_are_injectable_by_walk_level(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        for contract in (
            "PtwCorruptBeat",
            "inject_response_error_after",
            "inject_response_error_at",
            "pending_response_error_->clean_requests",
            "pending_response_error_->address",
            "selected_corrupt_beat",
            "denied || (corrupt && selected_corrupt_beat)",
            "response.denied",
            "response.corrupt",
            "ptw_error_response_requests",
            "ptw_denied_d_beats",
            "ptw_corrupt_d_beats",
            "dcache_requests_covering_since",
        ):
            self.assertIn(contract, environment)
        for contract in (
            "sv39-load-root-denied",
            "sv39-load-leaf-corrupt",
            "sv48-store-middle-corrupt",
            "sv48-store-leaf-denied",
            "first_beat_corrupt=",
            "last_beat_corrupt=",
            "clean_recoveries=",
            "expected_failed_block_rereads",
            "MEMBLOCK_PTW_ERRORS_PASS",
        ):
            self.assertIn(contract, main)
        self.assertIn("ptw-errors:", makefile)

    def test_mixed_commit_boundary_does_not_auto_commit_next_rob(self) -> None:
        main = read_cpp_source("memblock_main.cpp")
        self.assertIn("rob_offset - 1", main)
        self.assertIn("Keep the commit boundary at the last uop", main)

    def test_vector_store_commit_uses_its_enqueue_time_sq_target(self) -> None:
        enqueue = read_cpp_source("environment/issue.cpp")
        queues = read_cpp_source("environment/queue_accounting.cpp")
        completion = read_cpp_definition("environment/completion.cpp", "Environment::Impl::commit_vector_store")
        self.assertIn(
            "queues_.allocate_vector_store(vector_store_key(transaction), enqueue.num_ls_elem)", enqueue
        )
        self.assertIn("vector_store_sq_targets_[key] = sq_allocated_ - sq_canceled_", queues)
        self.assertIn("vector_store_sq_targets_.find(key)", queues)
        self.assertIn("return it->second", queues)
        self.assertIn("queues_.vector_store_target(\n        vector_store_key(transaction))", completion)
        self.assertIn("const std::uint64_t target = *target_it", completion)
        self.assertIn("if (queues_.sq_dequeued() < target", completion)
        self.assertIn("run_until_sq_dequeued(target, timeout)", completion)

    def test_reference_memory_is_separate_from_bus_backing_memory(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        for contract in (
            "SparseMemory bus_memory_;",
            "SparseMemory memory_;",
            "memory_(&bus_memory_)",
            "memory_agent_(bus_memory_, memory_)",
            "ptw_agent_(bus_memory_)",
            "uncache_agent_(bus_memory_)",
            "memory_.write_reference_byte(",
            "reference_memory_.read_beat(",
            "bus_expected_load",
        ):
            self.assertIn(contract, environment)

    def test_random_cacheable_memory_is_stable_before_hardware_prefetch(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        initialize = read_cpp_definition("scenarios/mixed_initialization.cpp", "MixedScenario::initialize_memory")
        initialize_run = read_cpp_definition("scenarios/mixed_initialization.cpp", "MixedScenario::initialize_run")
        run = read_cpp_definition("scenarios/mixed_reporting.cpp", "MixedScenario::run")
        execute = read_cpp_definition("scenarios/mixed_scheduler.cpp", "MixedScenario::execute")

        for contract in (
            "define_seeded_random(",
            "define_reference_seeded_random(",
            "seeded_random_",
            "seeded_random_word(address, region->seed)",
        ):
            self.assertIn(contract, environment)
        self.assertLess(run.index("initialize_memory();"), run.index("execute();"))
        self.assertLess(execute.index("initialize_run()"), execute.index("prepare_tail()"))
        self.assertIn("if (!environment.reset()", initialize_run)
        for definition in (
            "bare_base, 0x20000, options.seed ^ 0x19U",
            "cache0_base, 0x93000, options.seed ^ 0x43U",
            "cache1_base, 0x10000, options.seed ^ 0x71U",
            "atomic_base, 0x1000, options.seed ^ 0xd3U",
            "stage1_napot_base, 0x40000, options.seed ^ 0xe1U",
            "load_merge_base, load_merge_span, options.seed ^ 0x41U",
            "miss_burst_base, miss_burst_span, options.seed ^ 0x61U",
            "bank_conflict_base, bank_conflict_span, options.seed ^ 0x71U",
        ):
            self.assertIn(definition, initialize)
        load_merge = read_cpp_definition("scenarios/mixed_action_scalar.cpp", "MixedScenario::execute_load_merge")
        miss_burst = read_cpp_definition("scenarios/mixed_action_pressure.cpp", "MixedScenario::execute_miss_burst")
        self.assertNotIn("write_byte", load_merge)
        self.assertNotIn("fill_incrementing", miss_burst)
        for contract in (
            "dcache_requests_covering_since(line, 0) != 0",
            "manager_delta[1] < manager_delta[0]",
            "manager_delta[4] < depth",
            "manager_delta[5] < manager_delta[4]",
            "manager_delta[8] != (include_vector_load ? 1U : 0U)",
            "vector_transaction ? vector_transaction->flow_num : 0U",
        ):
            self.assertIn(contract, driver)
        self.assertRegex(load_merge, r"dcache_requests_covering_since\(\s*candidate, 0\) == 0")
        self.assertRegex(miss_burst, r"dcache_outstanding_requests\(\) >=\s*pressure_depth")

    def test_mem_direct_debug_is_read_only_and_optional(self) -> None:
        makefile = read_makefile()
        environment = read_cpp_source("environment/implementation.hpp")
        debug = environment[
            environment.index(
                "#ifdef MEMBLOCK_MEM_DIRECT_DEBUG\n"
                "    static std::uint64_t debug_line_from_env()"
            ):
            environment.index(
                "#endif\n    bool wait_for_enqueue_capacity"
            )
        ]

        for read_contract in (
            "dut.internal_signal(name)",
            "signal->bytes()",
            "signal->value()",
            "loadQueue.exceptionBuffer.",
            "lq_exception retained=",
            "lq_exception candidate_port=",
        ):
            self.assertIn(read_contract, debug)
        self.assertNotIn("crossPageCanDeq_0", debug)
        self.assertIn("crossPageWithHit", debug)
        self.assertIn("doDeq_T", debug)
        for forbidden_write_path in ("VPI", "ImmSet", "SetBytes", "->Set(", ".set(", "->set(", "set_bytes("):
            self.assertNotIn(forbidden_write_path, debug)
        adapter = read_cpp_source("environment/dut_adapter.cpp")
        self.assertIn("auto *signal = impl_->dut.GetInternalSignal(name)", adapter)
        self.assertIn("inserted->second = ReadPort(signal, 0)", adapter)
        read_port = read_cpp_source("environment/dut_adapter.hpp").split(
            "class ReadPort {", 1
        )[1].split("class DrivenPort", 1)[0]
        self.assertNotIn("void set", read_port)
        export_rule = makefile[
            makefile.index("$(PICKER) export"):
            makefile.index("\n\t@touch $@", makefile.index("$(PICKER) export"))
        ]
        self.assertIn("--rw mem_direct", export_rule)
        self.assertNotIn("--vpi", export_rule)
        self.assertIn("scripts/prepare_picker_export.py", makefile)
        self.assertIn("mem_direct/export.bin > $(PICKER_OFFSET)", makefile)
        harness = (
            MEMBLOCK_ROOT / "scripts/prepare_picker_harness.py"
        ).read_text()
        self.assertIn('picker_output / "MemBlock_offset.yaml"', harness)

    def test_vector_timeout_reports_pending_top_level_identities(self) -> None:
        scoreboards = (
            MEMBLOCK_ROOT / "cpp/environment/scoreboards.inc"
        ).read_text()
        environment = read_cpp_source("memblock_env.hpp")

        self.assertIn("std::string pending_summary() const", scoreboards)
        for field in (
            "expected.last_uop",
            "expected.lq",
            "expected.lq_flag",
            "expected.sq",
            "expected.sq_flag",
            "expected.flow_num",
            "expected.addressing",
            "expected.address",
        ):
            self.assertIn(field, scoreboards)
        for field in (
            "transaction.vuop_idx",
            "transaction.last_uop",
            "transaction.lq_flag",
            "transaction.sq_flag",
        ):
            self.assertIn(field, environment)
        self.assertGreaterEqual(
            environment.count("vector_scoreboard_.pending_summary()"), 4
        )

    def test_uncache_store_order_uses_bus_backing_oracle(self) -> None:
        main = read_cpp_source("memblock_main.cpp")
        self.assertIn("int run_store_rdata_order", main)
        self.assertIn("environment.bus_expected_load(older.address", main)
        self.assertIn("environment.bus_expected_load(younger.address", main)

    def test_reset_recovery_reasserts_reset_and_is_a_registered_target(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        reset = read_cpp_source("environment/reset.cpp")
        adapter = read_cpp_source("environment/dut_adapter.cpp")
        self.assertIn("dut_.reset.external.set(std::uint64_t{1});", reset)
        self.assertIn("reset.external = DrivenPort(&impl_->dut.reset, 1)", adapter)
        self.assertIn("int run_reset_recovery", main)
        self.assertIn('"name": "reset-recovery"', main)
        self.assertIn("reset-recovery", makefile)
        self.assertIn("account_lq_cancellation", main)
        self.assertEqual(environment.count("void reset_link_state()"), 3)
        for contract in (
            "memory_agent_.reset_link_state();",
            "ptw_agent_.reset_link_state();",
            "uncache_agent_.reset_link_state();",
            '" outstanding_resets=13"',
            '" dcache=1 ptw=1 uncache=1"',
        ):
            self.assertIn(contract, environment + main)

    def test_reset_tree_contract_covers_functional_dft_and_scan_modes(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        benchmark = (MEMBLOCK_ROOT / "scripts/benchmark_tests.py").read_text()
        for contract in (
            "check_reset_backend_contract",
            '"functional-assert"',
            '"functional-release"',
            '"dft-functional-isolation"',
            '"dft-functional-release"',
            '"scan-assert"',
            '"scan-release"',
            "functional_release_cycles = 2 * sync_stages",
            '"name": "reset-tree-contracts"',
            "reset-tree-contracts",
        ):
            self.assertIn(contract, environment + main + makefile + benchmark)

    def test_wfi_safety_drains_each_memory_manager_before_safe(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
        load_unit = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala"
        ).read_text()
        pma = (
            REPO_ROOT / "src/main/scala/xiangshan/backend/fu/PMA.scala"
        ).read_text()
        new_csr = (
            REPO_ROOT / "src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala"
        ).read_text()
        soc = (REPO_ROOT / "src/main/scala/system/SoC.scala").read_text()
        for contract in (
            "pte_pbmt_io",
            "pulse_pending_load",
            "wait_for_mmio_store_request",
            "set_debug_mode",
            "io_ooo_to_mem_tlbCsr_priv_debug",
            "expected_debug_is_mmio",
            "expected_debug_is_ncio",
            "struct UncacheDeviceAccess",
            "configure_device_window",
            "record_device_access",
            "configure_uncache_device",
            "uncache_device_accesses",
            "run_until_load_writebacks",
            "struct LoadMmioStats",
            "io_mem_to_ooo_lsqio_loadMmio_0",
            "io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value",
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
            "pma_denied_wakeups=",
            "pma_denied_cancels=",
            "debug_physical_base = 0x38020000ULL",
            "phase=pma-debug-load",
            "phase=pma-debug-store",
            "pma_debug_loads=",
            "pma_debug_stores=",
            "pma-edge-device",
            "pma-edge-ddr",
            "pma_edge_loads=",
            "pma_edge_dcache=",
            "pma_edge_uncache=",
            "pma_cycles=",
            "phase=device-read-error-policy",
            "phase=device-write-error-policy",
            "phase=device-access-log",
            "accesses.size() != 7",
            "device_accesses=",
            "device_read_clear=1 device_partial_write=1",
            "device_error_reads=2 device_error_writes=1",
            "device_error_side_effects=0",
            "phase=device-queued-first-delay",
            "phase=device-queued-second-delay",
            "phase=device-queued-order",
            "queued_device_loads=",
            "queued_device_max_outstanding=",
            "phase=device-mixed-load-delay",
            "phase=device-mixed-store-delay",
            "phase=device-mixed-order",
            "mixed_device_ops=",
            "mixed_device_max_outstanding=",
            "load_mmio_before_response",
            "load_mmio_pulses=3 load_mmio_input_lanes=3",
            "load_mmio_output_slots=",
            "non_mmio_controls=2",
            "phase=load-mmio-parallel-check",
            "mmio-contracts",
        ):
            self.assertIn(contract, main + makefile)
        self.assertIn("io.tlb.debug := debugMode", new_csr)
        self.assertIn(
            "Mux(addr >= debugStart.U && addr <= debugEnd.U, debug, true.B)",
            pma,
        )
        self.assertIn(
            "PMAConfigEntry(0x80000000L, a = 1, w = true, r = true)",
            soc,
        )

    def test_fp_loads_cover_data_paths_and_exception_suppression(self) -> None:
        main = read_cpp_source("memblock_main.cpp")
        fp_loads = main[
            main.index("int run_fp_loads") : main.index("int run_trigger_contracts")
        ]
        load_unit = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala"
        ).read_text()
        for contract in (
            '" mmio_fp="',
            '" mmio_flh_success="',
            '" mmio_flh_sign0="',
            '" mmio_flh_sign1="',
            '" mmio_flw_success="',
            '" mmio_flw_sign0="',
            '" mmio_flw_sign1="',
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

    def test_lsq_enqueue_accounting_is_observed_from_top_level_pins(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        generated = ((MEMBLOCK_ROOT / "cpp/generated_port_defaults.hpp").read_text() + (MEMBLOCK_ROOT / "cpp/generated_interface_types.hpp").read_text())

        for contract in (
            "struct LsqEnqueueObservation",
            "sample_lsq_enqueue",
            "bits_numLsElem.U()",
            "needAlloc_0.U()",
        ):
            self.assertIn(contract, generated)
        for contract in (
            "lq_enqueued_observed_ += enqueue.num_ls_elem",
            "sq_enqueued_observed_ += enqueue.num_ls_elem",
            "lsq_enqueue_widths_observed_.at(lsq_enqueue_width - 1)",
            "LSQ enqueue monitor disagrees with driver accounting",
            "invalid accepted LSQ enqueue on lane",
            "dut_.queues.retirement.lq_cancel_cnt.value()",
            "dut_.queues.retirement.sq_cancel_cnt.value()",
            "lq_redirect_canceled_observed_ += lq_canceled",
            "sq_redirect_canceled_observed_ += sq_canceled",
            "lq_canceled_unobserved_ += count",
            "sq_canceled_unobserved_ += count",
        ):
            self.assertIn(contract, environment)
        adapter = read_cpp_source("environment/dut_adapter.cpp")
        for binding in (
            "queues.retirement.lq_cancel_cnt = ReadPort(&impl_->dut.io_mem_to_ooo_lqCancelCnt, 7)",
            "queues.retirement.sq_cancel_cnt = ReadPort(&impl_->dut.io_mem_to_ooo_sqCancelCnt, 6)",
            "generated::sample_lsq_enqueue(impl_->dut, lane)",
        ):
            self.assertIn(binding, adapter)
        self.assertIn("dut_.sample_lsq_enqueue(lane)", read_cpp_source("environment/queue_accounting.cpp"))
        self.assertIn(
            "environment.lsq_enqueue_widths_observed()", main
        )
        self.assertIn(
            "environment.lsq_enqueue_lanes_observed()", main
        )
        self.assertIn("lsq_monitor_schema=3", main)
        self.assertIn("redirect_cancels_observed=", main)
        self.assertIn("unobserved_cancels=", main)

    def test_vector_oracle_allows_only_spec_permitted_agnostic_data(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        for contract in (
            "matches_load_data",
            "vector_writeback_global_element",
            "vector_indexed_vd_index(transaction) * element_count + element",
            "vector_tail_elements",
            "vector_mask_inactive_elements",
            "tail_agnostic",
            "mask_agnostic",
            "!preserved && !all_ones",
            "fof_unspecified_elements",
            "FOF unspecified elements must be active load elements",
            "expected.fof_unspecified_elements >> element",
            "writeback.vmask, expected.active_elements,",
        ):
            self.assertIn(contract, environment)

    def test_vector_full_mask_bits_are_modeled_and_driven(self) -> None:
        model = read_cpp_source("environment/model.inc")
        manager = read_cpp_source("environment/manager_control.cpp")
        scenario = read_cpp_source("scenarios/cache_queue.inc")
        for contract in (
            "std::array<std::uint64_t, 2> mask_high{}",
            "inline bool vector_mask_enabled(",
            "if (global_element < 128)",
            "vector_mask_enabled(transaction, global_element)",
            "!vector_mask_enabled(transaction, *global)",
        ):
            self.assertIn(contract, model)
        for contract in (
            "std::array<unsigned char, 16> mask_bytes{}",
            "if (vector_mask_enabled(transaction, bit))",
            "mask_bytes[bit / 8] |=",
            "issue.vmask = mask_bytes",
            "issue.src[3] = mask_bytes",
        ):
            self.assertIn(contract, manager)
        for contract in (
            "high_mask_loads[1].mask_high[0] = 0x00ff",
            "high_mask_stores[1].mask_high[0] = 0x0f0f",
            "high_mask_indexed[1].mask_high[0] = 0x00ff",
        ):
            self.assertIn(contract, scenario)

    def test_canceled_load_result_is_explicit_and_opt_in(self) -> None:
        model = read_cpp_source("environment/model.inc")
        scoreboard = read_cpp_source("environment/scoreboards.inc")
        environment = read_cpp_source("memblock_env.hpp")
        feedback = read_cpp_source("environment/feedback_monitor.cpp")
        scenario = read_cpp_source("scenarios/translation_permissions.inc")
        recovery = read_cpp_source("scenarios/alias_refetch.hpp")
        for contract in (
            "bool tolerate_canceled_result = false",
        ):
            self.assertIn(contract, model)
        for contract in (
            "bool tolerate_canceled_result;",
            "canceled_observed_",
            "it->second.tolerate_canceled_result",
            "canceled_observed() const",
        ):
            self.assertIn(contract, scoreboard)
        self.assertIn("scalar_load_canceled_observed()", environment)
        self.assertIn("scoreboard_.observe(lane, writeback, memory_violation)", feedback)
        for contract in (
            "load.tolerate_canceled_result = true",
            "load.tolerate_canceled_result = false",
            "scalar_load_canceled_observed()",
            "lq_redirect_canceled_observed()",
        ):
            self.assertIn(contract, recovery)
        self.assertIn("MEMBLOCK_TRANSLATION_ALIAS_FIRST_ACCESS", scenario)
        self.assertIn("complete_alias_load(environment, in_flight_load", scenario)
        self.assertIn("complete_alias_load(canceled_alias, first_access_load", scenario)

    def test_non_cacheable_final_effect_tracking_is_registered(self) -> None:
        completion = read_cpp_source("environment/completion.cpp")
        environment = read_cpp_source("memblock_env.hpp")
        reporting = read_cpp_source("scenarios/mixed_reporting.cpp")
        for contract in (
            "final_observation_nc_words_",
            "final_observation_nc_bytes_",
            "non-cacheable final effect mismatch address=0x",
            "bus_memory_.read_u64(word)",
        ):
            self.assertIn(contract, completion)
        for contract in (
            "final_observation_nc_words()",
            "final_observation_nc_bytes()",
            "verify_final_nc_effects()",
        ):
            self.assertIn(contract, environment + reporting)
        self.assertIn("return environment.verify_final_nc_effects();", reporting)

    def test_vector_fault_only_first_contract_is_registered(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
            "first_element_faults=1",
            "first_fault_fix_vl=2",
            "first_fault.exception_vaddr() != first_fault_virtual",
            "phase=subsequent-scalar-fault-address",
            "subsequent_scalar_faults=1",
            "first_fault_data.rob, first_fault_data.rob_flag, true, true",
            "vector-fof",
        ):
            self.assertIn(contract, environment + main + makefile + benchmark)
        self.assertIn("entries.hasException", vfof)
        self.assertIn("io.uopWriteback.bits.data             := entries.vl", vfof)
        self.assertIn(
            "value after the redirect is deliberately not sampled", main
        )
        self.assertNotIn(
            "first_fault.exception_vaddr() != first_fault_virtual ||\n"
            "        !first_fault.run_cycles(8) ||\n"
            "        first_fault.exception_vaddr() != first_fault_virtual",
            main,
        )

    def test_constrained_random_vfof_avoids_unqualified_fault_vaddr_oracle(self) -> None:
        main = read_cpp_definition("scenarios/mixed_action_vector.cpp", "MixedScenario::execute_vector_memory")
        begin = main.index('phase = vector_fof_first_fault')
        end = main.index(
            "++constraint_coverage.vector_fof_presence[1]", begin
        )
        scenario = main[begin:end]

        for contract in (
            ':initial-completion-drain',
            ':initial-queue-drain',
            ':vector-completion',
            "if (vector_fof_first_fault &&",
            ':fix-vl-issue',
            ':fix-vl-completion',
            ':fix-vl-lq-allocation',
            ':fix-vl-writeback',
            ':trap-redirect',
            "data_uop.rob, data_uop.rob_flag, true, true)",
        ):
            self.assertIn(contract, scenario)

        self.assertNotIn("exception_vaddr()", scenario)
        self.assertNotIn(":fault-vaddr", scenario)
        self.assertRegex(scenario, r"environment\.redirect_after\(\s*data_uop\.rob, data_uop\.rob_flag, true, true\)")
        self.assertLess(scenario.index(":fix-vl-writeback"), scenario.index("environment.redirect_after("))

    def test_redirect_helper_distinguishes_vls_exception_contract(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")

        for contract in (
            "bool is_vls_exception = false",
            "is_vls_exception && !flush_itself",
            '"VLS exception redirect must enter as a flush redirect"',
            "dut_.queues.redirect.is_vls_exception.set(is_vls_exception)",
            "dut_.queues.redirect.is_vls_exception.set(std::uint64_t{0})",
            "data_uop.rob, data_uop.rob_flag, true, true",
            "vector_load.rob, vector_load.rob_flag, true, true",
            "vector_store.rob, vector_store.rob_flag, true, true",
            "transaction.rob, transaction.rob_flag, true, true",
        ):
            self.assertIn(contract, environment + main)
        self.assertIn(
            "queues.redirect.is_vls_exception = DrivenPort(&impl_->dut.io_redirect_bits_isVlsException, 1)",
            read_cpp_source("environment/dut_adapter.cpp"),
        )

    def test_vector_segment_contract_is_registered(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
            "vector_segment_field_index",
            "vector_segment_vd_index",
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

    def test_vector_segment_lmul_emul_nf_matrix_matches_rtl_contract(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        common = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VecCommon.scala"
        ).read_text()
        segment = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VSegmentUnit.scala"
        ).read_text()
        uop_info = (
            REPO_ROOT / "src/main/scala/xiangshan/backend/decode/UopInfoGen.scala"
        ).read_text()

        configurations = 0
        uops = 0
        flows = 0
        for eew in range(4):
            for vsew in range(4):
                for lmul_log2 in range(-3, 4):
                    emul_log2 = eew - vsew + lmul_log2
                    if lmul_log2 < vsew - 3 or not -3 <= emul_log2 <= 3:
                        continue
                    uops_per_field = 1 << max(emul_log2, 0)
                    bytes_per_uop = 16 >> max(-emul_log2, 0)
                    flow_num = bytes_per_uop >> eew
                    for fields in range(2, 9):
                        if uops_per_field * fields > 8:
                            continue
                        configurations += 1
                        uops += uops_per_field * fields
                        flows += uops_per_field * fields * flow_num
        self.assertEqual(configurations, 338)
        self.assertEqual(uops, 1774)
        self.assertEqual(flows, 9792)

        for contract in (
            "vector_segment_data_mul_log2",
            "vector_segment_uops_per_field",
            "vector_segment_field_index",
            "vector_segment_vd_index",
            "make_segment_lmul_uops",
            "segment_lmul_configurations != 676",
            "segment_lmul_unit_stride != 338",
            "segment_lmul_strided != 338",
            "segment_lmul_positive_stride != 169",
            "segment_lmul_negative_stride != 169",
            "segment_lmul_load_uops != 7096",
            "segment_lmul_store_uops != 3548",
            "segment_lmul_rob_wraps != 12",
            '<< " lmul_configurations="',
        ):
            self.assertIn(contract, environment + main)
        self.assertIn(
            "GenRealFlowNum(instType, emul, lmul, eew, sew, true)", segment
        )
        self.assertIn("val splitPtrOffset = Mux(", segment)
        self.assertIn("class strdiedLSNumOfUopTable", uop_info)
        self.assertIn("(MulDataSize(emul) >> eew).asUInt", common)

    def test_vector_indexed_segment_lmul_emul_nf_matrix_matches_rtl_contract(
        self,
    ) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        common = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VecCommon.scala"
        ).read_text()
        segment = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/vector/VSegmentUnit.scala"
        ).read_text()
        uop_info = (
            REPO_ROOT / "src/main/scala/xiangshan/backend/decode/UopInfoGen.scala"
        ).read_text()

        configurations = 0
        total_uops = 0
        data_uops = 0
        index_only_uops = 0
        data_flows = 0
        extra_index_configurations = 0
        for eew in range(4):
            for vsew in range(4):
                for lmul_log2 in range(-3, 4):
                    emul_log2 = eew - vsew + lmul_log2
                    if lmul_log2 < vsew - 3 or not -3 <= emul_log2 <= 3:
                        continue
                    lmul_uops = 1 << max(lmul_log2, 0)
                    emul_uops = 1 << max(emul_log2, 0)
                    bytes_per_uop = 16 >> max(-lmul_log2, 0)
                    flow_num = bytes_per_uop >> vsew
                    for fields in range(2, 9):
                        data_uop_count = lmul_uops * fields
                        if data_uop_count > 8:
                            continue
                        uop_count = max(data_uop_count, emul_uops)
                        configurations += 1
                        total_uops += uop_count
                        data_uops += data_uop_count
                        index_only_uops += uop_count - data_uop_count
                        data_flows += data_uop_count * flow_num
                        extra_index_configurations += emul_uops > data_uop_count
        self.assertEqual(configurations, 338)
        self.assertEqual(total_uops, 1816)
        self.assertEqual(data_uops, 1774)
        self.assertEqual(index_only_uops, 42)
        self.assertEqual(data_flows, 9792)
        self.assertEqual(extra_index_configurations, 16)

        for contract in (
            "std::optional<std::array<unsigned char, 128>> oracle_index_group",
            "std::optional<bool> vec_wen",
            "transaction.segment && transaction.oracle_index_group",
            "make_indexed_segment_uops",
            "const unsigned uop_count = std::max(data_uops, emul_uops)",
            ".vec_wen = !store && uop < data_uops",
            "indexed_segment_configurations != 676",
            "indexed_segment_unordered != 338",
            "indexed_segment_ordered != 338",
            "indexed_segment_extra_index_configs != 32",
            "indexed_segment_load_uops != 7264",
            "indexed_segment_store_uops != 3632",
            "indexed_segment_load_index_only_uops != 168",
            "indexed_segment_store_index_only_uops != 84",
            "indexed_segment_rob_wraps != 12",
            '<< " indexed_lmul_configurations="',
        ):
            self.assertIn(contract, environment + main)
        self.assertIn("class indexedLSNumOfUopTable", uop_info)
        self.assertIn(
            "GenRealFlowNum(instType, emul, lmul, eew, sew, true)", segment
        )
        self.assertIn("isIndexed(instType)", segment)
        self.assertIn("val splitPtrOffset = Mux(", segment)
        self.assertIn("val segmentIndexFlowNum", common)

    def test_vector_segment_fof_contract_is_registered(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
            ".allowed_additional_exception_mask =",
            "vector_page_fault_writebacks()",
            "first_element_faults=",
            "first_fault_fix_vl=2",
            "first_fault.exception_vaddr() != first_fault_virtual",
            "first_fault.exception_vaddr() != first_fault_virtual + 8U",
        ):
            self.assertIn(contract, environment + main + makefile + benchmark)
        self.assertNotIn("first_element_faults=2", main)
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

        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        makefile = read_makefile()
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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
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
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
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
        main = read_cpp_source("memblock_main.cpp")
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

    def test_translation_faults_cover_physical_ppn_overflow(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        for contract in (
            "kReferencePhysicalAddressBits = 48",
            "reference_pte_physical_address_fault",
            "reference_pte_guest_address_fault",
            "guest_address_bits",
            "bool access_fault = false",
            "!pte_translation.access_fault",
            "!final_translation.access_fault",
        ):
            self.assertIn(contract, environment)
        for contract in (
            "stage1-sv39-l1-ppn-bit36",
            "stage1-sv48-l2-ppn-bit43",
            "gstage-sv39-l1-ppn-bit36",
            "gstage-sv48-l2-ppn-bit43",
            "two_stage_reference.access_fault",
            "kExceptionLoadAccessFault",
            "kExceptionStoreAccessFault",
            "ppn_access_fault_cases=",
            "stage1_ppn_access_fault_cases=",
            "vs_only_ppn_access_fault_cases=",
            "gstage_ppn_access_fault_cases=",
            "leaf_ppn_access_fault_cases=",
            "nonleaf_ppn_access_fault_cases=",
            "intermediate_ppn_access_fault_cases=",
            "root_ppn_access_fault_cases=",
            "ppn_access_fault_ptw_requests=",
            "vs_only_ppn_access_fault_ptw_requests=",
            "vs-only-ppn-reference",
            "vs_gpa_width_cases=",
            "sv39x4_gpa_width_cases=",
            "sv48x4_gpa_width_cases=",
            "vs_gpa_intermediate_cases=",
            "expected_fault_pte_reads = 2",
            "expected_fault_gpa",
        ):
            self.assertIn(contract, main)

    def test_translation_permission_matrix_uses_top_level_csr_controls(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        main = read_cpp_source("memblock_main.cpp")
        for contract in (
            "ReferencePtePermissions",
            "ReferencePbmt",
            "reference_load_permitted",
            "reference_store_permitted",
            "reference_pte_permissions",
            "ReferencePermissionWalkResult",
            "reference_page_walk_permissions",
            "ReferenceMemoryAccess",
            "reference_memory_access_permitted",
            "reference_implicit_gstage_read_permitted",
            "reference_two_stage_access",
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
            "std::array<StageOneStoreCase, 16>",
            "std::array<TwoStageLoadVariant, 8>",
            "std::array<TwoStageStoreVariant, 10>",
            "std::vector<TwoStageLoadCase>",
            "std::vector<TwoStageStoreCase>",
            "sv39-s-user-sum0",
            "sv48-xonly-mxr1",
            "sv39-store-dirty0",
            "sv48-store-accessed0",
            "sv39-store-u-user",
            "sv48-store-u-supervisor-page",
            "vs-xonly-vmxr1",
            "g-accessed0",
            "g-dirty0",
            "g-user0",
            "std::array<ImplicitGPermissionVariant, 3>",
            'name = "xonly-mxr1"',
            'name = "user0"',
            'name = "accessed0"',
            "phase=implicit-g-reference",
            "phase=implicit-g-access",
            "phase=implicit-g-readonly-store-reference",
            "phase=implicit-g-readonly-store-readback",
            "exception_gpaddr() != *fault_pte_gpa",
            "ptw_requests_covering_since",
            "implicit_g_permission_faults=",
            "implicit_g_permission_configs=",
            "implicit_g_load_permission_faults=",
            "implicit_g_store_permission_faults=",
            "implicit_g_readonly_store_cases=",
            "expected_implicit_g_permission_configs = 42",
            "expected_implicit_g_permission_faults = 84",
            "expected_implicit_g_load_permission_faults = 42",
            "expected_implicit_g_store_permission_faults = 42",
            "expected_implicit_g_readonly_store_cases = 4",
            "expected_translation_permission_cases = 194",
            "phase=coverage",
            "permission_modes.size() * permission_modes.size() *",
            ".vs_mode = vs_mode",
            ".g_mode = g_mode",
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
        main = read_cpp_source("memblock_main.cpp")
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

    def test_miss_burst_capacity_plus_one_releases_after_issue_is_presented(
        self,
    ) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        dcache_agent = (
            MEMBLOCK_ROOT / "cpp/environment/dcache_agent.inc"
        ).read_text()
        miss_burst = read_cpp_definition("scenarios/mixed_action_pressure.cpp", "MixedScenario::execute_miss_burst")

        for contract in (
            "vector_transaction && !capacity_plus_one",
            "scalar_issue_limit = capacity_plus_one",
            "burst_lines.begin() + pressure_depth",
            "schedule_held_dcache_response_release_at",
            "const bool overflow_issued = vector_transaction",
            "transactions.at(pressure_depth)",
            "saturation_delta[0] = 1",
            "saturation_delta[1] = 1",
            "saturation_delta[2] = 1",
        ):
            self.assertIn(contract, miss_burst)
        self.assertLess(
            miss_burst.index("schedule_held_dcache_response_release_at"),
            miss_burst.index("const bool overflow_issued"),
        )
        for contract in (
            "after_ticks == 0",
            "a DCache held response release is already scheduled",
            "no complete held DCache response to schedule for release",
            "!scheduled_held_response_release_",
        ):
            self.assertIn(contract, dcache_agent)
        update = dcache_agent[
            dcache_agent.index("void update_after_tick()"):
            dcache_agent.index("bool ok() const")
        ]
        self.assertLess(
            update.index("captured_c_.reset();"),
            update.index("if (scheduled_held_response_release_)"),
        )
        self.assertIn(
            "memory_agent_.schedule_held_response_release_at(", environment
        )
        tick = (MEMBLOCK_ROOT / "cpp/environment/cycle.cpp").read_text().split(
            "auto Environment::Impl::tick(", 1)[1].split(
            "auto Environment::Impl::check_components(", 1)[0]
        self.assertLess(
            tick.index("dut_.step();"),
            tick.index("memory_agent_.update_after_tick();"),
        )
        adapter = read_cpp_source("environment/dut_adapter.cpp")
        for binding in (
            "void DutAdapter::step() { impl_->dut.Step(); }",
            "generated::drive_scalar_load_issue(impl_->dut, lane, item)",
            "generated::clear_scalar_load_issue_valid(impl_->dut, lane)",
            "generated::drive_vector_memory_issue(impl_->dut, lane, item)",
            "generated::clear_vector_memory_issue_valids(impl_->dut)",
        ):
            self.assertIn(binding, adapter)
        for issue_name, drive, clear in (
            (
                "auto Environment::Impl::issue_load(const LoadTransaction &transaction",
                "dut_.drive_scalar_load_issue",
                "dut_.clear_scalar_load_issue_valid",
            ),
            (
                "auto Environment::Impl::issue_vector(const VectorMemoryTransaction &transaction",
                "dut_.drive_vector_memory_issue",
                "dut_.clear_vector_memory_issue_valids",
            ),
        ):
            issue = environment[
                environment.index(issue_name):
                environment.index("\nauto Environment::Impl::", environment.index(issue_name) + 10)
            ]
            self.assertLess(issue.index(drive), issue.index("tick();"))
            self.assertLess(issue.index("if (ready)"), issue.index(clear))

    def test_constrained_random_preserves_atomic_serialization_contract(self) -> None:
        driver = read_cpp_source("memblock_main.cpp")
        memblock = (
            REPO_ROOT / "src/main/scala/xiangshan/mem/MemBlock.scala"
        ).read_text()
        concurrent_tail = read_cpp_definition("scenarios/mixed_concurrent.cpp", "MixedScenario::run_concurrent_windows")

        self.assertIn("atomics insts (LR/SC/AMO) will block the pipeline", memblock)
        self.assertIn("uses_concurrent_special_operations", concurrent_tail)
        self.assertIn("choose_concurrent_special_operation(random())", concurrent_tail)
        self.assertNotIn("issue_atomic", concurrent_tail)

    def test_constrained_random_recovers_hlvx_and_atomic_precise_errors(self) -> None:
        driver = read_cpp_source("scenarios/mixed_action_hypervisor.cpp")
        hypervisor_load = driver[
            driver.index("const bool physical_pma_execute_denied"):
            driver.index("const std::uint64_t expected_uncache", driver.index(
                "const bool physical_pma_execute_denied"
            ))
        ]
        atomic = read_cpp_definition("scenarios/mixed_action_atomic.cpp", "MixedScenario::execute_atomic")
        atomic_error = atomic[
            atomic.index('phase = *atomic_error_denied'):
            atomic.index(
                "} else if (atomic_family == RandomConstraints::atomic_amo)",
                atomic.index('phase = *atomic_error_denied'),
            )
        ]

        self.assertIn(
            "if (physical_pmp_denied || physical_pma_execute_denied)",
            hypervisor_load,
        )
        self.assertIn("recover_random_precise_error(", hypervisor_load)
        self.assertNotIn(
            "physical_pma_execute_denied) {\n"
            "                        completed_load = issue_load(transaction)",
            hypervisor_load,
        )
        self.assertRegex(atomic_error, r"environment\.redirect_after\(\s*transaction\.rob, transaction\.rob_flag, true\)")
        self.assertIn(":trap-recovery", atomic_error)
        self.assertIn("--rob_offset", atomic_error)

    def test_constrained_random_windows_use_full_shape_vector_instructions(self) -> None:
        driver = read_cpp_source("memblock_main.cpp")
        concurrent_tail = read_cpp_definition("scenarios/mixed_concurrent.cpp", "MixedScenario::run_concurrent_windows")

        for contract in (
            "choose_vector_shape(",
            "0U, concurrent_vector_load_flow_limit, true)",
            "1U, concurrent_vector_store_flow_limit, true)",
            "make_random_vector_uops(",
            "for (const auto &uop : vector_load_uops)",
            "for (const auto &uop : vector_store_uops)",
            "window_vectors.insert(",
            "auto vector_readback_uops = vector_store_uops",
            "issue_random_vector_instruction(vector_readback_uops)",
            "sample_concurrent_vector_shape(",
            "environment.run_until_load_complete(",
        ):
            self.assertIn(contract, concurrent_tail)
        self.assertNotIn("auto vector_load = make_vector(", concurrent_tail)
        self.assertNotIn("auto vector_store = make_vector(", concurrent_tail)
        self.assertNotIn("uop.sq_flag = false", concurrent_tail)
        self.assertRegex(concurrent_tail, r"make_random_vector_uops\(\s*false, window_base \+ 32, vector_load_shape, vector_corner\)")
        self.assertRegex(concurrent_tail, r"make_random_vector_uops\(\s*true, window_base \+ 0x100, vector_store_shape, vector_corner\)")
        self.assertIn(
            "uop.sq_flag = memblock::sq_pointer_flag(sq_offset)",
            concurrent_tail,
        )
        self.assertIn(
            ".lq_flag = memblock::lq_pointer_flag(lq_pointer)",
            driver,
        )
        self.assertIn(
            ".sq_flag = memblock::sq_pointer_flag(sq_pointer)",
            driver,
        )

    def test_indexed_unordered_uops_cover_both_issue_orders(self) -> None:
        driver = read_cpp_source("memblock_main.cpp")
        scenario = driver[
            driver.index("int run_vector_issue_order"):
            driver.index("int run_vector_addressing")
        ]

        self.assertIn('"name": "vector-issue-order"', driver)
        self.assertIn("VectorAddressingMode::indexed_unordered", scenario)
        self.assertIn("VectorAddressingMode::indexed_ordered", scenario)
        self.assertIn("case=masked-indexed-ordered", scenario)
        self.assertIn("const bool reverse = order != 0", scenario)
        self.assertIn("environment.expect_vector(transaction)", scenario)
        self.assertIn("run_until_vector_complete_with_replays", scenario)
        self.assertIn("run_until_lq_retired", scenario)

    def test_vector_cross_16_misalignment_advances_rob_head(self) -> None:
        driver = read_cpp_source("memblock_main.cpp")
        self.assertIn("requires_misaligned_head", driver)
        self.assertIn("((address & 0xfU) + element_bytes) > 16U", driver)
        self.assertIn("requires_store_pending", driver)
        self.assertIn("(address & (element_bytes - 1)) != 0", driver)
        self.assertIn("requires_store_pending))", driver)
        self.assertIn(".address = base + (index == 3 ? 0x1800 : 0x1803)", driver)
        self.assertIn("loads[index].index[8] = index == 4 ? 0xa0 : 0xa8", driver)

    def test_constrained_random_window_honors_scalar_misalignment_contract(self) -> None:
        driver = read_cpp_source("memblock_main.cpp")
        for contract in (
            "constrained_window_address",
            "constraints.misaligned_per_mille",
            "scalar_store_crosses_page",
            "(scalar_store.address & 0xfffU) + scalar_store_bytes > 0x1000U",
        ):
            self.assertIn(contract, driver)
        concurrent = read_cpp_definition("scenarios/mixed_concurrent.cpp", "MixedScenario::run_concurrent_windows")
        self.assertRegex(concurrent, r"environment\.set_rob_head\(\s*scalar_store\.rob, scalar_store\.rob_flag\)")
        self.assertRegex(concurrent, r"scalar_store, constrained_completion_timeout,\s*scalar_store_crosses_page")

        environment = read_cpp_source("memblock_env.hpp")
        for contract in (
            "bool hold_pending_store = false",
            "const auto clear_pending_store",
            "dut_.queues.backend.pendingst.set(std::uint64_t{1})",
        ):
            self.assertIn(contract, environment)
        self.assertIn(
            "queues.backend.pendingst = DrivenPort(&impl_->dut.io_ooo_to_mem_lsqio_pendingst, 1)",
            read_cpp_source("environment/dut_adapter.cpp"),
        )

        for contract in (
            "phase=translated-scalar-store",
            "run_until_store_tlb_misses",
            "translated_store, 8192, true",
            "translated_scalar_cross_page=1",
        ):
            self.assertIn(contract, driver)

    def test_cross_page_vector_stores_require_back_to_back_progress(self) -> None:
        driver = read_cpp_source("memblock_main.cpp")
        scenario = driver[
            driver.index("int run_misaligned_stores"):
            driver.index("int run_vector_issue_order")
        ]
        environment = read_cpp_source("memblock_env.hpp")

        for contract in (
            "cross_page_progress",
            "std::array<memblock::VectorMemoryTransaction, 2> progress_stores",
            "store, 16384, index == 0, true",
            "phase=cross-page-progress-store",
            "phase=cross-page-progress-readback",
            "cross_page_progress_stores=",
        ):
            self.assertIn(contract, scenario)
        self.assertIn("bool hold_pending_store = false", environment)
        self.assertIn("pulse_store_commit_keep_pending", environment)

    def test_scalar_misaligned_keeps_rar_pressure_behind_pending_splits(
        self,
    ) -> None:
        driver = read_cpp_source("memblock_main.cpp")
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

    def test_constrained_random_has_no_directed_prefix(self) -> None:
        execute = read_cpp_definition("scenarios/mixed_scheduler.cpp", "MixedScenario::execute")
        self.assertIn("initialize_run()", execute)
        self.assertNotIn("run_prefix()", execute)
        self.assertLess(execute.index("initialize_run()"), execute.index("prepare_tail()"))
        self.assertLess(execute.index("prepare_tail()"), execute.index("run_concurrent_windows()"))
        self.assertFalse((MEMBLOCK_ROOT / "cpp/scenarios/mixed_prefix.cpp").exists())
        self.assertNotIn(
            'phase = "dcache-dirty-pressure"',
            read_cpp_source("scenarios/mixed_scheduler.cpp"),
        )

    def test_random_mmio_store_replays_until_tlb_hit(self) -> None:
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")
        self.assertIn("bool issue_store_address_until_tlb_hit(", environment)
        self.assertIn(
            "environment.issue_store_address_until_tlb_hit(", driver
        )

    def test_make_targets_forward_seed_and_action_counts(self) -> None:
        makefile = read_makefile()
        self.assertIn("--seed $(or $(SEED),1)", makefile)
        self.assertIn("--actions $(or $(ACTIONS),16384)", makefile)
        self.assertIn("ACTIONS ?= 16384", makefile)
        benchmark_rule = makefile[
            makefile.index("benchmark-tests:"):makefile.index("analyze-spec-counters:")
        ]
        self.assertIn("BENCHMARK_TIMEOUT_SECONDS ?= 7200", makefile)
        self.assertIn(
            "--timeout-seconds $(BENCHMARK_TIMEOUT_SECONDS)", benchmark_rule
        )
        self.assertNotIn("--rtl-metadata", benchmark_rule)
        self.assertNotIn("$(CONTROLLER_FILES)", benchmark_rule)
        self.assertNotIn("--rtl-metadata $(FROZEN_RTL_METADATA)", makefile)
        self.assertNotIn("$${SEED:-1}", makefile)
        self.assertNotIn("$${ACTIONS:-", makefile)
        final_acceptance = makefile[
            makefile.index("final-acceptance:"):makefile.index("\nunit:")
        ]
        for target in (
            "$(MAKE) unit",
            "$(MAKE) benchmark-tests",
            "$(MAKE) known-bug-sentinel",
            "$(MAKE) final-regression",
            "$(MAKE) verify-final-results",
            "$(MAKE) endurance-regression",
            "$(MAKE) verify-endurance-results",
        ):
            self.assertIn(target, final_acceptance)



    def test_random_final_store_observation_and_protocol_lifecycle(self) -> None:
        # Static integration-token inventory, not a behavioral regression:
        # the directed CBO/hypervisor/protocol cases and mixed campaigns are
        # the behavioral evidence for these paths.
        environment = read_cpp_source("memblock_env.hpp")
        driver = read_cpp_source("memblock_main.cpp")

        for contract in (
            "set_final_observation_tracking(true)",
            "observe_final_writes()",
            "track_final_observation_bytes(",
            "final_observation_words()",
            "final_observation_alias_skips()",
            "DCache TileLink E GrantAck before first D acceptance",
            "DCache TileLink sink reused before GrantAck",
            "uncache TileLink A reused an active source before D",
            "uncache TileLink D completed an inactive source",
            "PTW TileLink A reused an active source before D",
            "PTW TileLink D completed an inactive source",
            "hypervisor-cacheable-store-readback",
            "add_final_observation_observed_words",
            "final_observation_observed_words",
            "following_store",
            "atomic_probe_persistence",
            "multibeat source mismatch",
            "invalid DCache Release identity",
        ):
            self.assertIn(contract, environment + driver)
        for skipped in (
            "transaction.cacheable = false;",
            "transaction.cacheable = !(final_nc || final_io);",
        ):
            self.assertIn(skipped, driver)
        # The final sweep must not count its own readbacks as actions or
        # bypass the independent reference image.
        self.assertIn("environment.memory().read_u64(word)", driver)
        self.assertIn("!observe_final_writes()", driver)

if __name__ == "__main__":
    unittest.main()
