from __future__ import annotations

import pytest

from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationPtwResponseOverride,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.support import PmpPmaConfig, fold_pc
from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


_CFVEC_EXCEPTION_BITS = (1, 2, 12, 19, 20)
_NC_TIMING_SIGNALS = {
    "nc_pending": (
        "Frontend_top.Frontend.u_frontend_funcov_hub.u_nc.nc_pending",
    ),
    "backend_redirect": (
        "Frontend_top.Frontend.inner_ftq.backendRedirect_valid",
    ),
    "ifu_flush": (
        "Frontend_top.Frontend.inner_ifu.s2_flush",
    ),
    "uncache_resp_valid": (
        "Frontend_top.Frontend.inner_ifu._uncacheUnit_io_resp_valid",
    ),
    "uncache_state": (
        "Frontend_top.Frontend.inner_ifu.uncacheUnit.uncacheState",
    ),
    "to_ibuffer_valid": (
        "Frontend_top.Frontend._inner_ifu_io_toIBuffer_valid",
    ),
    "to_ibuffer_ready": (
        "Frontend_top.Frontend._inner_ibuffer_io_in_ready",
    ),
    "to_ibuffer_enq": (
        "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_enqEnable",
    ),
    "exception_cross_page": (
        "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_exceptionCrossPage",
    ),
    "exception_type": (
        "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_exceptionType_value",
    ),
    "prev_end_half_rvi": (
        "Frontend_top.Frontend.inner_ifu.s2_prevEndIsHalfRvi",
    ),
    "wb_path_valid": (
        "Frontend_top.Frontend.inner_ifu.wbValid",
    ),
    "wb_redirect": (
        "Frontend_top.Frontend.inner_ifu.wbRedirect_valid",
    ),
}


def _register_nc_timing_observer(env) -> list[dict]:
    samples: list[dict] = []

    def capture(cycle, current_env) -> None:
        sample = {
            name: uncache._require_first_dut_signal(current_env, signal_names)
            for name, signal_names in _NC_TIMING_SIGNALS.items()
        }
        sample["cycle"] = int(cycle)
        sample["tl_a_valid"] = int(current_env.uncache_if.a_valid.value)
        samples.append(sample)

    env.register_cycle_observer(capture)
    return samples


def _encode_jal_x0(offset: int) -> int:
    assert int(offset) % 2 == 0
    assert -(1 << 20) <= int(offset) < (1 << 20)
    imm = int(offset) & 0x1FFFFF
    return (
        (((imm >> 20) & 0x1) << 31)
        | (((imm >> 1) & 0x3FF) << 21)
        | (((imm >> 11) & 0x1) << 20)
        | (((imm >> 12) & 0xFF) << 12)
        | 0x6F
    )


def _s2_has_taken_prediction_at(env, target_pc: int) -> bool:
    for slot in range(35):
        prefix = f"Frontend_top.Frontend.inner_ifu.s2_alignedInstrVec_{slot}_"
        if uncache._require_first_dut_signal(env, (prefix + "valid",)) != 1:
            continue
        pc = uncache._require_first_dut_signal(
            env,
            (f"Frontend_top.Frontend.inner_ifu.s2_alignedInstrPcVec_{slot}_addr",),
        ) << 1
        if (
            int(pc) == int(target_pc)
            and uncache._require_first_dut_signal(env, (prefix + "isPredTaken",)) == 1
        ):
            return True
    return False


def _wait_for_taken_prediction(env, target_pc: int, *, max_cycles: int) -> bool:
    for _ in range(int(max_cycles)):
        if _s2_has_taken_prediction_at(env, target_pc):
            return True
        env.step(1)
    return _s2_has_taken_prediction_at(env, target_pc)


def _register_cfvec_exception_observer(env) -> list[dict]:
    records: list[dict] = []

    def capture(cycle, current_env) -> None:
        observe = current_env.backend_observe_if
        for slot in range(8):
            if int(observe.cfvec_valid[slot].value) != 1:
                continue
            bits = tuple(
                bit
                for bit in _CFVEC_EXCEPTION_BITS
                if int(observe.cfvec_exception_vec[slot][bit].value) != 0
            )
            if bits:
                records.append(
                    {
                        "cycle": int(cycle),
                        "slot": int(slot),
                        "pc": int(observe.cfvec_pc[slot].value),
                        "foldpc": int(observe.cfvec_foldpc[slot].value),
                        "cross_page": int(
                            observe.cfvec_cross_page_ipf_fix[slot].value
                        ),
                        "bits": bits,
                    }
                )

    env.register_cycle_observer(capture)
    return records


def _configure_exec_attrs_for_pages(env, pages: tuple[int, ...]) -> None:
    for index, page in enumerate(pages):
        env.write_pmp_entry(
            index,
            PmpPmaConfig(match="napot", read=True, write=True, execute=True),
            int(page),
            size=uncache._SV39_PAGE_SIZE,
            settle_cycles=4,
        )
        env.write_pma_entry(
            index,
            PmpPmaConfig(
                match="napot",
                read=True,
                write=True,
                execute=True,
                cacheable=True,
                atomic=True,
            ),
            int(page),
            size=uncache._SV39_PAGE_SIZE,
            settle_cycles=4,
        )


def _nc_cross_page_fault_scenario(
    *, fault: str, scenario_index: int = 0
) -> tuple[TranslationScenario, int, int]:
    # End on PTE sector lane 7 so the second page requires a new PTW response.
    page_pair_offset = 2 * int(scenario_index) * uncache._SV39_PAGE_SIZE
    cross_page_va = (
        uncache._NORMAL_BASE
        + (8 + 8 * int(scenario_index)) * uncache._SV39_PAGE_SIZE
        - 2
    )
    cross_page_pa = (
        uncache._NORMAL_PHYS_BASE
        + page_pair_offset
        + uncache._SV39_PAGE_SIZE
        - 2
    )
    first_page_pa = cross_page_pa & ~(uncache._SV39_PAGE_SIZE - 1)
    payload = int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")
    payload += int(uncache._CNOP).to_bytes(2, "little") * 64
    pmp_entries = tuple(
        TranslationPmpPmaEntry(
            kind="pmp",
            index=page,
            config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
            addr=first_page_pa + page * uncache._SV39_PAGE_SIZE,
            size=uncache._SV39_PAGE_SIZE,
        )
        for page in range(2)
    )
    pma_entries = tuple(
        TranslationPmpPmaEntry(
            kind="pma",
            index=page,
            config=PmpPmaConfig(
                match="napot",
                read=True,
                write=True,
                execute=True,
                cacheable=True,
                atomic=True,
            ),
            addr=first_page_pa + page * uncache._SV39_PAGE_SIZE,
            size=uncache._SV39_PAGE_SIZE,
        )
        for page in range(2)
    )
    if fault == "gpf":
        gpa = (
            uncache._NORMAL_PHYS_BASE
            + 0x20000
            + page_pair_offset
            + uncache._SV39_PAGE_SIZE
            - 2
        )
        scenario = TranslationScenario(
            scenario_id=f"nc-cross-page-second-gpf-{scenario_index}",
            va=cross_page_va,
            gpa=gpa,
            pa=cross_page_pa,
            payload=payload,
            page_count=2,
            s2xlate=3,
            priv_virt=1,
            s1_pte=TranslationPte(asid=5, vmid=7, pbmt=uncache._PBMT_NC),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            ptw_response_overrides=(
                TranslationPtwResponseOverride(
                    vpn=(cross_page_va >> 12) + 1,
                    s2xlate=3,
                    patch=(("s2_gpf", 1),),
                ),
                TranslationPtwResponseOverride(
                    vpn=(cross_page_va >> 12) + 1,
                    s2xlate=3,
                    get_gpa=1,
                    patch=(("s2_gpf", 1),),
                ),
            ),
            expected_path="fault",
            expected_result="guest_fault",
            pmp_entries=pmp_entries,
            pma_entries=pma_entries,
        )
    else:
        patch_name = "s1_pf" if fault == "pf" else "s1_af"
        scenario = TranslationScenario(
            scenario_id=f"nc-cross-page-second-{fault}-{scenario_index}",
            va=cross_page_va,
            pa=cross_page_pa,
            payload=payload,
            page_count=2,
            s1_pte=TranslationPte(pbmt=uncache._PBMT_NC),
            ptw_response_overrides=(
                TranslationPtwResponseOverride(
                    vpn=(cross_page_va >> 12) + 1,
                    s2xlate=0,
                    patch=((patch_name, 1),),
                ),
            ),
            expected_path="fault",
            expected_result="page_fault" if fault == "pf" else "access_fault",
            pmp_entries=pmp_entries,
            pma_entries=pma_entries,
        )
    return scenario, cross_page_va, cross_page_pa


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_tl_a_backpressure_holds_payload_until_fire(env):
    """Exercise NC TL-A stall/stability/release with a non-MMIO PBMT.NC page."""
    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        pbmt=uncache._PBMT_NC,
        instr_count=64,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_attrs_for_mapping(env, mapping)
    env.uncache_agent.set_a_ready(0)
    uncache._force_redirect_to(env, mapping.vaddr)

    assert uncache._wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert uncache._wait_for_uncache_a_valid_addr(
        env, mapping.paddr, max_cycles=6000
    ), {
        "mapping": mapping,
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }

    stalled_addr = int(env.uncache_if.a_bits_address.value)
    req_before = int(env.uncache_agent.get_stats().get("req_count", 0))
    assert int(env.uncache_if.a_valid.value) == 1
    assert int(env.uncache_if.a_ready.value) == 0
    assert stalled_addr == int(mapping.paddr)

    env.step(8)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == req_before
    assert int(env.uncache_if.a_valid.value) == 1
    assert int(env.uncache_if.a_bits_address.value) == stalled_addr

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_uncache_req(env, max_cycles=6000) > req_before
    assert uncache._wait_for_uncache_resp(env, max_cycles=6000) > 0
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) > req_before
    assert int(env.uncache_agent.get_stats().get("resp_count", 0)) > 0
    assert not env.memory.is_mmio(mapping.paddr)
    assert mapping.paddr in env.uncache_agent.get_stats().get("request_addrs", [])
    assert uncache._wait_for_observed_pc(env, mapping.vaddr, max_cycles=12000)
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "fault,expected_exception_bit",
    [
        pytest.param({"corrupt": 1}, 19, id="corrupt-hwe"),
        pytest.param({"corrupt": 1, "denied": 1}, 1, id="denied-iaf"),
    ],
)
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_d_response_fault_reports_exception(env, fault, expected_exception_bit):
    """Exercise NC D corrupt/denied responses and require an exception-marked cfVec."""
    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        pbmt=uncache._PBMT_NC,
        instr_count=64,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_attrs_for_mapping(env, mapping)
    env.uncache_agent.inject_response_fault_at(mapping.paddr, **fault)
    exception_records = _register_cfvec_exception_observer(env)
    uncache._force_redirect_to(env, mapping.vaddr)

    assert uncache._wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert uncache._wait_for_request_addr(env, mapping.paddr, max_cycles=6000), {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
    }
    assert uncache._wait_for_uncache_resp(env, max_cycles=6000) > 0
    assert uncache._wait_for_monitor_exception(env, max_cycles=6000)

    stats = env.uncache_agent.get_stats()
    assert not env.memory.is_mmio(mapping.paddr)
    assert int(stats.get("req_count", 0)) > 0
    assert int(stats.get("resp_count", 0)) > 0
    assert int(stats.get("corrupt_resp_count", 0)) == int(fault.get("corrupt", 0))
    assert int(stats.get("denied_resp_count", 0)) == int(fault.get("denied", 0))
    assert env.monitor.exception_mark_count > 0
    assert any(
        record["pc"] == int(mapping.vaddr)
        and record["bits"] == (int(expected_exception_bit),)
        for record in exception_records
    ), {
        "expected_pc": hex(int(mapping.vaddr)),
        "expected_exception_bit": int(expected_exception_bit),
        "exception_records": exception_records,
    }
    assert all(record["pc"] != 0 for record in exception_records), exception_records
    assert not env.monitor.get_errors()


@pytest.mark.parametrize("is_rvc", [pytest.param(False, id="rvi"), pytest.param(True, id="rvc")])
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_8b_tail_delivery_uses_correct_second_beat_policy(env, tmp_path, is_rvc):
    """Exercise NC 8B-tail RVI resend and RVC no-resend at physical offset ...e."""
    payload = bytearray(int(uncache._CNOP).to_bytes(2, "little") * 7)
    if is_rvc:
        payload.extend(int(uncache._CNOP).to_bytes(2, "little"))
    else:
        payload.extend(int(uncache._ADDI_X0_X0_0).to_bytes(4, "little"))
    payload.extend(int(uncache._CNOP).to_bytes(2, "little") * 128)
    bin_path = tmp_path / ("nc_8b_tail_rvc.bin" if is_rvc else "nc_8b_tail_rvi.bin")
    bin_path.write_bytes(bytes(payload))

    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        pbmt=uncache._PBMT_NC,
        bin_path=bin_path,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr + 0xE)
    uncache._configure_exec_attrs_for_mapping(env, mapping)
    uncache._force_redirect_to(env, mapping.vaddr + 0xE)

    first_beat = mapping.paddr + 0x8
    second_beat = mapping.paddr + 0x10
    assert uncache._wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=6000), {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
    }
    assert uncache._wait_for_observed_pc(env, mapping.vaddr + 0xE, max_cycles=12000), {
        "mapping": mapping,
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-16:]],
        "uncache": env.uncache_agent.get_stats(),
    }

    observed = next(
        obs for obs in env.monitor.observations if int(obs.pc) == int(mapping.vaddr + 0xE)
    )
    assert bool(observed.is_rvc) is bool(is_rvc)
    if is_rvc:
        stats_at_delivery = env.uncache_agent.get_stats()
        assert stats_at_delivery.get("request_addrs", []).count(first_beat) == 1
        assert second_beat not in stats_at_delivery.get("request_addrs", [])
    else:
        assert uncache._wait_for_request_addr(env, second_beat, max_cycles=6000), {
            "mapping": mapping,
            "uncache": env.uncache_agent.get_stats(),
        }
        assert env.uncache_agent.get_stats().get("request_addrs", []).count(first_beat) == 1
        assert env.uncache_agent.get_stats().get("request_addrs", []).count(second_beat) == 1
        assert int(observed.instr) == int(uncache._ADDI_X0_X0_0)
        assert not bool(observed.is_rvc)
    assert not env.memory.is_mmio(mapping.paddr)
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "instruction,branch_kind",
    [
        pytest.param(0x00001063, "branch", id="branch"),  # BNE x0, x0, +0 (not taken)
        pytest.param(uncache._JAL_X0_PLUS_4, "jump", id="jump"),
    ],
)
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_cfi_instruction_is_delivered_with_control_flow_type(env, tmp_path, instruction, branch_kind):
    """Exercise NC branch/jump delivery without relying on a Python coverage key."""
    payload = bytearray(int(instruction).to_bytes(4, "little"))
    payload.extend(int(uncache._CNOP).to_bytes(2, "little") * 128)
    bin_path = tmp_path / f"nc_{branch_kind}.bin"
    bin_path.write_bytes(bytes(payload))

    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        pbmt=uncache._PBMT_NC,
        bin_path=bin_path,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_attrs_for_mapping(env, mapping)
    uncache._force_redirect_to(env, mapping.vaddr)

    assert uncache._wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert uncache._wait_for_request_addr(env, mapping.paddr, max_cycles=6000), {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
    }
    assert uncache._wait_for_observed_pc(env, mapping.vaddr, max_cycles=12000), {
        "mapping": mapping,
        "observed": [
            (int(obs.pc), int(obs.instr), bool(obs.is_rvc))
            for obs in env.monitor.observations[-16:]
        ],
        "uncache": env.uncache_agent.get_stats(),
    }
    observed = next(obs for obs in env.monitor.observations if int(obs.pc) == int(mapping.vaddr))
    assert int(observed.instr) == int(instruction)
    assert not bool(observed.is_rvc)
    assert env.branch_checker.get_stats()["by_type"][branch_kind] >= 1
    assert not env.memory.is_mmio(mapping.paddr)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_mixed_beat_types_preserve_delivery_order_and_pc_progress(env, tmp_path):
    """Exercise all missing RVI/RVC transitions across bounded backend backpressure."""
    payload = bytearray()
    payload.extend(int(uncache._ADDI_X0_X0_0).to_bytes(4, "little"))
    payload.extend(int(uncache._CNOP).to_bytes(2, "little") * 2)
    payload.extend(int(uncache._ADDI_X0_X0_0).to_bytes(4, "little"))
    payload.extend(int(uncache._CNOP).to_bytes(2, "little") * 128)
    bin_path = tmp_path / "nc_mixed_beat_types.bin"
    bin_path.write_bytes(bytes(payload))

    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        pbmt=uncache._PBMT_NC,
        bin_path=bin_path,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_attrs_for_mapping(env, mapping)
    env.uncache_agent.configure(latency=16, mmio_latency=16)
    uncache._force_redirect_to(env, mapping.vaddr)

    representative = {
        mapping.vaddr + 0x00: False,
        mapping.vaddr + 0x04: True,
        mapping.vaddr + 0x06: True,
        mapping.vaddr + 0x08: False,
    }
    assert uncache._wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert uncache._wait_for_uncache_req_count(env, 1, max_cycles=12000), {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
    }
    assert uncache._wait_for_observed_pc_sequence(
        env, list(representative), max_cycles=12000
    ), {
        "mapping": mapping,
        "observed": [
            (int(obs.pc), int(obs.instr), bool(obs.is_rvc))
            for obs in env.monitor.observations[-32:]
        ],
        "uncache": env.uncache_agent.get_stats(),
    }

    observed = {
        int(obs.pc): obs
        for obs in env.monitor.observations
        if int(obs.pc) in representative
    }
    for pc, is_rvc in representative.items():
        assert bool(observed[pc].is_rvc) is bool(is_rvc)
        assert int(observed[pc].instr) == int(uncache._ADDI_X0_X0_0)
    ordered = [
        int(obs.pc)
        for obs in env.monitor.observations
        if int(obs.pc) in representative
    ]
    assert ordered[: len(representative)] == list(representative), ordered
    stats = env.uncache_agent.get_stats()
    assert int(stats.get("req_count", 0)) >= len(representative), stats
    assert int(stats.get("resp_count", 0)) >= len(representative), stats
    assert not env.memory.is_mmio(mapping.paddr)
    assert not env.monitor.get_errors()


@pytest.mark.parametrize("is_rvc", [pytest.param(False, id="rvi"), pytest.param(True, id="rvc")])
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_page_tail_delivery_uses_correct_next_page_policy(env, tmp_path, is_rvc):
    """Exercise a real NC page tail at the sampler-compatible physical ...ffe offset."""
    payload = bytearray(
        int(uncache._CNOP).to_bytes(2, "little")
        * (uncache._SV39_PAGE_SIZE // 2 + 128)
    )
    if not is_rvc:
        payload[uncache._SV39_PAGE_SIZE - 2 : uncache._SV39_PAGE_SIZE + 2] = int(
            uncache._ADDI_X0_X0_0
        ).to_bytes(4, "little")
    bin_path = tmp_path / ("nc_page_tail_rvc.bin" if is_rvc else "nc_page_tail_rvi.bin")
    bin_path.write_bytes(bytes(payload))

    physical_pages = (
        uncache._NORMAL_PHYS_BASE,
        uncache._NORMAL_PHYS_BASE + uncache._SV39_PAGE_SIZE,
    )
    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr_pages=physical_pages,
        pbmt=uncache._PBMT_NC,
        bin_path=bin_path,
    )
    if is_rvc:
        env.page_table.map_page(
            (mapping.vaddr >> 12) + 1,
            mapping.paddr_pages[1] >> 12,
            v=1,
            r=1,
            x=1,
            pbmt=uncache._PBMT_PMA,
        )
    tail_vaddr = mapping.vaddr + uncache._SV39_PAGE_SIZE - 2
    tail_paddr = mapping.paddr + uncache._SV39_PAGE_SIZE - 2
    first_beat = tail_paddr & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    next_page = mapping.paddr_pages[1]

    env.uncache_agent.configure(latency=16, mmio_latency=16)
    uncache._initialize_sv39_fetch(env, reset_vector=tail_vaddr)
    _configure_exec_attrs_for_pages(env, mapping.paddr_pages)
    timing_samples = _register_nc_timing_observer(env)
    uncache._force_redirect_to(env, tail_vaddr)

    assert uncache._wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=6000), {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
    }
    if not is_rvc:
        assert uncache._wait_for_request_addr(env, next_page, max_cycles=12000), {
            "mapping": mapping,
            "uncache": env.uncache_agent.get_stats(),
        }
    assert uncache._wait_for_observed_pc(env, tail_vaddr, max_cycles=12000), {
        "mapping": mapping,
        "observed": [
            (int(obs.pc), int(obs.instr), bool(obs.is_rvc))
            for obs in env.monitor.observations[-16:]
        ],
        "uncache": env.uncache_agent.get_stats(),
    }

    observed = next(obs for obs in env.monitor.observations if int(obs.pc) == int(tail_vaddr))
    stats_at_delivery = env.uncache_agent.get_stats()
    assert not env.memory.is_mmio(tail_paddr)
    assert stats_at_delivery.get("request_addrs", []).count(first_beat) == 1
    assert bool(observed.is_rvc) is bool(is_rvc)
    if is_rvc:
        assert next_page not in stats_at_delivery.get("request_addrs", [])
        assert uncache._wait_for_icache_req(env, max_cycles=6000) > 0
        assert uncache._wait_for_observed_pc(
            env, mapping.vaddr + uncache._SV39_PAGE_SIZE, max_cycles=12000
        ), {
            "mapping": mapping,
            "icache": env.icache_agent.get_stats(),
            "uncache": env.uncache_agent.get_stats(),
            "observed": [hex(int(obs.pc)) for obs in env.monitor.observations[-16:]],
        }
        next_observed = next(
            obs
            for obs in env.monitor.observations
            if int(obs.pc) == int(mapping.vaddr + uncache._SV39_PAGE_SIZE)
        )
        assert bool(next_observed.is_rvc)
        assert int(next_observed.instr) == int(uncache._ADDI_X0_X0_0)
        assert next_page not in env.uncache_agent.get_stats().get("request_addrs", [])
    else:
        assert next_page in stats_at_delivery.get("request_addrs", [])
        assert stats_at_delivery.get("request_addrs", []).count(next_page) == 1
        assert int(observed.instr) == int(uncache._ADDI_X0_X0_0)
        resumed = [
            sample
            for sample in timing_samples
            if sample["prev_end_half_rvi"] == 1
            and sample["to_ibuffer_valid"] == 1
            and sample["to_ibuffer_ready"] == 1
            and int(sample["to_ibuffer_enq"]).bit_count() == 1
        ]
        assert resumed, {
            "reason": "cross-page RVI never resumed from the saved first half",
            "tail_vaddr": hex(int(tail_vaddr)),
            "timing_tail": timing_samples[-32:],
        }
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_page_tail_denied_response_reports_exact_instruction_access_fault(env, tmp_path):
    """Reject a denied first-page tail response without an illegal resend."""
    payload = bytearray(
        int(uncache._CNOP).to_bytes(2, "little")
        * (uncache._SV39_PAGE_SIZE // 2 + 128)
    )
    payload[uncache._SV39_PAGE_SIZE - 2 : uncache._SV39_PAGE_SIZE + 2] = int(
        uncache._ADDI_X0_X0_0
    ).to_bytes(4, "little")
    bin_path = tmp_path / "nc_page_tail_denied.bin"
    bin_path.write_bytes(bytes(payload))

    physical_pages = (
        uncache._NORMAL_PHYS_BASE,
        uncache._NORMAL_PHYS_BASE + uncache._SV39_PAGE_SIZE,
    )
    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE + 7 * uncache._SV39_PAGE_SIZE,
        paddr_pages=physical_pages,
        pbmt=uncache._PBMT_NC,
        bin_path=bin_path,
    )
    tail_vaddr = mapping.vaddr + uncache._SV39_PAGE_SIZE - 2
    tail_paddr = mapping.paddr + uncache._SV39_PAGE_SIZE - 2
    first_beat = tail_paddr & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    next_page = mapping.paddr_pages[1]

    uncache._initialize_sv39_fetch(env, reset_vector=tail_vaddr)
    _configure_exec_attrs_for_pages(env, mapping.paddr_pages)
    env.uncache_agent.inject_response_fault_at(first_beat, corrupt=1, denied=1)
    exception_records = _register_cfvec_exception_observer(env)
    uncache._force_redirect_to(env, tail_vaddr)

    assert uncache._wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=6000), {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
    }
    assert uncache._wait_for_uncache_resp(env, max_cycles=6000) > 0
    assert uncache._wait_for_monitor_exception(env, max_cycles=6000)

    stats = env.uncache_agent.get_stats()
    assert int(stats.get("corrupt_resp_count", 0)) == 1
    assert int(stats.get("denied_resp_count", 0)) == 1
    assert next_page not in stats.get("request_addrs", [])
    assert any(
        record["pc"] == int(tail_vaddr) and record["bits"] == (1,)
        for record in exception_records
    ), {
        "expected_pc": hex(int(tail_vaddr)),
        "exception_records": exception_records,
    }
    assert all(record["pc"] != 0 for record in exception_records)
    assert not env.memory.is_mmio(first_beat)
    assert not env.monitor.get_errors()


@pytest.mark.parametrize("fault", ["pf", "gpf", "af"])
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_cross_page_second_page_translation_fault_has_exact_pc(env, fault):
    """Require a PBMT.NC cross-page fault to retain its original virtual PC."""
    scenario, cross_page_va, cross_page_pa = _nc_cross_page_fault_scenario(fault=fault)
    env.uncache_agent.configure(latency=16, mmio_latency=16)
    uncache._initialize_sv39_fetch(env, reset_vector=cross_page_va)
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(cross_page_va)
    exception_records = _register_cfvec_exception_observer(env)
    timing_samples = _register_nc_timing_observer(env)
    env.arm_translation_scenario(state)
    uncache._force_redirect_to(env, cross_page_va)

    first_beat = cross_page_pa & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=12000), {
        "fault": fault,
        "uncache": env.uncache_agent.get_stats(),
        "ptw": env.ptw_agent.get_stats(),
    }
    for _ in range(12000):
        active = env.translation_oracle.get_active()
        if active is not None and active.get("fault_seen"):
            break
        env.step(1)

    active = env.translation_oracle.get_active()
    assert int(env.ptw_agent.get_stats().get("response_override_hit_count", 0)) >= 1, {
        "fault": fault,
        "ptw": env.ptw_agent.get_stats(),
    }
    assert active is not None and active.get("fault_seen"), {
        "fault": fault,
        "active": active,
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    assert active["expected_fault"] == {
        "pf": "instruction_page_fault",
        "gpf": "instruction_guest_page_fault",
        "af": "instruction_access_fault",
    }[fault]
    expected_exception_bit = {"pf": 12, "gpf": 20, "af": 1}[fault]
    assert uncache._wait_for_monitor_exception(env, max_cycles=12000), {
        "fault": fault,
        "active": active,
        "observed": [int(obs.pc) for obs in env.monitor.observations[-16:]],
    }
    assert any(
        record["pc"] == int(cross_page_va)
        and record["bits"] == (expected_exception_bit,)
        for record in exception_records
    ), {
        "fault": fault,
        "expected_pc": hex(int(cross_page_va)),
        "expected_exception_bit": expected_exception_bit,
        "exception_records": exception_records,
    }
    assert any(int(obs.pc) == int(cross_page_va) for obs in env.monitor.observations), {
        "fault": fault,
        "expected_pc": hex(int(cross_page_va)),
        "observed": [hex(int(obs.pc)) for obs in env.monitor.observations[-16:]],
    }
    assert all(int(obs.pc) != 0 for obs in env.monitor.observations)
    assert env.monitor.exception_mark_count > 0
    expected_exception_type = {"pf": 1, "gpf": 2, "af": 3}[fault]
    cross_page_samples = [
        sample
        for sample in timing_samples
        if sample["prev_end_half_rvi"] == 1
        and sample["to_ibuffer_valid"] == 1
        and sample["exception_cross_page"] == 1
        and sample["exception_type"] == expected_exception_type
    ]
    assert cross_page_samples, {
        "fault": fault,
        "expected_exception_type": expected_exception_type,
        "timing_tail": timing_samples[-32:],
    }
    assert not env.memory.is_mmio(first_beat)
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1127")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_cross_page_second_page_fault_matrix_keeps_original_identity(env):
    fault_matrix = ("pf", "gpf", "af")
    scenarios = [
        _nc_cross_page_fault_scenario(fault=fault, scenario_index=index)
        for index, fault in enumerate(fault_matrix)
    ]
    env.uncache_agent.configure(latency=16, mmio_latency=16)
    uncache._initialize_sv39_fetch(env, reset_vector=scenarios[0][1])
    exception_records = _register_cfvec_exception_observer(env)

    for index, (fault, scenario_info) in enumerate(zip(fault_matrix, scenarios)):
        scenario, cross_page_va, cross_page_pa = scenario_info
        state = TranslationScenarioBuilder(env).build(scenario)
        env.monitor.clear()
        env.monitor.set_expected_pc(cross_page_va)
        env.arm_translation_scenario(state)
        exception_cursor = len(exception_records)
        request_cursor = len(env.uncache_agent.get_stats().get("request_addrs", []))
        uncache._force_redirect_to(env, cross_page_va)

        first_beat = cross_page_pa & ~(uncache._UNCACHE_BEAT_BYTES - 1)
        for _ in range(12000):
            new_requests = env.uncache_agent.get_stats().get(
                "request_addrs", []
            )[request_cursor:]
            if first_beat in new_requests:
                break
            env.step(1)
        else:
            raise AssertionError(
                {
                    "fault": fault,
                    "first_beat": hex(first_beat),
                    "uncache": env.uncache_agent.get_stats(),
                    "ptw": env.ptw_agent.get_stats(),
                }
            )

        for _ in range(12000):
            active = env.translation_oracle.get_active()
            if active is not None and active.get("fault_seen"):
                break
            env.step(1)
        active = env.translation_oracle.get_active()
        assert active is not None and active.get("fault_seen"), {
            "fault": fault,
            "active": active,
            "ptw": env.ptw_agent.get_stats(),
        }

        for _ in range(6000):
            active = env.translation_oracle.get_active()
            expected_keys = {
                (
                    int(request["vpn"]),
                    int(request["s2xlate"]),
                    int(request["get_gpa"]),
                )
                for request in active["expected_ptw_requests"]
            }
            responded_keys = {
                tuple(int(value) for value in key)
                for key in active["responded_ptw_request_keys"]
            }
            if expected_keys.issubset(responded_keys):
                break
            env.step(1)
        else:
            raise AssertionError({"fault": fault, "active": active})

        expected_exception_bit = {"pf": 12, "gpf": 20, "af": 1}[fault]
        phase_exception_records = exception_records[exception_cursor:]
        assert any(
            record["bits"] == (expected_exception_bit,)
            and record["cross_page"] == 1
            and (
                record["pc"] == int(cross_page_va)
                or (
                    record["pc"] == 0
                    and record["foldpc"] == fold_pc(cross_page_va)
                )
            )
            for record in phase_exception_records
        ), {
            "fault": fault,
            "expected_pc": hex(cross_page_va),
            "records": phase_exception_records,
        }

        new_requests = env.uncache_agent.get_stats().get(
            "request_addrs", []
        )[request_cursor:]
        assert first_beat in new_requests
        assert (cross_page_pa + 2) not in new_requests
        assert env.assert_translation_scenario()["error_count"] == 0
        assert not env.monitor.get_errors()
        if index < len(fault_matrix) - 1:
            assert not env.functional_coverage.key_hit(
                "ifu_instruncache_owner_v3", "instruncache_leaf_034"
            )
        env.translation_oracle.disarm()

    assert env.functional_coverage.key_hit(
        "ifu_instruncache_owner_v3", "instruncache_leaf_034"
    )


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_pmp_execute_denied_reports_exact_instruction_access_fault(env):
    """Reject a PBMT.NC fetch before issuing TL-A when PMP denies execute."""
    scenario = TranslationScenario(
        scenario_id="nc-pbmt-nc-pmp-execute-denied",
        va=uncache._NORMAL_BASE,
        pa=uncache._NORMAL_PHYS_BASE,
        payload=int(uncache._CNOP).to_bytes(2, "little") * 32,
        s1_pte=TranslationPte(pbmt=uncache._PBMT_NC),
        expected_path="fault",
        expected_result="access_fault",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(
                    match="napot", read=True, write=True, execute=False
                ),
                addr=uncache._NORMAL_PHYS_BASE,
                size=uncache._SV39_PAGE_SIZE,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=uncache._NORMAL_PHYS_BASE,
                size=uncache._SV39_PAGE_SIZE,
            ),
        ),
    )
    uncache._initialize_sv39_fetch(env, reset_vector=scenario.va)
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    exception_records = _register_cfvec_exception_observer(env)
    env.arm_translation_scenario(state)
    uncache._force_redirect_to(env, scenario.va)

    for _ in range(6000):
        active = env.translation_oracle.get_active()
        if active is not None and active.get("fault_seen"):
            break
        env.step(1)

    active = env.translation_oracle.get_active()
    assert active is not None and active.get("fault_seen"), {
        "active": active,
        "ptw": env.ptw_agent.get_stats(),
    }
    assert active["expected_fault"] == "instruction_access_fault"
    assert uncache._wait_for_monitor_exception(env, max_cycles=6000)
    assert any(
        record["pc"] == int(scenario.va) and record["bits"] == (1,)
        for record in exception_records
    ), {
        "expected_pc": hex(int(scenario.va)),
        "exception_records": exception_records,
    }
    assert all(record["pc"] != 0 for record in exception_records)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == 0
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_pending_backend_can_accept_fall_holds_then_releases_response(env):
    """Exercise an NC pending response across a backend canAccept fall/rise."""
    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        pbmt=uncache._PBMT_NC,
        instr_count=64,
    )
    env.uncache_agent.configure(latency=32, mmio_latency=32)
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_attrs_for_mapping(env, mapping)
    uncache._force_redirect_to(env, mapping.vaddr)

    assert uncache._wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert uncache._wait_for_request_addr(env, mapping.paddr, max_cycles=6000), {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
    }
    observations_before_fall = len(env.monitor.observations)
    env.backend_model.set_can_accept(0)
    env.step(40)
    assert len(env.monitor.observations) == observations_before_fall

    env.backend_model.set_can_accept(1)
    assert uncache._wait_for_uncache_resp(env, max_cycles=6000) > 0
    assert uncache._wait_for_observed_pc(env, mapping.vaddr, max_cycles=12000), {
        "mapping": mapping,
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-16:]],
        "uncache": env.uncache_agent.get_stats(),
    }
    stats = env.uncache_agent.get_stats()
    assert not env.memory.is_mmio(mapping.paddr)
    assert int(stats.get("req_count", 0)) > 0
    assert int(stats.get("resp_count", 0)) > 0
    assert mapping.paddr in stats.get("request_addrs", [])
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_full_ibuffer_holds_result_then_recovers(env, tmp_path):
    """Fill IBuffer with an NC stream, then require lossless recovery."""
    payload = int(uncache._CNOP).to_bytes(2, "little") * 1024
    bin_path = tmp_path / "nc_ibuffer_backpressure.bin"
    bin_path.write_bytes(payload)
    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        pbmt=uncache._PBMT_NC,
        bin_path=bin_path,
    )
    env.uncache_agent.configure(latency=64, mmio_latency=64)
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_attrs_for_mapping(env, mapping)
    timing_samples = _register_nc_timing_observer(env)
    uncache._force_redirect_to(env, mapping.vaddr)

    assert uncache._wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert uncache._wait_for_request_addr(env, mapping.paddr, max_cycles=6000), {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
    }
    env.backend_model.set_can_accept(0)
    ibuffer_full = env.frontend_info_if.io_frontendInfo_ibufFull
    held_result = None
    stalled_send = None
    checked_sample_count = 0
    for _ in range(24000):
        for sample in timing_samples[checked_sample_count:]:
            if (
                held_result is None
                and sample["uncache_resp_valid"] == 1
                and sample["to_ibuffer_valid"] == 1
                and sample["to_ibuffer_ready"] == 0
            ):
                held_result = sample
            if (
                stalled_send is None
                and sample["nc_pending"] == 1
                and sample["uncache_state"] == uncache._IFU_UNCACHE_SEND_REQ
                and sample["to_ibuffer_ready"] == 0
                and sample["tl_a_valid"] == 0
            ):
                stalled_send = sample
        checked_sample_count = len(timing_samples)
        if held_result is not None and stalled_send is not None:
            break
        env.step(1)

    assert held_result is not None, {
        "reason": "NC response did not remain valid while IBuffer rejected it",
        "uncache": env.uncache_agent.get_stats(),
        "timing_tail": timing_samples[-64:],
        "observed": [hex(int(obs.pc)) for obs in env.monitor.observations[-16:]],
    }
    assert stalled_send is not None, {
        "reason": "NC request did not stop in SEND_REQ while IBuffer was stalled",
        "uncache": env.uncache_agent.get_stats(),
        "timing_tail": timing_samples[-64:],
    }
    assert int(ibuffer_full.value) == 1
    req_count_while_full = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.step(4)
    assert int(ibuffer_full.value) == 1
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == req_count_while_full

    env.backend_model.set_can_accept(1)
    assert uncache._wait_for_observed_pc(env, mapping.vaddr, max_cycles=12000), {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
        "observed": [hex(int(obs.pc)) for obs in env.monitor.observations[-16:]],
    }
    assert uncache._wait_for_uncache_req_count(
        env, req_count_while_full + 1, max_cycles=6000
    ), env.uncache_agent.get_stats()
    for _ in range(6000):
        if int(ibuffer_full.value) == 0:
            break
        env.step(1)
    assert int(ibuffer_full.value) == 0
    first = next(obs for obs in env.monitor.observations if int(obs.pc) == int(mapping.vaddr))
    assert int(first.instr) == int(uncache._ADDI_X0_X0_0)
    assert bool(first.is_rvc)
    assert not env.memory.is_mmio(mapping.paddr)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_response_and_redirect_same_cycle_recover_on_cacheable_path(env):
    """Flush an outstanding NC response and recover through a cacheable page."""
    nc_expected, cacheable_pcs = uncache._prepare_sv39_dual_nc_cacheable_stream(env)
    cacheable_target = int(cacheable_pcs[0])
    expected_instr = int(nc_expected[0][1])
    expected_is_rvc = bool(nc_expected[0][2])
    env.uncache_agent.configure(latency=48, mmio_latency=48)
    env.icache_agent.configure(hit_latency=8, miss_latency=8, miss_rate=0.0, seed=7)
    uncache._initialize_sv39_fetch(env, reset_vector=uncache._NORMAL_BASE)
    uncache._configure_exec_attrs_16k(env, base_addr=0x80000000)
    timing_samples = _register_nc_timing_observer(env)
    uncache._force_redirect_to(env, uncache._NORMAL_BASE)

    assert uncache._wait_for_request_addr(
        env, uncache._NORMAL_PHYS_BASE, max_cycles=6000
    ), env.uncache_agent.get_stats()
    assert env.uncache_agent.pending
    ready_cycle = int(env.uncache_agent.pending[0].ready_cycle)
    redirect_delay = ready_cycle - int(env.current_cycle)
    assert redirect_delay > 0
    observations_before_redirect = len(env.monitor.observations)
    env.backend_model.inject_redirect(
        cacheable_target,
        "ctrl_redirect",
        delay_cycles=redirect_delay,
    )

    assert uncache._wait_for_uncache_resp(env, max_cycles=6000) > 0
    coincident = [
        sample
        for sample in timing_samples
        if sample["backend_redirect"] == 1
        and sample["ifu_flush"] == 1
        and sample["uncache_resp_valid"] == 1
    ]
    assert coincident, {
        "ready_cycle": ready_cycle,
        "redirect_delay": redirect_delay,
        "timing_tail": timing_samples[-64:],
    }
    assert uncache._wait_for_icache_req(env, max_cycles=6000) > 0
    assert uncache._wait_for_observed_pc(env, cacheable_target, max_cycles=12000), {
        "cacheable_target": hex(cacheable_target),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
        "observed": [hex(int(obs.pc)) for obs in env.monitor.observations[-16:]],
    }

    observed = next(
        obs for obs in env.monitor.observations if int(obs.pc) == cacheable_target
    )
    assert int(observed.instr) == expected_instr
    assert bool(observed.is_rvc) is expected_is_rvc
    assert not any(
        int(obs.pc) == int(uncache._NORMAL_BASE)
        for obs in env.monitor.observations[observations_before_redirect:]
    )
    assert int(env.uncache_agent.get_stats().get("resp_count", 0)) > 0
    assert int(env.icache_agent.get_stats().get("req_count", 0)) > 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_pending_overlaps_natural_predchecker_redirect(env, tmp_path):
    """Create a stale cacheable prediction whose target fetch is PBMT.NC."""
    source_va = uncache._NORMAL_BASE
    target_va = source_va + uncache._SV39_PAGE_SIZE
    source_branch_offset = 26
    target_branch_offset = uncache._SV39_PAGE_SIZE + 26
    source_branch_pc = source_va + source_branch_offset
    target_branch_pc = source_va + target_branch_offset

    payload = bytearray(
        int(uncache._CNOP).to_bytes(2, "little") * (2 * uncache._SV39_PAGE_SIZE)
    )
    payload[source_branch_offset : source_branch_offset + 4] = _encode_jal_x0(
        target_va - source_branch_pc
    ).to_bytes(4, "little")
    payload[target_branch_offset : target_branch_offset + 4] = _encode_jal_x0(
        source_va - target_branch_pc
    ).to_bytes(4, "little")
    bin_path = tmp_path / "nc_predchecker_overlap.bin"
    bin_path.write_bytes(bytes(payload))

    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=source_va,
        paddr_pages=(
            uncache._NORMAL_PHYS_BASE,
            uncache._NORMAL_PHYS_BASE + uncache._SV39_PAGE_SIZE,
        ),
        pbmt=uncache._PBMT_PMA,
        bin_path=bin_path,
    )
    uncache._remap_sv39_page_pbmt(
        env,
        vaddr=target_va,
        paddr=mapping.paddr_pages[1],
        pbmt=uncache._PBMT_NC,
    )
    env.icache_agent.configure(hit_latency=8, miss_latency=8, miss_rate=0.0, seed=11)
    env.uncache_agent.configure(latency=24, mmio_latency=24)
    uncache._initialize_sv39_fetch(env, reset_vector=source_va)
    _configure_exec_attrs_for_pages(env, mapping.paddr_pages)
    uncache._force_redirect_to(env, source_va)

    assert _wait_for_taken_prediction(env, source_branch_pc, max_cycles=24000), {
        "reason": "cacheable page-tail JAL did not become a taken prediction",
        "source_branch_pc": hex(int(source_branch_pc)),
        "backend": env.backend_model.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    uncache_req_count_before_fault = int(
        env.uncache_agent.get_stats().get("req_count", 0)
    )

    source_branch_paddr = mapping.paddr_pages[0] + source_branch_offset
    env.memory.write_u32(source_branch_paddr, uncache._ADDI_X0_X0_0)
    env.clock_reset.io_fencei.value = 1
    env.step(1)
    env.clock_reset.io_fencei.value = 0
    env.step(2)
    env.monitor.clear()
    env.monitor.set_expected_pc(source_va)
    timing_samples = _register_nc_timing_observer(env)
    uncache._force_redirect_to(env, source_va)

    overlap = None
    checked_sample_count = 1
    for _ in range(12000):
        for index in range(checked_sample_count, len(timing_samples)):
            current = timing_samples[index]
            if (
                current["nc_pending"] == 1
                and current["wb_path_valid"] == 1
                and current["wb_redirect"] == 1
            ):
                overlap = {"current": current}
                break
        checked_sample_count = max(1, len(timing_samples))
        if overlap is not None:
            break
        env.step(1)

    assert overlap is not None, {
        "reason": "stale cacheable prediction did not redirect while an NC fetch was pending",
        "source_branch_pc": hex(int(source_branch_pc)),
        "target_va": hex(int(target_va)),
        "timing_tail": timing_samples[-64:],
        "backend": env.backend_model.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    assert uncache._wait_for_uncache_req_count(
        env, uncache_req_count_before_fault + 1, max_cycles=6000
    ), env.uncache_agent.get_stats()
    assert uncache._wait_for_observed_pc(env, source_branch_pc, max_cycles=6000), {
        "source_branch_pc": hex(int(source_branch_pc)),
        "observed": [hex(int(obs.pc)) for obs in env.monitor.observations[-32:]],
    }
    assert uncache._wait_for_observed_pc(env, target_va, max_cycles=12000), {
        "target_va": hex(int(target_va)),
        "observed": [hex(int(obs.pc)) for obs in env.monitor.observations[-32:]],
        "uncache": env.uncache_agent.get_stats(),
    }
    post_fault_observations = list(env.monitor.observations)
    assert any(int(obs.pc) == int(source_branch_pc) for obs in post_fault_observations)
    assert any(int(obs.pc) == int(target_va) for obs in post_fault_observations)
    source_branch_obs = next(
        obs for obs in post_fault_observations if int(obs.pc) == int(source_branch_pc)
    )
    assert int(source_branch_obs.instr) == int(uncache._ADDI_X0_X0_0)
    assert not bool(source_branch_obs.is_rvc)
    assert not env.memory.is_mmio(mapping.paddr_pages[1])
    assert not env.monitor.get_errors()
