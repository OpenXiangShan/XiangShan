from __future__ import annotations

import pytest

from env.funcov.py.ifu.mmio_nc_owner_funcov import (
    derive_nc_pending,
    read_nc_timing_runtime_snapshot,
)
from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationPtwResponseOverride,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.support import PmpPmaConfig
from tests.py.zhaoxinran import test_address_translation_fault as translation_faults
from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


_CFVEC_EXCEPTION_BITS = (1, 2, 12, 19, 20)


def _read_exception_bit(signal) -> int:
    value = getattr(signal, "value", None)
    return 0 if value is None else int(value)


_NC_TIMING_SNAPSHOT_KEYS = {
    "backend_redirect": "backend_redirect",
    "ifu_flush": "ifu_flush",
    "uncache_resp_valid": "resp_valid",
    "instr_uncache_resp_valid": "instr_resp_valid",
    "uncache_state": "uncache_state",
    "to_ibuffer_valid": "to_valid",
    "to_ibuffer_ready": "to_ready",
    "to_ibuffer_enq": "to_enq",
    "exception_cross_page": "to_exception_cross_page",
    "exception_type": "to_exception",
    "prev_end_half_rvi": "prev_end_half",
    "wb_path_valid": "wb_path_valid",
    "wb_redirect": "checker_redirect",
    "tl_a_valid": "tl_a_valid",
    "s2_req_uncache": "s2_req_uncache",
    "s2_pbmt": "s2_pbmt",
}

_IFU_UNCACHE_REQ_VALID_SIGNALS = (
    "Frontend_top.Frontend.inner_ifu.uncacheUnit.io_req_valid",
    "TOP.Frontend_top.Frontend.inner_ifu.uncacheUnit.io_req_valid",
)


def _register_nc_timing_observer(env) -> list[dict]:
    samples: list[dict] = []

    def capture(cycle, current_env) -> None:
        recorder = current_env.functional_coverage
        assert recorder is not None, "NC timing observer requires functional coverage"
        snapshot = read_nc_timing_runtime_snapshot(recorder, current_env.dut)
        missing = [
            snapshot_key
            for snapshot_key in _NC_TIMING_SNAPSHOT_KEYS.values()
            if snapshot[snapshot_key] is None
        ]
        assert not missing, {"missing_runtime_semantics": missing}
        state = getattr(recorder, "_ifu_mmio_nc_owner_state", None)
        assert isinstance(state, dict) and "nc_active" in state
        sample = {
            name: int(snapshot[snapshot_key])
            for name, snapshot_key in _NC_TIMING_SNAPSHOT_KEYS.items()
        }
        sample["nc_pending"] = int(
            derive_nc_pending(snapshot, nc_active=bool(state["nc_active"]))
        )
        sample["cycle"] = int(cycle)
        sample["uncache_req_valid"] = uncache._require_first_dut_signal(
            current_env, _IFU_UNCACHE_REQ_VALID_SIGNALS
        )
        sample["tl_d_valid"] = int(current_env.uncache_if.d_valid.value)
        sample["backend_redirect_drive"] = int(
            current_env.backend_ctrl_if.redirect_valid.value
        )
        samples.append(sample)

    env.register_pre_drive_cycle_observer(capture)
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
                if _read_exception_bit(observe.cfvec_exception_vec[slot][bit]) != 0
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
    *,
    s2xlate: int,
    response_field: str,
    expected_result: str,
    scenario_index: int = 0,
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
    translation = {
        "mode": "bare" if s2xlate == 2 else "sv39",
        "s2xlate": s2xlate,
        "priv_virt": int(s2xlate != 0),
    }
    if s2xlate == 2:
        translation.update(
            {
                "s2_pte": TranslationPte(vmid=7, pbmt=uncache._PBMT_NC),
                "hgatp_vmid": 7,
            }
        )
    elif s2xlate == 3:
        gpa = (
            uncache._NORMAL_PHYS_BASE
            + 0x20000
            + page_pair_offset
            + uncache._SV39_PAGE_SIZE
            - 2
        )
        translation.update(
            {
                "gpa": gpa,
                "s1_pte": TranslationPte(
                    asid=5,
                    vmid=7,
                    pbmt=uncache._PBMT_NC,
                ),
                "s2_pte": TranslationPte(vmid=7),
                "vsatp_asid": 5,
                "hgatp_vmid": 7,
            }
        )
    else:
        translation["s1_pte"] = TranslationPte(
            pbmt=uncache._PBMT_NC,
        )
    response_patch = ((response_field, 1),)
    ptw_response_overrides = (
        TranslationPtwResponseOverride(
            vpn=(cross_page_va >> 12) + 1,
            s2xlate=s2xlate,
            patch=response_patch,
        ),
    )
    if s2xlate == 3 and response_field == "s2_gpf":
        ptw_response_overrides += (
            TranslationPtwResponseOverride(
                vpn=(cross_page_va >> 12) + 1,
                s2xlate=s2xlate,
                get_gpa=1,
                patch=response_patch,
            ),
        )
    scenario = TranslationScenario(
        scenario_id=(
            f"nc-cross-page-second-s2xlate-{s2xlate}-{response_field}-"
            f"{scenario_index}"
        ),
        va=cross_page_va,
        pa=cross_page_pa,
        payload=payload,
        page_count=2,
        ptw_response_overrides=ptw_response_overrides,
        expected_path="fault",
        expected_result=expected_result,
        pmp_entries=pmp_entries,
        pma_entries=pma_entries,
        **translation,
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


@pytest.mark.funcov_bins("BIN-1128")
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
    assert env.functional_coverage.key_hit(
        "ifu_instruncache_owner_v3", "instruncache_leaf_035"
    ), env.functional_coverage.raw_path()
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "s2xlate,response_field,expected_result,expected_fault",
    translation_faults._CROSS_PAGE_FAULT_CASES,
)
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_cross_page_second_page_translation_fault_has_exact_pc(
    env,
    s2xlate: int,
    response_field: str,
    expected_result: str,
    expected_fault: str,
):
    """Require a PBMT.NC cross-page fault to retain its original virtual PC."""
    scenario, cross_page_va, cross_page_pa = _nc_cross_page_fault_scenario(
        s2xlate=s2xlate,
        response_field=response_field,
        expected_result=expected_result,
    )
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
        "s2xlate": s2xlate,
        "response_field": response_field,
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
        "s2xlate": s2xlate,
        "response_field": response_field,
        "ptw": env.ptw_agent.get_stats(),
    }
    assert active is not None and active.get("fault_seen"), {
        "s2xlate": s2xlate,
        "response_field": response_field,
        "active": active,
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    second_page_vpn = (cross_page_va >> 12) + 1
    assert any(
        int(request["vpn"]) == second_page_vpn
        and int(request["s2xlate"]) == s2xlate
        for request in active["expected_ptw_requests"]
    ), active["expected_ptw_requests"]
    assert active["expected_fault"] == expected_fault
    expected_exception_bit = translation_faults._FAULT_BITS[expected_fault]
    assert uncache._wait_for_monitor_exception(env, max_cycles=12000), {
        "s2xlate": s2xlate,
        "response_field": response_field,
        "active": active,
        "observed": [int(obs.pc) for obs in env.monitor.observations[-16:]],
    }
    assert any(
        record["pc"] == int(cross_page_va)
        and record["bits"] == (expected_exception_bit,)
        for record in exception_records
    ), {
        "s2xlate": s2xlate,
        "response_field": response_field,
        "expected_pc": hex(int(cross_page_va)),
        "expected_exception_bit": expected_exception_bit,
        "exception_records": exception_records,
    }
    assert any(int(obs.pc) == int(cross_page_va) for obs in env.monitor.observations), {
        "s2xlate": s2xlate,
        "response_field": response_field,
        "expected_pc": hex(int(cross_page_va)),
        "observed": [hex(int(obs.pc)) for obs in env.monitor.observations[-16:]],
    }
    assert all(int(obs.pc) != 0 for obs in env.monitor.observations)
    assert env.monitor.exception_mark_count > 0
    expected_exception_type = {
        "instruction_page_fault": 1,
        "instruction_guest_page_fault": 2,
        "instruction_access_fault": 3,
    }[expected_fault]
    cross_page_samples = [
        sample
        for sample in timing_samples
        if sample["prev_end_half_rvi"] == 1
        and sample["to_ibuffer_valid"] == 1
        and sample["exception_cross_page"] == 1
        and sample["exception_type"] == expected_exception_type
    ]
    assert cross_page_samples, {
        "s2xlate": s2xlate,
        "response_field": response_field,
        "expected_exception_type": expected_exception_type,
        "timing_tail": timing_samples[-32:],
    }
    assert not env.memory.is_mmio(first_beat)
    next_page_pa = (
        cross_page_pa & ~(uncache._SV39_PAGE_SIZE - 1)
    ) + uncache._SV39_PAGE_SIZE
    assert next_page_pa not in env.uncache_agent.get_stats().get(
        "request_addrs", []
    )
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1127")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_cross_page_second_page_fault_matrix_keeps_original_identity(env):
    fault_matrix = (
        (0, "s1_pf", "page_fault", "instruction_page_fault"),
        (3, "s2_gpf", "guest_fault", "instruction_guest_page_fault"),
        (0, "s1_af", "access_fault", "instruction_access_fault"),
    )
    scenarios = [
        _nc_cross_page_fault_scenario(
            s2xlate=s2xlate,
            response_field=response_field,
            expected_result=expected_result,
            scenario_index=index,
        )
        for index, (s2xlate, response_field, expected_result, _) in enumerate(
            fault_matrix
        )
    ]
    env.uncache_agent.configure(latency=16, mmio_latency=16)
    uncache._initialize_sv39_fetch(env, reset_vector=scenarios[0][1])
    exception_records = _register_cfvec_exception_observer(env)

    for index, (fault_case, scenario_info) in enumerate(zip(fault_matrix, scenarios)):
        s2xlate, response_field, _, expected_fault = fault_case
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
                    "s2xlate": s2xlate,
                    "response_field": response_field,
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
            "s2xlate": s2xlate,
            "response_field": response_field,
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
            raise AssertionError(
                {
                    "s2xlate": s2xlate,
                    "response_field": response_field,
                    "active": active,
                }
            )

        expected_exception_bit = translation_faults._FAULT_BITS[expected_fault]
        phase_exception_records = exception_records[exception_cursor:]
        assert any(
            record["bits"] == (expected_exception_bit,)
            and record["cross_page"] == 1
            and record["pc"] == int(cross_page_va)
            for record in phase_exception_records
        ), {
            "s2xlate": s2xlate,
            "response_field": response_field,
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
    commits_before_fall = int(env.backend_model.commit_count)
    env.backend_model.set_can_accept(0)
    env.step(40)
    held_observations = env.monitor.observations[observations_before_fall:]
    assert held_observations
    assert {
        (int(obs.pc), int(obs.instr), bool(obs.is_rvc))
        for obs in held_observations
    } == {
        (
            mapping.vaddr,
            int(_expected_block[0][1]),
            bool(_expected_block[0][2]),
        )
    }, held_observations
    assert int(env.backend_model.commit_count) == commits_before_fall

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
    # Both agents drive after the ready-cycle rising-edge callback. Step() is
    # measured in half-cycles, so advance through the next rising edge where
    # the DUT consumes the response and redirect together.
    env.step(2)
    coincident_inputs = [
        sample
        for sample in timing_samples
        if sample["backend_redirect_drive"] == 1
        and sample["tl_d_valid"] == 1
    ]
    coincident = [
        sample
        for sample in timing_samples
        if sample["backend_redirect"] == 1
        and sample["ifu_flush"] == 1
        and sample["instr_uncache_resp_valid"] == 1
        and sample["uncache_resp_valid"] == 0
    ]
    timing_window = [
        {
            key: sample[key]
            for key in (
                "cycle",
                "backend_redirect",
                "ifu_flush",
                "uncache_resp_valid",
                "instr_uncache_resp_valid",
                "uncache_state",
                "nc_pending",
            )
        }
        for sample in timing_samples
        if ready_cycle - 2 <= sample["cycle"] <= ready_cycle + 2
    ]
    assert coincident_inputs, {
        "ready_cycle": ready_cycle,
        "redirect_delay": redirect_delay,
        "timing_window": timing_window,
    }
    assert coincident, {
        "ready_cycle": ready_cycle,
        "redirect_delay": redirect_delay,
        "timing_window": timing_window,
    }
    assert uncache._wait_for_icache_req(env, max_cycles=6000) > 0
    assert any(
        sample["backend_redirect"] == 1 and sample["ifu_flush"] == 1
        for sample in timing_samples
    ), timing_samples[-64:]
    assert not any(
        sample["uncache_resp_valid"] == 1 for sample in timing_samples
    ), timing_samples[-64:]
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
def test_nc_request_selection_overlaps_natural_predchecker_redirect(env, tmp_path):
    """Cancel a stale prediction's PBMT.NC request before it becomes pending."""
    source_va = uncache._NORMAL_BASE
    target_va = source_va + uncache._SV39_PAGE_SIZE
    source_branch_offset = 26
    target_branch_offset = uncache._SV39_PAGE_SIZE + uncache._FETCH_BLOCK_SIZE + 26
    source_branch_pc = source_va + source_branch_offset
    target_branch_pc = source_va + target_branch_offset

    payload = bytearray(
        int(uncache._CNOP).to_bytes(2, "little") * uncache._SV39_PAGE_SIZE
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
    source_branch_paddr = mapping.paddr_pages[0] + source_branch_offset
    env.memory.write_u32(source_branch_paddr, uncache._ADDI_X0_X0_0)
    env.clock_reset.io_fencei.value = 1
    env.step(1)
    env.clock_reset.io_fencei.value = 0
    env.step(2)
    uncache._remap_sv39_page_pbmt(
        env,
        vaddr=target_va,
        paddr=mapping.paddr_pages[1],
        pbmt=uncache._PBMT_NC,
    )
    uncache._pulse_sfence(env, addr=target_va, rs1=1, rs2=0)
    env.monitor.clear()
    env.monitor.set_expected_pc(source_va)
    timing_samples = _register_nc_timing_observer(env)
    uncache_req_count_before_fault = int(
        env.uncache_agent.get_stats().get("req_count", 0)
    )
    uncache._force_redirect_to(env, source_va)

    overlap = None
    checked_sample_count = 1
    for _ in range(12000):
        for index in range(checked_sample_count, len(timing_samples)):
            current = timing_samples[index]
            if (
                current["uncache_req_valid"] == 1
                and current["s2_req_uncache"] == 1
                and current["s2_pbmt"] == uncache._PBMT_NC
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
        "reason": "stale cacheable prediction did not cancel the selected NC request",
        "source_branch_pc": hex(int(source_branch_pc)),
        "target_va": hex(int(target_va)),
        "timing_tail": timing_samples[-64:],
        "backend": env.backend_model.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    assert overlap["current"]["nc_pending"] == 0
    assert overlap["current"]["uncache_state"] == uncache._IFU_UNCACHE_INVALID
    assert overlap["current"]["tl_a_valid"] == 0
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == uncache_req_count_before_fault
    assert uncache._wait_for_observed_pc(env, source_branch_pc, max_cycles=6000), {
        "source_branch_pc": hex(int(source_branch_pc)),
        "observed": [hex(int(obs.pc)) for obs in env.monitor.observations[-32:]],
    }
    post_fault_observations = list(env.monitor.observations)
    assert any(int(obs.pc) == int(source_branch_pc) for obs in post_fault_observations)
    assert not any(int(obs.pc) == int(target_va) for obs in post_fault_observations)
    source_branch_obs = next(
        obs for obs in post_fault_observations if int(obs.pc) == int(source_branch_pc)
    )
    assert int(source_branch_obs.instr) == int(uncache._ADDI_X0_X0_0)
    assert not bool(source_branch_obs.is_rvc)
    assert not env.memory.is_mmio(mapping.paddr_pages[1])
    assert not env.monitor.get_errors()
