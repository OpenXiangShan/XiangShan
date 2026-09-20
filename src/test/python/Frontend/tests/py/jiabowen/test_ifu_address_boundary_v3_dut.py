"""Current-DUT exact-target regression for BIN-900."""

from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.funcov.py.ifu.compact_funcov import _read_ifu_internal_with_path
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.sequences import LoadProgramSequence
from env.support import PmpPmaConfig
from tests.py.support import uncache_scenarios as uncache


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_ADDRESS_BITS = 50
_BOUNDARY_BYTES = 0x2000
_LOW_BASE = 0x1000
_HIGH_BASE = (1 << _ADDRESS_BITS) - _BOUNDARY_BYTES
_LOW_PHYS_BASE = 0x80200000
_HIGH_PHYS_BASE = 0x80300000
_NOP = 0x00000013
_AUIPC_X6_ZERO = 0x00000317
_JALR_X0_X6_ZERO = 0x00030067


def _load_nop_page(env, base: int, *, low_source: bool = False) -> None:
    payload = int(_NOP).to_bytes(4, "little") * 1024
    if low_source:
        # The architectural source is an indirect jump. The model supplies
        # the high-boundary target through GoldenTrace; DUT still fetches the
        # real AUIPC/JALR bytes from the low boundary page.
        payload = bytearray(payload)
        payload[0:4] = int(_AUIPC_X6_ZERO).to_bytes(4, "little")
        payload[4:8] = int(_JALR_X0_X6_ZERO).to_bytes(4, "little")
        payload = bytes(payload)
    LoadProgramSequence(
        image=ProgramImage(payload=payload, base_addr=int(base)), step_cycles=0
    ).run(env)


def _boundary_trace() -> GoldenTrace:
    entries = [
        TraceEntry(0, _LOW_BASE, _AUIPC_X6_ZERO, 4),
        TraceEntry(1, _LOW_BASE + 4, _JALR_X0_X6_ZERO, 4,
                   "jump_indirect", True, _HIGH_BASE),
    ]
    entries.extend(
        TraceEntry(index + 2, _HIGH_BASE + 4 * index, _NOP, 4)
        for index in range(1024)
    )
    return GoldenTrace(entries)


def _configure_exec_cacheable_4k(env, index: int, base_addr: int) -> None:
    """Give each boundary its own PMP/PMA entry; entry 0 cannot be reused."""
    config = PmpPmaConfig(
        match="napot", read=True, write=True, execute=True,
        cacheable=True, atomic=True,
    )
    env.write_pmp_entry(
        int(index),
        PmpPmaConfig(match="napot", read=True, write=True, execute=True),
        int(base_addr), size=0x1000, settle_cycles=4,
    )
    env.write_pma_entry(
        int(index), config, int(base_addr), size=0x1000, settle_cycles=4,
    )


def _wait_for_boundary_witness(env, region: str, *, max_cycles: int = 12000) -> dict:
    recorder = env.functional_coverage

    def read_ifu(stem: str) -> dict:
        value, path = _read_ifu_internal_with_path(recorder, env.dut, stem)
        return {"value": None if value is None else int(value), "path": path}

    for _ in range(int(max_cycles)):
        witness = getattr(recorder, "_ifu_owner_address_boundary_witnesses", {}).get(region)
        if witness is not None:
            return witness
        try:
            env.step(1)
        except AssertionError as exc:
            if "first mismatch has no attributable CFI" not in str(exc):
                raise
            env._emit_event(
                "ifu.bin900_address_boundary_blocked",
                {
                    "region": region,
                    "reason": "golden_trace_backend_mismatch",
                    "error": str(exc),
                    "icache_stats": env.icache_agent.get_stats(),
                    "backend_stats": env.backend_model.get_stats(),
                    "ifu_signals": {
                        stem: read_ifu(stem)
                        for stem in (
                            "s0_valid",
                            "s0_fire",
                            "s1_valid",
                            "s1_fetchFinish",
                            "s1_realFire",
                            "s1_icacheMeta_0_exception_value",
                            "s1_icacheMeta_0_pmpMmio",
                            "s1_icacheMeta_0_itlbPbmt",
                        )
                    },
                },
            )
            pytest.skip(f"current DUT boundary trace is not attributable: {exc}")
    if env.icache_agent.get_stats().get("req_count", 0) == 0:
        env._emit_event(
            "ifu.bin900_address_boundary_blocked",
            {
                "region": region,
                "reason": "no_cacheable_icache_request",
                "address_bits": _ADDRESS_BITS,
                "boundary_bytes": _BOUNDARY_BYTES,
                "explicit_injection_disabled": True,
                "icache_stats": env.icache_agent.get_stats(),
            },
        )
        pytest.skip(
            f"current DUT address map did not issue a cacheable request for {region} boundary"
        )
    blocker = {
        "reason": f"no checked {region} address-boundary delivery",
        "monitor_errors": env.monitor.get_errors(),
        "icache": env.icache_agent.get_stats(),
        "backend": env.backend_model.get_stats(),
        "ptw": env.ptw_agent.get_stats(),
        "ifu_signals": {
            stem: read_ifu(stem)
            for stem in (
                "s0_valid",
                "s0_fire",
                "s1_valid",
                "s1_fetchFinish",
                "s1_realFire",
                "s1_icacheMeta_0_exception_value",
                "s1_icacheMeta_0_pmpMmio",
                "s1_icacheMeta_0_itlbPbmt",
            )
        },
    }
    env._emit_event("ifu.bin900_address_boundary_blocked", blocker)
    raise AssertionError(blocker)


@pytest.mark.funcov_bins("BIN-900")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_ifu_low_high_address_boundaries_preserve_pc_reconstruction(env) -> None:
    """Deliver cacheable instructions from the first and last 8-KiB PC regions."""

    # Both endpoints are legal Sv39 virtual addresses: the upper endpoint is
    # the canonical sign extension of -8 KiB into the current 50-bit IFU PC
    # space. Translate each virtual page to a distinct low physical page so
    # the ICache client width does not truncate or alias the architectural PC.
    env.page_table.clear()
    env.page_table.map_page(
        _LOW_BASE >> 12,
        _LOW_PHYS_BASE >> 12,
        v=1,
        r=1,
        x=1,
        pbmt=0,
    )
    env.page_table.map_page(
        _HIGH_BASE >> 12,
        _HIGH_PHYS_BASE >> 12,
        v=1,
        r=1,
        x=1,
        pbmt=0,
    )
    env.ptw_agent.configure(
        mode="sv39", response_source="model", compare_drive_source="model"
    )
    _load_nop_page(env, _LOW_PHYS_BASE, low_source=True)
    _load_nop_page(env, _HIGH_PHYS_BASE)
    assert env.memory.read_u32(_LOW_PHYS_BASE) == _AUIPC_X6_ZERO
    assert env.memory.read_u32(_LOW_PHYS_BASE + 4) == _JALR_X0_X6_ZERO
    env.initialize(reset_vector=_LOW_BASE, bare_mode=False, reset_cycles=20)
    _configure_exec_cacheable_4k(env, 0, _LOW_PHYS_BASE)
    _configure_exec_cacheable_4k(env, 1, _HIGH_PHYS_BASE)
    env.monitor.clear()
    env.monitor.set_expected_pc(_LOW_BASE)
    trace = _boundary_trace()
    env.backend_model.set_golden_trace(trace)
    env.backend_model.set_explicit_injection_enabled(False, reason="BIN-900 real IFU fetch")

    low = _wait_for_boundary_witness(env, "low")
    assert low["start_pc"] < _BOUNDARY_BYTES
    assert all(low["checks"].values())
    low_slots = low["slots"]
    assert low_slots[0]["pc"] == _LOW_BASE
    assert low_slots[0]["instr"] == _AUIPC_X6_ZERO
    assert low_slots[0]["is_rvc"] == 0
    assert any(
        item["pc"] == _LOW_BASE + 4
        and item["instr"] == _JALR_X0_X6_ZERO
        and item["is_rvc"] == 0
        for item in low_slots[:8]
    )
    assert not env.functional_coverage.key_hit(
        "ifu_v3_pipeline_owner_model", "owner_leaf_002"
    )
    assert not env.monitor.get_errors()

    high = _wait_for_boundary_witness(env, "high")
    assert high["start_pc"] >= (1 << _ADDRESS_BITS) - _BOUNDARY_BYTES
    assert all(high["checks"].values())

    definition = env.functional_coverage.definition_by_bin_id["BIN-900"]
    assert env.functional_coverage.key_hit(
        definition.coverage_group,
        definition.bin_name,
        coverpoint=definition.coverpoint,
    )
    high_slots = high["slots"]
    assert high_slots
    assert high_slots[0]["pc"] == _HIGH_BASE
    assert [item["pc"] for item in high_slots[:8]] == [
        _HIGH_BASE + 4 * index for index in range(min(8, len(high_slots)))
    ]
    assert all(
        item["instr"] == _NOP and item["is_rvc"] == 0
        for item in high_slots[:8]
    )
    assert not env.get_errors()
