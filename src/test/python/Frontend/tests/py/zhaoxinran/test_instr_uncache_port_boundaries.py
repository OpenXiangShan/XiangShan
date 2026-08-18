from __future__ import annotations

import os
import random
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

import pytest

from env.runtime.pylib import frontend_offset_path

from env.sequences import (
    InjectRedirectSequence,
    LoadProgramSequence,
    RunUntilCommitSequence,
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationScenario,
    TranslationScenarioBuilder,
    TranslationSectorLane,
    TranslationScenarioPhase,
    TranslationScenarioSequence,
    TranslationSfenceAction,
)
from env.core.transactions import CommitTarget, ProgramImage, RedirectTxn
from env.support import PmpPmaConfig


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_MMIO_BASE = 0x10001000
_NORMAL_BASE = 0x80000000
_NORMAL_PHYS_BASE = 0x80001000
_NORMAL_ALT_BASE = 0x80002000
_NORMAL_ALT_PHYS_BASE = 0x80002000
_CROSS_BEAT_PC = _MMIO_BASE + 0x6
_CNOP = 0x0001
_ADDI_X0_X0_0 = 0x00000013
_JAL_X0_PLUS_4 = 0x0040006F
_FETCH_BLOCK_SIZE = 64
_UNCACHE_BEAT_BYTES = 8
_PBMT_PMA = 0
_PBMT_NC = 1
_PBMT_IO = 2
_IFU_UNCACHE_INVALID = 0
_IFU_UNCACHE_WAIT_LAST_COMMIT = 1
_IFU_UNCACHE_SEND_REQ = 2
_IFU_UNCACHE_WAIT_RESP = 3
_INSTR_UNCACHE_INVALID = 0
_INSTR_UNCACHE_REFILL_REQ = 1
_INSTR_UNCACHE_REFILL_RESP = 2
_INSTR_UNCACHE_SEND_RESP = 3
_SV39_PAGE_SIZE = 0x1000
_CROSS_PAGE_PC = _MMIO_BASE + _SV39_PAGE_SIZE - 2
_SV39_RANDOM_VADDR_MIN = 0x40000000
_SV39_RANDOM_VADDR_MAX = 0x7FFF0000
_SV39_RANDOM_PADDR_MIN = 0x80000000
_SV39_RANDOM_PADDR_MAX = 0x8FFF0000
_SV39_RANDOM_MAP_SEED = 0x5A39C001


@dataclass(frozen=True)
class Sv39Mapping:
    vaddr: int
    paddr: int
    size: int
    paddr_pages: tuple[int, ...]


def _prepare_mmio_cnop_stream(env, *, instr_count: int = 256) -> None:
    payload = int(_CNOP).to_bytes(2, "little") * int(instr_count)
    env.memory.mmio_ranges.append((_MMIO_BASE, _MMIO_BASE + len(payload)))
    LoadProgramSequence(image=ProgramImage(payload=payload, base_addr=_MMIO_BASE), step_cycles=0).run(env)


def _prepare_cross_beat_rvi_stream(env) -> None:
    payload = bytearray()
    payload.extend(int(_CNOP).to_bytes(2, "little"))
    payload.extend(int(_CNOP).to_bytes(2, "little"))
    payload.extend(int(_CNOP).to_bytes(2, "little"))
    payload.extend(int(_ADDI_X0_X0_0).to_bytes(4, "little"))
    payload.extend(int(_CNOP).to_bytes(2, "little") * 128)
    env.memory.mmio_ranges.append((_MMIO_BASE, _MMIO_BASE + len(payload)))
    LoadProgramSequence(image=ProgramImage(payload=bytes(payload), base_addr=_MMIO_BASE), step_cycles=0).run(env)


def _prepare_cross_page_rvi_stream(env) -> None:
    payload = bytearray(int(_CNOP).to_bytes(2, "little") * (_SV39_PAGE_SIZE // 2 + 128))
    tail_offset = _CROSS_PAGE_PC - _MMIO_BASE
    payload[tail_offset:tail_offset + 4] = int(_ADDI_X0_X0_0).to_bytes(4, "little")
    env.memory.mmio_ranges.append((_MMIO_BASE, _MMIO_BASE + len(payload)))
    LoadProgramSequence(image=ProgramImage(payload=bytes(payload), base_addr=_MMIO_BASE), step_cycles=0).run(env)


def _prepare_cross_page_rvc_stream(env) -> None:
    payload = int(_CNOP).to_bytes(2, "little") * (_SV39_PAGE_SIZE // 2 + 128)
    env.memory.mmio_ranges.append((_MMIO_BASE, _MMIO_BASE + len(payload)))
    LoadProgramSequence(image=ProgramImage(payload=payload, base_addr=_MMIO_BASE), step_cycles=0).run(env)


def _prepare_normal_and_mmio_cnop_stream(env, *, instr_count: int = 256) -> None:
    payload = int(_CNOP).to_bytes(2, "little") * int(instr_count)
    env.memory.mmio_ranges.append((_MMIO_BASE, _MMIO_BASE + len(payload)))
    LoadProgramSequence(image=ProgramImage(payload=payload, base_addr=_NORMAL_BASE), step_cycles=0).run(env)
    LoadProgramSequence(image=ProgramImage(payload=payload, base_addr=_MMIO_BASE), step_cycles=0).run(env)


def _prepare_sv39_mapped_cnop_stream(env, *, vaddr: int, paddr: int, pbmt: int, instr_count: int = 256) -> None:
    payload = int(_CNOP).to_bytes(2, "little") * int(instr_count)
    env.page_table.clear()
    env.page_table.map_page(vaddr >> 12, paddr >> 12, v=1, r=1, x=1, pbmt=int(pbmt))
    env.ptw_agent.configure(mode="sv39", response_source="model", compare_drive_source="model")
    LoadProgramSequence(image=ProgramImage(payload=payload, base_addr=int(paddr)), step_cycles=0).run(env)


def _prepare_sv39_dual_nc_cacheable_stream(env, *, instr_count: int = 512) -> tuple[list[tuple[int, int, bool]], list[int]]:
    nc_payload = bytearray()
    for _ in range(8):
        nc_payload.extend(int(_CNOP).to_bytes(2, "little"))
    nc_payload.extend(int(_JAL_X0_PLUS_4).to_bytes(4, "little"))
    while len(nc_payload) < _FETCH_BLOCK_SIZE:
        nc_payload.extend(int(_CNOP).to_bytes(2, "little"))
    nc_payload.extend(int(_CNOP).to_bytes(2, "little") * int(instr_count))

    env.page_table.clear()
    env.page_table.map_page(_NORMAL_BASE >> 12, _NORMAL_PHYS_BASE >> 12, v=1, r=1, x=1, pbmt=_PBMT_NC)
    env.page_table.map_page(_NORMAL_ALT_BASE >> 12, _NORMAL_ALT_PHYS_BASE >> 12, v=1, r=1, x=1, pbmt=_PBMT_PMA)
    env.ptw_agent.configure(mode="sv39", response_source="model", compare_drive_source="model")
    LoadProgramSequence(image=ProgramImage(payload=bytes(nc_payload), base_addr=_NORMAL_PHYS_BASE), step_cycles=0).run(env)
    LoadProgramSequence(image=ProgramImage(payload=bytes(nc_payload), base_addr=_NORMAL_ALT_PHYS_BASE), step_cycles=0).run(env)
    nc_expected = _decode_fetch_block(bytes(nc_payload), vaddr=_NORMAL_BASE)
    cacheable_pcs = [pc - _NORMAL_BASE + _NORMAL_ALT_BASE for pc, _, _ in nc_expected]
    return nc_expected, cacheable_pcs


def _remap_sv39_page_pbmt(env, *, vaddr: int, paddr: int, pbmt: int) -> None:
    env.page_table.map_page(int(vaddr) >> 12, int(paddr) >> 12, v=1, r=1, x=1, pbmt=int(pbmt))


def _page_align_down(addr: int) -> int:
    return int(addr) & ~(_SV39_PAGE_SIZE - 1)


def _addr_overlaps_any_range(addr: int, size: int, ranges: list[tuple[int, int]]) -> bool:
    lo = int(addr)
    hi = lo + max(1, int(size)) - 1
    for range_lo, range_hi in ranges:
        if lo <= int(range_hi) and hi >= int(range_lo):
            return True
    return False


def _random_page_base(rng: random.Random, *, lo: int, hi: int, size: int, forbidden_ranges: list[tuple[int, int]]) -> int:
    page_count = max(1, (max(1, int(size)) + _SV39_PAGE_SIZE - 1) // _SV39_PAGE_SIZE)
    min_page = _page_align_down(int(lo)) >> 12
    max_page = _page_align_down(int(hi)) >> 12
    max_start_page = max(int(min_page), int(max_page) - int(page_count) + 1)
    for _ in range(1024):
        base = rng.randint(int(min_page), int(max_start_page)) << 12
        if not _addr_overlaps_any_range(base, page_count * _SV39_PAGE_SIZE, forbidden_ranges):
            return int(base)
    raise ValueError("failed to generate non-overlapping SV39 mapping")


def _random_phys_pages(
    rng: random.Random,
    *,
    count: int,
    forbidden_ranges: list[tuple[int, int]],
) -> tuple[int, ...]:
    pages: list[int] = []
    used_ranges = list(forbidden_ranges)
    for _ in range(int(count)):
        page_base = _random_page_base(
            rng,
            lo=_SV39_RANDOM_PADDR_MIN,
            hi=_SV39_RANDOM_PADDR_MAX,
            size=_SV39_PAGE_SIZE,
            forbidden_ranges=used_ranges,
        )
        pages.append(int(page_base))
        used_ranges.append((int(page_base), int(page_base) + _SV39_PAGE_SIZE - 1))
    return tuple(pages)


def _map_random_sv39_program(
    env,
    *,
    payload_size: int,
    pbmt: int,
    vaddr: int | None = None,
    paddr: int | None = None,
    paddr_pages: Sequence[int] | None = None,
    seed: int = _SV39_RANDOM_MAP_SEED,
) -> Sv39Mapping:
    size = max(1, int(payload_size))
    rng = random.Random(int(seed))
    page_count = max(1, (size + _SV39_PAGE_SIZE - 1) // _SV39_PAGE_SIZE)
    forbidden_phys_ranges = [(int(lo), int(hi)) for lo, hi in env.memory.mmio_ranges]
    mapped_vaddr = (
        _page_align_down(int(vaddr))
        if vaddr is not None
        else _random_page_base(
            rng,
            lo=_SV39_RANDOM_VADDR_MIN,
            hi=_SV39_RANDOM_VADDR_MAX,
            size=size,
            forbidden_ranges=[],
        )
    )
    if paddr_pages is not None:
        mapped_paddr_pages = tuple(_page_align_down(int(page)) for page in paddr_pages)
        if len(mapped_paddr_pages) < page_count:
            raise ValueError(f"need {page_count} paddr pages, got {len(mapped_paddr_pages)}")
        mapped_paddr_pages = mapped_paddr_pages[:page_count]
        for page_base in mapped_paddr_pages:
            if _addr_overlaps_any_range(int(page_base), _SV39_PAGE_SIZE, forbidden_phys_ranges):
                raise ValueError(f"paddr page 0x{int(page_base):x} overlaps MMIO range")
    elif paddr is not None:
        first_paddr = _page_align_down(int(paddr))
        mapped_paddr_pages = tuple(first_paddr + page * _SV39_PAGE_SIZE for page in range(page_count))
    else:
        mapped_paddr_pages = _random_phys_pages(
            rng,
            count=page_count,
            forbidden_ranges=forbidden_phys_ranges,
        )

    env.page_table.clear()
    for page in range(page_count):
        env.page_table.map_page(
            (mapped_vaddr >> 12) + page,
            int(mapped_paddr_pages[page]) >> 12,
            v=1,
            r=1,
            x=1,
            pbmt=int(pbmt),
        )
    env.ptw_agent.configure(mode="sv39", response_source="model", compare_drive_source="model")
    return Sv39Mapping(
        vaddr=int(mapped_vaddr),
        paddr=int(mapped_paddr_pages[0]),
        size=int(size),
        paddr_pages=tuple(int(page) for page in mapped_paddr_pages),
    )


def _decode_fetch_block(payload: bytes, *, vaddr: int, block_size: int = _FETCH_BLOCK_SIZE) -> list[tuple[int, int, bool]]:
    expected: list[tuple[int, int, bool]] = []
    offset = 0
    while offset < min(len(payload), int(block_size)):
        pc = int(vaddr) + int(offset)
        halfword = int.from_bytes(payload[offset:offset + 2].ljust(2, b"\x00"), "little")
        is_rvc = (halfword & 0x3) != 0x3
        if is_rvc:
            instr = _ADDI_X0_X0_0 if halfword == _CNOP else halfword
            offset += 2
        else:
            instr = int.from_bytes(payload[offset:offset + 4].ljust(4, b"\x00"), "little")
            offset += 4
        expected.append((pc, instr, is_rvc))
    return expected


def _prepare_sv39_mapped_pbmt_nc_cfi_stream(
    env,
    *,
    vaddr: int | None = None,
    paddr: int | None = None,
    paddr_pages: Sequence[int] | None = None,
    instr_count: int = 256,
    bin_path: str | os.PathLike[str] | None = None,
    map_seed: int = _SV39_RANDOM_MAP_SEED,
    pbmt: int = _PBMT_NC,
) -> tuple[list[tuple[int, int, bool]], Sv39Mapping]:
    if bin_path is not None:
        payload = Path(bin_path).read_bytes()
    else:
        payload = bytearray()
        for _ in range(8):
            payload.extend(int(_CNOP).to_bytes(2, "little"))
        payload.extend(int(_JAL_X0_PLUS_4).to_bytes(4, "little"))
        while len(payload) < _FETCH_BLOCK_SIZE:
            payload.extend(int(_CNOP).to_bytes(2, "little"))
        payload.extend(int(_CNOP).to_bytes(2, "little") * int(instr_count))

    if not payload:
        raise ValueError("PBMT NC stream payload is empty")

    mapping = _map_random_sv39_program(
        env,
        payload_size=len(payload),
        pbmt=int(pbmt),
        vaddr=vaddr,
        paddr=paddr,
        paddr_pages=paddr_pages,
        seed=int(map_seed),
    )
    expected_block = _decode_fetch_block(bytes(payload), vaddr=int(mapping.vaddr))
    for page_index, page_paddr in enumerate(mapping.paddr_pages):
        start = page_index * _SV39_PAGE_SIZE
        page_payload = bytes(payload[start:start + _SV39_PAGE_SIZE])
        if not page_payload:
            continue
        LoadProgramSequence(
            image=ProgramImage(payload=page_payload, base_addr=int(page_paddr)),
            step_cycles=0,
        ).run(env)
    return expected_block, mapping


def _initialize_sv39_fetch(env, *, reset_vector: int) -> None:
    env.initialize(reset_vector=int(reset_vector), bare_mode=False, reset_cycles=20)
    try:
        env.dut.io_tlbCsr_satp_changed.value = 1
        env.step(1)
        env.dut.io_tlbCsr_satp_changed.value = 0
    except Exception:
        pass
    env.monitor.clear()
    env.monitor.set_expected_pc(int(reset_vector))


def _configure_exec_cacheable_pma(env, *, base_addr: int, size: int) -> None:
    env.write_pma_entry(
        0,
        PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
        int(base_addr),
        size=int(size),
        settle_cycles=4,
    )


def _configure_exec_mmio_pma(env, *, base_addr: int, size: int) -> None:
    env.write_pma_entry(
        0,
        PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=False),
        int(base_addr),
        size=int(size),
        settle_cycles=4,
    )


def _configure_exec_pmp(env, *, base_addr: int, size: int) -> None:
    env.write_pmp_entry(
        0,
        PmpPmaConfig(match="napot", read=True, write=True, execute=True),
        int(base_addr),
        size=int(size),
        settle_cycles=4,
    )


def _configure_exec_cacheable_pma_4k(env, *, base_addr: int) -> None:
    _configure_exec_cacheable_pma(env, base_addr=int(base_addr), size=0x1000)


def _configure_exec_mmio_pma_4k(env, *, base_addr: int) -> None:
    _configure_exec_mmio_pma(env, base_addr=int(base_addr), size=0x1000)


def _configure_exec_pmp_4k(env, *, base_addr: int) -> None:
    _configure_exec_pmp(env, base_addr=int(base_addr), size=0x1000)


def _configure_exec_attrs_16k(env, *, base_addr: int) -> None:
    _configure_exec_pmp(env, base_addr=int(base_addr), size=0x4000)
    _configure_exec_cacheable_pma(env, base_addr=int(base_addr), size=0x4000)


def _configure_exec_attrs_for_mapping(env, mapping: Sv39Mapping) -> None:
    if len(mapping.paddr_pages) != 1:
        raise ValueError("current CSR helper configures one PMP/PMA entry; use single-page mappings here")
    _configure_exec_pmp_4k(env, base_addr=int(mapping.paddr_pages[0]))
    _configure_exec_cacheable_pma_4k(env, base_addr=int(mapping.paddr_pages[0]))


def _initialize_mmio_fetch(env, *, reset_vector: int = _MMIO_BASE) -> None:
    env.initialize(reset_vector=int(reset_vector), bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(int(reset_vector))


def _wait_for_uncache_req(env, *, max_cycles: int = 2000) -> int:
    start = int(env.uncache_agent.get_stats().get("req_count", 0))
    for _ in range(int(max_cycles)):
        env.step(1)
        now = int(env.uncache_agent.get_stats().get("req_count", 0))
        if now > start:
            return now
    return int(env.uncache_agent.get_stats().get("req_count", 0))


def _wait_for_uncache_resp(env, *, max_cycles: int = 2000) -> int:
    start = int(env.uncache_agent.get_stats().get("resp_count", 0))
    for _ in range(int(max_cycles)):
        env.step(1)
        now = int(env.uncache_agent.get_stats().get("resp_count", 0))
        if now > start:
            return now
    return int(env.uncache_agent.get_stats().get("resp_count", 0))


def _wait_for_icache_req(env, *, max_cycles: int = 2000) -> int:
    start = int(env.icache_agent.get_stats().get("req_count", 0))
    for _ in range(int(max_cycles)):
        env.step(1)
        now = int(env.icache_agent.get_stats().get("req_count", 0))
        if now > start:
            return now
    return int(env.icache_agent.get_stats().get("req_count", 0))


def _wait_for_uncache_req_count(env, count: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if int(env.uncache_agent.get_stats().get("req_count", 0)) >= int(count):
            return True
        env.step(1)
    return int(env.uncache_agent.get_stats().get("req_count", 0)) >= int(count)


def _wait_for_icache_req_count(env, count: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if int(env.icache_agent.get_stats().get("req_count", 0)) >= int(count):
            return True
        env.step(1)
    return int(env.icache_agent.get_stats().get("req_count", 0)) >= int(count)


def _wait_for_uncache_req_delta(env, delta: int, *, max_cycles: int = 2000) -> bool:
    start = int(env.uncache_agent.get_stats().get("req_count", 0))
    target = start + int(delta)
    for _ in range(int(max_cycles)):
        if int(env.uncache_agent.get_stats().get("req_count", 0)) >= target:
            return True
        env.step(1)
    return int(env.uncache_agent.get_stats().get("req_count", 0)) >= target


def _wait_for_ptw_resp(env, *, max_cycles: int = 2000) -> int:
    start = int(env.ptw_agent.get_stats().get("resp_count", 0))
    for _ in range(int(max_cycles)):
        env.step(1)
        now = int(env.ptw_agent.get_stats().get("resp_count", 0))
        if now > start:
            return now
    return int(env.ptw_agent.get_stats().get("resp_count", 0))


def _wait_for_request_addr(env, addr: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if int(addr) in env.uncache_agent.get_stats().get("request_addrs", []):
            return True
        env.step(1)
    return int(addr) in env.uncache_agent.get_stats().get("request_addrs", [])


def _wait_for_resp_count(env, count: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if int(env.uncache_agent.get_stats().get("resp_count", 0)) >= int(count):
            return True
        env.step(1)
    return int(env.uncache_agent.get_stats().get("resp_count", 0)) >= int(count)


def _wait_for_monitor_exception(env, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if int(env.monitor.exception_mark_count) > 0:
            return True
        env.step(1)
    return int(env.monitor.exception_mark_count) > 0


def _wait_for_observed_pc(env, pc: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if any(int(obs.pc) == int(pc) for obs in env.monitor.observations):
            return True
        env.step(1)
    return any(int(obs.pc) == int(pc) for obs in env.monitor.observations)


def _wait_for_observed_pc_sequence(env, pcs: list[int], *, max_cycles: int = 2000) -> bool:
    expected = {int(pc) for pc in pcs}
    for _ in range(int(max_cycles)):
        seen = {int(obs.pc) for obs in env.monitor.observations if int(obs.pc) in expected}
        if expected.issubset(seen):
            return True
        env.step(1)
    seen = {int(obs.pc) for obs in env.monitor.observations if int(obs.pc) in expected}
    return expected.issubset(seen)


def _first_observed_index(env, pc: int) -> int:
    for idx, obs in enumerate(env.monitor.observations):
        if int(obs.pc) == int(pc):
            return int(idx)
    return -1


def _assert_observed_instrs(env, expected_block: list[tuple[int, int, bool]]) -> None:
    observed_by_pc = {
        int(obs.pc): obs
        for obs in env.monitor.observations
        if any(int(obs.pc) == int(pc) for pc, _, _ in expected_block)
    }
    for pc, instr, is_rvc in expected_block:
        obs = observed_by_pc[int(pc)]
        assert int(obs.instr) == int(instr), {
            "pc": hex(int(pc)),
            "expected_instr": hex(int(instr)),
            "observed_instr": hex(int(obs.instr)),
        }
        assert bool(obs.is_rvc) == bool(is_rvc), {
            "pc": hex(int(pc)),
            "expected_is_rvc": bool(is_rvc),
            "observed_is_rvc": bool(obs.is_rvc),
        }


def _collect_cfvec_cycles(env, *, max_cycles: int) -> list[dict]:
    cycles: list[dict] = []
    for _ in range(int(max_cycles)):
        env.step(1)
        slots = []
        for slot in range(8):
            if _read_dut_signal(env, f"io_backend_cfVec_{slot}_valid", 0) != 1:
                continue
            slots.append(
                {
                    "slot": int(slot),
                    "pc": _read_dut_signal(env, f"io_backend_cfVec_{slot}_bits_pc", 0),
                    "instr": _read_dut_signal(env, f"io_backend_cfVec_{slot}_bits_instr", 0),
                    "is_rvc": _read_dut_signal(env, f"io_backend_cfVec_{slot}_bits_isRvc", 0),
                }
            )
        if slots:
            cycles.append({"cycle": int(env.current_cycle), "slots": slots})
    return cycles


def _read_dut_signal(env, name: str, default: int = 0) -> int:
    value = _try_read_dut_signal(env, name)
    return int(default) if value is None else int(value)


def _try_read_dut_signal(env, name: str) -> int | None:
    try:
        signal = getattr(env.dut, name, None)
        if signal is None:
            getter = getattr(env.dut, "GetInternalSignal", None)
            if callable(getter):
                signal = getter(str(name))
        if signal is None:
            return None
        value = getattr(signal, "value", None)
        if value is None:
            return None
        return int(value)
    except Exception:
        return None


def _read_first_dut_signal(env, names: Sequence[str]) -> int | None:
    for name in names:
        value = _try_read_dut_signal(env, str(name))
        if value is not None:
            return int(value)
    return None


def _require_first_dut_signal(env, names: Sequence[str]) -> int:
    value = _read_first_dut_signal(env, names)
    assert value is not None, {"missing_internal_signals": list(names)}
    return int(value)


_IFU_UNCACHE_STATE_SIGNALS = (
    "Frontend_top.Frontend.inner_ifu.uncacheUnit.uncacheState",
    "TOP.Frontend_top.Frontend.inner_ifu.uncacheUnit.uncacheState",
)
_IFU_UNCACHE_TO_UNCACHE_VALID_SIGNALS = (
    "auto_inner_instrUncache_client_out_a_valid",
)
_INSTR_UNCACHE_ENTRY_STATE_SIGNALS = (
    "Frontend_top.Frontend.inner_instrUncache.entries_0.state",
    "TOP.Frontend_top.Frontend.inner_instrUncache.entries_0.state",
)
_IFU_PREV_HALF_RVI_SIGNALS = {
    "s0": (
        "Frontend_top.Frontend.inner_ifu.s0_prevEndIsHalfRvi",
        "TOP.Frontend_top.Frontend.inner_ifu.s0_prevEndIsHalfRvi",
    ),
    "s1": (
        "Frontend_top.Frontend.inner_ifu.s1_prevEndIsHalfRvi",
        "TOP.Frontend_top.Frontend.inner_ifu.s1_prevEndIsHalfRvi",
    ),
    "s1_data": (
        "Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviData",
        "TOP.Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviData",
    ),
    "s1_pc": (
        "Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviPc_addr",
        "TOP.Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviPc_addr",
    ),
    "s2": (
        "Frontend_top.Frontend.inner_ifu.s2_prevEndIsHalfRvi",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_prevEndIsHalfRvi",
    ),
    "s2_valid": (
        "Frontend_top.Frontend.inner_ifu.s2_valid_valid",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_valid_valid",
    ),
    "s2_data": (
        "Frontend_top.Frontend.inner_ifu.s2_prevEndHalfRviData",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_prevEndHalfRviData",
    ),
    "s2_pc": (
        "Frontend_top.Frontend.inner_ifu.s2_prevEndHalfPc_addr",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_prevEndHalfPc_addr",
    ),
}
_IFU_BACKEND_REDIRECT_SIGNALS = (
    "Frontend_top.Frontend.inner_ftq.backendRedirect_valid",
    "TOP.Frontend_top.Frontend.inner_ftq.backendRedirect_valid",
    "Frontend_top.io_backend_toFtq_redirect_valid",
    "io_backend_toFtq_redirect_valid",
)
_IFU_UNCACHE_NEED_RESEND_SIGNALS = (
    "Frontend_top.Frontend.inner_ifu.uncacheNeedResend",
    "TOP.Frontend_top.Frontend.inner_ifu.uncacheNeedResend",
)

def test_uncache_prev_half_signal_contract_matches_dut_inventory():
    """The cross-page observer must fail closed when any retimed state is absent."""
    offset = frontend_offset_path()
    assert offset.exists(), "DUT signal inventory is required before signal-contract tests"
    registered = {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }
    required = [
        *_IFU_PREV_HALF_RVI_SIGNALS.values(),
        _IFU_BACKEND_REDIRECT_SIGNALS,
        _IFU_UNCACHE_NEED_RESEND_SIGNALS,
    ]
    missing = [list(names) for names in required if not any(name in registered for name in names)]
    assert not missing, {"missing_internal_signals": missing}


def _capture_prev_half_rvi_state(env, cycle: int) -> dict:
    stats = env.uncache_agent.get_stats()
    request_addrs = list(stats.get("request_addrs", []))
    response_addrs = list(stats.get("response_addrs", []))
    return {
        "cycle": int(cycle),
        "s0": _require_first_dut_signal(env, _IFU_PREV_HALF_RVI_SIGNALS["s0"]),
        "s1": _require_first_dut_signal(env, _IFU_PREV_HALF_RVI_SIGNALS["s1"]),
        "s1_data": _require_first_dut_signal(env, _IFU_PREV_HALF_RVI_SIGNALS["s1_data"]),
        "s1_pc": _require_first_dut_signal(env, _IFU_PREV_HALF_RVI_SIGNALS["s1_pc"]),
        "s2": _require_first_dut_signal(env, _IFU_PREV_HALF_RVI_SIGNALS["s2"]),
        "s2_valid": _require_first_dut_signal(env, _IFU_PREV_HALF_RVI_SIGNALS["s2_valid"]),
        "s2_data": _require_first_dut_signal(env, _IFU_PREV_HALF_RVI_SIGNALS["s2_data"]),
        "s2_pc": _require_first_dut_signal(env, _IFU_PREV_HALF_RVI_SIGNALS["s2_pc"]),
        "backend_redirect": _require_first_dut_signal(env, _IFU_BACKEND_REDIRECT_SIGNALS),
        "need_resend": _require_first_dut_signal(env, _IFU_UNCACHE_NEED_RESEND_SIGNALS),
        "req_count": int(stats.get("req_count", 0)),
        "resp_count": int(stats.get("resp_count", 0)),
        "pending_count": int(stats.get("pending", 0)),
        "last_request_addr": request_addrs[-1] if request_addrs else None,
        "last_response_addr": response_addrs[-1] if response_addrs else None,
    }


def _register_prev_half_rvi_observer(env) -> list[dict]:
    samples: list[dict] = []
    env.register_cycle_observer(
        lambda cycle, active_env: samples.append(_capture_prev_half_rvi_state(active_env, cycle))
    )
    return samples


def _pending_uncache_samples(samples: Sequence[dict], addr: int) -> list[dict]:
    return [
        sample
        for sample in samples
        if sample["last_request_addr"] == int(addr)
        and sample["last_response_addr"] != int(addr)
        and int(sample["pending_count"]) > 0
    ]


def _assert_ifu_uncache_state(env, expected: int) -> None:
    assert _require_first_dut_signal(env, _IFU_UNCACHE_STATE_SIGNALS) == int(expected)


def _assert_instr_uncache_entry_state_in(env, expected: set[int]) -> None:
    state = _require_first_dut_signal(env, _INSTR_UNCACHE_ENTRY_STATE_SIGNALS)
    assert state in {int(item) for item in expected}, {"state": state, "expected": sorted(expected)}


def _wait_for_uncache_a_valid_addr(env, addr: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if (
            _require_first_dut_signal(env, _IFU_UNCACHE_TO_UNCACHE_VALID_SIGNALS) == 1
            and int(env.uncache_if.a_bits_address.value) == int(addr)
        ):
            return True
        env.step(1)
    return (
        _require_first_dut_signal(env, _IFU_UNCACHE_TO_UNCACHE_VALID_SIGNALS) == 1
        and int(env.uncache_if.a_bits_address.value) == int(addr)
    )


def _force_redirect_to(env, target_pc: int) -> None:
    env.backend_model.inject_redirect(int(target_pc), "ctrl_redirect", delay_cycles=0)


def _pulse_sfence(env, *, addr: int = 0, rs1: int = 0, rs2: int = 0, cycles: int = 1) -> None:
    env.pulse_sfence(addr=addr, rs1=rs1, rs2=rs2, cycles=cycles)


def _count_mmio_observations(env) -> int:
    return sum(1 for obs in env.monitor.observations if _MMIO_BASE <= int(obs.pc) < (_MMIO_BASE + 0x1000))


def _wait_for_mmio_observations(env, *, min_count: int, max_cycles: int) -> int:
    for _ in range(max(0, int(max_cycles))):
        count = _count_mmio_observations(env)
        if count >= int(min_count):
            return count
        env.step(1)
    return _count_mmio_observations(env)


def _recent_mmio_pcs(env, *, window: int) -> list[int]:
    return [
        int(obs.pc)
        for obs in list(env.monitor.observations)[-max(0, int(window)) :]
        if _MMIO_BASE <= int(obs.pc) < (_MMIO_BASE + 0x1000)
    ]


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_a_ready_backpressure_holds_request(env):
    _prepare_mmio_cnop_stream(env)
    _initialize_mmio_fetch(env)

    first_req = _wait_for_uncache_req(env)
    first_resp = _wait_for_uncache_resp(env)
    assert first_req > 0
    assert first_resp > 0

    env.uncache_agent.set_a_ready(0)
    for _ in range(256):
        env.step(1)
        if int(env.uncache_if.a_valid.value) == 1:
            break

    req_before = int(env.uncache_agent.get_stats().get("req_count", 0))
    assert int(env.uncache_if.a_ready.value) == 0
    assert int(env.uncache_if.a_valid.value) == 1
    stalled_addr = int(env.uncache_if.a_bits_address.value)

    env.step(8)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == req_before
    assert int(env.uncache_if.a_valid.value) == 1
    assert int(env.uncache_if.a_bits_address.value) == stalled_addr

    env.uncache_agent.set_a_ready(None)
    req_after = _wait_for_uncache_req(env)
    assert _wait_for_uncache_req_delta(env, 1)

    assert req_after > req_before
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) >= req_before + 2
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "fault_kwargs,expected_exception,expected_resp_type,expected_path_exception",
    [
        ({"corrupt": 1}, "hwe", "corrupt", None),
        ({"denied": 1}, "af", "denied", "mmio_x_af"),
    ],
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_response_fault_reports_dut_exception(
    env,
    fault_kwargs,
    expected_exception,
    expected_resp_type,
    expected_path_exception,
):
    _prepare_mmio_cnop_stream(env)
    env.uncache_agent.inject_next_response_fault(**fault_kwargs)
    _initialize_mmio_fetch(env)

    req_count = _wait_for_uncache_req(env)
    resp_count = _wait_for_uncache_resp(env)
    stats = env.uncache_agent.get_stats()

    assert req_count > 0
    assert resp_count > 0
    assert int(stats.get("corrupt_resp_count", 0)) == (1 if fault_kwargs.get("corrupt") else 0)
    assert int(stats.get("denied_resp_count", 0)) == (1 if fault_kwargs.get("denied") else 0)
    assert _wait_for_monitor_exception(env)
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_wfi_blocks_new_acquire_and_refill_not_safe(env):
    _prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=32)
    env.backend_model.set_wfi_req(1)
    _initialize_mmio_fetch(env)

    req_before = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.step(64)
    req_during_wfi = int(env.uncache_agent.get_stats().get("req_count", 0))

    env.backend_model.set_wfi_req(0)
    req_after = _wait_for_uncache_req(env)
    assert _wait_for_uncache_req_delta(env, 1)
    env.backend_model.set_wfi_req(1)
    saw_not_safe = False
    for _ in range(64):
        env.step(1)
        if _read_dut_signal(env, "io_backend_wfi_wfiSafe", 1) == 0:
            saw_not_safe = True
            break
    env.backend_model.set_wfi_req(0)

    assert req_during_wfi == req_before
    assert req_after > req_during_wfi
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) >= req_during_wfi + 2
    assert saw_not_safe
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_wfi_during_a_ready_backpressure_retracts_unaccepted_request(env):
    _prepare_mmio_cnop_stream(env)
    _initialize_mmio_fetch(env)

    assert _wait_for_uncache_req(env) > 0
    assert _wait_for_uncache_resp(env) > 0

    env.uncache_agent.set_a_ready(0)
    for _ in range(256):
        env.step(1)
        if int(env.uncache_if.a_valid.value) == 1:
            break

    req_before = int(env.uncache_agent.get_stats().get("req_count", 0))
    assert int(env.uncache_if.a_ready.value) == 0
    assert int(env.uncache_if.a_valid.value) == 1
    stalled_addr = int(env.uncache_if.a_bits_address.value)

    env.backend_model.set_wfi_req(1)
    env.step(16)
    req_during_wfi = int(env.uncache_agent.get_stats().get("req_count", 0))

    assert req_during_wfi == req_before
    assert int(env.uncache_if.a_bits_address.value) == stalled_addr
    assert int(env.uncache_if.a_valid.value) == 0
    assert _read_dut_signal(env, "io_backend_wfi_wfiSafe", 0) == 1

    env.backend_model.set_wfi_req(0)
    env.uncache_agent.set_a_ready(None)
    req_after = _wait_for_uncache_req(env)
    assert _wait_for_uncache_req_delta(env, 1)

    assert req_after > req_before
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) >= req_before + 2
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_pending_response_flushed_by_redirect(env):
    _prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=32)
    _initialize_mmio_fetch(env)

    assert _wait_for_uncache_req(env)
    _assert_ifu_uncache_state(env, _IFU_UNCACHE_WAIT_RESP)
    _assert_instr_uncache_entry_state_in(env, {_INSTR_UNCACHE_REFILL_REQ, _INSTR_UNCACHE_REFILL_RESP})
    req_before_redirect = int(env.uncache_agent.get_stats().get("req_count", 0))
    _force_redirect_to(env, _MMIO_BASE + 0x40)
    assert _wait_for_observed_pc(env, _MMIO_BASE + 0x40)
    assert _wait_for_uncache_resp(env, max_cycles=4000)

    assert int(env.uncache_agent.get_stats().get("resp_count", 0)) > 0
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) >= req_before_redirect
    assert not any(int(obs.pc) == _MMIO_BASE for obs in env.monitor.observations)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.parametrize(
    "fault_kwargs,blocked_exception",
    [
        ({"corrupt": 1}, "hwe"),
        ({"denied": 1}, "af"),
    ],
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_flushed_fault_response_does_not_report_exception(env, fault_kwargs, blocked_exception):
    _prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=32)
    env.uncache_agent.inject_response_fault_at(_MMIO_BASE, **fault_kwargs)
    _initialize_mmio_fetch(env)

    assert _wait_for_uncache_req(env)
    _assert_ifu_uncache_state(env, _IFU_UNCACHE_WAIT_RESP)
    _assert_instr_uncache_entry_state_in(env, {_INSTR_UNCACHE_REFILL_REQ, _INSTR_UNCACHE_REFILL_RESP})
    assert env.uncache_agent.pending
    ready_cycle = int(env.uncache_agent.pending[0].ready_cycle)
    while int(env.current_cycle) < ready_cycle - 2:
        env.step(1)
    _force_redirect_to(env, _MMIO_BASE + 0x40)
    assert _wait_for_observed_pc(env, _MMIO_BASE + 0x40)
    assert _wait_for_uncache_resp(env, max_cycles=4000)
    env.step(32)
    stats = env.uncache_agent.get_stats()

    assert int(stats.get("corrupt_resp_count", 0)) == (1 if fault_kwargs.get("corrupt") else 0)
    assert int(stats.get("denied_resp_count", 0)) == (1 if fault_kwargs.get("denied") else 0)
    assert env.monitor.exception_mark_count == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_consecutive_redirects_drop_older_pending_fetch(env):
    _prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=48)
    _initialize_mmio_fetch(env)

    assert _wait_for_uncache_req(env)
    _force_redirect_to(env, _MMIO_BASE + 0x40)
    env.step(4)
    _force_redirect_to(env, _MMIO_BASE + 0x80)

    assert _wait_for_observed_pc(env, _MMIO_BASE + 0x80, max_cycles=5000)
    assert not any(int(obs.pc) == _MMIO_BASE for obs in env.monitor.observations)
    assert int(env.uncache_agent.get_stats().get("resp_count", 0)) > 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-421")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_redirect_to_mmio_while_icache_response_pending(env):
    _prepare_normal_and_mmio_cnop_stream(env)
    env.icache_agent.configure(hit_latency=64, miss_latency=64, miss_rate=0.0, seed=1)
    _initialize_mmio_fetch(env, reset_vector=_NORMAL_BASE)

    assert _wait_for_icache_req(env)
    assert int(env.icache_agent.get_stats().get("pending", 0)) > 0

    _force_redirect_to(env, _MMIO_BASE)
    assert _wait_for_observed_pc(env, _MMIO_BASE, max_cycles=5000)
    assert _wait_for_request_addr(env, _MMIO_BASE, max_cycles=5000)

    env.step(128)

    assert int(env.icache_agent.get_stats().get("resp_beat_count", 0)) > 0
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) > 0
    assert not any(int(obs.pc) == _NORMAL_BASE for obs in env.monitor.observations)
    assert env.functional_coverage.key_hit("fetch_path_switch", "icache_to_mmio_clean")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_mmio_and_icache_pending_redirects_do_not_pollute_new_path(env):
    _prepare_normal_and_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=96)
    env.icache_agent.configure(hit_latency=96, miss_latency=96, miss_rate=0.0, seed=2)
    _initialize_mmio_fetch(env)

    assert _wait_for_request_addr(env, _MMIO_BASE)
    _force_redirect_to(env, _NORMAL_BASE)
    assert _wait_for_icache_req(env, max_cycles=5000)
    assert int(env.icache_agent.get_stats().get("pending", 0)) > 0

    target_pc = _MMIO_BASE + 0x80
    _force_redirect_to(env, target_pc)
    assert _wait_for_observed_pc(env, target_pc, max_cycles=6000)
    assert _wait_for_request_addr(env, target_pc, max_cycles=6000)
    env.step(160)

    assert int(env.uncache_agent.get_stats().get("resp_count", 0)) > 0
    assert int(env.icache_agent.get_stats().get("resp_beat_count", 0)) > 0
    assert not any(int(obs.pc) == _MMIO_BASE for obs in env.monitor.observations)
    assert not any(int(obs.pc) == _NORMAL_BASE for obs in env.monitor.observations)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-419")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_pbmt_nc_non_mmio_uses_uncache_path(env):
    expected_block, mapping = _prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=_NORMAL_BASE,
        paddr=_NORMAL_PHYS_BASE,
    )
    expected_block_pcs = [pc for pc, _, _ in expected_block]
    cfi_pc = mapping.vaddr + 0x10
    _initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    _configure_exec_pmp_4k(env, base_addr=mapping.paddr)
    _configure_exec_cacheable_pma_4k(env, base_addr=mapping.paddr)
    env.backend_model.set_can_accept(0)
    env.uncache_agent.set_a_ready(0)
    _force_redirect_to(env, mapping.vaddr)

    assert _wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert _wait_for_uncache_a_valid_addr(env, mapping.paddr, max_cycles=6000), {
        "ptw": env.ptw_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
        "mapping": mapping,
        "a_valid": int(env.uncache_if.a_valid.value),
        "a_addr": hex(int(env.uncache_if.a_bits_address.value)),
    }
    assert _require_first_dut_signal(env, _IFU_UNCACHE_TO_UNCACHE_VALID_SIGNALS) == 1
    env.uncache_agent.set_a_ready(None)
    assert _wait_for_request_addr(env, mapping.paddr, max_cycles=6000), env.uncache_agent.get_stats()
    assert _wait_for_resp_count(env, 1, max_cycles=6000), env.uncache_agent.get_stats()
    env.backend_model.set_can_accept(1)
    assert _wait_for_observed_pc_sequence(env, expected_block_pcs, max_cycles=12000), {
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-16:]],
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
        "mapping": mapping,
    }
    stats = env.uncache_agent.get_stats()
    expected_beat_addrs = [mapping.paddr + _UNCACHE_BEAT_BYTES * idx for idx in range(_FETCH_BLOCK_SIZE // _UNCACHE_BEAT_BYTES)]

    assert not env.memory.is_mmio(mapping.paddr)
    assert mapping.paddr in stats.get("request_addrs", [])
    assert all(addr in stats.get("request_addrs", []) for addr in expected_beat_addrs), stats
    _assert_observed_instrs(env, expected_block)
    observed_by_pc = {int(obs.pc): obs for obs in env.monitor.observations if int(obs.pc) in expected_block_pcs}
    assert int(observed_by_pc[cfi_pc].instr) == _JAL_X0_PLUS_4
    assert not bool(observed_by_pc[cfi_pc].is_rvc)
    assert int(env.ptw_agent.get_stats().get("resp_count", 0)) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-422")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_pbmt_io_waits_commit_on_cacheable_pma(env):
    expected_block, mapping = _prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=_NORMAL_BASE,
        paddr=_NORMAL_PHYS_BASE,
        pbmt=_PBMT_IO,
    )
    expected_block_pcs = [pc for pc, _, _ in expected_block]
    _initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    pbmte = getattr(env.dut, "io_tlbCsr_mPBMTE", None)
    if pbmte is None:
        pytest.skip("current generated DUT does not expose io_tlbCsr_mPBMTE required for PBMT.IO")
    pbmte.value = 1
    _configure_exec_pmp_4k(env, base_addr=mapping.paddr)
    _configure_exec_cacheable_pma_4k(env, base_addr=mapping.paddr)
    env.backend_model.set_can_accept(0)
    _force_redirect_to(env, mapping.vaddr)

    assert _wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert _wait_for_uncache_a_valid_addr(env, mapping.paddr, max_cycles=6000), {
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
        "mapping": mapping,
    }
    assert env.functional_coverage.key_hit("uncache_ordering", "pbmt_io_wait_commit")
    assert _require_first_dut_signal(env, _IFU_UNCACHE_TO_UNCACHE_VALID_SIGNALS) == 1
    assert _wait_for_request_addr(env, mapping.paddr, max_cycles=6000), env.uncache_agent.get_stats()
    assert _wait_for_resp_count(env, 1, max_cycles=6000), env.uncache_agent.get_stats()
    env.backend_model.set_can_accept(1)
    assert _wait_for_observed_pc_sequence(env, expected_block_pcs, max_cycles=12000), {
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-16:]],
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_pbmt_nc_after_ibuffer_backpressure_can_output_multiple_cfvec_lanes(env):
    expected_block, mapping = _prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=_NORMAL_BASE,
        paddr=_NORMAL_PHYS_BASE,
    )
    expected_by_pc = {pc: (instr, is_rvc) for pc, instr, is_rvc in expected_block}
    expected_block_pcs = list(expected_by_pc)
    env.backend_model.set_can_accept(0)
    _initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    _configure_exec_pmp_4k(env, base_addr=mapping.paddr)
    _configure_exec_cacheable_pma_4k(env, base_addr=mapping.paddr)
    _force_redirect_to(env, mapping.vaddr)

    assert _wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert _wait_for_request_addr(env, mapping.paddr, max_cycles=6000), env.uncache_agent.get_stats()
    assert _wait_for_resp_count(env, 1, max_cycles=6000), env.uncache_agent.get_stats()
    target_req_count = _FETCH_BLOCK_SIZE // _UNCACHE_BEAT_BYTES
    commit_count_before = int(env.backend_model.commit_count)
    saw_req_progress_without_commit = False
    for _ in range(12000):
        assert int(env.backend_ctrl_if.commit_valid.value) == 0
        req_count_before = int(env.uncache_agent.get_stats().get("req_count", 0))
        if req_count_before >= target_req_count:
            break
        env.step(1)
        assert int(env.backend_ctrl_if.commit_valid.value) == 0
        req_count_after = int(env.uncache_agent.get_stats().get("req_count", 0))
        saw_req_progress_without_commit |= req_count_after > req_count_before
        if req_count_after >= target_req_count:
            break

    assert int(env.backend_model.commit_count) == commit_count_before
    assert saw_req_progress_without_commit
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) >= target_req_count, {
        "uncache": env.uncache_agent.get_stats(),
        "mapping": mapping,
    }
    blocked_request_addrs = env.uncache_agent.get_stats().get("request_addrs", [])
    assert mapping.paddr in blocked_request_addrs, env.uncache_agent.get_stats()
    assert mapping.paddr + _UNCACHE_BEAT_BYTES in blocked_request_addrs, {
        "blocked_request_addrs": [hex(addr) for addr in blocked_request_addrs],
        "uncache": env.uncache_agent.get_stats(),
        "mapping": mapping,
    }
    env.step(32)

    env.backend_model.set_can_accept(1)
    cfvec_cycles = _collect_cfvec_cycles(env, max_cycles=512)
    expected_pc_set = set(expected_block_pcs)
    observed_pcs = {
        int(slot["pc"])
        for cycle in cfvec_cycles
        for slot in cycle["slots"]
        if int(slot["pc"]) in expected_pc_set
    }
    max_lanes = max((len(cycle["slots"]) for cycle in cfvec_cycles), default=0)
    multi_lane_cycles = [cycle for cycle in cfvec_cycles if len(cycle["slots"]) > 1]
    target_multi_lane_cycles = [
        cycle
        for cycle in multi_lane_cycles
        if sum(1 for slot in cycle["slots"] if int(slot["pc"]) in expected_pc_set) > 1
    ]

    assert expected_block_pcs[0] in observed_pcs, {
        "cfvec_cycles": cfvec_cycles[:16],
        "uncache": env.uncache_agent.get_stats(),
        "ptw": env.ptw_agent.get_stats(),
    }
    assert max_lanes > 1, {
        "max_lanes": int(max_lanes),
        "multi_lane_cycles": multi_lane_cycles[:8],
        "cfvec_cycles": cfvec_cycles[:32],
    }
    assert target_multi_lane_cycles, {
        "multi_lane_cycles": multi_lane_cycles[:8],
        "cfvec_cycles": cfvec_cycles[:32],
    }
    for slot in target_multi_lane_cycles[0]["slots"]:
        pc = int(slot["pc"])
        if pc not in expected_by_pc:
            continue
        instr, is_rvc = expected_by_pc[pc]
        assert int(slot["instr"]) == int(instr), slot
        assert bool(slot["is_rvc"]) == bool(is_rvc), slot
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-418")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_pbmt_nc_mmio_pma_second_fetch_waits_commit(env):
    _expected_block, mapping = _prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=_NORMAL_BASE,
        paddr=_NORMAL_PHYS_BASE,
    )
    env.backend_model.set_can_accept(0)
    _initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    _configure_exec_pmp_4k(env, base_addr=mapping.paddr)
    _configure_exec_mmio_pma_4k(env, base_addr=mapping.paddr)
    _force_redirect_to(env, mapping.vaddr)

    assert _wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    ptw_resp = env.ptw_agent.get_last_drive_expectation()
    assert ptw_resp is not None
    assert int(ptw_resp["resp"].get("s1_entry_pbmt", 0)) == _PBMT_NC, ptw_resp
    assert _wait_for_request_addr(env, mapping.paddr, max_cycles=6000), env.uncache_agent.get_stats()
    assert _wait_for_resp_count(env, 1, max_cycles=6000), env.uncache_agent.get_stats()
    req_before_commit = int(env.uncache_agent.get_stats().get("req_count", 0))
    commit_count_before = int(env.backend_model.commit_count)
    env.step(128)
    req_without_commit = int(env.uncache_agent.get_stats().get("req_count", 0))

    assert int(env.backend_ctrl_if.commit_valid.value) == 0
    assert int(env.backend_model.commit_count) == commit_count_before
    assert req_without_commit == req_before_commit

    env.backend_model.set_can_accept(1)
    req_after_commit = _wait_for_uncache_req(env, max_cycles=6000)

    assert req_after_commit > req_without_commit, {
        "uncache": env.uncache_agent.get_stats(),
        "mapping": mapping,
    }
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_pbmt_nc_real_bin_uses_generated_sv39_vaddr_mapping(env, tmp_path):
    bin_path = tmp_path / "pbmt_nc_real_payload.bin"
    payload = bytearray()
    for _ in range(8):
        payload.extend(int(_CNOP).to_bytes(2, "little"))
    payload.extend(int(_JAL_X0_PLUS_4).to_bytes(4, "little"))
    while len(payload) < _FETCH_BLOCK_SIZE:
        payload.extend(int(_CNOP).to_bytes(2, "little"))
    payload.extend(int(_CNOP).to_bytes(2, "little") * 64)
    bin_path.write_bytes(bytes(payload))

    expected_block, mapping = _prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        bin_path=bin_path,
        paddr_pages=(_NORMAL_PHYS_BASE,),
    )
    expected_block_pcs = [pc for pc, _, _ in expected_block]
    cfi_pc = mapping.vaddr + 0x10
    _initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    _configure_exec_attrs_for_mapping(env, mapping)
    _force_redirect_to(env, mapping.vaddr)

    assert _wait_for_ptw_resp(env, max_cycles=6000), {"mapping": mapping, "ptw": env.ptw_agent.get_stats()}
    assert _wait_for_request_addr(env, mapping.paddr, max_cycles=6000), {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
    }
    assert _wait_for_observed_pc_sequence(env, expected_block_pcs, max_cycles=12000), {
        "mapping": mapping,
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-20:]],
    }

    observed_by_pc = {int(obs.pc): obs for obs in env.monitor.observations if int(obs.pc) in expected_block_pcs}
    assert not env.memory.is_mmio(mapping.paddr)
    assert mapping.vaddr != mapping.paddr
    _assert_observed_instrs(env, expected_block)
    assert int(observed_by_pc[cfi_pc].instr) == _JAL_X0_PLUS_4
    assert not bool(observed_by_pc[cfi_pc].is_rvc)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-420")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_pbmt_nc_pending_redirect_to_cacheable_non_mmio_has_enough_requests(env):
    nc_expected, _cacheable_pcs = _prepare_sv39_dual_nc_cacheable_stream(env)
    nc_pcs = [pc for pc, _, _ in nc_expected]
    env.uncache_agent.configure(latency=24, mmio_latency=24)
    env.icache_agent.configure(hit_latency=16, miss_latency=16, miss_rate=0.0, seed=3)
    env.backend_model.set_can_accept(0)
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    _configure_exec_attrs_16k(env, base_addr=0x80000000)
    _force_redirect_to(env, _NORMAL_BASE)

    assert _wait_for_request_addr(env, _NORMAL_PHYS_BASE, max_cycles=6000), env.uncache_agent.get_stats()
    assert _wait_for_uncache_req_count(env, _FETCH_BLOCK_SIZE // _UNCACHE_BEAT_BYTES, max_cycles=12000), (
        env.uncache_agent.get_stats()
    )
    assert int(env.uncache_agent.get_stats().get("pending", 0)) > 0
    same_page_switch_index = len(env.monitor.observations)
    same_page_uncache_req_count = int(env.uncache_agent.get_stats().get("req_count", 0))
    _remap_sv39_page_pbmt(env, vaddr=_NORMAL_BASE, paddr=_NORMAL_PHYS_BASE, pbmt=_PBMT_PMA)
    _pulse_sfence(env, addr=_NORMAL_BASE, rs1=1, rs2=0)
    assert int(env.uncache_agent.get_stats().get("pending", 0)) > 0
    _force_redirect_to(env, _NORMAL_BASE)
    env.backend_model.set_can_accept(1)
    assert _wait_for_icache_req_count(env, 2, max_cycles=12000), env.icache_agent.get_stats()
    assert _wait_for_observed_pc_sequence(env, nc_pcs[:8], max_cycles=12000), {
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-32:]],
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    same_page_recovery = list(env.monitor.observations)[same_page_switch_index:]
    observed_after_switch = {int(obs.pc): obs for obs in same_page_recovery}
    for pc, instr, is_rvc in nc_expected[:8]:
        obs = observed_after_switch[int(pc)]
        assert int(obs.instr) == int(instr), {
            "pc": hex(int(pc)),
            "expected_instr": hex(int(instr)),
            "observed_instr": hex(int(obs.instr)),
        }
        assert bool(obs.is_rvc) == bool(is_rvc)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == same_page_uncache_req_count, (
        env.uncache_agent.get_stats()
    )
    uncache_stats = env.uncache_agent.get_stats()
    icache_stats = env.icache_agent.get_stats()

    assert int(uncache_stats.get("req_count", 0)) >= _FETCH_BLOCK_SIZE // _UNCACHE_BEAT_BYTES
    assert _NORMAL_PHYS_BASE in uncache_stats.get("request_addrs", [])
    assert int(icache_stats.get("req_count", 0)) >= 2, icache_stats
    recovery_index = _first_observed_index(env, nc_pcs[0])
    assert recovery_index >= 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-423")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_cacheable_pending_redirect_to_pbmt_nc_has_enough_requests(env):
    nc_expected, cacheable_pcs = _prepare_sv39_dual_nc_cacheable_stream(env)
    nc_pcs = [pc for pc, _, _ in nc_expected]
    env.uncache_agent.configure(latency=16, mmio_latency=16)
    env.icache_agent.configure(hit_latency=64, miss_latency=64, miss_rate=0.0, seed=4)
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_ALT_BASE)
    _configure_exec_attrs_16k(env, base_addr=0x80000000)
    _force_redirect_to(env, _NORMAL_ALT_BASE)

    assert _wait_for_icache_req_count(env, 2, max_cycles=12000), env.icache_agent.get_stats()
    assert int(env.icache_agent.get_stats().get("pending", 0)) > 0
    _pulse_sfence(env, addr=_NORMAL_ALT_BASE, rs1=1, rs2=0)
    assert int(env.icache_agent.get_stats().get("pending", 0)) > 0
    _force_redirect_to(env, _NORMAL_BASE)
    assert _wait_for_request_addr(env, _NORMAL_PHYS_BASE, max_cycles=6000), env.uncache_agent.get_stats()
    assert _wait_for_resp_count(env, _FETCH_BLOCK_SIZE // _UNCACHE_BEAT_BYTES, max_cycles=12000), {
        "uncache": env.uncache_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
    }
    assert _wait_for_observed_pc_sequence(env, nc_pcs, max_cycles=12000), {
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-40:]],
        "uncache": env.uncache_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
    }
    _assert_observed_instrs(env, nc_expected)
    env.step(128)
    uncache_stats = env.uncache_agent.get_stats()
    icache_stats = env.icache_agent.get_stats()

    assert all(
        _NORMAL_PHYS_BASE + _UNCACHE_BEAT_BYTES * idx in uncache_stats.get("request_addrs", [])
        for idx in range(_FETCH_BLOCK_SIZE // _UNCACHE_BEAT_BYTES)
    ), uncache_stats
    assert int(uncache_stats.get("req_count", 0)) >= _FETCH_BLOCK_SIZE // _UNCACHE_BEAT_BYTES
    assert int(icache_stats.get("req_count", 0)) >= 2, icache_stats
    recovery_index = _first_observed_index(env, nc_pcs[0])
    assert recovery_index >= 0
    assert not any(int(obs.pc) in set(cacheable_pcs) for obs in list(env.monitor.observations)[recovery_index:])
    assert not env.monitor.get_errors()


@pytest.mark.funcov_tps("ATP-113")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_cacheable_non_mmio_uses_icache_path(env):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="atp-113-sv39-cacheable-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 256,
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, _NORMAL_BASE)

    commits = RunUntilCommitSequence(target=CommitTarget(target_count=6, max_cycles=6000)).run(env)

    assert commits >= 6, {
        "ptw": env.ptw_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    env.step(32)

    assert not env.memory.is_mmio(_NORMAL_PHYS_BASE)
    assert any(int(obs.pc) == _NORMAL_BASE for obs in env.monitor.observations)
    assert _NORMAL_PHYS_BASE not in env.uncache_agent.get_stats().get("request_addrs", [])
    assert int(env.ptw_agent.get_stats().get("resp_count", 0)) >= 1
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_bare_fetch_uses_identity_pa_without_ptw(env):
    _initialize_mmio_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="bare-identity-fetch-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 256,
        mode="bare",
        expected_path="cacheable",
        expected_result="normal",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_BASE,
                size=0x1000,
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, scenario.va)

    assert RunUntilCommitSequence(target=CommitTarget(target_count=6, max_cycles=6000)).run(env) >= 6
    env.step(32)

    assert int(env.ptw_agent.get_stats()["req_count"]) == 0
    assert any(
        int(record["address"]) == (scenario.pa & ~0x3F)
        for record in env.icache_agent.get_stats()["request_records"]
    )
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv39_revisit_uses_existing_translation_refill(env):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="sv39-revisit-translation-refill-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 256,
        max_ptw_requests_per_key=1,
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, scenario.va)

    assert RunUntilCommitSequence(target=CommitTarget(target_count=6, max_cycles=6000)).run(env) >= 6
    first_ptw_requests = int(env.ptw_agent.get_stats()["req_count"])
    first_pc_observations = sum(int(obs.pc) == scenario.va for obs in env.monitor.observations)
    assert first_ptw_requests == 1
    assert first_pc_observations >= 1

    _force_redirect_to(env, scenario.va)
    assert RunUntilCommitSequence(target=CommitTarget(target_count=6, max_cycles=6000)).run(env) >= 6
    env.step(32)

    assert int(env.ptw_agent.get_stats()["req_count"]) == first_ptw_requests
    assert sum(int(obs.pc) == scenario.va for obs in env.monitor.observations) > first_pc_observations
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv39_sector_lane_reuses_refill_on_adjacent_page(env):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="sv39-sector-lane-reuse-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 2049,
        page_count=2,
        s1_sector_lanes=(TranslationSectorLane(lane=1, ppn=(_NORMAL_PHYS_BASE >> 12) + 1),),
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
            TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE + 0x1000,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
            TranslationPmpPmaEntry(
                kind="pma",
                index=1,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE + 0x1000,
                size=0x1000,
            ),
        ),
    )
    result = TranslationScenarioSequence(
        actions=(
            TranslationScenarioPhase(scenario=scenario, page_indexes=(0,)),
            TranslationScenarioPhase(
                reuse_previous=True,
                page_indexes=(1,),
                expect_ptw=False,
            ),
        )
    ).run(env)

    assert [item["kind"] for item in result] == ["phase", "phase"]
    assert int(env.ptw_agent.get_stats()["req_count"]) == 1
    assert any(
        int(record["address"]) == ((_NORMAL_PHYS_BASE + 0x1000) & ~0x3F)
        for record in env.icache_agent.get_stats()["request_records"]
    )
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv39_invalid_sector_lane_rewalks_and_refetches(env):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="sv39-sector-lane-invalid-rewalk-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 2049,
        page_count=2,
        s1_sector_lanes=(
            TranslationSectorLane(lane=1, ppn=(_NORMAL_PHYS_BASE >> 12) + 1, valid=0, pte_present=1),
        ),
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
            TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE + 0x1000,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
            TranslationPmpPmaEntry(
                kind="pma",
                index=1,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE + 0x1000,
                size=0x1000,
            ),
        ),
    )
    result = TranslationScenarioSequence(
        actions=(
            TranslationScenarioPhase(scenario=scenario, page_indexes=(0,)),
            TranslationScenarioPhase(reuse_previous=True, page_indexes=(1,)),
        )
    ).run(env)

    assert [item["kind"] for item in result] == ["phase", "phase"]
    assert int(env.ptw_agent.get_stats()["req_count"]) >= 2
    assert env.translation_oracle.get_active()["expected_fault"] is None
    assert any(
        int(record["address"]) == ((_NORMAL_PHYS_BASE + 0x1000) & ~0x3F)
        for record in env.icache_agent.get_stats()["request_records"]
    )
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_translation_sequence_refills_after_sfence(env):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="sv39-sequence-sfence-retranslate-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 256,
        ptw_response_latency=64,
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
    )
    refilled_scenario = TranslationScenario(
        scenario_id="sv39-sequence-sfence-retranslate-new-pa-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_ALT_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 256,
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_ALT_PHYS_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_ALT_PHYS_BASE,
                size=0x1000,
            ),
        ),
    )
    result = TranslationScenarioSequence(
        actions=(
            TranslationScenarioPhase(
                scenario=scenario,
                wait_for_ptw_requests=1,
                wait_for_completion=False,
            ),
            TranslationSfenceAction(addr=scenario.va, rs1=1),
            TranslationScenarioPhase(scenario=refilled_scenario, allow_speculative_before_response=True),
        )
    ).run(env)

    assert [item["kind"] for item in result] == ["phase", "sfence", "phase"]
    assert int(env.ptw_agent.get_stats()["req_count"]) >= 2
    assert any(
        int(record["address"]) == (_NORMAL_ALT_PHYS_BASE & ~0x3F)
        for record in env.icache_agent.get_stats()["request_records"]
    )
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv39_only_stage1_uses_vs_stage_physical_address(env):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="sv39-only-stage1-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 256,
        s2xlate=1,
        s1_pte=TranslationPte(asid=5),
        vsatp_asid=5,
        priv_virt=1,
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, scenario.va)

    assert RunUntilCommitSequence(target=CommitTarget(target_count=6, max_cycles=6000)).run(env) >= 6
    env.step(32)

    assert int(env.ptw_agent.get_stats()["resp_count"]) >= 1
    assert any(
        int(record["address"]) == (scenario.pa & ~0x3F)
        for record in env.icache_agent.get_stats()["request_records"]
    )
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv39_only_stage2_uses_g_stage_physical_address(env):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="sv39-only-stage2-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 256,
        mode="bare",
        stage2_mode="sv39",
        s2xlate=2,
        s2_pte=TranslationPte(vmid=7),
        hgatp_vmid=7,
        priv_virt=1,
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, scenario.va)

    assert RunUntilCommitSequence(target=CommitTarget(target_count=6, max_cycles=6000)).run(env) >= 6
    env.step(32)

    assert int(env.ptw_agent.get_stats()["resp_count"]) >= 1
    assert any(
        int(record["address"]) == (scenario.pa & ~0x3F)
        for record in env.icache_agent.get_stats()["request_records"]
    )
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_tps("ATP-119")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv39_all_stage_uses_stage2_physical_address(env):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="atp-119-sv39-all-stage-dut",
        va=_NORMAL_BASE,
        gpa=_NORMAL_PHYS_BASE,
        pa=_NORMAL_ALT_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 256,
        s2xlate=3,
        s1_pte=TranslationPte(asid=5, vmid=7),
        s2_pte=TranslationPte(vmid=7),
        vsatp_asid=5,
        hgatp_vmid=7,
        priv_virt=1,
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_ALT_PHYS_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_ALT_PHYS_BASE,
                size=0x1000,
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, scenario.va)

    commits = RunUntilCommitSequence(target=CommitTarget(target_count=6, max_cycles=6000)).run(env)

    assert commits >= 6, {
        "ptw": env.ptw_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    env.step(32)

    assert int(env.ptw_agent.get_stats().get("resp_count", 0)) >= 1
    assert _NORMAL_PHYS_BASE not in env.uncache_agent.get_stats().get("request_addrs", [])
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv39_level1_superpage_uses_mapped_physical_address(env):
    va = 0x8020_0000
    pa = 0x8040_0000
    superpage_size = 0x20_0000
    _initialize_sv39_fetch(env, reset_vector=va)
    scenario = TranslationScenario(
        scenario_id="sv39-level1-superpage-dut",
        va=va,
        pa=pa,
        payload=int(_CNOP).to_bytes(2, "little") * 256,
        s1_pte=TranslationPte(level=1),
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=pa,
                size=superpage_size,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=pa,
                size=superpage_size,
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, va)

    commits = RunUntilCommitSequence(target=CommitTarget(target_count=6, max_cycles=6000)).run(env)

    assert commits >= 6
    assert int(env.ptw_agent.get_stats().get("resp_count", 0)) >= 1
    assert any(int(record["address"]) == pa for record in env.icache_agent.get_stats()["request_records"])
    assert any(int(obs.pc) == va for obs in env.monitor.observations)
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv48x4_uses_stage2_physical_address(env):
    va = 0xFFFF_8000_8020_0000
    gpa = 0x8040_0000
    pa = 0x8060_0000
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="sv48x4-all-stage-dut",
        va=va,
        gpa=gpa,
        pa=pa,
        payload=int(_CNOP).to_bytes(2, "little") * 256,
        mode="sv48",
        stage2_mode="sv48",
        s2xlate=3,
        s1_pte=TranslationPte(asid=5, vmid=7),
        s2_pte=TranslationPte(vmid=7),
        vsatp_asid=5,
        hgatp_vmid=7,
        priv_virt=1,
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=pa,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=pa,
                size=0x1000,
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, va)

    commits = RunUntilCommitSequence(target=CommitTarget(target_count=6, max_cycles=6000)).run(env)

    assert commits >= 6, {
        "ptw": env.ptw_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
        "translation": env.translation_oracle.get_stats(),
    }
    assert int(env.ptw_agent.get_stats().get("resp_count", 0)) >= 1
    assert any(int(record["address"]) == pa for record in env.icache_agent.get_stats()["request_records"])
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "scenario_id,s1_pf,s1_af,s2_gpf,s2_gaf",
    [
        pytest.param("atp-134-vs-leaf-gpf-dut", 0, 0, 1, 0, marks=pytest.mark.funcov_tps("ATP-134")),
        pytest.param("atp-140-s1-pf-s2-leaf-dut", 1, 0, 0, 0, marks=pytest.mark.funcov_tps("ATP-140")),
        pytest.param("atp-141-s1-af-s2-leaf-dut", 0, 1, 0, 0, marks=pytest.mark.funcov_tps("ATP-141")),
        pytest.param("atp-142-s1-pf-s2-gpf-dut", 1, 0, 1, 0, marks=pytest.mark.funcov_tps("ATP-142")),
        pytest.param("atp-143-s1-pf-s2-gaf-dut", 1, 0, 0, 1, marks=pytest.mark.funcov_tps("ATP-143")),
        pytest.param("atp-144-s1-af-s2-gpf-dut", 0, 1, 1, 0, marks=pytest.mark.funcov_tps("ATP-144")),
        pytest.param("atp-145-s1-af-s2-gaf-dut", 0, 1, 0, 1, marks=pytest.mark.funcov_tps("ATP-145")),
    ],
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv39_all_stage_response_fault_priority(env, scenario_id, s1_pf, s1_af, s2_gpf, s2_gaf):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id=scenario_id,
        va=_NORMAL_BASE,
        gpa=_NORMAL_PHYS_BASE,
        pa=_NORMAL_ALT_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 32,
        s2xlate=3,
        s1_pte=TranslationPte(asid=5, vmid=7),
        s2_pte=TranslationPte(vmid=7),
        vsatp_asid=5,
        hgatp_vmid=7,
        priv_virt=1,
        s1_pf=s1_pf,
        s1_af=s1_af,
        s2_gpf=s2_gpf,
        s2_gaf=s2_gaf,
        expected_path="fault",
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, scenario.va)

    for _ in range(6000):
        env.step(1)
        active = env.translation_oracle.get_active()
        if active is not None and active["fault_seen"]:
            break

    env.step(32)

    assert int(env.ptw_agent.get_stats().get("resp_count", 0)) >= 1
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_tps("ATP-124")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv39_execute_denied_reports_instruction_page_fault(env):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="atp-124-sv39-execute-denied-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 32,
        s1_pte=TranslationPte(v=1, r=1, x=0, a=1),
        expected_path="fault",
        expected_result="page_fault",
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, scenario.va)

    for _ in range(6000):
        env.step(1)
        active = env.translation_oracle.get_active()
        if active is not None and active["fault_seen"]:
            break

    env.step(32)

    assert int(env.ptw_agent.get_stats().get("resp_count", 0)) >= 1
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_tps("ATP-076")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv39_pmp_execute_denied_reports_instruction_access_fault(env):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    scenario = TranslationScenario(
        scenario_id="atp-076-sv39-pmp-execute-denied-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 32,
        expected_path="fault",
        expected_result="access_fault",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=False),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, scenario.va)

    for _ in range(6000):
        env.step(1)
        active = env.translation_oracle.get_active()
        if active is not None and active["fault_seen"]:
            break

    env.step(32)

    assert int(env.ptw_agent.get_stats().get("resp_count", 0)) >= 1
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_tps("ATP-090")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_sv39_cross_page_rvi_uses_second_page_pma_path(env):
    cross_page_va = _NORMAL_BASE + _SV39_PAGE_SIZE - 2
    cross_page_pa = _NORMAL_PHYS_BASE + _SV39_PAGE_SIZE - 2
    next_page_pa = _NORMAL_PHYS_BASE + _SV39_PAGE_SIZE
    payload = int(_ADDI_X0_X0_0).to_bytes(4, "little") + int(_CNOP).to_bytes(2, "little") * 64

    _initialize_sv39_fetch(env, reset_vector=cross_page_va)
    scenario = TranslationScenario(
        scenario_id="atp-090-cross-page-pma-dut",
        va=cross_page_va,
        pa=cross_page_pa,
        payload=payload,
        page_count=2,
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE,
                size=_SV39_PAGE_SIZE,
            ),
            TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=next_page_pa,
                size=_SV39_PAGE_SIZE,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE,
                size=_SV39_PAGE_SIZE,
            ),
            TranslationPmpPmaEntry(
                kind="pma",
                index=1,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=False),
                addr=next_page_pa,
                size=_SV39_PAGE_SIZE,
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(cross_page_va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, cross_page_va)

    assert _wait_for_request_addr(env, next_page_pa, max_cycles=12000), {
        "ptw": env.ptw_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    assert _wait_for_observed_pc(env, cross_page_va, max_cycles=12000), {
        "ptw": env.ptw_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-16:]],
    }

    observed = next(obs for obs in env.monitor.observations if int(obs.pc) == cross_page_va)
    icache_records = env.icache_agent.get_stats()["request_records"]
    uncache_stats = env.uncache_agent.get_stats()
    oracle_stats = env.assert_translation_scenario()

    assert any(int(record["address"]) == (cross_page_pa & ~0x3F) for record in icache_records), icache_records
    assert next_page_pa in uncache_stats["request_addrs"], uncache_stats
    assert int(observed.instr) == _ADDI_X0_X0_0
    assert not bool(observed.is_rvc)
    assert env.monitor.exception_mark_count == 0
    assert oracle_stats["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_tps("ATP-035")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_csr_changed_before_ptw_response_discards_stale_translation(env):
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    old_scenario = TranslationScenario(
        scenario_id="atp-035-old-context",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 32,
        ptw_response_latency=64,
        satp_asid=1,
        s1_pte=TranslationPte(asid=1),
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
    )
    new_scenario = TranslationScenario(
        scenario_id="atp-035-new-context",
        va=_NORMAL_BASE,
        pa=_NORMAL_ALT_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 32,
        ptw_response_latency=64,
        satp_asid=2,
        s1_pte=TranslationPte(asid=2),
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_ALT_PHYS_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True, atomic=True),
                addr=_NORMAL_ALT_PHYS_BASE,
                size=0x1000,
            ),
        ),
    )
    steps = TranslationScenarioSequence(
        actions=(
            TranslationScenarioPhase(
                scenario=old_scenario,
                wait_for_ptw_requests=1,
                wait_for_completion=False,
            ),
            TranslationScenarioPhase(
                scenario=new_scenario,
                wait_for_stale_responses=1,
            ),
        )
    ).run(env)

    stats = env.translation_oracle.get_stats()
    assert [step["scenario_id"] for step in steps if step["kind"] == "phase"] == [
        old_scenario.scenario_id,
        new_scenario.scenario_id,
    ]
    assert int(env.ptw_agent.get_stats().get("resp_count", 0)) >= 2
    assert any(record["kind"] == "stale_ptw_response" for record in stats["records"])
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_resend_first_beat_corrupt_suppresses_resend(env):
    _prepare_cross_beat_rvi_stream(env)
    env.uncache_agent.inject_response_fault_at(
        _MMIO_BASE,
        corrupt=1,
    )
    _initialize_mmio_fetch(env, reset_vector=_CROSS_BEAT_PC)

    assert _wait_for_request_addr(env, _MMIO_BASE)
    assert _wait_for_uncache_resp(env)
    assert _wait_for_monitor_exception(env)
    stats = env.uncache_agent.get_stats()

    assert stats.get("request_addrs", []).count(_MMIO_BASE) == 1
    assert (_MMIO_BASE + 8) not in stats.get("request_addrs", [])
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_resend_first_beat_denied_allows_resend(env):
    _prepare_cross_beat_rvi_stream(env)
    env.uncache_agent.inject_response_fault_at(
        _MMIO_BASE,
        denied=1,
    )
    _initialize_mmio_fetch(env, reset_vector=_CROSS_BEAT_PC)

    assert _wait_for_request_addr(env, _MMIO_BASE)
    assert _wait_for_request_addr(env, _MMIO_BASE + 8)
    assert _wait_for_resp_count(env, 2)
    stats = env.uncache_agent.get_stats()

    assert int(stats.get("denied_resp_count", 0)) == 1
    assert int(stats.get("corrupt_resp_count", 0)) == 0
    assert stats.get("request_addrs", []).count(_MMIO_BASE) == 1
    assert (_MMIO_BASE + 8) in stats.get("request_addrs", [])
    assert env.monitor.exception_mark_count == 0
    assert not env.monitor.get_errors()


@pytest.mark.parametrize("fault,exception", [("corrupt", "hwe"), ("denied", "af")])
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_resend_second_beat_fault_reports_exception(env, fault, exception):
    _prepare_cross_beat_rvi_stream(env)
    env.uncache_agent.inject_response_fault_at(
        _MMIO_BASE + 8,
        corrupt=1 if fault == "corrupt" else 0,
        denied=1 if fault == "denied" else 0,
    )
    _initialize_mmio_fetch(env, reset_vector=_CROSS_BEAT_PC)

    assert _wait_for_request_addr(env, _MMIO_BASE)
    assert _wait_for_request_addr(env, _MMIO_BASE + 8)
    assert _wait_for_resp_count(env, 2)
    assert _wait_for_monitor_exception(env)
    stats = env.uncache_agent.get_stats()

    assert _MMIO_BASE in stats.get("request_addrs", [])
    assert (_MMIO_BASE + 8) in stats.get("request_addrs", [])
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-417")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_page_tail_rvi_need_resend_rechecks_next_page(env):
    _prepare_cross_page_rvi_stream(env)
    # Keep the second-page response outstanding long enough to observe the
    # retimed half-RVI state, rather than proving only the final cfVec result.
    env.uncache_agent.configure(latency=2, mmio_latency=16)
    prev_half_samples = _register_prev_half_rvi_observer(env)
    _initialize_mmio_fetch(env, reset_vector=_CROSS_PAGE_PC)

    first_beat = _CROSS_PAGE_PC & ~(_UNCACHE_BEAT_BYTES - 1)
    next_page = _MMIO_BASE + _SV39_PAGE_SIZE
    assert _wait_for_request_addr(env, first_beat, max_cycles=5000), env.uncache_agent.get_stats()
    assert _wait_for_request_addr(env, next_page, max_cycles=5000), env.uncache_agent.get_stats()
    assert _wait_for_observed_pc(env, _CROSS_PAGE_PC, max_cycles=8000), {
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-16:]],
        "uncache": env.uncache_agent.get_stats(),
    }
    stats = env.uncache_agent.get_stats()
    observed = next(obs for obs in env.monitor.observations if int(obs.pc) == _CROSS_PAGE_PC)

    assert stats.get("request_addrs", []).count(first_beat) == 1
    assert stats.get("request_addrs", []).count(next_page) == 1
    assert int(observed.instr) == _ADDI_X0_X0_0
    assert not bool(observed.is_rvc)

    recovery_samples = list(prev_half_samples)
    expected_half_data = _ADDI_X0_X0_0 & 0xFFFF
    expected_half_pc = _CROSS_PAGE_PC >> 1
    pending_samples = _pending_uncache_samples(recovery_samples, next_page)
    assert len(pending_samples) >= 2, {
        "reason": "no multi-cycle next-page response stall was observed",
        "pending_samples": pending_samples,
        "uncache": stats,
    }
    pending_cycles = [int(sample["cycle"]) for sample in pending_samples]
    assert any(
        current == previous + 1
        for previous, current in zip(pending_cycles, pending_cycles[1:])
    ), {"pending_cycles": pending_cycles}

    need_resend_samples = [
        sample
        for sample in recovery_samples
        if sample["last_request_addr"] == first_beat and int(sample["need_resend"]) == 1
    ]
    assert need_resend_samples, {
        "reason": "first-page RVI response did not expose needResend",
        "samples": recovery_samples[-16:],
    }

    transaction_samples = [
        sample
        for sample in recovery_samples
        if sample["last_request_addr"] in {first_beat, next_page}
    ]
    assert any(sample["s0"] == 1 for sample in transaction_samples), {
        "reason": "s0 never recorded the first-page RVI half",
        "samples": transaction_samples[-16:],
    }
    s1_recovery = [
        sample
        for sample in transaction_samples
        if sample["last_request_addr"] == first_beat
        and sample["s1"] == 1
        and int(sample["s1_data"]) == expected_half_data
        and int(sample["s1_pc"]) == expected_half_pc
    ]
    assert s1_recovery, {
        "reason": "s1 did not capture the first-page half-RVI payload",
        "samples": transaction_samples[-16:],
        "expected_half_data": expected_half_data,
        "expected_half_pc": expected_half_pc,
    }
    s2_recovery = [sample for sample in pending_samples if sample["s2"] == 1]
    assert s2_recovery, {
        "reason": "s2 never carried prevHalfRvi during the next-page response",
        "pending_samples": pending_samples,
    }

    # The delayed next-page response must hold the assembled instruction's
    # source half and PC on every cycle until the response is consumed.
    for sample in pending_samples:
        assert int(sample["s2"]) == 1, {"sample": sample, "reason": "s2 half-RVI flag dropped while stalled"}
        assert int(sample["s2_data"]) == expected_half_data, {
            "sample": sample,
            "expected_half_data": expected_half_data,
        }
        assert int(sample["s2_pc"]) == expected_half_pc, {
            "sample": sample,
            "expected_half_pc": expected_half_pc,
        }

    # Exercise backend redirect after recovery and make sure the old saved
    # half cannot leak into the redirected path.
    observations_before_redirect = len(env.monitor.observations)
    redirect_queued_cycle = int(env.current_cycle)
    redirect_target = _MMIO_BASE + 0x40
    _force_redirect_to(env, redirect_target)
    assert _wait_for_observed_pc(env, redirect_target, max_cycles=5000)
    redirect_samples = [
        sample for sample in prev_half_samples if int(sample["cycle"]) >= redirect_queued_cycle
    ]
    redirect_cycles = [
        int(sample["cycle"])
        for sample in redirect_samples
        if int(sample["backend_redirect"]) == 1
    ]
    assert redirect_cycles, {
        "reason": "backend redirect was not observable",
        "samples": redirect_samples[-16:],
    }
    first_redirect_cycle = min(redirect_cycles)
    redirect_clear_window = [
        sample
        for sample in redirect_samples
        if first_redirect_cycle <= int(sample["cycle"]) <= first_redirect_cycle + 3
    ]
    # S2 payload registers are RegEnable state; redirect invalidates them via
    # s2_valid rather than requiring the physically stale bits to be zero.
    assert any(
        int(sample["s0"]) == 0
        and int(sample["s1_data"]) == 0
        and int(sample["s1_pc"]) == 0
        and int(sample["s2_valid"]) == 0
        for sample in redirect_clear_window
    ), {
        "reason": "backend redirect did not clear saved half-RVI payload",
        "redirect_clear_window": redirect_clear_window,
    }
    assert not any(
        int(obs.pc) == _CROSS_PAGE_PC
        for obs in env.monitor.observations[observations_before_redirect:]
    )
    assert not env.monitor.get_errors()
    assert env.functional_coverage.key_hit("uncache_page_boundary", "rvi_tail_resend_next_page")


@pytest.mark.funcov_bins("BIN-416")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_page_tail_rvc_does_not_fetch_next_page_before_delivery(env):
    _prepare_cross_page_rvc_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=16)
    prev_half_samples = _register_prev_half_rvi_observer(env)
    _initialize_mmio_fetch(env, reset_vector=_CROSS_PAGE_PC)

    first_beat = _CROSS_PAGE_PC & ~(_UNCACHE_BEAT_BYTES - 1)
    next_page = _MMIO_BASE + _SV39_PAGE_SIZE
    assert _wait_for_request_addr(env, first_beat, max_cycles=5000), env.uncache_agent.get_stats()
    assert _wait_for_observed_pc(env, _CROSS_PAGE_PC, max_cycles=8000), {
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-16:]],
        "uncache": env.uncache_agent.get_stats(),
    }
    observed = next(obs for obs in env.monitor.observations if int(obs.pc) == _CROSS_PAGE_PC)
    stats = env.uncache_agent.get_stats()

    assert int(observed.instr) == _ADDI_X0_X0_0
    assert bool(observed.is_rvc)
    assert stats.get("request_addrs", []).count(first_beat) == 1
    assert next_page not in stats.get("request_addrs", [])
    pending_samples = _pending_uncache_samples(prev_half_samples, first_beat)
    assert len(pending_samples) >= 2, {
        "reason": "no multi-cycle RVC response window was observed",
        "pending_samples": pending_samples,
        "uncache": stats,
    }
    pending_cycles = [int(sample["cycle"]) for sample in pending_samples]
    assert any(
        current == previous + 1
        for previous, current in zip(pending_cycles, pending_cycles[1:])
    ), {"pending_cycles": pending_cycles}
    rvc_window = [
        sample
        for sample in prev_half_samples
        if min(pending_cycles) - 2 <= int(sample["cycle"]) <= max(pending_cycles) + 2
    ]
    assert not any(
        int(sample["s0"]) or int(sample["s1"]) or int(sample["s2"])
        for sample in rvc_window
    ), {
        "reason": "RVC page-tail path unexpectedly carried prevHalfRvi state",
        "samples": rvc_window,
    }
    assert not any(int(sample["need_resend"]) for sample in rvc_window), {
        "reason": "RVC page-tail response unexpectedly asserted needResend",
        "samples": rvc_window,
    }
    assert env.functional_coverage.key_hit("uncache_page_boundary", "rvc_tail_no_resend_before_delivery")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_mmio_commit_order_waits_last_commit(env):
    _prepare_mmio_cnop_stream(env)
    env.backend_model.set_can_accept(0)
    _initialize_mmio_fetch(env)

    assert _wait_for_uncache_req(env)
    assert _wait_for_uncache_resp(env)
    req_before_commit = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.step(64)
    req_without_commit = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.backend_model.set_can_accept(1)
    req_after_commit = _wait_for_uncache_req(env)

    assert req_without_commit == req_before_commit
    assert req_after_commit > req_without_commit
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_wfi_during_mmio_commit_gate_blocks_next_request(env):
    _prepare_mmio_cnop_stream(env)
    _initialize_mmio_fetch(env)

    assert _wait_for_uncache_req(env)
    assert _wait_for_uncache_resp(env)
    env.backend_model.set_can_accept(0)
    env.backend_model.set_wfi_req(1)
    req_before = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.step(64)
    req_during_gate_and_wfi = int(env.uncache_agent.get_stats().get("req_count", 0))

    env.backend_model.set_wfi_req(0)
    env.step(16)
    req_still_commit_gated = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.backend_model.set_can_accept(1)
    req_after_commit = _wait_for_uncache_req(env)

    assert req_during_gate_and_wfi == req_before
    assert req_still_commit_gated == req_before
    assert req_after_commit > req_before
    assert not env.monitor.get_errors()
