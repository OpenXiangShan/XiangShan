from __future__ import annotations

import os
import random
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

from env.sequences import LoadProgramSequence
from env.core.transactions import ProgramImage
from env.support import PmpPmaConfig

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

_INSTR_UNCACHE_REFILL_RESP = 2

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

_IFU_UNCACHE_TO_UNCACHE_VALID_SIGNALS = (
    "auto_inner_instrUncache_client_out_a_valid",
)

_IFU_PREV_HALF_RVI_SIGNALS = {
    "s0": (
        "Frontend_top.Frontend.inner_ifu.s0_prevEndIsHalfRvi",
        "TOP.Frontend_top.Frontend.inner_ifu.s0_prevEndIsHalfRvi",
    ),
    "s1": (
        "Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviInfo_valid",
        "TOP.Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviInfo_valid",
    ),
    "s1_data": (
        "Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviInfo_bits_data",
        "TOP.Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviInfo_bits_data",
    ),
    "s1_pc": (
        "Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviInfo_bits_pc_addr",
        "TOP.Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviInfo_bits_pc_addr",
    ),
    "s2": (
        "Frontend_top.Frontend.inner_ifu.s2_prevEndIsHalfRviInfo_valid",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_prevEndIsHalfRviInfo_valid",
    ),
    "s2_valid": (
        "Frontend_top.Frontend.inner_ifu.s2_valid_valid",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_valid_valid",
    ),
    "s2_data": (
        "Frontend_top.Frontend.inner_ifu.s2_prevEndIsHalfRviInfo_bits_data",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_prevEndIsHalfRviInfo_bits_data",
    ),
    "s2_pc": (
        "Frontend_top.Frontend.inner_ifu.s2_prevEndIsHalfRviInfo_bits_pc_addr",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_prevEndIsHalfRviInfo_bits_pc_addr",
    ),
}

_IFU_S1_VALID_SIGNALS = (
    "Frontend_top.Frontend.inner_ifu.s1_valid",
    "TOP.Frontend_top.Frontend.inner_ifu.s1_valid",
)

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
        "s1_pipeline_valid": _require_first_dut_signal(env, _IFU_S1_VALID_SIGNALS),
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


def _replace_u32_after_redirect_flush(
    env,
    address: int,
    value: int,
    *,
    redirect_target: int,
    reason: str,
) -> None:
    env.backend_model.set_can_accept(0)
    env.step(2)
    assert not env.monitor.get_errors()

    env.clock_reset.io_fencei.value = 1
    env.backend_model.inject_redirect(
        int(redirect_target),
        str(reason),
        delay_cycles=0,
    )
    env.step(3)
    assert not env.monitor.get_errors()
    env.memory.write_u32(int(address), int(value))
    env.step(2)
    env.clock_reset.io_fencei.value = 0
    env.step(2)
    env.backend_model.set_can_accept(1)

def _pulse_sfence(env, *, addr: int = 0, rs1: int = 0, rs2: int = 0, cycles: int = 1) -> None:
    env.pulse_sfence(addr=addr, rs1=rs1, rs2=rs2, cycles=cycles)
