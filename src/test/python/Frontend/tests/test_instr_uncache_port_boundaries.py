from __future__ import annotations

import os
import random
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

import pytest

from env.sequences import InjectRedirectSequence, LoadProgramSequence, RunUntilCommitSequence
from env.transactions import CommitTarget, ProgramImage, RedirectTxn


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
_IFU_UNCACHE_INVALID = 0
_IFU_UNCACHE_WAIT_LAST_COMMIT = 1
_IFU_UNCACHE_SEND_REQ = 2
_IFU_UNCACHE_WAIT_RESP = 3
_INSTR_UNCACHE_INVALID = 0
_INSTR_UNCACHE_REFILL_REQ = 1
_INSTR_UNCACHE_REFILL_RESP = 2
_INSTR_UNCACHE_SEND_RESP = 3
_PMPCFG0 = 0x3A0
_PMPADDR0 = 0x3B0
_PMACFG0 = 0x7C0
_PMAADDR0 = 0x7C8
_RWX_NAPOT_4K = 0x1F
_PMA_RWX_MMIO_NAPOT_4K = _RWX_NAPOT_4K
_PMA_RWX_CACHEABLE_NAPOT_4K = 0x7F
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
        pbmt=_PBMT_NC,
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


def _write_distributed_csr(env, *, addr: int, data: int) -> None:
    env.dut.io_csrCtrl_distribute_csr_w_bits_addr.value = int(addr)
    env.dut.io_csrCtrl_distribute_csr_w_bits_data.value = int(data)
    env.dut.io_csrCtrl_distribute_csr_w_valid.value = 1
    env.step(1)
    env.dut.io_csrCtrl_distribute_csr_w_valid.value = 0
    env.step(4)


def _napot_addr(*, base_addr: int, size: int) -> int:
    return (int(base_addr) + (int(size) // 2 - 1)) >> 2


def _configure_exec_cacheable_pma(env, *, base_addr: int, size: int) -> None:
    napot_addr = _napot_addr(base_addr=int(base_addr), size=int(size))
    _write_distributed_csr(env, addr=_PMACFG0, data=_PMA_RWX_CACHEABLE_NAPOT_4K)
    _write_distributed_csr(env, addr=_PMAADDR0, data=napot_addr)


def _configure_exec_mmio_pma(env, *, base_addr: int, size: int) -> None:
    napot_addr = _napot_addr(base_addr=int(base_addr), size=int(size))
    _write_distributed_csr(env, addr=_PMACFG0, data=_PMA_RWX_MMIO_NAPOT_4K)
    _write_distributed_csr(env, addr=_PMAADDR0, data=napot_addr)


def _configure_exec_pmp(env, *, base_addr: int, size: int) -> None:
    napot_addr = _napot_addr(base_addr=int(base_addr), size=int(size))
    _write_distributed_csr(env, addr=_PMPCFG0, data=_RWX_NAPOT_4K)
    _write_distributed_csr(env, addr=_PMPADDR0, data=napot_addr)


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


def _wait_for_frontend_exception(env, bin_name: str, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if env.functional_coverage.key_hit("frontend_exception_type", str(bin_name)):
            return True
        env.step(1)
    return env.functional_coverage.key_hit("frontend_exception_type", str(bin_name))


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
_IFU_UNCACHE_IS_MMIO_SIGNALS = (
    "Frontend_top.Frontend.inner_ifu.uncacheUnit.isMmio",
    "TOP.Frontend_top.Frontend.inner_ifu.uncacheUnit.isMmio",
)
_IFU_UNCACHE_TO_UNCACHE_VALID_SIGNALS = (
    "auto_inner_instrUncache_client_out_a_valid",
)
_IFU_UNCACHE_MMIO_COMMIT_VALID_SIGNALS = (
    "Frontend_top.Frontend.inner_ifu.uncacheUnit.io_mmioCommitRead_valid",
    "TOP.Frontend_top.Frontend.inner_ifu.uncacheUnit.io_mmioCommitRead_valid",
    "Frontend_top.Frontend.inner_ifu.io_toFtq_mmioCommitRead_valid",
    "TOP.Frontend_top.Frontend.inner_ifu.io_toFtq_mmioCommitRead_valid",
)
_INSTR_UNCACHE_ENTRY_STATE_SIGNALS = (
    "Frontend_top.Frontend.inner_instrUncache.entries_0.state",
    "TOP.Frontend_top.Frontend.inner_instrUncache.entries_0.state",
)


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
    env.dut.io_sfence_bits_addr.value = int(addr)
    env.dut.io_sfence_bits_rs1.value = int(rs1)
    env.dut.io_sfence_bits_rs2.value = int(rs2)
    env.dut.io_sfence_bits_id.value = 0
    env.dut.io_sfence_bits_hv.value = 0
    env.dut.io_sfence_bits_hg.value = 0
    env.dut.io_sfence_valid.value = 1
    assert _read_dut_signal(env, "io_sfence_valid", 0) == 1
    env.step(max(1, int(cycles)))
    env.dut.io_sfence_valid.value = 0


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
    assert env.functional_coverage.key_hit("uncache_req_state", "normal_fire")
    assert env.functional_coverage.key_hit("uncache_req_state", "a_ready_backpressure")
    assert env.functional_coverage.key_hit("uncache_resp_type", "clean")
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
    assert _wait_for_frontend_exception(env, expected_exception)
    assert env.functional_coverage.key_hit("uncache_resp_type", expected_resp_type)
    if expected_path_exception is not None:
        assert env.functional_coverage.key_hit("fetch_path_x_exception", expected_path_exception)
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
    assert env.functional_coverage.key_hit("uncache_req_state", "wfi_blocked")
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
    assert env.functional_coverage.key_hit("uncache_req_state", "a_ready_backpressure")
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
    assert env.functional_coverage.key_hit("uncache_flush_flow", "redirect_flush_pending")
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
    assert env.functional_coverage.key_hit("uncache_flush_flow", "redirect_flush_fault")
    assert not env.functional_coverage.key_hit("frontend_exception_type", blocked_exception)
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
    assert env.functional_coverage.key_hit("uncache_flush_flow", "consecutive_redirect_pending")
    assert not env.monitor.get_errors()


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
    assert _require_first_dut_signal(env, _IFU_UNCACHE_IS_MMIO_SIGNALS) == 0
    assert _require_first_dut_signal(env, _IFU_UNCACHE_MMIO_COMMIT_VALID_SIGNALS) == 0
    assert _require_first_dut_signal(env, _IFU_UNCACHE_TO_UNCACHE_VALID_SIGNALS) == 1
    env.uncache_agent.set_a_ready(None)
    assert _wait_for_request_addr(env, mapping.paddr, max_cycles=6000), env.uncache_agent.get_stats()
    assert _wait_for_resp_count(env, 1, max_cycles=6000), env.uncache_agent.get_stats()
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


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_cacheable_non_mmio_uses_icache_path(env):
    _prepare_sv39_mapped_cnop_stream(env, vaddr=_NORMAL_BASE, paddr=_NORMAL_PHYS_BASE, pbmt=_PBMT_PMA)
    _initialize_sv39_fetch(env, reset_vector=_NORMAL_BASE)
    _configure_exec_pmp_4k(env, base_addr=_NORMAL_PHYS_BASE)
    _configure_exec_cacheable_pma_4k(env, base_addr=_NORMAL_PHYS_BASE)
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
    assert _wait_for_frontend_exception(env, "hwe")
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
    assert env.functional_coverage.key_hit("uncache_resend_flow", "first_denied_resend")
    assert not env.functional_coverage.key_hit("frontend_exception_type", "af")
    assert not env.functional_coverage.key_hit("frontend_exception_type", "hwe")
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
    assert _wait_for_frontend_exception(env, exception)
    stats = env.uncache_agent.get_stats()

    assert _MMIO_BASE in stats.get("request_addrs", [])
    assert (_MMIO_BASE + 8) in stats.get("request_addrs", [])
    assert env.functional_coverage.key_hit("uncache_resend_flow", "second_beat_fault")
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_page_tail_rvi_need_resend_rechecks_next_page(env):
    _prepare_cross_page_rvi_stream(env)
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
    assert env.functional_coverage.key_hit("uncache_req_state", "wfi_blocked")
    assert not env.monitor.get_errors()
