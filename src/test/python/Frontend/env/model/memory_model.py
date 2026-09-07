from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Tuple

from ..support.exceptions import AddressError

DEFAULT_MMIO_RANGES: Tuple[Tuple[int, int], ...] = (
    (0x00000000, 0x10000000),
    (0x10000000, 0x20000000),
)


def _nop_pattern_byte(addr: int) -> int:
    return 0x13 if (addr & 0x3) == 0 else 0x00


_NEXUS_AM_COPY_LOADER_MARKERS: Tuple[Tuple[int, bytes], ...] = (
    (0x32, bytes.fromhex("33868440a285")),
    (0x50, bytes.fromhex("0f100000")),
    (0x60, bytes.fromhex("4264e260a2640561")),
)


def _sign_extend(value: int, width: int) -> int:
    sign = 1 << (width - 1)
    return (int(value) ^ sign) - sign


def _decode_auipc_i_target(
    data: bytes,
    base_addr: int,
    auipc_offset: int,
    lower_offset: int,
    *,
    register: int,
    lower_opcode: int = 0x13,
    lower_rd: Optional[int] = None,
) -> int:
    upper = int.from_bytes(data[auipc_offset : auipc_offset + 4], "little")
    lower = int.from_bytes(data[lower_offset : lower_offset + 4], "little")
    expected_lower_rd = register if lower_rd is None else int(lower_rd)
    if (
        (upper & 0x7F) != 0x17
        or ((upper >> 7) & 0x1F) != register
        or (lower & 0x7F) != lower_opcode
        or ((lower >> 12) & 0x7) != 0
        or ((lower >> 15) & 0x1F) != register
        or ((lower >> 7) & 0x1F) != expected_lower_rd
    ):
        raise ValueError(
            f"malformed Nexus-AM copy loader address pair: offsets=0x{auipc_offset:x}/0x{lower_offset:x}"
        )
    target = (
        int(base_addr)
        + int(auipc_offset)
        + _sign_extend(upper & 0xFFFFF000, 32)
        + _sign_extend((lower >> 20) & 0xFFF, 12)
    )
    return target & ~1 if lower_opcode == 0x67 else target


@dataclass
class PTE:
    ppn: int
    v: int = 1
    r: int = 1
    w: int = 0
    x: int = 1
    u: int = 0
    g: int = 0
    a: int = 1
    d: int = 0
    n: int = 0
    level: int = 0
    asid: int = 0
    vmid: int = 0
    pbmt: int = 0


class MemoryModel:
    def __init__(
        self,
        *,
        mmio_ranges: Optional[Iterable[Tuple[int, int]]] = None,
        strict_align: bool = False,
    ) -> None:
        self.mem: Dict[int, int] = {}
        self.mmio_ranges: List[Tuple[int, int]] = list(mmio_ranges or DEFAULT_MMIO_RANGES)
        self.strict_align = strict_align
        self.events: List[dict] = []

    def clear(self) -> None:
        self.mem.clear()
        self.events.clear()

    def _check_addr(self, addr: int) -> None:
        if addr < 0:
            raise AddressError(f"negative address: {addr}")

    def is_mmio(self, addr: int) -> bool:
        self._check_addr(addr)
        for lo, hi in self.mmio_ranges:
            if lo <= addr <= hi:
                return True
        return False

    def load_bin(self, bin_data: bytes, base_addr: int) -> None:
        self._check_addr(base_addr)
        for off, val in enumerate(bin_data):
            self.mem[base_addr + off] = int(val) & 0xFF

    def load_file(self, path: str, base_addr: int) -> None:
        with open(path, "rb") as f:
            self.load_bin(f.read(), base_addr)

    def materialize_nexus_am_loader_payload(self, bin_data: bytes, base_addr: int) -> int:
        if any(
            len(bin_data) < offset + len(marker)
            or bin_data[offset : offset + len(marker)] != marker
            for offset, marker in _NEXUS_AM_COPY_LOADER_MARKERS
        ):
            return 0

        destination = _decode_auipc_i_target(bin_data, base_addr, 0x38, 0x3C, register=10)
        jump_target = _decode_auipc_i_target(
            bin_data,
            base_addr,
            0x68,
            0x6C,
            register=6,
            lower_opcode=0x67,
            lower_rd=0,
        )
        source_start = _decode_auipc_i_target(bin_data, base_addr, 0x72, 0x76, register=10)
        source_end = _decode_auipc_i_target(bin_data, base_addr, 0x7A, 0x7E, register=11)
        image_end = int(base_addr) + len(bin_data)
        if not int(base_addr) <= source_start < source_end <= image_end:
            raise ValueError(
                "Nexus-AM copy loader payload is outside loaded program: "
                f"program=[0x{int(base_addr):x},0x{image_end:x}) "
                f"payload=[0x{source_start:x},0x{source_end:x})"
            )
        if destination != jump_target:
            raise ValueError(
                "Nexus-AM copy loader destination disagrees with jump target: "
                f"destination=0x{destination:x} jump_target=0x{jump_target:x}"
            )

        source_offset = source_start - int(base_addr)
        payload = bin_data[source_offset : source_offset + (source_end - source_start)]
        staged = {destination + offset: value for offset, value in enumerate(payload)}
        for addr, value in staged.items():
            loaded = self.mem.get(addr)
            if loaded is not None and loaded != value:
                raise ValueError(
                    f"Nexus-AM copy payload disagrees with loaded memory: address=0x{addr:x} "
                    f"memory=0x{loaded:02x} payload=0x{value:02x}"
                )
        self.mem.update(staged)
        return len(payload)

    def materialize_trace_instructions(self, entries: Iterable[object]) -> int:
        staged: Dict[int, int] = {}
        for position, entry in enumerate(entries):
            pc = int(getattr(entry, "pc"))
            instr = int(getattr(entry, "instr"))
            size = int(getattr(entry, "size"))
            index = int(getattr(entry, "index", position))
            self._check_addr(pc)
            if size not in (2, 4):
                raise ValueError(
                    f"unsupported trace instruction size: index={index} pc=0x{pc:x} size={size}"
                )
            if instr < 0 or instr >= (1 << (size * 8)):
                raise ValueError(
                    f"trace instruction exceeds size: index={index} pc=0x{pc:x} "
                    f"instr=0x{instr:x} size={size}"
                )
            for offset, value in enumerate(instr.to_bytes(size, byteorder="little")):
                addr = pc + offset
                prior_trace = staged.get(addr)
                if prior_trace is not None and prior_trace != value:
                    raise ValueError(
                        f"conflicting golden trace instruction bytes: address=0x{addr:x} "
                        f"first=0x{prior_trace:02x} next=0x{value:02x}"
                    )
                loaded = self.mem.get(addr)
                if loaded is not None and loaded != value:
                    raise ValueError(
                        f"golden trace instruction disagrees with loaded memory: address=0x{addr:x} "
                        f"memory=0x{loaded:02x} trace=0x{value:02x}"
                    )
                staged[addr] = value

        materialized = 0
        for addr, value in staged.items():
            if addr not in self.mem:
                self.mem[addr] = value
                materialized += 1
        return materialized

    def write_u8(self, addr: int, value: int) -> None:
        self._check_addr(addr)
        self.mem[addr] = value & 0xFF

    def write_u16(self, addr: int, value: int) -> None:
        if self.strict_align and (addr & 0x1):
            raise AddressError(f"unaligned u16 write: 0x{addr:x}")
        self.write_u8(addr + 0, value >> 0)
        self.write_u8(addr + 1, value >> 8)

    def write_u32(self, addr: int, value: int) -> None:
        if self.strict_align and (addr & 0x3):
            raise AddressError(f"unaligned u32 write: 0x{addr:x}")
        self.write_u8(addr + 0, value >> 0)
        self.write_u8(addr + 1, value >> 8)
        self.write_u8(addr + 2, value >> 16)
        self.write_u8(addr + 3, value >> 24)

    def read_u8(self, addr: int) -> int:
        self._check_addr(addr)
        return self.mem.get(addr, _nop_pattern_byte(addr))

    def read_u16(self, addr: int) -> int:
        if self.strict_align and (addr & 0x1):
            raise AddressError(f"unaligned u16 read: 0x{addr:x}")
        return self.read_u8(addr + 0) | (self.read_u8(addr + 1) << 8)

    def read_u32(self, addr: int) -> int:
        if self.strict_align and (addr & 0x3):
            raise AddressError(f"unaligned u32 read: 0x{addr:x}")
        return (
            self.read_u8(addr + 0)
            | (self.read_u8(addr + 1) << 8)
            | (self.read_u8(addr + 2) << 16)
            | (self.read_u8(addr + 3) << 24)
        )

    def read_block(self, addr: int, size: int, default_byte: Optional[int] = None) -> bytes:
        self._check_addr(addr)
        out = bytearray(size)
        for i in range(size):
            if default_byte is None:
                out[i] = self.mem.get(addr + i, _nop_pattern_byte(addr + i))
            else:
                out[i] = self.mem.get(addr + i, default_byte & 0xFF)
        return bytes(out)

    def read_cacheline(self, addr: int, line_bytes: int = 64) -> Tuple[int, int]:
        if line_bytes % 2 != 0:
            raise ValueError("line_bytes must be even")
        beat_bytes = line_bytes // 2
        base = addr & ~(line_bytes - 1)
        b0 = self.read_block(base, beat_bytes)
        b1 = self.read_block(base + beat_bytes, beat_bytes)
        return int.from_bytes(b0, "little"), int.from_bytes(b1, "little")


__all__ = [
    "DEFAULT_MMIO_RANGES",
    "MemoryModel",
    "PTE",
]
