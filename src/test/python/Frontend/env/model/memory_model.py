from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Tuple

from ..exceptions import AddressError

DEFAULT_MMIO_RANGES: Tuple[Tuple[int, int], ...] = (
    (0x00000000, 0x10000000),
)


def _nop_pattern_byte(addr: int) -> int:
    return 0x13 if (addr & 0x3) == 0 else 0x00


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
    level: int = 0
    asid: int = 0
    vmid: int = 0


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
