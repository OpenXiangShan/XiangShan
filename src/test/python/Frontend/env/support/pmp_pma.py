from __future__ import annotations

from dataclasses import dataclass
from typing import Union


PMP_CFG_BASE = 0x3A0
PMP_ADDR_BASE = 0x3B0
PMA_CFG_BASE = 0x7C0
PMA_ADDR_BASE = 0x7C8
PMP_PMA_ENTRY_COUNT = 32
_CFG_ENTRIES_PER_CSR = 8


@dataclass(frozen=True)
class PmpPmaConfig:
    """One PMP/PMA cfg byte, using the RTL field layout."""

    match: Union[str, int] = "off"
    read: bool = False
    write: bool = False
    execute: bool = False
    locked: bool = False
    cacheable: bool = False
    atomic: bool = False


def _match_value(match: Union[str, int]) -> int:
    if isinstance(match, str):
        values = {"off": 0, "tor": 1, "na4": 2, "napot": 3}
        try:
            return values[match.lower()]
        except KeyError as exc:
            raise ValueError(f"unsupported PMP/PMA match mode: {match}") from exc
    value = int(match)
    if value not in range(4):
        raise ValueError(f"unsupported PMP/PMA match mode: {value}")
    return value


def encode_pmp_pma_cfg(config: PmpPmaConfig) -> int:
    """Encode the l/c/atomic/a/x/w/r cfg byte accepted by Frontend CSR logic."""

    if bool(config.write) and not bool(config.read):
        raise ValueError("PMP/PMA cfg cannot set write when read is clear")
    match = _match_value(config.match)
    return (
        (int(bool(config.locked)) << 7)
        | (int(bool(config.cacheable)) << 6)
        | (int(bool(config.atomic)) << 5)
        | (match << 3)
        | (int(bool(config.execute)) << 2)
        | (int(bool(config.write)) << 1)
        | int(bool(config.read))
    )


def csr_addresses_for_entry(kind: str, index: int) -> tuple[int, int, int]:
    """Return cfg CSR, addr CSR and cfg-byte offset for one real entry."""

    normalized_kind = str(kind).lower()
    if normalized_kind not in {"pmp", "pma"}:
        raise ValueError(f"unsupported PMP/PMA kind: {kind}")
    entry = int(index)
    if not 0 <= entry < PMP_PMA_ENTRY_COUNT:
        raise ValueError(f"PMP/PMA entry index out of range: {entry}")
    cfg_base, addr_base = (
        (PMP_CFG_BASE, PMP_ADDR_BASE)
        if normalized_kind == "pmp"
        else (PMA_CFG_BASE, PMA_ADDR_BASE)
    )
    return cfg_base + 2 * (entry // _CFG_ENTRIES_PER_CSR), addr_base + entry, entry % _CFG_ENTRIES_PER_CSR


def encode_pmp_pma_addr(addr: int, config: PmpPmaConfig, *, size: int | None = None) -> int:
    """Encode a physical address for a PMP/PMA address CSR.

    ``addr`` is the physical base for OFF/NA4/NAPOT and the exclusive upper
    bound for TOR. NAPOT additionally requires its power-of-two region size.
    """

    physical_addr = int(addr)
    if physical_addr < 0 or physical_addr & 0x3:
        raise ValueError("PMP/PMA address must be non-negative and 4-byte aligned")
    match = _match_value(config.match)
    if match != 3:
        if size is not None:
            raise ValueError("size is only valid for NAPOT PMP/PMA entries")
        return physical_addr >> 2
    if size is None:
        raise ValueError("NAPOT PMP/PMA entries require size")
    region_size = int(size)
    if region_size < 8 or region_size & (region_size - 1):
        raise ValueError("NAPOT size must be a power of two of at least 8 bytes")
    if physical_addr & (region_size - 1):
        raise ValueError("NAPOT base address must align to its size")
    return (physical_addr + region_size // 2 - 1) >> 2


__all__ = [
    "PMA_ADDR_BASE",
    "PMA_CFG_BASE",
    "PMP_ADDR_BASE",
    "PMP_CFG_BASE",
    "PMP_PMA_ENTRY_COUNT",
    "PmpPmaConfig",
    "csr_addresses_for_entry",
    "encode_pmp_pma_addr",
    "encode_pmp_pma_cfg",
]
