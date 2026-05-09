from __future__ import annotations

from typing import Any, Iterable

from .model.page_table_model import PageTableModel


_MEMORY: dict[int, int] = {}
_STAGE1_ENTRIES: list[dict[str, int]] = []
_STAGE2_ENTRIES: list[dict[str, int]] = []


def _snapshot_value(snapshot: Any, key: str, default: int = 0) -> int:
    if isinstance(snapshot, dict):
        return int(snapshot.get(key, default))
    return int(getattr(snapshot, key, default))


def sync_memory(paddr: int, payload: bytes | bytearray | Iterable[int]) -> None:
    base = int(paddr)
    for offset, value in enumerate(bytes(payload)):
        _MEMORY[base + int(offset)] = int(value) & 0xFF


def sync_sv39_page_table(entries: Iterable[dict], *, root_paddr: int = 0) -> int:
    global _STAGE1_ENTRIES
    _STAGE1_ENTRIES = [dict(entry) for entry in entries]
    return int(root_paddr) >> 12


def sync_sv39x4_page_table(entries: Iterable[dict], *, root_paddr: int = 0) -> int:
    global _STAGE2_ENTRIES
    _STAGE2_ENTRIES = [dict(entry) for entry in entries]
    return int(root_paddr) >> 12


def _load_entries(model: PageTableModel) -> None:
    for entry in _STAGE1_ENTRIES:
        model.map_page(
            int(entry["vpn"]),
            int(entry["ppn"]),
            v=int(entry.get("v", 1)),
            r=int(entry.get("r", 1)),
            w=int(entry.get("w", 0)),
            x=int(entry.get("x", 1)),
            u=int(entry.get("u", 0)),
            g=int(entry.get("g", 0)),
            a=int(entry.get("a", 1)),
            d=int(entry.get("d", 0)),
            level=int(entry.get("level", 0)),
            asid=int(entry.get("asid", 0)),
            vmid=int(entry.get("vmid", 0)),
        )
    for entry in _STAGE2_ENTRIES:
        model.map_stage2_page(
            int(entry["vpn"]),
            int(entry["ppn"]),
            v=int(entry.get("v", 1)),
            r=int(entry.get("r", 1)),
            w=int(entry.get("w", 0)),
            x=int(entry.get("x", 1)),
            u=int(entry.get("u", 0)),
            g=int(entry.get("g", 0)),
            a=int(entry.get("a", 1)),
            d=int(entry.get("d", 0)),
            level=int(entry.get("level", 0)),
            vmid=int(entry.get("vmid", 0)),
        )


def build_ptw_resp(snapshot: Any) -> dict:
    satp_mode = _snapshot_value(snapshot, "satp_mode", 0)
    vsatp_mode = _snapshot_value(snapshot, "vsatp_mode", 0)
    mode = "sv39" if satp_mode != 0 or vsatp_mode != 0 else "bare"
    model = PageTableModel(mode=mode)
    if mode == "sv39":
        _load_entries(model)
    return model.build_ptw_resp(
        _snapshot_value(snapshot, "vpn", 0),
        s2xlate=_snapshot_value(snapshot, "s2xlate", 0),
        get_gpa=_snapshot_value(snapshot, "get_gpa", 0),
        memidx_is_ld=_snapshot_value(snapshot, "memidx_is_ld", 0),
        memidx_is_st=_snapshot_value(snapshot, "memidx_is_st", 0),
        memidx_idx=_snapshot_value(snapshot, "memidx_idx", 0),
    )


__all__ = [
    "build_ptw_resp",
    "sync_memory",
    "sync_sv39_page_table",
    "sync_sv39x4_page_table",
]
