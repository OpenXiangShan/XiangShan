from __future__ import annotations

from dataclasses import dataclass
from typing import Optional, Sequence

from ..support.pmp_pma import PmpPmaConfig, encode_pmp_pma_addr


@dataclass(frozen=True)
class PmpPmaCheckResult:
    pmp_checked: bool
    pma_checked: bool
    pmp_match_index: Optional[int]
    pma_match_index: Optional[int]
    pmp_execute_denied: bool
    pma_execute_denied: bool
    pma_cacheable: Optional[bool]

    @property
    def execute_allowed(self) -> bool:
        return not self.pmp_execute_denied and not self.pma_execute_denied

    @property
    def pma_mmio(self) -> Optional[bool]:
        return None if self.pma_cacheable is None else not self.pma_cacheable

    @property
    def reason(self) -> str:
        if self.pmp_execute_denied and self.pma_execute_denied:
            return "pmp_pma_execute_denied"
        if self.pmp_execute_denied:
            return "pmp_execute_denied"
        if self.pma_execute_denied:
            return "pma_execute_denied"
        return "ok"

    def as_dict(self) -> dict:
        return {
            "pmp_checked": self.pmp_checked,
            "pma_checked": self.pma_checked,
            "pmp_match_index": self.pmp_match_index,
            "pma_match_index": self.pma_match_index,
            "pmp_execute_denied": self.pmp_execute_denied,
            "pma_execute_denied": self.pma_execute_denied,
            "pma_cacheable": self.pma_cacheable,
            "pma_mmio": self.pma_mmio,
            "execute_allowed": self.execute_allowed,
            "reason": self.reason,
        }


class PmpPmaPermissionModel:
    """Reference instruction-fetch PMP/PMA check for explicit scenario entries."""

    _ENTRY_COUNT = 32

    @staticmethod
    def _match_mode(config: PmpPmaConfig) -> int:
        if isinstance(config.match, str):
            values = {"off": 0, "tor": 1, "na4": 2, "napot": 3}
            try:
                return values[config.match.lower()]
            except KeyError as exc:
                raise ValueError(f"unsupported PMP/PMA match mode: {config.match}") from exc
        value = int(config.match)
        if value not in range(4):
            raise ValueError(f"unsupported PMP/PMA match mode: {value}")
        return value

    @classmethod
    def _entries_by_index(cls, entries: Sequence[object], kind: str) -> dict[int, object]:
        indexed = {}
        for entry in entries:
            if str(getattr(entry, "kind", "")).lower() != kind:
                raise ValueError(f"{kind} model received a non-{kind} entry")
            index = int(getattr(entry, "index"))
            if not 0 <= index < cls._ENTRY_COUNT:
                raise ValueError(f"PMP/PMA entry index out of range: {index}")
            if index in indexed:
                raise ValueError(f"duplicate {kind} entry index: {index}")
            config = getattr(entry, "config")
            addr = int(getattr(entry, "addr"))
            size = getattr(entry, "size")
            cls._match_mode(config)
            encode_pmp_pma_addr(addr, config, size=size)
            indexed[index] = entry
        return indexed

    @classmethod
    def _contains(cls, entry: object, previous_addr: int, pa: int, size: int) -> bool:
        config = getattr(entry, "config")
        match_mode = cls._match_mode(config)
        if match_mode == 0:
            return False
        lower = int(getattr(entry, "addr")) if match_mode != 1 else int(previous_addr)
        if match_mode == 1:
            upper = int(getattr(entry, "addr"))
        elif match_mode == 2:
            upper = lower + 4
        else:
            upper = lower + int(getattr(entry, "size"))
        return lower <= int(pa) and int(pa) + int(size) <= upper

    @classmethod
    def _first_match(cls, entries: dict[int, object], pa: int, size: int) -> Optional[object]:
        for index in sorted(entries):
            entry = entries[index]
            previous = entries.get(index - 1)
            previous_addr = 0 if previous is None else int(getattr(previous, "addr"))
            if cls._contains(entry, previous_addr, pa, size):
                return entry
        return None

    @classmethod
    def check_instruction(
        cls,
        pa: int,
        *,
        pmp_entries: Sequence[object] = (),
        pma_entries: Sequence[object] = (),
        priv_imode: int = 1,
        size: int = 1,
        pmp_enabled: bool = True,
        pma_enabled: bool = True,
    ) -> PmpPmaCheckResult:
        if int(pa) < 0 or int(size) < 1:
            raise ValueError("PMP/PMA check requires a non-negative address and positive size")

        pmp_indexed = cls._entries_by_index(pmp_entries, "pmp")
        pma_indexed = cls._entries_by_index(pma_entries, "pma")
        pmp_match = cls._first_match(pmp_indexed, int(pa), int(size)) if pmp_enabled else None
        pma_match = cls._first_match(pma_indexed, int(pa), int(size)) if pma_enabled else None

        if not pmp_enabled:
            pmp_execute_denied = False
        elif pmp_match is None:
            pmp_execute_denied = int(priv_imode) < 2
        else:
            pmp_config = getattr(pmp_match, "config")
            pmp_execute_denied = not (int(priv_imode) >= 2 and not bool(pmp_config.locked)) and not bool(pmp_config.execute)

        if not pma_enabled:
            pma_execute_denied = False
            pma_cacheable = None
        elif pma_match is None:
            pma_execute_denied = True
            pma_cacheable = False
        else:
            pma_config = getattr(pma_match, "config")
            pma_execute_denied = not bool(pma_config.execute)
            pma_cacheable = bool(pma_config.cacheable)

        return PmpPmaCheckResult(
            pmp_checked=bool(pmp_enabled),
            pma_checked=bool(pma_enabled),
            pmp_match_index=None if pmp_match is None else int(getattr(pmp_match, "index")),
            pma_match_index=None if pma_match is None else int(getattr(pma_match, "index")),
            pmp_execute_denied=bool(pmp_execute_denied),
            pma_execute_denied=bool(pma_execute_denied),
            pma_cacheable=pma_cacheable,
        )


__all__ = ["PmpPmaCheckResult", "PmpPmaPermissionModel"]
