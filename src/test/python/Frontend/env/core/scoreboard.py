from __future__ import annotations

from dataclasses import dataclass
from typing import Tuple


@dataclass(frozen=True)
class ScoreboardResult:
    """Minimal scoreboard result shared by UVM-like checker pilots.

    It mirrors the role of a UVM scoreboard verdict without introducing a global
    component hierarchy or phasing model.
    """

    passed: bool
    checked: int
    errors: Tuple[str, ...] = ()

    def assert_passed(self) -> None:
        if not self.passed:
            detail = "; ".join(self.errors) if self.errors else "unknown scoreboard failure"
            raise AssertionError(detail)
