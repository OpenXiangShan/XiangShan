from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class CycleToken:
    index: int

    @property
    def transaction_id(self) -> str:
        return f"flush-cycle-{self.index}"


class CycleTracker:
    """Own verification cycle identity independently of DUT observations."""

    def __init__(self) -> None:
        self._cycle = 0

    @property
    def cycle(self) -> int:
        return self._cycle

    def next_token(self) -> CycleToken:
        return CycleToken(self._cycle + 1)

    def commit(self, token: CycleToken) -> None:
        if token.index != self._cycle + 1:
            raise AssertionError(
                f"cycle token {token.index} does not follow {self._cycle}"
            )
        self._cycle = token.index
