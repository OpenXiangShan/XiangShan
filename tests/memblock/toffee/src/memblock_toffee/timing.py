from __future__ import annotations

import asyncio
from dataclasses import dataclass
from types import TracebackType


class CycleBudgetExceeded(RuntimeError):
    """Raised before a test drives work beyond its configured budget."""

    def __init__(self, current: int, maximum: int) -> None:
        super().__init__(f"cycle budget exceeded: current={current} max={maximum}")
        self.current = current
        self.maximum = maximum


@dataclass(frozen=True, slots=True)
class CycleToken:
    index: int


class CycleTracker:
    """Own verification cycle identity independently of DUT observations."""

    def __init__(self, max_cycles: int | None = None) -> None:
        if max_cycles is not None and max_cycles < 1:
            raise ValueError("max_cycles must be positive when provided")
        self._cycle = 0
        self.max_cycles = max_cycles
        self._lock = asyncio.Lock()
        self._pending: CycleToken | None = None

    @property
    def cycle(self) -> int:
        return self._cycle

    def next_token(self) -> CycleToken:
        return self._reserve()

    def commit(self, token: CycleToken) -> None:
        if self._pending != token:
            raise AssertionError(
                f"cycle token {token.index} is not the active reservation"
            )
        self._cycle = token.index
        self._pending = None

    def abort(self, token: CycleToken) -> None:
        if self._pending == token:
            self._pending = None

    def reservation(self) -> CycleReservation:
        return CycleReservation(self)

    @property
    def remaining(self) -> int | None:
        if self.max_cycles is None:
            return None
        return self.max_cycles - self._cycle

    @property
    def pending(self) -> CycleToken | None:
        return self._pending

    def _reserve(self) -> CycleToken:
        if self._pending is not None:
            raise RuntimeError(
                f"cycle {self._pending.index} is already reserved"
            )
        self._require_budget()
        token = CycleToken(self._cycle + 1)
        self._pending = token
        return token

    def _require_budget(self) -> None:
        if self.max_cycles is not None and self._cycle >= self.max_cycles:
            raise CycleBudgetExceeded(self._cycle, self.max_cycles)


class CycleReservation:
    """Serialize one driver phase from cycle reservation through commit."""

    def __init__(self, tracker: CycleTracker) -> None:
        self._tracker = tracker
        self._token: CycleToken | None = None

    async def __aenter__(self) -> CycleToken:
        await self._tracker._lock.acquire()
        try:
            self._token = self._tracker._reserve()
        except BaseException:
            self._tracker._lock.release()
            raise
        return self._token

    async def __aexit__(
        self,
        exception_type: type[BaseException] | None,
        exception: BaseException | None,
        traceback: TracebackType | None,
    ) -> None:
        del exception, traceback
        assert self._token is not None
        try:
            if exception_type is None:
                self._tracker.commit(self._token)
            else:
                self._tracker.abort(self._token)
        finally:
            self._tracker._lock.release()
