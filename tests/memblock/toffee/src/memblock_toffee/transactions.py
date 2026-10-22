from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Any, Generic, Hashable, TypeVar

from .errors import FailureContext, IssueCollector


PayloadT = TypeVar("PayloadT")
ExpectedT = TypeVar("ExpectedT")
ActualT = TypeVar("ActualT")
KeyT = TypeVar("KeyT")


@dataclass(frozen=True, slots=True)
class TransactionKey:
    namespace: str
    value: Hashable

    def __post_init__(self) -> None:
        if not self.namespace:
            raise ValueError("transaction key namespace must not be empty")

    def __str__(self) -> str:
        return f"{self.namespace}:{self.value}"


@dataclass(frozen=True, slots=True)
class TransferEnvelope(Generic[KeyT, PayloadT]):
    key: KeyT
    payload: PayloadT
    cycle: int
    beat: int = 0
    last: bool = True

    def __post_init__(self) -> None:
        if self.cycle < 0:
            raise ValueError("transfer cycle must be non-negative")
        if self.beat < 0:
            raise ValueError("transfer beat must be non-negative")


class CompletionDisposition(Enum):
    COMPLETED = "completed"
    CANCELLED = "cancelled"


@dataclass(frozen=True, slots=True)
class Completion(Generic[KeyT, PayloadT]):
    key: KeyT
    payload: PayloadT | None
    cycle: int
    disposition: CompletionDisposition = CompletionDisposition.COMPLETED

    def __post_init__(self) -> None:
        if self.cycle < 0:
            raise ValueError("completion cycle must be non-negative")
        if (
            self.disposition is CompletionDisposition.CANCELLED
            and self.payload is not None
        ):
            raise ValueError("cancelled completion must not carry a payload")


@dataclass(slots=True)
class _OutstandingEntry(Generic[ExpectedT, ActualT]):
    expected: ExpectedT
    actual: ActualT | None = None
    disposed: CompletionDisposition | None = None


class OutstandingTable(Generic[KeyT, ExpectedT, ActualT]):
    """UVM-style exactly-once table with explicit legal cancellation."""

    def __init__(self, component: str) -> None:
        self.component = component
        self._entries: dict[KeyT, _OutstandingEntry[ExpectedT, ActualT]] = {}
        self.issues = IssueCollector()

    def expect(self, key: KeyT, expected: ExpectedT) -> None:
        if key in self._entries:
            raise ValueError(f"duplicate expectation for {key}")
        self._entries[key] = _OutstandingEntry(expected)

    def observe(
        self,
        key: KeyT,
        actual: ActualT | None,
        cycle: int,
        disposition: CompletionDisposition = CompletionDisposition.COMPLETED,
    ) -> bool:
        entry = self._entries.get(key)
        if entry is None:
            self.issues.add(
                FailureContext(self.component, "observe", cycle, str(key)),
                "unexpected observation",
            )
            return False
        if entry.disposed is not None:
            self.issues.add(
                FailureContext(self.component, "observe", cycle, str(key)),
                "duplicate observation",
            )
            return False
        if disposition is CompletionDisposition.CANCELLED and actual is not None:
            self.issues.add(
                FailureContext(self.component, "observe", cycle, str(key)),
                "cancelled transaction carried a payload",
            )
        entry.actual = actual
        entry.disposed = disposition
        return True

    def cancel(
        self,
        key: KeyT,
        cycle: int,
    ) -> bool:
        entry = self._entries.get(key)
        if entry is None:
            self.issues.add(
                FailureContext(self.component, "cancel", cycle, str(key)),
                "cancellation for an unknown transaction",
            )
            return False
        if entry.disposed is not None:
            self.issues.add(
                FailureContext(self.component, "cancel", cycle, str(key)),
                "transaction already disposed",
            )
            return False
        entry.disposed = CompletionDisposition.CANCELLED
        return True

    def finish(self) -> None:
        for key, entry in self._entries.items():
            if entry.disposed is None:
                self.issues.add(
                    FailureContext(self.component, "finish", transaction_id=str(key)),
                    "missing completion",
                )
        self.issues.raise_if_any()

    def expected(self, key: KeyT) -> ExpectedT:
        try:
            return self._entries[key].expected
        except KeyError as error:
            raise KeyError(f"unknown transaction {key}") from error

    @property
    def outstanding(self) -> int:
        return sum(entry.disposed is None for entry in self._entries.values())
