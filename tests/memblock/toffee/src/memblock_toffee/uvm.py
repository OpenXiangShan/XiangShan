from __future__ import annotations

import asyncio
from abc import ABC, abstractmethod
from collections.abc import Iterable
from typing import Generic, TypeVar


RequestT = TypeVar("RequestT")
ResponseT = TypeVar("ResponseT")
TransactionT = TypeVar("TransactionT")
ExpectedT = TypeVar("ExpectedT")
ActualT = TypeVar("ActualT")


class Sequence(ABC, Generic[RequestT]):
    """UVM-style sequence that contains intent, not pin-level timing."""

    @abstractmethod
    async def body(self, sequencer: Sequencer[RequestT]) -> None:
        raise NotImplementedError


class Sequencer(Generic[RequestT]):
    """Minimal software sequencer with deterministic request identity."""

    def __init__(self) -> None:
        self._queue: asyncio.Queue[RequestT] = asyncio.Queue()

    async def send(self, request: RequestT) -> None:
        await self._queue.put(request)

    async def recv(self) -> RequestT:
        return await self._queue.get()

    async def run(self, sequence: Sequence[RequestT]) -> None:
        await sequence.body(self)

    async def drain(self) -> list[RequestT]:
        items: list[RequestT] = []
        while not self._queue.empty():
            items.append(self._queue.get_nowait())
        return items


class Driver(ABC, Generic[RequestT, ResponseT]):
    """UVM role marker implemented by a protocol-specific driver agent."""


class Monitor(ABC, Generic[TransactionT]):
    """UVM role marker implemented by a passive protocol monitor."""


class Scoreboard(ABC, Generic[ExpectedT, ActualT]):
    @abstractmethod
    def expect(self, expected: ExpectedT) -> None:
        raise NotImplementedError

    @abstractmethod
    def observe(self, actual: ActualT) -> None:
        raise NotImplementedError

    @abstractmethod
    def finish(self) -> None:
        raise NotImplementedError

    def expect_all(self, expected: Iterable[ExpectedT]) -> None:
        for item in expected:
            self.expect(item)
