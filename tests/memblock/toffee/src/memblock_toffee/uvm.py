from __future__ import annotations

import asyncio
from abc import ABC, abstractmethod
from collections.abc import Hashable, Iterable
from dataclasses import dataclass
from typing import Generic, TypeVar


RequestT = TypeVar("RequestT")
ResponseT = TypeVar("ResponseT")
ExpectedModelT = TypeVar("ExpectedModelT")
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

    def __init__(self, name: str = "sequencer") -> None:
        if not name or name.strip() != name:
            raise ValueError("sequencer name must be non-empty and trimmed")
        self.name = name
        self._queue: asyncio.Queue[RequestT] = asyncio.Queue()
        self._outstanding: dict[Hashable, RequestHandle[RequestT, object]] = {}

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

    async def issue(
        self,
        request: RequestT,
        key: Hashable | None = None,
    ) -> RequestHandle[RequestT, object]:
        request_key = key if key is not None else id(request)
        if request_key in self._outstanding:
            raise ValueError(f"duplicate outstanding request key: {request_key}")
        handle = RequestHandle(
            key=request_key,
            request=request,
            future=asyncio.get_running_loop().create_future(),
        )
        self._outstanding[request_key] = handle
        return handle

    def complete(self, key: Hashable, response: object) -> None:
        try:
            handle = self._outstanding.pop(key)
        except KeyError as error:
            raise KeyError(f"unknown outstanding request key: {key}") from error
        if not handle.future.done():
            handle.future.set_result(response)

    def cancel(self, key: Hashable) -> None:
        try:
            handle = self._outstanding.pop(key)
        except KeyError as error:
            raise KeyError(f"unknown outstanding request key: {key}") from error
        if not handle.future.done():
            handle.future.cancel()

    async def wait(self, handle: RequestHandle[RequestT, ResponseT]) -> ResponseT:
        return await handle.future

    async def overlap(
        self,
        *handles: RequestHandle[RequestT, ResponseT],
    ) -> list[ResponseT]:
        return list(await asyncio.gather(*(handle.future for handle in handles)))

    @property
    def outstanding(self) -> int:
        return len(self._outstanding)

    def assert_complete(self) -> None:
        queued = self._queue.qsize()
        outstanding = tuple(self._outstanding)
        if not queued and not outstanding:
            return
        keys = ", ".join(repr(key) for key in outstanding)
        raise AssertionError(
            f"sequencer {self.name!r} has unfinished requests: "
            f"queued={queued} outstanding={len(outstanding)} keys=[{keys}]"
        )


@dataclass(slots=True)
class RequestHandle(Generic[RequestT, ResponseT]):
    key: Hashable
    request: RequestT
    future: asyncio.Future[ResponseT]


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


class ReferenceModel(ABC, Generic[RequestT, ExpectedModelT]):
    """Pure request-to-expectation model with an explicit commit seam."""

    @abstractmethod
    def expect(self, request: RequestT) -> ExpectedModelT:
        raise NotImplementedError

    def commit(self, event: object) -> None:
        del event
