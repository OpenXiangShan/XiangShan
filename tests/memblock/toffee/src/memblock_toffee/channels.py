from __future__ import annotations

from dataclasses import dataclass
from typing import Generic, Protocol, TypeVar

from .transactions import CompletionDisposition, TransactionKey


RequestT = TypeVar("RequestT")
ResponseT = TypeVar("ResponseT")


@dataclass(frozen=True, slots=True)
class ResponsePlan(Generic[ResponseT]):
    delay_cycles: int
    response: ResponseT | None
    disposition: CompletionDisposition = CompletionDisposition.COMPLETED

    def __post_init__(self) -> None:
        if self.delay_cycles < 0:
            raise ValueError("response delay must be non-negative")
        if (
            self.disposition is CompletionDisposition.CANCELLED
            and self.response is not None
        ):
            raise ValueError("cancelled responses must not carry a response payload")


class ResponsePolicy(Protocol[RequestT, ResponseT]):
    def on_request(self, request: RequestT) -> ResponsePlan[ResponseT]: ...


class ManagedChannel(Protocol[RequestT, ResponseT]):
    async def accept(self) -> RequestT: ...

    async def respond(self, key: TransactionKey, response: ResponseT) -> None: ...

    async def idle(self) -> bool: ...


class ManagerAgent(Generic[RequestT, ResponseT]):
    """Couples a passive channel to an independent response policy."""

    def __init__(
        self,
        channel: ManagedChannel[RequestT, ResponseT],
        policy: ResponsePolicy[RequestT, ResponseT],
    ) -> None:
        self.channel = channel
        self.policy = policy

    def plan(self, request: RequestT) -> ResponsePlan[ResponseT]:
        return self.policy.on_request(request)
