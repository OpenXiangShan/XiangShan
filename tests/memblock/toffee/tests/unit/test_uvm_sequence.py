import asyncio

import pytest

from memblock_toffee.agent import ControlDriverAgent
from memblock_toffee.monitor import L2FlushMonitor
from memblock_toffee.sequence import L2FlushSequencer
from memblock_toffee.uvm import Sequence, Sequencer
from memblock_toffee.uvm import Driver, Monitor


class RequestSequence(Sequence[int]):
    async def body(self, sequencer: Sequencer[int]) -> None:
        for value in (1, 2, 3):
            await sequencer.send(value)


@pytest.mark.asyncio
async def test_sequence_contains_intent_not_hardware_timing() -> None:
    sequencer: Sequencer[int] = Sequencer()
    await sequencer.run(RequestSequence())
    assert await sequencer.drain() == [1, 2, 3]


@pytest.mark.asyncio
async def test_sequencer_tracks_outstanding_and_out_of_order_completion() -> None:
    sequencer: Sequencer[str] = Sequencer()
    first = await sequencer.issue("first", key="a")
    second = await sequencer.issue("second", key="b")
    sequencer.complete("b", 2)
    sequencer.complete("a", 1)
    assert await sequencer.overlap(first, second) == [1, 2]
    assert sequencer.outstanding == 0


@pytest.mark.asyncio
async def test_sequencer_removes_cancelled_requests_from_outstanding() -> None:
    sequencer: Sequencer[str] = Sequencer()
    handle = await sequencer.issue("redirected", key="request-0")
    sequencer.cancel("request-0")
    assert sequencer.outstanding == 0
    with pytest.raises(asyncio.CancelledError):
        await sequencer.wait(handle)


@pytest.mark.asyncio
async def test_sequencer_completion_audit_checks_queue_and_outstanding() -> None:
    sequencer: Sequencer[str] = Sequencer("audit")
    await sequencer.send("queued")
    with pytest.raises(AssertionError, match="queued=1"):
        sequencer.assert_complete()
    await sequencer.drain()
    await sequencer.issue("outstanding", key="request-0")
    with pytest.raises(AssertionError, match="outstanding=1"):
        sequencer.assert_complete()


def test_protocol_components_preserve_uvm_roles() -> None:
    assert issubclass(ControlDriverAgent, Driver)
    assert issubclass(L2FlushMonitor, Monitor)
    assert issubclass(L2FlushSequencer, Sequencer)
