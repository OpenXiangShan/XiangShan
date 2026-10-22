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


def test_protocol_components_preserve_uvm_roles() -> None:
    assert issubclass(ControlDriverAgent, Driver)
    assert issubclass(L2FlushMonitor, Monitor)
    assert issubclass(L2FlushSequencer, Sequencer)
