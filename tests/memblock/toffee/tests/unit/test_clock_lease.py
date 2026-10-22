import asyncio
from types import SimpleNamespace

import pytest

from memblock_toffee.toffee_runtime import ClockLease


@pytest.mark.asyncio
async def test_clock_lease_cancels_only_the_task_it_created() -> None:
    unrelated_cancelled = asyncio.Event()

    async def unrelated_clock() -> None:
        try:
            await asyncio.Event().wait()
        finally:
            unrelated_cancelled.set()

    unrelated = asyncio.create_task(unrelated_clock())
    unrelated.set_name("__clock_loop")

    def starter(_: object) -> None:
        task = asyncio.create_task(asyncio.Event().wait())
        task.set_name("__clock_loop")

    lease = ClockLease.start(object(), starter=starter)
    assert lease.task is not unrelated
    await lease.close()
    assert lease.closed
    assert not unrelated.done()
    assert not unrelated_cancelled.is_set()
    unrelated.cancel()
    with pytest.raises(asyncio.CancelledError):
        await unrelated


@pytest.mark.asyncio
async def test_clock_lease_close_is_idempotent() -> None:
    def starter(_: object) -> asyncio.Task[None]:
        task = asyncio.create_task(asyncio.Event().wait())
        task.set_name("__clock_loop")
        return task

    lease = ClockLease.start(object(), starter=starter)
    await lease.close()
    await lease.close()


@pytest.mark.asyncio
async def test_clock_lease_clears_event_when_clock_task_fails() -> None:
    event = object()

    dut = SimpleNamespace(event=event)

    async def fail() -> None:
        raise RuntimeError("clock failed")

    def starter(_: object) -> asyncio.Task[None]:
        task = asyncio.create_task(fail())
        task.set_name("__clock_loop")
        return task

    loop = asyncio.get_running_loop()
    loop.global_clock_event = event  # type: ignore[attr-defined]
    lease = ClockLease.start(dut, starter=starter)
    await asyncio.sleep(0)
    with pytest.raises(RuntimeError, match="clock failed"):
        await lease.close()
    assert not hasattr(loop, "global_clock_event")
    assert not hasattr(loop, "test_done")
