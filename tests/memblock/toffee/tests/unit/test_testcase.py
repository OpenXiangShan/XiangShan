import asyncio

import pytest

from memblock_toffee.testcase import testcase as managed_testcase


@pytest.mark.asyncio
async def test_managed_testcase_does_not_globally_cancel_tasks() -> None:
    async def unrelated() -> None:
        await asyncio.Event().wait()

    task = asyncio.create_task(unrelated())

    @managed_testcase
    async def body() -> int:
        return 7

    try:
        assert await body() == 7
        assert not task.done()
        loop = asyncio.get_running_loop()
        assert loop.test_done is False  # type: ignore[attr-defined]
    finally:
        task.cancel()
        with pytest.raises(asyncio.CancelledError):
            await task
        loop = asyncio.get_running_loop()
        if hasattr(loop, "test_done"):
            delattr(loop, "test_done")
