from __future__ import annotations

import asyncio
from collections.abc import Callable
from contextlib import suppress
from typing import Any


ClockStarter = Callable[[Any], Any]


def run_toffee_case(
    case: Callable[[Any], Any],
    env_factory: Callable[[], Any],
) -> Any:
    """Run one Toffee case while keeping environment construction in-loop."""

    import toffee

    return toffee.run(case, env_factory)


class ClockLease:
    """Own exactly the Toffee clock task created for one environment."""

    def __init__(self, task: asyncio.Task[Any], event: Any) -> None:
        self._task = task
        self._event = event
        self._closed = False

    @classmethod
    def start(
        cls,
        dut: Any,
        *,
        starter: ClockStarter | None = None,
    ) -> ClockLease:
        if starter is None:
            import toffee

            starter = toffee.start_clock
        before = set(asyncio.all_tasks())
        returned = starter(dut)
        created = [
            task
            for task in asyncio.all_tasks()
            if task not in before and task.get_name() == "__clock_loop"
        ]
        if isinstance(returned, asyncio.Task):
            task = returned
        elif len(created) == 1:
            task = created[0]
        else:
            for candidate in created:
                candidate.cancel()
            raise RuntimeError(
                "Toffee clock start did not create exactly one clock task"
            )
        return cls(task=task, event=getattr(dut, "event", None))

    @property
    def task(self) -> asyncio.Task[Any]:
        return self._task

    @property
    def closed(self) -> bool:
        return self._closed

    async def close(self) -> None:
        if self._closed:
            return
        self._closed = True
        if not self._task.done():
            self._task.cancel()
        try:
            with suppress(asyncio.CancelledError):
                await self._task
        finally:
            loop = asyncio.get_running_loop()
            if hasattr(loop, "test_done"):
                delattr(loop, "test_done")
            if self._event is not None and getattr(
                loop,
                "global_clock_event",
                None,
            ) is self._event:
                delattr(loop, "global_clock_event")

    async def __aenter__(self) -> ClockLease:
        return self

    async def __aexit__(self, *_: object) -> None:
        await self.close()
