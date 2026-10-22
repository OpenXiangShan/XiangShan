from __future__ import annotations

import asyncio
from collections.abc import Awaitable, Callable
from contextlib import suppress
from typing import Any, TypeVar


ResultT = TypeVar("ResultT")


def run_toffee_case(
    case: Callable[[Any], Awaitable[ResultT]],
    env_factory: Callable[[], Any],
) -> ResultT:
    """Run one Toffee case while keeping environment construction in-loop."""

    import toffee

    return toffee.run(case, env_factory)


def start_toffee_clock(adapter: Any) -> None:
    import toffee

    toffee.start_clock(adapter)


async def stop_toffee_clock() -> None:
    current = asyncio.current_task()
    tasks = [task for task in asyncio.all_tasks() if task is not current]
    clock_tasks = [task for task in tasks if task.get_name() == "__clock_loop"]
    for task in clock_tasks:
        task.cancel()
    for task in clock_tasks:
        with suppress(asyncio.CancelledError):
            await task
