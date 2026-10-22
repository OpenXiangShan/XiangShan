from __future__ import annotations

import asyncio
import functools
from collections.abc import Callable
from typing import Any, TypeVar

import pytest


TestCaseT = TypeVar("TestCaseT", bound=Callable[..., Any])


def testcase(function: TestCaseT) -> TestCaseT:
    """Run a case on Toffee's clock without globally cancelling its tasks."""

    setattr(function, "is_toffee_testcase", True)

    @functools.wraps(function)
    @pytest.mark.asyncio
    async def wrapper(*args: Any, **kwargs: Any) -> Any:
        loop = asyncio.get_running_loop()
        loop.test_done = False  # type: ignore[attr-defined]
        loop.new_task_run = False  # type: ignore[attr-defined]
        if not hasattr(loop, "delayer_list"):
            loop.delayer_list = []  # type: ignore[attr-defined]
        current = asyncio.current_task()
        if current is not None:
            current.set_name("main_coro")
        return await function(*args, **kwargs)

    return wrapper  # type: ignore[return-value]
