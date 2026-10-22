import asyncio

import pytest

from memblock_toffee.timing import CycleBudgetExceeded, CycleTracker


def test_cycle_budget_fails_before_driving_past_limit() -> None:
    tracker = CycleTracker(max_cycles=2)
    for _ in range(2):
        token = tracker.next_token()
        tracker.commit(token)
    assert tracker.remaining == 0
    with pytest.raises(CycleBudgetExceeded, match="current=2 max=2"):
        tracker.next_token()


@pytest.mark.asyncio
async def test_overlapping_cycle_reservations_are_unique_and_serialized() -> None:
    tracker = CycleTracker(max_cycles=2)

    async def reserve() -> int:
        async with tracker.reservation() as token:
            await asyncio.sleep(0)
            return token.index

    assert sorted(await asyncio.gather(reserve(), reserve())) == [1, 2]
    assert tracker.cycle == 2
