from types import SimpleNamespace

import pytest

from env.support.signal_utils import read_first_signal, read_internal_signal, require_sig
from tests.py.support import dut_support, predchecker, uncache_scenarios


def test_signal_aliases_keep_order_zero_and_internal_fallback() -> None:
    calls = []

    def internal(name):
        calls.append(name)
        return SimpleNamespace(value=0) if name == "native" else None

    dut = SimpleNamespace(
        unreadable=SimpleNamespace(value=None),
        later=SimpleNamespace(value=9),
        GetInternalSignal=internal,
    )
    assert read_first_signal(dut, ("missing", "unreadable", "native", "later")) == 0
    assert calls == ["missing", "native"]


def test_signal_cache_keeps_handles_and_negative_lookups() -> None:
    pin = SimpleNamespace(value=3)
    env = SimpleNamespace(dut=SimpleNamespace(second=pin))
    aliases = ("first", "second")
    assert dut_support.read_dut_signal(env, aliases) == 3
    pin.value = 4
    env.dut.first = SimpleNamespace(value=8)
    assert dut_support.read_dut_signal(env, aliases) == 4
    assert predchecker._read_direct(env, *aliases) == 8
    pin.value = None
    assert dut_support.read_dut_signal(env, aliases, default=-1) == -1
    with pytest.raises(AssertionError) as error:
        dut_support.read_dut_signal(env, aliases, required=True)
    assert error.value.args[0] == {
        "reason": "required DUT signal is unavailable", "candidates": aliases,
    }
    assert dut_support.read_dut_signal(env, "absent") is None
    env.dut.absent = SimpleNamespace(value=5)
    assert dut_support.read_dut_signal(env, "absent") is None
    assert predchecker._read_direct(env, "absent") == 5


@pytest.mark.parametrize("container", (bytes, bytearray, list))
def test_uncache_reads_wide_signal_bytes_without_truncation(container) -> None:
    value = (1 << 63) | (1 << 35) | 3
    pin = SimpleNamespace(value=3, W=lambda: 64, GetBytes=lambda: container(value.to_bytes(8, "little")))
    env = SimpleNamespace(dut=SimpleNamespace(data=pin))
    assert uncache_scenarios._try_read_dut_signal(env, "data") == value
    assert uncache_scenarios._read_first_dut_signal(env, ("missing", "data")) == value
    assert dut_support.read_dut_signal(env, "data") == 3
    pin.W = lambda: 32
    assert uncache_scenarios._try_read_dut_signal(env, "data") == 3


def test_signal_failures_keep_optional_and_required_contracts() -> None:
    def unavailable(name):
        raise KeyError(name)

    env = SimpleNamespace(dut=SimpleNamespace(GetInternalSignal=unavailable))
    assert predchecker._read_direct(env, "missing") is None
    assert uncache_scenarios._try_read_dut_signal(env, "missing") is None
    with pytest.raises(AssertionError) as error:
        uncache_scenarios._require_first_dut_signal(env, ("missing",))
    assert error.value.args[0] == {"missing_internal_signals": ["missing"]}
    with pytest.raises(AssertionError) as error:
        require_sig(env.dut, "missing")
    assert error.value.args[0] == {"missing_dut_signal": "missing"}
    env.dut.pin = SimpleNamespace()
    with pytest.raises(AssertionError, match="unreadable_dut_signal"):
        require_sig(env.dut, "pin")


def test_internal_only_reader_does_not_use_external_pin() -> None:
    dut = SimpleNamespace(pin=SimpleNamespace(value=1), GetInternalSignal=lambda name: SimpleNamespace(value=2))
    assert read_internal_signal(dut, "pin") == 2
    assert read_first_signal(dut, ("pin",)) == 1


@pytest.mark.parametrize("final_check", (False, True))
@pytest.mark.parametrize("limit", (-1, 0, 1, 3))
@pytest.mark.parametrize("arrival", (-1, 0, 1, 2, 3))
def test_polling_preserves_check_phase_and_final_boundary(final_check, limit, arrival) -> None:
    env = SimpleNamespace(current_cycle=0)
    checks = []

    def step(cycles):
        assert cycles == 1
        env.current_cycle += 1

    def reached():
        checks.append(env.current_cycle)
        return arrival >= 0 and env.current_cycle >= arrival

    env.step = step
    wait = dut_support.run_until if final_check else dut_support.poll_until
    end = max(limit, 0)
    success = 0 <= arrival < end or final_check and arrival == end
    assert wait(env, reached, max_cycles=limit) is success
    assert env.current_cycle == (arrival if success else end)
    assert checks == list(range(env.current_cycle + int(success or final_check)))


@pytest.mark.parametrize("stat,wait", (
    ("req_count", dut_support._wait_for_req_delta),
    ("resp_count", dut_support._wait_for_resp_delta),
))
@pytest.mark.parametrize("limit", (0, 1, 3))
@pytest.mark.parametrize("delta", (0, 1, 3))
def test_delta_wait_steps_before_checking_and_keeps_final_read(stat, wait, limit, delta) -> None:
    env = SimpleNamespace(current_cycle=0)
    reads = []

    def step(cycles):
        env.current_cycle += cycles

    def stats():
        reads.append(env.current_cycle)
        return {stat: 3 + env.current_cycle // 2}

    env.step = step
    result = wait(env, SimpleNamespace(get_stats=stats), delta=delta, max_cycles=limit)
    reached = next((cycle for cycle in range(1, limit + 1) if cycle // 2 >= delta), None)
    end = reached if reached is not None else limit
    assert env.current_cycle == end
    assert result == 3 + end // 2
    assert reads == [0, *range(1, end + 1), *([] if reached is not None else [end])]


@pytest.mark.parametrize("limit", (0, 1, 3))
@pytest.mark.parametrize("arrival", (-1, 0, 1, 3))
def test_uncache_a_valid_wait_includes_final_check(limit, arrival) -> None:
    valid = SimpleNamespace(value=int(arrival == 0))
    address = 0x8040_0000
    env = SimpleNamespace(
        current_cycle=0,
        dut=SimpleNamespace(**{uncache_scenarios._IFU_UNCACHE_TO_UNCACHE_VALID_SIGNALS[0]: valid}),
        uncache_if=SimpleNamespace(a_bits_address=SimpleNamespace(value=address)),
    )

    def step(cycles):
        env.current_cycle += cycles
        valid.value = int(arrival >= 0 and env.current_cycle >= arrival)

    env.step = step
    assert uncache_scenarios._wait_for_uncache_a_valid_addr(env, address, max_cycles=limit) is (0 <= arrival <= limit)
    assert env.current_cycle == (arrival if 0 <= arrival <= limit else limit)
