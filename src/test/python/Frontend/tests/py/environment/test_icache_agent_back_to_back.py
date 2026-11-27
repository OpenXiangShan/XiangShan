from __future__ import annotations

from dataclasses import replace
from types import SimpleNamespace

import pytest

from env.agents.icache_agent import ICacheAgent
from env.core.frontend_env import FrontendEnv
from env.model import MemoryModel
from env.monitors.frontend_monitor import ICacheRequestSnapshot
from env.runtime.dut_factory import FakeDUTFrontend
from env.support.env_config import EnvConfig


_BASE = 0x80000000


class _Signal:
    def __init__(self) -> None:
        self.value = 0


def _agent(rate: float = 1.0, seed: int = 1) -> ICacheAgent:
    memory = MemoryModel()
    memory.load_bin(bytes(range(256)), _BASE)
    agent = ICacheAgent(memory)
    agent.interface = SimpleNamespace(**{
        name: _Signal() for name in (
            "a_ready", "d_valid", "d_bits_opcode", "d_bits_source",
            "d_bits_data", "d_bits_denied", "d_bits_corrupt",
        )
    })
    agent.configure(hit_latency=8, seed=seed, back_to_back_rate=rate)
    agent.interface.a_ready.value = 1
    return agent


def _tick(agent: ICacheAgent, cycle: int, source=None) -> None:
    agent.on_clock_edge(cycle, ICacheRequestSnapshot(
        cycle=cycle,
        valid=int(source is not None),
        ready=int(agent.interface.a_ready.value),
        source=0 if source is None else source,
        address=_BASE + (0 if source is None else source * 64),
    ))


def _run(agent: ICacheAgent, requests: dict[int, int], stop: int = 32) -> list[tuple]:
    outputs = []
    for cycle in range(stop):
        _tick(agent, cycle, requests.get(cycle))
        interface = agent.interface
        if interface.d_valid.value:
            outputs.append((
                cycle, interface.d_bits_source.value, interface.d_bits_data.value,
                interface.d_bits_denied.value, interface.d_bits_corrupt.value,
            ))
    return outputs


@pytest.mark.parametrize("rate,cycles", [
    (0.0, [8, 9, 16, 17, 18, 19]),
    (1.0, [8, 9, 10, 11, 12, 13]),
])
def test_each_line_selects_normal_or_back_to_back_successor(rate, cycles) -> None:
    agent = _agent(rate)
    outputs = _run(agent, {0: 0, 8: 1, 9: 2})

    assert [item[0] for item in outputs] == cycles
    assert [item[1] for item in outputs] == [0, 0, 1, 1, 2, 2]
    assert [item[2] for item in outputs] == [
        int.from_bytes(bytes(range(offset, offset + 32)), "little")
        for offset in range(0, 192, 32)
    ]
    assert [record["beat_idx"] for record in agent.response_records] == [0, 1] * 3
    assert agent.get_stats()["resp_line_count"] == 3
    assert not agent.pending


@pytest.mark.parametrize("arrival", [8, 9, 10])
def test_successor_can_arrive_during_response_or_on_boundary(arrival) -> None:
    outputs = _run(_agent(), {0: 0, arrival: 1})

    assert [item[0] for item in outputs] == [8, 9, 10, 11]


def test_absent_successor_expires_without_accelerating_late_request() -> None:
    outputs = _run(_agent(), {0: 0, 11: 1})

    assert [item[0] for item in outputs] == [8, 9, 19, 20]


def test_single_request_finishes_without_waiting_for_successor() -> None:
    agent = _agent()
    outputs = _run(agent, {0: 0})

    assert [item[0] for item in outputs] == [8, 9]
    assert agent._back_to_back_cycle is None


def test_choice_is_made_once_per_line_and_can_return_to_normal() -> None:
    agent = _agent(0.5)
    choices = iter((0.0, 1.0, 1.0))
    agent.back_to_back_rng = SimpleNamespace(random=lambda: next(choices))
    outputs = _run(agent, {0: 0, 8: 1, 9: 2})

    assert [item[0] for item in outputs] == [8, 9, 10, 11, 17, 18]
    assert next(choices, None) is None


def test_accelerated_response_preserves_selected_fault_beat() -> None:
    agent = _agent()
    agent.inject_response_fault_at(_BASE + 64, denied=1, beat=1)
    outputs = _run(agent, {0: 0, 8: 1})

    assert [item[0] for item in outputs] == [8, 9, 10, 11]
    assert [item[3:] for item in outputs] == [(0, 0), (0, 0), (0, 0), (1, 1)]
    assert agent.denied_resp_count == agent.corrupt_resp_count == 1


def test_reset_discards_selected_successor() -> None:
    agent = _agent()
    _run(agent, {0: 0}, stop=10)
    agent.reset()
    _tick(agent, 10, 1)

    assert agent.interface.d_valid.value == 0
    assert agent.req_count == 1
    _tick(agent, 11, 1)
    for cycle in range(12, 21):
        _tick(agent, cycle)
    assert [record["cycle"] for record in agent.response_records] == [8, 9, 19, 20]


def test_seed_replays_mixed_choices_without_changing_miss_draws() -> None:
    requests = {cycle: index for index, cycle in enumerate(range(0, 160, 8))}
    agents = []
    for rate, seed in ((0.5, 7), (0.5, 7), (0.5, 8), (0.0, 7)):
        agent = _agent(rate, seed)
        agent.configure(
            hit_latency=8, miss_latency=24, miss_rate=0.3,
            seed=seed, back_to_back_rate=rate,
        )
        agents.append(agent)
    outputs = [_run(agent, requests, stop=200) for agent in agents]

    assert outputs[0] == outputs[1]
    assert [item[0] for item in outputs[0]] != [item[0] for item in outputs[2]]
    assert agents[0].request_records == agents[3].request_records
    starts = [item[0] for item in outputs[0][::2]]
    gaps = [following - current for current, following in zip(starts, starts[1:])]
    assert 2 in gaps
    assert any(gap > 2 for gap in gaps)


def test_env_config_applies_back_to_back_probability() -> None:
    config = EnvConfig()
    env = FrontendEnv(
        FakeDUTFrontend(), register_callbacks=False,
        config=replace(config, icache=replace(config.icache, back_to_back_rate=0.75)),
    )

    assert env.icache_agent.get_stats()["back_to_back_rate"] == 0.75
