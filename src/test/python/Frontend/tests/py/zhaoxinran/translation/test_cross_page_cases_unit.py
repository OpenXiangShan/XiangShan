from types import SimpleNamespace

import pytest

from env.model.golden_trace import _decode_rv32
from tests.py.support.cross_page_cases import _case_addresses, _cross_page_program, _wrong_path_scenario


def test_cross_page_addresses_are_reproducible_and_seeded(monkeypatch) -> None:
    addresses = []
    for base_seed in (1, 2, 3):
        monkeypatch.setenv("TB_SEED", str(base_seed))
        env = SimpleNamespace()
        placement = _case_addresses(env, "unit-cross-page", 0x8020_0000)
        va, pa = placement.va, placement.pa
        assert (va & 0xFFF) == (pa & 0xFFF)
        assert 0xF80 <= (va & 0xFFF) <= 0xFC0
        assert env.random_scenario_records[0]["parameters"]["va"] == va
        assert _case_addresses(SimpleNamespace(), "unit-cross-page", 0x8020_0000) == placement
        addresses.append((va, pa))
    assert len(set(addresses)) == len(addresses)


@pytest.mark.parametrize("base_seed", (1, 2, 3))
def test_wrong_path_program_and_traces_follow_randomized_pc(monkeypatch, base_seed) -> None:
    monkeypatch.setenv("TB_SEED", str(base_seed))
    scenario, training, recovery = _wrong_path_scenario(SimpleNamespace())
    va = scenario.va
    second_page = (va & ~0xFFF) + 0x1000
    cross_pc = second_page - 2
    wrong_target = second_page + 2
    cross_offset = cross_pc - va
    branch = int.from_bytes(scenario.payload[4:8], "little")

    assert (va + 4) % 32 == 0
    assert _decode_rv32(va + 4, branch) == ("branch", False, wrong_target)
    assert scenario.payload[:4] == b"\x13\x00\x00\x00"
    assert scenario.payload[cross_offset:cross_offset + 8] == b"\x13\x00\x00\x00" * 2
    assert training.entries[1].pc == va + 4
    assert scenario.s1_page_pbmt == (0, 0)
    assert scenario.pma_entries[0].config.cacheable
    assert training.entries[1].target_pc == wrong_target
    assert recovery.entries[-1].pc == cross_pc
    assert recovery.entries[-1].exception == 12
    assert recovery.entries[-2].pc == cross_pc - 2


@pytest.mark.parametrize("base_seed", (1, 2, 3))
def test_mapped_program_keeps_cross_page_nop_at_boundary(monkeypatch, base_seed) -> None:
    monkeypatch.setenv("TB_SEED", str(base_seed))
    env = SimpleNamespace()
    va = _case_addresses(env, "unit-mapped-cross-page", 0x8000_0000).va
    payload, nops = _cross_page_program(va)
    cross_offset = ((va & ~0xFFF) + 0x1000 - 2) - va

    assert nops * 2 == cross_offset
    assert payload[:2] == b"\x01\x00"
    assert payload[cross_offset:cross_offset + 8] == b"\x13\x00\x00\x00" * 2
