from types import SimpleNamespace

import pytest

from env.agents.icache_ecc_injection_agent import ICacheECCInjectionAgent, ICacheInjectionUnavailable


class Word:
    def __init__(self, width, value=0):
        self.width, self.value = width, value

    def W(self):
        return self.width

    def ImmSet(self, value):
        self.value = value


def make_agent():
    signals = {}
    events = []
    dut = SimpleNamespace(GetInternalSignal=lambda name, **kwargs: signals.get(name))
    agent = ICacheECCInjectionAgent(SimpleNamespace(dut=dut, _emit_event=lambda *args: events.append(args)))
    base = 0x80000000
    signals[agent.ROOT + "metaArray.banks_0.validArray_0"] = Word(4, 3)
    for split in range(2):
        path = agent.ROOT + f"metaArray.banks_0.tagArray.array_0_{split}_0.array.array_ext.Memory"
        signals[path] = [Word(138) for _ in range(128)]
    pair = signals[agent.ROOT + "metaArray.banks_0.tagArray.array_0_0_0.array.array_ext.Memory"][0]
    pair.value = ((base >> 12) << 33) | ((((base + 0x4000) >> 12) << 33) << 69)
    data_path = agent.ROOT + "dataArray.banks_3.ways_0.array.array_ext.Memory"
    signals[data_path] = [Word(66, 0x123456789ABCDEF0 << 2) for _ in range(256)]
    return agent, signals, pair, base, events


def test_meta_code_changes_only_requested_way_and_restores_neighbor_safely():
    agent, _, pair, base, events = make_agent()
    original = pair.value
    item = agent.inject_meta_ecc(base)
    assert pair.value == original ^ 1
    pair.value ^= 1 << 75  # Independent RTL write to the other packed way.
    agent.verify_persisted(item)
    agent.restore_all()
    assert pair.value == original ^ (1 << 75)
    assert [event[0].rsplit(".", 1)[-1] for event in events] == ["deposit", "persisted", "restore"]


def test_data_injection_preserves_instruction_bits_and_replacement():
    agent, signals, _, base, _ = make_agent()
    item = agent.inject_data_ecc(base, bank=3)
    word = signals[item.path][item.row]
    assert item.before >> 2 == item.after >> 2
    assert item.before ^ item.after == 2
    word.value = 0x23456789 << 2  # A refill replaces the entire word.
    agent.restore_all()
    assert word.value == 0x23456789 << 2


def test_multiway_clones_only_into_valid_resident_destination():
    agent, signals, pair, base, _ = make_agent()
    original = pair.value
    agent.clone_meta_to_second_way(base, dest_way=1)
    assert agent.resident_ways(base) == (0, 1)
    agent.restore_all()
    assert pair.value == original
    with pytest.raises(AssertionError, match="warm a second"):
        agent.clone_meta_to_second_way(base, dest_way=2)


def test_missing_and_wrong_width_arrays_fail_without_mutating():
    agent, signals, pair, base, _ = make_agent()
    before = pair.value
    pair.width = 137
    with pytest.raises(ICacheInjectionUnavailable, match="138-bit"):
        agent.inject_meta_ecc(base)
    assert pair.value == before
    with pytest.raises(ValueError, match="bank"):
        agent.inject_data_ecc(base, bank=8)


def test_overwritten_injection_is_rejected():
    agent, _, pair, base, _ = make_agent()
    item = agent.inject_meta_ecc(base)
    pair.value = item.before
    with pytest.raises(AssertionError, match="overwritten"):
        agent.verify_persisted(item)
