from __future__ import annotations

from types import SimpleNamespace

import pytest

from env.agents.ptw_agent import PTWAgent


class _CountingSignal:
    def __init__(self, value: int = 0) -> None:
        self._value = int(value)
        self.write_count = 0

    @property
    def value(self) -> int:
        return self._value

    @value.setter
    def value(self, value: int) -> None:
        self._value = int(value)
        self.write_count += 1


class _SignalList:
    def __init__(self, count: int) -> None:
        self.signals = [_CountingSignal() for _ in range(count)]

    def __iter__(self):
        return iter(self.signals)


def _interface() -> SimpleNamespace:
    scalar_names = (
        "resp_valid",
        "resp_bits_s2xlate",
        "resp_bits_s2_entry_tag",
        "resp_bits_s2_entry_vmid",
        "resp_bits_s2_entry_n",
        "resp_bits_s2_entry_pbmt",
        "resp_bits_s2_entry_perm_d",
        "resp_bits_s2_entry_perm_r",
        "resp_bits_s2_entry_perm_w",
        "resp_bits_s2_entry_perm_a",
        "resp_bits_s2_entry_perm_x",
        "resp_bits_s2_entry_level",
        "resp_bits_s2_entry_ppn",
        "resp_bits_s2_gpf",
        "resp_bits_s2_gaf",
        "resp_bits_s1_entry_tag",
        "resp_bits_s1_entry_asid",
        "resp_bits_s1_entry_vmid",
        "resp_bits_s1_entry_n",
        "resp_bits_s1_entry_pbmt",
        "resp_bits_s1_entry_perm_d",
        "resp_bits_s1_entry_perm_a",
        "resp_bits_s1_entry_perm_g",
        "resp_bits_s1_entry_perm_u",
        "resp_bits_s1_entry_perm_x",
        "resp_bits_s1_entry_perm_w",
        "resp_bits_s1_entry_perm_r",
        "resp_bits_s1_entry_level",
        "resp_bits_s1_entry_v",
        "resp_bits_s1_entry_ppn",
        "resp_bits_s1_addr_low",
        "resp_bits_s1_pf",
        "resp_bits_s1_af",
    )
    interface = SimpleNamespace(**{name: _CountingSignal() for name in scalar_names})
    interface.resp_bits_s1_ppn_low = _SignalList(8)
    interface.resp_bits_s1_valididx = _SignalList(8)
    interface.resp_bits_s1_pteidx = _SignalList(8)
    return interface


def _write_count(interface: SimpleNamespace) -> int:
    total = sum(
        getattr(interface, name).write_count
        for name in vars(interface)
        if hasattr(getattr(interface, name), "write_count")
    )
    for name in ("resp_bits_s1_ppn_low", "resp_bits_s1_valididx", "resp_bits_s1_pteidx"):
        total += sum(signal.write_count for signal in getattr(interface, name).signals)
    return total


def test_ptw_idle_does_not_rewrite_valid_or_payload() -> None:
    interface = _interface()
    agent = PTWAgent.__new__(PTWAgent)
    agent.interface = interface
    agent._resp_valid_driven = 0
    agent._driven_response_payload = None

    agent._drive_response(None, 0)
    agent._drive_response(None, 0)

    assert _write_count(interface) == 0


def test_ptw_response_payload_is_written_once_and_valid_drops_once() -> None:
    interface = _interface()
    agent = PTWAgent.__new__(PTWAgent)
    agent.interface = interface
    agent._resp_valid_driven = 0
    agent._driven_response_payload = None
    response = {"s2xlate": 1, "s1_ppn_low": [3] * 8}

    agent._drive_response(response, 1)
    first_write_count = _write_count(interface)
    agent._drive_response(response, 1)

    assert first_write_count > 1
    assert _write_count(interface) == first_write_count
    assert int(interface.resp_valid.value) == 1

    next_response = {"s2xlate": 0, "s1_ppn_low": [4] * 8}
    agent._drive_response(next_response, 1)
    second_write_count = _write_count(interface)
    assert second_write_count > first_write_count
    assert int(interface.resp_valid.value) == 1

    agent._drive_response(None, 0)
    after_valid_drop = _write_count(interface)
    agent._drive_response(None, 0)

    assert after_valid_drop == second_write_count + 1
    assert _write_count(interface) == after_valid_drop
    assert int(interface.resp_valid.value) == 0


def test_ptw_drives_new_stage_permissions_from_response() -> None:
    interface = _interface()
    agent = PTWAgent.__new__(PTWAgent)
    agent.interface = interface
    agent._resp_valid_driven = 0
    agent._driven_response_payload = None
    agent._drive_response({"s1_entry_perm_d": 1, "s2_entry_perm_d": 1,
                           "s2_entry_perm_r": 1, "s2_entry_perm_w": 0}, 1)
    assert [getattr(interface, "resp_bits_" + key).value for key in (
        "s1_entry_perm_d", "s2_entry_perm_d", "s2_entry_perm_r", "s2_entry_perm_w"
    )] == [1, 1, 1, 0]


def test_nemu_adapter_must_supply_new_permissions() -> None:
    agent = PTWAgent.__new__(PTWAgent)
    agent._nemu_source = SimpleNamespace(build_response=lambda _snapshot: {})
    with pytest.raises(ValueError, match="missing required permission fields"):
        agent._build_nemu_response(None)
    fields = {key: 0 for key in ("s1_entry_perm_d", "s2_entry_perm_d",
                                  "s2_entry_perm_r", "s2_entry_perm_w")}
    fields["s2_entry_perm_r"] = 2
    agent._nemu_source = SimpleNamespace(build_response=lambda _snapshot: fields)
    with pytest.raises(ValueError, match="must be bits"):
        agent._build_nemu_response(None)
