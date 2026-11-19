from __future__ import annotations

from .scenario_coordinator import (
    BIN951_FTQ_SIGNAL_NAMES, BIN951_IFU_SIGNAL_NAMES, BIN951_REDIRECT_SIGNAL_NAMES,
)


class _FakeSignal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


class _FakeICacheAgent:
    def __init__(self) -> None:
        self.configuration: dict = {}

    def configure(self, **configuration) -> None:
        self.configuration = dict(configuration)


class _FakeUncacheAgent:
    def __init__(self, interface) -> None:
        self.interface = interface
        self.a_ready_override = None
        self.event_sink = None
        self.overrides: list[int | None] = []

    def set_a_ready(self, value) -> None:
        self.a_ready_override = None if value is None else (1 if int(value) else 0)
        self.overrides.append(self.a_ready_override)

    def on_clock_edge(self, cycle: int) -> None:
        ready = 1 if self.a_ready_override is None else int(self.a_ready_override)
        self.interface.a_ready.value = ready
        if int(self.interface.a_valid.value) != 1 or ready != 1:
            return
        if self.event_sink is not None:
            self.event_sink(
                {
                    "type": "handshake.uncache_a",
                    "source": "uncache_agent",
                    "cycle": int(cycle),
                    "payload": {
                        "address": int(self.interface.a_bits_address.value),
                        "mem_back_type_mm": int(
                            self.interface.a_bits_user_mem_back_type_mm.value
                        ),
                        "mem_page_type_nc": int(
                            self.interface.a_bits_user_mem_page_type_nc.value
                        ),
                    },
                }
            )


class _FakeBackendModel:
    def __init__(self, can_accept_signal: _FakeSignal) -> None:
        self.can_accept_signal = can_accept_signal
        self.values: list[int] = []

    def set_can_accept(self, value: int) -> None:
        normalized = 1 if int(value) else 0
        self.values.append(normalized)
        self.can_accept_signal.value = normalized


class _FakeCoordinatorEnv:
    def __init__(self, previous_event_sink) -> None:
        self.event_sink = previous_event_sink
        self.backend_ctrl_if = type(
            "BackendControl",
            (),
            {"can_accept": _FakeSignal(1), "redirect_valid": _FakeSignal(0)},
        )()
        self.frontend_info_if = type(
            "FrontendInfo",
            (),
            {"io_frontendInfo_ibufFull": _FakeSignal(0)},
        )()
        self.clock_reset = type("ClockReset", (), {"reset": _FakeSignal(0)})()
        self.uncache_if = type(
            "UncacheInterface",
            (),
            {
                "a_valid": _FakeSignal(0),
                "a_ready": _FakeSignal(1),
                "a_bits_address": _FakeSignal(0),
                "a_bits_user_mem_back_type_mm": _FakeSignal(0),
                "a_bits_user_mem_page_type_nc": _FakeSignal(0),
            },
        )()
        self.icache_agent = _FakeICacheAgent()
        self.uncache_agent = _FakeUncacheAgent(self.uncache_if)
        self.backend_model = _FakeBackendModel(self.backend_ctrl_if.can_accept)
        self.dut = None
        self.cycle_observers: list = []
        self.pre_drive_cycle_observers: list = []

    def set_event_sink(self, sink) -> None:
        self.event_sink = sink
        self.uncache_agent.event_sink = sink

    def register_cycle_observer(self, observer) -> None:
        self.cycle_observers.append(observer)

    def register_pre_drive_cycle_observer(self, observer) -> None:
        self.pre_drive_cycle_observers.append(observer)

    def drive_uncache_edge(self, cycle: int) -> None:
        for observer in self.pre_drive_cycle_observers:
            if getattr(observer, "__name__", "") == "observe_uncache_a_edge":
                observer(cycle, self)
        self.uncache_agent.on_clock_edge(cycle)


def _coordinator_program_map() -> dict:
    return {
        "binary_text_verified": True,
        "binary_sha256": "program-binary-sha256",
        "instructions": [
            {
                "pc": 0x8000003E,
                "size": 4,
                "episode_id": "boundary-episode",
                "crosses_fetch_block": True,
                "crosses_cacheline": True,
                "crosses_page": False,
                "conditional_branch": False,
                "call": False,
                "return": False,
            }
        ],
    }


class _FakeDutSignal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


def _fake_bin951_dut():
    dut = type("FakeBin951Dut", (), {})()
    for name in BIN951_REDIRECT_SIGNAL_NAMES.values():
        setattr(dut, name, _FakeDutSignal())
    for name in BIN951_IFU_SIGNAL_NAMES.values():
        setattr(dut, name, _FakeDutSignal())
    for name in BIN951_FTQ_SIGNAL_NAMES.values():
        setattr(dut, name, _FakeDutSignal())
    return dut
