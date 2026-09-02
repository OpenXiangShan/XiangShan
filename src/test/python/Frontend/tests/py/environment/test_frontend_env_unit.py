from __future__ import annotations

from env.support.env_config import DEFAULT_ENV_CONFIG, SV39_ENV_CONFIG
from env.runtime.dut_factory import FakeDUTFrontend
from env.core.frontend_env import FrontendEnv


def test_bare_mode_initialize_keeps_fetch_in_machine_mode() -> None:
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False)

    env.initialize(reset_vector=0x80000000, bare_mode=True, reset_cycles=0)

    assert dut.io_tlbCsr_priv_virt.value == 0
    assert dut.io_tlbCsr_priv_imode.value == 3
    assert dut.io_tlbCsr_satp_mode.value == 0


def test_sv39_config_initialize_uses_sv39_csr_defaults() -> None:
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False, config=SV39_ENV_CONFIG)

    env.initialize(reset_vector=0x80000000, reset_cycles=0)

    assert dut.io_tlbCsr_priv_virt.value == 0
    assert dut.io_tlbCsr_priv_imode.value == 1
    assert dut.io_tlbCsr_satp_mode.value == 8


def test_sv39_mode_flag_uses_sv39_defaults_on_bare_config() -> None:
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False, config=DEFAULT_ENV_CONFIG)

    env.initialize(reset_vector=0x80000000, bare_mode=False, reset_cycles=0)

    assert dut.io_tlbCsr_priv_virt.value == 0
    assert dut.io_tlbCsr_priv_imode.value == 1
    assert dut.io_tlbCsr_satp_mode.value == 8


def test_pre_drive_observer_samples_before_agents_and_regular_observer(monkeypatch) -> None:
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False)
    order = []

    env.register_pre_drive_cycle_observer(lambda _cycle, _env: order.append("pre"))
    env.register_cycle_observer(lambda _cycle, _env: order.append("regular"))
    monkeypatch.setattr(env.icache_agent, "on_clock_edge", lambda _cycle: order.append("icache"))
    monkeypatch.setattr(env.uncache_agent, "on_clock_edge", lambda _cycle: order.append("uncache"))
    monkeypatch.setattr(env.ptw_agent, "on_clock_edge", lambda _cycle: order.append("ptw"))
    monkeypatch.setattr(env.ptw_full_ppn_checker, "on_clock_edge", lambda _cycle: None)
    monkeypatch.setattr(env.ptw_resp_input_checker, "on_clock_edge", lambda _cycle: None)
    monkeypatch.setattr(env, "_begin_backend_cycle", lambda _cycle: order.append("backend_begin"))
    monkeypatch.setattr(env.monitor, "on_clock_edge", lambda _cycle: None)
    monkeypatch.setattr(env.translation_oracle, "on_clock_edge", lambda _cycle: None)
    monkeypatch.setattr(env, "_drive_backend_cycle", lambda _cycle: order.append("backend_drive"))
    dut.reset.value = 0

    env._on_clock_edge(7)

    assert order == [
        "pre",
        "icache",
        "uncache",
        "ptw",
        "backend_begin",
        "backend_drive",
        "regular",
    ]
