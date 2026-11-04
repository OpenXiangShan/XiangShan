from __future__ import annotations

import pytest

from env.support.env_config import DEFAULT_ENV_CONFIG, SV39_ENV_CONFIG
from env.runtime.dut_factory import FakeDUTFrontend, StrictFakeDUTFrontend
from env.core.frontend_env import FrontendEnv


def test_strict_fake_dut_rejects_unknown_signals() -> None:
    dut = StrictFakeDUTFrontend({"known_signal"}, initial_values={"known_signal": 7})

    assert dut.known_signal.value == 7
    with pytest.raises(AttributeError, match="unknown DUT signal"):
        _ = dut.unknown_signal


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
    monkeypatch.setattr(env.icache_agent, "on_clock_edge", lambda _cycle, _request: order.append("icache"))
    monkeypatch.setattr(env.uncache_agent, "on_clock_edge", lambda _cycle, _request: order.append("uncache"))
    monkeypatch.setattr(env.ptw_agent, "on_clock_edge", lambda _cycle, _sampled: order.append("ptw"))
    monkeypatch.setattr(env.ptw_full_ppn_checker, "on_clock_edge", lambda _cycle: None)
    monkeypatch.setattr(env.ptw_resp_input_checker, "on_clock_edge", lambda _cycle: None)
    monkeypatch.setattr(env, "_begin_backend_cycle", lambda _cycle: order.append("backend_begin"))
    monkeypatch.setattr(env.flow_checker, "on_clock_edge", lambda _cycle: None)
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


def test_walk_vtype_requires_resuming_vtype_at_clock_edge() -> None:
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False)
    dut.reset.value = 0
    env.backend_to_ibuf_if.walk_vtype_valid.value = 1

    with pytest.raises(AssertionError, match="walkVType.valid requires resumingVType"):
        env._on_clock_edge(1)

    env.backend_to_ibuf_if.resuming_vtype.value = 1
    env._on_clock_edge(2)


def test_reference_model_enable_controls_shared_checker(monkeypatch) -> None:
    dut = FakeDUTFrontend()
    dut._is_fake_frontend_dut = False
    monkeypatch.setenv("TB_ENABLE_REFERENCE_MODEL", "0")
    disabled = FrontendEnv(dut, register_callbacks=False)
    assert disabled.reference_model_enabled is False
    assert disabled.fallthrough_checker is None
    assert disabled.mbtb_checker is None
    assert disabled.ubtb_checker is None

    monkeypatch.setenv("TB_ENABLE_REFERENCE_MODEL", "1")
    enabled = FrontendEnv(dut, register_callbacks=False)
    assert enabled.reference_model_enabled is True
    assert enabled.fallthrough_checker is not None
    assert enabled.fallthrough_checker.on_cycle in enabled._cycle_observers
    assert enabled.mbtb_checker is not None
    assert enabled.mbtb_checker.on_cycle in enabled._cycle_observers
    assert enabled.ubtb_checker is not None
    assert enabled.ubtb_checker.on_cycle in enabled._cycle_observers


def test_reference_model_mismatches_are_non_fatal_by_default_and_strict_on_request(monkeypatch) -> None:
    dut = FakeDUTFrontend()
    dut._is_fake_frontend_dut = False
    monkeypatch.delenv("TB_REFERENCE_MODEL_STRICT", raising=False)
    default = FrontendEnv(dut, register_callbacks=False)
    assert default.reference_model_strict is False
    assert default.mbtb_checker.strict is False

    monkeypatch.setenv("TB_REFERENCE_MODEL_STRICT", "1")
    strict = FrontendEnv(dut, register_callbacks=False)
    assert strict.reference_model_strict is True
    assert strict.fallthrough_checker.strict is True
    assert strict.mbtb_checker.strict is True
    assert strict.ubtb_checker.strict is True
