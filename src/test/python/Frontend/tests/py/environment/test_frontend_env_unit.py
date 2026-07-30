from __future__ import annotations

from env.env_config import DEFAULT_ENV_CONFIG, SV39_ENV_CONFIG
from env.dut_factory import FakeDUTFrontend
from env.frontend_env import FrontendEnv


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
