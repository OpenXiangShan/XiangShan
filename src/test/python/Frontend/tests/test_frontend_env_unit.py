from __future__ import annotations

from env.dut_factory import FakeDUTFrontend
from env.frontend_env import FrontendEnv


def test_bare_mode_initialize_keeps_fetch_in_machine_mode() -> None:
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False)

    env.initialize(reset_vector=0x80000000, bare_mode=True, reset_cycles=0)

    assert dut.io_tlbCsr_priv_virt.value == 0
    assert dut.io_tlbCsr_priv_imode.value == 3
    assert dut.io_tlbCsr_satp_mode.value == 0
