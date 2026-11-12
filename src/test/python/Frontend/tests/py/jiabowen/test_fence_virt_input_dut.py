"""Check the independent data and fence virtualization input bindings."""

import os

import pytest


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_fence_virtualization_input_is_independent_of_data_context(env):
    env.update_translation_context(priv_virt=0, priv_fence_virt=1)
    env.step(1)
    assert int(env.dut.io_tlbCsr_priv_virt.value) == 0
    assert int(env.dut.io_tlbCsr_priv_fenceVirt.value) == 1

    env.update_translation_context(priv_virt=1, priv_fence_virt=0)
    env.step(1)
    assert int(env.dut.io_tlbCsr_priv_virt.value) == 1
    assert int(env.dut.io_tlbCsr_priv_fenceVirt.value) == 0
    assert not env.monitor.get_errors()
