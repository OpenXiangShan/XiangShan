import env.frontend_env as frontend_env_module


class _Pin:
    def __init__(self, value=0):
        self.value = int(value)


class _DummyDut:
    def __init__(self):
        self.step_ris_callbacks = []

    def StepRis(self, callback):
        self.step_ris_callbacks.append(callback)

    def Step(self, cycles):
        return int(cycles)


def test_frontend_env_exposes_phase1_bundles():
    env = frontend_env_module.FrontendEnv(_DummyDut(), register_callbacks=False)

    assert env.clock_reset is not None
    assert env.icache_if is not None
    assert env.uncache_if is not None
    assert env.ptw_if is not None
    assert env.backend_ctrl_if is not None
    assert env.backend_observe_if is not None
    assert env.backend_from_ftq_if is not None
    assert env.frontend_info_if is not None
    assert env.csr_ctrl_if is not None


def test_frontend_env_binds_bundle_interfaces_to_dummy_pins():
    dut = _DummyDut()
    dut.reset = _Pin(0)
    dut.clock = _Pin(0)
    dut.io_reset_vector_addr = _Pin(0)
    dut.io_fencei = _Pin(0)

    env = frontend_env_module.FrontendEnv(dut, register_callbacks=False)

    env.clock_reset.drive_idle()

    assert int(dut.reset.value) == 1
    assert int(dut.clock.value) == 0
