from env.monitor import BranchChecker as LegacyBranchChecker
from env.monitor import FrontendMonitor as LegacyFrontendMonitor
from env.monitor import Observation as LegacyObservation
from env.monitors.frontend_monitor import FrontendMonitor, Observation


class _Pin:
    def __init__(self, value=0):
        self.value = int(value)


class _DummyDut:
    def __init__(self):
        self._pins = {}

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        pin = self._pins.get(name)
        if pin is None:
            pin = _Pin(0)
            self._pins[name] = pin
        return pin


NOP = 0x00000013


def _drive_cfvec_slot(dut, *, slot=0, valid=1, pc=0, instr=NOP, is_rvc=0, pred_taken=0):
    dut.__getattr__(f"io_backend_cfVec_{slot}_valid").value = int(valid)
    dut.__getattr__(f"io_backend_cfVec_{slot}_bits_pc").value = int(pc)
    dut.__getattr__(f"io_backend_cfVec_{slot}_bits_instr").value = int(instr)
    dut.__getattr__(f"io_backend_cfVec_{slot}_bits_isRvc").value = int(is_rvc)
    dut.__getattr__(f"io_backend_cfVec_{slot}_bits_predTaken").value = int(pred_taken)


def _clear_cfvec(dut):
    for slot in range(8):
        _drive_cfvec_slot(dut, slot=slot, valid=0)


def test_new_module_exports_frontend_monitor_types():
    assert Observation is LegacyObservation
    assert FrontendMonitor is LegacyFrontendMonitor
    assert LegacyBranchChecker is not None


def test_valid_cfvec_slot_produces_observation():
    dut = _DummyDut()
    monitor = FrontendMonitor()
    monitor.bind(dut)

    _clear_cfvec(dut)
    _drive_cfvec_slot(dut, slot=3, valid=1, pc=0x80000000, instr=NOP, is_rvc=0, pred_taken=1)

    monitor.on_clock_edge(7)

    assert monitor.observations == [
        Observation(
            cycle=7,
            slot=3,
            pc=0x80000000,
            instr=NOP,
            is_rvc=False,
            pred_taken=True,
        )
    ]


def test_notify_redirect_updates_expected_pc_and_sync_state():
    monitor = FrontendMonitor(redirect_sync_max=11)
    monitor.current_cycle = 5

    monitor.notify_redirect(0x80000040, reason="unit", grace_cycles=3)

    assert monitor.expected_pc == 0x80000040
    assert monitor.redirect_grace == 3
    assert monitor.wait_sync_after_redirect is True
    assert monitor.redirect_sync_deadline == 16
    assert monitor.redirect_count == 1


def test_redirect_replay_packets_are_suppressed_during_wait_sync_and_grace():
    dut = _DummyDut()
    monitor = FrontendMonitor()
    monitor.bind(dut)
    monitor.set_expected_pc(0x80000000)

    _clear_cfvec(dut)
    monitor.notify_redirect(0x80000040, reason="unit", grace_cycles=1)

    _drive_cfvec_slot(dut, slot=0, valid=1, pc=0x80000000, instr=NOP)
    monitor.on_clock_edge(0)

    _clear_cfvec(dut)
    _drive_cfvec_slot(dut, slot=0, valid=1, pc=0x80000040, instr=NOP)
    monitor.on_clock_edge(1)

    for cycle in (2, 3):
        _clear_cfvec(dut)
        _drive_cfvec_slot(dut, slot=0, valid=1, pc=0x80000000, instr=NOP)
        monitor.on_clock_edge(cycle)

    assert monitor.get_errors() == []
    assert monitor.wait_sync_after_redirect is False
    assert monitor.redirect_grace == 0
