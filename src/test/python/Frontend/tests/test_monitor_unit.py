from __future__ import annotations

from env.model.memory_model import MemoryModel
from env.monitors.frontend_monitor import FrontendMonitor


class _Signal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


class _ObserveIf:
    def __init__(self) -> None:
        self.redirect_valid = _Signal()
        self.redirect_bits_pc = _Signal()
        self.redirect_bits_target = _Signal()
        self.redirect_bits_taken = _Signal()
        self.cfvec_valid = [_Signal() for _ in range(8)]
        self.cfvec_pc = [_Signal() for _ in range(8)]
        self.cfvec_instr = [_Signal(0x13) for _ in range(8)]
        self.cfvec_is_rvc = [_Signal() for _ in range(8)]
        self.cfvec_pred_taken = [_Signal() for _ in range(8)]
        self.cfvec_ftq_ptr_flag = [_Signal() for _ in range(8)]
        self.cfvec_ftq_ptr_value = [_Signal() for _ in range(8)]
        self.cfvec_ftq_offset = [_Signal() for _ in range(8)]
        self.cfvec_is_last_in_ftq_entry = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec = [[_Signal() for _ in range(24)] for _ in range(8)]


class _Trace:
    def __init__(self) -> None:
        self.cursor = 1
        self.entries = [object()]


class _BackendModel:
    def __init__(self) -> None:
        self.golden_trace = _Trace()


def _set_redirect(interface: _ObserveIf, *, valid: int, pc: int = 0, target: int = 0) -> None:
    interface.redirect_valid.value = int(valid)
    interface.redirect_bits_pc.value = int(pc)
    interface.redirect_bits_target.value = int(target)
    interface.redirect_bits_taken.value = 1 if int(valid) else 0


def _set_first_cfvec(
    interface: _ObserveIf,
    pc: int,
    *,
    ftq_value: int = 0,
    ftq_offset: int = 0,
) -> None:
    interface.cfvec_valid[0].value = 1
    interface.cfvec_pc[0].value = int(pc)
    interface.cfvec_ftq_ptr_value[0].value = int(ftq_value)
    interface.cfvec_ftq_offset[0].value = int(ftq_offset)


def _set_second_cfvec(interface: _ObserveIf, pc: int) -> None:
    interface.cfvec_valid[1].value = 1
    interface.cfvec_pc[1].value = int(pc)


def _new_monitor() -> tuple[FrontendMonitor, _ObserveIf]:
    monitor = FrontendMonitor()
    interface = _ObserveIf()
    monitor.interface = interface
    return monitor, interface


def _drive_redirect_to_monitor(monitor: FrontendMonitor, interface: _ObserveIf, *, pc: int, target: int) -> None:
    monitor.notify_redirect(target)
    _set_redirect(interface, valid=1, pc=pc, target=target)


def test_dut_redirect_skips_cfvec_at_t_and_t_plus_one() -> None:
    monitor, interface = _new_monitor()

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    monitor.on_clock_edge(10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008)
    monitor.on_clock_edge(11)

    assert monitor.get_errors() == []
    assert monitor.observations == []
    assert monitor.get_stats()["slots_valid"] == 0


def test_rvc_addi_x0_hint_matches_canonical_nop() -> None:
    assert FrontendMonitor._instr_equivalent_for_compare(0x00100013, 0x00000013, True) is True
    assert FrontendMonitor._instr_equivalent_for_compare(0x00100013, 0x00000013, False) is False


def test_monitor_does_not_replace_zero_cfvec_instr_from_memory() -> None:
    monitor = FrontendMonitor(memory=MemoryModel())
    interface = _ObserveIf()
    monitor.interface = interface

    _set_first_cfvec(interface, 0x8000_1000)
    interface.cfvec_instr[0].value = 0
    interface.cfvec_is_rvc[0].value = 0
    monitor.on_clock_edge(20)

    assert monitor.get_errors()[0]["kind"] == "INSTR_MISMATCH"
    assert monitor.get_errors()[0]["expected"] == 0x13
    assert monitor.get_errors()[0]["actual"] == 0


def test_monitor_reports_cfvec_pc_size_memory_mismatch_for_invalid_raw16() -> None:
    memory = MemoryModel()
    memory.write_u16(0x8000_3248, 0)
    monitor = FrontendMonitor(memory=memory)
    interface = _ObserveIf()
    monitor.interface = interface

    _set_first_cfvec(interface, 0x8000_3248)
    interface.cfvec_instr[0].value = 0x13
    interface.cfvec_is_rvc[0].value = 1
    monitor.on_clock_edge(20)

    assert monitor.get_errors()[0]["kind"] == "CFVEC_PC_SIZE_MEMORY_MISMATCH"
    assert monitor.get_errors()[0]["pc"] == 0x8000_3248
    assert monitor.get_errors()[0]["raw16"] == 0


def test_monitor_skips_instr_compare_for_exception_marked_cfvec() -> None:
    memory = MemoryModel()
    memory.write_u16(0x8000_3248, 0)
    monitor = FrontendMonitor(memory=memory)
    interface = _ObserveIf()
    monitor.interface = interface

    _set_first_cfvec(interface, 0x8000_3248)
    interface.cfvec_instr[0].value = 0x05130000
    interface.cfvec_is_rvc[0].value = 1
    interface.cfvec_exception_vec[0][2].value = 1
    monitor.on_clock_edge(20)

    assert monitor.get_errors() == []
    assert monitor.observations[0].pc == 0x8000_3248


def test_dut_redirect_skip_window_does_not_wait_for_target_cfvec() -> None:
    monitor, interface = _new_monitor()
    monitor.redirect_sync_max = 2

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    monitor.on_clock_edge(10)

    _set_redirect(interface, valid=0)
    interface.cfvec_valid[0].value = 0
    monitor.on_clock_edge(11)
    monitor.on_clock_edge(12)
    monitor.on_clock_edge(13)

    assert monitor.get_errors() == []


def test_dut_redirect_skips_model_observation_window_until_recovery_check() -> None:
    monitor, interface = _new_monitor()

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    monitor.on_clock_edge(10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008)
    monitor.on_clock_edge(11)

    assert monitor.get_errors() == []
    assert monitor.observations == []
    assert monitor.get_stats()["slots_valid"] == 0


def test_dut_redirect_first_sampled_cfvec_after_skip_must_be_target() -> None:
    monitor, interface = _new_monitor()

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    monitor.on_clock_edge(10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008)
    monitor.on_clock_edge(11)

    _set_first_cfvec(interface, 0x2004)
    monitor.on_clock_edge(12)

    assert monitor.get_errors()[0]["kind"] == "REDIRECT_RECOVERY_TARGET_MISMATCH"
    assert monitor.get_errors()[0]["expected"] == 0x2000
    assert monitor.get_errors()[0]["actual"] == 0x2004
    assert monitor.observations == []


def test_dut_redirect_to_mmio_target_does_not_require_recovery_cfvec() -> None:
    monitor = FrontendMonitor(memory=MemoryModel())
    interface = _ObserveIf()
    monitor.interface = interface

    _drive_redirect_to_monitor(monitor, interface, pc=0x8000_1000, target=0x1000)
    _set_first_cfvec(interface, 0x8000_1004)
    monitor.on_clock_edge(10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x8000_1008)
    monitor.on_clock_edge(11)

    _set_first_cfvec(interface, 0)
    monitor.on_clock_edge(12)

    assert monitor.get_errors() == []
    assert all(obs.pc != 0 for obs in monitor.observations)


def test_dut_redirect_clears_stale_ftq_tracking_before_recovery_target() -> None:
    monitor, interface = _new_monitor()

    _set_first_cfvec(interface, 0x1000, ftq_value=7, ftq_offset=0)
    monitor.on_clock_edge(8)

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004, ftq_value=7, ftq_offset=2)
    monitor.on_clock_edge(10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008, ftq_value=7, ftq_offset=4)
    monitor.on_clock_edge(11)

    _set_first_cfvec(interface, 0x2000, ftq_value=7, ftq_offset=0)
    monitor.on_clock_edge(12)

    assert monitor.get_errors() == []
    assert [obs.pc for obs in monitor.observations] == [0x1000, 0x2000]


def test_monitor_ignores_cfvec_after_golden_trace_complete() -> None:
    monitor, interface = _new_monitor()
    monitor.attach_backend_model(_BackendModel())

    _set_first_cfvec(interface, 0)
    interface.cfvec_is_rvc[0].value = 1
    monitor.on_clock_edge(20)

    assert monitor.get_errors() == []
    assert monitor.observations == []
