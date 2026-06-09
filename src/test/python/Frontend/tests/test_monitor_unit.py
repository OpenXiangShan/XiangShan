from __future__ import annotations

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


def _set_redirect(interface: _ObserveIf, *, valid: int, pc: int = 0, target: int = 0) -> None:
    interface.redirect_valid.value = int(valid)
    interface.redirect_bits_pc.value = int(pc)
    interface.redirect_bits_target.value = int(target)
    interface.redirect_bits_taken.value = 1 if int(valid) else 0


def _set_first_cfvec(interface: _ObserveIf, pc: int) -> None:
    interface.cfvec_valid[0].value = 1
    interface.cfvec_pc[0].value = int(pc)


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


def test_dut_redirect_allows_wrong_path_cfvec_at_t_and_t_plus_one() -> None:
    monitor, interface = _new_monitor()

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    monitor.on_clock_edge(10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008)
    monitor.on_clock_edge(11)

    assert monitor.get_errors() == []


def test_dut_redirect_requires_target_cfvec_from_t_plus_two() -> None:
    monitor, interface = _new_monitor()

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    monitor.on_clock_edge(10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008)
    monitor.on_clock_edge(11)
    _set_first_cfvec(interface, 0x100c)
    monitor.on_clock_edge(12)

    assert monitor.get_errors()[-1] == {
        "cycle": 12,
        "slot": 0,
        "kind": "REDIRECT_CFVEC_TARGET_MISMATCH",
        "expected": 0x2000,
        "actual": 0x100c,
        "redirect_cycle": 10,
    }


def test_dut_redirect_cfvec_target_match_clears_check() -> None:
    monitor, interface = _new_monitor()

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    monitor.on_clock_edge(10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008)
    monitor.on_clock_edge(11)
    _set_first_cfvec(interface, 0x2000)
    monitor.on_clock_edge(12)
    _set_first_cfvec(interface, 0x2004)
    monitor.on_clock_edge(13)

    assert monitor.get_errors() == []


def test_dut_redirect_t_plus_two_rejects_non_contiguous_cfvec_packet_after_target() -> None:
    monitor, interface = _new_monitor()

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    monitor.on_clock_edge(10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008)
    monitor.on_clock_edge(11)
    _set_first_cfvec(interface, 0x2000)
    _set_second_cfvec(interface, 0x100c)
    monitor.on_clock_edge(12)

    assert monitor.get_errors()[-1] == {
        "cycle": 12,
        "slot": 1,
        "kind": "REDIRECT_CFVEC_TARGET_MISMATCH",
        "expected": 0x2004,
        "actual": 0x100c,
        "redirect_cycle": 10,
    }
