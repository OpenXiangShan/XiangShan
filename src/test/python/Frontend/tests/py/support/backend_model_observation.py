"""Shared fakes and cfVec helpers for backend model tests."""

from env.core.backend_model import BackendModel
from env.model.backend_state import QueueInstr
from env.monitors.frontend_monitor import FrontendMonitor


class Signal:
    def __init__(self, value: int = 0) -> None:
        self._value = int(value)
        self.read_count = 0
        self.write_count = 0

    @property
    def value(self) -> int:
        self.read_count += 1
        return self._value

    @value.setter
    def value(self, value: int) -> None:
        self._value = int(value)
        self.write_count += 1


class ObserveIf:
    def __init__(self) -> None:
        self.redirect_valid = Signal()
        self.redirect_bits_pc = Signal()
        self.redirect_bits_target = Signal()
        self.redirect_bits_taken = Signal()
        self.cfvec_valid = [Signal() for _ in range(8)]
        self.cfvec_foldpc = [Signal() for _ in range(8)]
        self.cfvec_instr = [Signal(0x13) for _ in range(8)]
        self.cfvec_is_rvc = [Signal() for _ in range(8)]
        self.cfvec_pred_taken = [Signal() for _ in range(8)]
        self.cfvec_ftq_ptr_flag = [Signal() for _ in range(8)]
        self.cfvec_ftq_ptr_value = [Signal() for _ in range(8)]
        self.cfvec_ftq_offset = [Signal() for _ in range(8)]
        self.cfvec_exception_vec_1 = [Signal() for _ in range(8)]
        self.cfvec_exception_vec_2 = [Signal() for _ in range(8)]
        self.cfvec_exception_vec_12 = [Signal() for _ in range(8)]
        self.cfvec_exception_vec_19 = [Signal() for _ in range(8)]
        self.cfvec_exception_vec_20 = [Signal() for _ in range(8)]


def capture_cfvec(model: BackendModel, interface: ObserveIf):
    monitor = FrontendMonitor()
    monitor.cfvec_if = interface
    snapshot = monitor.capture_cfvec_snapshot(model.current_cycle)
    model.accept_cfvec_snapshot(snapshot)
    return snapshot


def queue_instr(pc: int, ftq_flag: int, ftq_value: int) -> QueueInstr:
    return QueueInstr(
        cycle=0,
        slot=0,
        pc=pc,
        instr=0,
        is_rvc=False,
        pred_taken=False,
        ftq_flag=ftq_flag,
        ftq_value=ftq_value,
        ftq_offset=0,
        is_last_in_entry=False,
    )
