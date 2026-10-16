from __future__ import annotations

from types import SimpleNamespace

import pytest

from env.model.memory_model import MemoryModel
from env.checkers.frontend_flow_checker import FrontendFlowChecker
from env.monitors.frontend_monitor import FrontendMonitor
from env.support import fold_pc, require_matching_foldpc


class _Signal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


class _ObserveIf:
    def __init__(self) -> None:
        self.redirect_valid = _Signal()
        self.redirect_bits_pc = _Signal()
        self.redirect_bits_target = _Signal()
        self.redirect_bits_taken = _Signal()
        self.redirect_bits_level = _Signal()
        self.redirect_bits_debug_is_ctrl = _Signal()
        self.redirect_bits_debug_is_mem_vio = _Signal()
        self.cfvec_valid = [_Signal() for _ in range(8)]
        self.cfvec_full_pc = [0 for _ in range(8)]
        self.cfvec_foldpc = [_Signal() for _ in range(8)]
        self.cfvec_instr = [_Signal(0x13) for _ in range(8)]
        self.cfvec_is_rvc = [_Signal() for _ in range(8)]
        self.cfvec_pred_taken = [_Signal() for _ in range(8)]
        self.cfvec_ftq_ptr_flag = [_Signal() for _ in range(8)]
        self.cfvec_ftq_ptr_value = [_Signal() for _ in range(8)]
        self.cfvec_ftq_offset = [_Signal() for _ in range(8)]
        self.cfvec_is_last_in_ftq_entry = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec_1 = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec_2 = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec_12 = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec_19 = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec_20 = [_Signal() for _ in range(8)]
        self.cfvec_cross_page_ipf_fix = [_Signal() for _ in range(8)]


class _Trace:
    def __init__(self) -> None:
        self.cursor = 1
        self.entries = [object()]


class _BackendModel:
    def __init__(self, interface: _ObserveIf | None = None, *, trace_complete: bool = False) -> None:
        self.golden_trace = _Trace() if trace_complete else None
        self.cfvec_if = interface

    def current_cycle_start_golden_pc(self):
        return None

    def observed_cfvec_snapshot_pc(self, snapshot, slot: int) -> int:
        assert self.cfvec_if is not None
        pc = int(self.cfvec_if.cfvec_full_pc[int(slot)])
        return require_matching_foldpc(pc, int(snapshot.slots[int(slot)].foldpc))


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
    interface.cfvec_full_pc[0] = int(pc)
    interface.cfvec_foldpc[0].value = fold_pc(int(pc))
    interface.cfvec_ftq_ptr_value[0].value = int(ftq_value)
    interface.cfvec_ftq_offset[0].value = int(ftq_offset)


def _set_second_cfvec(interface: _ObserveIf, pc: int) -> None:
    interface.cfvec_valid[1].value = 1
    interface.cfvec_full_pc[1] = int(pc)
    interface.cfvec_foldpc[1].value = fold_pc(int(pc))


def _new_monitor(memory: MemoryModel | None = None) -> tuple[FrontendFlowChecker, _ObserveIf]:
    monitor = FrontendFlowChecker(memory=memory)
    interface = _ObserveIf()
    sampler = FrontendMonitor()
    sampler.cfvec_if = interface
    sampler.backend_to_ftq_if = interface
    sampler.from_ftq_if = SimpleNamespace(
        io_backend_fromFtq_wen=_Signal(),
        io_backend_fromFtq_ftqIdx=_Signal(),
        io_backend_fromFtq_startPc_addr=_Signal(),
    )
    sampler.ptw_if = SimpleNamespace(
        req_0_valid=_Signal(), req_0_ready=_Signal(), req_0_bits_vpn=_Signal(),
        req_0_bits_s2xlate=_Signal(), resp_valid=_Signal(), resp_ready=_Signal(),
    )
    monitor.bind_monitor(sampler)
    monitor.attach_backend_model(_BackendModel(interface))
    return monitor, interface


def _tick(monitor: FrontendFlowChecker, cycle: int) -> None:
    monitor.monitor.capture_cycle_observation(cycle)
    monitor.on_clock_edge(cycle)


@pytest.mark.parametrize("sample_cycle", [None, 9])
def test_checker_requires_current_monitor_snapshot(sample_cycle: int | None) -> None:
    monitor, interface = _new_monitor()
    _set_first_cfvec(interface, 0x1000)
    if sample_cycle is not None:
        monitor.monitor.capture_cycle_observation(sample_cycle)

    with pytest.raises(AssertionError, match="current FrontendMonitor snapshot"):
        monitor.on_clock_edge(10)


def _drive_redirect_to_monitor(monitor: FrontendFlowChecker, interface: _ObserveIf, *, pc: int, target: int) -> None:
    monitor.notify_redirect(target)
    _set_redirect(interface, valid=1, pc=pc, target=target)


def test_dut_redirect_skips_cfvec_at_t_and_t_plus_one() -> None:
    monitor, interface = _new_monitor()

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    _tick(monitor, 10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008)
    _tick(monitor, 11)

    assert monitor.get_errors() == []
    assert monitor.observations == []
    assert monitor.get_stats()["slots_valid"] == 0


def test_rvc_addi_x0_hint_matches_canonical_nop() -> None:
    assert FrontendFlowChecker._instr_equivalent_for_compare(0x00100013, 0x00000013, True) is True
    assert FrontendFlowChecker._instr_equivalent_for_compare(0x00100013, 0x00000013, False) is False


def test_monitor_does_not_replace_zero_cfvec_instr_from_memory() -> None:
    monitor, interface = _new_monitor(memory=MemoryModel())

    _set_first_cfvec(interface, 0x8000_1000)
    interface.cfvec_instr[0].value = 0
    interface.cfvec_is_rvc[0].value = 0
    _tick(monitor, 20)

    assert monitor.get_errors()[0]["kind"] == "INSTR_MISMATCH"
    assert monitor.get_errors()[0]["expected"] == 0x13
    assert monitor.get_errors()[0]["actual"] == 0


def test_monitor_reports_cfvec_pc_size_memory_mismatch_for_invalid_raw16() -> None:
    memory = MemoryModel()
    memory.write_u16(0x8000_3248, 0)
    monitor, interface = _new_monitor(memory=memory)

    _set_first_cfvec(interface, 0x8000_3248)
    interface.cfvec_instr[0].value = 0x13
    interface.cfvec_is_rvc[0].value = 1
    _tick(monitor, 20)

    assert monitor.get_errors()[0]["kind"] == "CFVEC_PC_SIZE_MEMORY_MISMATCH"
    assert monitor.get_errors()[0]["pc"] == 0x8000_3248
    assert monitor.get_errors()[0]["raw16"] == 0


def test_monitor_skips_instr_compare_for_exception_marked_cfvec() -> None:
    memory = MemoryModel()
    memory.write_u16(0x8000_3248, 0)
    monitor, interface = _new_monitor(memory=memory)

    _set_first_cfvec(interface, 0x8000_3248)
    interface.cfvec_instr[0].value = 0x05130000
    interface.cfvec_is_rvc[0].value = 1
    interface.cfvec_exception_vec_2[0].value = 1
    _tick(monitor, 20)

    assert monitor.get_errors() == []
    assert monitor.observations[0].pc == 0x8000_3248



def test_reported_fetch_fault_suppresses_following_instr_compare() -> None:
    memory = MemoryModel()
    monitor, interface = _new_monitor(memory=memory)

    _set_first_cfvec(interface, 0x40)
    interface.cfvec_instr[0].value = 0
    interface.cfvec_exception_vec_1[0].value = 1
    _tick(monitor, 20)

    interface.cfvec_exception_vec_1[0].value = 0
    interface.cfvec_instr[0].value = 0
    _set_first_cfvec(interface, 0x44)
    _tick(monitor, 21)

    assert monitor.get_errors() == []
    assert monitor.exception_mark_count == 1


def test_dut_redirect_skip_window_does_not_wait_for_target_cfvec() -> None:
    monitor, interface = _new_monitor()
    monitor.redirect_sync_max = 2

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    _tick(monitor, 10)

    _set_redirect(interface, valid=0)
    interface.cfvec_valid[0].value = 0
    _tick(monitor, 11)
    _tick(monitor, 12)
    _tick(monitor, 13)

    assert monitor.get_errors() == []


def test_dut_redirect_skips_model_observation_window_until_recovery_check() -> None:
    monitor, interface = _new_monitor()

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    _tick(monitor, 10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008)
    _tick(monitor, 11)

    assert monitor.get_errors() == []
    assert monitor.observations == []
    assert monitor.get_stats()["slots_valid"] == 0


def test_dut_redirect_first_sampled_cfvec_after_skip_must_be_target() -> None:
    monitor, interface = _new_monitor()

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004)
    _tick(monitor, 10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008)
    _tick(monitor, 11)

    _set_first_cfvec(interface, 0x2004)
    _tick(monitor, 12)

    assert monitor.get_errors()[0]["kind"] == "REDIRECT_RECOVERY_TARGET_MISMATCH"
    assert monitor.get_errors()[0]["expected"] == 0x2000
    assert monitor.get_errors()[0]["actual"] == 0x2004
    assert monitor.observations == []


def test_redirect_recovery_accepts_ftq_derived_exception_pc_with_matching_foldpc() -> None:
    monitor, interface = _new_monitor()
    target = 0x8000_0FFE

    _drive_redirect_to_monitor(monitor, interface, pc=target, target=target)
    _tick(monitor, 10)
    _set_redirect(interface, valid=0)
    _tick(monitor, 11)

    _set_first_cfvec(interface, target)
    interface.cfvec_exception_vec_1[0].value = 1
    _tick(monitor, 12)

    assert monitor.get_errors() == []
    assert monitor.get_stats()["exception_mark_count"] == 1
    assert monitor.observations[0].foldpc == fold_pc(target)


def test_redirect_recovery_unguards_high_half_target() -> None:
    monitor, interface = _new_monitor()
    high_pc = (1 << 50) - 0x2000
    guarded_pc = (1 << 51) - 0x2000

    _set_redirect(interface, valid=1, pc=0x1004, target=guarded_pc)
    _tick(monitor, 10)
    _set_redirect(interface, valid=0)
    _tick(monitor, 11)
    _set_first_cfvec(interface, high_pc)
    _tick(monitor, 12)

    assert monitor.get_errors() == []
    assert monitor.last_dut_redirect["target_pc"] == high_pc


def test_redirect_recovery_rejects_ftq_derived_pc_with_wrong_foldpc() -> None:
    monitor, interface = _new_monitor()
    target = 0x8000_0FFE

    _drive_redirect_to_monitor(monitor, interface, pc=target, target=target)
    _tick(monitor, 10)
    _set_redirect(interface, valid=0)
    _tick(monitor, 11)

    _set_first_cfvec(interface, target)
    interface.cfvec_foldpc[0].value = fold_pc(target) ^ 1
    interface.cfvec_exception_vec_1[0].value = 1
    with pytest.raises(AssertionError, match="cfVec foldpc does not match FTQ-derived PC"):
        _tick(monitor, 12)


def test_dut_redirect_to_mmio_target_waits_for_recovery_cfvec() -> None:
    monitor, interface = _new_monitor(memory=MemoryModel())

    _drive_redirect_to_monitor(monitor, interface, pc=0x8000_1000, target=0x1000)
    _set_first_cfvec(interface, 0x8000_1004)
    _tick(monitor, 10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x8000_1008)
    _tick(monitor, 11)

    interface.cfvec_valid[0].value = 0
    _tick(monitor, 12)

    assert monitor.get_errors() == []
    assert monitor._recovery_target_pc == 0x1000

    _set_first_cfvec(interface, 0x1000)
    interface.cfvec_exception_vec_1[0].value = 1
    _tick(monitor, 13)

    assert monitor.get_errors() == []
    assert monitor._recovery_target_pc is None
    assert monitor.observations[0].pc == 0x1000
    assert monitor.exception_mark_count == 1


def test_dut_redirect_to_mmio_target_rejects_wrong_first_cfvec() -> None:
    monitor, interface = _new_monitor(memory=MemoryModel())

    _drive_redirect_to_monitor(monitor, interface, pc=0x8000_1000, target=0x1000)
    _tick(monitor, 10)
    _set_redirect(interface, valid=0)
    _tick(monitor, 11)
    _set_first_cfvec(interface, 0x1004)
    interface.cfvec_exception_vec_1[0].value = 1
    _tick(monitor, 12)

    assert monitor.get_errors()[0]["kind"] == "REDIRECT_RECOVERY_TARGET_MISMATCH"
    assert monitor.get_errors()[0]["expected"] == 0x1000
    assert monitor.get_errors()[0]["actual"] == 0x1004


def test_dut_redirect_clears_stale_ftq_tracking_before_recovery_target() -> None:
    monitor, interface = _new_monitor()

    _set_first_cfvec(interface, 0x1000, ftq_value=7, ftq_offset=0)
    _tick(monitor, 8)

    _drive_redirect_to_monitor(monitor, interface, pc=0x1000, target=0x2000)
    _set_first_cfvec(interface, 0x1004, ftq_value=7, ftq_offset=2)
    _tick(monitor, 10)

    _set_redirect(interface, valid=0)
    _set_first_cfvec(interface, 0x1008, ftq_value=7, ftq_offset=4)
    _tick(monitor, 11)

    _set_first_cfvec(interface, 0x2000, ftq_value=7, ftq_offset=0)
    _tick(monitor, 12)

    assert monitor.get_errors() == []
    assert [obs.pc for obs in monitor.observations] == [0x1000, 0x2000]


def test_monitor_ignores_cfvec_after_golden_trace_complete() -> None:
    monitor, interface = _new_monitor()
    monitor.attach_backend_model(_BackendModel(interface, trace_complete=True))

    _set_first_cfvec(interface, 0)
    interface.cfvec_is_rvc[0].value = 1
    _tick(monitor, 20)

    assert monitor.get_errors() == []
    assert monitor.observations == []
