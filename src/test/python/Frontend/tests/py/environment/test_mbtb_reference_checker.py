from types import SimpleNamespace
import logging

import pytest

from env.checkers.mbtb_reference_checker import MbtbReferenceChecker


_BPU = "Frontend_top.Frontend.inner_bpu."
_MBTB = _BPU + "mbtb."


class _Dut:
    def __init__(self) -> None:
        self.signals = {
            _MBTB + "io_enable": 1,
            _MBTB + "io_stageCtrl_s2_fire": 0,
            _MBTB + "io_stageCtrl_s3_fire": 0,
            _MBTB + "io_stageCtrl_t0_fire": 1,
            _MBTB + "io_train_startPcVec_dup_0_0_addr": 0x80000000 >> 1,
            _BPU + "s2_startPc_dup_0_addr": 0x80000000 >> 1,
        }
        for slot in range(8):
            self.signals[_MBTB + f"io_train_branches_{slot}_valid"] = 0
        for slot in range(10):
            self.signals[_MBTB + f"io_result_{slot}_valid"] = 0
        for align in range(2):
            self.signals[_MBTB + f"alignBanks_{align}.io_write_req_valid"] = 0

    def GetInternalSignal(self, name):
        return SimpleNamespace(value=self.signals[name])


def test_mbtb_checker_compares_known_hit_after_set_conflict():
    dut = _Dut()
    env = SimpleNamespace(dut=dut, clock_reset=SimpleNamespace(reset=SimpleNamespace(value=0)))
    checker = MbtbReferenceChecker(strict=True)
    train = _MBTB + "io_train_branches_0_"
    dut.signals.update({
        train + "valid": 1,
        train + "bits_taken": 1,
        train + "bits_cfiPosition": 2,
        train + "bits_target_addr": 0x80000080 >> 1,
        train + "bits_attribute_branchType": 2,
        train + "bits_attribute_rasAction": 0,
    })
    checker.on_cycle(1, env)

    result = _MBTB + "io_result_0_"
    dut.signals.update({
        _MBTB + "io_stageCtrl_t0_fire": 0,
        _MBTB + "io_stageCtrl_s2_fire": 1,
        result + "valid": 1,
        result + "bits_cfiPosition": 2,
        result + "bits_target_addr": 0x80000080 >> 1,
        result + "bits_attribute_branchType": 2,
        result + "bits_attribute_rasAction": 0,
        result + "bits_taken": 1,
    })
    checker.on_cycle(2, env)
    assert checker.checked_count == 1

    dut.signals[result + "bits_taken"] = 0
    checker.on_cycle(3, env)
    dut.signals[result + "bits_taken"] = 1

    dut.signals[result + "bits_target_addr"] = 0x800000C0 >> 1
    with pytest.raises(AssertionError, match="mbtb_reference"):
        checker.on_cycle(4, env)
    dut.signals[result + "bits_target_addr"] = 0x80000080 >> 1

    dut.signals[_MBTB + "io_stageCtrl_s2_fire"] = 0
    dut.signals[_MBTB + "io_stageCtrl_t0_fire"] = 1
    dut.signals[_MBTB + "io_train_startPcVec_dup_0_0_addr"] = 0x80010000 >> 1
    checker.on_cycle(5, env)
    assert checker.model.predict(0x80000000)
    assert checker.model.predict(0x80010000)

    dut.signals[_MBTB + "io_stageCtrl_t0_fire"] = 0
    dut.signals[_MBTB + "io_stageCtrl_s2_fire"] = 1
    checker.on_cycle(6, env)
    assert checker.checked_count == 3


def test_mbtb_checker_warns_without_failing_by_default(caplog):
    checker = MbtbReferenceChecker()
    env = SimpleNamespace(
        backend_model=SimpleNamespace(current_golden_cursor=lambda: 7),
    )
    caplog.set_level(logging.WARNING, logger="env.checkers")

    checker._check_equal(
        env, 4, 0x80000000, 0x80000002,
        (0x80000100, 2, 0), ((0x80000200, 2, 0),), "metadata",
    )

    assert checker.mismatch_count == 1
    assert "mbtb_reference metadata mismatch" in caplog.text


def test_mbtb_checker_checks_conditional_counter_write():
    dut = _Dut()
    env = SimpleNamespace(dut=dut, clock_reset=SimpleNamespace(reset=SimpleNamespace(value=0)))
    checker = MbtbReferenceChecker(strict=True)
    dut.signals[_MBTB + "io_stageCtrl_t0_fire"] = 0
    align = _MBTB + "alignBanks_0."
    write = align + "internalBanks_0."
    dut.signals.update({
        align + "io_write_req_valid": 1,
        align + "io_write_req_bits_startPc_addr": 0x80000000 >> 1,
        align + "io_write_req_bits_branches_0_valid": 1,
        align + "io_write_req_bits_branches_0_bits_attribute_branchType": 1,
        align + "io_write_req_bits_branches_0_bits_cfiPosition": 2,
        align + "io_write_req_bits_branches_0_bits_taken": 0,
        align + "io_write_req_bits_meta_0_rawHit": 1,
        align + "io_write_req_bits_meta_0_position": 2,
        align + "io_write_req_bits_meta_0_counter_value": 2,
        write + "io_writeCounter_req_valid": 1,
        write + "io_writeCounter_req_bits_wayMask": 1,
        write + "io_writeCounter_req_bits_counters_0_value": 1,
        write + "io_writeEntry_req_valid": 0,
    })
    for slot in range(1, 8):
        dut.signals[align + f"io_write_req_bits_branches_{slot}_valid"] = 0
    for way in range(1, 4):
        dut.signals[align + f"io_write_req_bits_meta_{way}_rawHit"] = 0
    for bank in range(1, 4):
        dut.signals[align + f"internalBanks_{bank}.io_writeCounter_req_valid"] = 0
    checker.on_cycle(1, env)
    dut.signals[write + "io_writeCounter_req_bits_counters_0_value"] = 2
    with pytest.raises(AssertionError, match="mbtb_reference counter mismatch"):
        checker.on_cycle(2, env)
