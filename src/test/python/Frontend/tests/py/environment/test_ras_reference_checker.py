from types import SimpleNamespace

import pytest

from env.checkers.ras_reference_checker import RasReferenceChecker
from env.model.memory_model import MemoryModel


_BPU = "Frontend_top.Frontend.inner_bpu."
_FTQ = "Frontend_top.Frontend.inner_ftq."
_RAS = _BPU + "ras."


class _Pin:
    def __init__(self, value):
        self.value = value


class _Dut:
    def __init__(self):
        self.signals = {}

    def GetInternalSignal(self, name):
        return _Pin(self.signals[name])


def _env():
    dut = _Dut()
    dut.signals.update({
        _RAS + "io_enable": 1,
        _RAS + "RasStack.specNearOverflowed": 0,
        _RAS + "io_commit_valid": 0,
        _FTQ + "redirect_1_valid": 0,
        _BPU + "s3_valid": 0,
        "io_backend_toFtq_commit_valid": 0,
    })
    memory = MemoryModel()
    memory.write_u32(0x100, 0x000000EF)
    memory.write_u32(0x200, 0x00008067)
    memory.write_u32(0x300, 0x000000EF)
    memory.write_u16(0x120, 0x9082)
    memory.write_u16(0x220, 0x8082)
    return SimpleNamespace(
        dut=dut, memory=memory,
        clock_reset=SimpleNamespace(reset=_Pin(0)),
        backend_model=SimpleNamespace(current_golden_cursor=lambda: 3),
    )


def _s3(env, flag, value, pc, action, top=0, width=4):
    env.dut.signals.update({
        _BPU + "s3_valid": 1,
        _BPU + "s3_ftqPtr_flag": flag,
        _BPU + "s3_ftqPtr_value": value,
        _BPU + "s3_prediction_attribute_rasAction": action,
        _BPU + "s3_startPc_dup_2_addr": pc >> 1,
        _BPU + "s3_prediction_cfiPosition": ((pc + width - 2) & 0x1F) >> 1,
        _RAS + "RasStack.timingTop_retAddr_addr": top >> 1,
    })


def test_ras_checker_compares_return_after_distinct_prediction_events():
    env = _env()
    checker = RasReferenceChecker()
    _s3(env, 0, 1, 0x100, 2)
    checker.on_cycle(1, env)
    _s3(env, 0, 2, 0x200, 1, top=0x104)
    checker.on_cycle(2, env)
    assert checker.checked_count == 1
    assert checker.mismatch_count == 0
    assert checker.model.top() is None


def test_ras_checker_redirect_restores_target_checkpoint():
    env = _env()
    checker = RasReferenceChecker()
    _s3(env, 0, 1, 0x100, 2)
    checker.on_cycle(1, env)
    _s3(env, 0, 2, 0x300, 2)
    checker.on_cycle(2, env)
    env.dut.signals.update({
        _BPU + "s3_valid": 0,
        _FTQ + "redirect_1_valid": 1,
        _FTQ + "redirect_1_bits_ftqIdx_flag": 0,
        _FTQ + "redirect_1_bits_ftqIdx_value": 1,
        _BPU + "io_fromFtq_redirect_bits_attribute_rasAction": 0,
    })
    checker.on_cycle(3, env)
    assert checker.events == [(0, 1)]
    assert checker.model.top() is None


def test_ras_checker_warning_and_strict_mode(caplog):
    env = _env()
    checker = RasReferenceChecker()
    _s3(env, 0, 1, 0x100, 2)
    checker.on_cycle(1, env)
    _s3(env, 0, 2, 0x200, 1, top=0x100)
    checker.on_cycle(2, env)
    assert checker.mismatch_count == 1
    assert "ras_reference return mismatch" in caplog.text

    strict = RasReferenceChecker(strict=True)
    _s3(env, 0, 1, 0x100, 2)
    strict.on_cycle(1, env)
    _s3(env, 0, 2, 0x200, 1, top=0x100)
    with pytest.raises(AssertionError, match="ras_reference return mismatch"):
        strict.on_cycle(2, env)


def test_ras_checker_decodes_compressed_call_return_address():
    env = _env()
    checker = RasReferenceChecker()
    _s3(env, 0, 1, 0x120, 2, width=2)
    checker.on_cycle(1, env)
    _s3(env, 0, 2, 0x220, 1, top=0x122, width=2)
    checker.on_cycle(2, env)
    assert checker.checked_count == 1
    assert checker.mismatch_count == 0


def test_ras_checker_reused_ftq_id_restores_before_comparison():
    env = _env()
    checker = RasReferenceChecker()
    _s3(env, 0, 1, 0x100, 2)
    checker.on_cycle(1, env)
    _s3(env, 0, 2, 0x300, 2)
    checker.on_cycle(2, env)
    _s3(env, 0, 2, 0x200, 1, top=0x104)
    checker.on_cycle(3, env)
    assert checker.checked_count == 1
    assert checker.mismatch_count == 0


def test_ras_checker_generic_commit_retires_checkpoint_before_pointer_reuse():
    env = _env()
    checker = RasReferenceChecker()
    _s3(env, 0, 1, 0x100, 2)
    checker.on_cycle(1, env)
    env.dut.signals.update({
        _BPU + "s3_valid": 0,
        "io_backend_toFtq_commit_valid": 1,
        "io_backend_toFtq_commit_bits_flag": 0,
        "io_backend_toFtq_commit_bits_value": 1,
    })
    checker.on_cycle(2, env)
    assert checker.events == []
    assert checker.model.top() == 0x104


def test_ras_checker_commit_uses_current_ftq_pointer():
    env = _env()
    checker = RasReferenceChecker()
    _s3(env, 0, 1, 0x100, 2)
    checker.on_cycle(1, env)
    env.dut.signals.update({
        _BPU + "s3_valid": 0,
        _RAS + "io_commit_valid": 1,
        _FTQ + "commitPtr_ptrs_0_flag": 0,
        _FTQ + "commitPtr_ptrs_0_value": 1,
    })
    checker.on_cycle(2, env)
    assert checker.events == []
    assert checker.commit_count == 1
    assert checker.model.top() == 0x104


def test_ras_checker_stops_comparing_at_near_overflow_and_recovers_on_new_call():
    env = _env()
    checker = RasReferenceChecker(strict=True)
    _s3(env, 0, 1, 0x100, 2)
    checker.on_cycle(1, env)

    env.dut.signals[_RAS + "RasStack.specNearOverflowed"] = 1
    _s3(env, 0, 2, 0x200, 1, top=0)
    checker.on_cycle(2, env)
    assert checker.checked_count == 0
    assert checker.model.top() is None
    assert checker.events == []

    env.dut.signals[_RAS + "RasStack.specNearOverflowed"] = 0
    _s3(env, 0, 3, 0x100, 2)
    checker.on_cycle(3, env)
    _s3(env, 0, 4, 0x200, 1, top=0x104)
    checker.on_cycle(4, env)
    assert checker.checked_count == 1
    assert checker.mismatch_count == 0
