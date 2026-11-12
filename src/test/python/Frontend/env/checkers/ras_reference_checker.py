"""Compare RAS return targets using accepted prediction transactions."""

import logging

from rm.ras_reference_model import RasReferenceModel, decode_ras_action
from ..support.signal_utils import require_sig


_BPU = "Frontend_top.Frontend.inner_bpu."
_FTQ = "Frontend_top.Frontend.inner_ftq."
_RAS = _BPU + "ras."
_TOP = _RAS + "RasStack.timingTop_retAddr_addr"
_NEAR_OVERFLOW = _RAS + "RasStack.specNearOverflowed"


class RasReferenceChecker:
    def __init__(self, *, strict: bool = False) -> None:
        self.model = RasReferenceModel()
        self.events: list[tuple[int, int]] = []
        self.checked_count = 0
        self.mismatch_count = 0
        self.unknown_count = 0
        self.prediction_count = 0
        self.call_count = 0
        self.return_count = 0
        self.commit_count = 0
        self.redirect_count = 0
        self.strict = bool(strict)
        self._compare_after = 0

    def _unknown(self) -> None:
        self.model.invalidate()
        self.events.clear()
        self.unknown_count += 1

    @staticmethod
    def _cfi_location(env, start_pc: int, position: int, action: int) -> tuple[int, int] | None:
        end_pc = (start_pc & ~0x1F) + (position << 1)
        if action not in (2, 3):
            return end_pc, 2
        memory = env.memory.mem
        pc = start_pc
        while pc <= end_pc:
            if pc not in memory or pc + 1 not in memory:
                return None
            halfword = env.memory.read_u16(pc)
            width = 2 if halfword & 3 != 3 else 4
            if width == 4 and (pc + 2 not in memory or pc + 3 not in memory):
                return None
            if pc + width - 2 == end_pc:
                instruction = halfword if width == 2 else env.memory.read_u32(pc)
                decoded_action, _ = decode_ras_action(instruction)
                return (pc, width) if decoded_action == action else None
            pc += width
        return None

    def _check(self, cycle: int, env, pc: int, expected: int) -> None:
        actual = require_sig(env.dut, _TOP) << 1
        self.checked_count += 1
        if actual == expected:
            return
        self.mismatch_count += 1
        cursor = env.backend_model.current_golden_cursor()
        message = (
            f"ras_reference return mismatch cycle={cycle} cursor={cursor} "
            f"cfi_pc=0x{pc:x} actual=0x{actual:x} expected=0x{expected:x}"
        )
        logger = logging.getLogger("env.checkers")
        if self.strict:
            logger.error(message)
            raise AssertionError(message)
        if self.mismatch_count <= 10:
            logger.warning(message)
        elif self.mismatch_count == 11:
            logger.warning("ras_reference further mismatches suppressed; see final count")

    def on_cycle(self, cycle: int, env) -> None:
        if int(env.clock_reset.reset.value):
            self.model.reset()
            self.events.clear()
            self._compare_after = 0
            return
        dut = env.dut
        if not require_sig(dut, _RAS + "io_enable"):
            self._unknown()
            return
        if require_sig(dut, _NEAR_OVERFLOW):
            self._unknown()
            return

        redirect = require_sig(dut, _FTQ + "redirect_1_valid")
        if require_sig(dut, _BPU + "s3_valid"):
            self.prediction_count += 1
            event = (
                require_sig(dut, _BPU + "s3_ftqPtr_flag"),
                require_sig(dut, _BPU + "s3_ftqPtr_value"),
            )
            action = require_sig(dut, _BPU + "s3_prediction_attribute_rasAction")
            self.call_count += action in (2, 3)
            self.return_count += action in (1, 3)
            start_pc = require_sig(dut, _BPU + "s3_startPc_dup_2_addr") << 1
            position = require_sig(dut, _BPU + "s3_prediction_cfiPosition")
            location = self._cfi_location(env, start_pc, position, action)
            if location is None:
                self._unknown()
            else:
                pc, width = location
                if event in self.events:
                    expected = self.model.redirect(event, action, pc, width)
                    del self.events[self.events.index(event):]
                else:
                    expected = self.model.predict(event, action, pc, width)
                self.events.append(event)
                if action in (1, 3) and expected is not None and not redirect and cycle >= self._compare_after:
                    self._check(cycle, env, pc, expected)

        if require_sig(dut, _RAS + "io_commit_valid"):
            self.commit_count += 1
            event = (
                require_sig(dut, _FTQ + "commitPtr_ptrs_0_flag"),
                require_sig(dut, _FTQ + "commitPtr_ptrs_0_value"),
            )
            if event in self.events:
                index = self.events.index(event)
                self.model.commit(self.events[index])
                del self.events[:index + 1]

        if require_sig(dut, "io_backend_toFtq_commit_valid"):
            event = (
                require_sig(dut, "io_backend_toFtq_commit_bits_flag"),
                require_sig(dut, "io_backend_toFtq_commit_bits_value"),
            )
            if event in self.events:
                index = self.events.index(event)
                self.model.commit(event)
                del self.events[:index + 1]

        if redirect:
            self.redirect_count += 1
            event = (
                require_sig(dut, _FTQ + "redirect_1_bits_ftqIdx_flag"),
                require_sig(dut, _FTQ + "redirect_1_bits_ftqIdx_value"),
            )
            action = require_sig(dut, _BPU + "io_fromFtq_redirect_bits_attribute_rasAction")
            if event not in self.events or action in (2, 3):
                self._unknown()
            else:
                self.model.redirect(event, action, 0, 2)
                del self.events[self.events.index(event) + 1:]
            self._compare_after = cycle + 2
