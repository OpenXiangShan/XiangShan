"""Compare FallThrough S1 output with the design-document model."""

import logging

from rm.fallthrough_reference_model import FallThroughReferenceModel
from ..support.signal_utils import require_sig


_BPU = "Frontend_top.Frontend.inner_bpu."
_FALLTHROUGH = _BPU + "fallThrough."


class FallThroughReferenceChecker:
    def __init__(self, *, strict: bool = False) -> None:
        self.checked_count = 0
        self.strict = bool(strict)
        self.mismatch_count = 0

    def _check_equal(self, cycle, start_pc, actual, expected) -> None:
        if actual == expected:
            return
        self.mismatch_count += 1
        message = (
            f"fallthrough_reference prediction mismatch cycle={int(cycle)} "
            f"start_pc=0x{start_pc:x} actual={actual} expected={expected}"
        )
        logger = logging.getLogger("env.checkers")
        if self.strict:
            logger.error(message)
            raise AssertionError(message)
        if self.mismatch_count <= 10:
            logger.warning(message)
        elif self.mismatch_count == 11:
            logger.warning("fallthrough_reference further mismatches suppressed; see final count")

    def on_cycle(self, cycle: int, env) -> None:
        if int(env.clock_reset.reset.value):
            return
        dut = env.dut
        if not require_sig(dut, _BPU + "s1_valid"):
            return
        start_pc = require_sig(dut, _BPU + "s1_startPc_dup_0_addr") << 1
        expected = FallThroughReferenceModel.predict(start_pc)
        actual = (
            require_sig(dut, _FALLTHROUGH + "io_prediction_target_addr") << 1,
            require_sig(dut, _FALLTHROUGH + "io_prediction_cfiPosition"),
        )
        wanted = (expected.target, expected.cfi_position)
        self._check_equal(cycle, start_pc, actual, wanted)
        self.checked_count += 1
