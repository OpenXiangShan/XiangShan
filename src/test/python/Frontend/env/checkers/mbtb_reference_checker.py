"""Compare known mBTB hits using a transaction-level reference model."""

import logging

from rm.mbtb_reference_model import MbtbReferenceModel
from ..support.signal_utils import require_sig


_BPU = "Frontend_top.Frontend.inner_bpu."
_MBTB = _BPU + "mbtb."


class MbtbReferenceChecker:
    """Feed training transactions to the mBTB functional model."""

    def __init__(self, *, strict: bool = False) -> None:
        self.model = MbtbReferenceModel()
        self.checked_count = 0
        self.strict = bool(strict)
        self.mismatch_count = 0

    def _check_equal(self, env, cycle, start_pc, cfi_pc, actual, expected, field) -> None:
        if actual == expected:
            return
        self.mismatch_count += 1
        cursor = getattr(getattr(env, "backend_model", None), "current_golden_cursor", lambda: None)()
        message = (
            f"mbtb_reference {field} mismatch cycle={int(cycle)} cursor={cursor} "
            f"start_pc=0x{start_pc:x} cfi_pc=0x{cfi_pc:x} "
            f"actual={actual} expected={expected}"
        )
        logger = logging.getLogger("env.checkers")
        if self.strict:
            logger.error(message)
            raise AssertionError(message)
        if self.mismatch_count <= 10:
            logger.warning(message)
        elif self.mismatch_count == 11:
            logger.warning("mbtb_reference further mismatches suppressed; see final count")

    def on_cycle(self, cycle: int, env) -> None:
        if int(env.clock_reset.reset.value):
            self.model.reset()
            return
        dut = env.dut
        if not require_sig(dut, _MBTB + "io_enable"):
            return

        if require_sig(dut, _MBTB + "io_stageCtrl_s2_fire"):
            start_pc = require_sig(dut, _BPU + "s2_startPc_dup_0_addr") << 1
            expected = {
                prediction.cfi_position: prediction
                for prediction in self.model.predict(start_pc)
            }
            for slot in range(10):
                prefix = _MBTB + f"io_result_{slot}_"
                if not require_sig(dut, prefix + "valid"):
                    continue
                position = require_sig(dut, prefix + "bits_cfiPosition")
                prediction = expected.get(position)
                if prediction is None:
                    continue
                actual = (
                    require_sig(dut, prefix + "bits_target_addr") << 1,
                    require_sig(dut, prefix + "bits_attribute_branchType"),
                    require_sig(dut, prefix + "bits_attribute_rasAction"),
                )
                allowed = self.model.allowed_metadata(prediction.cfi_pc, start_pc)
                if actual not in allowed:
                    self._check_equal(
                        env, cycle, start_pc, prediction.cfi_pc,
                        actual, allowed, "metadata",
                    )
                self.model.observe_metadata(prediction.cfi_pc, start_pc, actual)
                self.checked_count += 1

        self._check_counter_updates(cycle, env)

        if require_sig(dut, _MBTB + "io_stageCtrl_s3_fire") and require_sig(
            dut, _BPU + "s3_taken"
        ):
            start_pc = require_sig(dut, _BPU + "s3_startPc_dup_0_addr") << 1
            position = require_sig(dut, _BPU + "s3_prediction_cfiPosition")
            self.model.touch((start_pc & ~0x1F) + (position << 1))

        if not require_sig(dut, _MBTB + "io_stageCtrl_t0_fire"):
            return
        start_pc = require_sig(dut, _MBTB + "io_train_startPcVec_dup_0_0_addr") << 1
        aligned_start = start_pc & ~0x1F
        for slot in range(8):
            prefix = _MBTB + f"io_train_branches_{slot}_"
            if not require_sig(dut, prefix + "valid"):
                continue
            branch_type = require_sig(dut, prefix + "bits_attribute_branchType")
            if branch_type not in (1, 2, 3):
                continue
            cfi_pc = aligned_start + (
                require_sig(dut, prefix + "bits_cfiPosition") << 1
            )
            self.model.train(
                cfi_pc,
                target=require_sig(dut, prefix + "bits_target_addr") << 1,
                branch_type=branch_type,
                ras_action=require_sig(dut, prefix + "bits_attribute_rasAction"),
                taken=bool(require_sig(dut, prefix + "bits_taken")),
                start_pc=start_pc,
            )

    def _check_counter_updates(self, cycle: int, env) -> None:
        dut = env.dut
        for align in range(1):
            prefix = _MBTB + f"alignBanks_{align}."
            if not require_sig(dut, prefix + "io_write_req_valid"):
                continue
            branches = []
            for slot in range(8):
                branch = prefix + f"io_write_req_bits_branches_{slot}_"
                if require_sig(dut, branch + "valid") and require_sig(
                    dut, branch + "bits_attribute_branchType"
                ) == 1:
                    branches.append((
                        require_sig(dut, branch + "bits_cfiPosition"),
                        bool(require_sig(dut, branch + "bits_taken")),
                    ))
            if not branches:
                continue
            for way in range(4):
                meta = prefix + f"io_write_req_bits_meta_{way}_"
                if not require_sig(dut, meta + "rawHit"):
                    continue
                position = require_sig(dut, meta + "position")
                outcomes = [taken for branch_pos, taken in branches if branch_pos == position]
                if not outcomes:
                    continue
                expected = self.model.update_counter(
                    require_sig(dut, meta + "counter_value"), any(outcomes)
                )
                for bank in range(1):
                    write = prefix + f"internalBanks_{bank}."
                    if not require_sig(dut, write + "io_writeCounter_req_valid"):
                        continue
                    if not (require_sig(dut, write + "io_writeCounter_req_bits_wayMask") & (1 << way)):
                        continue
                    if require_sig(dut, write + "io_writeEntry_req_valid") and (
                        require_sig(dut, write + "io_writeEntry_req_bits_wayMask") & (1 << way)
                    ):
                        continue
                    actual = require_sig(
                        dut, write + f"io_writeCounter_req_bits_counters_{way}_value"
                    )
                    start_pc = require_sig(dut, prefix + "io_write_req_bits_startPc_addr") << 1
                    cfi_pc = (start_pc & ~0x1F) + (position << 1)
                    self._check_equal(env, cycle, start_pc, cfi_pc, actual, expected, "counter")
