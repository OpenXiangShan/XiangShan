"""Check settled uBTB hits against the design-document reference model."""

from rm.ubtb_reference_model import UbtbReferenceModel
from ..support.signal_utils import require_sig


_BPU = "Frontend_top.Frontend.inner_bpu."
_UBTB = _BPU + "ubtb."


class UbtbReferenceChecker:
    """Track observed writes; leave victim selection outside the model's scope."""

    def __init__(self) -> None:
        self.model = UbtbReferenceModel()
        self.slots: dict[int, int] = {}
        self.last_write: dict[int, int] = {}
        self.checked_count = 0

    def on_cycle(self, cycle: int, env) -> None:
        if int(env.clock_reset.reset.value):
            self.model.reset()
            self.slots.clear()
            self.last_write.clear()
            return

        dut = env.dut
        if require_sig(dut, _UBTB + "t1_fire_REG"):
            index = require_sig(dut, _UBTB + "t1_updateIdx")
            tag = require_sig(dut, _UBTB + "t1_tag")
            pc = tag << 1
            if require_sig(dut, _UBTB + "t1_allocate"):
                victim = self.slots.pop(index, None)
                if victim is not None:
                    self.model.forget(victim << 1)
                    self.last_write.pop(victim, None)
                for old_index, old_tag in list(self.slots.items()):
                    if old_tag == tag:
                        del self.slots[old_index]
                self.model.forget(pc)
                if require_sig(dut, _UBTB + "t1_actualTaken"):
                    self.model.train(
                        pc, taken=True,
                        position=require_sig(dut, _UBTB + "t1_position"),
                        target=require_sig(dut, _UBTB + "t1_target") << 1,
                        branch_type=require_sig(dut, _UBTB + "t1_attribute_branchType"),
                        ras_action=require_sig(dut, _UBTB + "t1_attribute_rasAction"),
                    )
                    self.slots[index] = tag
            elif self.slots.get(index) == tag:
                self.model.train(
                    pc, taken=bool(require_sig(dut, _UBTB + "t1_actualTaken")),
                    position=require_sig(dut, _UBTB + "t1_position"),
                    target=require_sig(dut, _UBTB + "t1_target") << 1,
                    branch_type=require_sig(dut, _UBTB + "t1_attribute_branchType"),
                    ras_action=require_sig(dut, _UBTB + "t1_attribute_rasAction"),
                )
            else:
                # A write we cannot associate with a documented allocation
                # makes the old slot and this tag uncertain.
                old_tag = self.slots.pop(index, None)
                if old_tag is not None:
                    self.model.forget(old_tag << 1)
                    self.last_write.pop(old_tag, None)
                self.model.forget(pc)
            self.last_write[tag] = int(cycle)

        if not int(env.csr_ctrl_if.io_csrCtrl_bp_ctrl_ubtbEnable.value):
            return
        if not require_sig(dut, _BPU + "s1_valid"):
            return
        pc = require_sig(dut, _UBTB + "s1_startPc_addr") << 1
        tag = self.model.tag(pc)
        if int(cycle) < self.last_write.get(tag, -100) + 4:
            return
        expected = self.model.predict(pc)
        if expected is None:
            return
        actual = (
            require_sig(dut, _UBTB + "io_prediction_bits_taken"),
            require_sig(dut, _UBTB + "io_prediction_bits_cfiPosition"),
            require_sig(dut, _UBTB + "io_prediction_bits_target_addr") << 1,
            require_sig(dut, _UBTB + "io_prediction_bits_attribute_branchType"),
            require_sig(dut, _UBTB + "io_prediction_bits_attribute_rasAction"),
        )
        wanted = (1, expected.position, expected.target,
                  expected.branch_type, expected.ras_action)
        assert actual == wanted, {
            "checker": "ubtb_reference", "cycle": int(cycle),
            "start_pc": hex(pc), "actual": actual, "expected": wanted,
        }
        self.checked_count += 1
