"""Schedule legal backend training and observe actual BPU stage-3 windows."""

from __future__ import annotations

from collections import deque


class BpuFtqScheduler:
    def __init__(self, env) -> None:
        self.env = env
        self.history = deque(maxlen=32)

    def wait_live_identity(self, *, pc: int | None = None, cfi_only: bool = False,
                           max_cycles: int = 1000) -> dict:
        for elapsed in range(int(max_cycles) + 1):
            for identity in reversed(self.env.backend_model.live_ftq_identities()):
                if (pc is None or identity["inst_pc"] == int(pc)) and (
                    not cfi_only or bool(identity.get("is_cfi"))
                ):
                    return identity
            if elapsed < int(max_cycles):
                self.env.step(1)
        raise AssertionError({"reason": "no live FTQ identity", "pc": pc, "max_cycles": max_cycles})

    def queue_mispredict(self, identity: dict, *, target: int, branch_type: int = 3):
        return self.env.backend_model.queue_directed_resolve(identity, target=target, branch_type=branch_type)

    def train_when(self, predicate, *, target: int, max_cycles: int = 1000, pc: int | None = None):
        """Capture identity only after the requested window, avoiding saved stale pointers."""
        for elapsed in range(int(max_cycles) + 1):
            if predicate(self.env):
                for identity in reversed(self.env.backend_model.live_ftq_identities()):
                    if pc is None or identity["inst_pc"] == int(pc):
                        return self.queue_mispredict(identity, target=target)
            if elapsed < int(max_cycles):
                self.env.step(1)
        raise AssertionError("training window and live FTQ identity never coincided")

    def wait_for_bpu_flush(self, sample, predicate, *, max_cycles: int = 1000) -> dict:
        """Require real sampled flush valid AND caller's exact live-pointer relation.

        A resolve only trains the predictor. It does not directly select the
        BPU s3 pointer; the caller must revisit the trained PC and observe it.
        """
        for elapsed in range(int(max_cycles) + 1):
            current = sample(self.env)
            self.history.append(current)
            if current.get("bpu_valid") == 1 and predicate(current):
                return current
            if elapsed < int(max_cycles):
                self.env.step(1)
        raise AssertionError({"reason": "BPU flush did not align with live stage", "tail": list(self.history)})

    def pulse_predictor_transition(self) -> None:
        """Existing fence.i/enable transition stimulus; this does not guarantee s3 flush."""
        self.env.set_bp_ctrl_enable(ubtb_enable=0, abtb_enable=0, mbtb_enable=0,
                                    tage_enable=0, sc_enable=0, ittage_enable=0)
        pin = getattr(self.env.dut, "io_fencei", None)
        if pin is None:
            raise AssertionError("io_fencei is unavailable")
        prior = int(pin.value)
        try:
            pin.value = 1
            self.env.step(1)
        finally:
            pin.value = prior
