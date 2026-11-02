"""Small uBTB model from XiangShan-Design-Doc BPU/ubtb.md and index.md.

Design document snapshot: 92428b64be5a7e016621e74203bcce104d36635c.

The model covers known entries without replacement or concurrent training.
It deliberately has no RTL pipeline, victim selector, or internal signal inputs.
"""

from dataclasses import dataclass


# The documented 22 stored address bits are in 2-byte units. Model PCs are bytes.
_TAG_BITS = 22
_TAG_MASK = (1 << _TAG_BITS) - 1
_TARGET_BITS = 23
_TARGET_MASK = (1 << _TARGET_BITS) - 1
_MAX_USEFUL = 3


@dataclass(frozen=True)
class UbtbPrediction:
    position: int
    target: int
    branch_type: int
    ras_action: int


@dataclass
class _Entry:
    prediction: UbtbPrediction
    useful: int


class UbtbReferenceModel:
    """Documented always-taken behavior, keyed by the stored 22-bit tag."""

    def __init__(self) -> None:
        self.entries: dict[int, _Entry] = {}

    def reset(self) -> None:
        self.entries.clear()

    @staticmethod
    def tag(start_pc: int) -> int:
        return (int(start_pc) >> 1) & _TAG_MASK

    def forget(self, start_pc: int) -> None:
        self.entries.pop(self.tag(start_pc), None)

    def predict(self, start_pc: int) -> UbtbPrediction | None:
        entry = self.entries.get(self.tag(start_pc))
        if entry is None or entry.useful == 0:
            return None
        pred = entry.prediction
        return UbtbPrediction(
            pred.position,
            (int(start_pc) & ~_TARGET_MASK) | (pred.target & _TARGET_MASK),
            pred.branch_type,
            pred.ras_action,
        )

    def train(
        self, start_pc: int, *, taken: bool, position: int,
        target: int, branch_type: int, ras_action: int,
    ) -> None:
        """Apply one settled training event; callers exclude replacement races."""
        tag = self.tag(start_pc)
        learned = UbtbPrediction(
            int(position), int(target) & _TARGET_MASK,
            int(branch_type), int(ras_action),
        )
        entry = self.entries.get(tag)
        if entry is None or entry.useful == 0:
            if taken:
                if entry is None and len(self.entries) >= 32:
                    raise ValueError("uBTB replacement is outside this model's scope")
                self.entries[tag] = _Entry(learned, _MAX_USEFUL)
            return
        if taken and entry.prediction == learned:
            entry.useful = min(_MAX_USEFUL, entry.useful + 1)
        else:
            entry.useful -= 1
