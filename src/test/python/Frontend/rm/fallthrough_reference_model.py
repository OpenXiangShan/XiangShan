"""FallThrough prediction from XiangShan-Design-Doc BPU/fallThrough.md.

Design document snapshot: 92428b64be5a7e016621e74203bcce104d36635c.
Addresses passed to this model are byte addresses.
"""

from dataclasses import dataclass


@dataclass(frozen=True)
class FallThroughPrediction:
    target: int
    cfi_position: int


class FallThroughReferenceModel:
    """Always-not-taken prediction with half-align and page boundaries."""

    @staticmethod
    def predict(start_pc: int) -> FallThroughPrediction:
        next_pc = int(start_pc) + 64
        if (int(start_pc) >> 12) != (next_pc >> 12):
            target = next_pc & ~0xFFF
        else:
            target = next_pc & ~0x1F
        position = ((target - 2 - (int(start_pc) & ~0x1F)) >> 1) & 0x1F
        return FallThroughPrediction(target, position)
