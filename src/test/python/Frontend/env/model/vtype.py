from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, Optional


@dataclass(frozen=True)
class VTypeState:
    """The five VType fields exposed by IBuffer/cfVec."""

    illegal: bool = True
    vma: bool = False
    vta: bool = False
    vsew: int = 0
    vlmul: int = 0

    @classmethod
    def initial(cls) -> "VTypeState":
        return cls()


@dataclass(frozen=True)
class VTypeTransition:
    instruction: int
    kind: str
    specvtype: VTypeState
    vtype: VTypeState


class VTypeModel:
    """Small reference model for IBuffer's immediate VType propagation.

    The model follows the same lane ordering as ``VTypeGen``: ``specvtype``
    is the state before a lane, while ``vtype`` is the state after that lane.
    ``vsetvl`` recovery inputs are intentionally outside this model; they are
    driven by backend state rather than by the instruction bits in cfVec.
    """

    _OPCODE = 0x57
    _FUNCT3 = 0x7

    # RVV encodings used by VSetFuncUnit.  The value is the encoded LMUL.
    _LMUL_NUMERATOR = {
        0b000: (1, 1),
        0b001: (2, 1),
        0b010: (4, 1),
        0b011: (8, 1),
        0b101: (1, 8),
        0b110: (1, 4),
        0b111: (1, 2),
    }
    _SEW = {0b000: 8, 0b001: 16, 0b010: 32, 0b011: 64}

    def __init__(self, *, elen: int = 64, vlen: int = 128) -> None:
        if int(elen) <= 0 or int(vlen) <= 0:
            raise ValueError("elen and vlen must be positive")
        self.elen = int(elen)
        self.vlen = int(vlen)
        self.state = VTypeState.initial()

    def reset(self) -> None:
        self.state = VTypeState.initial()

    @classmethod
    def _instruction_kind(cls, instruction: int) -> Optional[str]:
        instruction = int(instruction) & 0xFFFFFFFF
        if (instruction & 0x7F) != cls._OPCODE or ((instruction >> 12) & 0x7) != cls._FUNCT3:
            return None
        if ((instruction >> 31) & 0x1) == 0:
            return "vsetvli"
        if ((instruction >> 30) & 0x3) == 0x3:
            return "vsetivli"
        return None

    def _vlmax(self, state: VTypeState) -> Optional[int]:
        sew = self._SEW.get(int(state.vsew))
        lmul = self._LMUL_NUMERATOR.get(int(state.vlmul))
        if sew is None or lmul is None:
            return None
        return self.vlen * lmul[0] // (sew * lmul[1])

    def _decode_vset(self, instruction: int, kind: str) -> VTypeState:
        instruction = int(instruction) & 0xFFFFFFFF
        zimm = (instruction >> 20) & (0x3FF if kind == "vsetivli" else 0x7FF)
        vlmul = zimm & 0x7
        vsew = (zimm >> 3) & 0x7
        vta = bool((zimm >> 6) & 0x1)
        vma = bool((zimm >> 7) & 0x1)

        reserved = zimm >> 8
        sew = self._SEW.get(vsew)
        lmul = self._LMUL_NUMERATOR.get(vlmul)
        illegal = reserved != 0 or sew is None or lmul is None
        if not illegal:
            # This is the same ELEN legality rule used by VSetFuncUnit:
            # fractional LMULs reduce the maximum supported SEW.
            illegal = sew * lmul[1] > self.elen * min(lmul[0], lmul[1])

        candidate = VTypeState(
            illegal=bool(illegal),
            vma=vma if not illegal else False,
            vta=vta if not illegal else False,
            vsew=vsew if not illegal else 0,
            vlmul=vlmul if not illegal else 0,
        )

        # VTypeGen patches the rd=x0, rs1/uimm=0 form when changing VLMax
        # would invalidate the retained VL.  Keep this check aligned with
        # that patch even though the checker does not model VL itself.
        rd_is_zero = ((instruction >> 7) & 0x1F) == 0
        rs1_or_uimm_is_zero = ((instruction >> 15) & 0x1F) == 0
        if rd_is_zero and rs1_or_uimm_is_zero and self._vlmax(self.state) != self._vlmax(candidate):
            candidate = VTypeState(True, candidate.vma, candidate.vta, candidate.vsew, candidate.vlmul)
        return candidate

    def step(self, instruction: int) -> VTypeTransition:
        instruction = int(instruction) & 0xFFFFFFFF
        before = self.state
        kind = self._instruction_kind(instruction)
        after = self._decode_vset(instruction, kind) if kind is not None else before
        self.state = after
        return VTypeTransition(
            instruction=instruction,
            kind=kind or "ordinary",
            specvtype=before,
            vtype=after,
        )

    def step_lanes(self, instructions: Iterable[Optional[int]]) -> list[VTypeTransition]:
        return [self.step(instruction) for instruction in instructions if instruction is not None]


__all__ = ["VTypeModel", "VTypeState", "VTypeTransition"]
