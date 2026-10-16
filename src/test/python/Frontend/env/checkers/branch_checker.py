from __future__ import annotations

from typing import Optional


class BranchChecker:
    def __init__(self) -> None:
        self.total_instructions = 0
        self.total_branches = 0
        self.mispredict = 0
        self.by_type = {
            "branch": 0,
            "jump": 0,
            "jump_indirect": 0,
        }

    @staticmethod
    def _kind(instr: int, is_rvc: bool) -> Optional[str]:
        if is_rvc:
            return None
        opc = instr & 0x7F
        if opc == 0x63:
            return "branch"
        if opc == 0x6F:
            return "jump"
        if opc == 0x67:
            return "jump_indirect"
        return None

    def observe(self, obs) -> None:
        self.total_instructions += 1
        kind = self._kind(obs.instr, obs.is_rvc)
        if kind is None:
            return
        self.total_branches += 1
        self.by_type[kind] += 1

    def record_mispredict(self, count: int = 1) -> None:
        self.mispredict += int(count)

    def get_stats(self) -> dict:
        mpki = (self.mispredict * 1000.0) / max(1, self.total_instructions)
        return {
            "total_instruction": self.total_instructions,
            "total_branch": self.total_branches,
            "mispredict": self.mispredict,
            "mpki": mpki,
            "by_type": dict(self.by_type),
        }


__all__ = ["BranchChecker"]
