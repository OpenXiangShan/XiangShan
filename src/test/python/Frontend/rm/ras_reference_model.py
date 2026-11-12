"""Transaction-level RAS semantics for call/return prediction events.

Actions follow XiangShan-Design-Doc BPU/index.md: 0 none, 1 pop,
2 push, 3 pop-and-push. A missing top means the predicted return target
cannot be determined. At the conservative known-depth limit, older stack
contents become unknown. Event IDs identify prediction occurrences, including
repeat PCs.
"""

from collections.abc import Hashable


def decode_ras_action(instruction: int) -> tuple[int, int]:
    """Return (RAS action, instruction length) from RISC-V link-register hints."""
    insn = int(instruction)
    if insn & 0x3 != 0x3:
        width = 2
        # C.JR and C.JALR: quadrant 2, funct3 100, rs2 zero, rs1 nonzero.
        if insn & 0xE003 == 0x8002 and (insn >> 2) & 0x1F == 0:
            rs1 = (insn >> 7) & 0x1F
            if rs1:
                rd = 1 if insn & 0x1000 else 0
                return _jalr_ras_action(rd, rs1), width
        return 0, width

    width = 4
    opcode = insn & 0x7F
    rd = (insn >> 7) & 0x1F
    if opcode == 0x6F:
        return (2 if rd in (1, 5) else 0), width
    if opcode == 0x67 and (insn >> 12) & 0x7 == 0:
        return _jalr_ras_action(rd, (insn >> 15) & 0x1F), width
    return 0, width


def _jalr_ras_action(rd: int, rs1: int) -> int:
    rd_link = rd in (1, 5)
    rs1_link = rs1 in (1, 5)
    if rd_link and rs1_link:
        return 2 if rd == rs1 else 3
    if rd_link:
        return 2
    return 1 if rs1_link else 0


class RasReferenceModel:
    # The current DUT has 16 committed RAS entries. This is a confidence bound,
    # not a model of its speculative overflow or replacement policy.
    MAX_KNOWN_DEPTH = 16

    def __init__(self) -> None:
        self.reset()

    def reset(self) -> None:
        self._stack: list[int | None] = []
        self._pending: list[tuple[Hashable, list[int | None]]] = []

    def invalidate(self) -> None:
        """Forget speculative history after an unmodeled recovery event."""
        self._stack = [None]
        self._pending.clear()

    def _forget_old_history(self) -> None:
        self._stack = [None]
        self._pending = [(event_id, [None]) for event_id, _ in self._pending]

    def top(self) -> int | None:
        return self._stack[-1] if self._stack else None

    def apply_instruction(self, cfi_pc: int, instruction: int) -> int | None:
        action, width = decode_ras_action(instruction)
        return self.apply(action, cfi_pc, width)

    def apply(self, action: int, cfi_pc: int, instruction_bytes: int) -> int | None:
        """Return the pre-action return target, then apply one taken CFI."""
        self._validate_action(action, instruction_bytes)

        target = self.top() if action in (1, 3) else None
        if action in (1, 3) and self._stack and self._stack[-1] is not None:
            self._stack.pop()
        if action in (2, 3):
            if len(self._stack) >= self.MAX_KNOWN_DEPTH:
                self._forget_old_history()
            else:
                self._stack.append(cfi_pc + instruction_bytes)
        return target

    @staticmethod
    def _validate_action(action: int, instruction_bytes: int) -> None:
        if action not in (0, 1, 2, 3):
            raise ValueError("invalid RAS action")
        if action in (2, 3) and instruction_bytes not in (2, 4):
            raise ValueError("call instruction must be 2 or 4 bytes")

    def predict(
        self, event_id: Hashable, action: int, cfi_pc: int, instruction_bytes: int
    ) -> int | None:
        """Accept one prediction occurrence and retain its pre-action state."""
        if any(pending_id == event_id for pending_id, _ in self._pending):
            raise ValueError("duplicate RAS prediction event")
        self._validate_action(action, instruction_bytes)
        before = self._stack.copy()
        self._pending.append((event_id, before))
        return self.apply(action, cfi_pc, instruction_bytes)

    def commit(self, event_id: Hashable) -> None:
        """Retire an event and all older checkpoints without replaying actions."""
        index = self._pending_index(event_id)
        del self._pending[:index + 1]

    def redirect(
        self, event_id: Hashable, action: int, cfi_pc: int, instruction_bytes: int
    ) -> int | None:
        """Replace an event and discard all younger speculative actions."""
        index = self._pending_index(event_id)
        self._validate_action(action, instruction_bytes)
        self._stack = self._pending[index][1].copy()
        del self._pending[index:]
        return self.predict(event_id, action, cfi_pc, instruction_bytes)

    def _pending_index(self, event_id: Hashable) -> int:
        for index, (pending_id, _) in enumerate(self._pending):
            if pending_id == event_id:
                return index
        raise ValueError("unknown RAS prediction event")
