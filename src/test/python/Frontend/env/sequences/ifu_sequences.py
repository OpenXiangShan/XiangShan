from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, Tuple

from ..transactions import ProgramImage


_NOP = 0x00000013


def _instructions_to_bytes(instructions: Iterable[int]) -> bytes:
    buf = bytearray()
    for instr in instructions:
        buf.extend((int(instr) & 0xFFFFFFFF).to_bytes(4, "little"))
    return bytes(buf)


@dataclass(frozen=True)
class BaremodeSequentialIFUScenario:
    """UVM-like sequence/scenario for the first bare-mode IFU pilot.

    Corresponds to the low-risk subset of kmh_ut/IFU tc_non_mmio_basic: drive a
    deterministic non-MMIO NOP stream and define the expected sequential fetch PCs.
    Existing Python agents still perform the actual memory/cache/backend driving.
    """

    base_addr: int = 0x80000000
    words: int = 128
    expected_fetches: int = 4
    instr: int = _NOP

    def program_image(self) -> ProgramImage:
        return ProgramImage(
            payload=_instructions_to_bytes([int(self.instr)] * int(self.words)),
            base_addr=int(self.base_addr),
        )

    def expected_pcs(self) -> Tuple[int, ...]:
        return tuple(int(self.base_addr) + 4 * i for i in range(max(0, int(self.expected_fetches))))
