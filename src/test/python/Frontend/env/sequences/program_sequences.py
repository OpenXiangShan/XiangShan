from __future__ import annotations

from dataclasses import dataclass

from ..transactions import ProgramImage


@dataclass(frozen=True)
class LoadProgramSequence:
    image: ProgramImage
    step_cycles: int = 0

    def run(self, env) -> int:
        env.load_program(self.image.payload, self.image.base_addr)
        if int(self.step_cycles) > 0:
            env.step(int(self.step_cycles))
        return len(self.image.payload)


@dataclass(frozen=True)
class LoadProgramFileSequence:
    path: str
    base_addr: int
    step_cycles: int = 0

    def run(self, env) -> int:
        size = int(env.load_program_file(str(self.path), int(self.base_addr)))
        if int(self.step_cycles) > 0:
            env.step(int(self.step_cycles))
        return size
