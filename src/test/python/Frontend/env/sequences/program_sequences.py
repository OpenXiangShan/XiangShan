from __future__ import annotations

from dataclasses import dataclass

from ..transactions import ProgramImage


@dataclass(frozen=True)
class LoadProgramSequence:
    image: ProgramImage

    def run(self, env) -> int:
        env.load_program(self.image.payload, self.image.base_addr)
        return len(self.image.payload)


@dataclass(frozen=True)
class LoadProgramFileSequence:
    path: str
    base_addr: int

    def run(self, env) -> int:
        return int(env.load_program_file(str(self.path), int(self.base_addr)))
