from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class InitializeFrontendSequence:
    reset_vector: int = 0x80000000
    bare_mode: bool = True
    reset_cycles: int = 20
    step_cycles: int = 0

    def run(self, env):
        env.initialize(
            reset_vector=int(self.reset_vector),
            bare_mode=bool(self.bare_mode),
            reset_cycles=int(self.reset_cycles),
        )
        if int(self.step_cycles) > 0:
            env.step(int(self.step_cycles))


@dataclass(frozen=True)
class ResetFrontendSequence:
    reset_cycles: int = 1
    step_cycles: int = 0

    def run(self, env):
        env.reset(int(self.reset_cycles))
        if int(self.step_cycles) > 0:
            env.step(int(self.step_cycles))
