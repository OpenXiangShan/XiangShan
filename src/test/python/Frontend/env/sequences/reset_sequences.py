from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class ResetFrontendSequence:
    reset_cycles: int = 1

    def run(self, env):
        return env.reset(int(self.reset_cycles))
