from __future__ import annotations

from dataclasses import dataclass

from ..transactions import CommitTarget, GoldenTraceSource


@dataclass(frozen=True)
class LoadGoldenTraceSequence:
    source: GoldenTraceSource
    step_cycles: int = 0

    def run(self, env) -> int:
        count = int(env.load_golden_trace_file(self.source.path))
        if int(self.step_cycles) > 0:
            env.step(int(self.step_cycles))
        return count


@dataclass(frozen=True)
class RunUntilCommitSequence:
    target: CommitTarget

    def run(self, env) -> int:
        return int(env.backend_model.wait_for_commits(self.target.target_count, self.target.max_cycles))
