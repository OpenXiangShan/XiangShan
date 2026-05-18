from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, List, Sequence, Tuple

from ..core import ScoreboardResult
from ..monitors.frontend_monitor import Observation
from ..transactions import IFUFetchTransaction


def _fetch_path_from_pc(pc: int) -> str:
    # Keep this bridge deliberately simple for the first bare-mode sample. The
    # full path taxonomy remains owned by FunctionalCoverageRecorder.
    return "mmio_uncache" if int(pc) < 0x80000000 else "icache_seq"


@dataclass(frozen=True)
class IFUFetchMonitorAdapter:
    """Adapter from the existing FrontendMonitor observations to IFU transactions.

    UVM mapping: monitor analysis item normalization. It avoids changing the
    production monitor while giving new tests a stable transaction contract.
    """

    def from_observations(self, observations: Iterable[Observation]) -> Tuple[IFUFetchTransaction, ...]:
        txns: List[IFUFetchTransaction] = []
        for obs in observations:
            txns.append(
                IFUFetchTransaction(
                    cycle=int(obs.cycle),
                    pc=int(obs.pc),
                    instr=int(obs.instr),
                    is_rvc=bool(obs.is_rvc),
                    valid=True,
                    exception=None,
                    fetch_path=_fetch_path_from_pc(int(obs.pc)),
                    slot=int(obs.slot),
                )
            )
        return tuple(txns)

    def from_env(self, env) -> Tuple[IFUFetchTransaction, ...]:
        monitor = getattr(env, "monitor", None)
        observations = getattr(monitor, "observations", [])
        return self.from_observations(observations)


@dataclass(frozen=True)
class SequentialIFUReferenceModel:
    """Reference/checker for the bare-mode sequential IFU fetch pilot.

    UVM mapping: a tiny RM+scoreboard pair for the first Python sample. It checks
    ordered expected PCs against actual IFU fetch transactions while tolerating
    bubbles and multi-slot cycles.
    """

    expected_pcs: Sequence[int]
    fetch_path: str = "icache_seq"

    def compare(self, actual: Iterable[IFUFetchTransaction]) -> ScoreboardResult:
        expected = [int(pc) for pc in self.expected_pcs]
        if not expected:
            return ScoreboardResult(passed=True, checked=0)

        cursor = 0
        errors: List[str] = []
        for txn in actual:
            if not bool(txn.valid):
                continue
            if str(txn.fetch_path) != str(self.fetch_path):
                continue
            if int(txn.pc) == expected[cursor]:
                cursor += 1
                if cursor >= len(expected):
                    return ScoreboardResult(passed=True, checked=cursor)

        if cursor < len(expected):
            errors.append(
                "sequential IFU PC stream incomplete: "
                f"matched={cursor}/{len(expected)} "
                f"next_expected=0x{expected[cursor]:x}"
            )
        return ScoreboardResult(passed=False, checked=cursor, errors=tuple(errors))
