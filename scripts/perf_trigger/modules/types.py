from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
import subprocess
import time
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .gcpt import GCPT


@dataclass
class EmuConfig:
    """Configuration for emu runs"""

    warmup: int
    max_instr: int
    threads: int
    with_numactl: bool = True
    cst_file: Path | None = None
    dump_db: bool = False
    dry_run: bool = False
    max_cycles: int | None = None
    save_dramsim3: bool = False


@dataclass
class FreeCoreInfo:
    free: bool
    mem_node: int
    start: int
    end: int
    total: int

    @classmethod
    def none(cls) -> "FreeCoreInfo":
        return FreeCoreInfo(False, 0, 0, 0, 0)

    def num(self) -> int:
        return self.end - self.start + 1

    def split(self, req: int) -> "FreeCoreInfo":
        if not self.free or req > self.num():
            return FreeCoreInfo(free=False, mem_node=0, start=0, end=0, total=0)

        allocated = FreeCoreInfo(
            free=True,
            mem_node=self.mem_node,
            start=self.start,
            end=self.start + req - 1,
            total=self.total,
        )

        self.start += req
        if self.start > self.end:
            self.free = False

        return allocated


@dataclass
class PendingTask:
    proc: subprocess.Popen
    name: str
    free: FreeCoreInfo
    started: float = field(default_factory=time.time)
    command: tuple[str, ...] = ()
    stdout_path: Path | None = None
    stderr_path: Path | None = None

    gcpt: GCPT | None = None

    @property
    def elapsed(self) -> float:
        return time.time() - self.started
