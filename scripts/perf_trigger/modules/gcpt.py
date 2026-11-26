from enum import Enum
import hashlib
from pathlib import Path
import re


class GCPT:
    class State(Enum):
        NONE = 0
        RUNNING = 1
        FINISHED = 2
        ABORTED = 3

    def __init__(
        self,
        gcpt_path: Path,
        result_path: Path,
        benchmark: str,
        checkpoint: str,
        weight: float,
        eta: int = 0,
        bin_path: Path | None = None,
        name: str | None = None,
    ):
        self.__gcpt_path = gcpt_path
        self.__benchmark = benchmark
        self.__checkpoint = checkpoint
        self.__weight = weight
        self.__eta = eta
        self.__bin_path_override = bin_path
        self.__name_override = name
        self.__state = GCPT.State.NONE
        self.__result_path = result_path / str(self)
        self.__retry_count = 0

    @classmethod
    def from_image(cls, image_path: Path, result_path: Path) -> "GCPT":
        resolved = image_path.expanduser().resolve()
        digest = hashlib.sha256(str(resolved).encode()).hexdigest()[:8]
        safe_name = resolved.name.replace("/", "_")
        return cls(
            gcpt_path=resolved.parent,
            result_path=result_path,
            benchmark=resolved.stem,
            checkpoint="image",
            weight=1.0,
            bin_path=resolved,
            name=f"{safe_name}_{digest}",
        )

    def __str__(self) -> str:
        if self.__name_override is not None:
            return self.__name_override
        return "_".join([self.__benchmark, self.__checkpoint, f"{self.__weight:.6f}"])

    @property
    def benchmark(self) -> str:
        return self.__benchmark

    @property
    def benchmark_group(self) -> str:
        return self.__benchmark.split("_")[0]  # gcc_s02 -> gcc

    @property
    def checkpoint(self) -> str:
        return self.__checkpoint

    @property
    def weight(self) -> float:
        return self.__weight

    @property
    def eta(self) -> float:
        return self.__eta

    @property
    def state(self) -> "GCPT.State":
        return self.__state

    @property
    def bin_path(self) -> Path:
        if self.__bin_path_override is not None:
            return self.__bin_path_override
        return (
            self.__gcpt_path
            / self.__benchmark
            / self.__checkpoint
            / f"_{self.__checkpoint}_{self.__weight:.6f}_memory_.zstd"
        )

    @property
    def result_path(self) -> Path:
        return self.__result_path

    @property
    def stdout_path(self) -> Path:
        return self.__result_path / "simulator_out.txt"

    @property
    def stderr_path(self) -> Path:
        return self.__result_path / "simulator_err.txt"

    @property
    def db_path(self) -> Path:
        return self.__result_path / "simulator.db"

    def refresh_state(self) -> "GCPT.State":
        if (
            not self.stdout_path.exists()
            or self.__state == GCPT.State.FINISHED
            or self.__state == GCPT.State.ABORTED
        ):
            return self.__state

        self.__state = GCPT.State.RUNNING
        with self.stdout_path.open("r", encoding="utf-8", errors="replace") as stdout:
            for line in stdout:
                if "ABORT at pc" in line or "FATAL:" in line or "Error:" in line:
                    self.__state = GCPT.State.ABORTED
                    break
                elif "EXCEEDING CYCLE/INSTR LIMIT" in line or "GOOD TRAP" in line:
                    self.__state = GCPT.State.FINISHED
                    break
                elif "SOME SIGNAL STOPS THE PROGRAM" in line:
                    self.__state = GCPT.State.NONE
                    break

        if self.__state != GCPT.State.RUNNING:
            return self.__state

        with self.stderr_path.open("r", encoding="utf-8", errors="replace") as stderr:
            for line in stderr:
                if "Assertion" in line and "failed" in line:
                    self.__state = GCPT.State.ABORTED

        return self.__state

    def clear_state(self) -> None:
        self.__state = GCPT.State.NONE

    def get_perf(
        self, counters: set[str] | None = None, full_name: bool = False
    ) -> dict[str, int]:
        if self.refresh_state() != GCPT.State.FINISHED:
            return {}

        perf_data = {}
        pattern = re.compile(
            r"\[PERF\s*\]\[time=\s*\d+\] (([a-zA-Z0-9_]+\.)+[a-zA-Z0-9_@]+): ((\w| |\')+),\s+-?(\d+)$"
        )

        with self.stderr_path.open("r", encoding="utf-8", errors="replace") as f:
            for line in f:
                m = pattern.match(line)
                if not m:
                    continue
                name = f"{m.group(1)}.{m.group(3)}" if full_name else m.group(3)
                try:
                    value = int(m.group(5))
                except ValueError:
                    continue
                if counters is None or name in counters:
                    perf_data[name] = value

        return perf_data

    def get_cpi(self) -> float | None:
        if self.refresh_state() != GCPT.State.FINISHED:
            return None

        data = self.get_perf({"clock_cycle", "commitInstr"}, full_name=False)

        if "clock_cycle" not in data or "commitInstr" not in data:
            return None

        return data["clock_cycle"] / data["commitInstr"]

    def get_dramsim3_config(self) -> str:
        with self.stdout_path.open("r", encoding="utf-8", errors="replace") as f:
            for line in f:
                if "DRAMSIM3 config:" in line:
                    return line.split("DRAMSIM3 config:")[1].strip()
        return ""

    def retry(self, limit: int = 3) -> bool:
        if self.__retry_count >= limit:
            return False
        self.__retry_count += 1
        self.clear_state()
        return True
