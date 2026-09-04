"""Select checkpoints for emu-performance CI test"""

from dataclasses import dataclass, asdict
import json
from pathlib import Path
import os
import random
import sys
from typing import Callable

CKPT_JSON_LEGACY = os.environ.get("CKPT_JSON_LEGACY", "")
CKPT_JSON_XSCC = os.environ.get("CKPT_JSON_XSCC", "")
CKPT_JSON = os.environ.get("CKPT_JSON", "")

RANDOM_SEED = os.environ.get("GITHUB_RUN_NUMBER", "0")  # for reproducibility


@dataclass
class SelectedCkpt:
    """Structure for selected checkpoint"""

    name: str
    point: str
    weight: float
    path: str
    prefix: str


@dataclass
class CkptJson:
    """Structure for original checkpoint JSON"""

    @dataclass
    class Benchmark:
        """Structure for each benchmark data"""

        insts: int
        points: dict[str, float]

    path: Path
    # the original is group_benchmark -> {point -> weight}
    # i.e. {"gcc_s04": {"point1": 0.5, "point2": 0.5}}
    # here we parse it into group -> {benchmark -> {point -> weight}}
    # i.e. {"gcc": {"s04": {"point1": 0.5, "point2": 0.5}}}
    benchmarks: dict[str, dict[str, Benchmark]]

    @staticmethod
    def from_json(path: Path) -> "CkptJson":
        """Parse the JSON file into a CkptJson object"""
        with path.open("r", encoding="utf-8") as f:
            content = json.load(f)
        benchmarks = {}
        for group_benchmark, data in content.items():
            if "_" in group_benchmark:
                group, benchmark = group_benchmark.split("_", 1)
            else:
                group, benchmark = group_benchmark, ""
            if group not in benchmarks:
                benchmarks[group] = {}
            benchmarks[group][benchmark] = CkptJson.Benchmark(
                insts=data["insts"], points=data["points"]
            )
        return CkptJson(path=path, benchmarks=benchmarks)

    @property
    def ckpt_path(self) -> Path:
        """Get the path to the checkpoint directory"""
        if (
            self.path.parent.parent / "checkpoint-0-0-0"
        ).exists():  # backward compatibility
            return self.path.parent.parent / "checkpoint-0-0-0"
        return self.path.parent.parent / "checkpoint"


def __format_name(group: str, benchmark: str) -> str:
    """Format the name of the checkpoint"""
    return f"{group}_{benchmark}" if benchmark else group


def __select_ckpts(
    j: CkptJson,
    prefix: str,
    select_func: Callable[[dict[tuple[str, str], float]], tuple[str, str]],
    exclude: set[tuple[str, str]] = set(),
) -> list[SelectedCkpt]:
    """Select checkpoints based on the provided selection function"""
    selected = []
    for group, benchmarks in j.benchmarks.items():
        # flatten the benchmarks into a single dictionary of benchmark_point -> weight
        flattened = {
            (benchmark, point): weight
            for benchmark, data in benchmarks.items()
            for point, weight in data.points.items()
            if (__format_name(group, benchmark), point) not in exclude
        }
        benchmark, point = select_func(flattened)
        name = __format_name(group, benchmark)
        ckpt_path = next((j.ckpt_path / name / point).glob("*.zstd"), None)
        if ckpt_path is None:
            print(
                f"Warning: No checkpoint found for '{name}_{point}'",
                file=sys.stderr,
            )
            continue
        selected.append(
            SelectedCkpt(
                name=name,
                point=point,
                weight=benchmarks[benchmark].points[point],
                path=str(ckpt_path),
                prefix=prefix,
            )
        )
    return selected


def select_most_weighted(j: CkptJson, prefix: str) -> list[SelectedCkpt]:
    """Select the most weighted checkpoint for each benchmark"""
    return __select_ckpts(
        j, prefix, lambda points: max(points, key=lambda p: points[p])
    )

def select_random(
    j: CkptJson, prefix: str, n: int, already_selected: list[SelectedCkpt]
) -> list[SelectedCkpt]:
    """Select n random benchmarks, and 1 checkpoint for each benchmark"""
    selected = __select_ckpts(
        j,
        prefix,
        lambda points: random.choice(list(points.keys())),
        exclude={(c.name, c.point) for c in already_selected},
    )
    return random.sample(selected, min(n, len(selected))) if selected else []


def main() -> None:
    """Entrypoint"""
    random.seed(int(RANDOM_SEED))  # for reproducibility
    selected = []

    # if legacy exists, run them
    # use legacy- prefix for performance report
    if CKPT_JSON_LEGACY != "":
        legacy_ckpt_json = CkptJson.from_json(Path(CKPT_JSON_LEGACY))
        selected.extend(select_most_weighted(legacy_ckpt_json, "legacy-"))

    # run the most weighted checkpoints from the main ckpt json
    ckpt_json = CkptJson.from_json(Path(CKPT_JSON))
    selected.extend(select_most_weighted(ckpt_json, ""))

    # run random 5 checkpoints from the main ckpt json
    # use fuzz- prefix to skip performance report
    selected.extend(select_random(ckpt_json, "fuzz-", 5, already_selected=selected))

    # also run random 5 checkpoints from xscc, use fuzz- prefix too
    if CKPT_JSON_XSCC != "":
        xscc_ckpt_json = CkptJson.from_json(Path(CKPT_JSON_XSCC))
        selected.extend(select_random(xscc_ckpt_json, "fuzz-", 5, already_selected=selected))

    print(json.dumps([asdict(c) for c in selected]))


if __name__ == "__main__":
    main()
