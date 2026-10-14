"""Select checkpoints for emu-performance CI test"""

import argparse
from dataclasses import dataclass, asdict
import json
from pathlib import Path
import os
import random
import sys
from typing import Callable


@dataclass
class SelectedCkpt:
    """Structure for selected checkpoint"""

    name: str
    point: str
    weight: float
    eta: float
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
        etas: dict[str, float]

        def filtered_points(self, eta_threshold: float | None) -> dict[str, float]:
            """Get the filtered points based on ETA_THRESHOLD"""
            return (
                {
                    point: weight
                    for point, weight in self.points.items()
                    if self.etas.get(point, 0.0) < eta_threshold
                }
                if eta_threshold is not None
                else self.points
            )

    path: Path
    # the original is group_benchmark -> {point -> weight}
    # i.e. {"gcc_s04": {"point1": 0.5, "point2": 0.5}}
    # here we parse it into group -> {benchmark -> {point -> weight}}
    # i.e. {"gcc": {"s04": {"point1": 0.5, "point2": 0.5}}}
    benchmarks: dict[str, dict[str, Benchmark]]

    @staticmethod
    def from_json(path: Path, profile_path: Path | None = None) -> "CkptJson":
        """Parse the JSON file into a CkptJson object"""
        with path.open("r", encoding="utf-8") as f:
            content = json.load(f)

        # load profile (group_benchmark -> {point -> eta}) if PROFILE_PATH is set
        profile_content = {}
        if profile_path:
            profile_json_path = profile_path / f"{path.parent.parent.name}.json"
            if profile_json_path.exists():
                print(f"Loading profile from {profile_json_path}", file=sys.stderr)
                with profile_json_path.open("r", encoding="utf-8") as f:
                    profile_content = json.load(f)
            else:
                print(f"{profile_json_path} does not exist, skipping", file=sys.stderr)
        else:
            print("PROFILE_PATH is not set, skipping profile loading", file=sys.stderr)

        benchmarks = {}
        for group_benchmark, data in content.items():
            if "_" in group_benchmark:
                group, benchmark = group_benchmark.split("_", 1)
            else:
                group, benchmark = group_benchmark, ""
            if group not in benchmarks:
                benchmarks[group] = {}

            benchmarks[group][benchmark] = CkptJson.Benchmark(
                insts=data["insts"],
                points=data["points"],
                etas=profile_content.get(group_benchmark, {}),
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


def select_most_weighted(
    j: CkptJson, prefix: str, eta_threshold: float | None = None
) -> list[SelectedCkpt]:
    """Select the most weighted checkpoint for each benchmark"""
    selected = []
    for group, benchmarks in j.benchmarks.items():
        # flatten the benchmarks into a single dictionary of benchmark_point -> weight
        flattened = {
            (benchmark, point): weight
            for benchmark, data in benchmarks.items()
            for point, weight in data.filtered_points(eta_threshold).items()
        }
        benchmark, point = max(flattened, key=lambda p: flattened[p])
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
                eta=benchmarks[benchmark].etas.get(point, 0.0),
                path=str(ckpt_path),
                prefix=prefix,
            )
        )
    return selected


def select_random(
    j: CkptJson,
    prefix: str,
    n: int,
    already_selected: list[SelectedCkpt] | None = None,
    eta_threshold: float | None = None,
) -> list[SelectedCkpt]:
    """Select n random checkpoints"""
    flattened = {
        (__format_name(group, benchmark), point): (
            weight,
            benchmarks[benchmark].etas.get(point, 0.0),
        )
        for group, benchmarks in j.benchmarks.items()
        for benchmark, data in benchmarks.items()
        for point, weight in data.filtered_points(eta_threshold).items()
    }

    for c in already_selected or []:
        flattened.pop((c.name, c.point), None)

    selected = random.sample(list(flattened.items()), k=min(n, len(flattened)))

    return [
        SelectedCkpt(
            name=name,
            point=point,
            weight=weight,
            eta=eta,
            path=str(next((j.ckpt_path / name / point).glob("*.zstd"), None)),
            prefix=prefix,
        )
        for (name, point), (weight, eta) in selected
    ]


def main() -> None:
    """Entrypoint"""
    parser = argparse.ArgumentParser(description="Select checkpoints for CI test")
    parser.add_argument(
        "--seed", type=int, default=0, help="Random seed for reproducibility"
    )
    parser.add_argument(
        "--profile",
        type=Path,
        help="Path to profile JSON for filtering/sorting benchmarks based on ETA",
    )
    parser.add_argument(
        "--eta-threshold",
        type=float,
        help="ETA threshold for filtering benchmarks (in seconds)",
    )

    parser.add_argument(
        "--weighted",
        type=str, # do not use Path here, because we need to parse prefix:path
        action="append",
        help="prefix:path for weighted selection",
    )
    parser.add_argument(
        "--random",
        type=str, # similar to --weighted, but with prefix:count:path
        action="append",
        help="prefix:count:path for random selection",
    )

    parser.add_argument(
        "--output",
        choices=["json", "list"],
        default="json",
        help="Output format: config json or file path list",
    )

    args = parser.parse_args()

    random.seed(int(args.seed))  # for reproducibility
    selected = []

    for conf in args.weighted or []:
        prefix, path = conf.split(":")
        if not path or not (path := Path(path)).exists():
            print(
                f"Warning: Weighted selection path '{path}' for '{prefix}' does not exist",
                file=sys.stderr,
            )
            continue
        j = CkptJson.from_json(path, profile_path=args.profile)
        selected.extend(select_most_weighted(j, prefix, args.eta_threshold))

    for conf in args.random or []:
        prefix, count, path = conf.split(":")
        count = int(count)
        if not path or not (path := Path(path)).exists():
            print(
                f"Warning: Random selection path '{path}' for '{prefix}' does not exist",
                file=sys.stderr,
            )
            continue
        j = CkptJson.from_json(path, profile_path=args.profile)
        selected.extend(select_random(j, prefix, count, selected, args.eta_threshold))

    # sort by eta for better scheduling, descending order
    selected.sort(key=lambda c: -c.eta)

    if args.output == "json":
        print(json.dumps([asdict(c) for c in selected]))
    else:
        for c in selected:
            print(c.path)


if __name__ == "__main__":
    main()
