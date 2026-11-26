#!/usr/bin/env python3
"""Update and inspect SPEC checkpoint runtime profiles."""

import argparse
from collections import defaultdict, deque
import json
from pathlib import Path
import re
import statistics
from typing import Deque, Literal

DEFAULT_LOG_ROOT = Path("/nfs/home/ci-runner/perf-report")
PROFILE_DIR = Path(__file__).resolve().parent
DATA_DIR = PROFILE_DIR / "data"
CURRENT_DIR = PROFILE_DIR / "current"
SortMode = Literal["json", "avgtime", "maxtime"]

NEW_SUCCESS_RE = re.compile(
    r"\[(?P<progress>[^\]]+)\]\s+(?P<checkpoint>\S+)\s+succeeded\s+on\s+\S+\s+in\s+"
    r"(?P<hours>\d+)h\s+(?P<minutes>\d+)m\s+(?P<seconds>\d+(?:\.\d+)?)s\b"
)
SUCCESS_RE = re.compile(
    r"\[(?P<progress>[^\]]+)\]\s+(?P<checkpoint>\S+)\s+succeeded\s+on\b"
)
ELAPSED_RE = re.compile(
    r"\[(?P<progress>[^\]]+)\]\s+\.\.\.\s+elapsed:\s+"
    r"(?P<elapsed>[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?)\b"
)


def load_profile(profile_path: Path) -> dict:
    with profile_path.open("r", encoding="utf-8") as profile_file:
        profile = json.load(profile_file)

    if not isinstance(profile, dict):
        raise ValueError(f"Profile must contain a JSON object: {profile_path}")
    return profile


def get_known_checkpoints(profile: dict) -> set[tuple[str, str]]:
    """Return checkpoint identifiers represented by the profile."""
    checkpoints: set[tuple[str, str]] = set()
    for benchmark, points in profile.items():
        if not isinstance(points, dict):
            continue
        checkpoints.update((str(benchmark), str(point)) for point in points)
    return checkpoints


def parse_checkpoint_name(
    name: str, known: set[tuple[str, str]]
) -> tuple[str, str] | None:
    """Map ``benchmark_point_weight`` to a checkpoint in the profile."""
    parts = name.rsplit("_", 2)
    if len(parts) != 3:
        return None
    checkpoint = (parts[0], parts[1])
    return checkpoint if checkpoint in known else None


def get_runtimes(
    log_files: list[Path],
    known: set[tuple[str, str]],
) -> dict[tuple[str, str], list[float]]:
    """Collect all complete checkpoint runtime samples from runner logs."""
    runtimes: dict[tuple[str, str], list[float]] = defaultdict(list)
    for log_path in log_files:
        pending: dict[str, Deque[tuple[str, str] | None]] = defaultdict(deque)

        with log_path.open("r", encoding="utf-8", errors="replace") as log_file:
            for line in log_file:
                # New runner logs include the elapsed time on the success line.
                match = NEW_SUCCESS_RE.search(line)
                if match:
                    checkpoint = parse_checkpoint_name(match.group("checkpoint"), known)
                    if checkpoint is not None:
                        elapsed = (
                            int(match.group("hours")) * 3600
                            + int(match.group("minutes")) * 60
                            + float(match.group("seconds"))
                        )
                        runtimes[checkpoint].append(elapsed)
                    continue

                # Old runner logs put the elapsed time on the following line.
                match = SUCCESS_RE.search(line)
                if match:
                    checkpoint = parse_checkpoint_name(match.group("checkpoint"), known)
                    pending[match.group("progress")].append(checkpoint)
                    continue

                # The next line should be an elapsed line.
                match = ELAPSED_RE.search(line)
                if not match:
                    continue

                progress = match.group("progress")
                queue = pending.get(progress)
                if not queue:
                    continue

                checkpoint = queue.popleft()
                if not queue:
                    del pending[progress]
                if checkpoint is not None:
                    runtimes[checkpoint].append(float(match.group("elapsed")))

    return dict(runtimes)


def update_profile(profile_path: Path, log_files: list[Path]) -> tuple[int, int]:
    """Replace profile runtimes with averages found in runner logs."""
    profile = load_profile(profile_path)
    known = get_known_checkpoints(profile)
    runtimes = get_runtimes(log_files, known)

    for (benchmark, point), samples in runtimes.items():
        # keep integer seconds is enough
        profile[benchmark][point] = int(sum(samples) // len(samples))

    with profile_path.open("w", encoding="utf-8") as profile_file:
        json.dump(profile, profile_file, indent=2, ensure_ascii=False)

    return len(runtimes), sum(len(samples) for samples in runtimes.values())


def create_profile(checkpoint_path: Path, profile_path: Path) -> None:
    """Create a zeroed runtime profile from a checkpoint description."""
    with checkpoint_path.open("r", encoding="utf-8") as checkpoint_file:
        data = json.load(checkpoint_file)

    for benchmark, checkpoint in data.items():
        data[benchmark] = {point: 0 for point in checkpoint["points"]}

    profile_path.parent.mkdir(parents=True, exist_ok=True)
    with profile_path.open("w", encoding="utf-8") as profile_file:
        json.dump(data, profile_file, indent=2, ensure_ascii=False)


def current_profiles() -> list[Path]:
    """Return the profiles indexed by the ``current`` directory."""
    if not CURRENT_DIR.is_dir():
        return []
    return sorted(path for path in CURRENT_DIR.iterdir() if path.is_file())


def set_current_profile(profile_path: Path, name: str) -> Path | None:
    """Index a profile from ``current`` under ``name``.

    Return the link path when it was created, or ``None`` when an existing
    link was kept after the user declined to replace it.
    """
    if not name:
        raise ValueError("current profile name must not be empty")
    if not name.endswith(".json"):
        name += ".json"
    if Path(name).name != name or name in {".", ".."}:
        raise ValueError(
            f"current profile name must be a single path component: {name}"
        )

    current_path = CURRENT_DIR / name
    if current_path.exists() or current_path.is_symlink():
        try:
            answer = input(
                f"{current_path} already exists; overwrite it? [y/N] "
            )
        except EOFError:
            answer = ""
        if answer.strip().lower() not in {"y", "yes"}:
            print(f"Kept existing current entry {current_path}")
            return None
        if current_path.is_dir() and not current_path.is_symlink():
            raise IsADirectoryError(f"cannot replace directory: {current_path}")
        current_path.unlink()

    CURRENT_DIR.mkdir(parents=True, exist_ok=True)
    current_path.symlink_to(Path("..") / "data" / profile_path.name)
    return current_path


def get_profile_runtimes(profile: dict) -> dict[str, list[float]]:
    """Return numeric point runtimes grouped by benchmark."""
    runtimes: dict[str, list[float]] = {}
    for benchmark, points in profile.items():
        if not isinstance(points, dict):
            continue

        benchmark_runtimes = []
        for runtime in points.values():
            value = parse_runtime(runtime)
            if value is not None:
                benchmark_runtimes.append(value)
        runtimes[str(benchmark)] = benchmark_runtimes
    return runtimes


def parse_runtime(runtime: object) -> float | None:
    """Convert a profile value to a runtime, ignoring non-numeric values."""
    if isinstance(runtime, bool):
        return None
    try:
        return float(runtime)
    except (TypeError, ValueError):
        return None


def _measured_runtimes(values: list[float]) -> list[float]:
    return [runtime for runtime in values if runtime > 0]


def get_profile_slices(profile: dict) -> list[tuple[str, str, float]]:
    """Return numeric ``(benchmark, slice, runtime)`` entries in JSON order."""
    slices: list[tuple[str, str, float]] = []
    for benchmark, points in profile.items():
        if not isinstance(points, dict):
            continue
        for point, runtime in points.items():
            value = parse_runtime(runtime)
            if value is not None:
                slices.append((str(benchmark), str(point), value))
    return slices


def _ordered_benchmarks(
    benchmark_runtimes: dict[str, list[float]], sort: SortMode | None
) -> list[str]:
    """Order benchmark rows while keeping ties stable and deterministic."""
    benchmarks = list(benchmark_runtimes)
    if sort == "json":
        return benchmarks
    if sort == "avgtime":
        return sorted(
            benchmarks,
            key=lambda benchmark: _sort_time(benchmark_runtimes[benchmark], "avgtime"),
            reverse=True,
        )
    if sort == "maxtime":
        return sorted(
            benchmarks,
            key=lambda benchmark: _sort_time(benchmark_runtimes[benchmark], "maxtime"),
            reverse=True,
        )
    # Keep the historical --show ordering when no sort was requested.
    return sorted(benchmarks)


def _sort_time(values: list[float], sort: SortMode) -> float:
    measured = _measured_runtimes(values)
    if not measured:
        return float("-inf")
    if sort == "avgtime":
        return statistics.mean(measured)
    return max(measured)


def _ordered_slices(
    slices: list[tuple[str, str, float]], sort: SortMode | None
) -> list[tuple[str, str, float]]:
    """Order detail rows, preserving JSON order unless a time sort is requested."""
    if sort in {"avgtime", "maxtime"}:
        return sorted(slices, key=lambda item: item[2], reverse=True)
    return slices


def runtime_distribution(runtimes: list[float]) -> list[tuple[str, int]]:
    distribution = {
        "< 0": 0,
        "0": 0,
        "(0, 5,000]": 0,
        "(5,000, 7,500]": 0,
        "(7,500, 10,000]": 0,
        "(10,000, 15,000]": 0,
        "(15,000, 20,000]": 0,
        "(20,000, 30,000]": 0,
        "(30,000, 40,000]": 0,
        "> 40,000": 0,
    }
    for runtime in runtimes:
        if runtime < 0:
            bucket = "< 0"
        elif runtime == 0:
            bucket = "0"
        elif runtime <= 5000:
            bucket = "(0, 5,000]"
        elif runtime <= 7500:
            bucket = "(5,000, 7,500]"
        elif runtime <= 10000:
            bucket = "(7,500, 10,000]"
        elif runtime <= 15000:
            bucket = "(10,000, 15,000]"
        elif runtime <= 20000:
            bucket = "(15,000, 20,000]"
        elif runtime <= 30000:
            bucket = "(20,000, 30,000]"
        elif runtime <= 40000:
            bucket = "(30,000, 40,000]"
        else:
            bucket = "> 40,000"
        distribution[bucket] += 1
    return list(distribution.items())


def format_seconds(value: float) -> str:
    return f"{value:.6g}"


def show_profile(
    profile_path: Path,
    detail: bool = False,
    sort: SortMode | None = None,
) -> None:
    """Print profile totals and either benchmark or per-slice statistics."""
    profile = load_profile(profile_path)
    benchmark_runtimes = get_profile_runtimes(profile)
    runtimes = [runtime for values in benchmark_runtimes.values() for runtime in values]
    measured = [runtime for runtime in runtimes if runtime > 0]
    unmeasured = sum(runtime == 0 for runtime in runtimes)

    print(f"Profile: {profile_path}")
    print(f"Benchmarks: {len(benchmark_runtimes)}")
    print(f"Checkpoints: {len(runtimes)}")
    print(f"Measured checkpoints: {len(measured)}")
    print(f"Unmeasured checkpoints: {unmeasured}")

    print("\nRuntime statistics (measured checkpoints, seconds):")
    if measured:
        print(f"  Average: {format_seconds(statistics.mean(measured))}")
        print(f"  Median:  {format_seconds(statistics.median(measured))}")
        print(f"  Minimum: {format_seconds(min(measured))}")
        print(f"  Maximum: {format_seconds(max(measured))}")
    else:
        print("  No measured runtimes.")

    print("\nRuntime distribution (seconds):")
    total = len(runtimes)
    for bucket, count in runtime_distribution(runtimes):
        percentage = count / total * 100 if total else 0
        print(f"  {bucket:>18}: {count:4d} ({percentage:5.1f}%)")

    if detail:
        print("\nSlice statistics (seconds):")
        print(f"  {'Slice':<40} {'Runtime':>12}")
        for benchmark, point, runtime in _ordered_slices(
            get_profile_slices(profile), sort
        ):
            slice_name = f"{benchmark}_{point}"
            print(f"  {slice_name:<40} {format_seconds(runtime):>12}")
        return

    print("\nBenchmark statistics (seconds):")
    print(
        f"  {'Benchmark':<30} {'Points':>6} {'Measured':>9} "
        f"{'Average':>12} {'Minimum':>12} {'Maximum':>12}"
    )
    for benchmark in _ordered_benchmarks(benchmark_runtimes, sort):
        values = benchmark_runtimes[benchmark]
        measured_values = _measured_runtimes(values)
        if measured_values:
            average = format_seconds(statistics.mean(measured_values))
            minimum = format_seconds(min(measured_values))
            maximum = format_seconds(max(measured_values))
        else:
            average = minimum = maximum = "-"
        print(
            f"  {benchmark:<30} {len(values):6d} {len(measured_values):9d} "
            f"{average:>12} {minimum:>12} {maximum:>12}"
        )


def main():
    parser = argparse.ArgumentParser(
        description="Update or inspect SPEC checkpoint runtime profiles."
    )
    parser.add_argument(
        "--update",
        action="store_true",
        help="Update profile runtimes from runner logs",
    )
    parser.add_argument(
        "--show",
        action="store_true",
        help="Show profile runtime statistics",
    )
    parser.add_argument(
        "--detail",
        action="store_true",
        help="Show runtime statistics for each slice (use with --show)",
    )
    parser.add_argument(
        "--sort",
        choices=("json", "avgtime", "maxtime"),
        help=(
            "Sort statistics rows: json keeps profile order, avgtime sorts by "
            "average runtime, maxtime sorts by maximum runtime"
        ),
    )
    parser.add_argument(
        "--log-root",
        type=Path,
        default=DEFAULT_LOG_ROOT,
        help=f"Root containing */runner*.log (default: {DEFAULT_LOG_ROOT})",
    )
    parser.add_argument(
        "--profile",
        type=Path,
        help="Profile JSON to update or show (defaults to current profiles for --update)",
    )
    parser.add_argument(
        "--create-from",
        type=Path,
        default=None,
        help="Create profile from checkpoint json file",
    )
    parser.add_argument(
        "--set-current",
        metavar="NAME",
        help="Index a created profile in current/ under NAME",
    )

    args = parser.parse_args()

    if not args.update and not args.show and not args.create_from:
        parser.print_help()
        return
    if args.set_current and not args.create_from:
        parser.error("--set-current requires --create-from")

    profile_paths: list[Path] = []
    if args.create_from:
        if not args.create_from.is_file():
            raise FileNotFoundError(
                f"checkpoint json does not exist: {args.create_from}"
            )
        profile_name = args.create_from.parent.parent.name
        profile_path = DATA_DIR / f"{profile_name}.json"
        create_profile(args.create_from, profile_path)
        profile_paths.append(profile_path)
        print(f"Created profile {profile_path} from {args.create_from}")
        if args.set_current:
            current_path = set_current_profile(profile_path, args.set_current)
            if current_path is not None:
                print(f"Indexed profile as {current_path}")
    elif args.profile is not None:
        profile_paths.append(args.profile)

    if args.show and not profile_paths:
        parser.error("--show requires --profile")

    for profile_path in profile_paths:
        if not profile_path.is_file():
            raise FileNotFoundError(f"profile does not exist: {profile_path}")

    if args.update:
        if not args.log_root.is_dir():
            raise ValueError(f"log_path is not a directory: {args.log_root}")

        files = sorted(
            path for path in args.log_root.glob("*/runner*.log") if path.is_file()
        )
        if not files:
            print(
                f"No runner logs found under {args.log_root}; "
                "profile was not changed."
            )
        else:
            if not profile_paths:
                profile_paths = current_profiles()
                if not profile_paths:
                    print(
                        f"No profiles found under {CURRENT_DIR}; "
                        "profile was not changed."
                    )

            for profile_path in profile_paths:
                updated, samples = update_profile(profile_path, files)
                print(
                    f"Updated {updated} checkpoints from {samples} runtime samples "
                    f"in {len(files)} runner logs for {profile_path}."
                )

    if args.show:
        show_profile(profile_paths[0], detail=args.detail, sort=args.sort)


if __name__ == "__main__":
    main()
