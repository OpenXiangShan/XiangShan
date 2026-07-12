#!/usr/bin/env python3
"""Run native XiangShan TopDown as a thin CI post-processing wrapper."""

import argparse
import importlib
import os
from pathlib import Path
import subprocess
import sys


DEPENDENCIES = ("numpy", "pandas", "matplotlib", "psutil")
WEIGHTED_CSV = Path("results") / "results-weighted_base.csv"


def parse_args():
    parser = argparse.ArgumentParser(
        description="Run native XiangShan TopDown for a SPEC CI result directory."
    )
    parser.add_argument("--stat-dir", required=True, help="SPEC result directory")
    parser.add_argument("--cluster-json", required=True, help="checkpoint cluster JSON")
    parser.add_argument("--xiangshan-root", required=True, help="XiangShan checkout root")
    parser.add_argument("--benchmark-type", required=True, help="CI benchmark type")
    parser.add_argument("--output", required=True, help="TopDown output directory")
    return parser.parse_args()


def write_text(path, content):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def write_result(output_dir, status, details=None):
    output_dir.mkdir(parents=True, exist_ok=True)
    write_text(output_dir / "topdown_status.txt", status + "\n")

    lines = [f"Output: `{output_dir}`"]
    if not status.startswith("OK"):
        lines += ["", f"Python executable: `{sys.executable}`"]
    if details:
        lines += ["", details]
    write_text(output_dir / "topdown.md", "\n".join(lines) + "\n")


def fail(output_dir, reason):
    status = f"FAILED: {reason}"
    write_result(output_dir, status)
    print(status, file=sys.stderr)
    return 1


def skip(output_dir, reason):
    status = f"SKIPPED: {reason}"
    write_result(output_dir, status)
    print(status)
    return 0


def require_file(path, description):
    if not path.is_file():
        raise FileNotFoundError(f"missing {description}: {path}")


def require_dir(path, description):
    if not path.is_dir():
        raise FileNotFoundError(f"missing {description}: {path}")


def check_dependencies():
    missing = []
    for module in DEPENDENCIES:
        try:
            importlib.import_module(module)
        except ImportError:
            missing.append(module)
    return missing


def find_simulator_err(stat_dir):
    for path in stat_dir.rglob("simulator_err.txt"):
        if path.is_file():
            return path
    return None


def run_topdown(args):
    output_dir = Path(args.output).resolve()
    stat_dir = Path(args.stat_dir).resolve()
    cluster_json = Path(args.cluster_json).resolve()
    xiangshan_root = Path(args.xiangshan_root).resolve()
    topdown_script = xiangshan_root / "scripts" / "top-down" / "top_down.py"
    topdown_config = xiangshan_root / "scripts" / "top-down" / "configs.py"

    output_dir.mkdir(parents=True, exist_ok=True)

    if args.benchmark_type == "dryrun":
        return skip(
            output_dir,
            "dryrun benchmark_type is a smoke run, not performance data",
        )

    try:
        require_dir(stat_dir, "SPEC result directory")
        require_file(cluster_json, "checkpoint cluster JSON")
        require_file(topdown_script, "native TopDown script")
        require_file(topdown_config, "native TopDown config")
    except FileNotFoundError as exc:
        return fail(output_dir, str(exc))

    if find_simulator_err(stat_dir) is None:
        return skip(output_dir, f"no simulator_err.txt found under {stat_dir}")

    missing = check_dependencies()
    if missing:
        return fail(output_dir, "missing dependency " + ", ".join(missing))

    expected_csv = output_dir / WEIGHTED_CSV
    if expected_csv.exists():
        expected_csv.unlink()

    stdout_path = output_dir / "topdown_stdout.txt"
    stderr_path = output_dir / "topdown_stderr.txt"
    command = [
        sys.executable,
        str(topdown_script),
        "--base-stat-dir",
        str(stat_dir),
        "-j",
        str(cluster_json),
        "--base-label",
        "TEST",
    ]

    env = os.environ.copy()
    env["MPLBACKEND"] = "Agg"

    with stdout_path.open("w", encoding="utf-8") as stdout, stderr_path.open(
        "w", encoding="utf-8"
    ) as stderr:
        completed = subprocess.run(
            command,
            cwd=str(output_dir),
            env=env,
            stdout=stdout,
            stderr=stderr,
            check=False,
        )

    if completed.returncode != 0:
        return fail(
            output_dir,
            "native TopDown failed with exit code "
            f"{completed.returncode}; see {stdout_path} and {stderr_path}",
        )

    if not expected_csv.exists():
        return fail(output_dir, f"missing native weighted CSV: {expected_csv}")

    write_result(output_dir, "OK")
    print("OK")
    return 0


def main():
    args = parse_args()
    try:
        return run_topdown(args)
    except Exception as exc:
        output_dir = Path(args.output).resolve()
        return fail(output_dir, f"internal wrapper error: {exc}")


if __name__ == "__main__":
    sys.exit(main())
