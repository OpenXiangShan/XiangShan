#!/usr/bin/env python3
"""Build an isolated Python-enabled Picker/Toffee verification environment."""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import venv
from pathlib import Path

from bootstrap_picker import (
    BootstrapError,
    checkout_revision,
    ensure_checkout,
    load_toolchain,
    local_swig,
    run,
    write_if_changed,
)


def installed_version(python: Path, name: str, environment: dict[str, str]) -> str:
    script = (
        "import importlib.metadata as metadata, sys; "
        "print(metadata.version(sys.argv[1]))"
    )
    try:
        return run(
            [str(python), "-c", script, name],
            capture_output=True,
            env=environment,
        )
    except subprocess.CalledProcessError:
        return "missing"


def previous_revisions(path: Path) -> tuple[str, str]:
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return "", ""
    if not isinstance(document, dict):
        return "", ""
    picker = document.get("picker_commit")
    xcomm = document.get("xcomm_commit")
    return (
        picker if isinstance(picker, str) else "",
        xcomm if isinstance(xcomm, str) else "",
    )


def create_environment(environment_dir: Path) -> Path:
    python = environment_dir / "bin" / "python"
    if not python.is_file():
        builder = venv.EnvBuilder(with_pip=True, clear=False, symlinks=True)
        builder.create(environment_dir)
    return python


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path)
    parser.add_argument("--picker-root", type=Path)
    parser.add_argument("--environment", type=Path, required=True)
    parser.add_argument("--metadata", type=Path, required=True)
    parser.add_argument("--stamp", type=Path, required=True)
    parser.add_argument("--project", type=Path, required=True)
    parser.add_argument("--lock", type=Path, default=None)
    parser.add_argument("--jobs", type=int, default=8)
    parser.add_argument(
        "--python-only",
        action="store_true",
        help="Install the locked Python environment without building Picker.",
    )
    args = parser.parse_args()

    try:
        if args.jobs < 1:
            raise BootstrapError("--jobs must be positive")
        if not args.python_only and (args.root is None or args.picker_root is None):
            raise BootstrapError("--root and --picker-root are required for Picker builds")
        tools = load_toolchain(args.lock) if args.lock else load_toolchain()
        toffee_spec = tools["toffee"]
        toffee_test_spec = tools["toffee_test"]
        picker_commit = ""
        xcomm_commit = ""
        swig = ""
        picker_root: Path | None = None
        if not args.python_only:
            assert args.root is not None
            assert args.picker_root is not None
            picker_spec = tools["picker"]
            xcomm_spec = tools["xcomm"]
            picker_root = args.picker_root.resolve()
            picker_commit = ensure_checkout(
                picker_root,
                "Toffee Picker",
                str(picker_spec["repository"]),
                str(picker_spec["ref"]),
                str(picker_spec["commit"]),
            )
            build_environment = os.environ.copy()
            build_environment["BUILD_XSPCOMM_SWIG"] = "python"
            swig = local_swig(picker_root.parent, build_environment)
            run(["make", "init"], cwd=picker_root, env=build_environment)
            xcomm = picker_root / "dependence/xcomm"
            xcomm_commit = checkout_revision(
                xcomm,
                "Toffee xcomm",
                str(xcomm_spec["ref"]),
                str(xcomm_spec["commit"]),
            )
            revisions = (picker_commit, xcomm_commit)
            if previous_revisions(args.metadata) != revisions:
                shutil.rmtree(picker_root / "build", ignore_errors=True)
            run(
                [
                    "make",
                    "build",
                    f"NPROC={args.jobs}",
                    f"ARGS=-DSWIG_EXECUTABLE={swig}",
                ],
                cwd=picker_root,
                env=build_environment,
            )

        env_dir = args.environment.resolve()
        python = create_environment(env_dir)
        pip_environment = os.environ.copy()
        pip_environment["PYTHONNOUSERSITE"] = "1"
        for spec in (toffee_spec, toffee_test_spec):
            run(
                [
                    str(python),
                    "-m",
                    "pip",
                    "install",
                    f"git+{spec['repository']}@{spec['commit']}",
                ],
                env=pip_environment,
            )
        run(
            [
                str(python),
                "-m",
                "pip",
                "install",
                "--no-deps",
                "-e",
                str(args.project.resolve()),
            ],
            env=pip_environment,
        )

        metadata = {
            "schema_version": 1,
            "profile": "toffee-python" if args.python_only else "toffee",
            "picker": str(picker_root / "build/bin/picker") if picker_root else "",
            "picker_commit": picker_commit,
            "xcomm_commit": xcomm_commit,
            "toffee_commit": str(toffee_spec["commit"]),
            "toffee_test_commit": str(toffee_test_spec["commit"]),
            "python": str(python),
            "python_version": sys.version.split()[0],
            "toffee_version": installed_version(python, "pytoffee", pip_environment),
            "toffee_test_version": installed_version(
                python,
                "toffee-test",
                pip_environment,
            ),
            "pytest_version": installed_version(python, "pytest", pip_environment),
            "pytest_asyncio_version": installed_version(
                python,
                "pytest-asyncio",
                pip_environment,
            ),
            "pytest_xdist_version": installed_version(
                python,
                "pytest-xdist",
                pip_environment,
            ),
            "swig": str(swig),
            "project": str(args.project.resolve()),
        }
        write_if_changed(
            args.metadata,
            json.dumps(metadata, indent=2, sort_keys=True) + "\n",
        )
        args.stamp.parent.mkdir(parents=True, exist_ok=True)
        args.stamp.write_text("complete\n", encoding="utf-8")
    except (BootstrapError, OSError, subprocess.CalledProcessError) as error:
        print(f"bootstrap_toffee.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
