#!/usr/bin/env python3
from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path


_HERE = Path(__file__).resolve().parent
_REPO_ROOT = _HERE.parents[3]
_PYLIB_PATH = _REPO_ROOT / "build-frontend" / "pylib"
_DEFAULT_TESTS = (
    "src/test/python/Frontend/tests/test_backend_model_sync.py",
    "src/test/python/Frontend/tests/test_layout_import_compat.py",
    "src/test/python/Frontend/tests/test_sequence_unit.py",
)


def _with_pythonpath(env: dict[str, str], *paths: Path) -> dict[str, str]:
    merged = dict(env)
    existing = [entry for entry in merged.get("PYTHONPATH", "").split(os.pathsep) if entry]
    prefix = [str(path) for path in paths if str(path) not in existing]
    merged["PYTHONPATH"] = os.pathsep.join(prefix + existing)
    return merged


def build_pytest_command(args: list[str] | None = None) -> list[str]:
    extra_args = list(args or ())
    if extra_args:
        return [sys.executable, "-m", "pytest", *extra_args]
    return [sys.executable, "-m", "pytest", "-q", *_DEFAULT_TESTS]


def build_guard_env(env: dict[str, str] | None = None) -> dict[str, str]:
    return _with_pythonpath(dict(env or os.environ), _HERE, _PYLIB_PATH)


def main(argv: list[str] | None = None) -> int:
    cmd = build_pytest_command(list(argv or ()))
    env = build_guard_env()
    print("[frontend-change-guard] running:", " ".join(cmd[2:]))
    completed = subprocess.run(cmd, cwd=str(_REPO_ROOT), env=env, check=False)
    return int(completed.returncode)


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
