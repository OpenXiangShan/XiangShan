from __future__ import annotations

import os

from change_guard import _DEFAULT_TESTS, _HERE, _PYLIB_PATH, build_guard_env, build_pytest_command


def test_change_guard_default_command_targets_fast_frontend_smoke_suite() -> None:
    cmd = build_pytest_command()

    assert cmd[:3] == [cmd[0], "-m", "pytest"]
    assert cmd[3] == "-q"
    assert tuple(cmd[4:]) == _DEFAULT_TESTS


def test_change_guard_env_prepends_frontend_paths_without_dropping_existing_pythonpath() -> None:
    original = {"PYTHONPATH": os.pathsep.join(("/tmp/existing-a", "/tmp/existing-b"))}

    merged = build_guard_env(original)

    pythonpath = merged["PYTHONPATH"].split(os.pathsep)
    assert pythonpath[:2] == [str(_HERE), str(_PYLIB_PATH)]
    assert pythonpath[2:] == ["/tmp/existing-a", "/tmp/existing-b"]
