from pathlib import Path

import pytest

from tools.write_frontend_build_manifest import (
    _git,
    _worktree_head,
    _is_dut_input_path,
    _resolve_design_baseline_sha,
)


def test_linked_worktree_provenance_uses_its_own_head_and_index(tmp_path):
    import subprocess

    main = tmp_path / "main"
    linked = tmp_path / "linked"
    main.mkdir()

    def git(*args):
        return subprocess.check_output(["git", "-C", str(main), *args], text=True).strip()

    git("init", "-q")
    git("config", "user.name", "provenance-test")
    git("config", "user.email", "test@example.invalid")
    src = main / "Makefile"
    src.write_text("old\n")
    git("add", "Makefile")
    git("commit", "-qm", "old")
    old = git("rev-parse", "HEAD")
    src.write_text("new\n")
    git("commit", "-qam", "new")
    new = git("rev-parse", "HEAD")
    git("worktree", "add", "--detach", str(linked), old)
    src.write_text("user dirty\n")
    assert _worktree_head(linked) == old
    assert _git(linked, "status", "--porcelain") == ""
    assert _git(linked, "rev-parse", "HEAD") == old
    assert _git(main, "rev-parse", "HEAD") == new
    (linked / "Makefile").write_text("linked dirty\n")
    assert "Makefile" in _git(linked, "status", "--porcelain")
    assert src.read_text() == "user dirty\n"


@pytest.mark.parametrize(
    "path",
    [
        "Makefile",
        "build.sc",
        "src/main/scala/xiangshan/frontend/Frontend.scala",
        "src/main/resources/frontend.conf",
        "rocket-chip/src/main/scala/foo.scala",
    ],
)
def test_design_inputs_require_dut_rebuild(path):
    assert _is_dut_input_path(path)


@pytest.mark.parametrize(
    "path",
    [
        "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv",
        "src/test/python/Frontend/env/funcov/py/ifu/cfvec_funcov.py",
        "src/test/python/Frontend/tests/py/jiabowen/test_functional_coverage_baremode.py",
        "docs/README.md",
    ],
)
def test_verification_inputs_do_not_require_dut_rebuild(path):
    assert not _is_dut_input_path(path)


def test_design_baseline_tracks_latest_v3_merge_parent(monkeypatch):
    frontend_parent = "1" * 40
    design_parent = "2" * 40
    older_design_parent = "3" * 40
    merge_log = "\n".join(
        (
            f"{'4' * 40}\0{frontend_parent} {design_parent}\0"
            "Merge remote-tracking branch 'origin/kunminghu-v3' into frontend-bt",
            f"{'5' * 40}\0{frontend_parent} {older_design_parent}\0"
            "Merge remote-tracking branch 'origin/kunminghu-v3' into frontend-bt",
        )
    )
    monkeypatch.delenv("FRONTEND_DESIGN_BASELINE_SHA", raising=False)
    monkeypatch.setattr(
        "tools.write_frontend_build_manifest._git",
        lambda *_args: merge_log,
    )

    assert (
        _resolve_design_baseline_sha(Path("."), "a" * 40)
        == design_parent
    )


def test_explicit_design_baseline_overrides_merge_history(monkeypatch):
    monkeypatch.setenv("FRONTEND_DESIGN_BASELINE_SHA", "6" * 40)
    assert (
        _resolve_design_baseline_sha(Path("."), "a" * 40, "7" * 40)
        == "7" * 40
    )
