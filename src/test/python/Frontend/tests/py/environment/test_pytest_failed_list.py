from pathlib import Path

import pytest


pytest_plugins = ("pytester",)
_FRONTEND = Path(__file__).resolve().parents[3]


@pytest.mark.parametrize("sim", ("verilator", "vcs"))
def test_failed_list_can_be_replayed(pytester, monkeypatch, sim):
    monkeypatch.setenv("TB_FRONTEND_SIM", sim)
    monkeypatch.setenv("TB_RUN_ID", "failed-list-test")
    monkeypatch.setenv("TB_SKIP_DUT_FINISH", "1")
    monkeypatch.delenv("TB_PYTEST_EXIT_STATUS_FILE", raising=False)
    monkeypatch.delenv("TB_FAILURE_LOG", raising=False)
    monkeypatch.setenv("PYTEST_DISABLE_PLUGIN_AUTOLOAD", "1")
    monkeypatch.delenv("PYTEST_ADDOPTS", raising=False)

    artifact_root = pytester.path / "build-frontend" / "artifacts"
    if sim == "vcs":
        artifact_root = pytester.path / "explicit-artifacts"
        monkeypatch.setenv("TB_ARTIFACT_DIR", str(artifact_root))
    else:
        artifact_root /= "failed-list-test"
        monkeypatch.delenv("TB_ARTIFACT_DIR", raising=False)
    failed_list = artifact_root / "failed.lst"

    # Simulate VCS's process exit in Finish(), before pytest's terminal summary.
    pytester.makeconftest(
        "import sys\n"
        "from pathlib import Path\n"
        f"sys.path.insert(0, {str(_FRONTEND)!r})\n"
        "from env.runtime import pylib\n"
        "pylib.frontend_build_root_path = lambda: Path(__file__).parent / 'build-frontend'\n"
        + (_FRONTEND / "conftest.py").read_text(encoding="utf-8")
        + """
def pytest_sessionstart(session):
    if os.environ["TB_FRONTEND_SIM"] == "vcs" and not session.config.getoption("collectonly"):
        from env.runtime import fixtures

        class VcsDut:
            def Finish(self):
                os._exit(17)

        fixtures._VCS_BATCH_DUT = VcsDut()
"""
    )
    pytester.makepyfile(
        test_cases="""
import pytest

@pytest.fixture
def failing_teardown():
    yield
    assert False

@pytest.mark.parametrize("case", ["failed parameter"])
def test_failed(case, failing_teardown):
    assert False

def test_pass():
    pass

@pytest.mark.xfail
def test_expected_failure():
    assert False
"""
    )

    result = pytester.runpytest_subprocess("-p", "no:cacheprovider", "-q")
    assert result.ret == (17 if sim == "vcs" else pytest.ExitCode.TESTS_FAILED)
    expected = "test_cases.py::test_failed[failed parameter]\n"
    assert failed_list.read_text(encoding="utf-8") == expected
    assert f"[frontend] failed test list: {failed_list}" in result.stdout.str()

    result = pytester.runpytest_subprocess(
        "-p", "no:cacheprovider", "--collect-only", "-q", f"@{failed_list}"
    )
    assert result.ret == pytest.ExitCode.OK
    result.stdout.fnmatch_lines(["test_cases.py::test_failed*"])
    assert failed_list.read_text(encoding="utf-8") == expected

    result = pytester.runpytest_subprocess("-p", "no:cacheprovider", "-q", "test_cases.py::test_pass")
    assert result.ret == (17 if sim == "vcs" else pytest.ExitCode.OK)
    assert not failed_list.exists()
