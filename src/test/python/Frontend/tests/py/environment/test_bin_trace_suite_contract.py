import os
import subprocess
from pathlib import Path


def test_bin_trace_suite_uses_dedicated_active_list_and_pipeline_wrapper() -> None:
    frontend_root = Path(__file__).resolve().parents[3]
    suite_source = (frontend_root / "scripts/run_bin_trace_suite.sh").read_text(encoding="utf-8")
    pipeline_source = (frontend_root / "scripts/run_bin_trace_pipeline.sh").read_text(encoding="utf-8")

    assert "pass bin paths or --list-file <path>" in suite_source
    assert "--list-file" in suite_source
    assert "run_bin_trace_pipeline.sh" in suite_source
    assert 'TB_BIN_TRACE_SUITE_CONTINUE_ON_FAIL="${TB_BIN_TRACE_SUITE_CONTINUE_ON_FAIL:-0}"' in suite_source
    assert 'SUITE_ARTIFACT_DIR="${SUITE_RUNS_ROOT}/suites/${SUITE_DATE}/${SUITE_TIME}_${SUITE_ID}"' in suite_source
    assert 'case_artifact_dir="${SUITE_ARTIFACT_DIR}/cases/${case_stem}"' in suite_source
    assert 'refusing to reuse existing suite root' in suite_source
    assert 'TB_SUITE_DATE must use YYYYMMDD' in suite_source
    assert 'TB_SUITE_TIME must use HHMMSS' in suite_source
    assert "tests/py/zhaoxinran/test_bin_trace_dut.py::test_bin_trace" in pipeline_source


def test_bin_trace_suite_allocates_dated_root_and_refuses_reuse(tmp_path: Path) -> None:
    frontend_root = Path(__file__).resolve().parents[3]
    suite_script = frontend_root / "scripts/run_bin_trace_suite.sh"
    env = {
        **os.environ,
        "TB_SUITE_ARTIFACT_DIR": str(tmp_path),
        "TB_SUITE_DATE": "20260809",
        "TB_SUITE_TIME": "235959",
        "TB_RUN_ID": "layout_contract",
    }
    command = ["bash", str(suite_script), "missing-layout-contract.bin"]

    first = subprocess.run(command, cwd=frontend_root.parents[3], env=env, capture_output=True, text=True)
    suite_root = tmp_path / "suites/20260809/235959_layout_contract"

    assert first.returncode == 2
    assert (suite_root / "cases").is_dir()
    assert "suite_dir:" in first.stdout

    second = subprocess.run(command, cwd=frontend_root.parents[3], env=env, capture_output=True, text=True)

    assert second.returncode == 2
    assert "refusing to reuse existing suite root" in second.stderr
