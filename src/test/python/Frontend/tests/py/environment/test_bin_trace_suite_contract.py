import os
import subprocess
from pathlib import Path


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
