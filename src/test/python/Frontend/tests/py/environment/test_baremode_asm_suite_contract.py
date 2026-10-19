import subprocess
from pathlib import Path

from env.funcov.runtime_context import FrontendFuncovSampleHub, default_pilot_csv_path


def test_baremode_asm_suite_discovers_and_scopes_all_cases(tmp_path: Path) -> None:
    frontend_root = Path(__file__).resolve().parents[3]
    suite_script = frontend_root / "scripts/run_baremode_asm_suite.sh"
    listed_cases = subprocess.run(
        ["bash", str(suite_script), "--list"],
        cwd=frontend_root.parents[3],
        check=True,
        capture_output=True,
        text=True,
    ).stdout.splitlines()
    active_testcases = {
        item.suggested_testcase
        for item in FrontendFuncovSampleHub.from_pilot_csv(
            default_pilot_csv_path(),
            testcase_name="runner-contract",
            artifact_tag="runner-contract",
            output_dir=tmp_path,
        ).definitions
    }
    tracked_asm_cases = {
        str(path.resolve())
        for path in (frontend_root / "tests/asm_cases").rglob("*.S")
    }

    assert listed_cases == sorted(tracked_asm_cases)
    assert len(listed_cases) == len(tracked_asm_cases)
    listed_stems = {Path(path).stem for path in listed_cases}
    assert listed_stems & active_testcases
