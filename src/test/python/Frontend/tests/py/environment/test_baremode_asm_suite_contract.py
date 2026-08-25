import subprocess
from pathlib import Path

from env.funcov.recorder import FunctionalCoverageRecorder, default_pilot_csv_path


def test_baremode_asm_suite_discovers_and_scopes_all_cases(tmp_path: Path) -> None:
    frontend_root = Path(__file__).resolve().parents[3]
    wrapper_source = (frontend_root / "scripts/run_baremode_asm_bin_trace.sh").read_text(
        encoding="utf-8"
    )
    suite_source = (frontend_root / "scripts/run_baremode_asm_suite.sh").read_text(
        encoding="utf-8"
    )

    assert 'if [[ -z "${TB_FUNCOV_TARGET_TESTCASES+x}" ]]; then' in wrapper_source
    assert 'PILOT_CSV="${FRONTEND_DIR}/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"' in wrapper_source
    assert "funcov testcase not in canonical registry; using unscoped observation" in wrapper_source
    assert 'TB_FUNCOV_TARGET_TESTCASES="${TB_FUNCOV_TARGET_TESTCASES}"' in wrapper_source
    assert "mapfile -d '' -t DEFAULT_CASES" in suite_source
    assert "-type f -name '*.S' -print0 | sort -z" in suite_source
    assert 'case_run_id="${SUITE_ID}_${case_stem}"' in suite_source
    assert 'SUITE_ARTIFACT_DIR="${SUITE_RUNS_ROOT}/suites/${SUITE_DATE}/${SUITE_TIME}_${SUITE_ID}"' in suite_source
    assert 'case_artifact_dir="${SUITE_ARTIFACT_DIR}/cases/${case_stem}"' in suite_source
    assert 'suite_report_dir="${SUITE_ARTIFACT_DIR}/report"' in suite_source
    assert '--data-dir "${SUITE_ARTIFACT_DIR}/cases"' in suite_source
    assert '--glob "*/coverage/*.dat"' in suite_source
    assert 'refusing to reuse existing suite root' in suite_source
    assert 'TB_SUITE_DATE must use YYYYMMDD' in suite_source
    assert 'TB_SUITE_TIME must use HHMMSS' in suite_source
    assert "tools/backannotate_funcov.py" in suite_source
    assert "tools/merge_funcov.py" in suite_source
    assert 'for funcov_bin_prefix in BIN-4 BIN-5; do' in suite_source
    assert "backannotation_audit_${funcov_bin_prefix}.json" in suite_source
    assert "code_coverage_summary.json" in suite_source
    assert "${SUITE_ID}_observed" in suite_source
    assert "--check" in suite_source
    assert 'raw coverage summary skipped: TB_RUN_DUT=0' in suite_source

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
        for item in FunctionalCoverageRecorder.from_pilot_csv(
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
    assert listed_stems - active_testcases
