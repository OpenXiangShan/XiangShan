from pathlib import Path


def test_bin_trace_suite_uses_dedicated_active_list_and_pipeline_wrapper() -> None:
    frontend_root = Path(__file__).resolve().parents[1]
    suite_source = (frontend_root / "scripts/run_bin_trace_suite.sh").read_text(encoding="utf-8")

    assert "pass bin paths or --list-file <path>" in suite_source
    assert "--list-file" in suite_source
    assert "run_bin_trace_pipeline.sh" in suite_source
    assert 'TB_BIN_TRACE_SUITE_CONTINUE_ON_FAIL="${TB_BIN_TRACE_SUITE_CONTINUE_ON_FAIL:-0}"' in suite_source
