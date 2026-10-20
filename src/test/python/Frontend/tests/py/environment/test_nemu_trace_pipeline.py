from __future__ import annotations

import pytest

from env.nemu import trace_pipeline


def test_empty_nemu_trace_fails_before_dut_run(tmp_path, monkeypatch) -> None:
    bin_path = tmp_path / "wrong_address.bin"
    bin_path.write_bytes(b"\x13\x00\x00\x00")
    log_path = tmp_path / "nemu.log"
    log_path.write_text("NEMU will start from pc 0x80000000\n", encoding="utf-8")
    monkeypatch.setattr(
        trace_pipeline,
        "run_nemu_for_log",
        lambda **kwargs: {"nemu_log_path": str(log_path)},
    )
    monkeypatch.setattr(trace_pipeline, "convert_nemu_log_file", lambda **kwargs: 0)

    with pytest.raises(ValueError, match="empty golden trace.*binary load address"):
        trace_pipeline.generate_nemu_trace_from_bin(
            str(bin_path), str(tmp_path / "trace.jsonl"), nemu_log_path=str(log_path)
        )
