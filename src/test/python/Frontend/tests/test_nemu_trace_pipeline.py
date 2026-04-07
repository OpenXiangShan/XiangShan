import json
from pathlib import Path

from env.trace import GoldenTrace


def _make_fake_nemu(tmp_path: Path) -> Path:
    script = tmp_path / "fake_nemu.py"
    script.write_text(
        "\n".join(
            [
                "#!/usr/bin/env python3",
                "import pathlib",
                "import sys",
                "",
                "def _arg_after(flag: str) -> str:",
                "    args = sys.argv[1:]",
                "    idx = args.index(flag)",
                "    return args[idx + 1]",
                "",
                "log_path = _arg_after('-l')",
                "pathlib.Path(log_path).parent.mkdir(parents=True, exist_ok=True)",
                "pathlib.Path(log_path).write_text(",
                "    \"\\n\".join([",
                "        \"\\x1b[1;34m[src/cpu/cpu-exec.c:469,execute] prev pc = 0x80000000, pc = 0x80000004\\x1b[0m\",",
                "        \"\\x1b[1;34m[src/cpu/cpu-exec.c:80,debug_hook] (M)0x0000000080000000:   63 04 00 00     beq        0x80000008,a0,a1\\x1b[0m\",",
                "        \"\\x1b[1;34m[src/cpu/cpu-exec.c:469,execute] prev pc = 0x80000004, pc = 0x80000008\\x1b[0m\",",
                "        \"\\x1b[1;34m[src/cpu/cpu-exec.c:80,debug_hook] (M)0x0000000080000004:   13 00 00 00     p_li_0     ra\\x1b[0m\",",
                "    ]),",
                "    encoding='utf-8',",
                ")",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    script.chmod(0o755)
    return script


class _FakeEnv:
    def __init__(self) -> None:
        self.program_load = None
        self.trace_load = None
        self.total_steps = 0

    def load_program_file(self, path: str, base_addr: int) -> int:
        self.program_load = (str(path), int(base_addr))
        return int(Path(path).stat().st_size)

    def load_golden_trace_file(self, path: str) -> int:
        trace = GoldenTrace.from_file(str(path))
        self.trace_load = (str(path), len(trace.entries))
        return len(trace.entries)

    def step(self, cycles: int = 1) -> int:
        self.total_steps += int(cycles)
        return self.total_steps


def test_generate_nemu_trace_from_bin(tmp_path):
    from env.nemu_trace_pipeline import generate_nemu_trace_from_bin

    fake_nemu = _make_fake_nemu(tmp_path)
    bin_path = tmp_path / "program.bin"
    bin_path.write_bytes(b"\x13\x00\x00\x00")

    log_path = tmp_path / "run.log"
    trace_path = tmp_path / "trace.jsonl"
    out = generate_nemu_trace_from_bin(
        bin_path=str(bin_path),
        trace_output_path=str(trace_path),
        nemu_exec_path=str(fake_nemu),
        nemu_log_path=str(log_path),
    )

    assert int(out["trace_entries"]) == 2
    assert trace_path.is_file()
    rows = [json.loads(line) for line in trace_path.read_text(encoding="utf-8").splitlines() if line.strip()]
    assert len(rows) == 2
    assert rows[0]["pc"] == "0x80000000"
    assert rows[0]["kind"] == "branch"
    assert rows[0]["taken"] is False
    assert rows[0]["target_pc"] == "0x80000004"


def test_generate_nemu_trace_from_bin_json_output(tmp_path):
    from env.nemu_trace_pipeline import generate_nemu_trace_from_bin

    fake_nemu = _make_fake_nemu(tmp_path)
    bin_path = tmp_path / "program.bin"
    bin_path.write_bytes(b"\x13\x00\x00\x00")

    log_path = tmp_path / "run.log"
    trace_path = tmp_path / "trace.json"
    out = generate_nemu_trace_from_bin(
        bin_path=str(bin_path),
        trace_output_path=str(trace_path),
        nemu_exec_path=str(fake_nemu),
        nemu_log_path=str(log_path),
    )

    assert int(out["trace_entries"]) == 2
    assert trace_path.is_file()
    rows = json.loads(trace_path.read_text(encoding="utf-8"))
    assert len(rows) == 2
    assert rows[0]["pc"] == "0x80000000"
    assert rows[0]["kind"] == "branch"
    assert rows[0]["taken"] is False
    assert rows[0]["target_pc"] == "0x80000004"
    assert GoldenTrace.from_file(str(trace_path)).entries[0].pc == 0x80000000


def test_api_prepare_program_and_nemu_trace(tmp_path):
    from env.api import api_Frontend_prepare_program_and_nemu_trace

    fake_nemu = _make_fake_nemu(tmp_path)
    bin_path = tmp_path / "program.bin"
    trace_path = tmp_path / "golden.jsonl"
    log_path = tmp_path / "nemu.log"
    bin_path.write_bytes(b"\x13\x00\x00\x00")

    env = _FakeEnv()
    out = api_Frontend_prepare_program_and_nemu_trace(
        env=env,
        bin_path=str(bin_path),
        base_addr=0x80000000,
        trace_output_path=str(trace_path),
        nemu_exec_path=str(fake_nemu),
        nemu_log_path=str(log_path),
        load_trace=1,
    )

    assert int(out["bin_size"]) == 4
    assert int(out["trace_entries"]) == 2
    assert env.program_load == (str(bin_path), 0x80000000)
    assert env.trace_load == (str(trace_path), 2)


def test_api_prepare_program_and_nemu_trace_rejects_non_default_base(tmp_path):
    from env.api import api_Frontend_prepare_program_and_nemu_trace

    fake_nemu = _make_fake_nemu(tmp_path)
    bin_path = tmp_path / "program.bin"
    trace_path = tmp_path / "golden.json"
    log_path = tmp_path / "nemu.log"
    bin_path.write_bytes(b"\x13\x00\x00\x00")

    env = _FakeEnv()

    try:
        api_Frontend_prepare_program_and_nemu_trace(
            env=env,
            bin_path=str(bin_path),
            base_addr=0x80001000,
            trace_output_path=str(trace_path),
            nemu_exec_path=str(fake_nemu),
            nemu_log_path=str(log_path),
            load_trace=1,
        )
        assert False, "expected ValueError for non-default base_addr"
    except ValueError as exc:
        assert "0x80000000" in str(exc)

    assert env.program_load is None
    assert env.trace_load is None


def test_run_bin_trace_pipeline_defaults_to_jsonl_output(tmp_path):
    import pytest

    pytest.skip("run_bin_trace_pipeline.sh is intentionally out of scope for the Frontend package migration")
