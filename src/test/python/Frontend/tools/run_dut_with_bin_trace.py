from __future__ import annotations

import argparse
import json
from pathlib import Path
import sys


_TOOLS_ROOT = Path(__file__).resolve().parent
_FRONTEND_ROOT = _TOOLS_ROOT.parent
_REPO_ROOT = _FRONTEND_ROOT.parents[3]
_PYLIB = _REPO_ROOT / "build-frontend" / "pylib"

for _path in (str(_FRONTEND_ROOT), str(_PYLIB)):
    if _path not in sys.path:
        sys.path.insert(0, _path)

from env.api import api_Frontend_load_golden_trace, api_Frontend_load_program_file
from env.frontend_env import FrontendEnv
from env.trace import GoldenTrace


def _parse_int(s: str) -> int:
    return int(str(s), 0)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Start DUT env, load bin and golden trace jsonl."
    )
    parser.add_argument("bin", help="Input bin path")
    parser.add_argument("trace_jsonl", help="Input trace json/jsonl path")
    parser.add_argument(
        "--base-addr",
        default="0x80000000",
        help="Program load base address (default: 0x80000000)",
    )
    parser.add_argument(
        "--start-pc",
        default=None,
        help="Reset PC / start PC for DUT (default: same as --base-addr)",
    )
    parser.add_argument(
        "--step-cycles",
        type=int,
        default=0,
        help="Optional extra env.step cycles after load",
    )
    parser.add_argument(
        "--trace-start-index",
        type=int,
        default=0,
        help="Start golden trace comparison from this jsonl index",
    )
    parser.add_argument(
        "--no-dut",
        action="store_true",
        help="Only validate bin/trace inputs without importing Frontend or starting DUT",
    )
    args = parser.parse_args(argv)

    bin_path = str(args.bin)
    trace_path = str(args.trace_jsonl)
    base_addr = _parse_int(str(args.base_addr))
    start_pc = base_addr if args.start_pc is None else _parse_int(str(args.start_pc))
    step_cycles = int(args.step_cycles)
    trace_start_index = int(args.trace_start_index)
    no_dut = bool(args.no_dut)

    bin_file = Path(bin_path)
    trace_file = Path(trace_path)
    if not bin_file.is_file():
        raise FileNotFoundError(f"bin not found: {bin_file}")
    if not trace_file.is_file():
        raise FileNotFoundError(f"trace not found: {trace_file}")

    trace = GoldenTrace.from_file(str(trace_file))
    if no_dut:
        out = {
            "mode": "check_only",
            "bin_path": str(bin_file),
            "trace_path": str(trace_file),
            "base_addr": int(base_addr),
            "start_pc": int(start_pc),
            "trace_start_index": int(trace_start_index),
            "bin_size": int(bin_file.stat().st_size),
            "trace_entries": int(len(trace.entries)),
            "first_pc": (None if not trace.entries else int(trace.entries[0].pc)),
        }
        print("[testbench] runner check result:")
        print(json.dumps(out, ensure_ascii=True, indent=2))
        return 0

    from Frontend import DUTFrontend

    dut = DUTFrontend()
    dut.InitClock("clock")
    try:
        env = FrontendEnv(dut)
        env.initialize(reset_vector=start_pc, bare_mode=True, reset_cycles=20)
        bin_size = int(api_Frontend_load_program_file(env, bin_path, base_addr))
        trace_entries = int(api_Frontend_load_golden_trace(env, trace_path, start_index=trace_start_index))
        if step_cycles > 0:
            env.step(step_cycles)
        out = {
            "bin_path": bin_path,
            "trace_path": trace_path,
            "base_addr": int(base_addr),
            "start_pc": int(start_pc),
            "trace_start_index": int(trace_start_index),
            "bin_size": int(bin_size),
            "trace_entries": int(trace_entries),
            "extra_step_cycles": int(step_cycles),
        }
        print("[testbench] dut load result:")
        print(json.dumps(out, ensure_ascii=True, indent=2))
    finally:
        dut.Finish()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
