"""Run NEMU and materialize trace files for the frontend env.

This module stays in ``env`` during the current refactor phase. The
package split did not require a dedicated ``tools/`` package yet, so Task 9
documents the decision and leaves the file in place.
"""

from __future__ import annotations

import argparse
import subprocess
from pathlib import Path
from typing import Dict, List, Optional

from .nemu_trace_converter import convert_nemu_log_file


def _repo_root() -> Path:
    return Path(__file__).resolve().parents[5]


def _default_nemu_exec_path() -> Path:
    return _repo_root() / "NEMU" / "build" / "riscv64-nemu-interpreter"


def _default_nemu_log_path(trace_output_path: str) -> Path:
    out = Path(trace_output_path)
    return out.parent / f"{out.stem}.nemu.log"


def _build_nemu_cmd(
    nemu_exec_path: str,
    bin_path: str,
    nemu_log_path: str,
    nemu_max_instr: int = 0,
) -> List[str]:
    # Keep argument order aligned with expected invocation:
    # ./NEMU/build/riscv64-nemu-interpreter -b <bin> -l <log>
    cmd = [str(nemu_exec_path), "-b", str(bin_path), "-l", str(nemu_log_path)]
    if int(nemu_max_instr) > 0:
        cmd.extend(["-I", str(int(nemu_max_instr))])
    return cmd


def run_nemu_for_log(
    bin_path: str,
    nemu_log_path: str,
    nemu_exec_path: Optional[str] = None,
    nemu_max_instr: int = 0,
) -> Dict:
    bin_file = Path(bin_path)
    if not bin_file.is_file():
        raise FileNotFoundError(f"bin not found: {bin_file}")

    exec_path = Path(nemu_exec_path) if nemu_exec_path else _default_nemu_exec_path()
    if not exec_path.is_file():
        raise FileNotFoundError(f"NEMU executable not found: {exec_path}")

    log_file = Path(nemu_log_path)
    log_file.parent.mkdir(parents=True, exist_ok=True)

    cmd = _build_nemu_cmd(
        nemu_exec_path=str(exec_path),
        bin_path=str(bin_file),
        nemu_log_path=str(log_file),
        nemu_max_instr=int(nemu_max_instr),
    )
    result = subprocess.run(cmd, capture_output=True, text=True, check=False)
    if int(result.returncode) != 0:
        raise RuntimeError(
            "NEMU run failed: "
            f"returncode={result.returncode}, cmd={' '.join(cmd)}, "
            f"stdout={result.stdout[-1000:]}, stderr={result.stderr[-1000:]}"
        )

    return {
        "bin_path": str(bin_file),
        "nemu_exec_path": str(exec_path),
        "nemu_log_path": str(log_file),
        "nemu_cmd": cmd,
    }


def generate_nemu_trace_from_bin(
    bin_path: str,
    trace_output_path: str,
    nemu_exec_path: Optional[str] = None,
    nemu_log_path: Optional[str] = None,
    nemu_max_instr: int = 0,
    trace_limit: int = 0,
) -> Dict:
    trace_file = Path(trace_output_path)
    trace_file.parent.mkdir(parents=True, exist_ok=True)
    log_path = Path(nemu_log_path) if nemu_log_path else _default_nemu_log_path(str(trace_file))

    run_out = run_nemu_for_log(
        bin_path=str(bin_path),
        nemu_log_path=str(log_path),
        nemu_exec_path=nemu_exec_path,
        nemu_max_instr=int(nemu_max_instr),
    )
    trace_entries = int(
        convert_nemu_log_file(
            input_path=str(log_path),
            output_path=str(trace_file),
            limit=int(trace_limit),
        )
    )

    out = dict(run_out)
    out["trace_output_path"] = str(trace_file)
    out["trace_entries"] = trace_entries
    return out


def main(argv: Optional[List[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        description="Run NEMU with a real bin and convert fetch_decode log to GoldenTrace JSON/JSONL."
    )
    parser.add_argument("bin", help="Input binary image path")
    parser.add_argument("output", help="Output JSON/JSONL trace path")
    parser.add_argument("--nemu-exec", default="", help="Path to NEMU executable")
    parser.add_argument("--nemu-log", default="", help="Path to NEMU raw log")
    parser.add_argument("--nemu-max-instr", type=int, default=0, help="Pass -I to NEMU when > 0")
    parser.add_argument("--trace-limit", type=int, default=0, help="Max output trace entries")
    args = parser.parse_args(argv)

    out = generate_nemu_trace_from_bin(
        bin_path=str(args.bin),
        trace_output_path=str(args.output),
        nemu_exec_path=(None if not args.nemu_exec else str(args.nemu_exec)),
        nemu_log_path=(None if not args.nemu_log else str(args.nemu_log)),
        nemu_max_instr=int(args.nemu_max_instr),
        trace_limit=int(args.trace_limit),
    )
    print(
        f"trace generated: entries={int(out['trace_entries'])} "
        f"log={out['nemu_log_path']} output={out['trace_output_path']}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
