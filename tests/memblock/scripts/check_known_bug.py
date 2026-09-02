#!/usr/bin/env python3
"""Check the split guest-fault sentinel before or after the RTL fix.

The same scenario is useful at both points in the bug lifecycle: an unpatched
RTL must report the historical GPA offset, while a repaired RTL must complete
with the independently walked GPA.  ``--expect`` makes either state explicit
for mutation testing; the default accepts either recognized state.
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path


def classify_output(returncode: int, output: str) -> str | None:
    """Return the recognized sentinel state, or ``None`` for an invalid run."""
    clean_failure = re.compile(
        r"MEMBLOCK_VECTOR_GUEST_FAULT_FAIL .*phase=exception-metadata .*"
        r"expected_gpaddr=0x94001800 actual_gpaddr=0x94001808 .*"
        r"expected_vs_nonleaf=1 actual_vs_nonleaf=1"
    )
    repaired_pass = re.compile(
        r"MEMBLOCK_VECTOR_GUEST_FAULT_PASS .*"
        r"vaddr=0x[0-9a-f]+ gpaddr=0x94001800 .*"
        r"rtl_sha256=[0-9a-f]{64}"
    )
    if returncode != 0 and clean_failure.search(output) is not None:
        return "clean-failure"
    if returncode == 0 and repaired_pass.search(output) is not None:
        return "repaired-pass"
    return None


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path, required=True)
    parser.add_argument(
        "--expect",
        choices=("auto", "clean-failure", "repaired-pass"),
        default="auto",
        help="require the unpatched failure, repaired pass, or either (default)",
    )
    args = parser.parse_args()
    completed = subprocess.run(
        [str(args.binary), "--test", "vector-guest-fault-split"],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    output = completed.stdout
    state = classify_output(completed.returncode, output)
    if state is None or (args.expect != "auto" and state != args.expect):
        print(
            "known-bug sentinel did not produce the expected state "
            f"(expected={args.expect}, observed={state or 'invalid'})",
            file=sys.stderr,
        )
        print(output, file=sys.stderr)
        return 1
    print(
        "MEMBLOCK_KNOWN_BUG_SENTINEL_PASS "
        f"state={state} expected={args.expect}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
