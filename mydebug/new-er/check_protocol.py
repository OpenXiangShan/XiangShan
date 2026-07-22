#!/usr/bin/env python3

from pathlib import Path
import sys


ROOT = Path(__file__).resolve().parent
README = ROOT / "README.md"

REQUIRED_SNIPPETS = [
    "Read History First",
    "task tag",
    "all prior records for that task tag",
    "Do not bulk-read records from other tasks",
    "Prior records for this task",
    "Cross-task relevance",
    "Debug Record Location",
    "Hit-Good-Trap Rule",
    "Wave And Log Capture",
    "Per-Failure Template",
    "command",
    "configuration",
    "stdout",
    "stderr",
    "wave",
    "symptom",
    "hypothesis",
    "root cause",
    "fix",
    "validation",
    "next action",
    "hit-good-trap",
    "failed",
]


def main() -> int:
    if not README.is_file():
        print(f"missing protocol document: {README}", file=sys.stderr)
        return 1

    text = README.read_text(encoding="utf-8")
    missing = [snippet for snippet in REQUIRED_SNIPPETS if snippet not in text]
    if missing:
        print("protocol document is missing required text:", file=sys.stderr)
        for snippet in missing:
            print(f"  - {snippet}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
