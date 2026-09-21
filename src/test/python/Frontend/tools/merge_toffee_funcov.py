#!/usr/bin/env python3
from __future__ import annotations

import argparse
import sys
from pathlib import Path


_FRONTEND_ROOT = Path(__file__).resolve().parents[1]
if str(_FRONTEND_ROOT) not in sys.path:
    sys.path.insert(0, str(_FRONTEND_ROOT))

from env.funcov.toffee_bridge import merge_native_toffee_reports  # noqa: E402


def main() -> int:
    parser = argparse.ArgumentParser(description="Merge native Toffee funcov reports")
    parser.add_argument("--artifact", action="append", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()

    output = merge_native_toffee_reports(args.artifact, args.output)
    print(output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
