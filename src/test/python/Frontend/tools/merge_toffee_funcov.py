#!/usr/bin/env python3
from __future__ import annotations

import argparse
import sys
from pathlib import Path


_FRONTEND_ROOT = Path(__file__).resolve().parents[1]
if str(_FRONTEND_ROOT) not in sys.path:
    sys.path.insert(0, str(_FRONTEND_ROOT))

from env.funcov.toffee_artifact import merge_toffee_artifacts  # noqa: E402


def main() -> int:
    parser = argparse.ArgumentParser(description="Merge compatible Toffee funcov artifacts")
    parser.add_argument("--artifact", action="append", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()

    output = merge_toffee_artifacts(args.artifact, args.output)
    print(output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
