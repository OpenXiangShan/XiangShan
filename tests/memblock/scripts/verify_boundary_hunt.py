#!/usr/bin/env python3
"""Verify that a constrained-random VS-non-leaf boundary hunt found its oracle hit."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


class BoundaryHuntError(RuntimeError):
    pass


def verify_boundary_hunt(
    path: Path,
    *,
    min_seeds: int,
    transactions: int,
    expect: str = "historical-failure",
) -> dict[str, int | str]:
    if expect not in ("historical-failure", "repaired-all-pass"):
        raise BoundaryHuntError(f"unsupported boundary-hunt expectation: {expect}")
    document = json.loads(path.read_text(encoding="utf-8"))
    results = document.get("results")
    if not isinstance(results, list) or len(results) < min_seeds:
        raise BoundaryHuntError("boundary hunt has too few seed results")
    ordered = sorted(results, key=lambda result: int(result.get("seed", -1)))
    seeds = [result.get("seed") for result in ordered]
    if seeds != list(range(seeds[0], seeds[0] + len(seeds))):
        raise BoundaryHuntError("boundary hunt seeds are not continuous")
    total_failures = 0
    for result in ordered:
        if result.get("scenario") != "random-boundary-hunt":
            raise BoundaryHuntError("unexpected scenario in boundary hunt")
        if result.get("transactions") != transactions:
            raise BoundaryHuntError("boundary hunt transaction count mismatch")
        failures = result.get("failures")
        if not isinstance(failures, int) or failures < 0:
            raise BoundaryHuntError("boundary hunt has an invalid failure count")
        total_failures += failures
        expected_status = "fail" if failures else "pass"
        if result.get("status") != expected_status:
            raise BoundaryHuntError("boundary hunt status does not match failure count")
        returncode = result.get("returncode")
        if not isinstance(returncode, int) or isinstance(returncode, bool):
            raise BoundaryHuntError("boundary hunt has no integer return code")
        if (failures == 0 and returncode != 0) or (failures != 0 and returncode == 0):
            raise BoundaryHuntError("boundary hunt return code does not match status")
        if failures:
            output = result.get("output", "")
            if "MEMBLOCK_RANDOM_BOUNDARY_HUNT_SAMPLE_FAIL" not in output:
                raise BoundaryHuntError("boundary hunt omitted failing sample provenance")
    if expect == "historical-failure" and total_failures == 0:
        raise BoundaryHuntError("boundary hunt found no oracle failure across the campaign")
    if expect == "repaired-all-pass" and total_failures != 0:
        raise BoundaryHuntError(
            f"repaired boundary hunt still found {total_failures} oracle failures"
        )
    return {
        "seeds": len(ordered),
        "samples": sum(int(result["transactions"]) for result in ordered),
        "failures": total_failures,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True)
    parser.add_argument("--min-seeds", type=int, default=1)
    parser.add_argument("--transactions", type=int, required=True)
    parser.add_argument(
        "--expect",
        choices=("historical-failure", "repaired-all-pass"),
        default="historical-failure",
    )
    args = parser.parse_args()
    try:
        summary = verify_boundary_hunt(
            args.input,
            min_seeds=args.min_seeds,
            transactions=args.transactions,
            expect=args.expect,
        )
    except (OSError, json.JSONDecodeError, BoundaryHuntError, TypeError, ValueError) as error:
        print(f"verify_boundary_hunt.py: error: {error}", file=sys.stderr)
        return 1
    print(
        "MEMBLOCK_BOUNDARY_HUNT_VERIFY_PASS seeds={seeds} samples={samples} failures={failures}".format(
            **summary
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
