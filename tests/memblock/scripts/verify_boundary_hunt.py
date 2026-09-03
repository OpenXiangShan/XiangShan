#!/usr/bin/env python3
"""Verify that a constrained-random VS-non-leaf boundary hunt found its oracle hit."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


class BoundaryHuntError(RuntimeError):
    pass


def _valid_sha256(value: object) -> bool:
    return (
        isinstance(value, str)
        and len(value) == 64
        and all(character in "0123456789abcdef" for character in value)
    )


def verify_boundary_hunt(
    path: Path,
    *,
    min_seeds: int,
    transactions: int,
    rtl_sha256: str | None = None,
    rtl_metadata: Path | None = None,
    require_failure: bool = True,
) -> dict[str, int | str]:
    document = json.loads(path.read_text(encoding="utf-8"))
    if rtl_metadata is not None:
        metadata = json.loads(rtl_metadata.read_text(encoding="utf-8"))
        metadata_hash = metadata.get("complete_rtl_sha256")
        if not _valid_sha256(metadata_hash):
            raise BoundaryHuntError("RTL metadata has no complete hash")
        rtl_sha256 = metadata_hash
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
        reported_hash = result.get("rtl_sha256")
        if rtl_sha256 is not None and reported_hash != rtl_sha256:
            raise BoundaryHuntError("boundary hunt RTL hash mismatch")
        if failures:
            output = result.get("output", "")
            if "MEMBLOCK_RANDOM_BOUNDARY_HUNT_SAMPLE_FAIL" not in output:
                raise BoundaryHuntError("boundary hunt omitted failing sample provenance")
    if require_failure and total_failures == 0:
        raise BoundaryHuntError("boundary hunt found no oracle failure across the campaign")
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
    parser.add_argument("--rtl-sha256")
    parser.add_argument("--rtl-metadata", type=Path)
    parser.add_argument(
        "--allow-all-pass",
        action="store_true",
        help="accept a fully passing campaign after the RTL defect is fixed",
    )
    args = parser.parse_args()
    try:
        summary = verify_boundary_hunt(
            args.input,
            min_seeds=args.min_seeds,
            transactions=args.transactions,
            rtl_sha256=args.rtl_sha256,
            rtl_metadata=args.rtl_metadata,
            require_failure=not args.allow_all_pass,
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
