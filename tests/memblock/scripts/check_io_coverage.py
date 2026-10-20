#!/usr/bin/env python3
"""Check or refresh the generated interface inventory in IO_COVERAGE.md."""

from __future__ import annotations

import argparse
import json
import re
import sys
from collections import Counter
from pathlib import Path


BEGIN_MARKER = "<!-- BEGIN GENERATED INTERFACE INVENTORY -->"
END_MARKER = "<!-- END GENERATED INTERFACE INVENTORY -->"
SCHEMA_RE = re.compile(r"constraint_schema=(?P<schema>[0-9]+)")


class CoverageDocError(RuntimeError):
    pass


def inventory_markdown(manifest: dict[str, object]) -> str:
    ports = manifest.get("ports")
    if not isinstance(ports, list):
        raise CoverageDocError("port manifest has no port list")
    counts: dict[str, Counter[str]] = {}
    total = Counter()
    for port in ports:
        if not isinstance(port, dict):
            raise CoverageDocError("port manifest contains a non-object port")
        group = port.get("group")
        direction = port.get("direction")
        if not isinstance(group, str) or direction not in ("input", "output"):
            raise CoverageDocError("port manifest has invalid group or direction")
        group_counts = counts.setdefault(group, Counter())
        group_counts[direction] += 1
        total[direction] += 1

    lines = [
        BEGIN_MARKER,
        "| Interface group | Ports | Inputs | Outputs |",
        "| --- | ---: | ---: | ---: |",
    ]
    for group in sorted(counts):
        group_counts = counts[group]
        port_count = group_counts["input"] + group_counts["output"]
        lines.append(
            f"| `{group}` | {port_count} | {group_counts['input']} | "
            f"{group_counts['output']} |"
        )
    total_ports = total["input"] + total["output"]
    lines.extend(
        [
            f"| **Total** | **{total_ports}** | **{total['input']}** | "
            f"**{total['output']}** |",
            END_MARKER,
        ]
    )
    return "\n".join(lines)


def replace_inventory(document: str, inventory: str) -> str:
    start = document.find(BEGIN_MARKER)
    end = document.find(END_MARKER)
    if start < 0 or end < start or document.find(BEGIN_MARKER, start + 1) >= 0:
        raise CoverageDocError("coverage document has malformed inventory markers")
    end += len(END_MARKER)
    return document[:start] + inventory + document[end:]


def constraints_schema(path: Path) -> str:
    match = SCHEMA_RE.search(path.read_text(encoding="utf-8"))
    if match is None:
        raise CoverageDocError(f"constraint schema is absent from {path}")
    return match.group("schema")


def check(
    manifest_path: Path,
    document_path: Path,
    constraints_path: Path,
    related_documents: tuple[Path, ...] = (),
) -> None:
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    document = document_path.read_text(encoding="utf-8")
    expected = inventory_markdown(manifest)
    start = document.find(BEGIN_MARKER)
    end = document.find(END_MARKER)
    if start < 0 or end < start:
        raise CoverageDocError("coverage document has no generated inventory block")
    end += len(END_MARKER)
    if document[start:end] != expected:
        raise CoverageDocError(
            "IO coverage inventory is stale; run check_io_coverage.py generate"
        )
    schema = constraints_schema(constraints_path)
    if f"constraint_schema={schema}" not in document:
        raise CoverageDocError(
            f"IO coverage document does not record constraint_schema={schema}"
        )
    for related_path in related_documents:
        related = related_path.read_text(encoding="utf-8")
        recorded = SCHEMA_RE.findall(related)
        if any(value != schema for value in recorded):
            raise CoverageDocError(
                f"{related_path} records a stale constraint schema: {recorded}"
            )
        if "--rtl-metadata" in related:
            raise CoverageDocError(
                f"{related_path} contains the removed --rtl-metadata option"
            )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("check", "generate"))
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--doc", type=Path, required=True)
    parser.add_argument("--constraints", type=Path, required=True)
    parser.add_argument(
        "--related-doc", type=Path, action="append", default=[]
    )
    args = parser.parse_args()
    try:
        if args.command == "generate":
            manifest = json.loads(args.manifest.read_text(encoding="utf-8"))
            document = args.doc.read_text(encoding="utf-8")
            args.doc.write_text(
                replace_inventory(document, inventory_markdown(manifest)),
                encoding="utf-8",
            )
        check(args.manifest, args.doc, args.constraints, tuple(args.related_doc))
    except (OSError, json.JSONDecodeError, CoverageDocError) as error:
        print(f"check_io_coverage.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
