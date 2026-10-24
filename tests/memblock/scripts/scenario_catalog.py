#!/usr/bin/env python3
"""Validate the MemBlock scenario catalog and render its build/dispatch adapters."""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Any

DEFAULT_CATALOG = Path(__file__).resolve().parents[1] / "cpp/scenarios/catalog.json"
NAME = re.compile(r"[a-z][a-z0-9]*(?:-[a-z0-9]+)*\Z")
GROUP = re.compile(r"[a-z][a-z0-9_]*\Z")
CALL = re.compile(r"(run_[a-z0-9_]+)\(argc, argv((?:, (?:options|true|false))*)\)\Z")


class CatalogError(ValueError):
    pass


def validate_catalog(document: Any, source_dir: Path) -> list[dict[str, Any]]:
    if not isinstance(document, dict) or set(document) != {"schema_version", "scenarios"}:
        raise CatalogError("catalog requires schema_version and scenarios")
    if document["schema_version"] != 1:
        raise CatalogError("unsupported scenario catalog schema")
    entries = document["scenarios"]
    if not isinstance(entries, list) or not entries:
        raise CatalogError("scenario catalog cannot be empty")
    names: set[str] = set()
    targets: set[str] = set()
    orders: set[int] = set()
    modules: set[str] = set()
    allowed = {"name", "kind", "group", "call", "make_targets", "benchmark_order", "benchmark_actions"}
    required = {"name", "kind", "group", "call", "make_targets", "benchmark_order"}
    for entry in entries:
        if not isinstance(entry, dict) or not required <= set(entry) or set(entry) - allowed:
            raise CatalogError("invalid scenario fields")
        name, kind, group, call = (
            entry[field] for field in ("name", "kind", "group", "call")
        )
        if not isinstance(name, str) or not NAME.fullmatch(name):
            raise CatalogError(f"invalid scenario name: {name!r}")
        if name in names:
            raise CatalogError(f"duplicate scenario: {name}")
        names.add(name)
        if kind not in {"directed", "constrained-random"}:
            raise CatalogError(f"invalid scenario kind for {name}: {kind!r}")
        if not isinstance(group, str) or not GROUP.fullmatch(group):
            raise CatalogError(f"invalid scenario group: {group!r}")
        source = source_dir / f"{group}.inc"
        if not source.is_file():
            raise CatalogError(f"missing scenario module: {source}")
        modules.add(group)
        match = CALL.fullmatch(call) if isinstance(call, str) else None
        if not match:
            raise CatalogError(f"unsupported call expression for {name}: {call!r}")
        if not re.search(r"^int " + re.escape(match[1]) + r"\(", source.read_text(), re.MULTILINE):
            raise CatalogError(f"unknown scenario function {match[1]} in {source.name}")
        order = entry["benchmark_order"]
        if order is not None:
            if type(order) is not int or order < 0 or order in orders:
                raise CatalogError(f"invalid or duplicate benchmark order for {name}")
            orders.add(order)
        if "benchmark_actions" in entry:
            count = entry["benchmark_actions"]
            if type(count) is not int or count <= 0 or order is None:
                raise CatalogError(f"invalid benchmark actions for {name}")
        if not isinstance(entry["make_targets"], list):
            raise CatalogError(f"invalid make targets for {name}")
        for target in entry["make_targets"]:
            if not isinstance(target, dict) or set(target) != {"name", "args"}:
                raise CatalogError(f"invalid make target fields for {name}")
            target_name = target["name"]
            if not isinstance(target_name, str) or not NAME.fullmatch(target_name):
                raise CatalogError(f"invalid make target: {target_name!r}")
            if target_name in targets:
                raise CatalogError(f"duplicate make target: {target_name}")
            targets.add(target_name)
            if not isinstance(target["args"], str) or any(c in target["args"] for c in "\n\r\x00"):
                raise CatalogError(f"invalid make arguments for {target_name}")
    actual_modules = {path.stem for path in source_dir.glob("*.inc")}
    if actual_modules != modules:
        raise CatalogError("unregistered scenario modules: " + ", ".join(sorted(actual_modules - modules)))
    if orders != set(range(len(orders))):
        raise CatalogError("benchmark order must be contiguous from zero")
    return entries


def load_catalog(path: Path = DEFAULT_CATALOG) -> list[dict[str, Any]]:
    return validate_catalog(json.loads(path.read_text()), path.parent)


def benchmark_scenarios(entries: list[dict[str, Any]]) -> list[dict[str, Any]]:
    return sorted((entry for entry in entries if entry["benchmark_order"] is not None), key=lambda entry: entry["benchmark_order"])


def entry_symbol(entry: dict[str, Any]) -> str:
    return "scenario_" + entry["name"].replace("-", "_")


def render_make(entries: list[dict[str, Any]]) -> str:
    targets = [target["name"] for entry in entries for target in entry["make_targets"]]
    lines = ["# Generated by scripts/scenario_catalog.py; edit catalog.json instead.", ".PHONY: " + " ".join(targets), ""]
    for entry in entries:
        for target in entry["make_targets"]:
            args = " " + target["args"] if target["args"] else ""
            lines.extend((target["name"] + ": $(PICKER_TEST_BINARY)", "\t$(PICKER_TEST_BINARY) --test " + entry["name"] + args, ""))
    return "\n".join(lines)


def render_modules(entries: list[dict[str, Any]]) -> dict[str, str]:
    """Produce independent TUs; existing scenario bodies retain private linkage."""
    signature = "(int argc, char **argv, const Options &options)"
    declarations = ["// Generated from catalog.json; do not edit.", "#pragma once", '#include "shared.hpp"', "namespace memblock::scenarios {"]
    declarations.extend("int " + entry_symbol(entry) + signature + ";" for entry in entries)
    declarations.extend(("} // namespace memblock::scenarios", ""))
    files = {"generated_entries.hpp": "\n".join(declarations)}
    for group in sorted({entry["group"] for entry in entries}):
        lines = ["// Generated from catalog.json; do not edit.", '#include "generated_entries.hpp"', "namespace memblock::scenarios {", "namespace {", f'#include "{group}.inc"', "} // namespace", ""]
        for entry in entries:
            if entry["group"] == group:
                lines.extend(("int " + entry_symbol(entry) + signature, "{", "    return " + entry["call"] + ";", "}", ""))
        lines.extend(("} // namespace memblock::scenarios", ""))
        files[f"module_{group}.cpp"] = "\n".join(lines)
    lines = ["// Generated from catalog.json; do not edit.", '#include "registry.hpp"', '#include "generated_entries.hpp"', "namespace memblock::scenarios {", "namespace {", f"const std::array<ScenarioEntry, {len(entries)}> entries{{{{"]
    lines.extend('    {"' + entry["name"] + '", &' + entry_symbol(entry) + "}," for entry in entries)
    lines.extend(("}};", "} // namespace", "std::span<const ScenarioEntry> scenario_catalog() { return entries; }", "} // namespace memblock::scenarios", ""))
    files["registry.cpp"] = "\n".join(lines)
    return files


def write_modules(entries: list[dict[str, Any]], destination: Path) -> list[Path]:
    destination.mkdir(parents=True, exist_ok=True)
    outputs = []
    for name, content in render_modules(entries).items():
        path = destination / name
        if not path.exists() or path.read_text() != content:
            path.write_text(content)
        if path.suffix == ".cpp":
            outputs.append(path)
    return outputs


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("check", "generate-make", "list"))
    parser.add_argument("--catalog", type=Path, default=DEFAULT_CATALOG)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    try:
        entries = load_catalog(args.catalog)
        if args.action == "list":
            print("\n".join(entry["name"] for entry in entries))
        elif args.action == "generate-make":
            if args.output is None:
                raise CatalogError("--output is required")
            args.output.write_text(render_make(entries))
        elif args.output is not None and args.output.read_text() != render_make(entries):
            raise CatalogError(f"generated Make targets are stale: {args.output}")
    except (OSError, ValueError) as error:
        print(f"scenario_catalog.py: error: {error}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
