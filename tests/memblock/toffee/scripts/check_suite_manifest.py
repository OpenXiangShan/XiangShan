#!/usr/bin/env python3
"""Check that every legacy MemBlock leaf has one explicit Toffee suite."""

from __future__ import annotations

import argparse
import ast
import json
import sys
from pathlib import Path
from typing import Any


PROJECT = Path(__file__).resolve().parents[1]
MEMBLOCK = PROJECT.parent
DEFAULT_MANIFEST = PROJECT / "suite_manifest.json"
DEFAULT_SCENARIOS = MEMBLOCK / "scripts" / "benchmark_tests.py"


class ManifestError(RuntimeError):
    pass


def read_scenarios(path: Path) -> tuple[str, ...]:
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    for node in tree.body:
        if not isinstance(node, ast.Assign):
            continue
        if any(
            isinstance(target, ast.Name) and target.id == "SCENARIOS"
            for target in node.targets
        ):
            value = ast.literal_eval(node.value)
            if not isinstance(value, tuple) or not all(isinstance(item, str) for item in value):
                break
            return value
    raise ManifestError(f"cannot find SCENARIOS tuple in {path}")


def load_manifest(path: Path) -> list[dict[str, Any]]:
    document = json.loads(path.read_text(encoding="utf-8"))
    if document.get("schema_version") != 1:
        raise ManifestError("unsupported Toffee suite manifest schema")
    suites = document.get("suites")
    if not isinstance(suites, list) or not suites:
        raise ManifestError("Toffee suite manifest has no suites")
    return suites


def suite_matches(suite: dict[str, Any], scenario: str) -> bool:
    exact = suite.get("exact", [])
    prefixes = suite.get("prefixes", [])
    if not isinstance(exact, list) or not isinstance(prefixes, list):
        raise ManifestError(f"suite {suite.get('name')} has invalid match fields")
    return scenario in exact or any(scenario.startswith(prefix) for prefix in prefixes)


def check_test_node(test_spec: str, marker: str) -> None:
    try:
        relative_path, function_name = test_spec.split("::", 1)
    except ValueError as error:
        raise ManifestError(f"invalid migrated test node: {test_spec}") from error
    test_path = PROJECT / relative_path
    tree = ast.parse(test_path.read_text(encoding="utf-8"), filename=str(test_path))
    function = next(
        (
            node
            for node in tree.body
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
            and node.name == function_name
        ),
        None,
    )
    if function is None:
        raise ManifestError(f"migrated test function is missing: {test_spec}")
    decorators = [ast.unparse(decorator) for decorator in function.decorator_list]
    if "toffee_test.testcase" not in decorators:
        raise ManifestError(f"migrated test is not a Toffee testcase: {test_spec}")
    if not any(marker in decorator for decorator in decorators):
        raise ManifestError(f"migrated test has no {marker} marker: {test_spec}")


def check(manifest_path: Path, scenario_path: Path, strict_complete: bool) -> tuple[int, int]:
    scenarios = read_scenarios(scenario_path)
    suites = load_manifest(manifest_path)
    names: set[str] = set()
    for suite in suites:
        name = suite.get("name")
        if not isinstance(name, str) or not name or name in names:
            raise ManifestError(f"invalid or duplicate suite name: {name!r}")
        names.add(name)
        status = suite.get("status")
        if status not in ("implemented", "partial", "planned"):
            raise ManifestError(f"suite {name} has invalid status {status!r}")
        if status in ("implemented", "partial"):
            tests = suite.get("tests")
            marker = suite.get("marker")
            if not isinstance(tests, str) or not isinstance(marker, str):
                raise ManifestError(f"active suite {name} has no tests or marker")
            test_dir = PROJECT / tests
            contents = "\n".join(
                path.read_text(encoding="utf-8") for path in test_dir.rglob("*.py")
            )
            if marker not in contents:
                raise ManifestError(f"suite {name} marker {marker} is not used in {test_dir}")

    assignments: dict[str, list[str]] = {}
    for scenario in scenarios:
        assignments[scenario] = [
            str(suite["name"]) for suite in suites if suite_matches(suite, scenario)
        ]
    missing = sorted(name for name, owners in assignments.items() if not owners)
    duplicate = sorted(name for name, owners in assignments.items() if len(owners) > 1)
    if missing:
        raise ManifestError(f"legacy scenarios without Toffee suite: {', '.join(missing)}")
    if duplicate:
        details = ", ".join(f"{name}={assignments[name]}" for name in duplicate)
        raise ManifestError(f"legacy scenarios assigned more than once: {details}")

    migrated_names: list[str] = []
    for suite in suites:
        migrated_nodes = suite.get("migrated_nodes", [])
        if not isinstance(migrated_nodes, list):
            raise ManifestError(f"suite {suite['name']} has invalid migrated_nodes")
        for node in migrated_nodes:
            if not isinstance(node, dict):
                raise ManifestError(f"suite {suite['name']} has invalid migrated node")
            scenario = node.get("scenario")
            test_spec = node.get("test")
            if not isinstance(scenario, str) or not isinstance(test_spec, str):
                raise ManifestError(f"suite {suite['name']} has incomplete migrated node")
            if scenario not in scenarios or not suite_matches(suite, scenario):
                raise ManifestError(
                    f"migrated scenario {scenario!r} is outside suite {suite['name']}"
                )
            check_test_node(test_spec, str(suite["marker"]))
            migrated_names.append(scenario)
        owned_count = sum(1 for scenario in scenarios if suite_matches(suite, scenario))
        if suite["status"] == "planned" and migrated_nodes:
            raise ManifestError(f"planned suite {suite['name']} has migrated nodes")
        if suite["status"] == "implemented" and len(migrated_nodes) != owned_count:
            raise ManifestError(f"implemented suite {suite['name']} is incomplete")
        if suite["status"] == "partial" and not 0 < len(migrated_nodes) < owned_count:
            raise ManifestError(f"partial suite {suite['name']} has invalid migration count")
    if len(migrated_names) != len(set(migrated_names)):
        raise ManifestError("a migrated scenario is listed by more than one suite")
    planned = len(scenarios) - len(migrated_names)
    migrated = len(migrated_names)
    if strict_complete and planned:
        raise ManifestError(f"Toffee migration is incomplete: {planned} scenarios planned")
    return migrated, len(scenarios)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    parser.add_argument("--scenarios", type=Path, default=DEFAULT_SCENARIOS)
    parser.add_argument("--strict-complete", action="store_true")
    args = parser.parse_args()
    try:
        migrated, total = check(args.manifest, args.scenarios, args.strict_complete)
    except (OSError, SyntaxError, ValueError, json.JSONDecodeError, ManifestError) as error:
        print(f"check_suite_manifest.py: error: {error}", file=sys.stderr)
        return 1
    print(f"TOFFEE_SUITE_MANIFEST_PASS migrated={migrated}/{total} planned={total-migrated}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
