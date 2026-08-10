#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Export the VCS VPI internal-signal inventory used by Frontend tests"
    )
    parser.add_argument("--pylib", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    return parser.parse_args()


def _public_signal_names(node: object, prefix: str = "") -> set[str]:
    if not isinstance(node, dict):
        return set()
    if node.get("_") is True:
        return {f"Frontend_top.{prefix.rstrip('_')}"}
    names: set[str] = set()
    for key, value in node.items():
        if key == "_":
            continue
        names.update(_public_signal_names(value, f"{prefix}{key}_"))
    return names


def _vtogcov_aliases(signals: set[str]) -> set[str]:
    aliases: set[str] = set()
    for name in signals:
        parent, separator, leaf = name.rpartition(".")
        if separator and leaf and not leaf.startswith("__Vtogcov__"):
            aliases.add(f"{parent}.__Vtogcov__{leaf}")
    return aliases


def main() -> int:
    args = _parse_args()
    pylib = args.pylib.resolve()
    if not (pylib / "Frontend" / "__init__.py").is_file():
        raise RuntimeError(f"Frontend VCS Python package not found: {pylib / 'Frontend'}")
    sys.path.insert(0, str(pylib))

    from Frontend import DUTFrontend

    dut = DUTFrontend()
    # VCS maps DUTFrontend.__del__ to the generated top-level $finish.  This
    # short-lived exporter must write the inventory before Python destroys dut.
    type(dut).__del__ = lambda _dut: None
    signals = {str(name) for name in dut.VPIInternalSignalList() if str(name)}
    if not signals:
        raise RuntimeError("VPI internal-signal inventory is empty; compile Frontend with --vpi")
    signal_map = pylib / "Frontend" / "signals.json"
    signals.update(_public_signal_names(json.loads(signal_map.read_text(encoding="utf-8"))))
    signals.update(_vtogcov_aliases(signals))

    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(
        "variables:\n" + "".join(f"  - name: {name}\n" for name in sorted(signals)),
        encoding="utf-8",
    )
    print(f"[frontend] VCS VPI signal inventory: {args.output} ({len(signals)} signals)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
