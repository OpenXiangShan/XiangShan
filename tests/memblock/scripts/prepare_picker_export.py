#!/usr/bin/env python3
"""Validate and prepare a generated Picker mem-direct export."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path


class PickerExportError(RuntimeError):
    pass


def prepare(picker_output: Path) -> Path:
    makefile = picker_output.resolve() / "Makefile"
    if not makefile.is_file():
        raise PickerExportError(f"generated Picker Makefile is missing: {makefile}")

    contents = makefile.read_text(encoding="utf-8")
    if "RW_TYPE := MEM_DIRECT" not in contents:
        raise PickerExportError("Picker export is not configured for MEM_DIRECT")

    lines = contents.splitlines(keepends=True)
    commands = [
        index
        for index, line in enumerate(lines)
        if "--source_module_name" in line and "--sim" in line
    ]
    if len(commands) != 1:
        raise PickerExportError(
            "expected exactly one nested mem-direct Picker command"
        )

    command_index = commands[0]
    command = lines[command_index]
    if "--language" not in command and "--lang" not in command:
        newline = "\n" if command.endswith("\n") else ""
        lines[command_index] = (
            command.removesuffix("\n") + " --language $(TLANG)" + newline
        )
        makefile.write_text("".join(lines), encoding="utf-8")
    return makefile


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--picker-output", type=Path, required=True)
    args = parser.parse_args()
    try:
        print(prepare(args.picker_output))
    except (OSError, PickerExportError) as error:
        print(f"prepare_picker_export.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
