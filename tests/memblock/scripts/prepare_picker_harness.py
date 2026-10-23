#!/usr/bin/env python3
"""Assemble the checked C++ harness around a compiled Picker DUT model."""

from __future__ import annotations

import argparse
import json
import re
import shutil
import sys
from pathlib import Path
from typing import Sequence

import scenario_catalog


class HarnessError(RuntimeError):
    pass


def copy_required(source: Path, destination: Path) -> None:
    if not source.is_file():
        raise HarnessError(f"required generated file is missing: {source}")
    # Preserve timestamps on no-op preparation, but give changed bytes a new
    # destination timestamp even when a restored source has an older mtime.
    if not destination.is_file() or source.read_bytes() != destination.read_bytes():
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy(source, destination)


def sync_sources(source: Path, destination: Path, generated: set[str]) -> None:
    desired = {path.relative_to(source) for path in source.rglob("*") if path.is_file()}
    if destination.exists():
        for path in destination.rglob("*"):
            if path.is_file() and path.relative_to(destination) not in desired and path.relative_to(destination).as_posix() not in generated:
                path.unlink()
    for relative in sorted(desired):
        copy_required(source / relative, destination / relative)


def prepare(
    picker_output: Path,
    main_source: Path,
    environment_source: Path | None = None,
    defaults_source: Path | None = None,
    support_source_dirs: Sequence[Path] | None = None,
) -> Path:
    picker_output = picker_output.resolve()
    main_source = main_source.resolve()
    entries = scenario_catalog.load_catalog(
        main_source.parent / "scenarios/catalog.json"
    )
    compiled = picker_output / "build/UT_MemBlock"
    if not (compiled / "libUTMemBlock.so").is_file():
        raise HarnessError(
            f"compiled MemBlock model is missing under {compiled}; build the Picker model first"
        )
    target = picker_output / "UT_MemBlock"
    target.mkdir(parents=True, exist_ok=True)
    for artifact in compiled.iterdir():
        if artifact.is_file():
            copy_required(artifact, target / artifact.name)

    for header in picker_output.glob("*.hpp"):
        copy_required(header, target / header.name)
    copy_required(
        picker_output / "MemBlock_offset.yaml",
        target / "MemBlock_offset.yaml",
    )
    copy_required(picker_output / "cpp/dut.cpp", target / "UT_MemBlock.cpp")
    copy_required(picker_output / "cpp/dut.hpp", target / "UT_MemBlock.hpp")
    copy_required(picker_output / "cpp/Makefile", target / "Makefile")
    copy_required(picker_output / "cpp/cmake/verilator.cmake", target / "MemBlock.cmake")
    environment_source = (
        main_source.with_name("memblock_env.hpp")
        if environment_source is None
        else environment_source.resolve()
    )
    defaults_source = (
        main_source.with_name("generated_port_defaults.hpp")
        if defaults_source is None
        else defaults_source.resolve()
    )
    copy_required(main_source, target / "example.cpp")
    # Root headers are public harness interfaces. Discover them so new typed
    # interfaces do not need another hand-maintained packaging inventory.
    headers = sorted(main_source.parent.glob("*.hpp"))
    owned_headers = {header.name for header in headers} | {
        "memblock_env.hpp", "generated_port_defaults.hpp",
    }
    inventory = target / ".harness-root-headers.json"
    if inventory.exists():
        try:
            previous = json.loads(inventory.read_text())
        except ValueError as error:
            raise HarnessError("invalid copied root-header inventory") from error
        if not isinstance(previous, list) or any(
            not isinstance(name, str) or Path(name).name != name or not name.endswith(".hpp")
            for name in previous
        ):
            raise HarnessError("invalid copied root-header inventory")
        for name in set(previous) - owned_headers:
            (target / name).unlink(missing_ok=True)
    for header in headers:
        copy_required(header, target / header.name)
    copy_required(environment_source, target / "memblock_env.hpp")
    copy_required(defaults_source, target / "generated_port_defaults.hpp")
    content = json.dumps(sorted(owned_headers)) + "\n"
    if not inventory.exists() or inventory.read_text() != content:
        inventory.write_text(content)
    if support_source_dirs is None:
        support_source_dirs = [
            main_source.parent / "scenarios",
            main_source.parent / "environment",
            main_source.parent / "random",
        ]
    copied_names: set[str] = set()
    for source_dir in support_source_dirs:
        source_dir = source_dir.resolve()
        if not source_dir.is_dir():
            raise HarnessError(
                f"required support source directory is missing: {source_dir}"
            )
        if source_dir.name in copied_names:
            raise HarnessError(
                f"duplicate support source directory name: {source_dir.name}"
            )
        copied_names.add(source_dir.name)
        generated = set(scenario_catalog.render_modules(entries)) if source_dir.name == "scenarios" else set()
        sync_sources(source_dir, target / source_dir.name, generated)
    scenario_catalog.write_modules(entries, target / "scenarios")
    sources = sorted(
        str(path.relative_to(target))
        for directory in (target / "scenarios", target / "environment")
        for path in directory.glob("*.cpp")
    )
    if any(not re.fullmatch(r"[A-Za-z0-9_./-]+", source) for source in sources):
        raise HarnessError("unsupported CMake source filename")
    cmake_path = target / "CMakeLists.txt"
    cmake = (picker_output / "cpp/CMakeLists.txt").read_text()
    anchor = "add_library(example OBJECT example.cpp)"
    if cmake.count(anchor) != 1:
        raise HarnessError("expected one Picker example OBJECT target")
    content = cmake.replace(
            anchor,
            anchor + "\ntarget_sources(example PRIVATE\n"
            + "".join(f"    {source}\n" for source in sources)
            + ")",
        )
    if not cmake_path.exists() or cmake_path.read_text() != content:
        cmake_path.write_text(content)
    return target


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--picker-output", type=Path, required=True)
    parser.add_argument("--main-source", type=Path, required=True)
    parser.add_argument("--environment-source", type=Path)
    parser.add_argument("--defaults-source", type=Path)
    parser.add_argument("--support-source-dir", type=Path, action="append")
    args = parser.parse_args()
    try:
        print(
            prepare(
                args.picker_output,
                args.main_source,
                args.environment_source,
                args.defaults_source,
                args.support_source_dir,
            )
        )
    except (OSError, ValueError, HarnessError) as error:
        print(f"prepare_picker_harness.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
