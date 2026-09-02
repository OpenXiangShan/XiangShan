#!/usr/bin/env python3
"""Assemble the checked C++ harness around a compiled Picker DUT model."""

from __future__ import annotations

import argparse
import shutil
import sys
from pathlib import Path


class HarnessError(RuntimeError):
    pass


def copy_required(source: Path, destination: Path) -> None:
    if not source.is_file():
        raise HarnessError(f"required generated file is missing: {source}")
    shutil.copy2(source, destination)


def prepare(
    picker_output: Path,
    main_source: Path,
    environment_source: Path | None = None,
    defaults_source: Path | None = None,
) -> Path:
    picker_output = picker_output.resolve()
    compiled = picker_output / "build/UT_MemBlock"
    if not (compiled / "libUTMemBlock.so").is_file():
        raise HarnessError(
            f"compiled MemBlock model is missing under {compiled}; build the Picker model first"
        )
    target = picker_output / "UT_MemBlock"
    if target.exists():
        shutil.rmtree(target)
    shutil.copytree(compiled, target)

    for header in picker_output.glob("*.hpp"):
        shutil.copy2(header, target / header.name)
    copy_required(picker_output / "cpp/dut.cpp", target / "UT_MemBlock.cpp")
    copy_required(picker_output / "cpp/dut.hpp", target / "UT_MemBlock.hpp")
    copy_required(picker_output / "cpp/CMakeLists.txt", target / "CMakeLists.txt")
    copy_required(picker_output / "cpp/Makefile", target / "Makefile")
    copy_required(picker_output / "cpp/cmake/verilator.cmake", target / "MemBlock.cmake")
    main_source = main_source.resolve()
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
    copy_required(environment_source, target / "memblock_env.hpp")
    copy_required(defaults_source, target / "generated_port_defaults.hpp")
    return target


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--picker-output", type=Path, required=True)
    parser.add_argument("--main-source", type=Path, required=True)
    parser.add_argument("--environment-source", type=Path)
    parser.add_argument("--defaults-source", type=Path)
    args = parser.parse_args()
    try:
        print(
            prepare(
                args.picker_output,
                args.main_source,
                args.environment_source,
                args.defaults_source,
            )
        )
    except (OSError, HarnessError) as error:
        print(f"prepare_picker_harness.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
