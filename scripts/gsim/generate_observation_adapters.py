#!/usr/bin/env python3
import argparse
import re
from pathlib import Path


def generate(header, rtl_dir):
    declarations = set(re.findall(r"^void (\w+)\(([^\n]*)\);", header, re.MULTILINE))
    output = ["#include <cstdint>", ""]
    for name, arguments in sorted(declarations):
        if name.endswith("_constantinReader"):
            if arguments != "uint64_t& _0":
                raise ValueError(f"unsupported Constantin ABI: {name}({arguments})")
            reader = name.removesuffix("_constantinReader") + "_constantin_read"
            output.extend([
                f'extern "C" uint64_t {reader}();',
                f"void {name}({arguments}) {{ _0 = {reader}(); }}",
                "",
            ])
        elif name.endswith("Writer"):
            verilog = (rtl_dir / f"{name}.v").read_text()
            match = re.search(r'import "DPI-C" function void (\w+)\s*\((.*?)\);', verilog, re.DOTALL)
            if not match:
                raise ValueError(f"missing writer DPI declaration: {name}")
            writer, parameters = match.groups()
            columns = re.findall(r"input longint (\w+)", parameters)
            if not columns or columns[-1] != "stamp":
                raise ValueError(f"unsupported writer timestamp: {name}")
            columns = columns[:-1]
            types = [argument.strip() for argument in arguments.split(",")]
            if len(types) != len(columns) + 4 or types[:4] != ["const char* _0", "uint8_t _1", "uint8_t _2", "uint64_t _3"]:
                raise ValueError(f"unsupported writer ABI: {name}({arguments})")
            ports = re.findall(r"input(?:\s+\[[^\]]+\])?\s+(data_\w+)", verilog)
            if ports != [f"data_{column}" for column in columns]:
                raise ValueError(f"unsupported packed writer columns: {name}")
            dpi_arguments = [f"uint64_t {column}" for column in columns] + ["uint64_t stamp", "char* site"]
            call_arguments = [f"static_cast<uint64_t>(_{index})" for index in reversed(range(4, len(types)))]
            call_arguments.extend(["_3", "const_cast<char*>(_0)"])
            output.extend([
                f'extern "C" void {writer}({", ".join(dpi_arguments)});',
                f"void {name}({arguments}) {{",
                f"    if (_2 && !_1) {writer}({', '.join(call_arguments)});",
                "}",
                "",
            ])
    return "\n".join(output)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--header", type=Path, required=True)
    parser.add_argument("--rtl-dir", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    body = generate(args.header.read_text(), args.rtl_dir)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(body)
    print(f"Generated {args.output}")


if __name__ == "__main__":
    main()
