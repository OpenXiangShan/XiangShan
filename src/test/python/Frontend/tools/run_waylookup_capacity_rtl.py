"""Build a directed module test from existing generated RTL; never edit Scala.

Results are explicitly standalone RTL evidence, not FrontendTop funcov artifacts.
"""

import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rtl", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    rtl = args.rtl.resolve()
    source = rtl.read_text()
    template = Path(__file__).resolve().parents[1] / "tests/sv/icache_waylookup_capacity_tb.sv"
    body = template.read_text()
    module = re.search(r"module ICacheWayLookup\((.*?)\n\);", source, re.S)
    if module is None:
        raise RuntimeError("unsupported WayLookup module declaration")
    driven = set(re.findall(r"(?:reg|wire)\s+(?:\[[^]]+\]\s*)?(\w+)\s*[=;]", body))
    ports = re.findall(r"\b(input|output)\s+(?:\[[^]]+\]\s*)?(\w+)\s*[,\n]", module.group(1) + "\n")
    connections = []
    for direction, name in ports:
        value = name if name in driven else "'0" if direction == "input" else ""
        connections.append(f".{name}({value})")
    body = body.replace("/* DUT_CONNECTIONS */", "ICacheWayLookup dut (" + ",\n".join(connections) + ");")
    verilator = shutil.which("verilator")
    if verilator is None:
        raise RuntimeError("verilator executable not found")
    with tempfile.TemporaryDirectory(prefix="waylookup-rtl-") as build:
        work = Path(build)
        tb = work / template.name
        tb.write_text(body)
        command = [verilator, "--binary", "--timing", "--assert", "-Wno-fatal", "--top-module",
                   "icache_waylookup_capacity_tb", "--Mdir", str(work / "obj"), "-j", "2", str(rtl), str(tb)]
        compiled = subprocess.run(command, capture_output=True, text=True)
        if compiled.returncode:
            raise RuntimeError(compiled.stdout + compiled.stderr)
        run = subprocess.run([str(work / "obj/Vicache_waylookup_capacity_tb")], capture_output=True, text=True)
        report = {"scope": "standalone_ICacheWayLookup", "frontend_top_hit_eligible": False,
                  "rtl": str(rtl), "rtl_sha256": hashlib.sha256(rtl.read_bytes()).hexdigest(),
                  "testbench_sha256": hashlib.sha256(template.read_bytes()).hexdigest(),
                  "exit_code": run.returncode, "output": run.stdout + run.stderr,
                  "checks": re.findall(r"^PASS (.+)$", run.stdout, re.M)}
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n")
        print(report["output"], end="")
        if run.returncode:
            raise SystemExit(run.returncode)


if __name__ == "__main__":
    main()
