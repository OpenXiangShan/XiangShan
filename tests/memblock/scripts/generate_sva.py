#!/usr/bin/env python3
"""Generate Verilator-compatible ready/valid stability assertions."""

from __future__ import annotations

import argparse
import json
import re
import sys
from collections import defaultdict
from pathlib import Path
from typing import Any


class SvaGenerationError(RuntimeError):
    pass


def identifier(value: str) -> str:
    return re.sub(r"[^A-Za-z0-9_$]", "_", value)


def producer_interfaces(manifest: dict[str, Any]) -> list[dict[str, Any]]:
    bundles: dict[tuple[str, str, str | None], list[dict[str, Any]]] = defaultdict(list)
    for port in manifest["ports"]:
        if port["protocol"] not in ("decoupled", "tilelink") or not port["interface"]:
            continue
        key = (port["protocol"], port["interface"], port["channel"])
        bundles[key].append(port)

    producers: list[dict[str, Any]] = []
    for key, ports in sorted(bundles.items()):
        valid = [port for port in ports if port["role"] == "valid"]
        ready = [port for port in ports if port["role"] == "ready"]
        payload = [port for port in ports if port["role"] == "payload"]
        if len(valid) != 1 or len(ready) != 1:
            continue
        if valid[0]["direction"] != "output" or ready[0]["direction"] != "input":
            continue
        if any(port["direction"] != "output" for port in payload):
            raise SvaGenerationError(f"mixed payload directions for producer {key}")
        payload_bits = sum(port["width"] or 0 for port in payload)
        if not payload or not payload_bits or any(port["width"] is None for port in payload):
            raise SvaGenerationError(f"producer {key} has no statically sized payload")
        protocol, interface, channel = key
        display_name = interface + (f"_{channel}" if channel else "")
        producers.append(
            {
                "name": display_name,
                "protocol": protocol,
                "valid": valid[0]["name"],
                "ready": ready[0]["name"],
                "payload": [port["name"] for port in payload],
                "width": payload_bits,
            }
        )
    return producers


def store_tlb_miss_preserve_assertions(store_queue_entries: int) -> list[str]:
    lines = [
        "module memblock_store_tlb_miss_preserve_assertion #(",
        "  parameter logic [5:0] SQ_INDEX = 6'd0",
        ") (",
        "  input logic       clock,",
        "  input logic       reset,",
        "  input logic       lane0_valid,",
        "  input logic       lane0_update,",
        "  input logic       lane0_miss,",
        "  input logic [5:0] lane0_sq_idx,",
        "  input logic       lane1_valid,",
        "  input logic       lane1_update,",
        "  input logic       lane1_miss,",
        "  input logic [5:0] lane1_sq_idx,",
        "  input logic       allocated,",
        "  input logic       addr_valid,",
        "  input logic       stored_rob_flag,",
        "  input logic [7:0] stored_rob_value,",
        "  input logic [6:0] stored_uop_idx",
        ");",
        "  logic check_next;",
        "  logic previous_rob_flag;",
        "  logic [7:0] previous_rob_value;",
        "  logic [6:0] previous_uop_idx;",
        "  always_ff @(posedge clock) begin",
        "    if (reset) begin",
        "      check_next <= 1'b0;",
        "      previous_rob_flag <= 1'b0;",
        "      previous_rob_value <= '0;",
        "      previous_uop_idx <= '0;",
        "    end",
        "    else begin",
        "      if (check_next) begin",
        "        assert (!allocated ||",
        "                stored_rob_flag != previous_rob_flag ||",
        "                stored_rob_value != previous_rob_value ||",
        "                stored_uop_idx != previous_uop_idx ||",
        "                addr_valid)",
        "          else $error(\"StoreQueue entry %0d lost addrvalid on TLB miss\", SQ_INDEX);",
        "      end",
        "      check_next <= allocated && addr_valid && (",
        "        lane0_valid && lane0_update && lane0_miss && lane0_sq_idx == SQ_INDEX",
        "        || lane1_valid && lane1_update && lane1_miss && lane1_sq_idx == SQ_INDEX",
        "      );",
        "      previous_rob_flag <= stored_rob_flag;",
        "      previous_rob_value <= stored_rob_value;",
        "      previous_uop_idx <= stored_uop_idx;",
        "    end",
        "  end",
        "endmodule",
        "",
    ]
    for index in range(store_queue_entries):
        lines.extend(
            [
                "bind StoreQueue memblock_store_tlb_miss_preserve_assertion #(",
                f"  .SQ_INDEX(6'd{index})",
                f") memblock_store_tlb_miss_preserve_{index} (",
                "  .clock(clock),",
                "  .reset(reset),",
                "  .lane0_valid(io_storeAddrIn_0_valid),",
                "  .lane0_update(io_storeAddrIn_0_bits_updateAddrValid),",
                "  .lane0_miss(io_storeAddrIn_0_bits_miss),",
                "  .lane0_sq_idx(io_storeAddrIn_0_bits_uop_sqIdx_value),",
                "  .lane1_valid(io_storeAddrIn_1_valid),",
                "  .lane1_update(io_storeAddrIn_1_bits_updateAddrValid),",
                "  .lane1_miss(io_storeAddrIn_1_bits_miss),",
                "  .lane1_sq_idx(io_storeAddrIn_1_bits_uop_sqIdx_value),",
                f"  .allocated(allocated_{index}),",
                f"  .addr_valid(addrvalid_{index}),",
                f"  .stored_rob_flag(uop_{index}_robIdx_flag),",
                f"  .stored_rob_value(uop_{index}_robIdx_value),",
                f"  .stored_uop_idx(uop_{index}_uopIdx)",
                ");",
                "",
            ]
        )
    return lines


def store_queue_entries(manifest: dict[str, Any]) -> int:
    try:
        value = manifest["dimensions"]["queue"]["store_entries"]
    except (KeyError, TypeError) as error:
        raise SvaGenerationError(
            "manifest has no queue.store_entries dimension"
        ) from error
    if not isinstance(value, int) or isinstance(value, bool) or value < 1:
        raise SvaGenerationError(
            "manifest queue.store_entries must be a positive integer"
        )
    return value


def render(manifest: dict[str, Any]) -> str:
    producers = producer_interfaces(manifest)
    lines = [
        "// Generated by scripts/generate_sva.py; do not edit manually.",
        f"// MemBlock RTL SHA-256: {manifest.get('complete_rtl_sha256', manifest['rtl_sha256'])}",
        "",
        "`ifndef SYNTHESIS",
        "module memblock_stability_assertion #(",
        "  parameter integer WIDTH = 1,",
        "  parameter integer ASSERTION_ID = 0",
        ") (",
        "  input logic                 clock,",
        "  input logic                 reset,",
        "  input logic                 valid,",
        "  input logic                 ready,",
        "  input logic [WIDTH-1:0]     payload",
        ");",
        "  logic                 previous_stalled;",
        "  logic [WIDTH-1:0]     previous_payload;",
        "",
        "  always_ff @(posedge clock) begin",
        "    if (reset) begin",
        "      previous_stalled <= 1'b0;",
        "      previous_payload <= '0;",
        "    end",
        "    else begin",
        "      if (previous_stalled) begin",
        "        assert (valid)",
        "          else $error(\"MemBlock producer %0d dropped valid while stalled\", ASSERTION_ID);",
        "        assert (payload == previous_payload)",
        "          else $error(\"MemBlock producer %0d changed payload while stalled\", ASSERTION_ID);",
        "      end",
        "      previous_stalled <= valid && !ready;",
        "      previous_payload <= payload;",
        "    end",
        "  end",
        "endmodule",
        "",
    ]
    for assertion_id, producer in enumerate(producers):
        instance = identifier("memblock_stable_" + producer["name"])
        lines.extend(
            [
                f"// {assertion_id}: {producer['protocol']} {producer['name']}",
                "bind MemBlock memblock_stability_assertion #(",
                f"  .WIDTH({producer['width']}),",
                f"  .ASSERTION_ID({assertion_id})",
                f") {instance} (",
                "  .clock(clock),",
                "  .reset(reset),",
                f"  .valid({producer['valid']}),",
                f"  .ready({producer['ready']}),",
                "  .payload({",
            ]
        )
        for index, payload in enumerate(producer["payload"]):
            comma = "," if index + 1 < len(producer["payload"]) else ""
            lines.append(f"    {payload}{comma}")
        lines.extend(["  })", ");", ""])
    lines.extend(store_tlb_miss_preserve_assertions(store_queue_entries(manifest)))
    lines.extend(["`endif", ""])
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("generate", "check"))
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    try:
        manifest = json.loads(args.manifest.read_text(encoding="utf-8"))
        content = render(manifest)
        if args.action == "generate":
            args.output.parent.mkdir(parents=True, exist_ok=True)
            if not args.output.exists() or args.output.read_text(encoding="utf-8") != content:
                args.output.write_text(content, encoding="utf-8")
        elif not args.output.exists() or args.output.read_text(encoding="utf-8") != content:
            raise SvaGenerationError(
                f"stale assertion file {args.output}; regenerate and review the diff"
            )
    except (OSError, json.JSONDecodeError, KeyError, SvaGenerationError) as error:
        print(f"generate_sva.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
