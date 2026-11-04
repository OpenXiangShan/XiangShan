"""Read native PERF without overwriting counters that share a printed name.

Repeated names retain every value. Scalar consumers use only unique names;
multiset comparison ignores print order but cannot restore missing entry IDs.
"""
import re
from pathlib import Path

PERF = re.compile(r'\[PERF\s*\]\[time=\s*(\d+)\] (.*): ([^,]+),\s*(-?\d+)')

def read_blocks(path):
    blocks = {}
    for match in PERF.finditer(Path(path).read_text()):
        cycle, module, name, value = match.groups()
        blocks.setdefault(int(cycle), {}).setdefault(module + '.' + name, []).append(int(value))
    return blocks

def unique_counters(block):
    return {name: values[0] for name, values in block.items() if len(values) == 1}

def counter_multisets(block):
    return {name: sorted(values) for name, values in block.items()}
