from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Tuple


def _frontend_root() -> Path:
    return Path(__file__).resolve().parents[2]


def default_pilot_csv_path() -> Path:
    return (
        _frontend_root()
        / "docs"
        / "03_funcov_model"
        / "frontend_bt_functional_coverage_pilot.csv"
    )


def decode_signal_inventory_name(value: str) -> str:
    raw = str(value).strip()
    if len(raw) >= 2 and raw[0] == raw[-1] == '"':
        try:
            return str(json.loads(raw))
        except ValueError:
            return raw
    if len(raw) >= 2 and raw[0] == raw[-1] == "'":
        return raw[1:-1].replace("''", "'")
    return raw


@dataclass(frozen=True)
class CoverageBinDef:
    bin_id: str
    stage: str
    coverage_type: str
    coverage_group: str
    coverpoint: str
    bin_name: str
    mapped_path: str
    sample_event: str
    observe_object: str
    hit_rule: str
    priority: str
    suggested_testcase: str

    @property
    def key(self) -> Tuple[str, str, str]:
        return (self.coverage_group, self.coverpoint, self.bin_name)

    @property
    def group_bin_key(self) -> Tuple[str, str]:
        return (self.coverage_group, self.bin_name)


__all__ = [
    "CoverageBinDef",
    "decode_signal_inventory_name",
    "default_pilot_csv_path",
]
