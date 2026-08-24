from __future__ import annotations

from dataclasses import dataclass
from typing import Any


OWNER_V3_EVENT_TYPE = "ifu.v3.owner_leaf"
OWNER_V3_COVERPOINT = "verified_leaf_event"


@dataclass(frozen=True)
class OwnerV3BinSpec:
    bin_id: str
    coverage_group: str
    bin_name: str
    suggested_testcase: str

    @property
    def sampler_key(self) -> tuple[str, str]:
        return (self.coverage_group, self.bin_name)


def _build_owner_v3_bin_specs() -> tuple[OwnerV3BinSpec, ...]:
    block_layout = (
        (60, "ifu_v3_pipeline_owner_model", "fe_ifu_v3_pipeline_owner_model"),
        (1, "ifu_v3_nc_owner_model", "fe_ifu_v3_nc_owner_model"),
        (45, "ifu_v3_boundary_owner_model", "fe_ifu_v3_boundary_owner_model"),
    )
    specs: list[OwnerV3BinSpec] = []
    for count, group, testcase in block_layout:
        for _ in range(count):
            ordinal = len(specs) + 1
            specs.append(
                OwnerV3BinSpec(
                    bin_id=f"BIN-{898 + ordinal:03d}",
                    coverage_group=group,
                    bin_name=f"owner_leaf_{ordinal:03d}",
                    suggested_testcase=testcase,
                )
            )
    return tuple(specs)


OWNER_V3_BIN_SPECS = _build_owner_v3_bin_specs()
OWNER_V3_BIN_BY_ID = {spec.bin_id: spec for spec in OWNER_V3_BIN_SPECS}
OWNER_V3_COVERPOINTS = {
    spec.coverage_group: OWNER_V3_COVERPOINT for spec in OWNER_V3_BIN_SPECS
}
OWNER_V3_SAMPLER_BIN_KEYS = frozenset(
    spec.sampler_key for spec in OWNER_V3_BIN_SPECS
)


def handle_owner_v3_event(recorder, event: dict[str, Any]) -> bool:
    """Record a checked owner-leaf event emitted by a directed checker.

    These bins model semantic scenarios whose complete predicates span DUT
    state, stimulus history, and scoreboard results. The producer must attach
    both predicate/checkpoint decisions and the observations used to make
    them; a bare event is intentionally insufficient to claim coverage.
    """

    if str(event.get("type", "")) != OWNER_V3_EVENT_TYPE:
        return False

    cycle = int(event.get("cycle", 0))
    payload = event.get("payload")
    if not isinstance(payload, dict):
        payload = {}
    bin_id = str(payload.get("bin_id", "")).strip()
    spec = OWNER_V3_BIN_BY_ID.get(bin_id)
    observations = payload.get("observations")
    accepted = (
        spec is not None
        and payload.get("condition_met") is True
        and payload.get("checkpoint_passed") is True
        and isinstance(observations, dict)
        and bool(observations)
    )
    if not accepted:
        recorder.risk_observations.append(
            {
                "event": "ifu_v3_owner_leaf_rejected",
                "cycle": cycle,
                "bin_id": bin_id,
                "known_bin": spec is not None,
                "condition_met": payload.get("condition_met"),
                "checkpoint_passed": payload.get("checkpoint_passed"),
                "has_observations": isinstance(observations, dict)
                and bool(observations),
            }
        )
        return False

    recorder.mark(
        spec.coverage_group,
        spec.bin_name,
        cycle,
        {
            "event": OWNER_V3_EVENT_TYPE,
            "bin_id": spec.bin_id,
            "condition_met": True,
            "checkpoint_passed": True,
            "observations": observations,
            "producer": str(payload.get("producer", "directed_checker")),
        },
    )
    return True


__all__ = [
    "OWNER_V3_BIN_BY_ID",
    "OWNER_V3_BIN_SPECS",
    "OWNER_V3_COVERPOINT",
    "OWNER_V3_COVERPOINTS",
    "OWNER_V3_EVENT_TYPE",
    "OWNER_V3_SAMPLER_BIN_KEYS",
    "OwnerV3BinSpec",
    "handle_owner_v3_event",
]
