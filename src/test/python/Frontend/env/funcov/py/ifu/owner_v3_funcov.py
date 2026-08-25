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


@dataclass(frozen=True)
class OwnerV3SourceRule:
    bin_id: str
    source_bin_ids: tuple[str, ...]


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

# These rules are deliberately limited to semantic one-to-one relationships.
# The owner leaf is recorded only in a new run that observes the canonical
# source bin; old artifacts are never rewritten to manufacture owner hits.
OWNER_V3_SOURCE_RULES = (
    OwnerV3SourceRule("BIN-899", ("BIN-831",)),
    OwnerV3SourceRule("BIN-902", ("BIN-427",)),
    OwnerV3SourceRule("BIN-903", ("BIN-1055",)),
    OwnerV3SourceRule("BIN-905", ("BIN-762", "BIN-942")),
    OwnerV3SourceRule("BIN-905", ("BIN-763", "BIN-942")),
    OwnerV3SourceRule("BIN-906", ("BIN-636", "BIN-942")),
    OwnerV3SourceRule("BIN-912", ("BIN-874",)),
    OwnerV3SourceRule("BIN-913", ("BIN-867",)),
    OwnerV3SourceRule("BIN-919", ("BIN-898",)),
    OwnerV3SourceRule("BIN-924", ("BIN-431",)),
    OwnerV3SourceRule("BIN-925", ("BIN-432",)),
    OwnerV3SourceRule("BIN-926", ("BIN-432",)),
    OwnerV3SourceRule("BIN-933", ("BIN-832", "BIN-886", "BIN-898")),
    OwnerV3SourceRule("BIN-934", ("BIN-892",)),
    OwnerV3SourceRule("BIN-942", ("BIN-432",)),
    OwnerV3SourceRule("BIN-944", ("BIN-836",)),
    OwnerV3SourceRule("BIN-952", ("BIN-814", "BIN-815")),
    OwnerV3SourceRule("BIN-955", ("BIN-807", "BIN-808", "BIN-828", "BIN-866")),
    OwnerV3SourceRule("BIN-956", ("BIN-812", "BIN-814", "BIN-883", "BIN-884")),
    OwnerV3SourceRule("BIN-958", ("BIN-432", "BIN-832", "BIN-886", "BIN-897")),
    OwnerV3SourceRule("BIN-961", ("BIN-874",)),
    OwnerV3SourceRule("BIN-962", ("BIN-432",)),
    OwnerV3SourceRule("BIN-964", ("BIN-878",)),
    OwnerV3SourceRule("BIN-965", ("BIN-879",)),
    OwnerV3SourceRule("BIN-966", ("BIN-832",)),
    OwnerV3SourceRule("BIN-967", ("BIN-880",)),
    OwnerV3SourceRule("BIN-968", ("BIN-861",)),
    OwnerV3SourceRule("BIN-969", ("BIN-848",)),
    OwnerV3SourceRule("BIN-970", ("BIN-432",)),
)
_OWNER_V3_SOURCE_RULES_BY_TRIGGER = {
    source_bin_id: tuple(
        rule for rule in OWNER_V3_SOURCE_RULES if source_bin_id in rule.source_bin_ids
    )
    for source_bin_id in {
        source_bin_id
        for rule in OWNER_V3_SOURCE_RULES
        for source_bin_id in rule.source_bin_ids
    }
}


def mark_owner_v3_checked(
    recorder,
    bin_id: str,
    cycle: int,
    observations: dict[str, Any],
    *,
    producer: str,
) -> bool:
    return handle_owner_v3_event(
        recorder,
        {
            "type": OWNER_V3_EVENT_TYPE,
            "cycle": int(cycle),
            "payload": {
                "bin_id": str(bin_id),
                "condition_met": True,
                "checkpoint_passed": True,
                "observations": observations,
                "producer": str(producer),
            },
        },
    )


def derive_owner_v3_from_source(
    recorder,
    source_bin_id: str,
    cycle: int,
    evidence: dict[str, Any] | None,
) -> None:
    for rule in _OWNER_V3_SOURCE_RULES_BY_TRIGGER.get(str(source_bin_id), ()):
        source_hits = {}
        for required_bin_id in rule.source_bin_ids:
            definition = recorder.definition_by_bin_id.get(required_bin_id)
            hit = None if definition is None else recorder.hits.get(definition.key)
            if hit is None or hit.hits <= 0:
                break
            source_hits[required_bin_id] = int(hit.hits)
        else:
            mark_owner_v3_checked(
                recorder,
                rule.bin_id,
                cycle,
                {
                    "derivation": "canonical_dut_bin_equivalence",
                    "source_bin_ids": list(rule.source_bin_ids),
                    "source_hits": source_hits,
                    "trigger_evidence": evidence or {},
                },
                producer="owner_v3_source_derivation",
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
    "OWNER_V3_SOURCE_RULES",
    "OwnerV3BinSpec",
    "OwnerV3SourceRule",
    "derive_owner_v3_from_source",
    "handle_owner_v3_event",
    "mark_owner_v3_checked",
]
