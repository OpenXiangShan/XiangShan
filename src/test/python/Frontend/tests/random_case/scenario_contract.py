from __future__ import annotations

import json
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any


SCENARIO_SCHEMA_VERSION = 2
REALIZATION_SCHEMA_VERSION = 1
GOAL_MIN_DYNAMIC_INSTRUCTIONS = 5000
UINT64_LIMIT = 1 << 64
REALIZATION_STATUSES = frozenset({"REQUESTED", "REALIZED", "NOT_REALIZED", "FAIL", "INFRA"})


@dataclass(frozen=True)
class EventRequirement:
    event: str
    relation: str = "observed"
    anchor: str = ""
    min_count: int = 1
    evidence_stage: str = "any"

    def validate(self) -> None:
        if not self.event.strip():
            raise ValueError("event requirement must name an event")
        if type(self.min_count) is not int or self.min_count <= 0:
            raise ValueError("event requirement min_count must be a positive integer")
        if self.evidence_stage not in {"any", "program", "dut", "funcov"}:
            raise ValueError(f"unsupported event evidence stage: {self.evidence_stage}")

    @property
    def key(self) -> tuple[str, str, str]:
        return (self.event, self.relation, self.anchor)


@dataclass(frozen=True)
class ScenarioSpec:
    scenario_id: str
    family: str
    ordinal: int
    program_seed: int
    timing_seed: int
    testpoint_ids: tuple[str, ...] = ()
    risk_tags: tuple[str, ...] = ()
    required_events: tuple[EventRequirement, ...] = ()
    parameters: dict[str, Any] = field(default_factory=dict)
    minimum_dynamic_instructions: int = GOAL_MIN_DYNAMIC_INSTRUCTIONS

    def validate(self) -> None:
        if not self.scenario_id.strip() or not self.family.strip():
            raise ValueError("scenario_id and family are required")
        if type(self.ordinal) is not int or self.ordinal < 0:
            raise ValueError("ordinal must be a nonnegative integer")
        for name, seed in (("program_seed", self.program_seed), ("timing_seed", self.timing_seed)):
            if type(seed) is not int or seed < 0 or seed >= UINT64_LIMIT:
                raise ValueError(f"{name} must fit in an unsigned 64-bit integer")
        if (type(self.minimum_dynamic_instructions) is not int
                or self.minimum_dynamic_instructions < GOAL_MIN_DYNAMIC_INSTRUCTIONS):
            raise ValueError(
                "minimum_dynamic_instructions must be >= "
                f"{GOAL_MIN_DYNAMIC_INSTRUCTIONS} for random-goal cases"
            )
        if len(set(self.testpoint_ids)) != len(self.testpoint_ids):
            raise ValueError("testpoint_ids must not contain duplicates")
        if len(set(self.risk_tags)) != len(self.risk_tags):
            raise ValueError("risk_tags must not contain duplicates")
        if not self.testpoint_ids and not self.risk_tags:
            raise ValueError("a scenario must trace to at least one testpoint or risk tag")
        for requirement in self.required_events:
            requirement.validate()

    def to_dict(self) -> dict[str, Any]:
        self.validate()
        return {
            "schema_version": SCENARIO_SCHEMA_VERSION,
            **asdict(self),
            "coverage_attribution": {
                "policy": "dynamic_many_to_many",
                "single_target_bin": False,
                "planned_testpoint_ids": list(self.testpoint_ids),
                "observed_bin_ids": "recorded_after_compatible_DUT_run",
            },
            "lifecycle_status": "REQUESTED",
        }


@dataclass(frozen=True)
class RealizedEvent:
    event: str
    count: int
    evidence_source: str
    relation: str = "observed"
    anchor: str = ""
    first_index: int | None = None
    first_cycle: int | None = None
    first_pc: int | None = None
    details: dict[str, Any] = field(default_factory=dict)

    def validate(self) -> None:
        if not self.event.strip() or not self.evidence_source.strip():
            raise ValueError("realized event and evidence_source are required")
        if type(self.count) is not int or self.count < 0:
            raise ValueError("realized event count must be a nonnegative integer")

    @property
    def key(self) -> tuple[str, str, str]:
        return (self.event, self.relation, self.anchor)


@dataclass(frozen=True)
class ScenarioRealization:
    scenario_id: str
    status: str
    evidence_stage: str
    required_events: tuple[EventRequirement, ...]
    realized_events: tuple[RealizedEvent, ...]
    missing_events: tuple[EventRequirement, ...]
    observed_bin_ids: tuple[str, ...] = ()
    realized_episode_ids: tuple[str, ...] = ()
    artifact_paths: dict[str, str] = field(default_factory=dict)
    details: dict[str, Any] = field(default_factory=dict)

    def validate(self) -> None:
        if not self.scenario_id.strip() or not self.evidence_stage.strip():
            raise ValueError("scenario_id and evidence_stage are required")
        if self.status not in REALIZATION_STATUSES:
            raise ValueError(f"unsupported realization status: {self.status}")
        for requirement in self.required_events:
            requirement.validate()
        for event in self.realized_events:
            event.validate()
        if len(set(self.observed_bin_ids)) != len(self.observed_bin_ids):
            raise ValueError("observed_bin_ids must not contain duplicates")
        if len(set(self.realized_episode_ids)) != len(self.realized_episode_ids):
            raise ValueError("realized_episode_ids must not contain duplicates")

    def to_dict(self) -> dict[str, Any]:
        self.validate()
        return {
            "schema_version": REALIZATION_SCHEMA_VERSION,
            **asdict(self),
            "coverage_attribution": {
                "policy": "dynamic_many_to_many",
                "observed_bin_ids": list(self.observed_bin_ids),
            },
        }


def evaluate_realization(
    spec: ScenarioSpec,
    realized_events: tuple[RealizedEvent, ...],
    *,
    evidence_stage: str,
    execution_status: str,
    exit_code: int,
    observed_bin_ids: tuple[str, ...] = (),
    realized_episode_ids: tuple[str, ...] = (),
    artifact_paths: dict[str, str] | None = None,
    details: dict[str, Any] | None = None,
) -> ScenarioRealization:
    spec.validate()
    counts: dict[tuple[str, tuple[str, str, str]], int] = {}
    source_stages = {
        "nemu_committed_trace": "program",
        "dut_top_level_timeline": "dut",
        "compatible_functional_coverage_artifact": "funcov",
    }
    for event in realized_events:
        event.validate()
        for stage in ("any", source_stages.get(event.evidence_source)):
            if stage is not None:
                key = (stage, event.key)
                counts[key] = counts.get(key, 0) + event.count
    stage_map = {
        "nemu_program": {"any", "program"},
        "dut_program_plus_funcov": {"any", "program", "dut", "funcov"},
    }
    if evidence_stage not in stage_map:
        raise ValueError(f"unsupported realization evidence stage: {evidence_stage}")
    available_stages = stage_map[evidence_stage]
    applicable_requirements = tuple(
        requirement
        for requirement in spec.required_events
        if requirement.evidence_stage in available_stages
    )
    missing = tuple(
        requirement
        for requirement in applicable_requirements
        if counts.get((requirement.evidence_stage, requirement.key), 0) < requirement.min_count
    )
    if execution_status == "infra":
        status = "INFRA"
    elif exit_code != 0 or execution_status in {"timeout", "fail"}:
        status = "FAIL"
    elif missing or not applicable_requirements:
        status = "NOT_REALIZED"
    else:
        status = "REALIZED"
    return ScenarioRealization(
        scenario_id=spec.scenario_id,
        status=status,
        evidence_stage=evidence_stage,
        required_events=applicable_requirements,
        realized_events=realized_events,
        missing_events=missing,
        observed_bin_ids=tuple(sorted(set(observed_bin_ids))),
        realized_episode_ids=tuple(sorted(set(realized_episode_ids))),
        artifact_paths=dict(artifact_paths or {}),
        details={**dict(details or {}), **({"no_applicable_requirements": True} if not applicable_requirements else {})},
    )


def write_json_contract(path: Path, payload: dict[str, Any]) -> None:
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.tmp")
    temporary.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    temporary.replace(path)


def scenario_spec_from_dict(payload: dict[str, Any]) -> ScenarioSpec:
    """Read a scenario without coercing malformed identity or evidence limits."""
    def string(value: Any, name: str) -> str:
        if not isinstance(value, str):
            raise ValueError(f"{name} must be a string")
        return value

    def integer(value: Any, name: str) -> int:
        if type(value) is not int:
            raise ValueError(f"{name} must be an integer")
        return value

    def sequence(value: Any, name: str) -> tuple:
        if not isinstance(value, (tuple, list)):
            raise ValueError(f"{name} must be a list")
        return tuple(value)

    try:
        if not isinstance(payload, dict):
            raise ValueError("scenario must be an object")
        requirements = []
        for item in sequence(payload.get("required_events", []), "required_events"):
            if not isinstance(item, dict):
                raise ValueError("event requirement must be an object")
            requirements.append(EventRequirement(
                event=string(item["event"], "event"),
                relation=string(item.get("relation", "observed"), "relation"),
                anchor=string(item.get("anchor", ""), "anchor"),
                min_count=integer(item.get("min_count", 1), "min_count"),
                evidence_stage=string(item.get("evidence_stage", "any"), "evidence_stage"),
            ))
        parameters = payload.get("parameters", {})
        if not isinstance(parameters, dict):
            raise ValueError("parameters must be an object")
        scenario = ScenarioSpec(
            scenario_id=string(payload["scenario_id"], "scenario_id"),
            family=string(payload["family"], "family"),
            ordinal=integer(payload["ordinal"], "ordinal"),
            program_seed=integer(payload["program_seed"], "program_seed"),
            timing_seed=integer(payload["timing_seed"], "timing_seed"),
            testpoint_ids=tuple(string(x, "testpoint_id") for x in sequence(payload.get("testpoint_ids", []), "testpoint_ids")),
            risk_tags=tuple(string(x, "risk_tag") for x in sequence(payload.get("risk_tags", []), "risk_tags")),
            required_events=tuple(requirements), parameters=dict(parameters),
            minimum_dynamic_instructions=integer(
                payload.get("minimum_dynamic_instructions", GOAL_MIN_DYNAMIC_INSTRUCTIONS),
                "minimum_dynamic_instructions"),
        )
        scenario.validate()
    except (KeyError, TypeError, ValueError) as exc:
        raise ValueError(f"invalid scenario spec payload: {exc}") from exc
    return scenario
