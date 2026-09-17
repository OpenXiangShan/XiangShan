from __future__ import annotations

import hashlib
import json
import logging
import os
import random
from typing import Any


logger = logging.getLogger("env.random_regression")


def read_seed(name: str = "TB_SEED", default: int = 1) -> int:
    raw = os.getenv(str(name), str(int(default))).strip()
    try:
        return int(raw, 0)
    except ValueError as exc:
        raise AssertionError(f"{name} must be a valid integer, got: {raw}") from exc


def derive_seed(base_seed: int, scenario_key: str, *, ordinal: int = 0) -> int:
    key = str(scenario_key).strip()
    if not key:
        raise ValueError("random scenario key must be non-empty")
    if int(ordinal) < 0:
        raise ValueError("random scenario ordinal must be non-negative")
    identity = f"{int(base_seed)}\0{key}\0{int(ordinal)}"
    digest = hashlib.sha256(identity.encode("utf-8")).digest()
    return int.from_bytes(digest[:8], "big")


def scenario_rng(
    scenario_key: str,
    *,
    ordinal: int = 0,
    base_seed: int | None = None,
) -> tuple[int, int, random.Random]:
    selected_base_seed = read_seed() if base_seed is None else int(base_seed)
    selected_seed = derive_seed(
        selected_base_seed,
        str(scenario_key),
        ordinal=int(ordinal),
    )
    return selected_base_seed, selected_seed, random.Random(selected_seed)


def _json_value(value: Any) -> Any:
    if value is None or isinstance(value, (bool, int, float, str)):
        return value
    if isinstance(value, dict):
        return {str(key): _json_value(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_json_value(item) for item in value]
    raise TypeError(f"random scenario parameter is not JSON serializable: {type(value).__name__}")


def record_scenario(
    env,
    scenario_key: str,
    *,
    base_seed: int,
    seed: int,
    ordinal: int = 0,
    parameters: dict[str, Any] | None = None,
) -> dict[str, Any]:
    key = str(scenario_key).strip()
    if not key:
        raise ValueError("random scenario key must be non-empty")
    if int(ordinal) < 0:
        raise ValueError("random scenario ordinal must be non-negative")
    normalized_parameters = _json_value(parameters or {})
    parameters_json = json.dumps(normalized_parameters, sort_keys=True, separators=(",", ":"))
    record = {
        "scenario_key": key,
        "base_seed": int(base_seed),
        "seed": int(seed),
        "ordinal": int(ordinal),
        "parameters_sha256": hashlib.sha256(parameters_json.encode("utf-8")).hexdigest(),
        "parameters": normalized_parameters,
    }
    records = getattr(env, "random_scenario_records", None)
    if records is None:
        records = []
        setattr(env, "random_scenario_records", records)
    records.append(record)
    summary_keys = (
        "scenario_id",
        "pc",
        "va",
        "pa",
        "gpa",
        "first_beat",
        "second_beat",
        "latency",
        "resolve_delay",
        "expected_path",
        "declared_expected_path",
        "payload_sha256",
        "program_sha256",
    )
    summary = {
        field: normalized_parameters[field]
        for field in summary_keys
        if field in normalized_parameters
    }
    oracle_outcome = normalized_parameters.get("oracle_expected_outcome")
    if isinstance(oracle_outcome, dict):
        summary["oracle_expected_path"] = oracle_outcome.get("expected_path")
        summary["oracle_fault"] = oracle_outcome.get("fault")
    logger.info(
        "random scenario: key=%s base_seed=%d seed=%d ordinal=%d parameters_sha256=%s summary=%s",
        key,
        int(base_seed),
        int(seed),
        int(ordinal),
        record["parameters_sha256"],
        json.dumps(summary, sort_keys=True, separators=(",", ":")),
    )
    return record
