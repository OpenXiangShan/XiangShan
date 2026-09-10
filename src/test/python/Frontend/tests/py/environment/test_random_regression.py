from __future__ import annotations

from types import SimpleNamespace

import pytest

import env.runtime.fixtures as fixtures_module
from env.support.random_regression import (
    derive_seed,
    read_seed,
    record_scenario,
    scenario_rng,
)


def test_scenario_rng_replays_independently_of_call_order() -> None:
    first = scenario_rng("zhaoxinran/multi-branch/dense-loop", base_seed=0x1234)
    scenario_rng("zhaoxinran/translation/stage1", ordinal=7, base_seed=0x1234)
    replay = scenario_rng("zhaoxinran/multi-branch/dense-loop", base_seed=0x1234)

    assert first[:2] == replay[:2]
    assert [first[2].getrandbits(32) for _ in range(8)] == [
        replay[2].getrandbits(32) for _ in range(8)
    ]


def test_scenario_seed_changes_with_each_identity_component() -> None:
    baseline = derive_seed(1, "scenario-a", ordinal=0)

    assert derive_seed(2, "scenario-a", ordinal=0) != baseline
    assert derive_seed(1, "scenario-b", ordinal=0) != baseline
    assert derive_seed(1, "scenario-a", ordinal=1) != baseline


def test_read_seed_accepts_prefixed_integer_and_rejects_invalid_value(monkeypatch) -> None:
    monkeypatch.setenv("TB_SEED", "0x5a39")
    assert read_seed() == 0x5A39

    monkeypatch.setenv("TB_SEED", "invalid")
    with pytest.raises(AssertionError, match="TB_SEED must be a valid integer"):
        read_seed()


def test_record_scenario_keeps_replay_identity_and_json_parameters() -> None:
    env = SimpleNamespace()
    record = record_scenario(
        env,
        "zhaoxinran/translation/stage2",
        base_seed=0x5A39,
        seed=0x1234,
        ordinal=2,
        parameters={"path": "fault", "latency": (2, 7)},
    )

    assert record == {
        "scenario_key": "zhaoxinran/translation/stage2",
        "base_seed": 0x5A39,
        "seed": 0x1234,
        "ordinal": 2,
        "parameters_sha256": "a580d74f236658377f416d33e183752d0b2fbdee710da4a0fe232af0cc5480f0",
        "parameters": {"path": "fault", "latency": [2, 7]},
    }
    assert env.random_scenario_records == [record]


def test_funcov_run_metadata_includes_random_scenario_records(
    tmp_path,
    monkeypatch,
) -> None:
    records = [
        {
            "scenario_key": "zhaoxinran/multi-branch/dense-loop",
            "base_seed": 0x12,
            "seed": 0x9A,
            "ordinal": 0,
            "parameters": {"loop_size": 12},
        }
    ]
    request = SimpleNamespace(
        node=SimpleNamespace(
            nodeid="tests/test_case.py::test_case",
            path=None,
            rep_call=SimpleNamespace(outcome="passed"),
        )
    )
    env = SimpleNamespace(
        get_errors=lambda: [],
        random_scenario_records=records,
        config=SimpleNamespace(
            icache=SimpleNamespace(seed=0x56),
            ptw=SimpleNamespace(seed=0x78),
        ),
    )
    monkeypatch.setenv("TB_ARTIFACT_DIR", str(tmp_path))
    monkeypatch.setenv("TB_SEED", "0x12")
    monkeypatch.setenv("TB_BACKEND_RANDOM_SEED", "0x34")

    execution = fixtures_module._funcov_run_metadata(request, env)["execution"]

    assert execution["random_scenarios"] == records
    assert execution["random_scenarios"] is not records


@pytest.mark.parametrize("scenario_key,ordinal", [("", 0), ("valid", -1)])
def test_random_scenario_identity_rejects_ambiguous_values(
    scenario_key: str,
    ordinal: int,
) -> None:
    with pytest.raises(ValueError):
        derive_seed(1, scenario_key, ordinal=ordinal)
