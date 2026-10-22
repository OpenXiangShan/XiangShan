import pytest

from memblock_toffee.config import ConfigDB, VerificationConfig
from memblock_toffee.randomness import RandomStreams


def test_config_db_uses_uvm_style_paths() -> None:
    database = ConfigDB()
    database.set("env.agent.seed", 17)
    assert database.get("env.agent.seed") == 17
    assert database.get("missing", 3) == 3
    with pytest.raises(KeyError):
        database.get("missing")


def test_verification_config_rejects_invalid_budget() -> None:
    with pytest.raises(ValueError):
        VerificationConfig(max_cycles=0)


def test_verification_config_uses_run_scoped_artifacts() -> None:
    config = VerificationConfig(run_id="seed-17", artifact_dir="results")
    assert config.run_dir.as_posix() == "results/seed-17"
    assert config.document()["seed"] == 1


def test_verification_config_rejects_unsafe_run_id() -> None:
    with pytest.raises(ValueError, match="run_id"):
        VerificationConfig(run_id="../escape")


def test_named_random_streams_are_replayable_and_independent() -> None:
    first = RandomStreams(seed=17)
    replay = RandomStreams(seed=17)
    assert [first.stream("dcache").randrange(1000) for _ in range(8)] == [
        replay.stream("dcache").randrange(1000) for _ in range(8)
    ]
    assert first.stream("ptw").randrange(10_000) != first.stream("dcache").randrange(
        10_000
    )
