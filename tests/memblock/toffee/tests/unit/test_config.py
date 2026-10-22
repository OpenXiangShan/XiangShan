import pytest

from memblock_toffee.config import ConfigDB, VerificationConfig


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
