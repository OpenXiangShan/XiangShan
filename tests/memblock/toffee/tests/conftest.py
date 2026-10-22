from __future__ import annotations

import json
import os
from pathlib import Path

import pytest

from memblock_toffee import RandomStreams, VerificationConfig
from memblock_toffee.fixtures import memblock_env


def pytest_addoption(parser: pytest.Parser) -> None:
    group = parser.getgroup("memblock")
    group.addoption("--memblock-profile", default=None)
    group.addoption("--memblock-seed", type=int, default=None)
    group.addoption("--memblock-run-id", default=None)
    group.addoption("--memblock-max-cycles", type=int, default=None)
    group.addoption("--memblock-artifact-dir", type=Path, default=None)


def _configured(
    pytestconfig: pytest.Config,
    option: str,
    environment: str,
    default: str,
) -> str:
    cli = pytestconfig.getoption(option)
    if cli is not None:
        return str(cli)
    return os.environ.get(environment, default)


@pytest.fixture(scope="session")
def verification_config(pytestconfig: pytest.Config) -> VerificationConfig:
    artifact_dir = _configured(
        pytestconfig,
        "--memblock-artifact-dir",
        "MEMBLOCK_TOFFEE_ARTIFACT_DIR",
        "build/memblock/toffee/results",
    )
    config = VerificationConfig(
        profile=_configured(
            pytestconfig,
            "--memblock-profile",
            "MEMBLOCK_TOFFEE_PROFILE",
            "coverage",
        ),
        seed=int(
            _configured(
                pytestconfig,
                "--memblock-seed",
                "MEMBLOCK_TOFFEE_SEED",
                "1",
            )
        ),
        run_id=_configured(
            pytestconfig,
            "--memblock-run-id",
            "MEMBLOCK_TOFFEE_RUN_ID",
            f"local-{os.getpid()}",
        ),
        max_cycles=int(
            _configured(
                pytestconfig,
                "--memblock-max-cycles",
                "MEMBLOCK_TOFFEE_MAX_CYCLES",
                "100000",
            )
        ),
        artifact_dir=Path(artifact_dir),
    )
    config.run_dir.mkdir(parents=True, exist_ok=True)
    temporary = config.run_dir / f"run.json.{os.getpid()}.tmp"
    temporary.write_text(
        json.dumps(config.document(), indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    temporary.replace(config.run_dir / "run.json")
    return config


@pytest.fixture
def random_streams(verification_config: VerificationConfig) -> RandomStreams:
    return RandomStreams(verification_config.seed)


__all__ = ["memblock_env", "random_streams", "verification_config"]
