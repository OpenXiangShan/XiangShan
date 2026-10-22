from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, TypeVar


T = TypeVar("T")
_MISSING = object()


class ConfigDB:
    """Small typed configuration database with UVM-style dotted paths."""

    def __init__(self) -> None:
        self._values: dict[str, Any] = {}

    def set(self, path: str, value: Any) -> None:
        if not path or path.startswith(".") or path.endswith("."):
            raise ValueError(f"invalid configuration path: {path!r}")
        self._values[path] = value

    def get(self, path: str, default: T | object = _MISSING) -> T:
        if path in self._values:
            return self._values[path]
        if default is not _MISSING:
            return default  # type: ignore[return-value]
        raise KeyError(path)

    def copy(self) -> ConfigDB:
        copied = ConfigDB()
        copied._values = dict(self._values)
        return copied


@dataclass(frozen=True, slots=True)
class VerificationConfig:
    """Run identity and budgets shared by Toffee components."""

    module: str = "MemBlock"
    profile: str = "coverage"
    seed: int = 1
    run_id: str = "local"
    max_cycles: int = 100_000
    artifact_dir: Path = field(default_factory=lambda: Path("artifacts"))
    config_db: ConfigDB = field(default_factory=ConfigDB, compare=False)

    def __post_init__(self) -> None:
        if not self.module:
            raise ValueError("module must not be empty")
        if not self.profile:
            raise ValueError("profile must not be empty")
        if self.seed < 0:
            raise ValueError("seed must be non-negative")
        if self.max_cycles < 1:
            raise ValueError("max_cycles must be positive")
        if not self.run_id:
            raise ValueError("run_id must not be empty")
        object.__setattr__(self, "artifact_dir", Path(self.artifact_dir))
