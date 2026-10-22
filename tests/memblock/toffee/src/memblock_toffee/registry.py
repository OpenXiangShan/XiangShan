from __future__ import annotations

from collections.abc import Callable, Iterable
from dataclasses import dataclass
from typing import Any, Protocol

from .config import VerificationConfig
from .randomness import RandomStreams
from .timing import CycleTracker
from .uvm import Sequencer


class SuiteComponent(Protocol):
    @property
    def agents(self) -> dict[str, object]: ...

    @property
    def sequencers(self) -> dict[str, Sequencer[Any]]: ...

    async def reset(self, cycles: int = 8) -> None: ...

    async def execute(self, sequence: Any) -> Any: ...

    async def tick(self, cycles: int = 1, *, require_idle: bool = False) -> None: ...

    def finish(self) -> None: ...


@dataclass(frozen=True, slots=True)
class EnvironmentContext:
    adapter: Any
    config: VerificationConfig
    rng: RandomStreams
    cycle_tracker: CycleTracker


class VerificationRegistry:
    """Register independently owned protocol components and coverage providers."""

    def __init__(self) -> None:
        self._components: dict[
            str,
            Callable[[EnvironmentContext], SuiteComponent],
        ] = {}
        self._coverage: dict[str, Callable[[Any], Any]] = {}

    def add_component(
        self,
        name: str,
        factory: Callable[[EnvironmentContext], SuiteComponent],
    ) -> None:
        self._add(self._components, name, factory, "component")

    def add_coverage(
        self,
        name: str,
        provider: Callable[[Any], Any],
    ) -> None:
        self._add(self._coverage, name, provider, "coverage provider")

    @property
    def component_names(self) -> tuple[str, ...]:
        return tuple(self._components)

    @property
    def coverage_names(self) -> tuple[str, ...]:
        return tuple(self._coverage)

    def build_components(
        self,
        context: EnvironmentContext,
    ) -> dict[str, SuiteComponent]:
        return {
            name: factory(context)
            for name, factory in self._components.items()
        }

    def build_coverage(self, env: Any) -> list[Any]:
        groups: list[Any] = []
        for provider in self._coverage.values():
            result = provider(env)
            if result is None:
                continue
            if isinstance(result, Iterable) and not isinstance(result, (str, bytes)):
                groups.extend(result)
            else:
                groups.append(result)
        return groups

    @staticmethod
    def _add(
        registry: dict[str, Any],
        name: str,
        factory: Any,
        kind: str,
    ) -> None:
        if not name or name.strip() != name:
            raise ValueError(f"{kind} name must be non-empty and trimmed")
        if name in registry:
            raise ValueError(f"duplicate {kind}: {name}")
        registry[name] = factory
