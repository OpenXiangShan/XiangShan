from __future__ import annotations

import inspect
from typing import Any

from toffee import Env

from memblock_toffee.adapters import MemBlockDutAdapter
from memblock_toffee.config import VerificationConfig
from memblock_toffee.coverage import control_coverage
from memblock_toffee.env.control import ControlComponent
from memblock_toffee.randomness import RandomStreams
from memblock_toffee.registry import EnvironmentContext, VerificationRegistry
from memblock_toffee.timing import CycleTracker
from memblock_toffee.toffee_runtime import ClockLease
from memblock_toffee.uvm import Sequence, Sequencer


def default_registry() -> VerificationRegistry:
    registry = VerificationRegistry()
    registry.add_component("control", ControlComponent)
    registry.add_coverage(
        "control",
        lambda env: control_coverage(env.component("control")),
    )
    return registry


class MemBlockEnv(Env):
    """Stable software-facing facade composed from registered protocol slices."""

    def __init__(
        self,
        adapter: MemBlockDutAdapter,
        *,
        config: VerificationConfig | None = None,
        rng: RandomStreams | None = None,
        registry: VerificationRegistry | None = None,
        default_component: str = "control",
    ) -> None:
        super().__init__()
        self._adapter = adapter
        self.config = config or VerificationConfig()
        self.rng = rng or RandomStreams(self.config.seed)
        self.cycle_tracker = CycleTracker(self.config.max_cycles)
        context = EnvironmentContext(
            adapter=adapter,
            config=self.config,
            rng=self.rng,
            cycle_tracker=self.cycle_tracker,
        )
        self.registry = registry or default_registry()
        self.default_component = default_component
        self._components = self.registry.build_components(context)
        if self.default_component not in self._components:
            raise ValueError(
                f"default component is not registered: {self.default_component}"
            )
        for component in self._components.values():
            for name, agent in getattr(component, "agents", {}).items():
                if hasattr(self, name):
                    raise ValueError(f"duplicate environment agent attribute: {name}")
                setattr(self, name, agent)
        self._sequencers: list[Sequencer[Any]] = []
        for component_name, component in self._components.items():
            for sequencer_name, sequencer in getattr(
                component,
                "sequencers",
                {},
            ).items():
                if not isinstance(sequencer, Sequencer):
                    raise TypeError(
                        f"component {component_name!r} exposes non-sequencer "
                        f"{sequencer_name!r}"
                    )
                self._sequencers.append(sequencer)
        self._coverage_groups = self.registry.build_coverage(self)
        self._clock: ClockLease | None = None
        self._closed = False

    @property
    def cycle(self) -> int:
        return self.cycle_tracker.cycle

    @property
    def remaining_cycles(self) -> int | None:
        return self.cycle_tracker.remaining

    @property
    def coverage_groups(self) -> tuple[Any, ...]:
        return tuple(self._coverage_groups)

    def component(self, name: str | None = None) -> Any:
        name = self.default_component if name is None else name
        try:
            return self._components[name]
        except KeyError as error:
            raise KeyError(f"unknown verification component: {name}") from error

    async def start(self) -> MemBlockEnv:
        if self._closed:
            raise RuntimeError("cannot restart a closed MemBlockEnv")
        if self._clock is None:
            self._clock = ClockLease.start(self._adapter)
        return self

    async def close(self) -> None:
        if self._closed:
            return
        self._closed = True
        errors: list[Exception] = []
        try:
            for component in reversed(tuple(self._components.values())):
                closer = getattr(component, "close", None)
                if closer is None:
                    continue
                try:
                    result = closer()
                    if inspect.isawaitable(result):
                        await result
                except Exception as error:
                    errors.append(error)
            for sequencer in self._sequencers:
                try:
                    sequencer.assert_complete()
                except Exception as error:
                    errors.append(error)
        finally:
            if self._clock is not None:
                try:
                    await self._clock.close()
                except Exception as error:
                    errors.append(error)
        if errors:
            raise ExceptionGroup("MemBlockEnv close failures", errors)

    async def reset(self, cycles: int = 8, *, component: str | None = None) -> None:
        await self.component(component).reset(cycles)

    async def tick(
        self,
        cycles: int = 1,
        *,
        require_idle: bool = False,
        component: str | None = None,
    ) -> None:
        await self.component(component).tick(cycles, require_idle=require_idle)

    async def run(
        self,
        sequence: Sequence[Any],
        *,
        component: str | None = None,
    ) -> Any:
        return await self.component(component).execute(sequence)

    def finish(self, component: str | None = None) -> None:
        selected = self.component(component)
        selected.finish()
        for sequencer in getattr(selected, "sequencers", {}).values():
            sequencer.assert_complete()

    def mark_coverage(self, test: Any, *points: str) -> None:
        if not points:
            return
        for point in points:
            matched = False
            for group in self._coverage_groups:
                try:
                    group.cover_point(point)
                except Exception:
                    continue
                group.mark_function(point, test)
                matched = True
            if not matched:
                raise KeyError(f"unknown coverage point: {point}")
