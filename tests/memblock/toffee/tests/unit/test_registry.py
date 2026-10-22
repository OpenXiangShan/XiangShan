from dataclasses import dataclass

import pytest

from memblock_toffee.config import VerificationConfig
from memblock_toffee.env import MemBlockEnv
from memblock_toffee.randomness import RandomStreams
from memblock_toffee.registry import EnvironmentContext, VerificationRegistry
from memblock_toffee.uvm import Sequencer


@dataclass
class FakeComponent:
    context: EnvironmentContext

    def __init__(self, context: EnvironmentContext) -> None:
        self.context = context
        self.requests: list[str] = []
        self.closed = False
        self.sequencer: Sequencer[str] = Sequencer("fake")

    @property
    def agents(self) -> dict[str, object]:
        return {}

    @property
    def sequencers(self) -> dict[str, Sequencer[str]]:
        return {"fake": self.sequencer}

    async def reset(self, cycles: int = 8) -> None:
        self.requests.append(f"reset-{cycles}")

    async def execute(self, sequence: str) -> str:
        self.requests.append(sequence)
        return sequence.upper()

    async def tick(self, cycles: int = 1, *, require_idle: bool = False) -> None:
        self.requests.append(f"tick-{cycles}-{require_idle}")

    def finish(self) -> None:
        self.requests.append("finish")

    async def close(self) -> None:
        self.closed = True


def test_registry_builds_components_and_flattens_coverage() -> None:
    registry = VerificationRegistry()
    registry.add_component("fake", FakeComponent)
    registry.add_coverage("one", lambda _: "group-1")
    registry.add_coverage("many", lambda _: (item for item in ("group-2", "group-3")))
    context = EnvironmentContext(object(), object(), object(), object())
    components = registry.build_components(context)
    env = type("Env", (), {"component": lambda self, name: components[name]})()
    assert tuple(components) == ("fake",)
    assert registry.build_coverage(env) == ["group-1", "group-2", "group-3"]


def test_registry_rejects_duplicate_extension_names() -> None:
    registry = VerificationRegistry()
    registry.add_component("fake", FakeComponent)
    with pytest.raises(ValueError, match="duplicate component"):
        registry.add_component("fake", FakeComponent)


@pytest.mark.asyncio
async def test_environment_facade_routes_component_operations() -> None:
    registry = VerificationRegistry()
    registry.add_component("fake", FakeComponent)
    env = MemBlockEnv(
        object(),  # type: ignore[arg-type]
        config=VerificationConfig(max_cycles=12),
        rng=RandomStreams(9),
        registry=registry,
        default_component="fake",
    )
    await env.reset(3, component="fake")
    assert await env.run("scenario", component="fake") == "SCENARIO"
    await env.tick(2, require_idle=True, component="fake")
    env.finish("fake")
    component = env.component("fake")
    assert component.requests == ["reset-3", "scenario", "tick-2-True", "finish"]
    await env.close()
    assert component.closed


@pytest.mark.asyncio
async def test_environment_finish_rejects_abandoned_requests() -> None:
    registry = VerificationRegistry()
    registry.add_component("fake", FakeComponent)
    env = MemBlockEnv(
        object(),  # type: ignore[arg-type]
        registry=registry,
        default_component="fake",
    )
    handle = await env.component("fake").sequencer.issue("pending", key="request-0")
    with pytest.raises(AssertionError, match="unfinished requests"):
        env.finish()
    env.component("fake").sequencer.cancel(handle.key)
    await env.close()
