from __future__ import annotations

from dataclasses import dataclass, replace

from .translation_scenarios import TranslationScenario, TranslationScenarioBuilder, TranslationScenarioState


@dataclass(frozen=True)
class TranslationScenarioPhase:
    """One translation context in a directed multi-action scenario."""

    scenario: TranslationScenario | None = None
    reuse_previous: bool = False
    redirect: bool = True
    target_pc: int | None = None
    wait_for_ptw_requests: int = 0
    wait_for_stale_responses: int = 0
    wait_for_completion: bool = True
    allow_speculative_before_response: bool = False
    max_cycles: int = 6000


@dataclass(frozen=True)
class TranslationSfenceAction:
    """A public SFENCE/HFENCE action between translation phases."""

    addr: int = 0
    rs1: int = 0
    rs2: int = 0
    ident: int = 0
    hv: int = 0
    hg: int = 0
    cycles: int = 1


@dataclass(frozen=True)
class TranslationScenarioSequence:
    """Run ordered translation phases without embedding timing loops in a testcase."""

    actions: tuple[TranslationScenarioPhase | TranslationSfenceAction, ...]

    @staticmethod
    def _wait(env, predicate, *, description: str, max_cycles: int) -> None:
        for _ in range(max(0, int(max_cycles))):
            if predicate():
                return
            env.step(1)
        if predicate():
            return
        raise AssertionError(f"translation scenario sequence timed out waiting for {description}")

    @staticmethod
    def _phase_complete(env) -> bool:
        active = env.translation_oracle.get_active()
        if active is None:
            return False
        if active["expected_path"] == "fault":
            return bool(active["fault_seen"])
        return bool(active["response_seen"] and active["fetch_seen"])

    def run(self, env) -> list[dict]:
        if not self.actions:
            raise ValueError("translation scenario sequence requires at least one action")

        results: list[dict] = []
        previous_state: TranslationScenarioState | None = None
        for action in self.actions:
            if isinstance(action, TranslationSfenceAction):
                ptw_stats = env.ptw_agent.get_stats()
                dropped_before = int(ptw_stats.get("sfence_dropped_responses", 0))
                record = env.pulse_sfence(
                    addr=int(action.addr),
                    rs1=int(action.rs1),
                    rs2=int(action.rs2),
                    ident=int(action.ident),
                    hv=int(action.hv),
                    hg=int(action.hg),
                    cycles=int(action.cycles),
                )
                dropped_after = int(env.ptw_agent.get_stats().get("sfence_dropped_responses", 0))
                env.translation_oracle.discard_pending_ptw_responses(
                    int(env.current_cycle),
                    agent_dropped=dropped_after - dropped_before,
                )
                results.append(
                    {
                        "kind": "sfence",
                        "record": record,
                    }
                )
                continue

            if action.reuse_previous:
                if previous_state is None or action.scenario is not None:
                    raise ValueError("reuse_previous phase must follow a phase and omit scenario")
                state = replace(previous_state, translation_epoch=int(env.translation_epoch))
            else:
                if action.scenario is None:
                    raise ValueError("translation phase requires a scenario")
                state = TranslationScenarioBuilder(env).build(action.scenario)
            scenario = state.scenario
            env.monitor.clear()
            env.monitor.set_expected_pc(int(action.target_pc if action.target_pc is not None else scenario.va))
            env.arm_translation_scenario(state)
            env.translation_oracle.set_speculative_request_policy(
                allow_before_response=bool(action.allow_speculative_before_response)
            )

            if int(action.wait_for_stale_responses):
                baseline_stale = sum(
                    record["kind"] == "stale_ptw_response"
                    for record in env.translation_oracle.get_stats()["records"]
                )
                self._wait(
                    env,
                    lambda: sum(
                        record["kind"] == "stale_ptw_response"
                        for record in env.translation_oracle.get_stats()["records"]
                    )
                    >= baseline_stale + int(action.wait_for_stale_responses),
                    description="stale PTW response",
                    max_cycles=int(action.max_cycles),
                )

            request_count = int(env.ptw_agent.get_stats()["req_count"])
            if action.redirect:
                env.backend_model.inject_redirect(
                    int(action.target_pc if action.target_pc is not None else scenario.va),
                    "translation-scenario-sequence",
                )
            if int(action.wait_for_ptw_requests):
                self._wait(
                    env,
                    lambda: int(env.ptw_agent.get_stats()["req_count"])
                    >= request_count + int(action.wait_for_ptw_requests),
                    description="PTW request",
                    max_cycles=int(action.max_cycles),
                )
            if action.wait_for_completion:
                self._wait(
                    env,
                    lambda: self._phase_complete(env),
                    description="translation completion",
                    max_cycles=int(action.max_cycles),
                )
                env.assert_translation_scenario()
            results.append(
                {
                    "kind": "phase",
                    "scenario_id": str(scenario.scenario_id),
                    "translation_epoch": int(state.translation_epoch),
                    "state": state,
                }
            )
            previous_state = state
        return results


__all__ = ["TranslationScenarioPhase", "TranslationScenarioSequence", "TranslationSfenceAction"]
