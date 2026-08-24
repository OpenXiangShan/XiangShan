from __future__ import annotations

import random
from dataclasses import dataclass, replace

from .translation_scenarios import (
    TranslationPmpPmaEntry,
    TranslationScenario,
    TranslationScenarioBuilder,
    TranslationScenarioRandomizer,
    TranslationScenarioState,
)
from ..support.pmp_pma import PmpPmaConfig


@dataclass(frozen=True)
class TranslationScenarioPhase:
    """One translation context in a directed multi-action scenario."""

    scenario: TranslationScenario | None = None
    reuse_previous: bool = False
    redirect: bool = False
    target_pc: int | None = None
    page_indexes: tuple[int, ...] | None = None
    expect_ptw: bool = True
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
class TranslationContextAction:
    """Apply one explicit translation-context change between scenario phases."""

    satp_mode: int | None = None
    satp_asid: int | None = None
    satp_ppn: int | None = None
    vsatp_mode: int | None = None
    vsatp_asid: int | None = None
    vsatp_ppn: int | None = None
    hgatp_mode: int | None = None
    hgatp_vmid: int | None = None
    hgatp_ppn: int | None = None
    priv_imode: int | None = None
    priv_virt: int | None = None
    cycles: int = 1


@dataclass(frozen=True)
class TranslationPmpPmaWriteAction:
    """Write one PMP/PMA entry after a phase, including locked-entry rewrite attempts."""

    entry: TranslationPmpPmaEntry
    settle_cycles: int = 0


@dataclass(frozen=True)
class TranslationPbmteAction:
    """Update the PTW PBMTE policy and drive a DUT control when one is live."""

    machine: int | None = None
    hypervisor: int | None = None
    require_supported: bool = False


@dataclass(frozen=True)
class TranslationScenarioSequence:
    """Run ordered translation phases without embedding timing loops in a testcase."""

    actions: tuple[
        TranslationScenarioPhase
        | TranslationSfenceAction
        | TranslationContextAction
        | TranslationPmpPmaWriteAction
        | TranslationPbmteAction,
        ...,
    ]

    @staticmethod
    def randomized_control_actions(seed: int, count: int) -> tuple[
        TranslationSfenceAction
        | TranslationContextAction
        | TranslationPmpPmaWriteAction,
        ...,
    ]:
        """Create a replayable legal control stream for translation regressions."""

        if int(count) < 0:
            raise ValueError("translation random control count must be non-negative")
        generator = random.Random(int(seed))
        actions = []
        for ordinal in range(int(count)):
            kind = ordinal % 3
            if kind == 0:
                actions.append(
                    TranslationSfenceAction(
                        addr=0x8020_0000 + generator.randrange(4) * 0x1000,
                        rs1=generator.randrange(2),
                        rs2=generator.randrange(2),
                        ident=generator.randrange(16),
                        hv=generator.randrange(2),
                        hg=generator.randrange(2),
                    )
                )
            elif kind == 1:
                actions.append(
                    TranslationContextAction(
                        satp_asid=generator.randrange(16),
                        vsatp_asid=generator.randrange(16),
                        hgatp_vmid=generator.randrange(16),
                        priv_imode=generator.randrange(3),
                        priv_virt=generator.randrange(2),
                    )
                )
            else:
                actions.append(
                    TranslationPmpPmaWriteAction(
                        TranslationPmpPmaEntry(
                            "pmp" if generator.randrange(2) else "pma",
                            31 - ((ordinal // 5) % 8),
                            PmpPmaConfig(
                                match="napot",
                                read=True,
                                execute=bool(generator.randrange(2)),
                                cacheable=bool(generator.randrange(2)),
                                locked=bool(generator.randrange(2)),
                            ),
                            0x8040_0000 + generator.randrange(8) * 0x1000,
                            size=0x1000,
                        )
                    )
                )
        return tuple(actions)

    @staticmethod
    def randomized_scenario_actions(
        seed: int,
        count: int,
        *,
        start_ordinal: int = 0,
        include_controls: bool = True,
    ) -> tuple[
        TranslationScenarioPhase
        | TranslationSfenceAction
        | TranslationContextAction
        | TranslationPmpPmaWriteAction,
        ...,
    ]:
        """Build one replayable reset/re-entry phase for a generic regression."""

        if int(count) < 0:
            raise ValueError("translation random scenario count must be non-negative")
        if int(start_ordinal) < 0:
            raise ValueError("translation random scenario start ordinal must be non-negative")
        if int(count) > 1:
            raise ValueError(
                "translation random phases require explicit reset/re-entry; "
                "run one ordinal per TranslationScenarioSequence"
            )
        if include_controls:
            raise ValueError(
                "translation random phase does not support control actions; "
                "configuration switching requires a redirect source context"
            )
        stop_ordinal = int(start_ordinal) + int(count)
        generated = TranslationScenarioRandomizer(int(seed)).generate(stop_ordinal)[int(start_ordinal) :]
        if not generated:
            return ()
        return (TranslationScenarioPhase(scenario=generated[0].scenario),)

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
        expected_ptw_request_keys = {
            (int(request["vpn"]), int(request["s2xlate"]), int(request["get_gpa"]))
            for request in active["expected_ptw_requests"]
        }
        expected_normal_cfvec_pages = {
            int(page)
            for page, outcome in zip(active["selected_pages"], active["expected_page_outcomes"])
            if bool(outcome.get("ok", False))
        }
        return bool(
            expected_ptw_request_keys.issubset(active["responded_ptw_request_keys"])
            and len(active["expected_fetches"]) == len(active["observed_fetch_pas"])
            and expected_normal_cfvec_pages.issubset(active["observed_normal_cfvec_pages"])
            and int(active["observed_normal_cfvec_count"]) >= 2
        )

    @staticmethod
    def _phase_finished(env) -> bool:
        return TranslationScenarioSequence._phase_complete(env) or bool(
            env.translation_oracle.get_stats()["errors"]
        )

    def initialize_first_phase(self, env, *, reset_cycles: int = 20) -> None:
        """Reset the DUT in the translation mode required by the first phase."""

        first_phase = next((action for action in self.actions if isinstance(action, TranslationScenarioPhase)), None)
        if first_phase is None or first_phase.scenario is None or first_phase.reuse_previous:
            raise ValueError("translation scenario sequence requires an explicit first phase for initialization")
        scenario = first_phase.scenario
        translation_enabled = str(scenario.mode).lower() != "bare" or int(scenario.s2xlate) != 0
        env.initialize(
            reset_vector=int(scenario.va),
            bare_mode=not translation_enabled,
            reset_cycles=int(reset_cycles),
        )

    def run(self, env) -> list[dict]:
        if not self.actions:
            raise ValueError("translation scenario sequence requires at least one action")

        results: list[dict] = []
        previous_state: TranslationScenarioState | None = None
        for action_index, action in enumerate(self.actions):
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

            if isinstance(action, TranslationContextAction):
                record = env.update_translation_context(
                    satp_mode=action.satp_mode,
                    satp_asid=action.satp_asid,
                    satp_ppn=action.satp_ppn,
                    vsatp_mode=action.vsatp_mode,
                    vsatp_asid=action.vsatp_asid,
                    vsatp_ppn=action.vsatp_ppn,
                    hgatp_mode=action.hgatp_mode,
                    hgatp_vmid=action.hgatp_vmid,
                    hgatp_ppn=action.hgatp_ppn,
                    priv_imode=action.priv_imode,
                    priv_virt=action.priv_virt,
                    cycles=int(action.cycles),
                )
                results.append({"kind": "translation_context", "record": record})
                continue

            if isinstance(action, TranslationPmpPmaWriteAction):
                entry = action.entry
                if entry.kind == "pmp":
                    record = env.write_pmp_entry(
                        entry.index,
                        entry.config,
                        entry.addr,
                        size=entry.size,
                        settle_cycles=int(action.settle_cycles),
                    )
                elif entry.kind == "pma":
                    record = env.write_pma_entry(
                        entry.index,
                        entry.config,
                        entry.addr,
                        size=entry.size,
                        settle_cycles=int(action.settle_cycles),
                    )
                else:
                    raise ValueError("translation PMP/PMA write action requires a PMP or PMA entry")
                results.append({"kind": f"{entry.kind}_write", "record": record})
                continue

            if isinstance(action, TranslationPbmteAction):
                record = env.set_translation_pbmte(machine=action.machine, hypervisor=action.hypervisor)
                if action.require_supported and not record["supported"]:
                    raise RuntimeError(
                        "current generated DUT does not expose requested PBMTE control(s): "
                        f"{record['unsupported']}"
                    )
                results.append({"kind": "pbmte", "record": record})
                continue

            if action.redirect:
                raise ValueError(
                    "translation phase redirect requires an explicit source context; "
                    "use reset/re-entry until configuration-switch support is implemented"
                )

            if action.reuse_previous:
                if previous_state is None or action.scenario is not None:
                    raise ValueError("reuse_previous phase must follow a phase and omit scenario")
                state = replace(previous_state, translation_epoch=int(env.translation_epoch))
            else:
                if action.scenario is None:
                    raise ValueError("translation phase requires a scenario")
                state = TranslationScenarioBuilder(env).build(action.scenario)
            scenario = state.scenario
            if not action.expect_ptw and int(action.wait_for_ptw_requests):
                raise ValueError("a no-PTW phase cannot wait for PTW requests")
            if action.page_indexes is not None and (
                not action.page_indexes
                or len(set(action.page_indexes)) != len(action.page_indexes)
                or any(int(page) < 0 or int(page) >= len(state.expected_page_outcomes) for page in action.page_indexes)
            ):
                raise ValueError("translation phase page_indexes must select declared scenario pages")
            if action.target_pc is not None:
                target_pc = int(action.target_pc)
            elif action.page_indexes is not None and len(action.page_indexes) == 1:
                target_pc = int(state.expected_page_outcomes[int(action.page_indexes[0])]["va"])
            else:
                target_pc = int(scenario.va)
            env.monitor.clear()
            env.monitor.set_expected_pc(target_pc)
            env.arm_translation_scenario(
                state,
                page_indexes=action.page_indexes,
                expect_ptw=bool(action.expect_ptw),
            )
            env.translation_oracle.set_fetch_observation_ready(ready=not action.redirect)
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
                    lambda: self._phase_finished(env),
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
            if action.wait_for_completion and action_index < len(self.actions) - 1:
                env.translation_oracle.disarm()
        return results


__all__ = [
    "TranslationContextAction",
    "TranslationPbmteAction",
    "TranslationPmpPmaWriteAction",
    "TranslationScenarioPhase",
    "TranslationScenarioSequence",
    "TranslationSfenceAction",
]
