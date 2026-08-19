from __future__ import annotations

from dataclasses import dataclass, replace

from .translation_scenarios import TranslationPmpPmaEntry, TranslationScenario, TranslationScenarioBuilder, TranslationScenarioState


@dataclass(frozen=True)
class TranslationScenarioPhase:
    """One translation context in a directed multi-action scenario."""

    scenario: TranslationScenario | None = None
    reuse_previous: bool = False
    redirect: bool = True
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
        return bool((not active["expected_ptw_requests"] or active["response_seen"]) and active["fetch_seen"])

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
            if action.redirect:
                env.backend_model.inject_redirect(
                    target_pc,
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


__all__ = [
    "TranslationContextAction",
    "TranslationPbmteAction",
    "TranslationPmpPmaWriteAction",
    "TranslationScenarioPhase",
    "TranslationScenarioSequence",
    "TranslationSfenceAction",
]
