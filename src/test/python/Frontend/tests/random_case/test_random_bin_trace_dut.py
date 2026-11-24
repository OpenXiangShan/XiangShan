from __future__ import annotations

import json
import logging
import os
from pathlib import Path

import pytest

from env.api import (
    api_Frontend_load_golden_trace,
    api_Frontend_load_program_file,
    api_Frontend_run_until_golden_complete,
)
from env.model import GoldenTrace

from .program_map import build_program_map
from .scenario_contract import scenario_spec_from_dict, write_json_contract
from .scenario_coordinator import ScenarioCoordinator, TimingPlan
from .translation_timing import build_translation_scenario
from .native_runtime_binding import freeze_runtime_contract, verify_runtime_contract


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_RUN_PIPELINE_TEST = os.getenv("TB_BIN_TRACE_PIPELINE") == "1"
logger = logging.getLogger("env.random_bin_trace_test")


def _require_env(name: str) -> str:
    value = os.getenv(name, "").strip()
    if not value:
        raise AssertionError(f"missing required env: {name}")
    return value


def _parse_int(raw: str) -> int:
    return int(str(raw), 0)


def _trace_cycle_limit() -> int:
    value = _parse_int(os.getenv("TB_TRACE_MAX_CYCLES", "0"))
    if value < 0:
        raise AssertionError("TB_TRACE_MAX_CYCLES must be >= 0")
    return value


def _timeline_event_limit(trace_entries: int) -> int:
    configured = _parse_int(os.getenv("TB_RANDOM_MAX_TIMELINE_EVENTS", "0"))
    if configured < 0:
        raise AssertionError("TB_RANDOM_MAX_TIMELINE_EVENTS must be >= 0")
    if configured:
        return configured
    # Long uncache/control-flow traces can emit several DUT-facing events per
    # committed instruction. Size the evidence buffer from the replay itself.
    return max(20_000, int(trace_entries) * 8)


def _build_verified_program_map() -> tuple[dict, Path]:
    source_path = Path(_require_env("TB_ASM_PATH"))
    binary_path = Path(_require_env("TB_BIN_PATH"))
    generator_manifest_path = Path(_require_env("TB_RANDOM_GENERATOR_MANIFEST"))
    output_path = Path(_require_env("TB_RANDOM_PROGRAM_MAP_PATH"))
    metadata = json.loads(generator_manifest_path.read_text(encoding="utf-8"))
    program_map = build_program_map(
        source_path,
        binary_path,
        metadata,
        link_address=_parse_int(os.getenv("TB_RESET_VECTOR", "0x80000000")),
        binary_base=_parse_int(os.getenv("TB_BASE_ADDR", "0x80000000")),
    )
    write_json_contract(output_path, program_map)
    return program_map, output_path


def _oracle_health(stats: dict) -> dict:
    oracle = stats.get("translation_oracle")
    if not isinstance(oracle, dict):
        raise ValueError("translation oracle statistics are missing")
    count, errors = oracle.get("error_count"), oracle.get("errors")
    if type(count) is not int or count < 0 or not isinstance(errors, list):
        raise ValueError("translation oracle statistics have invalid types")
    return {"translation_oracle_error_count": count, "translation_oracle_errors": errors}


def _arm_strict_translation(env, state):
    # Keep the public translation checks armed; the loaded golden trace later
    # scopes fetch requirements and requires exact backend fault squash evidence.
    return env.arm_translation_scenario(
        state, page_indexes=tuple(range(len(state.expected_page_outcomes))), expect_ptw=True,
    )


def _configure_translation_coverage(recorder, page_outcomes) -> bool:
    configure = getattr(recorder, "configure_program_address_mapping", None)
    if not callable(configure):
        return False
    configure(page_outcomes)
    return True


def _require_translation_coverage_mapping(applied) -> None:
    if applied is not True:
        raise AssertionError(
            "strict translation replay completed, but native coverage mapping is unavailable"
        )


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.skipif(not _RUN_PIPELINE_TEST, reason="pipeline-only test")
def test_random_bin_trace(env):
    binary_path = Path(_require_env("TB_BIN_PATH"))
    trace_path = Path(_require_env("TB_TRACE_PATH"))
    scenario_path = Path(_require_env("TB_RANDOM_SCENARIO_SPEC"))
    timeline_path = Path(_require_env("TB_RANDOM_TIMELINE_PATH"))
    base_address = _parse_int(os.getenv("TB_BASE_ADDR", "0x80000000"))
    reset_vector = _parse_int(os.getenv("TB_RESET_VECTOR", hex(base_address)))
    trace_start_index = _parse_int(os.getenv("TB_TRACE_START_INDEX", "0"))

    assert binary_path.is_file(), f"bin file not found: {binary_path}"
    assert trace_path.is_file(), f"trace file not found: {trace_path}"
    scenario = scenario_spec_from_dict(json.loads(scenario_path.read_text(encoding="utf-8")))
    program_map, program_map_path = _build_verified_program_map()
    golden_trace_entries = GoldenTrace.from_file(str(trace_path)).entries[
        trace_start_index:
    ]
    timing_plan_payload = scenario.parameters.get("timing_plan")
    timing_plan = (
        TimingPlan.from_dict(timing_plan_payload)
        if isinstance(timing_plan_payload, dict)
        else TimingPlan.from_seed(scenario.timing_seed)
    )
    coordinator = ScenarioCoordinator(
        scenario_id=scenario.scenario_id,
        timing_plan=timing_plan,
        program_map=program_map,
        maximum_timeline_events=_timeline_event_limit(len(golden_trace_entries)),
    )
    native_contract_path = timeline_path.with_suffix(".native-contract.json")
    native_pin = freeze_runtime_contract(
        env, output=native_contract_path,
        coord_contract=Path(_require_env("TB_RANDOM_COORD_CONTRACT")),
        coord_sha256=_require_env("TB_RANDOM_COORD_CONTRACT_SHA256"),
        run_id=_require_env("TB_RUN_ID"),
        minimum_dynamic_instructions=scenario.minimum_dynamic_instructions,
        input_paths={"asm": Path(_require_env("TB_ASM_PATH")), "bin": binary_path,
                     "trace": trace_path,
                     "generator": Path(_require_env("TB_RANDOM_GENERATOR_MANIFEST")),
                     "scenario": scenario_path},
    )
    coordinator.install(env)

    execution_status = "FAIL"
    timeline_details: dict[str, object] = {
        "scenario_spec_path": str(scenario_path.resolve()),
        "program_map_path": str(program_map_path.resolve()),
        "native_runtime_contract": {"path": str(native_contract_path.resolve()), "sha256": native_pin},
        "native_runtime_postcheck": False,
    }
    try:
        translation_scenario = None
        if scenario.family == "translation_timing_cross":
            translation_scenario = build_translation_scenario(
                scenario,
                binary_path.read_bytes(),
                virtual_base=reset_vector,
                physical_base=int(scenario.parameters["translation"]["physical_base"]),
                guest_physical_base=int(
                    scenario.parameters["translation"]["guest_physical_base"]
                ),
            )
            translation_enabled = (
                str(translation_scenario.mode).lower() != "bare"
                or int(translation_scenario.s2xlate) != 0
            )
            env.initialize(
                reset_vector=int(translation_scenario.va),
                bare_mode=not translation_enabled,
                reset_cycles=20,
            )
            from env.sequences import TranslationScenarioBuilder

            translation_state = TranslationScenarioBuilder(env).build(
                translation_scenario
            )
            coordinator.configure_translation_pages(translation_state.expected_page_outcomes)
            coverage_mapping_applied = _configure_translation_coverage(
                env.functional_coverage, translation_state.expected_page_outcomes
            )
            timeline_details["translation_coverage_mapping_applied"] = coverage_mapping_applied
            pbmte_record = env.set_translation_pbmte(
                machine=translation_scenario.ptw_machine_pbmte,
                hypervisor=translation_scenario.ptw_hypervisor_pbmte,
            )
            timeline_details["translation_pbmte_control"] = pbmte_record
            for control, requested in {
                "machine": translation_scenario.ptw_machine_pbmte,
                "hypervisor": translation_scenario.ptw_hypervisor_pbmte,
            }.items():
                if requested is None:
                    continue
                assert pbmte_record["supported"], (
                    f"requested PBMTE {control} control is unavailable on the DUT: "
                    f"{pbmte_record}"
                )
                assert control in pbmte_record["driven"], (
                    f"requested PBMTE {control} control was not driven on the DUT: "
                    f"{pbmte_record}"
                )
                assert int(pbmte_record["driven"][control]) == int(bool(requested))
            env.backend_model.set_explicit_injection_enabled(
                False,
                reason="translation random bin mode uses golden trace and mapped physical memory",
            )
            env.monitor.clear()
            env.monitor.set_expected_pc(int(translation_scenario.va))
            _arm_strict_translation(env, translation_state)
            binary_size = len(binary_path.read_bytes())
        else:
            env.initialize(reset_vector=reset_vector, bare_mode=True, reset_cycles=20)
            binary_size = api_Frontend_load_program_file(
                env,
                str(binary_path),
                base_address,
                max_cycles=0,
            )
        trace_entries = api_Frontend_load_golden_trace(
            env,
            str(trace_path),
            max_cycles=0,
            start_index=trace_start_index,
        )
        if translation_scenario is not None:
            env.translation_checker.bind_golden_replay(env.backend_model.golden_trace)
        run_ok = api_Frontend_run_until_golden_complete(env, max_cycles=_trace_cycle_limit())
        result = getattr(env, "_last_run_until_golden_result", None)
        assert run_ok is True, (
            "golden trace run stopped unexpectedly"
            if result is None
            else (
                "golden trace run stopped unexpectedly: "
                f"status={result.status} cycles={result.cycles_run} "
                f"cursor={result.cursor}/{result.total_entries} "
                f"pending_work={result.pending_work} "
                f"monitor_errors={result.monitor_error_count}"
            )
        )
        assert result is not None, "golden trace run did not report a stop reason"
        assert result.status == "completed"
        assert result.completed is True
        assert result.cursor >= result.total_entries
        assert result.pending_work == 0
        assert binary_size > 0
        assert trace_entries >= scenario.minimum_dynamic_instructions
        assert type(result.monitor_error_count) is int and result.monitor_error_count == 0
        assert not env.monitor.get_errors()
        assert not coordinator.has_active_action, (
            "golden trace completed while a scheduled timing action was still active: "
            + ",".join(coordinator.unrealized_action_ids)
        )
        assert not coordinator.unrealized_action_ids, (
            "scheduled timing actions were not realized: "
            + ",".join(coordinator.unrealized_action_ids)
        )

        realized = {event.event: event.count for event in coordinator.realized_events()}
        for requirement in scenario.required_events:
            if requirement.evidence_stage != "dut":
                continue
            assert realized.get(requirement.event, 0) >= requirement.min_count, (
                f"DUT scenario event was requested but not realized: {requirement.event} "
                f"got={realized.get(requirement.event, 0)} required={requirement.min_count}"
            )
        if translation_scenario is not None:
            assert coordinator.translation_mapping_valid, (
                "translation program mapping missing or invalidated during replay"
            )
            # A passing golden-trace/checker run is insufficient: the translation
            # oracle validates PTW context, translated fetches, and cfVec faults.
            env.assert_translation_scenario()
            _require_translation_coverage_mapping(coverage_mapping_applied)
        verify_runtime_contract(native_contract_path, native_pin)
        timeline_details["native_runtime_postcheck"] = True
        execution_status = "REALIZED"
        timeline_details.update(
            {
                "cycles_run": int(result.cycles_run),
                "golden_cursor": int(result.cursor),
                "golden_entries": int(result.total_entries),
                "monitor_errors": result.monitor_error_count,
                "pending_work": int(result.pending_work),
                "completed": bool(result.completed),
            }
        )
        logger.info(
            "random scenario complete: id=%s cycles=%d cursor=%d actions=%d",
            scenario.scenario_id,
            result.cycles_run,
            result.cursor,
            coordinator.action_count,
        )
    except BaseException as exc:
        timeline_details["error_type"] = type(exc).__name__
        timeline_details["error"] = str(exc)
        raise
    finally:
        try:
            timeline_details.update(_oracle_health(env.get_stats()))
        except Exception as exc:
            timeline_details["translation_oracle_stats_error"] = str(exc)
        coordinator.write_timeline(
            timeline_path,
            execution_status=execution_status,
            details=timeline_details,
        )
