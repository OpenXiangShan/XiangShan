"""Behavioral contracts for shared Frontend startup and fixture plans."""

import json
from dataclasses import replace

import pytest

from env import api
from env.core.frontend_env import FrontendEnv
from env.core.transactions import ProgramImage
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.runtime.dut_factory import FakeDUTFrontend
from env.runtime.frontend_init import (
    ArmTranslationScenario,
    ClearMonitor,
    ConfigureAgent,
    FrontendInitConfig,
    InitCallback,
    LoadGoldenTrace,
    LoadProgram,
    LoadProgramFile,
    MapPage,
    PrepareTranslationScenario,
    SetMemoryRegion,
    SetBpCtrl,
    SetExpectedPc,
    WaitCycles,
    WritePma,
    WritePmp,
    initialize_frontend,
    run_initialization_steps,
)
from env.sequences.translation_scenarios import TranslationScenario
from env.support.env_config import EnvConfig, SV39_ENV_CONFIG
from env.support.pmp_pma import PmpPmaConfig


@pytest.fixture(autouse=True)
def _disable_dut_coverage(monkeypatch):
    # These tests exercise real Env behavior against pins, without compiled RTL.
    monkeypatch.setenv("TB_ENABLE_FUNCTIONAL_COVERAGE", "0")
    monkeypatch.setenv("TB_ENABLE_TOFFEE_LINE_COVERAGE", "0")
    monkeypatch.delenv("TB_RESET_VECTOR", raising=False)
    monkeypatch.delenv("TB_RESET_CYCLES", raising=False)


@pytest.fixture
def dut():
    return FakeDUTFrontend()


def _env(config=None):
    return FrontendEnv(FakeDUTFrontend(), config=config)


@pytest.mark.parametrize(
    "frontend_init_config",
    [
        FrontendInitConfig(
            env_config=replace(
                SV39_ENV_CONFIG, icache=replace(EnvConfig().icache, miss_latency=37)
            ),
            reset_vector=0x40000000,
            reset_cycles=2,
            before_reset=(
                LoadProgram(ProgramImage(b"\x13\0\0\0" * 16, 0x80001000)),
                MapPage(0x40000, 0x80001, {"r": 1, "x": 1}),
            ),
        )
    ],
    indirect=True,
)
def test_fixture_applies_custom_mode_program_mapping_and_agent_config(env):
    assert env.dut.io_tlbCsr_satp_mode.value == 8
    assert env.dut.io_tlbCsr_priv_imode.value == 1
    assert env.dut.io_reset_vector_addr.value << 1 == 0x40000000
    assert env.memory.read_u32(0x80001000) == 0x13
    pa, ok, _metadata = env.page_table.translate(0x40000000)
    assert ok and pa == 0x80001000
    assert env.icache_agent.miss_latency == 37
    assert len(env.init_history) == 1
    assert env.init_result.program_bytes == 64
    assert env.init_result.completed


def test_reset_release_observers_see_prepared_state_and_ordered_phases():
    env = _env()
    events = []
    env.register_cycle_observer(
        lambda _cycle, tb: events.append(
            ("edge", tb.dut.reset.value, tb.memory.read_u32(0x80000000))
        )
    )
    plan = FrontendInitConfig(
        reset_vector=0x80000000,
        reset_cycles=2,
        before_reset=(LoadProgram(ProgramImage(b"\x13\0\0\0", 0x80000000)),),
        before_release=(
            InitCallback(lambda tb: events.append(("armed", tb.dut.reset.value))),
        ),
        after_reset=(
            InitCallback(lambda tb: events.append(("after", tb.dut.reset.value))),
        ),
    )
    result = initialize_frontend(env, plan)
    assert events == [
        ("edge", 1, 0x13),
        ("edge", 1, 0x13),
        ("armed", 1),
        ("edge", 0, 0x13),
        ("after", 0),
    ]
    assert [step.phase for step in result.steps] == [
        "before_reset",
        "before_release",
        "after_reset",
    ]
    assert result.end_cycle - result.start_cycle == 3


def test_explicit_settings_override_environment_then_sequence_defaults(monkeypatch):
    env = _env(
        replace(
            EnvConfig(),
            sequence=replace(
                EnvConfig().sequence, reset_vector=0x80002000, reset_cycles=3
            ),
        )
    )
    result = initialize_frontend(env)
    assert (result.reset_vector, result.reset_cycles) == (0x80002000, 3)
    monkeypatch.setenv("TB_RESET_VECTOR", "0x80004000")
    monkeypatch.setenv("TB_RESET_CYCLES", "0x2")
    result = initialize_frontend(env)
    assert (result.reset_vector, result.reset_cycles) == (0x80004000, 2)
    result = initialize_frontend(
        env, FrontendInitConfig(reset_vector=0x80006000, reset_cycles=1)
    )
    assert (result.reset_vector, result.reset_cycles) == (0x80006000, 1)


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
def test_deferred_fixture_does_not_clock_or_release_the_dut(env):
    assert env.dut.reset.value == 1
    assert env.current_cycle == 0
    assert "init_history" not in vars(env)
    initialize_frontend(env, FrontendInitConfig(reset_cycles=2))
    assert env.dut.reset.value == 0
    assert len(env.init_history) == 1


def test_shared_plan_creates_independent_trace_cursors_and_model_state():
    source = GoldenTrace(
        [TraceEntry(0, 0x80000000, 0x13, 4), TraceEntry(1, 0x80000004, 0x13, 4)]
    )
    plan = FrontendInitConfig(
        reset_cycles=0, after_reset=(LoadGoldenTrace(factory=lambda: source),)
    )
    first, second = _env(), _env()
    initialize_frontend(first, plan)
    first.backend_model.golden_trace.reset(1)
    initialize_frontend(second, plan)
    assert first.backend_model.golden_trace.cursor == 1
    assert second.backend_model.golden_trace.cursor == source.cursor == 0
    assert first.backend_model.golden_trace is not second.backend_model.golden_trace
    assert first.init_result.trace_entries == second.init_result.trace_entries == 2
    assert (
        first.backend_model.backend_mode
        == second.backend_model.backend_mode
        == "auto_bin"
    )
    first.memory.load_bin(b"\x99", 0x80000000)
    assert second.memory.read_u8(0x80000000) != 0x99


def test_subscenario_restart_preserves_errors_coverage_and_clock_callbacks():
    env = _env()
    env.functional_coverage = {"existing_hit": True}
    error = {"kind": "existing_checker_error"}
    env.monitor.errors.append(error)
    callbacks = len(env.dut._step_ris_callbacks)
    initialize_frontend(
        env, FrontendInitConfig(reset_vector=0x80000000, reset_cycles=1)
    )
    initialize_frontend(
        env, FrontendInitConfig(reset_vector=0x80001000, reset_cycles=1)
    )
    assert env.get_errors() == [error]
    assert env.functional_coverage == {"existing_hit": True}
    assert len(env.dut._step_ris_callbacks) == callbacks
    assert [item.reset_vector for item in env.init_history] == [0x80000000, 0x80001000]


def test_initialization_failure_preserves_original_exception_and_partial_result(
    tmp_path,
):
    env = _env()
    plan = FrontendInitConfig(
        reset_cycles=0,
        after_reset=(LoadProgramFile(str(tmp_path / "missing.bin"), 0x80000000),),
    )
    with pytest.raises(FileNotFoundError):
        initialize_frontend(env, plan)
    assert env.init_result.completed is False
    assert env.init_result.program_bytes == 0
    assert env.init_result.steps[-1].phase == "after_reset"


def test_monitor_clearing_and_predictor_controls_are_explicit_recorded_steps():
    env = _env()
    env.monitor.errors.append({"kind": "retained_until_requested"})
    env.functional_coverage = {"existing_hit": True}
    result = initialize_frontend(
        env,
        FrontendInitConfig(
            reset_cycles=0,
            after_reset=(
                ClearMonitor(),
                SetExpectedPc(0x80001000),
                SetBpCtrl(ubtb_enable=0, abtb_enable=0, mbtb_enable=1),
            ),
        ),
    )
    assert env.get_errors() == []
    assert env.functional_coverage == {"existing_hit": True}
    assert env.monitor.expected_pc == 0x80001000
    assert env.csr_ctrl_if.io_csrCtrl_bp_ctrl_ubtbEnable.value == 0
    assert env.csr_ctrl_if.io_csrCtrl_bp_ctrl_abtbEnable.value == 0
    assert env.csr_ctrl_if.io_csrCtrl_bp_ctrl_mbtbEnable.value == 1
    assert [step.operation for step in result.steps] == [
        "ClearMonitor",
        "SetExpectedPc",
        "SetBpCtrl",
    ]
    assert result.end_cycle - result.start_cycle == 1


@pytest.mark.parametrize("cycles", [0, 300])
def test_runtime_wait_preserves_startup_evidence_and_records_observed_cycles(cycles):
    env = _env()
    startup = initialize_frontend(env, FrontendInitConfig(reset_cycles=1))
    startup_metadata = startup.metadata()
    start = env.current_cycle
    error = {"kind": "retained_during_wait"}
    env.monitor.errors.append(error)
    env.functional_coverage = {"existing_hit": True}
    observed = []
    env.register_cycle_observer(
        lambda cycle, tb: observed.append((cycle, int(tb.dut.reset.value)))
    )

    records = run_initialization_steps(
        env, (WaitCycles(cycles),), phase="sram_init_wait"
    )

    assert env.current_cycle == start + cycles
    assert observed == [(cycle, 0) for cycle in range(start + 1, start + cycles + 1)]
    assert env.init_result is startup and startup.metadata() == startup_metadata
    assert env.init_history == [startup]
    assert env.get_errors() == [error]
    assert env.functional_coverage == {"existing_hit": True}
    assert records == env.init_preparation_history
    assert records[0].phase == "sram_init_wait"
    assert (records[0].start_cycle, records[0].end_cycle) == (start, start + cycles)


def test_negative_runtime_wait_fails_without_clocking_or_restarting():
    env = _env()
    startup = initialize_frontend(env, FrontendInitConfig(reset_cycles=1))
    start = env.current_cycle

    with pytest.raises(ValueError, match="wait cycles must be non-negative"):
        run_initialization_steps(env, (WaitCycles(-1),), phase="sram_init_wait")

    assert env.current_cycle == start and env.init_history == [startup]
    record = env.init_preparation_history[-1]
    assert record.start_cycle == record.end_cycle == start


def test_legacy_load_apis_preserve_return_values_cursor_and_cycle_timing(tmp_path):
    env = _env()
    initialize_frontend(env, FrontendInitConfig(reset_cycles=0))
    program = tmp_path / "program.bin"
    program.write_bytes(b"\x13\0\0\0" * 4)
    trace = tmp_path / "program.json"
    trace.write_text(
        json.dumps(
            [
                {"pc": 0x80000000 + 4 * index, "instr": 0x13, "size": 4}
                for index in range(4)
            ]
        )
    )
    start = env.current_cycle
    assert api.api_Frontend_load_program(env, [0x13], 0x80001000, max_cycles=0) == 4
    assert (
        api.api_Frontend_load_program_file(env, str(program), 0x80000000, max_cycles=0)
        == 16
    )
    assert (
        api.api_Frontend_load_golden_trace(env, str(trace), max_cycles=2, start_index=1)
        == 4
    )
    assert env.current_cycle == start + 4
    assert env.backend_model.golden_trace.cursor == 1
    assert env.memory.read_u32(0x80001000) == env.memory.read_u32(0x80000000) == 0x13
    assert [
        (record.start_cycle, record.end_cycle)
        for record in env.init_preparation_history
    ] == [
        (start, start + 1),
        (start + 1, start + 2),
        (start + 2, start + 4),
    ]
    assert len(env.init_history) == 1


def test_program_trace_plan_preserves_program_load_edge_before_trace_attachment(tmp_path):
    program = tmp_path / "program.bin"
    program.write_bytes(b"\x13\0\0\0" * 4)
    trace = tmp_path / "program.json"
    trace.write_text(
        json.dumps(
            [
                {"pc": 0x80000000 + 4 * index, "instr": 0x13, "size": 4}
                for index in range(4)
            ]
        )
    )
    env = _env()
    edge_cursors = []
    env.register_cycle_observer(
        lambda cycle, tb: edge_cursors.append((cycle, tb.backend_model.golden_trace))
    )
    result = initialize_frontend(
        env,
        FrontendInitConfig(
            reset_vector=0x80000000,
            bare_mode=True,
            reset_cycles=20,
            after_reset=(
                LoadProgramFile(str(program), 0x80000000, step_cycles=1),
                LoadGoldenTrace(path=trace, start_index=0),
            ),
        ),
    )
    assert result.program_bytes == 16 and result.trace_entries == 4
    assert env.current_cycle == 22
    assert all(trace is None for _cycle, trace in edge_cursors)
    assert env.backend_model.golden_trace.cursor == 0
    assert env.memory.read_u32(0x80000000) == 0x13


def test_uncache_regions_permissions_and_response_configuration_are_applied():
    env = _env()
    permission = PmpPmaConfig(match="napot", read=True, execute=True)
    plan = FrontendInitConfig(
        reset_cycles=0,
        before_reset=(
            SetMemoryRegion(0x10001000, 0x10002000),
            ConfigureAgent("uncache", {"latency": 8, "mmio_latency": 16}),
        ),
        after_reset=(
            WritePmp(0, permission, 0x10001000, size=0x1000),
            WritePma(0, replace(permission, cacheable=False), 0x10001000, size=0x1000),
        ),
    )
    initialize_frontend(env, plan)
    assert (0x10001000, 0x10002000) in env.memory.mmio_ranges
    assert env.uncache_agent.latency == 8
    assert env.uncache_agent.mmio_latency == 16
    assert len(env.csr_write_log) == 4


@pytest.mark.parametrize("arm", [True, False])
def test_translation_plan_controls_oracle_before_first_active_edge(arm):
    env = _env()
    scenario = TranslationScenario(
        "init-phase", 0x40000000, 0x80001000, b"\x13\0\0\0" * 64
    )
    released = []
    env.register_cycle_observer(
        lambda _cycle, tb: (
            released.append(tb.translation_oracle.get_stats())
            if not tb.dut.reset.value
            else None
        )
    )
    result = initialize_frontend(
        env,
        FrontendInitConfig(
            bare_mode=False,
            reset_vector=scenario.va,
            reset_cycles=2,
            before_release=(PrepareTranslationScenario(scenario, arm=arm),),
        ),
    )
    assert len(released) == 1
    assert (released[0]["active"] is not None) == arm
    assert result.steps[0].value.scenario is scenario
    assert env.memory.read_u32(scenario.pa) == 0x13


@pytest.mark.parametrize(
    "page_indexes,expect_ptw,selected_pages",
    [
        (None, True, [0, 1]),
        ((0,), True, [0]),
        ((0, 1), True, [0, 1]),
        ((1,), False, [1]),
    ],
)
def test_runtime_translation_activation_preserves_pages_context_and_startup(
    page_indexes, expect_ptw, selected_pages
):
    env = _env()
    startup = initialize_frontend(env, FrontendInitConfig(reset_cycles=1))
    startup_metadata = startup.metadata()
    scenario = TranslationScenario(
        "runtime-activation",
        0x40000000,
        0x80001000,
        b"\x13\0\0\0" * 2048,
        page_count=2,
    )
    error = {"kind": "retained_until_activation"}
    env.monitor.errors.append(error)
    env.functional_coverage = {"existing_hit": True}
    state = run_initialization_steps(
        env,
        (PrepareTranslationScenario(scenario, arm=False),),
        phase="translation_prepare",
    )[0].value
    assert state.scenario is scenario
    assert env.translation_oracle.get_active() is None
    assert env.get_errors() == [error]
    activation_cycle = env.current_cycle

    records = run_initialization_steps(
        env,
        (
            ClearMonitor(),
            SetExpectedPc(scenario.va),
            ArmTranslationScenario(state, page_indexes, expect_ptw),
        ),
        phase="translation_activate",
    )

    active = env.translation_oracle.get_active()
    assert active["selected_pages"] == selected_pages
    assert len(active["expected_ptw_requests"]) == (
        len(selected_pages) if expect_ptw else 0
    )
    assert records[-1].value == active
    assert env.monitor.expected_pc == scenario.va and env.get_errors() == []
    # ClearMonitor discards this context; arm must restore it after the clear.
    assert env.flow_checker._translation_s2xlate == scenario.s2xlate
    assert env.current_cycle == activation_cycle
    assert env.init_result is startup and startup.metadata() == startup_metadata
    assert env.init_history == [startup]
    assert env.functional_coverage == {"existing_hit": True}
    assert all(
        record.phase == "translation_activate"
        and record.start_cycle == record.end_cycle == activation_cycle
        for record in records
    )


@pytest.mark.parametrize("invalid_state", ["stale_epoch", "undeclared_page"])
def test_runtime_arm_preserves_validation_errors_without_reset_or_monitor_clear(
    invalid_state,
):
    env = _env()
    startup = initialize_frontend(env, FrontendInitConfig(reset_cycles=1))
    scenario = TranslationScenario(
        "invalid-runtime-activation", 0x40000000, 0x80001000, b"\x13\0\0\0" * 16
    )
    state = run_initialization_steps(
        env, (PrepareTranslationScenario(scenario, arm=False),)
    )[0].value
    if invalid_state == "stale_epoch":
        env.update_translation_context(satp_asid=1)
        step = ArmTranslationScenario(state)
        message = "cannot arm translation scenario from epoch"
    else:
        step = ArmTranslationScenario(state, page_indexes=(1,))
        message = "page_indexes must select declared scenario pages"
    start = env.current_cycle
    error = {"kind": "must_survive_failed_arm"}
    env.monitor.errors.append(error)

    with pytest.raises(ValueError, match=message):
        run_initialization_steps(env, (step,), phase="translation_activate")

    assert env.current_cycle == start and env.init_history == [startup]
    assert env.get_errors() == [error]
    assert env.translation_oracle.get_active() is None
    record = env.init_preparation_history[-1]
    assert record.start_cycle == record.end_cycle == start


@pytest.mark.parametrize(
    "config", [FrontendInitConfig(reset_vector=3), FrontendInitConfig(reset_cycles=-1)]
)
def test_invalid_startup_settings_fail_before_clocking(config):
    env = _env()
    with pytest.raises(ValueError):
        initialize_frontend(env, config)
    assert env.current_cycle == 0
