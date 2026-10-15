"""Startup contracts: program bytes, reset edges, runtime controls, and failures."""

import hashlib

import pytest

from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend
from env.runtime.frontend_init import (
    FrontendInitConfig,
    LoadProgram,
    ProgramImage,
    initialize_frontend,
)
from env.support.env_config import EnvConfig
from tests.py.icache.directed_dut import (
    support,
    programs,
)
from tests.py.icache.unit.test_icache_directed_support import _BpuTrainingCoverage


@pytest.mark.parametrize(
    "builder,kwargs,digest",
    [
        (
            programs.build_fetch_loop_program,
            {"target_offset": 0},
            "100467028914e7c1bf3bb1485d38c8e0d6739d2093e748bd22b4df353520ca07",
        ),
        (
            programs.build_fetch_loop_program,
            {"target_offset": 0x34},
            "b7f9dd95eebe543cb4ab6cd72bad835c8cf9887e1940c58f2eaf522953604761",
        ),
        (
            programs.build_idle_loop_program,
            {},
            "800657d0d41e5089b6f4165ec4ca6b33499606d133b0b0c4f114ee071ba22c95",
        ),
        (
            programs.build_same_set_jump_program,
            {"base": 0x80600000, "stride": 0x4000, "line_count": 4},
            "1953522ff57f85e2f8cf6ea19b4d3ab2d16486c6daaa1658c10eee7cdc1ee72c",
        ),
    ],
)
def test_relocated_program_builders_preserve_original_bytes(builder, kwargs, digest):
    # Digests captured from the original test-local loaders before relocation.
    assert hashlib.sha256(builder(**kwargs)).hexdigest() == digest


@pytest.mark.parametrize(
    "factory,kwargs,base,first_halfword,latency",
    [
        (
            support.make_cacheable_init_config,
            {"base": 0x80040000, "latency": 7},
            0x80040000,
            0x13,
            7,
        ),
        (support.make_dual_refill_init_config, {}, 0x80060000, 0x0413, 8),
        (
            support.make_fetch_loop_init_config,
            {"base": 0x80040000, "target_offset": 0, "latency": 8},
            0x80040000,
            0x13,
            8,
        ),
        (support.make_overlap2_init_config, {"base": 0x80050000}, 0x80050000, 1, 16),
        (
            support.make_missunit_init_config,
            {"latency": 48, "prefetch_enabled": False, "backend_can_accept": False},
            0x80100000,
            0x13,
            48,
        ),
    ],
)
def test_plans_prepare_program_and_agent_before_all_reset_edges(
    factory,
    kwargs,
    base,
    first_halfword,
    latency,
):
    env = FrontendEnv(FakeDUTFrontend())
    samples = []
    env.register_cycle_observer(
        lambda _, active: samples.append(
            (
                int(active.dut.reset.value),
                active.memory.read_u16(base),
                active.icache_agent.miss_latency,
                int(active.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value),
            )
        )
    )
    original_memory = env.memory.read_block(base, 256)
    config = factory(env.config, **kwargs)
    assert (
        env.current_cycle == 0 and env.memory.read_block(base, 256) == original_memory
    )
    result = initialize_frontend(env, config)
    assert [reset for reset, _, _, _ in samples] == [1] * 20 + [0]
    assert all(
        word == first_halfword and delay == latency for _, word, delay, _ in samples
    )
    assert env.monitor.expected_pc == config.reset_vector
    assert result.completed and result.end_cycle == 21
    if factory is support.make_missunit_init_config:
        assert all(prefetch == 0 for _, _, _, prefetch in samples)
        assert env.backend_model.can_accept == 0


def test_sv39_notification_preserves_the_extra_live_cycle_and_pc():
    env = FrontendEnv(FakeDUTFrontend())
    samples = []
    env.register_cycle_observer(
        lambda _, active: samples.append(
            (
                int(active.dut.reset.value),
                int(active.dut.io_tlbCsr_satp_changed.value),
            )
        )
    )
    result = initialize_frontend(
        env, support.make_sv39_init_config(env.config, reset_vector=0x40000FFE)
    )
    assert samples == [(1, 0)] * 20 + [(0, 0), (0, 1)]
    assert env.dut.io_tlbCsr_satp_changed.value == 0
    assert env.page_table.mode == "sv39"
    assert env.monitor.expected_pc == 0x40000FFE
    assert result.end_cycle == 22


def test_plan_retains_explicit_program_load_edge_and_can_preserve_errors():
    env = FrontendEnv(FakeDUTFrontend())
    initialize_frontend(env, FrontendInitConfig(reset_cycles=0))
    error = {"kind": "must_survive_restart"}
    env.monitor.errors.append(error)
    samples = []
    env.register_cycle_observer(
        lambda _, active: samples.append(int(active.dut.reset.value))
    )
    config = support.make_program_init_config(
        env.config,
        reset_vector=0x80000000,
        reset_cycles=2,
        before_reset=(
            LoadProgram(ProgramImage(b"\x13\0\0\0", 0x80000000), step_cycles=1),
        ),
        after_reset=(),
    )
    result = initialize_frontend(env, config)
    assert samples == [0, 1, 1, 0]
    assert env.get_errors() == [error]
    assert result.program_bytes == 4
    assert result.end_cycle - result.start_cycle == 4


def test_bpu_warmup_failure_is_recorded_and_does_not_apply_later_configuration(
    monkeypatch,
):
    env = FrontendEnv(FakeDUTFrontend())
    env.ready_cycle = 100
    env.missing = None
    env.functional_coverage = _BpuTrainingCoverage(env)
    monkeypatch.setenv("TB_TWO_FETCH_TRAIN_MAX_CYCLES", "2")
    with pytest.raises(AssertionError, match="short-block loop"):
        initialize_frontend(env, support.make_bpu_s3_init_config(env.config))
    result = env.init_result
    assert not result.completed and result.end_cycle == 23
    assert result.steps[-1].operation == "WarmBpuTraining"
    assert (result.steps[-1].start_cycle, result.steps[-1].end_cycle) == (21, 23)
    assert env.icache_agent.miss_latency == 20


def test_reusing_a_plan_does_not_share_monitor_or_memory_state():
    plan = support.make_cacheable_init_config(EnvConfig(), 0x80040000, latency=7)
    first, second = FrontendEnv(FakeDUTFrontend()), FrontendEnv(FakeDUTFrontend())
    initialize_frontend(first, plan)
    first.memory.load_bin(b"\x99", 0x80040000)
    first.monitor.errors.append({"kind": "first_only"})
    initialize_frontend(second, plan)
    assert second.memory.read_u8(0x80040000) == 0x13
    assert second.get_errors() == []
    assert first.init_result is not second.init_result


def test_same_set_startup_preserves_monitor_evidence():
    env = FrontendEnv(FakeDUTFrontend())
    error = {"kind": "retain_for_same_set_training"}
    env.monitor.errors.append(error)
    initialize_frontend(
        env,
        support.make_same_set_jump_init_config(
            env.config,
            0x80600000,
            stride=0x4000,
            line_count=4,
        ),
    )
    assert env.get_errors() == [error]
    assert env.backend_model.can_accept == 1
    assert env.bp_ctrl_mbtb_enable == env.bp_ctrl_ubtb_enable == 1
    assert env.current_cycle == 21


def test_mixed_sources_startup_loads_both_images_and_stops_backend():
    env = FrontendEnv(FakeDUTFrontend())
    result = initialize_frontend(
        env,
        support.make_mixed_sources_init_config(
            env.config,
            idle_base=0x80000000,
            target=0x80A00000,
        ),
    )
    assert env.memory.read_u32(0x80000000) == 0x6F  # JAL x0,0
    assert env.memory.read_u32(0x80A00000 + 62) == 0x13
    assert result.program_bytes == 1280
    assert env.backend_model.can_accept == 0
    assert env.bp_ctrl_mbtb_enable == env.bp_ctrl_ubtb_enable == 0
    assert env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value == 1
    assert env.monitor.expected_pc == 0x80000000
