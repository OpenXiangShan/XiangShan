"""Real-DUT checks for the observable V3 FrontendTrigger contract."""

from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.sequences import LoadProgramSequence
from tests.py.zhaoxinran.test_instr_uncache_port_boundaries import _force_redirect_to


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x8000_0000
_TARGET = _BASE + 0x200
_RECOVERY_TARGET = _BASE + 0x3000
_CNOP = 0x0001
_PIPELINE_OWNER_GROUP = "ifu_v3_pipeline_owner_model"
_BOUNDARY_OWNER_GROUP = "ifu_v3_boundary_owner_model"
_OWNER_LEAVES = {
    "BIN-927": "owner_leaf_029",
    "BIN-928": "owner_leaf_030",
    "BIN-996": "owner_leaf_098",
    "BIN-997": "owner_leaf_099",
    "BIN-998": "owner_leaf_100",
    "BIN-999": "owner_leaf_101",
    "BIN-1000": "owner_leaf_102",
    "BIN-1001": "owner_leaf_103",
    "BIN-1002": "owner_leaf_104",
    "BIN-1003": "owner_leaf_105",
}


def _write(env, name: str, value: int) -> None:
    signal = getattr(env.dut, str(name), None)
    assert signal is not None, f"missing FrontendTrigger DUT input: {name}"
    signal.value = int(value)


def _set_enable_mask(env, mask: int) -> None:
    for slot in range(4):
        _write(env, f"io_csrCtrl_frontend_trigger_tEnableVec_{slot}", (int(mask) >> slot) & 1)
    env.step(2)


def _configure_trigger(
    env,
    slot: int,
    *,
    match_type: int,
    tdata2: int,
    select: int = 0,
    timing: int = 0,
    action: int = 0,
    chain: int = 0,
    debug_mode: int = 0,
    can_raise_bp: int = 1,
) -> None:
    values = {
        "matchType": match_type,
        "select": select,
        "timing": timing,
        "action": action,
        "chain": chain,
        "tdata2": tdata2,
    }
    _write(env, "io_csrCtrl_frontend_trigger_debugMode", debug_mode)
    _write(env, "io_csrCtrl_frontend_trigger_triggerCanRaiseBpExp", can_raise_bp)
    _write(env, "io_csrCtrl_frontend_trigger_tUpdate_bits_addr", slot)
    for field, value in values.items():
        _write(env, f"io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_{field}", value)
    _write(env, "io_csrCtrl_frontend_trigger_tUpdate_valid", 1)
    env.step(2)
    _write(env, "io_csrCtrl_frontend_trigger_tUpdate_valid", 0)
    env.step(2)


def _wait_until(env, predicate, *, max_cycles: int = 3000) -> None:
    for _ in range(int(max_cycles)):
        if predicate():
            return
        env.step(1)
    state = getattr(env.functional_coverage, "_ifu_frontend_trigger_state", {})
    raise AssertionError(
        {
            "reason": "FrontendTrigger directed phase timed out",
            "marked": sorted(state.get("marked", ())),
            "match_type_samples": {
                str(mode): sorted(samples)
                for mode, samples in state.get("match_type_samples", {}).items()
            },
            "suppression_samples": sorted(state.get("suppression_samples", ())),
            "chain_samples": sorted(state.get("chain_samples", ())),
            "action_samples": sorted(state.get("action_samples", ())),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _hit(env, bin_id: str) -> bool:
    group = (
        _PIPELINE_OWNER_GROUP
        if str(bin_id) in {"BIN-927", "BIN-928"}
        else _BOUNDARY_OWNER_GROUP
    )
    return env.functional_coverage.key_hit(group, _OWNER_LEAVES[str(bin_id)])


def _redirect_to_target(env) -> None:
    _force_redirect_to(env, _TARGET)


@pytest.mark.funcov_bins(
    "BIN-927",
    "BIN-928",
    "BIN-996",
    "BIN-997",
    "BIN-998",
    "BIN-999",
    "BIN-1000",
    "BIN-1001",
    "BIN-1002",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_frontend_trigger_config_compare_chain_action_and_lane_alignment(env) -> None:
    payload = int(_CNOP).to_bytes(2, "little") * 8192
    LoadProgramSequence(
        image=ProgramImage(payload=payload, base_addr=_BASE),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)

    _set_enable_mask(env, 0)
    _configure_trigger(env, 0, match_type=0, tdata2=_TARGET)
    _wait_until(
        env,
        lambda: any(
            "disabled" in samples
            for samples in getattr(
                env.functional_coverage, "_ifu_frontend_trigger_state", {}
            ).get("enable_samples", {}).values()
        ),
    )

    _set_enable_mask(env, 0b0001)
    _redirect_to_target(env)
    _wait_until(env, lambda: _hit(env, "BIN-996") and _hit(env, "BIN-997"))

    _configure_trigger(env, 0, match_type=2, tdata2=_TARGET + 0x400)
    _wait_until(
        env,
        lambda: {"hit", "miss"}
        <= getattr(env.functional_coverage, "_ifu_frontend_trigger_state", {})
        .get("match_type_samples", {})
        .get(2, set()),
    )

    _configure_trigger(env, 0, match_type=3, tdata2=_TARGET + 0x800)
    _wait_until(env, lambda: _hit(env, "BIN-998"))

    _configure_trigger(env, 0, match_type=2, tdata2=_BASE, select=1)
    _wait_until(
        env,
        lambda: "select"
        in getattr(env.functional_coverage, "_ifu_frontend_trigger_state", {}).get(
            "suppression_samples", set()
        ),
    )
    _configure_trigger(
        env,
        0,
        match_type=2,
        tdata2=_BASE,
        debug_mode=1,
    )
    _wait_until(env, lambda: _hit(env, "BIN-999"))

    _set_enable_mask(env, 0)
    _configure_trigger(
        env,
        0,
        match_type=2,
        tdata2=_BASE,
        timing=0,
        action=0,
        chain=1,
        debug_mode=0,
    )
    _configure_trigger(
        env,
        1,
        match_type=2,
        tdata2=_BASE,
        timing=0,
        action=1,
        chain=0,
    )
    _set_enable_mask(env, 0b0011)
    _wait_until(
        env,
        lambda: "chain_pass"
        in getattr(env.functional_coverage, "_ifu_frontend_trigger_state", {}).get(
            "chain_samples", set()
        ),
    )

    _configure_trigger(
        env,
        1,
        match_type=2,
        tdata2=_BASE,
        timing=1,
        action=1,
        chain=0,
    )
    _wait_until(env, lambda: _hit(env, "BIN-1000"))

    _set_enable_mask(env, 0b0001)
    _configure_trigger(
        env,
        0,
        match_type=2,
        tdata2=_BASE,
        timing=0,
        action=0,
        chain=0,
        can_raise_bp=1,
    )
    _wait_until(
        env,
        lambda: _hit(env, "BIN-1001") and _hit(env, "BIN-1002"),
    )

    assert all(
        _hit(env, bin_id)
        for bin_id in (
            "BIN-927",
            "BIN-928",
            "BIN-996",
            "BIN-997",
            "BIN-998",
            "BIN-999",
            "BIN-1000",
            "BIN-1001",
            "BIN-1002",
        )
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1003")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_frontend_trigger_redirect_flush_drops_held_identity(env) -> None:
    payload = int(_CNOP).to_bytes(2, "little") * 8192
    LoadProgramSequence(
        image=ProgramImage(payload=payload, base_addr=_BASE),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)

    _configure_trigger(env, 0, match_type=2, tdata2=_BASE)
    _set_enable_mask(env, 0b0001)
    env.backend_model.set_can_accept(0)
    _wait_until(
        env,
        lambda: getattr(
            env.functional_coverage, "_ifu_frontend_trigger_state", {}
        ).get("held_trigger")
        is not None,
    )

    env.monitor.clear()
    env.monitor.notify_redirect(_RECOVERY_TARGET, reason="ctrl_redirect")
    _force_redirect_to(env, _RECOVERY_TARGET)
    _wait_until(env, lambda: _hit(env, "BIN-1003"))
    env.backend_model.set_can_accept(1)
    assert not env.monitor.get_errors()
