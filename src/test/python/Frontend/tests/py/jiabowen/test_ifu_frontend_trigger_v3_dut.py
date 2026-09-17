"""Real-DUT checks for the observable V3 FrontendTrigger contract."""

from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.sequences import LoadProgramSequence
from tests.py.support.uncache_scenarios import _force_redirect_to


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


def _trigger_reader(env, paths):
    from env.funcov.py.ifu.compact_funcov import _FRONTEND_TRIGGER_PREFIXES, _IFU_INTERNAL_PREFIXES

    def read(stem, trigger=False):
        values = {}
        for prefix in (_FRONTEND_TRIGGER_PREFIXES if trigger else _IFU_INTERNAL_PREFIXES):
            value = env.functional_coverage._try_read_dut_signal(env.dut, prefix + stem)
            if value is not None:
                values[prefix + stem] = int(value)
        assert values, {"missing_trigger_checker_probe": stem}
        assert len(set(values.values())) == 1, {"alias_mismatch": values}
        paths[stem] = list(values)
        return next(iter(values.values()))
    return read


def _install_pc_trigger_checker(env):
    """Independent five-field / single-trigger checker for the CNOP program."""
    from env.support.pc_utils import fold_pc

    proof = dict(paths={}, checked_lanes=0, outcomes=set(), held=None, flushed=[], updates=[])
    read = _trigger_reader(env, proof["paths"])

    def observe(cycle, _env):
        valid, ready = read("io_toIBuffer_valid"), read("io_toIBuffer_ready")
        if read("s2_flush") == 1:
            if proof["held"] is not None and read("io_fromFtq_redirect_valid") == 1:
                assert valid == 0, "flushed held trigger must not transfer"
                proof["flushed"].append(dict(cycle=cycle, held=proof["held"]))
            proof["held"] = None
            return
        if read("s2_valid_valid") != 1 or valid != 1:
            return
        config = {f: read(f"tdataVec_0_{f}", True)
                  for f in ("matchType", "select", "action", "chain", "tdata2")}
        assert config["chain"] == 0  # This checker does not model unavailable timing chains.
        enabled = read("triggerEnableVec_0", True)
        assert all(read(f"triggerEnableVec_{i}", True) == 0 for i in (1, 2, 3))
        debug, allow_bp = read("io_frontendTrigger_debugMode"), read("io_frontendTrigger_triggerCanRaiseBpExp")
        mask = read("io_toIBuffer_bits_enqEnable") & read("io_toIBuffer_bits_valid")
        held = []
        for lane in range(35):
            if not (mask >> lane) & 1:
                continue
            pc = read(f"s2_alignedInstrPcVec_{lane}_addr") << 1
            mode, threshold = config["matchType"], config["tdata2"]
            raw = pc == threshold if mode == 0 else pc >= threshold if mode == 2 else pc < threshold if mode == 3 else False
            expected_hit = int(raw and enabled and not config["select"] and not debug)
            assert read(f"triggerHitVec_{lane}_0", True) == expected_hit
            fire_stem = "triggerCanFireVec_0" if lane == 0 else f"triggerCanFireVec_{lane}_0"
            assert read(fire_stem, True) == expected_hit
            action = 15 if not expected_hit else 1 if config["action"] == 1 else 0 if config["action"] == 0 and allow_bp else 15
            assert read(f"io_toIBuffer_bits_triggered_{lane}") == action
            assert read(f"s2_alignedInstrVec_{lane}_data") & 0xFFFF == _CNOP
            assert read(f"s2_alignedInstrVec_{lane}_isRvc") == 1
            assert read(f"s2_alignedPdInfoVec_{lane}_isRVC") == 1
            assert read(f"s2_alignedPdInfoVec_{lane}_brAttribute_branchType") == 0
            assert read(f"s2_alignedPdInfoVec_{lane}_brAttribute_rasAction") == 0
            assert read(f"io_toIBuffer_bits_instrs_{lane}") == 0x13
            assert read(f"io_toIBuffer_bits_foldpc_{lane}") == fold_pc(pc)
            owner = read(f"s2_alignedInstrVec_{lane}_blockSel") | read(f"s2_alignedInstrVec_{lane}_isCrossBlockInstr")
            ftq = [read(f"s2_fetchBlock_{owner}_ftqIdx_{f}") for f in ("flag", "value")]
            assert [read(f"io_toIBuffer_bits_ftqPtr_{lane}_{f}") for f in ("flag", "value")] == ftq
            assert read(f"io_toIBuffer_bits_instrEndOffset_{lane}_offset") == read(f"s2_alignedInstrVec_{lane}_endOffset")
            proof["checked_lanes"] += 1
            proof["outcomes"].add((mode, config["select"], debug, enabled, expected_hit, action, allow_bp))
            if action != 15 and not ready:
                held.append(dict(pc=pc, lane=lane, ftq=ftq, action=action))
        proof["held"] = dict(cycle=cycle, lanes=held) if held else None

    env.register_cycle_observer(observe)
    env._trigger_pc_checker = proof
    return proof


def _save_pc_trigger_checker(env):
    proof = env._trigger_pc_checker
    assert proof["checked_lanes"] > 0
    env.functional_coverage.risk_observations.append(dict(
        event="frontend_trigger_independent_pc_checkpoint",
        **{k:v for k,v in proof.items() if k != "outcomes"},
        outcomes=[list(x) for x in sorted(proof["outcomes"])], no_timing_default=True))


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
    timing: int | None = None,
    action: int = 0,
    chain: int = 0,
    debug_mode: int = 0,
    can_raise_bp: int = 1,
) -> None:
    paths = {}
    read = _trigger_reader(env, paths)
    fields = ("matchType", "select", "action", "chain", "tdata2")
    before = [[read(f"tdataVec_{i}_{f}", True) for f in fields] for i in range(4)]
    values = {
        "matchType": match_type,
        "select": select,
        "action": action,
        "chain": chain,
        "tdata2": tdata2,
    }
    # None means this testcase makes no timing assertion or drive. A caller
    # explicitly requesting 0/1 still fails on the missing current-DUT pin.
    if timing is not None:
        values["timing"] = timing
    _write(env, "io_csrCtrl_frontend_trigger_debugMode", debug_mode)
    _write(env, "io_csrCtrl_frontend_trigger_triggerCanRaiseBpExp", can_raise_bp)
    _write(env, "io_csrCtrl_frontend_trigger_tUpdate_bits_addr", slot)
    for field, value in values.items():
        _write(env, f"io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_{field}", value)
    _write(env, "io_csrCtrl_frontend_trigger_tUpdate_valid", 1)
    env.step(2)
    _write(env, "io_csrCtrl_frontend_trigger_tUpdate_valid", 0)
    env.step(2)
    after = [[read(f"tdataVec_{i}_{f}", True) for f in fields] for i in range(4)]
    assert after[slot] == [values[f] for f in fields]
    assert all(after[i] == before[i] for i in range(4) if i != slot)
    if hasattr(env, "_trigger_pc_checker"):
        env._trigger_pc_checker["updates"].append(dict(cycle=env.current_cycle, slot=slot,
            values=values, before=before, after=after, paths=paths))


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


@pytest.mark.parametrize("timing_mismatch", [
    pytest.param(False, id="current-pc-contract", marks=pytest.mark.funcov_bins(
        "BIN-927", "BIN-928", "BIN-996", "BIN-997", "BIN-998", "BIN-999", "BIN-1001", "BIN-1002")),
    pytest.param(True, id="legacy-timing-contract", marks=pytest.mark.funcov_bins("BIN-1000")),
])
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_frontend_trigger_config_compare_chain_action_and_lane_alignment(env, timing_mismatch) -> None:
    payload = int(_CNOP).to_bytes(2, "little") * 8192
    LoadProgramSequence(
        image=ProgramImage(payload=payload, base_addr=_BASE),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)

    if not timing_mismatch:
        _install_pc_trigger_checker(env)
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

    if timing_mismatch:
        # Retain the original explicit timing scenario for a capable module
        # environment / design review. It is not replaced by a chain-only HIT.
        _set_enable_mask(env, 0)
        _configure_trigger(env, 0, match_type=2, tdata2=_BASE, timing=0,
                           action=0, chain=1, debug_mode=0)
        _configure_trigger(env, 1, match_type=2, tdata2=_BASE, timing=0,
                           action=1, chain=0)
        _set_enable_mask(env, 0b0011)
        _wait_until(env, lambda: "chain_pass" in
                    getattr(env.functional_coverage, "_ifu_frontend_trigger_state", {}).get("chain_samples", set()))
        _configure_trigger(env, 1, match_type=2, tdata2=_BASE, timing=1, action=1, chain=0)
        _wait_until(env, lambda: _hit(env, "BIN-1000"))
    else:
        _configure_trigger(env, 0, match_type=2, tdata2=_BASE,
                           action=1, debug_mode=0)
        _wait_until(env, lambda: "debug_action" in
                    getattr(env.functional_coverage, "_ifu_frontend_trigger_state", {}).get("action_samples", set()))

    _set_enable_mask(env, 0b0001)
    _configure_trigger(
        env,
        0,
        match_type=2,
        tdata2=_BASE,
        timing=0 if timing_mismatch else None,
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
            "BIN-1001",
            "BIN-1002",
        )
    )
    assert _hit(env, "BIN-1000") is timing_mismatch
    if not timing_mismatch:
        _configure_trigger(env, 0, match_type=2, tdata2=_BASE,
                           action=0, can_raise_bp=0)
        _wait_until(env, lambda: (2, 0, 0, 1, 1, 15, 0)
                    in env._trigger_pc_checker["outcomes"])
        _save_pc_trigger_checker(env)
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

    proof = _install_pc_trigger_checker(env)
    _configure_trigger(env, 0, match_type=3, tdata2=_BASE + 0x1000)
    _set_enable_mask(env, 0b0001)
    env.backend_model.set_can_accept(0)
    _wait_until(
        env,
        lambda: getattr(
            env.functional_coverage, "_ifu_frontend_trigger_state", {}
        ).get("held_trigger")
        is not None,
    )

    assert proof["held"] is not None
    recovery_start = len(env.monitor.observations)
    _force_redirect_to(env, _RECOVERY_TARGET)
    _wait_until(env, lambda: _hit(env, "BIN-1003"))
    env.backend_model.set_can_accept(1)
    assert len(proof["flushed"]) == 1, proof["flushed"]
    flush_cycle = proof["flushed"][0]["cycle"]
    # Queuing a backend event does not yet assert the DUT redirect; older
    # IBuffer traffic before that physical flush is legal. No post-flush
    # observation is discarded, even if it would belong to the old path.
    _wait_until(env, lambda: sum(item.cycle > flush_cycle for item in
                                env.monitor.observations[recovery_start:]) >= 16)
    delivered = [item for item in env.monitor.observations[recovery_start:]
                 if item.cycle > flush_cycle]
    assert [item.pc for item in delivered] == [_RECOVERY_TARGET + 2 * i for i in range(len(delivered))]
    assert all(item.instr == 0x13 and item.is_rvc for item in delivered)
    proof["recovery_delivery"] = [dict(cycle=item.cycle, pc=item.pc, instr=item.instr)
                                  for item in delivered]
    _save_pc_trigger_checker(env)
    assert not env.monitor.get_errors()
