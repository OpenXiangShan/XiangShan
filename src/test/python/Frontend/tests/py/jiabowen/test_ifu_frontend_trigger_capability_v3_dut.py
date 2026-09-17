"""Current ABI canary; deliberately not an owner-bin closure testcase.

Exercises only exported config fields, never supplies a missing timing value.
The timing-dependent producer must remain visibly ineligible after migration.
"""

import os

import pytest

from env.core.transactions import ProgramImage
from env.sequences import LoadProgramSequence
from env.funcov.py.ifu.compact_funcov import (
    _FRONTEND_TRIGGER_PREFIXES, _IFU_INTERNAL_PREFIXES,
)
from tests.py.jiabowen.test_ifu_frontend_trigger_v3_dut import _write, _set_enable_mask
from tests.py.support.uncache_scenarios import _force_redirect_to


_BASE = 0x80000000
_FIELDS = ("matchType", "select", "action", "chain", "tdata2")


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
def test_frontend_trigger_exported_config_and_pc_compare_canary(env):
    recorder = env.functional_coverage
    paths, results = {}, []

    def read(stem, *, trigger=False):
        prefixes = _FRONTEND_TRIGGER_PREFIXES if trigger else _IFU_INTERNAL_PREFIXES
        values = {}
        for prefix in prefixes:
            value = recorder._try_read_dut_signal(env.dut, prefix + stem)
            if value is not None:
                values[prefix + stem] = int(value)
        assert values, {"missing_canary_probe": stem}
        assert len(set(values.values())) == 1, {"alias_mismatch": values}
        paths[stem] = list(values)
        return next(iter(values.values()))

    LoadProgramSequence(image=ProgramImage(
        payload=b"\x01\x00" * 8192, base_addr=_BASE), step_cycles=0).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    assert getattr(env.dut, "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing", None) is None
    for slot in range(4):
        assert all(recorder._try_read_dut_signal(env.dut, prefix + f"tdataVec_{slot}_timing") is None
                   for prefix in _FRONTEND_TRIGGER_PREFIXES)

    # One independent active trigger: no unknown chain/timing interaction is
    # inferred. Compare modes / gating / debug action are checked against PCs.
    phases = [("equal", 0, 0, 0, 1), ("greater_equal", 2, 0, 0, 1),
              ("less", 3, 0, 0, 1), ("disabled", 2, 0, 0, 0),
              ("select_suppressed", 2, 1, 0, 1), ("debug_suppressed", 2, 0, 1, 1)]
    for name, mode, select, debug, enabled in phases:
        _set_enable_mask(env, 0)
        before = [[read(f"tdataVec_{slot}_{field}", trigger=True) for field in _FIELDS]
                  for slot in range(4)]
        threshold = _BASE + 0x100 if name in {"equal", "greater_equal", "less"} else _BASE
        config = dict(matchType=mode, select=select, action=1, chain=0, tdata2=threshold)
        _write(env, "io_csrCtrl_frontend_trigger_debugMode", debug)
        _write(env, "io_csrCtrl_frontend_trigger_triggerCanRaiseBpExp", 1)
        _write(env, "io_csrCtrl_frontend_trigger_tUpdate_bits_addr", 0)
        for field, value in config.items():
            _write(env, "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_" + field, value)
        _write(env, "io_csrCtrl_frontend_trigger_tUpdate_valid", 1)
        env.step(2)
        _write(env, "io_csrCtrl_frontend_trigger_tUpdate_valid", 0)
        env.step(2)
        after = [[read(f"tdataVec_{slot}_{field}", trigger=True) for field in _FIELDS]
                 for slot in range(4)]
        assert after[0] == [config[field] for field in _FIELDS]
        assert after[1:] == before[1:]
        _set_enable_mask(env, enabled)
        _force_redirect_to(env, _BASE)
        samples = {}
        for _ in range(1500):
            env.step(1)
            if read("s2_valid_valid") != 1 or read("s2_flush") != 0:
                continue
            if read("io_toIBuffer_valid") != 1 or read("io_toIBuffer_ready") != 1:
                continue
            mask = read("io_toIBuffer_bits_enqEnable") & read("io_toIBuffer_bits_valid")
            for lane in range(35):
                if not (mask >> lane) & 1:
                    continue
                pc = read(f"s2_alignedInstrPcVec_{lane}_addr") << 1
                raw_match = pc == threshold if mode == 0 else pc >= threshold if mode == 2 else pc < threshold
                expected_hit = int(raw_match and enabled and not select and not debug)
                hit = read(f"triggerHitVec_{lane}_0", trigger=True)
                can_name = "triggerCanFireVec_0" if lane == 0 else f"triggerCanFireVec_{lane}_0"
                fire = read(can_name, trigger=True)
                action = read(f"io_toIBuffer_bits_triggered_{lane}")
                assert hit == fire == expected_hit, (name, pc, hit, fire, expected_hit)
                assert action == (1 if expected_hit else 15), (name, pc, action)
                assert read(f"s2_alignedInstrVec_{lane}_isRvc") == 1
                assert read(f"s2_alignedInstrVec_{lane}_data") & 0xFFFF == 1
                assert read(f"io_toIBuffer_bits_instrs_{lane}") == 0x13
                samples.setdefault(str(expected_hit), dict(cycle=env.current_cycle, lane=lane,
                    pc=pc, hit=hit, can_fire=fire, action=action))
            needed = {"0", "1"} if name in {"equal", "greater_equal", "less"} else {"0"}
            if needed <= samples.keys():
                break
        assert needed <= samples.keys(), (name, samples)
        results.append(dict(phase=name, config=config, debug=debug, enabled=enabled,
                            sampled_outcomes=samples, non_target_configs_stable=True))

    gap = recorder._raw_dict()["sampler_diagnostics"]["frontend_trigger_config_gap"]
    assert gap and gap["active"] and set(gap["missing"]) == {f"tdataVec_{slot}_timing" for slot in range(4)}
    assert gap["affected_bin_ids"] == ["BIN-1000"]
    assert not any(recorder.definition_by_key[key].bin_id == "BIN-1000" for key in recorder.hits)
    recorder.risk_observations.append(dict(event="frontend_trigger_exported_abi_canary",
        phases=results, actual_paths=paths, unavailable_control="timing",
        missing_timing_value_supplied=False, owner_backannotation_eligible=False,
        coverage_promotion="none"))
    assert not env.monitor.get_errors()
