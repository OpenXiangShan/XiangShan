from copy import deepcopy
from types import SimpleNamespace

import pytest

from env.support import fold_pc
from env.runtime.pylib import frontend_offset_path
from tests.py.jiabowen import test_backend_fault_handler_recovery_dut as recovery


def _fault_records():
    return {
        "redirects": [{"cycle": 10, "ftq_flag": 0, "ftq_value": 5}],
        "cfvec": [{"cycle": 14, "pc": 0x80201000, "foldpc": fold_pc(0x80201000),
                   "ftq_flag": 0, "ftq_value": 5, "ftq_offset": 1, "is_rvc": 0,
                   "exception_bits": (1,), "backend_exception": 1}],
    }


def test_handler_source_requires_a_delivered_backend_fault():
    records = _fault_records()
    assert recovery._backend_fault_source(records, fault_pc=0x80201000, fault_bit=1) == records["cfvec"][0]


@pytest.mark.parametrize("patch", [
    {"cycle": 10}, {"pc": 4, "foldpc": 1}, {"ftq_value": 6},
    {"exception_bits": ()}, {"exception_bits": (1, 2)}, {"backend_exception": 0},
    {"ftq_offset": 3},
])
def test_handler_source_rejects_wrong_epoch_identity_or_exception(patch):
    records = _fault_records()
    records["cfvec"][0].update(patch)
    with pytest.raises(AssertionError):
        recovery._backend_fault_source(records, fault_pc=0x80201000, fault_bit=1)


def _delivery():
    first = {"cycle": 22, "pc": 0x80200F00, "ftq_flag": 0, "ftq_value": 5,
             "ftq_offset": 1, "is_rvc": 0, "exception_bits": (), "backend_exception": 0}
    observations = [SimpleNamespace(cycle=22 + i, pc=first["pc"] + 4*i, instr=0x13, is_rvc=False) for i in range(16)]
    return {"cfvec": [first]}, observations


def _check(records, observations):
    recovery._assert_handler_delivery(records, observations, redirect_cycle=20,
                                      handler_pc=0x80200F00, source={"ftq_flag": 0, "ftq_value": 5})


def test_handler_checks_instruction_pc_identity_and_exception():
    _check(*_delivery())


@pytest.mark.parametrize("patch", [
    {"cycle": 20}, {"pc": 0x80201000}, {"ftq_value": 6}, {"ftq_offset": 0},
    {"exception_bits": (12,)}, {"backend_exception": 1},
])
def test_handler_rejects_stale_or_misattributed_cfvec(patch):
    records, observations = _delivery()
    records["cfvec"][0].update(patch)
    with pytest.raises(AssertionError):
        _check(records, observations)


@pytest.mark.parametrize("patch", [{"instr": 0}, {"pc": 4}, {"is_rvc": True}, {"cycle": 20}])
def test_handler_rejects_wrong_or_insufficient_checked_instructions(patch):
    records, observations = _delivery()
    observations[0] = SimpleNamespace(**{**vars(observations[0]), **patch})
    with pytest.raises(AssertionError):
        _check(records, observations)


def test_handler_rejects_late_stale_exception():
    records, observations = _delivery()
    stale = deepcopy(records["cfvec"][0])
    stale.update(cycle=24, exception_bits=(1,))
    records["cfvec"].append(stale)
    with pytest.raises(AssertionError):
        _check(records, observations)


def test_missing_probe_cannot_default_to_clean_state(monkeypatch):
    monkeypatch.setattr(recovery.probes, "_try_read_dut_signal", lambda *_: None)
    with pytest.raises(AssertionError, match="missing_internal_signals"):
        recovery._read_required_signal(object(), "way_exception", {})


def test_handler_probe_paths_exist_in_current_compiled_inventory():
    names = {
        line.removeprefix("  - name: ").strip()
        for line in frontend_offset_path().read_text().splitlines()
        if line.startswith("  - name: ")
    }
    for stem in recovery._SIGNALS.values():
        scope, leaf = stem.rsplit(".", 1)
        assert names.intersection({
            f"Frontend_top.Frontend.{scope}.{leaf}",
            f"Frontend_top.Frontend.{scope}.__Vtogcov__{leaf}",
        }), stem
