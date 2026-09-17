"""Separate handler recovery from RFR-S01's unproven natural-clear scenario.

These tests do not qualify RFR_backend_exception_three_main_fetch_cp: a clean
backend redirect clears the flag independently of the fetch-distance guard.
Keep the original RFR-S01 tests and testpoint unchanged pending design review.
"""

from __future__ import annotations

import os

import pytest

from env.core.transactions import BackendRedirectClass, RedirectTxn
from env.sequences import InjectRedirectSequence
from env.support import fold_pc
from tests.py.support import uncache_scenarios as probes
from tests.py.zhaoxinran import test_redirect_flush_recovery_scenarios as rfr
from tests.py.zhaoxinran.translation import test_instruction_fetch_permission_boundary as faults


_SIGNALS = {
    "way_exception": "inner_icache.wayLookup.exceptionEntry_valid",
    # ICache.sv connects this input and WayLookup.io_write_0_ready to one wire.
    "way_ready": "inner_icache.prefetcher.io_wayLookupWrite_0_ready",
    "way_flush": "inner_icache.io_fromFtq_redirectFlush",
    "backend_exception": "inner_ftq.backendException_value",
}


def _read_required_signal(env, name: str, selected_paths: dict[str, str]) -> int:
    if name in selected_paths:
        value = probes._try_read_dut_signal(env, selected_paths[name])
        assert value is not None, {"signal_became_unreadable": selected_paths[name]}
        return value
    scope, leaf = _SIGNALS[name].rsplit(".", 1)
    paths = (
        f"Frontend_top.Frontend.{scope}.{leaf}",
        f"Frontend_top.Frontend.{scope}.__Vtogcov__{leaf}",
    )
    for path in paths:
        value = probes._try_read_dut_signal(env, path)
        if value is not None:
            selected_paths[name] = path
            return value
    raise AssertionError({"missing_internal_signals": paths})


def _backend_fault_source(records: dict, *, fault_pc: int, fault_bit: int) -> dict:
    redirect = records["redirects"][-1]
    sources = [
        record for record in records["cfvec"]
        if record["cycle"] >= redirect["cycle"] + 2
        and (record["pc"] == fault_pc or record["foldpc"] == fold_pc(fault_pc))
        and (record["ftq_flag"], record["ftq_value"])
        == (redirect["ftq_flag"], redirect["ftq_value"])
        and record["exception_bits"] == (fault_bit,)
        and record["backend_exception"] == 1
    ]
    assert sources, {"reason": "no source-bound backend fault delivery"}
    source = sources[-1]
    assert source["ftq_offset"] == 0 or (
        source["ftq_offset"] == 1 and source["is_rvc"] == 0
    ), {"reason": "handler source must reuse the fault FTQ head", "source": source}
    return source


def _assert_handler_delivery(
    records: dict, observations: list, *, redirect_cycle: int, handler_pc: int,
    source: dict, count: int = 16,
) -> None:
    fresh = [record for record in records["cfvec"] if record["cycle"] > redirect_cycle + 1]
    assert fresh, {"reason": "no post-handler cfVec"}
    assert all(not record["exception_bits"] and record["backend_exception"] == 0 for record in fresh), {
        "reason": "stale exception after handler redirect", "cfvec": fresh,
    }
    first = fresh[0]
    assert first["pc"] == handler_pc
    assert (first["ftq_flag"], first["ftq_value"]) == (source["ftq_flag"], source["ftq_value"])
    assert first["ftq_offset"] == 1 and first["is_rvc"] == 0
    observed = [item for item in observations if item.cycle > redirect_cycle + 1]
    assert len(observed) >= count, {"reason": "insufficient checked handler instructions"}
    assert [item.pc for item in observed[:count]] == [handler_pc + 4 * i for i in range(count)]
    assert all(item.instr == 0x13 and not item.is_rvc for item in observed[:count])


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
@pytest.mark.parametrize(
    "fault_kind,fault_bit,redirect_faults",
    [("iaf", 1, {"backend_iaf": 1}), ("ipf", 12, {"backend_ipf": 1}), ("igpf", 20, {"backend_igpf": 1})],
    ids=["iaf", "ipf", "igpf"],
)
def test_backend_fault_handler_redirect_recovers_three_fetches(
    env, fault_kind: str, fault_bit: int, redirect_faults: dict[str, int],
) -> None:
    records = faults._capture_backend_fault_recovery(env)
    fetches = rfr._register_main_fetch_observer(env)
    selected_paths: dict[str, str] = {}
    states: list[dict] = []

    def capture(cycle, current_env) -> None:
        states.append({"cycle": int(cycle), **{
            name: _read_required_signal(current_env, name, selected_paths) for name in _SIGNALS
        }})

    env.register_cycle_observer(capture)
    # Preserve all original checks: front-end fault, exact fault-bearing redirect,
    # and second cfVec carrying the backend fault with its original FTQ identity.
    faults._run_backend_fault_redirect_recovery(
        env, fault_kind=fault_kind, fault_bit=fault_bit, redirect_faults=redirect_faults,
    )
    _, normal = faults._backend_fault_recovery_scenarios(fault_kind)
    # Leave a full page for handler prefetch. A page-tail handler can refetch
    # the original faulting translation; redirect alone is not an sfence.
    handler_pc = normal.va & ~0xFFF
    fault_pc = handler_pc + 0x1000
    env.load_program((0x13).to_bytes(4, "little") * 1024, normal.pa & ~0xFFF)
    source = _backend_fault_source(records, fault_pc=fault_pc, fault_bit=fault_bit)
    assert states[-1]["way_exception"] == 1 and states[-1]["way_ready"] == 0
    assert states[-1]["backend_exception"] != 0
    before_redirects = len(records["redirects"])
    InjectRedirectSequence(RedirectTxn(
        source_pc=fault_pc,
        source_ftq_flag=source["ftq_flag"], source_ftq_value=source["ftq_value"],
        source_ftq_offset=source["ftq_offset"], target_pc=handler_pc,
        reason=f"backend-{fault_kind}-clean-handler", level=1,
        redirect_class=BackendRedirectClass.TRAP_HANDLER,
    )).inject(env)
    faults._wait_until(env, lambda: len(records["redirects"]) > before_redirects, description="clean handler redirect")
    redirect = records["redirects"][-1]
    assert redirect == {
        "cycle": redirect["cycle"], "pc": fault_pc, "target_pc": handler_pc,
        "ftq_flag": source["ftq_flag"], "ftq_value": source["ftq_value"],
        "ftq_offset": source["ftq_offset"], "level": 1,
        "backend_iaf": 0, "backend_ipf": 0, "backend_igpf": 0,
        "debug_is_ctrl": 0, "debug_is_mem_vio": 0,
    }
    rfr._wait_for_three_main_fetches_after_redirect(env, fetches)
    faults._wait_until(
        env,
        lambda: len([item for item in env.monitor.observations if item.cycle > redirect["cycle"] + 1]) >= 16,
        description="sixteen checked handler instructions",
    )
    _assert_handler_delivery(
        records, env.monitor.observations, redirect_cycle=redirect["cycle"],
        handler_pc=handler_pc, source=source,
    )
    assert any(state["way_flush"] for state in states if state["cycle"] >= redirect["cycle"])
    assert states[-1]["backend_exception"] == 0 and states[-1]["way_exception"] == 0
    assert states[-1]["way_ready"] == 1
    assert not env.monitor.get_errors()
    assert not env.get_errors()
    env.logger.info(
        "handler recovery only (not natural-clear HIT): source=%s redirect=%s paths=%s state_tail=%s",
        source, redirect, selected_paths, states[-12:],
    )
