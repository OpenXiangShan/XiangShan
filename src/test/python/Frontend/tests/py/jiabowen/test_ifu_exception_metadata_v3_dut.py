"""BIN-954 component diagnostics and one-run metadata regression."""

from __future__ import annotations

import os

import pytest

from env.funcov.py.ifu.compact_funcov import _read_ifu_internal_with_path
from tests.py.zhaoxinran.translation import test_address_translation_context_switch as context
from tests.py.zhaoxinran.translation import test_address_translation_gstage_provenance as gstage
from tests.py.zhaoxinran.translation import test_address_translation_fault as translation_faults
from tests.py.zhaoxinran.translation import test_instruction_fetch_permission_boundary as faults


pytestmark = pytest.mark.skipif(
    os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT"
)


def _capture_metadata(env):
    records, paths = [], {}
    stems = (
        "io_toIBuffer_valid", "io_toIBuffer_ready",
        "io_toIBuffer_bits_exceptionType_value", "io_toIBuffer_bits_isBackendException",
        "io_toIBuffer_bits_hasSatpFlush", "io_toIBuffer_bits_exceptionCrossPage",
        "s2_icacheMeta_0_exception_value", "s2_icacheMeta_0_isBackendException",
        "s2_icacheMeta_0_hasSatpFlush", "s2_icacheMeta_0_gpAddr_addr",
        "s2_icacheMeta_0_isForVSnonLeafPTE", "s2_prevEndIsHalfRviInfo_valid",
        "s2_prevEndIsHalfRviInfo_bits_pc_addr", "s2_prevEndIsHalfRviInfo_bits_data",
        "s2_fetchBlock_0_ftqIdx_flag", "s2_fetchBlock_0_ftqIdx_value",
        "s2_fetchBlock_0_startVAddr_addr", "io_toIBuffer_bits_enqEnable",
    )

    def capture(cycle, active_env):
        if int(active_env.dut.reset.value):
            return
        state = {"cycle": int(cycle)}
        for stem in stems:
            value, path = _read_ifu_internal_with_path(
                active_env.functional_coverage, active_env.dut, stem
            )
            assert value is not None, {"missing_metadata_probe": stem}
            state[stem] = int(value)
            paths[stem] = path
        if not state["io_toIBuffer_valid"]:
            return
        # Compare aliases in this observer phase, not values captured a cycle later.
        canaries = {}
        for stem in stems:
            readable = {}
            for prefix in (
                "Frontend_top.Frontend.inner_ifu.",
                "Frontend_top.Frontend._inner_ifu_",
                "Frontend_top.Frontend.inner_ifu.__Vtogcov__",
            ):
                path = prefix + stem
                value = active_env.functional_coverage._try_read_dut_signal(active_env.dut, path)
                if value is not None:
                    readable[path] = int(value)
                    assert int(value) == state[stem], {"alias_disagreement": readable, "stem": stem}
            canaries[stem] = readable
        gp = active_env.backend_observe_if
        state["gpaddr"] = {
            "wen": int(gp.gpaddr_mem_wen.value), "waddr": int(gp.gpaddr_mem_waddr.value),
            "addr": int(gp.gpaddr_mem_gpaddr.value),
            "nonleaf": int(gp.gpaddr_mem_is_for_vs_nonleaf_pte.value),
        }
        state["signal_paths"] = dict(paths)
        state["canaries"] = canaries
        records.append(state)

    env.register_cycle_observer(capture)
    return records


def _save_review(env, records, component):
    interesting = [r for r in records if (
        r["io_toIBuffer_bits_exceptionType_value"]
        or r["io_toIBuffer_bits_hasSatpFlush"]
    )]
    expected = {
        "satp_without_fault": {"satp_flush"}, "vs_nonleaf_gpf": {"gpaddr", "vs_nonleaf_pte"},
        "backend_fault": {"backend_exception"}, "cross_page": {"cross_page"},
    }[component]
    assert expected <= set(getattr(env.functional_coverage, "_ifu_exception_metadata_checks", ()))
    env._emit_event("ifu.exception_metadata_component_review", {
        "component": component, "transactions": interesting,
        "sampler_checks": sorted(getattr(env.functional_coverage, "_ifu_exception_metadata_checks", ())),
        "coverage_promotion": "none: partial BIN-954 diagnostic",
    })
    assert not env.get_errors()


@pytest.mark.funcov_bins("BIN-954")
def test_ifu_exception_metadata_satp_without_fault_diagnostic(env):
    records = _capture_metadata(env)
    # Retain the existing independent translation/PC/context-switch assertions.
    context.test_satp_asid_switch_restarts_translation_from_live_cfvec(env)
    delivered = [r for r in records if r["io_toIBuffer_ready"] and r["io_toIBuffer_bits_hasSatpFlush"]]
    assert delivered, {"reason": "no accepted satp-flush metadata"}
    assert all(r["s2_icacheMeta_0_hasSatpFlush"] == 1 for r in delivered)
    assert all(r["io_toIBuffer_bits_exceptionType_value"] == 0 for r in delivered)
    assert all(r["io_toIBuffer_bits_isBackendException"] == 0 for r in delivered)
    assert delivered[0]["s2_fetchBlock_0_startVAddr_addr"] << 1 == context._NEW_VA
    _save_review(env, records, "satp_without_fault")


@pytest.mark.funcov_bins("BIN-954")
def test_ifu_exception_metadata_vs_nonleaf_gpf_diagnostic(env):
    records = _capture_metadata(env)
    scenario = gstage._scenario(
        "ifu-bin954-vs-nonleaf-gpf",
        s1_patch=(("s1_entry_v", 1), ("s1_entry_perm_r", 0),
                  ("s1_entry_perm_w", 0), ("s1_entry_perm_x", 0),
                  ("s1_pf", 0), ("s1_af", 0)),
        gpf=1, gaf=0,
    )
    gstage.test_address_translation_gstage_provenance(
        env, scenario, "instruction_guest_page_fault", True
    )
    delivered = [r for r in records if r["io_toIBuffer_ready"] and r["gpaddr"]["wen"]]
    assert delivered, {"reason": "no accepted GPF metadata"}
    for r in delivered:
        assert r["io_toIBuffer_bits_exceptionType_value"] == r["s2_icacheMeta_0_exception_value"] == 2
        assert r["gpaddr"]["addr"] == gstage._GPA
        assert r["s2_icacheMeta_0_gpAddr_addr"] << 1 == gstage._GPA
        assert r["gpaddr"]["nonleaf"] == r["s2_icacheMeta_0_isForVSnonLeafPTE"] == 1
        assert r["gpaddr"]["waddr"] == r["s2_fetchBlock_0_ftqIdx_value"]
        assert r["s2_fetchBlock_0_startVAddr_addr"] << 1 == gstage._VA
    _save_review(env, records, "vs_nonleaf_gpf")


@pytest.mark.funcov_bins("BIN-954")
def test_ifu_exception_metadata_backend_fault_diagnostic(env):
    records = _capture_metadata(env)
    faults._run_backend_fault_redirect_recovery(
        env, fault_kind="ipf", fault_bit=12, redirect_faults={"backend_ipf": 1}
    )
    delivered = [r for r in records if r["io_toIBuffer_ready"] and r["io_toIBuffer_bits_isBackendException"]]
    assert delivered, {"reason": "no accepted backend exception metadata"}
    assert all(r["s2_icacheMeta_0_isBackendException"] == 1 for r in delivered)
    assert all(r["io_toIBuffer_bits_exceptionType_value"] == r["s2_icacheMeta_0_exception_value"] == 1 for r in delivered)
    _save_review(env, records, "backend_fault")


@pytest.mark.funcov_bins("BIN-954")
def test_ifu_exception_metadata_cross_page_fault_diagnostic(env):
    records = _capture_metadata(env)
    translation_faults.test_cacheable_cross_page_second_page_translation_fault(
        env, 0, "s1_pf", "page_fault", "instruction_page_fault"
    )
    delivered = [r for r in records if r["io_toIBuffer_ready"] and r["io_toIBuffer_bits_exceptionCrossPage"]]
    assert delivered, {"reason": "no accepted cross-page exception metadata"}
    for r in delivered:
        assert r["s2_prevEndIsHalfRviInfo_valid"] == 1
        assert r["s2_prevEndIsHalfRviInfo_bits_pc_addr"] << 1 == translation_faults._CACHEABLE_CROSS_PAGE_RVI_PC
        assert r["s2_prevEndIsHalfRviInfo_bits_data"] == 0x13
        assert r["io_toIBuffer_bits_exceptionType_value"] == r["s2_icacheMeta_0_exception_value"] == 1
        assert r["s2_fetchBlock_0_startVAddr_addr"] << 1 == translation_faults._CACHEABLE_CROSS_PAGE_RVI_PC + 2
    _save_review(env, records, "cross_page")


@pytest.mark.funcov_bins("BIN-954")
def test_ifu_exception_metadata_all_components_one_run(env, monkeypatch):
    # The testpoint explicitly says to construct these components separately.
    # Use real reset/re-entry between them, retaining the same DUT, recorder,
    # waveform and artifact. Never combine independent pytest artifacts.
    original_clear = env.monitor.clear

    def checked_clear():
        assert not env.monitor.get_errors(), {"reason": "phase setup must not erase monitor errors"}
        original_clear()

    monkeypatch.setattr(env.monitor, "clear", checked_clear)
    phase_reports = []
    for phase in (
        test_ifu_exception_metadata_satp_without_fault_diagnostic,
        test_ifu_exception_metadata_vs_nonleaf_gpf_diagnostic,
        test_ifu_exception_metadata_cross_page_fault_diagnostic,
        test_ifu_exception_metadata_backend_fault_diagnostic,
    ):
        assert not env.get_errors()
        env._emit_event("ifu.exception_metadata_phase_start", {
            "phase": phase.__name__, "cycle": int(env.current_cycle),
            "backend_commit_ptr": [env.backend_model.commit_ptr_flag, env.backend_model.commit_ptr_value],
        })
        phase(env)
        assert not env.get_errors()
        phase_reports.append({
            "phase": phase.__name__, "end_cycle": int(env.current_cycle),
            "monitor_errors": len(env.monitor.get_errors()),
            "translation_error_count": env.translation_oracle.get_stats()["error_count"],
            "sampler_checks": sorted(getattr(env.functional_coverage, "_ifu_exception_metadata_checks", ())),
        })
        # A fully checked phase has ended; do not interpret reset cycles under
        # the prior phase's translation oracle. No errors/counters are erased.
        env.translation_oracle.disarm()
    expected = {"backend_exception", "satp_flush", "cross_page", "gpaddr", "vs_nonleaf_pte"}
    assert env.functional_coverage._ifu_exception_metadata_checks == expected
    assert env.functional_coverage.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_056")
    env._emit_event("ifu.exception_metadata_one_run_checkpoint", {
        "phases": phase_reports,
        "component_witnesses": env.functional_coverage._ifu_exception_metadata_witnesses,
        "same_recorder_and_dut": True,
    })
