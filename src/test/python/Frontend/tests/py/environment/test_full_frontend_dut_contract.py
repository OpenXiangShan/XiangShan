from __future__ import annotations

import json
from pathlib import Path

import pytest
import yaml
from toffee.bundle import Signal, SignalList

from env.bundles import (
    CSRControlBundle,
    FrontendPerformanceBundle,
    ICacheBundle,
    ICacheControlBundle,
    UncacheBundle,
)
from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend, FakeSignal
from env.runtime.pylib import frontend_build_root_path


_INPUT_SENTINEL = 0x5A5A5A5A5A5A5A5A

_FULL_RTL_ADDED_SIGNALS = {
    "auto_inner_icache_client_out_a_bits_user_alias": ("output", 2),
    "auto_inner_icache_ctrlUnitOpt_in_a_bits_address": ("input", 30),
    "auto_inner_icache_ctrlUnitOpt_in_a_bits_data": ("input", 64),
    "auto_inner_icache_ctrlUnitOpt_in_a_bits_mask": ("input", 8),
    "auto_inner_icache_ctrlUnitOpt_in_a_bits_opcode": ("input", 4),
    "auto_inner_icache_ctrlUnitOpt_in_a_bits_size": ("input", 2),
    "auto_inner_icache_ctrlUnitOpt_in_a_bits_source": ("input", 5),
    "auto_inner_icache_ctrlUnitOpt_in_a_valid": ("input", 1),
    "auto_inner_icache_ctrlUnitOpt_in_a_ready": ("output", 1),
    "auto_inner_icache_ctrlUnitOpt_in_d_bits_data": ("output", 64),
    "auto_inner_icache_ctrlUnitOpt_in_d_bits_opcode": ("output", 4),
    "auto_inner_icache_ctrlUnitOpt_in_d_bits_size": ("output", 2),
    "auto_inner_icache_ctrlUnitOpt_in_d_bits_source": ("output", 5),
    "auto_inner_icache_ctrlUnitOpt_in_d_ready": ("input", 1),
    "auto_inner_icache_ctrlUnitOpt_in_d_valid": ("output", 1),
    "auto_inner_instrUncache_client_out_a_bits_user_memBackType_MM": ("output", 1),
    "auto_inner_instrUncache_client_out_a_bits_user_memPageType_NC": ("output", 1),
    "io_csrCtrl_cache_error_enable": ("input", 1),
    "io_csrCtrl_ldld_vio_check_enable": ("input", 1),
    "io_csrCtrl_mem_trigger_debugMode": ("input", 1),
    "io_csrCtrl_mem_trigger_tEnableVec_0": ("input", 1),
    "io_csrCtrl_mem_trigger_tEnableVec_1": ("input", 1),
    "io_csrCtrl_mem_trigger_tEnableVec_2": ("input", 1),
    "io_csrCtrl_mem_trigger_tEnableVec_3": ("input", 1),
    "io_csrCtrl_mem_trigger_tUpdate_bits_addr": ("input", 2),
    "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_action": ("input", 4),
    "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain": ("input", 1),
    "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_load": ("input", 1),
    "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType": ("input", 2),
    "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_select": ("input", 1),
    "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_store": ("input", 1),
    "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2": ("input", 64),
    "io_csrCtrl_mem_trigger_tUpdate_valid": ("input", 1),
    "io_csrCtrl_mem_trigger_triggerCanRaiseBpExp": ("input", 1),
    "io_csrCtrl_sbuffer_timeout": ("input", 22),
    "io_perf_0_value": ("output", 6),
    "io_perf_1_value": ("output", 6),
    "io_perf_2_value": ("output", 6),
    "io_perf_3_value": ("output", 6),
    "io_perf_4_value": ("output", 6),
    "io_perf_5_value": ("output", 6),
    "io_perf_6_value": ("output", 6),
    "io_perf_7_value": ("output", 6),
    "io_tlbCsr_hPBMTE": ("input", 1),
    "io_tlbCsr_hgatp_ppn": ("input", 44),
    "io_tlbCsr_mPBMTE": ("input", 1),
    "io_tlbCsr_mbmc_BCLEAR": ("input", 1),
    "io_tlbCsr_mbmc_BMA": ("input", 58),
    "io_tlbCsr_pmm_henvcfg": ("input", 2),
    "io_tlbCsr_pmm_hstatus": ("input", 2),
    "io_tlbCsr_pmm_menvcfg": ("input", 2),
    "io_tlbCsr_pmm_mseccfg": ("input", 2),
    "io_tlbCsr_pmm_senvcfg": ("input", 2),
    "io_tlbCsr_priv_dmode": ("input", 2),
    "io_tlbCsr_priv_mxr": ("input", 1),
    "io_tlbCsr_priv_spvp": ("input", 1),
    "io_tlbCsr_priv_sum": ("input", 1),
    "io_tlbCsr_priv_vmxr": ("input", 1),
    "io_tlbCsr_priv_vsum": ("input", 1),
    "io_tlbCsr_satp_ppn": ("input", 44),
    "io_tlbCsr_vsatp_ppn": ("input", 44),
}

class _FullInputProbeDut(FakeDUTFrontend):
    _is_fake_frontend_dut = False

    def __init__(self, input_names: set[str]) -> None:
        super().__init__()
        self._frontend_is_fake_dut = False
        for name in input_names:
            setattr(self, name, FakeSignal(_INPUT_SENTINEL))

    def GetInternalSignal(self, _name: str):
        return FakeSignal(0)


def _selected_full_build_root() -> Path:
    return frontend_build_root_path().resolve()


def _flatten_signals(value: object, prefix: str = "") -> dict[str, tuple[str, int]]:
    if not isinstance(value, dict):
        return {}
    if {"Pin", "High", "Low"}.issubset(value):
        high = int(value["High"])
        low = int(value["Low"])
        width = 1 if high < low else high - low + 1
        return {prefix: (str(value["Pin"]), width)}
    result: dict[str, tuple[str, int]] = {}
    for key, child in value.items():
        child_prefix = f"{prefix}_{key}" if prefix else str(key)
        result.update(_flatten_signals(child, child_prefix))
    return result


def _required_public_signals() -> dict[str, tuple[str, int]]:
    required = {
        "clock": ("input", 1),
        "reset": ("input", 1),
        "auto_inner_icache_client_out_a_ready": ("input", 1),
        "auto_inner_icache_client_out_a_valid": ("output", 1),
        "auto_inner_icache_client_out_a_bits_source": ("output", 4),
        "auto_inner_icache_client_out_a_bits_address": ("output", 48),
        "auto_inner_icache_client_out_a_bits_user_alias": ("output", 2),
        "auto_inner_icache_client_out_d_valid": ("input", 1),
        "auto_inner_icache_client_out_d_bits_opcode": ("input", 4),
        "auto_inner_icache_client_out_d_bits_source": ("input", 4),
        "auto_inner_icache_client_out_d_bits_denied": ("input", 1),
        "auto_inner_icache_client_out_d_bits_data": ("input", 256),
        "auto_inner_icache_client_out_d_bits_corrupt": ("input", 1),
        "auto_inner_icache_ctrlUnitOpt_in_a_ready": ("output", 1),
        "auto_inner_icache_ctrlUnitOpt_in_a_valid": ("input", 1),
        "auto_inner_icache_ctrlUnitOpt_in_a_bits_opcode": ("input", 4),
        "auto_inner_icache_ctrlUnitOpt_in_a_bits_size": ("input", 2),
        "auto_inner_icache_ctrlUnitOpt_in_a_bits_source": ("input", 5),
        "auto_inner_icache_ctrlUnitOpt_in_a_bits_mask": ("input", 8),
        "auto_inner_icache_ctrlUnitOpt_in_a_bits_address": ("input", 30),
        "auto_inner_icache_ctrlUnitOpt_in_a_bits_data": ("input", 64),
        "auto_inner_icache_ctrlUnitOpt_in_d_ready": ("input", 1),
        "auto_inner_icache_ctrlUnitOpt_in_d_valid": ("output", 1),
        "auto_inner_icache_ctrlUnitOpt_in_d_bits_opcode": ("output", 4),
        "auto_inner_icache_ctrlUnitOpt_in_d_bits_size": ("output", 2),
        "auto_inner_icache_ctrlUnitOpt_in_d_bits_source": ("output", 5),
        "auto_inner_icache_ctrlUnitOpt_in_d_bits_data": ("output", 64),
        "auto_inner_instrUncache_client_out_a_ready": ("input", 1),
        "auto_inner_instrUncache_client_out_a_valid": ("output", 1),
        "auto_inner_instrUncache_client_out_a_bits_address": ("output", 48),
        "auto_inner_instrUncache_client_out_a_bits_user_memBackType_MM": ("output", 1),
        "auto_inner_instrUncache_client_out_a_bits_user_memPageType_NC": ("output", 1),
        "auto_inner_instrUncache_client_out_d_valid": ("input", 1),
        "auto_inner_instrUncache_client_out_d_bits_source": ("input", 1),
        "auto_inner_instrUncache_client_out_d_bits_denied": ("input", 1),
        "auto_inner_instrUncache_client_out_d_bits_data": ("input", 64),
        "auto_inner_instrUncache_client_out_d_bits_corrupt": ("input", 1),
        "io_ptw_req_0_ready": ("input", 1),
        "io_ptw_req_0_valid": ("output", 1),
        "io_ptw_req_0_bits_vpn": ("output", 38),
        "io_ptw_req_0_bits_s2xlate": ("output", 2),
        "io_ptw_resp_ready": ("output", 1),
        "io_ptw_resp_valid": ("input", 1),
        "io_ptw_resp_bits_s1_entry_pbmt": ("input", 2),
        "io_ptw_resp_bits_s1_entry_ppn": ("input", 41),
        "io_ptw_resp_bits_s1_pf": ("input", 1),
        "io_ptw_resp_bits_s1_af": ("input", 1),
        "io_backend_toIBuf_decodeCanAccept": ("input", 1),
        "io_backend_toFtq_commit_valid": ("input", 1),
        "io_backend_toFtq_redirect_valid": ("input", 1),
        "io_backend_toFtq_redirect_bits_pc": ("input", 50),
        "io_backend_toFtq_resolve_0_valid": ("input", 1),
        "io_backend_fromFtq_wen": ("output", 1),
        "io_backend_fromFtq_ftqIdx": ("output", 6),
        "io_backend_fromFtq_startPc_addr": ("output", 50),
        "io_tlbCsr_mPBMTE": ("input", 1),
        "io_tlbCsr_hPBMTE": ("input", 1),
    }
    for slot in range(8):
        required.update(
            {
                f"io_backend_cfVec_{slot}_valid": ("output", 1),
                f"io_backend_cfVec_{slot}_bits_foldpc": ("output", 10),
                f"io_backend_cfVec_{slot}_bits_instr": ("output", 32),
                f"io_backend_cfVec_{slot}_bits_isRvc": ("output", 1),
                f"io_backend_cfVec_{slot}_bits_ftqPtr_value": ("output", 6),
                f"io_backend_cfVec_{slot}_bits_ftqOffset": ("output", 5),
            }
        )
    required.update(_FULL_RTL_ADDED_SIGNALS)
    return required


def _bundle_signal_names(bundle_types: tuple[type, ...]) -> set[str]:
    names = set()
    for bundle_type in bundle_types:
        bundle = bundle_type()
        bindings = getattr(bundle_type, "SIGNAL_BINDINGS", {})
        for attr_name in dir(bundle):
            if attr_name.startswith("_"):
                continue
            value = getattr(bundle, attr_name)
            if isinstance(value, Signal):
                names.add(str(bindings.get(attr_name, attr_name)))
            elif isinstance(value, SignalList):
                names.update(str(name) for name in value.names)
    return names


def test_full_frontend_public_signal_contract() -> None:
    build_root = _selected_full_build_root()
    pylib = build_root / "pylib-verilator" / "Frontend"
    signals_path = pylib / "signals.json"
    assert signals_path.is_file(), f"full RTL signal inventory is required: {signals_path}"

    signals = _flatten_signals(json.loads(signals_path.read_text(encoding="utf-8")))
    required = _required_public_signals()
    mismatches = {
        name: {"expected": expected, "actual": signals.get(name)}
        for name, expected in required.items()
        if signals.get(name) != expected
    }
    assert not mismatches, {"full_frontend_public_signal_mismatches": mismatches}
    historical_cfvec_pc = sorted(
        name
        for name in signals
        if name.startswith("io_backend_cfVec_") and name.endswith("_bits_pc")
    )
    assert not historical_cfvec_pc, {
        "historical_cfvec_pc_must_not_be_added_to_full_frontend": historical_cfvec_pc
    }


def test_full_rtl_added_signals_are_exposed_by_environment_bundles() -> None:
    modelled = _bundle_signal_names(
        (
            CSRControlBundle,
            FrontendPerformanceBundle,
            ICacheBundle,
            ICacheControlBundle,
            UncacheBundle,
        )
    )
    missing = sorted(set(_FULL_RTL_ADDED_SIGNALS) - modelled)
    assert not missing, {"full_rtl_added_signals_missing_from_bundles": missing}


def test_full_frontend_internal_observer_contract() -> None:
    build_root = _selected_full_build_root()
    offset_path = build_root / "pylib-verilator" / "Frontend" / "Frontend_offset.yaml"
    assert offset_path.is_file(), f"full RTL offset inventory is required: {offset_path}"

    loader = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
    inventory = yaml.load(offset_path.read_text(encoding="utf-8"), Loader=loader)
    assert isinstance(inventory, dict) and isinstance(inventory.get("variables"), list)
    registered = {
        str(item["name"]): int(item["rtl_width"])
        for item in inventory["variables"]
        if isinstance(item, dict) and "name" in item and "rtl_width" in item
    }
    required = {
        "Frontend_top.Frontend._inner_ifu_io_toIBuffer_valid": 1,
        "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_foldpc_0": 10,
        "Frontend_top.Frontend._inner_itlb_io_ptw_req_0_bits_getGpa": 1,
        "Frontend_top.Frontend.inner_ifu.uncacheUnit.uncacheState": 2,
        "Frontend_top.Frontend.inner_instrUncache.entries_0.state": 2,
        "Frontend_top.Frontend.inner_icache.mainPipe.s0_valid": 1,
        "Frontend_top.Frontend.inner_ftq.backendRedirect_valid": 1,
    }
    mismatches = {
        name: {"expected_width": width, "actual_width": registered.get(name)}
        for name, width in required.items()
        if registered.get(name) != width
    }
    assert not mismatches, {"full_frontend_internal_observer_mismatches": mismatches}


def test_full_frontend_picker_instance_hierarchy() -> None:
    build_root = _selected_full_build_root()
    top_path = build_root / "pylib-verilator" / "Frontend" / "Frontend_top.sv"
    assert top_path.is_file(), f"Picker generated top is required: {top_path}"
    text = top_path.read_text(encoding="utf-8")
    assert "Frontend Frontend(" in text
    assert "FrontendDutTop" not in text


def test_full_frontend_environment_initializes_every_input() -> None:
    build_root = _selected_full_build_root()
    signals_path = build_root / "pylib-verilator" / "Frontend" / "signals.json"
    signals = _flatten_signals(json.loads(signals_path.read_text(encoding="utf-8")))
    input_names = {name for name, (direction, _width) in signals.items() if direction == "input"}
    dut = _FullInputProbeDut(input_names)

    FrontendEnv(dut, register_callbacks=False)

    unchanged = sorted(name for name in input_names if int(getattr(dut, name).value) == _INPUT_SENTINEL)
    assert not unchanged, {"full_frontend_inputs_left_at_simulator_default": unchanged}
