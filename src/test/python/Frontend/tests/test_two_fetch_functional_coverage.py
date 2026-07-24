import csv
import hashlib
import json
import os
import re
import subprocess
import sys
from pathlib import Path
from types import SimpleNamespace

import pytest

import env.fixtures as fixtures_module
import env.functional_coverage as functional_coverage_module
import tools.backannotate_funcov as backannotate_module
from env.funcov import (
    CFVEC_SAMPLER_BIN_KEYS,
    _RAW_INSTR_FIELDS,
    TWO_FETCH_COVERPOINTS,
    TWO_FETCH_SAMPLER_BIN_KEYS,
    _TWO_FETCH_SIGNALS,
    _tf_raw_instr_field_candidates,
    sample_cfvec_coverage,
    sample_two_fetch_coverage,
)
from env.artifact_provenance import load_frontend_build_manifest, write_frontend_build_manifest
from env.functional_coverage import (
    FUNCTIONAL_COVERAGE_SAMPLER_BIN_KEYS,
    FunctionalCoverageRecorder,
    default_pilot_csv_path,
)
from tools.backannotate_funcov import (
    PilotBin,
    _target_matches,
    backannotate,
    load_artifacts,
    load_pilot,
    validate_mapping,
)
from tools.backannotate_funcov import build_artifact_audit


class _Signal:
    def __init__(self, value=0):
        self.value = int(value)


class _FakeDut:
    def set(self, name, value):
        signal = getattr(self, str(name), None)
        if signal is None:
            signal = _Signal()
            setattr(self, str(name), signal)
        signal.value = int(value)

    def set_key(self, key, value):
        self.set(_TWO_FETCH_SIGNALS[str(key)][0], value)


class _Memory:
    @staticmethod
    def is_mmio(_addr):
        return False

    @staticmethod
    def read_u8(_addr):
        return 0


def _set_raw_instr_vector(
    dut,
    *,
    first_size,
    total_size=32,
    cross_index=None,
    cross_data=0,
):
    base = "Frontend_top.Frontend.inner_ifu.instrBoundary."
    for index in range(32):
        is_cross = int(cross_index is not None and index == int(cross_index))
        values = {
            "valid": int(index < int(total_size) and index != (int(cross_index) + 1 if cross_index is not None else -1)),
            "data": int(cross_data) if is_cross else 0,
            "isRvc": 0 if is_cross else 1,
            "blockSel": int(index >= int(first_size) or (is_cross and index == int(first_size) - 1)),
            "isCrossBlockInstr": is_cross,
            "startOffset": 31 if is_cross else (index - int(first_size) if index >= int(first_size) else index),
        }
        for field, value in values.items():
            dut.set(f"{base}io_resp_rawInstrVec_{index}_{field}", value)
            dut.set(f"{base}__Vtogcov__io_resp_rawInstrVec_{index}_{field}", value)


def _set_cfvec_entries(dut, entries):
    for slot in range(8):
        base = f"io_backend_cfVec_{slot}_"
        dut.set(base + "valid", 0)
        for cause in (1, 2, 12, 19, 20):
            dut.set(base + f"bits_exceptionVec_{cause}", 0)
    for slot, pc, is_rvc, instr in entries:
        base = f"io_backend_cfVec_{int(slot)}_"
        dut.set(base + "valid", 1)
        dut.set(base + "bits_pc", pc)
        dut.set(base + "bits_isRvc", is_rvc)
        dut.set(base + "bits_instr", instr)
        dut.set(base + "bits_ftqPtr_flag", 0)
        dut.set(base + "bits_ftqPtr_value", int(slot))


def _set_ibuffer_entries(dut, entries):
    base = "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_"
    enable = 0
    for index in range(36):
        dut.set(f"{base}pc_{index}_addr", 0)
        dut.set(f"{base}isRvc_{index}", 0)
        dut.set(f"{base}ftqPtr_{index}_flag", 0)
        dut.set(f"{base}ftqPtr_{index}_value", 0)
    for slot, pc, is_rvc, flag, value in entries:
        slot = int(slot)
        enable |= 1 << slot
        dut.set(f"{base}pc_{slot}_addr", pc)
        dut.set(f"{base}isRvc_{slot}", is_rvc)
        dut.set(f"{base}ftqPtr_{slot}_flag", flag)
        dut.set(f"{base}ftqPtr_{slot}_value", value)
    dut.set(base + "enqEnable", enable)


def _set_mainpipe_waylookup_inputs(
    dut,
    *,
    second_meta_valid=1,
    mmio0=0,
    mmio1=0,
    itlb0=0,
    itlb1=0,
    vset=0,
):
    dut.set_key("main_wli1_valid", second_meta_valid)
    dut.set_key("main_wli0_is_mmio", mmio0)
    dut.set_key("main_wli1_is_mmio", mmio1)
    dut.set_key("main_wli0_itlb_exception", itlb0)
    dut.set_key("main_wli1_itlb_exception", itlb1)
    dut.set_key("main_wli0_vset0", vset)
    dut.set_key("main_wli0_vset1", vset)
    dut.set_key("main_wli1_vset0", vset)
    dut.set_key("main_wli1_vset1", vset)


def _eligible_provenance():
    frontend_root = Path(__file__).resolve().parents[1]

    def file_sha256(path):
        return hashlib.sha256(Path(path).read_bytes()).hexdigest()

    definitions_sha256 = hashlib.sha256(b"[]").hexdigest()
    sampler_sha256 = hashlib.sha256(
        json.dumps(
            {
                "functional_coverage.py": file_sha256(frontend_root / "env/functional_coverage.py"),
                "funcov.py": file_sha256(frontend_root / "env/funcov.py"),
            },
            ensure_ascii=False,
            sort_keys=True,
            separators=(",", ":"),
        ).encode("utf-8")
    ).hexdigest()
    build_root = Path("/tmp/frontend-funcov-unit") / f"manifest-{os.getpid()}" / "build-frontend"
    manifest_path = build_root / "frontend_build_manifest.json"
    if not manifest_path.is_file():
        pylib = build_root / "pylib" / "Frontend"
        rtl = build_root / "rtl"
        pylib.mkdir(parents=True, exist_ok=True)
        rtl.mkdir(parents=True, exist_ok=True)
        (pylib / "libUTFrontend.so").write_bytes(b"unit-dut-model")
        (pylib / "_UT_Frontend.so").write_bytes(b"unit-python-extension")
        (pylib / "Frontend_offset.yaml").write_text("signals: []\n", encoding="utf-8")
        (rtl / "Frontend.sv").write_text("module Frontend; endmodule\n", encoding="utf-8")
        write_frontend_build_manifest(
            manifest_path,
            build_root=build_root,
            dut_source_sha="a" * 40,
            source_tree_dirty=False,
            build_config="frontend-test",
            build_command="make frontend",
        )
    manifest = load_frontend_build_manifest(build_root, manifest_path)
    assert manifest["build_manifest_status"] == "valid", manifest["build_manifest_reasons"]
    values = {
        **{
            key: manifest[key]
            for key in (
                "dut_build_sha256",
                "dut_python_extension_sha256",
                "generated_rtl_sha256",
                "signal_contract_sha256",
            )
        },
        "build_manifest_path": str(manifest_path.resolve()),
        "build_manifest_sha256": manifest["build_manifest_sha256"],
    }
    values["registry_sha256"] = file_sha256(default_pilot_csv_path())
    values["definitions_sha256"] = definitions_sha256
    values["sampler_sha256"] = sampler_sha256
    values["dut_source_sha"] = manifest["dut_source_sha"]
    values["implementation_sha"] = manifest["implementation_sha"]
    values["design_baseline_sha"] = manifest["design_baseline_sha"]
    values["source_sha_override"] = manifest["source_sha_override"]
    values["source_delta_sha256"] = manifest["source_delta_sha256"]
    values["source_delta_files"] = manifest["source_delta_files"]
    values["source_delta_policy"] = manifest["source_delta_policy"]
    values["build_config"] = manifest["build_config"]
    values["toolchain"] = "python-test"
    values["build_manifest_status"] = manifest["build_manifest_status"]
    values["build_manifest_reasons"] = manifest["build_manifest_reasons"]
    return _resign_provenance(values)


def _resign_provenance(values):
    compatibility_fields = (
        "dut_source_sha",
        "implementation_sha",
        "design_baseline_sha",
        "source_sha_override",
        "source_delta_sha256",
        "source_delta_files",
        "source_delta_policy",
        "dut_build_sha256",
        "dut_python_extension_sha256",
        "generated_rtl_sha256",
        "registry_sha256",
        "sampler_sha256",
        "signal_contract_sha256",
        "build_config",
        "toolchain",
    )
    values["compatibility_signature"] = hashlib.sha256(
        json.dumps(
            {field: values[field] for field in compatibility_fields},
            sort_keys=True,
            separators=(",", ":"),
        ).encode("utf-8")
    ).hexdigest()
    return values


def _eligible_artifact_paths(run_id, stem="case"):
    artifact_root = Path("/tmp/frontend-funcov-unit") / str(run_id)
    waveform_path = artifact_root / "waveforms" / f"{stem}.fst"
    line_coverage_path = artifact_root / "coverage" / f"{stem}.dat"
    waveform_path.parent.mkdir(parents=True, exist_ok=True)
    line_coverage_path.parent.mkdir(parents=True, exist_ok=True)
    waveform_path.write_bytes(b"fst")
    line_coverage_path.write_bytes(b"coverage")
    return {
        "source_csv": str(default_pilot_csv_path()),
        "definitions": [],
        "waveform_path": str(waveform_path),
        "line_coverage_path": str(line_coverage_path),
    }


def _eligible_run(
    run_id,
    *,
    outcome="passed",
    exit_code=0,
    checker=None,
):
    artifact_root = Path("/tmp/frontend-funcov-unit") / str(run_id)
    testcase_path = artifact_root / "inputs" / "test_case.py"
    case_log_path = artifact_root / "logs" / "case.log"
    funcov_path = artifact_root / "funcov" / "case.funcov.json"
    testcase_path.parent.mkdir(parents=True, exist_ok=True)
    case_log_path.parent.mkdir(parents=True, exist_ok=True)
    funcov_path.parent.mkdir(parents=True, exist_ok=True)
    testcase_path.write_text("def test_case():\n    pass\n", encoding="utf-8")
    case_log_path.write_text("", encoding="utf-8")
    funcov_path.write_text("{}\n", encoding="utf-8")
    return {
        "run_id": str(run_id),
        "pytest_outcome": str(outcome),
        "exit_code": int(exit_code),
        "checker": checker or {"status": "pass", "error_count": 0, "errors": []},
        "testcase_nodeid": "tests/test_case.py::test_case",
        "testcase_path": str(testcase_path),
        "testcase_sha256": hashlib.sha256(testcase_path.read_bytes()).hexdigest(),
        "run_command": "pytest tests/test_case.py::test_case",
        "artifact_root": str(artifact_root),
        "case_log_path": str(case_log_path),
        "funcov_path": str(funcov_path),
        "seed": 1,
        "seeds": {"test": 1, "backend": 1, "icache": 1, "ptw": 1},
    }


def _make_recorder(tmp_path, *, target_bin_ids=None, target_tp_ids=None, target_testcases=None):
    dut = _FakeDut()
    for key in (
        "bpu_s3_flush",
        "main_s1_flush",
        "main_s1_exception",
        "main_s1_mmio",
        "backend_redirect",
    ):
        dut.set_key(key, 0)
    env = SimpleNamespace(
        dut=dut,
        config=SimpleNamespace(backend=SimpleNamespace(ftq_size=64)),
        memory=_Memory(),
    )
    recorder = FunctionalCoverageRecorder.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="two_fetch_unit",
        artifact_tag="two_fetch_unit",
        output_dir=tmp_path,
        target_bin_ids=target_bin_ids,
        target_tp_ids=target_tp_ids,
        target_testcases=target_testcases,
    )
    recorder.attach(env)
    return recorder, env, dut


def _install_eligible_provenance(recorder):
    provenance = _eligible_provenance()
    provenance["definitions_sha256"] = recorder.provenance["definitions_sha256"]
    recorder.provenance = provenance


def test_two_fetch_ftq_eligibility_and_pointer_bins(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("ftq_valid", 1)
    dut.set_key("ftq_ready", 1)
    dut.set_key("ftq_req1_valid", 1)
    dut.set_key("ftq_req0_start", 0x80000000)
    dut.set_key("ftq_req1_start", 0x80000020)
    dut.set_key("ftq_req0_end", 7)
    dut.set_key("ftq_req1_end", 7)
    dut.set_key("ftq_req0_exception", 0)
    dut.set_key("bpu_ptr_flag", 1)
    dut.set_key("bpu_ptr_value", 2)
    dut.set_key("fetch_ptr_flag", 0)
    dut.set_key("fetch_ptr_value", 62)
    dut.set_key("way_out_valid", 1)
    dut.set_key("way_out_ready", 1)
    dut.set_key("main_s0_fire", 1)
    dut.set_key("way_real_two", 1)

    sample_two_fetch_coverage(recorder, env, 1)
    assert recorder.key_hit("two_fetch_ftq_eligibility", "eligible_dual")

    dut.set_key("ftq_req1_valid", 0)
    dut.set_key("ftq_req1_start", 0x80001000)
    dut.set_key("bpu_ptr_value", 4)
    dut.set_key("fetch_ptr_flag", 1)
    dut.set_key("fetch_ptr_value", 0)
    sample_two_fetch_coverage(recorder, env, 2)

    assert recorder.key_hit("two_fetch_ftq_eligibility", "blocked_cross_page")
    assert recorder.key_hit("two_fetch_pointer_advance", "step_two")
    assert recorder.key_hit("two_fetch_pointer_advance", "wrap_step_two")


def test_two_fetch_backend_exception_requires_observed_exception_signal(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("ftq_valid", 1)
    dut.set_key("ftq_ready", 1)
    dut.set_key("ftq_req1_valid", 0)
    dut.set_key("ftq_req0_start", 0x40000000)
    dut.set_key("ftq_req1_start", 0x40000010)
    dut.set_key("ftq_req0_end", 7)
    dut.set_key("ftq_req1_end", 7)
    dut.set_key("ftq_req0_exception", 0)
    dut.set_key("bpu_ptr_flag", 0)
    dut.set_key("bpu_ptr_value", 8)
    dut.set_key("fetch_ptr_flag", 0)
    dut.set_key("fetch_ptr_value", 0)

    sample_two_fetch_coverage(recorder, env, 1)
    assert not recorder.key_hit("two_fetch_ftq_eligibility", "blocked_backend_exception")

    dut.set_key("ftq_req0_exception", 1)
    sample_two_fetch_coverage(recorder, env, 2)
    assert recorder.key_hit("two_fetch_ftq_eligibility", "blocked_backend_exception")


def test_two_fetch_cross_page_uses_active_ftq_entries_when_request_probe_is_absent(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("ftq_valid", 1)
    dut.set_key("ftq_ready", 1)
    dut.set_key("ftq_req1_valid", 0)
    dut.set_key("ftq_req0_end", 7)
    dut.set_key("ftq_req1_end", 7)
    dut.set_key("ftq_req0_exception", 0)
    dut.set_key("bpu_ptr_flag", 0)
    dut.set_key("bpu_ptr_value", 8)
    dut.set_key("fetch_ptr_flag", 0)
    dut.set_key("fetch_ptr_value", 0)
    dut.set("Frontend_top.Frontend.inner_ftq.entryQueue_0_startPc_addr", 0x80000FFE)
    dut.set("Frontend_top.Frontend.inner_ftq.entryQueue_1_startPc_addr", 0x80001000)

    sample_two_fetch_coverage(recorder, env, 1)

    assert recorder.key_hit("two_fetch_ftq_eligibility", "blocked_cross_page")


def test_two_fetch_waylookup_ifu_and_delivery_bins(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("way_req1_valid", 1)
    dut.set_key("way_out_valid", 1)
    dut.set_key("way_out_ready", 1)
    dut.set_key("main_s0_fire", 1)
    dut.set_key("way_real_two", 1)
    dut.set_key("ifu_valid", 1)
    dut.set_key("ifu_ready", 1)
    dut.set_key("ifu_req1_valid", 1)
    dut.set_key("ifu_req0_size", 8)
    dut.set_key("ifu_req0_start", 0x40000000)
    dut.set_key("ifu_req1_start", 0x40000008)
    _set_raw_instr_vector(dut, first_size=8)
    dut.set_key("ifu_second_valid", 1)
    dut.set_key("ifu_s2_valid", 1)
    dut.set_key("ifu_s2_ftq0_flag", 0)
    dut.set_key("ifu_s2_ftq0_value", 5)
    dut.set_key("ifu_s2_ftq1_flag", 0)
    dut.set_key("ifu_s2_ftq1_value", 6)
    dut.set_key("to_ibuffer_valid", 1)
    dut.set_key("to_ibuffer_ready", 1)
    _set_ibuffer_entries(
        dut,
        [
            (0, 0x40000000, 1, 0, 5),
            (1, 0x40000008, 1, 0, 6),
        ],
    )

    sample_two_fetch_coverage(recorder, env, 3)

    assert recorder.key_hit("two_fetch_waylookup_result", "dual_served")
    assert recorder.key_hit("two_fetch_ifu_window", "dual_window")
    assert recorder.key_hit("two_fetch_ifu_source", "blocksel_switch")
    assert recorder.key_hit("two_fetch_delivery", "dual_fire")

    dut.set_key("way_real_two", 0)
    _set_mainpipe_waylookup_inputs(dut, second_meta_valid=0)

    sample_two_fetch_coverage(recorder, env, 4)
    assert recorder.key_hit("two_fetch_waylookup_result", "single_fallback")
    assert recorder.key_hit("two_fetch_waylookup_block_reason", "insufficient_meta")


@pytest.mark.parametrize(
    ("inputs", "expected_bin"),
    [
        ({"second_meta_valid": 0}, "insufficient_meta"),
        ({"mmio1": 1}, "mmio"),
        ({"itlb1": 1}, "itlb_exception"),
    ],
)
def test_two_fetch_mainpipe_reason_is_reconstructed_from_waylookup_inputs(tmp_path, inputs, expected_bin):
    recorder, _env, dut = _make_recorder(tmp_path / f"reason-{expected_bin}")
    dut.set_key("way_req1_valid", 1)
    dut.set_key("way_out_valid", 1)
    dut.set_key("way_out_ready", 1)
    dut.set_key("main_s0_fire", 1)
    dut.set_key("way_real_two", 0)
    _set_mainpipe_waylookup_inputs(dut, **inputs)

    sample_two_fetch_coverage(recorder, _env, 1)

    assert recorder.key_hit("two_fetch_waylookup_result", "single_fallback")
    assert recorder.key_hit("two_fetch_waylookup_block_reason", expected_bin)
    assert sum(
        recorder.key_hit("two_fetch_waylookup_block_reason", candidate)
        for candidate in ("insufficient_meta", "data_bank_conflict", "mmio", "itlb_exception")
    ) == 1


@pytest.mark.parametrize(
    ("real_two", "step"),
    [(0, 1), (1, 2)],
)
def test_two_fetch_pointer_advance_binds_mainpipe_result(tmp_path, real_two, step):
    recorder, _env, dut = _make_recorder(tmp_path / f"pointer-{real_two}")
    dut.set_key("fetch_ptr_flag", 0)
    dut.set_key("fetch_ptr_value", 0)
    dut.set_key("way_req1_valid", 1)
    dut.set_key("way_out_valid", 1)
    dut.set_key("way_out_ready", 1)
    dut.set_key("main_s0_fire", 1)
    dut.set_key("way_real_two", real_two)
    sample_two_fetch_coverage(recorder, _env, 1)

    dut.set_key("fetch_ptr_value", step)
    sample_two_fetch_coverage(recorder, _env, 2)

    expected = "step_two" if step == 2 else "step_one"
    assert recorder.key_hit("two_fetch_pointer_advance", expected)


def test_two_fetch_pointer_advance_ignores_single_candidate_without_req1(tmp_path):
    recorder, _env, dut = _make_recorder(tmp_path)
    dut.set_key("fetch_ptr_flag", 0)
    dut.set_key("fetch_ptr_value", 0)
    dut.set_key("way_req1_valid", 0)
    dut.set_key("way_out_valid", 1)
    dut.set_key("way_out_ready", 1)
    dut.set_key("main_s0_fire", 1)
    dut.set_key("way_real_two", 0)
    sample_two_fetch_coverage(recorder, _env, 1)

    dut.set_key("fetch_ptr_value", 1)
    sample_two_fetch_coverage(recorder, _env, 2)

    assert not recorder.key_hit("two_fetch_pointer_advance", "step_one")


def test_waylookup_data_conflict_does_not_hit_without_observable_bank_inputs(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("way_req1_valid", 1)
    dut.set_key("way_out_valid", 1)
    dut.set_key("way_out_ready", 1)
    dut.set_key("main_s0_fire", 1)
    dut.set_key("way_real_two", 0)
    _set_mainpipe_waylookup_inputs(dut)
    dut.set_key("main_wli1_vset1", 3)

    sample_two_fetch_coverage(recorder, env, 4)

    assert recorder.key_hit("two_fetch_waylookup_result", "single_fallback")
    assert not recorder.key_hit("two_fetch_waylookup_block_reason", "data_bank_conflict")
    assert any(
        item.get("event") == "mainpipe_fallback_reason_unobservable"
        for item in recorder.risk_observations
    )


def test_waylookup_empty_write_observation_is_not_gated_by_two_fetch(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("way_empty", 1)
    dut.set_key("way_write_ptr_flag", 0)
    dut.set_key("way_write_ptr_value", 0)

    sample_two_fetch_coverage(recorder, env, 1)

    dut.set_key("way_empty", 0)
    dut.set_key("way_write_ptr_value", 1)
    sample_two_fetch_coverage(recorder, env, 2)

    observations = [
        item
        for item in recorder.risk_observations
        if item.get("event") == "waylookup_empty_write_timing"
    ]
    assert any(item.get("empty") == 1 for item in observations)
    assert any(
        item.get("write_ptr_changed") == 1 and item.get("previous_empty") == 1
        for item in observations
    )
    assert not recorder.key_hit("two_fetch_waylookup_result", "single_fallback")


def test_two_fetch_mainpipe_refill_completion_bin(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("main_s1_valid", 1)
    dut.set_key("main_req1_valid", 1)
    dut.set_key("main_s1_ftq0_flag", 0)
    dut.set_key("main_s1_ftq0_value", 4)
    dut.set_key("main_s1_ftq1_flag", 0)
    dut.set_key("main_s1_ftq1_value", 5)
    dut.set_key("ifu_valid", 0)
    dut.set_key("ifu_ready", 0)
    dut.set_key("ifu_req1_valid", 0)
    for index, value in enumerate((0, 0, 1, 0)):
        dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_shouldFetch_{index}", value)
    for req in range(2):
        for line in range(2):
            dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValid_{req}_{line}", 0)
            dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValidReg_{req}_{line}", 0)

    sample_two_fetch_coverage(recorder, env, 4)
    assert not recorder.key_hit("two_fetch_mainpipe_hit_pattern", "hit_miss")

    dut.set("Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValid_1_0", 1)
    sample_two_fetch_coverage(recorder, env, 5)
    dut.set("Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValid_1_0", 0)
    dut.set("Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValidReg_1_0", 1)
    dut.set_key("main_s1_valid", 0)
    dut.set_key("ifu_valid", 1)
    dut.set_key("ifu_ready", 1)
    dut.set_key("ifu_req1_valid", 1)
    dut.set_key("ifu_req0_ftq_flag", 0)
    dut.set_key("ifu_req0_ftq_value", 4)
    dut.set_key("ifu_req1_ftq_flag", 0)
    dut.set_key("ifu_req1_ftq_value", 5)
    sample_two_fetch_coverage(recorder, env, 6)

    assert recorder.key_hit("two_fetch_mainpipe_hit_pattern", "hit_miss")
    assert recorder.key_hit("two_fetch_mainpipe_completion", "wait_refill_then_dual")


@pytest.mark.parametrize(
    ("should_fetch", "expected_bin"),
    [
        ((0, 0, 0, 0), "hit_hit"),
        ((0, 0, 1, 0), "hit_miss"),
        ((1, 0, 0, 0), "miss_hit"),
        ((1, 0, 1, 0), "miss_miss"),
    ],
)
def test_two_fetch_mainpipe_classifies_all_hit_miss_combinations(
    tmp_path, should_fetch, expected_bin
):
    recorder, env, dut = _make_recorder(tmp_path / expected_bin)
    dut.set_key("main_s1_valid", 1)
    dut.set_key("main_req1_valid", 1)
    dut.set_key("main_s1_ftq0_flag", 0)
    dut.set_key("main_s1_ftq0_value", 8)
    dut.set_key("main_s1_ftq1_flag", 0)
    dut.set_key("main_s1_ftq1_value", 9)
    for index, value in enumerate(should_fetch):
        dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_shouldFetch_{index}", value)
        req, line = divmod(index, 2)
        dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValid_{req}_{line}", 0)
        dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValidReg_{req}_{line}", 0)
    dut.set_key("ifu_req0_ftq_flag", 0)
    dut.set_key("ifu_req0_ftq_value", 8)
    dut.set_key("ifu_req1_ftq_flag", 0)
    dut.set_key("ifu_req1_ftq_value", 9)
    dut.set_key("ifu_ready", 1)
    dut.set_key("ifu_req1_valid", 1)

    required = [index for index, value in enumerate(should_fetch) if value]
    dut.set_key("ifu_valid", int(not required))
    sample_two_fetch_coverage(recorder, env, 1)
    if required:
        for index in required:
            req, line = divmod(index, 2)
            dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValid_{req}_{line}", 1)
        sample_two_fetch_coverage(recorder, env, 2)
        for index in required:
            req, line = divmod(index, 2)
            dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValid_{req}_{line}", 0)
            dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValidReg_{req}_{line}", 1)
        dut.set_key("ifu_valid", 1)
        sample_two_fetch_coverage(recorder, env, 3)

    assert recorder.key_hit("two_fetch_mainpipe_hit_pattern", expected_bin)
    assert sum(
        recorder.key_hit("two_fetch_mainpipe_hit_pattern", candidate)
        for candidate in ("hit_hit", "hit_miss", "miss_hit", "miss_miss")
    ) == 1


@pytest.mark.parametrize("gate_key", ["main_s1_exception", "main_s1_mmio"])
def test_two_fetch_mainpipe_exception_or_mmio_is_not_hit_hit(tmp_path, gate_key):
    recorder, env, dut = _make_recorder(tmp_path / gate_key)
    dut.set_key(gate_key, 1)
    dut.set_key("main_s1_valid", 1)
    dut.set_key("main_req1_valid", 1)
    dut.set_key("main_s1_ftq0_flag", 0)
    dut.set_key("main_s1_ftq0_value", 8)
    dut.set_key("main_s1_ftq1_flag", 0)
    dut.set_key("main_s1_ftq1_value", 9)
    for index in range(4):
        dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_shouldFetch_{index}", 0)
    dut.set_key("ifu_valid", 1)
    dut.set_key("ifu_ready", 1)
    dut.set_key("ifu_req1_valid", 1)
    dut.set_key("ifu_req0_ftq_flag", 0)
    dut.set_key("ifu_req0_ftq_value", 8)
    dut.set_key("ifu_req1_ftq_flag", 0)
    dut.set_key("ifu_req1_ftq_value", 9)

    sample_two_fetch_coverage(recorder, env, 1)

    assert not recorder.key_hit("two_fetch_mainpipe_hit_pattern", "hit_hit")


@pytest.mark.parametrize("flush_key", ["main_s1_flush", "bpu_s3_flush", "backend_redirect"])
def test_two_fetch_flush_discards_pending_refill_association(tmp_path, flush_key):
    recorder, env, dut = _make_recorder(tmp_path / flush_key)
    for candidate in ("main_s1_flush", "bpu_s3_flush", "backend_redirect"):
        dut.set_key(candidate, 0)
    dut.set_key("main_s1_valid", 1)
    dut.set_key("main_req1_valid", 1)
    dut.set_key("main_s1_ftq0_flag", 0)
    dut.set_key("main_s1_ftq0_value", 10)
    dut.set_key("main_s1_ftq1_flag", 0)
    dut.set_key("main_s1_ftq1_value", 11)
    for index, value in enumerate((1, 0, 0, 0)):
        dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_shouldFetch_{index}", value)
    sample_two_fetch_coverage(recorder, env, 1)
    assert recorder._two_fetch_refill_pending is not None

    dut.set_key("main_s1_valid", 0)
    dut.set_key(flush_key, 1)
    sample_two_fetch_coverage(recorder, env, 2)

    assert recorder._two_fetch_refill_pending is None


def test_two_fetch_bpu_s3_flush_drop_requires_pending_pointer_match(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("ftq_valid", 1)
    dut.set_key("ftq_ready", 0)
    dut.set_key("ftq_req1_valid", 1)
    dut.set_key("fetch_ptr_flag", 0)
    dut.set_key("fetch_ptr_value", 12)

    sample_two_fetch_coverage(recorder, env, 1)

    dut.set_key("bpu_s3_flush", 1)
    dut.set_key("bpu_s3_flush_ptr_flag", 0)
    dut.set_key("bpu_s3_flush_ptr_value", 12)
    sample_two_fetch_coverage(recorder, env, 2)

    assert recorder.key_hit("two_fetch_flush_flow", "bpu_s3_drop_before_issue")

    mismatch, mismatch_env, mismatch_dut = _make_recorder(tmp_path / "mismatch")
    mismatch_dut.set_key("ftq_valid", 1)
    mismatch_dut.set_key("ftq_ready", 0)
    mismatch_dut.set_key("ftq_req1_valid", 1)
    mismatch_dut.set_key("fetch_ptr_flag", 0)
    mismatch_dut.set_key("fetch_ptr_value", 12)
    sample_two_fetch_coverage(mismatch, mismatch_env, 1)

    mismatch_dut.set_key("bpu_s3_flush", 1)
    mismatch_dut.set_key("bpu_s3_flush_ptr_flag", 0)
    mismatch_dut.set_key("bpu_s3_flush_ptr_value", 13)
    sample_two_fetch_coverage(mismatch, mismatch_env, 2)

    assert not mismatch.key_hit("two_fetch_flush_flow", "bpu_s3_drop_before_issue")
    assert any(
        item.get("event") == "bpu_s3_pending_dual_flush_ptr_unmatched_or_unobservable"
        for item in mismatch.risk_observations
    )


def test_two_fetch_replacing_pending_refill_fails_checker(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("main_s1_valid", 1)
    dut.set_key("main_req1_valid", 1)
    dut.set_key("main_s1_ftq0_flag", 0)
    dut.set_key("main_s1_ftq0_value", 12)
    dut.set_key("main_s1_ftq1_flag", 0)
    dut.set_key("main_s1_ftq1_value", 13)
    for index, value in enumerate((1, 0, 0, 0)):
        dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_shouldFetch_{index}", value)
    sample_two_fetch_coverage(recorder, env, 1)

    dut.set_key("main_s1_ftq0_value", 14)
    dut.set_key("main_s1_ftq1_value", 15)
    sample_two_fetch_coverage(recorder, env, 2)
    raw = recorder._raw_dict()

    assert any(
        item.get("event") == "two_fetch_refill_replaced_before_completion"
        for item in recorder.risk_observations
    )
    assert raw["checker"]["status"] == "fail"


def test_two_fetch_checker_second_invalid_taken_bin(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    recorder._two_fetch_last_dual_cycle = 6
    dut.set_key("checker_valid", 1)
    dut.set_key("checker_select", 1)
    dut.set_key("checker_invalid", 1)
    dut.set_key("fixed_instr_valid", 1)

    sample_two_fetch_coverage(recorder, env, 7)

    assert recorder.key_hit("two_fetch_checker_redirect", "second_block")
    assert recorder.key_hit("two_fetch_checker_priority", "second_after_first_valid")
    assert recorder.key_hit("two_fetch_invalid_taken", "second_redirect")


def test_two_fetch_first_invalid_taken_uses_s1_mask_observation(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("ifu_s1_valid", 1)
    dut.set_key("ifu_first_invalid", 1)
    dut.set_key("ifu_s1_instr_count", 8)
    dut.set_key("ifu_req1_valid", 1)
    _set_raw_instr_vector(dut, first_size=8)

    sample_two_fetch_coverage(recorder, env, 9)

    assert recorder.key_hit("two_fetch_invalid_taken", "first_masks_second")


def test_two_fetch_rvi_stitch_requires_exact_boundary_data_and_no_duplicate(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("ifu_valid", 1)
    dut.set_key("ifu_ready", 1)
    dut.set_key("ifu_req1_valid", 1)
    dut.set_key("ifu_req0_size", 8)
    dut.set_key("ifu_req0_start", 0x40000000)
    dut.set_key("ifu_req1_start", 0x40000008)
    _set_raw_instr_vector(dut, first_size=8, cross_index=7, cross_data=0)

    sample_two_fetch_coverage(recorder, env, 1)

    assert recorder.key_hit("two_fetch_ifu_source", "blocksel_switch")
    assert recorder.key_hit("two_fetch_cross_block", "rvi_stitch")


def test_two_fetch_rvi_stitch_does_not_hit_when_second_half_is_duplicated(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("ifu_valid", 1)
    dut.set_key("ifu_ready", 1)
    dut.set_key("ifu_req1_valid", 1)
    dut.set_key("ifu_req0_size", 8)
    dut.set_key("ifu_req0_start", 0x40000000)
    dut.set_key("ifu_req1_start", 0x40000008)
    _set_raw_instr_vector(dut, first_size=8, cross_index=7, cross_data=0)
    base = "Frontend_top.Frontend.inner_ifu.instrBoundary."
    dut.set(f"{base}io_resp_rawInstrVec_8_valid", 1)
    dut.set(f"{base}__Vtogcov__io_resp_rawInstrVec_8_valid", 1)

    sample_two_fetch_coverage(recorder, env, 1)

    assert not recorder.key_hit("two_fetch_cross_block", "rvi_stitch")


def test_two_fetch_ibuffer_backpressure_requires_stable_full_payload(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("ifu_second_valid", 1)
    dut.set_key("ifu_s2_valid", 1)
    dut.set_key("to_ibuffer_valid", 1)
    dut.set_key("to_ibuffer_ready", 0)
    offset = Path(__file__).resolve().parents[5] / "build-frontend/pylib/Frontend/Frontend_offset.yaml"
    prefix = "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_"
    names = sorted(
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ") and line[len("  - name: ") :].startswith(prefix)
    )
    assert names
    for index, name in enumerate(names):
        dut.set(name, index + 1)

    sample_two_fetch_coverage(recorder, env, 10)
    assert not recorder.key_hit("two_fetch_delivery", "dual_stall")
    sample_two_fetch_coverage(recorder, env, 11)
    assert not recorder.key_hit("two_fetch_delivery", "dual_stall")
    dut.set_key("to_ibuffer_ready", 1)
    sample_two_fetch_coverage(recorder, env, 12)
    assert recorder.key_hit("two_fetch_delivery", "dual_stall")

    dut.set_key("to_ibuffer_ready", 0)
    sample_two_fetch_coverage(recorder, env, 13)
    dut.set(names[-1], 0xBAD)
    sample_two_fetch_coverage(recorder, env, 14)
    assert any(
        item.get("event") == "ibuffer_payload_changed_under_backpressure"
        for item in recorder.risk_observations
    )


def test_two_fetch_ibuffer_fire_ends_the_payload_hold_window(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("ifu_second_valid", 1)
    dut.set_key("ifu_s2_valid", 1)
    dut.set_key("to_ibuffer_valid", 1)
    offset = Path(__file__).resolve().parents[5] / "build-frontend/pylib/Frontend/Frontend_offset.yaml"
    prefix = "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_"
    names = sorted(
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ") and line[len("  - name: ") :].startswith(prefix)
    )
    assert names
    for index, name in enumerate(names):
        dut.set(name, index + 1)

    dut.set_key("to_ibuffer_ready", 0)
    sample_two_fetch_coverage(recorder, env, 1)
    assert recorder._two_fetch_stalled_payload is not None

    dut.set_key("to_ibuffer_ready", 1)
    sample_two_fetch_coverage(recorder, env, 2)
    assert recorder._two_fetch_stalled_payload is None

    dut.set(names[-1], 0xBAD)
    dut.set_key("to_ibuffer_ready", 0)
    sample_two_fetch_coverage(recorder, env, 3)
    assert recorder._two_fetch_stalled_payload is not None
    assert not any(
        item.get("event") == "ibuffer_payload_changed_under_backpressure"
        for item in recorder.risk_observations
    )


def test_checker_priority_does_not_infer_two_faults_from_select_only(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    recorder._two_fetch_last_dual_cycle = 6
    dut.set_key("checker_valid", 1)
    dut.set_key("checker_select", 0)

    sample_two_fetch_coverage(recorder, env, 7)

    assert recorder.key_hit("two_fetch_checker_redirect", "first_block")
    assert not recorder.key_hit("two_fetch_checker_priority", "first_masks_second")


def test_compressed_cfvec_uses_expanded_instruction_for_cfi_classification(tmp_path):
    from env.funcov import _classify_cfi_kind

    assert _classify_cfi_kind(0x0000006F, True) == "jal"
    assert _classify_cfi_kind(0x00000063, True) == "branch"
    assert _classify_cfi_kind(0x00008067, True) == "jalr"


def test_two_fetch_backend_two_ftq_source_and_mixed_bins(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    recorder._two_fetch_last_dual_cycle = 8
    recorder._two_fetch_expected_cfvec = {
        "tags": ((0, 3), (0, 4)),
        "cycle": 8,
    }
    for slot in range(8):
        dut.set(f"io_backend_cfVec_{slot}_valid", 0)

    entries = (
        (0, 0x8000001E, 1, 0x00000001, 0, 3),
        (1, 0x80000020, 0, 0x00000013, 0, 4),
    )
    for slot, pc, is_rvc, instr, flag, value in entries:
        base = f"io_backend_cfVec_{slot}_"
        dut.set(base + "valid", 1)
        dut.set(base + "bits_pc", pc)
        dut.set(base + "bits_isRvc", is_rvc)
        dut.set(base + "bits_instr", instr)
        dut.set(base + "bits_predTaken", 0)
        dut.set(base + "bits_ftqPtr_flag", flag)
        dut.set(base + "bits_ftqPtr_value", value)
        for cause in (1, 2, 12, 19, 20):
            dut.set(base + f"bits_exceptionVec_{cause}", 0)

    sample_cfvec_coverage(recorder, env, 9)

    assert recorder.key_hit("two_fetch_ifu_source", "two_ftq_sources")
    assert recorder.key_hit("two_fetch_delivery", "two_ftq_entries_same_cycle")
    assert recorder.key_hit("two_fetch_cross_block", "mixed_rvc_rvi")
    assert recorder.key_hit("two_fetch_cross_block", "rvc_independent")


def test_two_fetch_backend_sources_require_exact_delivered_ftq_pair(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    recorder._two_fetch_last_dual_cycle = 8
    recorder._two_fetch_expected_cfvec = {
        "tags": ((0, 3), (0, 4)),
        "cycle": 8,
    }
    _set_cfvec_entries(
        dut,
        [
            (0, 0x80000000, 0, 0x00000013),
            (1, 0x80000004, 1, 0x00000001),
        ],
    )

    sample_cfvec_coverage(recorder, env, 9)

    assert not recorder.key_hit("two_fetch_ifu_source", "two_ftq_sources")
    assert not recorder.key_hit("two_fetch_delivery", "two_ftq_entries_same_cycle")


def test_backend_redirect_requires_old_tags_dropped_and_new_target_delivery(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("ifu_second_valid", 0)
    dut.set_key("ifu_s2_valid", 0)
    dut.set_key("to_ibuffer_valid", 0)
    dut.set_key("to_ibuffer_ready", 0)
    dut.set_key("ifu_s2_ftq0_flag", 0)
    dut.set_key("ifu_s2_ftq0_value", 0)
    dut.set_key("ifu_s2_ftq1_flag", 0)
    dut.set_key("ifu_s2_ftq1_value", 0)
    _set_ibuffer_entries(dut, [])
    recorder._two_fetch_expected_cfvec = {
        "tags": ((0, 10), (0, 11)),
        "cycle": 1,
    }
    target = 0x80000100
    dut.set_key("backend_redirect", 1)
    dut.set_key("backend_redirect_target", target)
    dut.set_key("ifu_flush", 1)

    sample_two_fetch_coverage(recorder, env, 1)

    assert {
        key: recorder._two_fetch_redirect_pending.get(key)
        for key in ("old_tags", "target", "cycle")
    } == {
        "old_tags": ((0, 10), (0, 11)),
        "target": target,
        "cycle": 1,
    }
    assert not recorder.key_hit("two_fetch_flush_flow", "backend_redirect_drops_inflight")

    dut.set_key("backend_redirect", 0)
    dut.set_key("ifu_flush", 0)
    dut.set_key("ifu_second_valid", 1)
    dut.set_key("ifu_s2_valid", 1)
    dut.set_key("ifu_s2_ftq0_flag", 0)
    dut.set_key("ifu_s2_ftq0_value", 20)
    dut.set_key("ifu_s2_ftq1_flag", 0)
    dut.set_key("ifu_s2_ftq1_value", 21)
    dut.set_key("to_ibuffer_valid", 1)
    dut.set_key("to_ibuffer_ready", 1)
    _set_ibuffer_entries(
        dut,
        [
            (0, target >> 1, 1, 0, 20),
            (1, (target >> 1) + 1, 0, 0, 20),
            (2, (target >> 1) + 3, 1, 0, 21),
        ],
    )

    sample_two_fetch_coverage(recorder, env, 2)

    assert recorder.key_hit("two_fetch_flush_flow", "backend_redirect_drops_inflight")
    assert not recorder.risk_observations


def test_backend_redirect_old_tag_delivery_is_a_protocol_risk_not_a_hit(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("ifu_second_valid", 0)
    dut.set_key("ifu_s2_valid", 0)
    dut.set_key("to_ibuffer_valid", 0)
    dut.set_key("to_ibuffer_ready", 0)
    dut.set_key("ifu_s2_ftq0_flag", 0)
    dut.set_key("ifu_s2_ftq0_value", 0)
    dut.set_key("ifu_s2_ftq1_flag", 0)
    dut.set_key("ifu_s2_ftq1_value", 0)
    _set_ibuffer_entries(dut, [])
    recorder._two_fetch_expected_cfvec = {
        "tags": ((0, 10), (0, 11)),
        "cycle": 1,
    }
    target = 0x80000100
    dut.set_key("backend_redirect", 1)
    dut.set_key("backend_redirect_target", target)
    dut.set_key("ifu_flush", 1)
    sample_two_fetch_coverage(recorder, env, 1)

    dut.set_key("backend_redirect", 0)
    dut.set_key("ifu_flush", 0)
    dut.set_key("ifu_second_valid", 1)
    dut.set_key("ifu_s2_valid", 1)
    dut.set_key("ifu_s2_ftq0_flag", 0)
    dut.set_key("ifu_s2_ftq0_value", 10)
    dut.set_key("ifu_s2_ftq1_flag", 0)
    dut.set_key("ifu_s2_ftq1_value", 11)
    dut.set_key("to_ibuffer_valid", 1)
    dut.set_key("to_ibuffer_ready", 1)
    _set_ibuffer_entries(
        dut,
        [
            (0, 0x40000000, 1, 0, 10),
            (1, 0x40000010, 1, 0, 11),
        ],
    )

    sample_two_fetch_coverage(recorder, env, 2)

    assert not recorder.key_hit("two_fetch_flush_flow", "backend_redirect_drops_inflight")
    assert any(
        item.get("event") == "two_fetch_redirect_old_tag_delivery"
        for item in recorder.risk_observations
    )


def test_canonical_registry_matches_the_single_sampler_contract():
    repo_root = Path(__file__).resolve().parents[5]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_功能覆盖率建模/frontend_bt_functional_coverage_pilot.csv"
    )
    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        active = {
            (row["Coverage_Group"].strip(), row["Bin_Name"].strip())
            for row in csv.DictReader(handle)
            if row["Coverpoint"].strip()
        }
    assert len(active) == 64
    assert active == set(FUNCTIONAL_COVERAGE_SAMPLER_BIN_KEYS)
    assert len(CFVEC_SAMPLER_BIN_KEYS) == 17
    assert len(TWO_FETCH_SAMPLER_BIN_KEYS) == 41


def test_cfvec_mixed_bins_are_window_scoped_and_do_not_hit_on_rvi_only(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path / "homogeneous")

    _set_cfvec_entries(dut, [(0, 0x80000000, 0, 0x00000013)])
    sample_cfvec_coverage(recorder, env, 1)
    _set_cfvec_entries(dut, [(0, 0x80000004, 0, 0x00000013)])
    sample_cfvec_coverage(recorder, env, 2)

    assert not recorder.key_hit("ifu_instr_size_type", "mixed_rvi_rvc_seen")
    assert not recorder.key_hit("ifu_pc_step_type", "mixed_no_gap_no_dup")

    mixed, mixed_env, mixed_dut = _make_recorder(tmp_path / "mixed")
    _set_cfvec_entries(
        mixed_dut,
        [
            (0, 0x80000000, 0, 0x00000013),
            (1, 0x80000004, 1, 0x00000001),
        ],
    )
    sample_cfvec_coverage(mixed, mixed_env, 3)

    assert mixed.key_hit("ifu_instr_size_type", "mixed_rvi_rvc_seen")
    assert mixed.key_hit("ifu_pc_step_type", "mixed_no_gap_no_dup")


def test_cfvec_redirect_invalidates_current_and_next_cycle(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    recorder.handle_event({"type": "backend.redirect", "cycle": 10, "payload": {}})
    _set_cfvec_entries(dut, [(0, 0x80000000, 0, 0x00000013)])
    sample_cfvec_coverage(recorder, env, 10)
    sample_cfvec_coverage(recorder, env, 11)
    assert not recorder.key_hit("ifu_instr_size_type", "rvi_seen")

    sample_cfvec_coverage(recorder, env, 12)
    assert recorder.key_hit("ifu_instr_size_type", "rvi_seen")


def test_on_cycle_gates_sampling_during_reset_and_resumes_after_release(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_cfvec_entries(dut, [(0, 0x80000000, 0, 0x00000013)])
    dut.set("reset", 1)
    recorder._ifu_last_cfvec = {"pc": 0xDEADBEEF, "is_rvc": 0, "slot": 0}

    recorder.on_cycle(1, env)

    assert recorder._ifu_last_cfvec is None
    assert not recorder.key_hit("ifu_instr_size_type", "rvi_seen")

    dut.set("reset", 0)
    recorder.on_cycle(2, env)

    assert recorder._reset_release_cycle == 2
    assert recorder.key_hit("ifu_instr_size_type", "rvi_seen")


def test_sampler_cannot_silently_mark_an_unmodeled_bin(tmp_path):
    recorder, _env, _dut = _make_recorder(tmp_path)
    with pytest.raises(KeyError, match="unmodeled bin"):
        recorder.mark("old_bpu_group", "old_ftq_bin", 1)


def test_two_fetch_signal_map_matches_current_frontend_offset():
    offset = Path(__file__).resolve().parents[5] / "build-frontend/pylib/Frontend/Frontend_offset.yaml"
    assert offset.exists(), "DUT signal inventory is required before signal-contract tests"

    registered = {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }
    dynamic_ftq_start_keys = {"ftq_req0_start", "ftq_req1_start"}
    assert all(
        any(signal in registered for signal in candidates)
        for key, candidates in _TWO_FETCH_SIGNALS.items()
        if key not in dynamic_ftq_start_keys
    )
    assert {
        "Frontend_top.Frontend.inner_ftq.entryQueue_0_startPc_addr",
        "Frontend_top.Frontend.inner_ftq.entryQueue_63_startPc_addr",
    } <= registered


    generated = [
        *(
            f"Frontend_top.Frontend.inner_icache.mainPipe.s1_shouldFetch_{index}"
            for index in range(4)
        ),
        *(
        "Frontend_top.Frontend.inner_icache.wayLookup.entryUpdate_updated"
            + ("" if index == 0 else f"_{index}")
            for index in range(64)
        ),
        *(
            f"Frontend_top.Frontend.inner_icache.wayLookup.entries_{index}_isMmio"
            for index in range(32)
        ),
    ]
    missing_raw_fields = [
        (index, field, _tf_raw_instr_field_candidates(index, field))
        for field in _RAW_INSTR_FIELDS
        for index in range(32)
        if not (
            field == "isCrossBlockInstr"
            and index == 31
            and not any(signal in registered for signal in _tf_raw_instr_field_candidates(index, field))
        )
        and not any(signal in registered for signal in _tf_raw_instr_field_candidates(index, field))
    ]
    generated.extend(
        f"Frontend_top.Frontend.inner_ifu.predChecker.remaskFault_{index}"
        for index in range(34)
    )
    generated.append("Frontend_top.Frontend.inner_ifu.s2_fixedInstrValid")
    generated.extend(
        (
            "Frontend_top.Frontend.inner_icache.mainPipe.s0_fire",
            "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toFtq_fromMainPipe_realTwoFetchValid",
        )
    )
    assert not missing_raw_fields
    assert not [signal for signal in generated if signal not in registered]


def test_funcov_and_backannotation_compatibility_fields_stay_identical():
    assert tuple(functional_coverage_module.COMPATIBILITY_FIELDS) == tuple(
        backannotate_module._COMPATIBILITY_FIELDS
    )


def test_two_fetch_backannotation_matches_registry_and_sampler():
    repo_root = Path(__file__).resolve().parents[5]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_功能覆盖率建模/frontend_bt_functional_coverage_pilot.csv"
    )
    with pilot_path.open(encoding="utf-8-sig", newline="") as f:
        pilot_rows = [row for row in csv.DictReader(f) if row["Bin_ID"].startswith("BIN-5")]
    assert len(pilot_rows) == 41
    assert all(row["Coverpoint"] for row in pilot_rows)
    assert all(
        row["Coverpoint"] == TWO_FETCH_COVERPOINTS[row["Coverage_Group"]]
        for row in pilot_rows
    )
    assert {
        (row["Coverage_Group"], row["Coverpoint"], row["Bin_Name"])
        for row in pilot_rows
    } == {
        (group, TWO_FETCH_COVERPOINTS[group], bin_name)
        for group, bin_name in TWO_FETCH_SAMPLER_BIN_KEYS
    }
    assert {
        (row["Coverage_Group"], row["Bin_Name"])
        for row in pilot_rows
    } == TWO_FETCH_SAMPLER_BIN_KEYS

    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_测试点分解/Frontend_testpoint_0525_coverage_backannotated.csv"
    )
    mapping = validate_mapping(
        testpoint_path,
        load_pilot(pilot_path, bin_prefix="BIN-5"),
        bin_prefix="BIN-5",
    )
    assert len(mapping) == 41


def test_frontend_fixture_has_one_funcov_path_and_keeps_code_coverage(tmp_path):
    repo_root = Path(__file__).resolve().parents[5]
    frontend_root = repo_root / "src/test/python/Frontend"
    fixture_source = (frontend_root / "env/fixtures.py").read_text(encoding="utf-8")
    sampler_source = (frontend_root / "env/funcov.py").read_text(encoding="utf-8")
    recorder_source = (frontend_root / "env/functional_coverage.py").read_text(encoding="utf-8")
    recorder = FunctionalCoverageRecorder.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="contract",
        artifact_tag="contract",
        output_dir=tmp_path,
    )

    assert not (frontend_root / "env/coverage_def.py").exists()
    assert "TB_ENABLE_TOFFEE_FUNCOV" not in fixture_source
    assert "get_coverage_groups" not in fixture_source
    assert "bpu_basic_pred_type" not in sampler_source
    assert "bpu_basic_pred_type" not in recorder_source
    assert "s1_icacheMeta_0_itlbPbmt" not in recorder_source
    assert "s1_icacheMeta_0_pmpMmio" not in recorder_source
    assert "s1_icacheMetaIn_0_itlbPbmt" in recorder_source
    assert "s1_icacheMetaIn_0_pmpMmio" in recorder_source
    assert len(recorder.definitions) == 64
    assert all(item.coverpoint for item in recorder.definitions)
    assert "FunctionalCoverageRecorder.from_pilot_csv" in fixture_source
    assert "set_line_coverage" in fixture_source
    assert "TB_ENABLE_TOFFEE_LINE_COVERAGE" in fixture_source
    assert not (frontend_root / "docs/frontend_bt_functional_coverage_pilot.csv").exists()


def test_frontend_runners_keep_artifacts_scoped_to_one_run(tmp_path):
    frontend_root = Path(__file__).resolve().parents[1]
    pipeline_source = (frontend_root / "scripts/run_bin_trace_pipeline.sh").read_text(
        encoding="utf-8"
    )
    wrapper_source = (frontend_root / "scripts/run_baremode_asm_bin_trace.sh").read_text(
        encoding="utf-8"
    )
    suite_source = (frontend_root / "scripts/run_baremode_asm_suite.sh").read_text(
        encoding="utf-8"
    )
    asm_source = (frontend_root / "scripts/asm_to_jsonl.sh").read_text(encoding="utf-8")
    raw_coverage_source = (frontend_root / "scripts/report_raw_code_coverage.py").read_text(
        encoding="utf-8"
    )
    html_coverage_source = (frontend_root / "scripts/gen_coverage_html.sh").read_text(
        encoding="utf-8"
    )

    assert (
        'ARTIFACT_ROOT="${TB_ARTIFACT_DIR:-${FRONTEND_DIR}/data/runs/${RUN_ID}}"'
        in pipeline_source
    )
    assert "TB_RUN_ID must contain only" in pipeline_source
    assert (
        'TB_ARTIFACT_DIR="${TB_ARTIFACT_DIR:-${FRONTEND_DIR}/data/runs/${TB_RUN_ID}}"'
        in wrapper_source
    )
    assert 'TB_RUN_COMMAND="${TB_RUN_COMMAND% }"' in wrapper_source
    assert '${ARTIFACT_ROOT}/inputs/${BIN_STEM}.trace.jsonl' in pipeline_source
    assert '${ARTIFACT_ROOT}/inputs/${BIN_STEM}.nemu.log' in pipeline_source
    assert 'PIPELINE_REASON="artifact_output_not_empty"' in pipeline_source
    assert 'NEMU_LOG_PATH="${NEMU_LOG_PATH}"' in wrapper_source
    assert "refusing to overwrite non-empty run root" in wrapper_source
    assert "refusing to overwrite non-empty run root" in asm_source
    assert "fe_baremode_python_pilot_mix" not in wrapper_source
    assert not (frontend_root / "tests/asm_cases/fe_baremode_python_pilot_mix.S").exists()
    output_vars = {
        "coverage": "TB_COVERAGE_DIR",
        "waveforms": "TB_WAVEFORM_DIR",
        "funcov": "TB_FUNCOV_DIR",
        "logs": "TB_CASE_LOG_DIR",
    }
    for directory, env_name in output_vars.items():
        assert f'{env_name}="${{case_artifact_dir}}/{directory}"' in suite_source
    assert 'case_run_id="${SUITE_ID}_${case_stem}"' in suite_source
    assert '--glob "${SUITE_ID}_*/coverage/*.dat"' in suite_source
    assert "tools/backannotate_funcov.py" in suite_source
    assert "tools/merge_funcov.py" in suite_source
    assert "backannotation_audit.json" in suite_source
    assert "code_coverage_summary.json" in suite_source
    assert "${SUITE_ID}_observed" in suite_source
    assert "--check" in suite_source
    assert 'raw coverage summary skipped: TB_RUN_DUT=0' in suite_source
    assert "DATE_STAMP" not in suite_source
    suite_cases = set(re.findall(r"asm_cases/([A-Za-z0-9_]+)\.S", suite_source))
    active_testcases = {
        item.suggested_testcase
        for item in FunctionalCoverageRecorder.from_pilot_csv(
            default_pilot_csv_path(),
            testcase_name="runner-contract",
            artifact_tag="runner-contract",
            output_dir=tmp_path,
        ).definitions
    }
    assert suite_cases
    assert suite_cases <= active_testcases
    for source in (pipeline_source, wrapper_source, suite_source, asm_source):
        assert "/NEMU/logs/" not in source
    assert 'required=True' in raw_coverage_source
    assert '"--json-output"' in raw_coverage_source
    assert 'coverage input is required' in html_coverage_source
    assert 'DEFAULT_DATA_DIR' not in html_coverage_source


def _write_codecov_provenance_fixture(
    *,
    dat_path: Path,
    source_root: Path,
    run_id: str = "unit-codecov-case",
) -> Path:
    pylib = source_root / "pylib" / "Frontend"
    rtl = source_root / "rtl"
    pylib.mkdir(parents=True, exist_ok=True)
    rtl.mkdir(parents=True, exist_ok=True)
    (pylib / "libUTFrontend.so").write_bytes(b"unit-dut-model")
    (pylib / "_UT_Frontend.so").write_bytes(b"unit-python-extension")
    (pylib / "Frontend_offset.yaml").write_text("signals: []\n", encoding="utf-8")
    (rtl / "Frontend.sv").write_text("module Frontend; endmodule\n", encoding="utf-8")
    manifest_path = source_root / "frontend_build_manifest.json"
    manifest = write_frontend_build_manifest(
        manifest_path,
        build_root=source_root,
        dut_source_sha="a" * 40,
        source_tree_dirty=False,
        build_config="frontend-unit",
        build_command="make frontend",
    )
    build_hashes = manifest["artifacts"]
    provenance = {
        "dut_source_sha": "a" * 40,
        "implementation_sha": "a" * 40,
        "design_baseline_sha": "a" * 40,
        "source_sha_override": False,
        "source_delta_sha256": hashlib.sha256(b"").hexdigest(),
        "source_delta_files": [],
        "source_delta_policy": "none",
        **build_hashes,
        "registry_sha256": "5" * 64,
        "sampler_sha256": "6" * 64,
        "build_config": "frontend-unit",
        "toolchain": "python-unit",
        "build_manifest_status": "valid",
        "build_manifest_reasons": [],
        "build_manifest_path": str(manifest_path.resolve()),
        "build_manifest_sha256": hashlib.sha256(manifest_path.read_bytes()).hexdigest(),
    }
    compatibility_payload = {
        field: provenance[field]
        for field in functional_coverage_module.COMPATIBILITY_FIELDS
    }
    provenance["compatibility_signature"] = hashlib.sha256(
        json.dumps(
            compatibility_payload,
            ensure_ascii=False,
            sort_keys=True,
            separators=(",", ":"),
        ).encode("utf-8")
    ).hexdigest()
    funcov_dir = dat_path.parent.parent / "funcov"
    waveform_dir = dat_path.parent.parent / "waveforms"
    funcov_dir.mkdir(parents=True, exist_ok=True)
    waveform_dir.mkdir(parents=True, exist_ok=True)
    waveform_path = waveform_dir / f"{dat_path.stem}.fst"
    waveform_path.write_bytes(b"unit-waveform")
    sidecar_path = funcov_dir / f"{dat_path.stem}.funcov.json"
    sidecar_path.write_text(
        json.dumps(
            {
                "artifact_schema_version": 2,
                "line_coverage_path": str(dat_path.resolve()),
                "waveform_path": str(waveform_path.resolve()),
                "provenance": provenance,
                "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
                "errors": [],
                "run": {
                    "run_id": run_id,
                    "pytest_outcome": "passed",
                    "exit_code": 0,
                    "checker": {"status": "pass", "error_count": 0, "errors": []},
                },
            },
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )
    return sidecar_path


def test_raw_code_coverage_report_writes_run_scoped_json(tmp_path):
    data_dir = tmp_path / "coverage"
    source_root = tmp_path / "build-frontend"
    output_path = tmp_path / "report" / "code_coverage_summary.json"
    data_dir.mkdir()
    source_root.mkdir()
    (source_root / "Frontend.sv").write_text("module Frontend;\nendmodule\n", encoding="utf-8")
    (data_dir / "case.dat").write_text(
        "C \x01f\x02Frontend.sv\x01t\x02line\x01x\x021\x02 1\n"
        "C \x01f\x02Frontend.sv\x01t\x02branch\x01x\x022\x02 0\n",
        encoding="utf-8",
    )
    sidecar_path = _write_codecov_provenance_fixture(
        dat_path=data_dir / "case.dat",
        source_root=source_root,
    )
    frontend_root = Path(__file__).resolve().parents[1]

    result = subprocess.run(
        [
            sys.executable,
            str(frontend_root / "scripts/report_raw_code_coverage.py"),
            "--data-dir",
            str(data_dir),
            "--source-root",
            str(source_root),
            "--run-id",
            "unit-codecov-json",
            "--json-output",
            str(output_path),
        ],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )

    assert result.returncode == 0, result.stderr
    summary = json.loads(output_path.read_text(encoding="utf-8"))
    assert summary["run_id"] == "unit-codecov-json"
    assert summary["dat_files"] == [
        {"path": str((data_dir / "case.dat").resolve()), "size_bytes": 64}
    ]
    assert summary["overall"]["line"] == {"hit": 1, "total": 1, "pct": 100.0}
    assert summary["overall"]["branch"] == {"hit": 0, "total": 1, "pct": 0.0}
    assert summary["scopes"]["all"]["source_lines"] == 3
    assert summary["provenance"]["run_ids"] == ["unit-codecov-case"]
    assert summary["provenance"]["dat_files"][0]["funcov_path"] == str(
        sidecar_path.resolve()
    )


@pytest.mark.parametrize(
    ("mutation", "expected_error"),
    [
        ("missing_sidecar", "missing funcov sidecar"),
        ("empty_dat", "empty .dat"),
        ("manifest_hash", "manifest hash mismatch"),
        ("dut_hash", "dut_build_sha256 mismatch"),
        ("runtime_dut", "build manifest runtime validation failed"),
        ("compatibility", "compatibility signature mismatch"),
        ("monitor", "monitor errors present"),
        ("monitor_missing_cycles", "monitor cycles_total is missing"),
        ("monitor_missing_count", "monitor error_count is missing"),
        ("checker_missing_count", "checker error_count is missing"),
        ("checker_missing_errors", "checker error details present"),
        ("raw_errors_missing", "funcov errors present"),
        ("waveform", "waveform gate failed"),
    ],
)
def test_raw_code_coverage_report_rejects_unproven_dat(
    tmp_path, mutation, expected_error
):
    data_dir = tmp_path / "coverage"
    source_root = tmp_path / "build-frontend"
    data_dir.mkdir()
    source_root.mkdir()
    (source_root / "Frontend.sv").write_text("module Frontend;\nendmodule\n", encoding="utf-8")
    dat_path = data_dir / "case.dat"
    dat_path.write_text(
        "C \x01f\x02Frontend.sv\x01t\x02line\x01x\x021\x02 1\n",
        encoding="utf-8",
    )
    sidecar_path = _write_codecov_provenance_fixture(
        dat_path=dat_path,
        source_root=source_root,
    )
    if mutation == "missing_sidecar":
        sidecar_path.unlink()
    elif mutation == "empty_dat":
        dat_path.write_bytes(b"")
    elif mutation == "runtime_dut":
        (source_root / "pylib" / "Frontend" / "libUTFrontend.so").write_bytes(
            b"tampered-after-manifest"
        )
    else:
        sidecar = json.loads(sidecar_path.read_text(encoding="utf-8"))
        if mutation == "manifest_hash":
            sidecar["provenance"]["build_manifest_sha256"] = "f" * 64
        elif mutation == "dut_hash":
            sidecar["provenance"]["dut_build_sha256"] = "f" * 64
            payload = {
                field: sidecar["provenance"][field]
                for field in functional_coverage_module.COMPATIBILITY_FIELDS
            }
            sidecar["provenance"]["compatibility_signature"] = hashlib.sha256(
                json.dumps(payload, sort_keys=True, separators=(",", ":")).encode("utf-8")
            ).hexdigest()
        elif mutation == "compatibility":
            sidecar["provenance"]["compatibility_signature"] = "f" * 64
        elif mutation == "monitor":
            sidecar["stats"]["monitor"]["error_count"] = 1
        elif mutation == "monitor_missing_cycles":
            del sidecar["stats"]["monitor"]["cycles_total"]
        elif mutation == "monitor_missing_count":
            del sidecar["stats"]["monitor"]["error_count"]
        elif mutation == "checker_missing_count":
            del sidecar["run"]["checker"]["error_count"]
        elif mutation == "checker_missing_errors":
            del sidecar["run"]["checker"]["errors"]
        elif mutation == "raw_errors_missing":
            del sidecar["errors"]
        elif mutation == "waveform":
            Path(sidecar["waveform_path"]).unlink()
        sidecar_path.write_text(json.dumps(sidecar) + "\n", encoding="utf-8")

    frontend_root = Path(__file__).resolve().parents[1]
    result = subprocess.run(
        [
            sys.executable,
            str(frontend_root / "scripts/report_raw_code_coverage.py"),
            "--data-dir",
            str(data_dir),
            "--source-root",
            str(source_root),
        ],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )

    assert result.returncode == 2
    assert expected_error in result.stderr


def test_raw_code_coverage_report_rejects_duplicate_run_ids(tmp_path):
    data_dir = tmp_path / "coverage"
    source_root = tmp_path / "build-frontend"
    data_dir.mkdir()
    source_root.mkdir()
    first_dat = data_dir / "first.dat"
    second_dat = data_dir / "second.dat"
    coverage_record = "C \x01f\x02Frontend.sv\x01t\x02line\x01x\x021\x02 1\n"
    first_dat.write_text(coverage_record, encoding="utf-8")
    second_dat.write_text(coverage_record, encoding="utf-8")
    first_sidecar = _write_codecov_provenance_fixture(
        dat_path=first_dat,
        source_root=source_root,
        run_id="duplicate-unit-run",
    )
    second_sidecar = first_sidecar.with_name("second.funcov.json")
    second = json.loads(first_sidecar.read_text(encoding="utf-8"))
    second["line_coverage_path"] = str(second_dat.resolve())
    second_sidecar.write_text(json.dumps(second) + "\n", encoding="utf-8")

    frontend_root = Path(__file__).resolve().parents[1]
    result = subprocess.run(
        [
            sys.executable,
            str(frontend_root / "scripts/report_raw_code_coverage.py"),
            "--data-dir",
            str(data_dir),
            "--source-root",
            str(source_root),
        ],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )

    assert result.returncode == 2
    assert "duplicate run_id across .dat files" in result.stderr


def test_effective_run_id_honors_explicit_value_after_default_generation(monkeypatch):
    import env.fixtures as fixtures_module

    previous_default = fixtures_module._DEFAULT_RUN_ID
    try:
        fixtures_module._DEFAULT_RUN_ID = None
        monkeypatch.delenv("TB_RUN_ID", raising=False)
        generated = fixtures_module._effective_run_id()
        assert generated.startswith("frontend_pytest_")

        monkeypatch.setenv("TB_RUN_ID", "late-explicit-run")
        assert fixtures_module._effective_run_id() == "late-explicit-run"
    finally:
        fixtures_module._DEFAULT_RUN_ID = previous_default


def test_funcov_artifact_uses_coverpoint_key_and_strict_merge_signature(tmp_path, monkeypatch):
    monkeypatch.setenv("TB_RUN_ID", "unit-merge-1")
    recorder, _env, _dut = _make_recorder(tmp_path)
    _install_eligible_provenance(recorder)
    assert recorder.mark(
        "two_fetch_ftq_eligibility",
        "eligible_dual",
        12,
        {"event": "unit"},
    )
    paths = recorder.write_artifacts()

    raw_path = Path(paths["raw_path"])
    raw = json.loads(raw_path.read_text(encoding="utf-8"))
    key = "two_fetch_ftq_eligibility::request_eligibility::eligible_dual"
    assert raw["artifact_schema_version"] == 2
    assert raw["hits"][key]["hits"] == 1
    assert raw["hits"][key]["coverpoint"] == "request_eligibility"
    assert raw["provenance"]["compatibility_signature"]

    second_path = tmp_path / "second.funcov.json"
    second = json.loads(raw_path.read_text(encoding="utf-8"))
    second["run"] = dict(second["run"])
    second["run"]["run_id"] = "unit-merge-2"
    second_path.write_text(json.dumps(second), encoding="utf-8")
    merged = FunctionalCoverageRecorder.merge_raw_files(
        [raw_path, second_path],
        artifact_tag="merged",
        output_dir=tmp_path / "merged",
    )
    assert merged.hits[
        ("two_fetch_ftq_eligibility", "request_eligibility", "eligible_dual")
    ].hits == 2
    with pytest.raises(ValueError, match="duplicate functional coverage run_id"):
        FunctionalCoverageRecorder.merge_raw_files(
            [raw_path, raw_path],
            artifact_tag="duplicate-run",
            output_dir=tmp_path / "duplicate-run",
        )

    incompatible_path = tmp_path / "incompatible.funcov.json"
    incompatible = dict(raw)
    incompatible["run"] = dict(raw["run"])
    incompatible["run"]["run_id"] = "unit-merge-incompatible"
    incompatible["provenance"] = dict(raw["provenance"])
    incompatible["provenance"]["compatibility_signature"] = "different-version"
    incompatible_path.write_text(json.dumps(incompatible), encoding="utf-8")
    with pytest.raises(ValueError, match="incompatible functional coverage artifacts"):
        FunctionalCoverageRecorder.merge_raw_files(
            [raw_path, incompatible_path],
            artifact_tag="rejected",
            output_dir=tmp_path / "rejected",
        )

    stale_fields_path = tmp_path / "stale-fields.funcov.json"
    stale_fields = json.loads(raw_path.read_text(encoding="utf-8"))
    stale_fields["provenance"]["toolchain"] = "python-tampered"
    stale_fields_path.write_text(json.dumps(stale_fields), encoding="utf-8")
    with pytest.raises(ValueError, match="signature does not match its provenance"):
        FunctionalCoverageRecorder.merge_raw_files(
            [stale_fields_path],
            artifact_tag="rejected-stale-fields",
            output_dir=tmp_path / "rejected-stale-fields",
        )

    stale_definitions_path = tmp_path / "stale-definitions.funcov.json"
    stale_definitions = json.loads(raw_path.read_text(encoding="utf-8"))
    stale_definitions["definitions"][0]["hit_rule"] = "tampered rule"
    stale_definitions_path.write_text(json.dumps(stale_definitions), encoding="utf-8")
    with pytest.raises(ValueError, match="definitions do not match provenance"):
        FunctionalCoverageRecorder.merge_raw_files(
            [stale_definitions_path],
            artifact_tag="rejected-stale-definitions",
            output_dir=tmp_path / "rejected-stale-definitions",
        )


def test_funcov_artifact_records_explicit_targets(tmp_path):
    recorder, _env, _dut = _make_recorder(
        tmp_path,
        target_bin_ids=["BIN-501"],
        target_tp_ids=["TP-001"],
    )

    assert recorder.coverage_targets == {
        "bin_ids": ["BIN-501"],
        "hit_keys": ["two_fetch_ftq_eligibility::request_eligibility::eligible_dual"],
        "tp_ids": ["TP-001"],
        "testcases": [],
    }

    raw = json.loads(Path(recorder.write_artifacts()["raw_path"]).read_text(encoding="utf-8"))
    assert raw["coverage_targets"]["bin_ids"] == ["BIN-501"]
    assert raw["coverage_targets"]["hit_keys"] == [
        "two_fetch_ftq_eligibility::request_eligibility::eligible_dual"
    ]

    with pytest.raises(ValueError, match="unknown functional coverage target Bin_ID"):
        _make_recorder(tmp_path / "bad_target", target_bin_ids=["BIN-999"])


def test_funcov_targets_resolve_from_exact_registry_testcase(tmp_path):
    recorder, _env, _dut = _make_recorder(
        tmp_path,
        target_testcases=["fe_2fetch_trained_short_blocks", "unrelated_case"],
    )

    assert recorder.coverage_targets["testcases"] == [
        "fe_2fetch_trained_short_blocks",
        "unrelated_case",
    ]
    assert recorder.coverage_targets["bin_ids"] == [
        "BIN-501",
        "BIN-506",
        "BIN-508",
        "BIN-514",
        "BIN-520",
        "BIN-525",
        "BIN-526",
        "BIN-527",
        "BIN-528",
        "BIN-530",
        "BIN-537",
        "BIN-538",
        "BIN-539",
    ]


def test_explicit_unknown_bin_stem_fails_target_resolution(tmp_path, monkeypatch):
    monkeypatch.setenv("TB_BIN_PATH", "/tmp/unknown_frontend_case.bin")
    with pytest.raises(ValueError, match="does not resolve to an active registry bin"):
        _make_recorder(tmp_path, target_testcases=["unknown_frontend_case"])


def test_explicit_unknown_testcase_cannot_hide_behind_a_known_target(tmp_path, monkeypatch):
    monkeypatch.setenv("TB_BIN_PATH", "/tmp/unknown_frontend_case.bin")
    with pytest.raises(ValueError, match="explicit testcase does not resolve"):
        _make_recorder(tmp_path, target_bin_ids=["BIN-501"], target_testcases=["fe_2fetch_trained_short_blocks"])


def test_build_manifest_binds_source_sha_to_compiled_artifacts(tmp_path):
    build_root = tmp_path / "build-frontend"
    pylib = build_root / "pylib" / "Frontend"
    rtl = build_root / "rtl"
    pylib.mkdir(parents=True)
    rtl.mkdir(parents=True)
    (pylib / "libUTFrontend.so").write_bytes(b"dut-model")
    (pylib / "_UT_Frontend.so").write_bytes(b"python-extension")
    (pylib / "Frontend_offset.yaml").write_text("signals: []\n", encoding="utf-8")
    (rtl / "Frontend.sv").write_text("module Frontend; endmodule\n", encoding="utf-8")
    manifest_path = build_root / "frontend_build_manifest.json"

    write_frontend_build_manifest(
        manifest_path,
        build_root=build_root,
        dut_source_sha="a" * 40,
        source_tree_dirty=False,
        build_config="frontend-test",
        build_command="make frontend",
    )
    valid = load_frontend_build_manifest(build_root)
    assert valid["build_manifest_status"] == "valid"
    assert valid["dut_source_sha"] == "a" * 40
    assert valid["build_config"] == "frontend-test"
    assert valid["implementation_sha"] == "a" * 40
    assert valid["design_baseline_sha"] == "a" * 40
    assert valid["source_delta_policy"] == "none"
    assert valid["source_delta_sha256"] == hashlib.sha256(b"").hexdigest()

    (pylib / "libUTFrontend.so").write_bytes(b"changed-dut-model")
    invalid = load_frontend_build_manifest(build_root)
    assert invalid["build_manifest_status"] == "invalid"
    assert invalid["dut_source_sha"] == "unavailable"
    assert "build_hash_mismatch:dut_build_sha256" in invalid["build_manifest_reasons"]


def test_build_manifest_cli_import_does_not_initialize_testbench_packages():
    repo_root = Path(__file__).resolve().parents[5]
    tool_path = repo_root / "src/test/python/Frontend/tools/write_frontend_build_manifest.py"

    result = subprocess.run(
        [
            sys.executable,
            "-S",
            "-c",
            "import runpy,sys; runpy.run_path(sys.argv[1], run_name='manifest_import_probe')",
            str(tool_path),
        ],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )

    assert result.returncode == 0, result.stderr


def test_build_manifest_runtime_rechecks_allowlisted_source_delta(tmp_path):
    repo = tmp_path / "repo"
    repo.mkdir()
    subprocess.run(["git", "-C", str(repo), "init", "-q"], check=True)
    subprocess.run(["git", "-C", str(repo), "config", "user.email", "unit@example.invalid"], check=True)
    subprocess.run(["git", "-C", str(repo), "config", "user.name", "Frontend Unit"], check=True)
    (repo / ".gitignore").write_text("/build-frontend/\n", encoding="utf-8")
    design_file = repo / "src/main/scala/xiangshan/frontend/icache/ICacheMainPipe.scala"
    design_file.parent.mkdir(parents=True)
    design_file.write_text("class MainPipe {\n  val old = true\n}\n", encoding="utf-8")
    subprocess.run(["git", "-C", str(repo), "add", "."], check=True)
    subprocess.run(["git", "-C", str(repo), "commit", "-q", "-m", "baseline"], check=True)
    baseline = subprocess.check_output(["git", "-C", str(repo), "rev-parse", "HEAD"], text=True).strip()
    design_file.write_text("class MainPipe {\n  val old = true\n  val observed = true\n}\n", encoding="utf-8")
    subprocess.run(["git", "-C", str(repo), "add", "."], check=True)
    subprocess.run(["git", "-C", str(repo), "commit", "-q", "-m", "observability"], check=True)
    implementation = subprocess.check_output(["git", "-C", str(repo), "rev-parse", "HEAD"], text=True).strip()

    build_root = repo / "build-frontend"
    pylib = build_root / "pylib" / "Frontend"
    rtl = build_root / "rtl"
    pylib.mkdir(parents=True)
    rtl.mkdir(parents=True)
    (pylib / "libUTFrontend.so").write_bytes(b"dut-model")
    (pylib / "_UT_Frontend.so").write_bytes(b"python-extension")
    (pylib / "Frontend_offset.yaml").write_text("signals: []\n", encoding="utf-8")
    (rtl / "Frontend.sv").write_text("module Frontend; endmodule\n", encoding="utf-8")
    tool_path = Path(__file__).resolve().parents[1] / "tools/write_frontend_build_manifest.py"
    result = subprocess.run(
        [
            sys.executable,
            str(tool_path),
            "--repo-root",
            str(repo),
            "--build-root",
            str(build_root),
            "--output",
            str(build_root / "frontend_build_manifest.json"),
            "--build-config",
            "unit",
            "--dut-source-sha",
            baseline,
            "--design-baseline-sha",
            baseline,
        ],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    assert result.returncode == 0, result.stderr
    loaded = load_frontend_build_manifest(build_root)
    assert loaded["build_manifest_status"] == "valid"
    assert loaded["dut_source_sha"] == baseline
    assert loaded["implementation_sha"] == implementation

    manifest_path = build_root / "frontend_build_manifest.json"
    tampered = json.loads(manifest_path.read_text(encoding="utf-8"))
    tampered["source_delta_sha256"] = "f" * 64
    manifest_path.write_text(json.dumps(tampered), encoding="utf-8")
    invalid = load_frontend_build_manifest(build_root)
    assert invalid["build_manifest_status"] == "invalid"
    assert "source_delta_hash_runtime_mismatch" in invalid["build_manifest_reasons"]


@pytest.mark.parametrize(
    ("mutation", "reason"),
    [
        ({"dut_source_sha": "not-a-git-object"}, "invalid_dut_source_sha"),
        ({"source_tree_dirty": True}, "source_tree_dirty"),
        ({"artifacts": []}, "manifest_artifacts_not_object"),
    ],
)
def test_build_manifest_rejects_malformed_provenance(tmp_path, mutation, reason):
    build_root = tmp_path / "build-frontend"
    pylib = build_root / "pylib" / "Frontend"
    rtl = build_root / "rtl"
    pylib.mkdir(parents=True)
    rtl.mkdir(parents=True)
    (pylib / "libUTFrontend.so").write_bytes(b"dut-model")
    (pylib / "_UT_Frontend.so").write_bytes(b"python-extension")
    (pylib / "Frontend_offset.yaml").write_text("signals: []\n", encoding="utf-8")
    (rtl / "Frontend.sv").write_text("module Frontend; endmodule\n", encoding="utf-8")
    manifest_path = build_root / "frontend_build_manifest.json"
    manifest = write_frontend_build_manifest(
        manifest_path,
        build_root=build_root,
        dut_source_sha="a" * 40,
        source_tree_dirty=False,
        build_config="frontend-test",
        build_command="make frontend",
    )
    manifest.update(mutation)
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

    loaded = load_frontend_build_manifest(build_root)

    assert loaded["build_manifest_status"] == "invalid"
    assert loaded["dut_source_sha"] == "unavailable"
    assert reason in loaded["build_manifest_reasons"]


def test_build_manifest_rejects_unchecked_source_sha_override(tmp_path):
    build_root = tmp_path / "build-frontend"
    pylib = build_root / "pylib" / "Frontend"
    rtl = build_root / "rtl"
    pylib.mkdir(parents=True)
    rtl.mkdir(parents=True)
    (pylib / "libUTFrontend.so").write_bytes(b"dut-model")
    (pylib / "_UT_Frontend.so").write_bytes(b"python-extension")
    (pylib / "Frontend_offset.yaml").write_text("signals: []\n", encoding="utf-8")
    (rtl / "Frontend.sv").write_text("module Frontend; endmodule\n", encoding="utf-8")
    manifest_path = build_root / "frontend_build_manifest.json"
    manifest = write_frontend_build_manifest(
        manifest_path,
        build_root=build_root,
        dut_source_sha="a" * 40,
        source_tree_dirty=False,
        build_config="frontend-test",
        build_command="make frontend",
        metadata={
            "implementation_sha": "b" * 40,
            "source_sha_override": True,
        },
    )
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

    loaded = load_frontend_build_manifest(build_root)

    assert loaded["build_manifest_status"] == "invalid"
    assert "source_delta_policy_not_allowlisted" in loaded["build_manifest_reasons"]


def test_recorder_rejects_overrides_that_disagree_with_valid_build_manifest(tmp_path, monkeypatch):
    manifest = {
        "dut_source_sha": "a" * 40,
        "dut_build_sha256": "b" * 64,
        "dut_python_extension_sha256": "c" * 64,
        "generated_rtl_sha256": "d" * 64,
        "signal_contract_sha256": "e" * 64,
        "build_config": "frontend-clean",
        "build_manifest_status": "valid",
        "build_manifest_sha256": "f" * 64,
        "build_manifest_reasons": [],
    }
    monkeypatch.setattr(
        functional_coverage_module,
        "load_frontend_build_manifest",
        lambda *_args, **_kwargs: dict(manifest),
    )
    monkeypatch.setenv("TB_DUT_SOURCE_SHA", "1" * 40)
    monkeypatch.setenv("TB_DUT_BUILD_CONFIG", "frontend-other")

    recorder, _env, _dut = _make_recorder(tmp_path / "mismatch")

    assert recorder.provenance["dut_source_sha"] == "a" * 40
    assert recorder.provenance["dut_source_origin"] == "build_manifest"
    assert recorder.provenance["build_config"] == "frontend-clean"
    assert recorder.provenance["build_manifest_status"] == "invalid"
    assert recorder.provenance["build_manifest_reasons"] == [
        "source_sha_override_mismatch",
        "build_config_override_mismatch",
    ]

    monkeypatch.setenv("TB_DUT_SOURCE_SHA", "A" * 40)
    monkeypatch.setenv("TB_DUT_BUILD_CONFIG", "frontend-clean")
    matching, _env, _dut = _make_recorder(tmp_path / "matching")
    assert matching.provenance["dut_source_sha"] == "a" * 40
    assert matching.provenance["build_manifest_status"] == "valid"
    assert matching.provenance["build_manifest_reasons"] == []


def test_backannotation_uses_explicit_targets_when_tag_does_not_match(tmp_path):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    dut_path = tmp_path / "dut.funcov.json"
    pilot_path.write_text(
        "Bin_ID,Coverage_Group,Coverpoint,Bin_Name,建议试点用例\n"
        "BIN-501,two_fetch_ftq_eligibility,request_eligibility,eligible_dual,case_a\n",
        encoding="utf-8-sig",
    )
    testpoint_path.write_text(
        "一级测试点,coverage,status,testcase,evidence\n"
        "leaf,\"covergroup two_fetch_ftq_eligibility, coverpoint request_eligibility, bins eligible_dual (BIN-501)\",MODELED,,\n",
        encoding="utf-8-sig",
    )
    dut_path.write_text(
        json.dumps(
            {
                "artifact_schema_version": 2,
                "artifact_tag": "totally_unrelated_tag",
                "coverage_targets": {
                    "bin_ids": ["BIN-501"],
                    "hit_keys": ["two_fetch_ftq_eligibility::request_eligibility::eligible_dual"],
                    "tp_ids": ["TP-001"],
                },
                **_eligible_artifact_paths("unit-explicit-targets", "case_a"),
                "provenance": _eligible_provenance(),
                "run": _eligible_run("unit-explicit-targets"),
                "stats": {"monitor": {"cycles_total": 10, "error_count": 0}},
                "errors": [],
                "hits": {
                    "two_fetch_ftq_eligibility::request_eligibility::eligible_dual": {
                        "hits": 3,
                        "first_cycle": 7,
                        "last_cycle": 9,
                        "evidence": [],
                    }
                },
            }
        ),
        encoding="utf-8",
    )

    counts = backannotate(
        testpoint_path,
        load_pilot(pilot_path),
        load_artifacts([dut_path]),
        apply=True,
    )
    assert counts["hit"] == 1
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        row = next(csv.DictReader(handle))
    assert row["status"] == "HIT"
    assert "DUT:totally_unrelated_tag:hits=3" in row["evidence"]
    assert "monitor_errors=0" in row["evidence"]


def test_explicit_bin_targets_do_not_expand_through_testcase_name():
    raw = {
        "coverage_targets": {
            "bin_ids": ["BIN-501"],
            "hit_keys": ["group::point::target"],
            "testcases": ["shared_case"],
        }
    }

    assert _target_matches(
        raw,
        "shared_case",
        PilotBin("BIN-501", "group", "point", "target", "shared_case"),
        "BIN-501",
        "group::point::target",
    )
    assert not _target_matches(
        raw,
        "shared_case",
        PilotBin("BIN-502", "group", "point", "other", "shared_case"),
        "BIN-502",
        "group::point::other",
    )


def test_backannotation_audit_reports_targets_and_gate_reasons(tmp_path):
    dut_path = tmp_path / "failed.funcov.json"
    dut_path.write_text(
        json.dumps(
            {
                "artifact_schema_version": 2,
                "artifact_tag": "audit_case",
                "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
                **_eligible_artifact_paths("unit-audit-failure", "audit_case"),
                "provenance": _eligible_provenance(),
                "run": _eligible_run(
                    "unit-audit-failure",
                    outcome="failed",
                    exit_code=1,
                    checker={
                        "status": "fail",
                        "error_count": 1,
                        "errors": [{"kind": "checker"}],
                    },
                ),
                "stats": {"monitor": {"cycles_total": 8, "error_count": 1}},
                "errors": [{"kind": "REDIRECT_TIMEOUT"}],
                "hits": {},
            }
        ),
        encoding="utf-8",
    )

    audit = build_artifact_audit(load_artifacts([dut_path]))
    assert audit[0]["artifact_tag"] == "audit_case"
    assert audit[0]["target_bin_ids"] == ["BIN-501"]
    assert audit[0]["eligible"] is False
    assert "pytest_outcome:failed" in audit[0]["reasons"]
    assert "monitor_errors:1" in audit[0]["reasons"]


def test_funcov_run_metadata_records_reproduction_identity(tmp_path, monkeypatch):
    testcase_path = tmp_path / "test_case.py"
    asm_path = tmp_path / "case.S"
    bin_path = tmp_path / "case.bin"
    trace_path = tmp_path / "case.trace.jsonl"
    testcase_path.write_text("def test_case():\n    pass\n", encoding="utf-8")
    asm_path.write_text("nop\n", encoding="utf-8")
    bin_path.write_bytes(b"frontend-bin")
    trace_path.write_text('{"pc": 2147483648}\n', encoding="utf-8")

    monkeypatch.setenv("TB_RUN_ID", "unit-reproduction-identity")
    monkeypatch.setenv("TB_RUN_COMMAND", "pytest tests/test_case.py::test_case")
    monkeypatch.setenv("TB_SEED", "0x12")
    monkeypatch.setenv("TB_BACKEND_RANDOM_SEED", "0x34")
    monkeypatch.setenv("TB_ASM_PATH", str(asm_path))
    monkeypatch.setenv("TB_BIN_PATH", str(bin_path))
    monkeypatch.setenv("TB_TRACE_PATH", str(trace_path))
    artifact_root = tmp_path / "run"
    case_log_path = artifact_root / "logs" / "case.log"
    monkeypatch.setenv("TB_ARTIFACT_DIR", str(artifact_root))

    request = SimpleNamespace(
        node=SimpleNamespace(
            nodeid="tests/test_case.py::test_case",
            path=testcase_path,
            rep_call=SimpleNamespace(outcome="passed"),
        )
    )
    env = SimpleNamespace(
        get_errors=lambda: [],
        dut=SimpleNamespace(_frontend_case_log_path=str(case_log_path)),
        config=SimpleNamespace(
            icache=SimpleNamespace(seed=0x56),
            ptw=SimpleNamespace(seed=0x78),
        ),
    )

    metadata = fixtures_module._funcov_run_metadata(request, env)
    execution = metadata["execution"]

    assert metadata["run_id"] == "unit-reproduction-identity"
    assert metadata["outcome"] == "passed"
    assert execution["testcase_path"] == str(testcase_path.resolve())
    assert execution["testcase_sha256"] == hashlib.sha256(testcase_path.read_bytes()).hexdigest()
    assert execution["asm_sha256"] == hashlib.sha256(asm_path.read_bytes()).hexdigest()
    assert execution["bin_sha256"] == hashlib.sha256(bin_path.read_bytes()).hexdigest()
    assert execution["trace_sha256"] == hashlib.sha256(trace_path.read_bytes()).hexdigest()
    assert execution["artifact_root"] == str(artifact_root.resolve())
    assert execution["case_log_path"] == str(case_log_path)
    assert execution["seed"] == 0x12
    assert execution["seeds"] == {
        "test": 0x12,
        "backend": 0x34,
        "icache": 0x56,
        "ptw": 0x78,
    }


def test_backannotation_gate_requires_reproducible_run_metadata(tmp_path):
    artifact_path = tmp_path / "missing-execution.funcov.json"
    artifact_path.write_text(
        json.dumps(
            {
                "artifact_schema_version": 2,
                "artifact_tag": "missing_execution",
                "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
                "provenance": _eligible_provenance(),
                "run": {
                    "run_id": "unit-missing-execution",
                    "pytest_outcome": "passed",
                    "exit_code": 0,
                    "checker": {"status": "pass", "error_count": 0, "errors": []},
                },
                "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
                "errors": [],
                "hits": {},
            }
        ),
        encoding="utf-8",
    )

    reasons = build_artifact_audit(load_artifacts([artifact_path]))[0]["reasons"]

    for field in (
        "testcase_nodeid",
        "testcase_path",
        "testcase_sha256",
        "run_command",
        "artifact_root",
        "case_log_path",
        "funcov_path",
        "seed",
    ):
        assert f"missing_run_metadata:{field}" in reasons
    for field in ("test", "backend", "icache", "ptw"):
        assert f"missing_run_seed:{field}" in reasons


@pytest.mark.parametrize(
    ("coverage_targets", "expected_reason"),
    [
        (None, "missing_coverage_targets"),
        ({}, "missing_coverage_targets:bin_ids"),
        ({"bin_ids": []}, "missing_coverage_targets:bin_ids"),
        ({"bin_ids": ["not-a-bin"]}, "invalid_coverage_targets:bin_ids=not-a-bin"),
    ],
)
def test_backannotation_gate_requires_explicit_bin_targets(
    tmp_path, coverage_targets, expected_reason
):
    artifact_path = tmp_path / "missing-targets.funcov.json"
    artifact = {
        "artifact_schema_version": 2,
        "artifact_tag": "case_a_test_bin_trace",
        **_eligible_artifact_paths("unit-explicit-bin-targets", "case_a"),
        "provenance": _eligible_provenance(),
        "run": _eligible_run("unit-explicit-bin-targets"),
        "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
        "errors": [],
        "hits": {},
    }
    if coverage_targets is not None:
        artifact["coverage_targets"] = coverage_targets
    artifact_path.write_text(json.dumps(artifact), encoding="utf-8")

    audit = build_artifact_audit(load_artifacts([artifact_path]))[0]

    assert audit["eligible"] is False
    assert expected_reason in audit["reasons"]


def test_backannotation_gate_requires_bin_trace_input_identity(tmp_path):
    artifact_path = tmp_path / "bin-trace.funcov.json"
    artifact = {
        "artifact_schema_version": 2,
        "testcase_name": "test_bin_trace",
        "artifact_tag": "case_test_bin_trace",
        "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
        **_eligible_artifact_paths("unit-bin-trace-inputs"),
        "provenance": _eligible_provenance(),
        "run": _eligible_run("unit-bin-trace-inputs"),
        "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
        "errors": [],
        "hits": {},
    }
    artifact_path.write_text(json.dumps(artifact), encoding="utf-8")

    audit = build_artifact_audit(load_artifacts([artifact_path]))[0]

    assert audit["eligible"] is False
    for field in ("bin_path", "bin_sha256", "trace_path", "trace_sha256"):
        assert f"missing_run_metadata:{field}" in audit["reasons"]

    artifact["run"].update(
        {
            "bin_path": str(tmp_path / "case.bin"),
            "trace_path": str(tmp_path / "case.trace.jsonl"),
        }
    )
    Path(artifact["run"]["bin_path"]).write_bytes(b"bin")
    Path(artifact["run"]["trace_path"]).write_text("{}\n", encoding="utf-8")
    artifact["run"]["bin_sha256"] = hashlib.sha256(
        Path(artifact["run"]["bin_path"]).read_bytes()
    ).hexdigest()
    artifact["run"]["trace_sha256"] = hashlib.sha256(
        Path(artifact["run"]["trace_path"]).read_bytes()
    ).hexdigest()
    artifact_path.write_text(json.dumps(artifact), encoding="utf-8")

    audit = build_artifact_audit(load_artifacts([artifact_path]))[0]
    assert audit["eligible"] is True
    assert audit["reasons"] == []


@pytest.mark.parametrize("field", ["waveform_path", "line_coverage_path", "case_log_path", "funcov_path"])
def test_backannotation_gate_rejects_artifact_outside_run(tmp_path, field):
    run_id = f"unit-outside-{field}"
    artifact = {
        "artifact_schema_version": 2,
        "artifact_tag": "outside_run",
        "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
        **_eligible_artifact_paths(run_id),
        "provenance": _eligible_provenance(),
        "run": _eligible_run(run_id),
        "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
        "errors": [],
        "hits": {},
    }
    if field in artifact:
        artifact[field] = f"/tmp/outside/{field}"
    else:
        artifact["run"][field] = f"/tmp/outside/{field}"
    artifact_path = tmp_path / f"outside-{field}.funcov.json"
    artifact_path.write_text(json.dumps(artifact), encoding="utf-8")

    audit = build_artifact_audit(load_artifacts([artifact_path]))[0]

    assert audit["eligible"] is False
    assert f"artifact_outside_run:{field}" in audit["reasons"]


@pytest.mark.parametrize("field", ["waveform_path", "line_coverage_path", "case_log_path", "funcov_path"])
def test_backannotation_gate_requires_artifact_files_to_exist(tmp_path, field):
    run_id = f"unit-missing-{field}"
    artifact = {
        "artifact_schema_version": 2,
        "artifact_tag": "missing_file",
        "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
        **_eligible_artifact_paths(run_id),
        "provenance": _eligible_provenance(),
        "run": _eligible_run(run_id),
        "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
        "errors": [],
        "hits": {},
    }
    missing_path = Path(artifact[field] if field in artifact else artifact["run"][field])
    missing_path.unlink()
    artifact_path = tmp_path / f"missing-{field}.funcov.json"
    artifact_path.write_text(json.dumps(artifact), encoding="utf-8")

    audit = build_artifact_audit(load_artifacts([artifact_path]))[0]

    assert audit["eligible"] is False
    assert f"missing_artifact_file:{field}" in audit["reasons"]


@pytest.mark.parametrize("field", ["waveform_path", "line_coverage_path", "funcov_path"])
def test_backannotation_gate_rejects_empty_evidence_files(tmp_path, field):
    run_id = f"unit-empty-{field}"
    artifact = {
        "artifact_schema_version": 2,
        "artifact_tag": "empty_file",
        "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
        **_eligible_artifact_paths(run_id),
        "provenance": _eligible_provenance(),
        "run": _eligible_run(run_id),
        "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
        "errors": [],
        "hits": {},
    }
    empty_path = Path(artifact[field] if field in artifact else artifact["run"][field])
    empty_path.write_bytes(b"")
    artifact_path = tmp_path / f"empty-{field}.funcov.json"
    artifact_path.write_text(json.dumps(artifact), encoding="utf-8")

    audit = build_artifact_audit(load_artifacts([artifact_path]))[0]

    assert audit["eligible"] is False
    assert f"empty_artifact_file:{field}" in audit["reasons"]


@pytest.mark.parametrize(
    ("mutation", "expected_reason"),
    [
        ("delete", "missing_input_file:testcase_path"),
        ("rewrite", "input_sha256_mismatch:testcase_path"),
    ],
)
def test_backannotation_gate_revalidates_testcase_identity(
    tmp_path, mutation, expected_reason
):
    run_id = f"unit-testcase-identity-{mutation}"
    artifact = {
        "artifact_schema_version": 2,
        "artifact_tag": "testcase_identity",
        "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
        **_eligible_artifact_paths(run_id),
        "provenance": _eligible_provenance(),
        "run": _eligible_run(run_id),
        "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
        "errors": [],
        "hits": {},
    }
    testcase_path = Path(artifact["run"]["testcase_path"])
    if mutation == "delete":
        testcase_path.unlink()
    else:
        testcase_path.write_text("def test_case():\n    assert False\n", encoding="utf-8")
    artifact_path = tmp_path / f"testcase-{mutation}.funcov.json"
    artifact_path.write_text(json.dumps(artifact), encoding="utf-8")

    audit = build_artifact_audit(load_artifacts([artifact_path]))[0]

    assert audit["eligible"] is False
    assert expected_reason in audit["reasons"]


@pytest.mark.parametrize(
    ("payload", "expected_reason"),
    [
        ("[]", "artifact_root_not_object"),
        ("{", "artifact_read_error:JSONDecodeError"),
    ],
)
def test_backannotation_audit_rejects_malformed_artifact_without_crashing(
    tmp_path, payload, expected_reason
):
    artifact_path = tmp_path / "malformed.funcov.json"
    artifact_path.write_text(payload, encoding="utf-8")

    audit = build_artifact_audit(load_artifacts([artifact_path]))

    assert audit[0]["kind"] == "invalid"
    assert audit[0]["eligible"] is False
    assert audit[0]["reasons"] == [expected_reason]


def test_backannotation_audit_rejects_malformed_nested_metadata(tmp_path):
    artifact_path = tmp_path / "malformed-nested.funcov.json"
    artifact_path.write_text(
        json.dumps(
            {
                "artifact_schema_version": 2,
                "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
                "provenance": [],
                "run": [],
                "checker": [],
                "errors": "malformed-error-container",
                "hits": [],
            }
        ),
        encoding="utf-8",
    )

    audit = build_artifact_audit(load_artifacts([artifact_path]))

    assert audit[0]["kind"] == "dut"
    assert audit[0]["eligible"] is False
    assert "missing_provenance:dut_source_sha" in audit[0]["reasons"]
    assert "missing_run_id" in audit[0]["reasons"]
    assert "funcov_errors:1" in audit[0]["reasons"]


def test_backannotation_rejects_stale_compatibility_signature(tmp_path):
    dut_path = tmp_path / "stale-signature.funcov.json"
    provenance = _eligible_provenance()
    provenance["toolchain"] = "python-other"
    dut_path.write_text(
        json.dumps(
            {
                "artifact_schema_version": 2,
                "artifact_tag": "stale_signature",
                "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
                **_eligible_artifact_paths("unit-stale-signature", "stale"),
                "provenance": provenance,
                "run": _eligible_run("unit-stale-signature"),
                "stats": {"monitor": {"cycles_total": 8, "error_count": 0}},
                "errors": [],
                "hits": {},
            }
        ),
        encoding="utf-8",
    )

    audit = build_artifact_audit(load_artifacts([dut_path]))

    assert audit[0]["eligible"] is False
    assert "compatibility_signature_mismatch" in audit[0]["reasons"]


@pytest.mark.parametrize(
    ("field", "expected_reason"),
    [
        ("registry_sha256", "registry_version_mismatch"),
        ("sampler_sha256", "sampler_version_mismatch"),
    ],
)
def test_backannotation_rejects_current_model_version_drift(
    tmp_path, field, expected_reason
):
    run_id = f"unit-version-drift-{field}"
    provenance = _eligible_provenance()
    provenance[field] = "0" * 64
    _resign_provenance(provenance)
    artifact = {
        "artifact_schema_version": 2,
        "artifact_tag": "version_drift",
        "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
        **_eligible_artifact_paths(run_id),
        "provenance": provenance,
        "run": _eligible_run(run_id),
        "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
        "errors": [],
        "hits": {},
    }
    artifact_path = tmp_path / f"{field}.funcov.json"
    artifact_path.write_text(json.dumps(artifact), encoding="utf-8")

    audit = build_artifact_audit(load_artifacts([artifact_path]))[0]

    assert audit["eligible"] is False
    assert expected_reason in audit["reasons"]
    assert "compatibility_signature_mismatch" not in audit["reasons"]


def test_backannotation_rejects_tampered_definitions(tmp_path):
    run_id = "unit-tampered-definitions"
    artifact = {
        "artifact_schema_version": 2,
        "artifact_tag": "tampered_definitions",
        "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
        **_eligible_artifact_paths(run_id),
        "provenance": _eligible_provenance(),
        "run": _eligible_run(run_id),
        "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
        "errors": [],
        "hits": {},
    }
    artifact["definitions"] = [{"bin_id": "BIN-501", "bin_name": "changed"}]
    artifact_path = tmp_path / "tampered-definitions.funcov.json"
    artifact_path.write_text(json.dumps(artifact), encoding="utf-8")

    audit = build_artifact_audit(load_artifacts([artifact_path]))[0]

    assert audit["eligible"] is False
    assert "definitions_sha256_mismatch" in audit["reasons"]


def test_funcov_merge_rejects_unknown_coverpoint_key(tmp_path):
    recorder, _env, _dut = _make_recorder(tmp_path)
    _install_eligible_provenance(recorder)
    recorder.run_metadata["run_id"] = "unit-unknown-coverpoint"
    raw_path = Path(recorder.write_artifacts()["raw_path"])
    raw = json.loads(raw_path.read_text(encoding="utf-8"))
    raw["hits"] = {
        "two_fetch_ftq_eligibility::wrong_point::eligible_dual": {
            "hits": 1,
            "first_cycle": 1,
            "last_cycle": 1,
            "evidence": [],
        }
    }
    raw_path.write_text(json.dumps(raw), encoding="utf-8")

    with pytest.raises(ValueError, match="unknown functional coverage hit key"):
        FunctionalCoverageRecorder.merge_raw_files(
            [raw_path],
            artifact_tag="rejected",
            output_dir=tmp_path / "rejected",
        )


def test_funcov_merge_rejects_legacy_group_bin_key(tmp_path):
    recorder, _env, _dut = _make_recorder(tmp_path)
    _install_eligible_provenance(recorder)
    recorder.run_metadata["run_id"] = "unit-legacy-hit-key"
    raw_path = Path(recorder.write_artifacts()["raw_path"])
    raw = json.loads(raw_path.read_text(encoding="utf-8"))
    raw["hits"] = {
        "two_fetch_ftq_eligibility::eligible_dual": {
            "hits": 1,
            "first_cycle": 1,
            "last_cycle": 1,
            "evidence": [],
        }
    }
    raw_path.write_text(json.dumps(raw), encoding="utf-8")

    with pytest.raises(ValueError, match="invalid functional coverage hit key"):
        FunctionalCoverageRecorder.merge_raw_files(
            [raw_path],
            artifact_tag="rejected",
            output_dir=tmp_path / "rejected",
        )


def test_backannotation_tool_distinguishes_model_dut_and_manual_close(tmp_path):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    model_path = tmp_path / "model.funcov.json"
    dut_path = tmp_path / "dut.funcov.json"

    pilot_path.write_text(
        "Bin_ID,Coverage_Group,Coverpoint,Bin_Name,建议试点用例\n"
        "BIN-501,two_fetch_ftq_eligibility,request_eligibility,eligible_dual,case_a\n",
        encoding="utf-8-sig",
    )
    testpoint_path.write_text(
        "一级测试点,coverage,status,testcase,evidence\n"
        "leaf,\"covergroup two_fetch_ftq_eligibility, coverpoint request_eligibility, bins eligible_dual (BIN-501)\",MODELED,,\n",
        encoding="utf-8-sig",
    )
    hit = {"two_fetch_ftq_eligibility::request_eligibility::eligible_dual": {"hits": 3}}
    model_path.write_text(
        json.dumps(
            {
                "artifact_tag": "case_a_unit",
                "stats": {"monitor": {"cycles_total": 0}},
                "hits": {next(iter(hit)): {"hits": 9}},
            }
        ),
        encoding="utf-8",
    )
    dut_path.write_text(
        json.dumps(
            {
                "artifact_schema_version": 2,
                "artifact_tag": "case_a_test_bin_trace",
                "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
                **_eligible_artifact_paths("unit-model-dut-close", "case_a"),
                "provenance": _eligible_provenance(),
                "run": _eligible_run("unit-model-dut-close"),
                "stats": {"monitor": {"cycles_total": 10, "error_count": 0}},
                "errors": [],
                "hits": hit,
            }
        ),
        encoding="utf-8",
    )

    pilot = load_pilot(pilot_path)
    artifacts = load_artifacts([model_path, dut_path])
    assert backannotate(testpoint_path, pilot, artifacts, apply=True)["hit"] == 1
    assert b"\r\n" not in testpoint_path.read_bytes()
    with testpoint_path.open(encoding="utf-8-sig", newline="") as f:
        row = next(csv.DictReader(f))
    assert row["status"] == "HIT"
    assert "MODEL:case_a_unit" in row["evidence"]
    assert "DUT:case_a_test_bin_trace:hits=3" in row["evidence"]

    row["status"] = "CLOSED"
    with testpoint_path.open("w", encoding="utf-8-sig", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=row.keys())
        writer.writeheader()
        writer.writerow(row)
    assert backannotate(testpoint_path, pilot, [], apply=True)["closed_preserved"] == 1
    with testpoint_path.open(encoding="utf-8-sig", newline="") as f:
        assert next(csv.DictReader(f))["status"] == "CLOSED"

    with testpoint_path.open("a", encoding="utf-8") as f:
        f.write(
            "duplicate,\"covergroup two_fetch_ftq_eligibility, coverpoint request_eligibility, bins eligible_dual (BIN-501)\",MODELED,,\n"
        )
    with pytest.raises(ValueError, match="already owned"):
        validate_mapping(testpoint_path, pilot)


def test_backannotation_keeps_stale_or_missing_dut_evidence_partial(tmp_path):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    pilot_path.write_text(
        "Bin_ID,Coverage_Group,Coverpoint,Bin_Name,建议试点用例\n"
        "BIN-501,two_fetch_ftq_eligibility,request_eligibility,eligible_dual,case_a\n",
        encoding="utf-8-sig",
    )
    testpoint_path.write_text(
        "一级测试点,coverage,status,testcase,evidence\n"
        "leaf,\"covergroup two_fetch_ftq_eligibility, coverpoint request_eligibility, "
        "bins eligible_dual (BIN-501)\",PARTIAL,case_a,OLD_DIAGNOSTIC\n",
        encoding="utf-8-sig",
    )

    counts = backannotate(
        testpoint_path,
        load_pilot(pilot_path),
        [],
        apply=True,
    )

    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        row = next(csv.DictReader(handle))
    assert row["status"] == "PARTIAL"
    assert row["evidence"] == "OLD_DIAGNOSTIC"
    assert counts["partial"] == 1
    assert counts["model"] == 0


def test_backannotation_rejects_hit_from_failed_dut_artifact(tmp_path):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    failed_path = tmp_path / "failed.funcov.json"
    pilot_path.write_text(
        "Bin_ID,Coverage_Group,Coverpoint,Bin_Name,建议试点用例\n"
        "BIN-501,two_fetch_ftq_eligibility,request_eligibility,eligible_dual,case_a\n",
        encoding="utf-8-sig",
    )
    testpoint_path.write_text(
        "一级测试点,coverage,status,testcase,evidence\n"
        "leaf,\"covergroup two_fetch_ftq_eligibility, coverpoint request_eligibility, bins eligible_dual (BIN-501)\",MODELED,,\n",
        encoding="utf-8-sig",
    )
    failed_path.write_text(
        json.dumps(
            {
                "artifact_schema_version": 2,
                "artifact_tag": "case_a_test_bin_trace",
                "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
                **_eligible_artifact_paths("unit-failed-target", "case_a"),
                "provenance": _eligible_provenance(),
                "run": _eligible_run(
                    "unit-failed-target",
                    outcome="failed",
                    exit_code=1,
                    checker={
                        "status": "fail",
                        "error_count": 1,
                        "errors": [{"kind": "checker"}],
                    },
                ),
                "stats": {"monitor": {"cycles_total": 10, "error_count": 1}},
                "errors": [{"kind": "REDIRECT_RECOVERY_TARGET_MISMATCH"}],
                "hits": {"two_fetch_ftq_eligibility::request_eligibility::eligible_dual": {"hits": 3}},
            }
        ),
        encoding="utf-8",
    )

    counts = backannotate(
        testpoint_path,
        load_pilot(pilot_path),
        load_artifacts([failed_path]),
        apply=True,
    )
    assert counts["hit"] == 0
    assert counts["failed"] == 1
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        row = next(csv.DictReader(handle))
    assert row["status"] == "PARTIAL"
    assert "DUT_REJECTED:case_a_test_bin_trace" in row["evidence"]
    assert "monitor_errors:1" in row["evidence"]


def test_backannotation_rejects_legacy_group_bin_hit_key(tmp_path):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    dut_path = tmp_path / "dut.funcov.json"
    pilot_path.write_text(
        "Bin_ID,Coverage_Group,Coverpoint,Bin_Name,建议试点用例\n"
        "BIN-501,two_fetch_ftq_eligibility,request_eligibility,eligible_dual,case_a\n",
        encoding="utf-8-sig",
    )
    testpoint_path.write_text(
        "一级测试点,coverage,status,testcase,evidence\n"
        "leaf,\"covergroup two_fetch_ftq_eligibility, coverpoint request_eligibility, bins eligible_dual (BIN-501)\",MODELED,,\n",
        encoding="utf-8-sig",
    )
    dut_path.write_text(
        json.dumps(
            {
                "artifact_schema_version": 2,
                "artifact_tag": "case_a_test_bin_trace",
                "coverage_targets": {"bin_ids": ["BIN-501"], "hit_keys": []},
                **_eligible_artifact_paths("unit-passing-target", "case_a"),
                "provenance": _eligible_provenance(),
                "run": _eligible_run("unit-passing-target"),
                "stats": {"monitor": {"cycles_total": 10, "error_count": 0}},
                "errors": [],
                "hits": {"two_fetch_ftq_eligibility::eligible_dual": {"hits": 3}},
            }
        ),
        encoding="utf-8",
    )

    counts = backannotate(
        testpoint_path,
        load_pilot(pilot_path),
        load_artifacts([dut_path]),
        apply=True,
    )
    assert counts["hit"] == 0
    assert counts["failed"] == 1
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        row = next(csv.DictReader(handle))
    assert row["status"] == "PARTIAL"
    assert "DUT_REJECTED:case_a_test_bin_trace:legacy_hit_key" in row["evidence"]
