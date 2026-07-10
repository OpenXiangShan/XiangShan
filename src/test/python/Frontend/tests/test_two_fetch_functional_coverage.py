import csv
import re
from pathlib import Path
from types import SimpleNamespace

from env import coverage_def as coverage_def_module
from env.funcov import _TWO_FETCH_SIGNALS, sample_cfvec_coverage, sample_two_fetch_coverage
from env.functional_coverage import FunctionalCoverageRecorder, default_pilot_csv_path


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


def _make_recorder(tmp_path):
    dut = _FakeDut()
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
    )
    recorder.attach(env)
    return recorder, env, dut


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


def test_two_fetch_waylookup_ifu_and_delivery_bins(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("way_req1_valid", 1)
    dut.set_key("way_out_valid", 1)
    dut.set_key("way_out_ready", 1)
    dut.set_key("way_real_two", 1)
    dut.set_key("ifu_valid", 1)
    dut.set_key("ifu_ready", 1)
    dut.set_key("ifu_req1_valid", 1)
    dut.set_key("ifu_req0_size", 8)
    dut.set("Frontend_top.Frontend.inner_ifu.instrBoundary.io_resp_rawInstrVec_8_blockSel", 1)
    dut.set_key("ifu_second_valid", 1)
    dut.set_key("to_ibuffer_valid", 1)
    dut.set_key("to_ibuffer_ready", 1)

    sample_two_fetch_coverage(recorder, env, 3)

    assert recorder.key_hit("two_fetch_waylookup_result", "dual_served")
    assert recorder.key_hit("two_fetch_ifu_window", "dual_window")
    assert recorder.key_hit("two_fetch_ifu_source", "blocksel_switch")
    assert recorder.key_hit("two_fetch_delivery", "dual_fire")

    dut.set_key("way_real_two", 0)
    dut.set_key("way_num_valid", 2)
    dut.set_key("way_read_ptr_flag", 0)
    dut.set_key("way_read_ptr_value", 0)
    dut.set_key("way_exception_valid", 0)
    dut.set_key("way_exception_ptr_flag", 0)
    dut.set_key("way_exception_ptr_value", 0)
    for index in range(64):
        suffix = "" if index == 0 else f"_{index}"
        dut.set(f"Frontend_top.Frontend.inner_icache.wayLookup.entryUpdate_updated{suffix}", 0)
    for index in range(32):
        dut.set(f"Frontend_top.Frontend.inner_icache.wayLookup.entries_{index}_isMmio", 0)

    sample_two_fetch_coverage(recorder, env, 4)
    assert recorder.key_hit("two_fetch_waylookup_block_reason", "data_bank_conflict")


def test_two_fetch_mainpipe_refill_completion_bin(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set_key("main_s1_valid", 1)
    dut.set_key("main_req1_valid", 1)
    dut.set_key("ifu_valid", 0)
    dut.set_key("ifu_ready", 0)
    dut.set_key("ifu_req1_valid", 0)
    for index, value in enumerate((0, 0, 1, 0)):
        dut.set(f"Frontend_top.Frontend.inner_icache.mainPipe.s1_shouldFetch_{index}", value)

    sample_two_fetch_coverage(recorder, env, 4)
    assert recorder.key_hit("two_fetch_mainpipe_hit_pattern", "hit_miss")

    dut.set_key("main_s1_valid", 0)
    dut.set_key("ifu_valid", 1)
    dut.set_key("ifu_ready", 1)
    dut.set_key("ifu_req1_valid", 1)
    sample_two_fetch_coverage(recorder, env, 5)

    assert recorder.key_hit("two_fetch_mainpipe_completion", "wait_refill_then_dual")


def test_two_fetch_checker_second_invalid_taken_bin(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    recorder._two_fetch_last_dual_cycle = 6
    dut.set_key("checker_valid", 1)
    dut.set_key("checker_select", 1)
    dut.set_key("checker_invalid", 1)

    sample_two_fetch_coverage(recorder, env, 7)

    assert recorder.key_hit("two_fetch_checker_redirect", "second_block")
    assert recorder.key_hit("two_fetch_checker_priority", "second_after_first_valid")
    assert recorder.key_hit("two_fetch_invalid_taken", "second_redirect")


def test_two_fetch_backend_two_ftq_source_and_mixed_bins(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    recorder._two_fetch_last_dual_cycle = 8
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


def test_two_fetch_signal_map_matches_current_frontend_offset():
    offset = Path(__file__).resolve().parents[5] / "build-frontend/pylib/Frontend/Frontend_offset.yaml"
    if not offset.exists():
        return

    registered = {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }
    assert all(any(signal in registered for signal in candidates) for candidates in _TWO_FETCH_SIGNALS.values())

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
        *(
            f"Frontend_top.Frontend.inner_ifu.instrBoundary.io_resp_rawInstrVec_{index}_{field}"
            for field in ("blockSel", "isCrossBlockInstr")
            for index in range(31)
        ),
    ]
    assert not [signal for signal in generated if signal not in registered]


def test_two_fetch_backannotation_matches_pilot_and_covergroups():
    class _CovGroup:
        def __init__(self, name):
            self.name = str(name)
            self.points = {}

        def add_watch_point(self, _dut, bins, name):
            self.points[str(name)] = set(bins)

    previous_fc = coverage_def_module.fc
    coverage_def_module.fc = SimpleNamespace(CovGroup=_CovGroup)
    try:
        groups = coverage_def_module.get_coverage_groups(_FakeDut())
    finally:
        coverage_def_module.fc = previous_fc

    model = {
        (group.name, point, bin_name)
        for group in groups
        for point, bins in group.points.items()
        for bin_name in bins
    }
    repo_root = Path(__file__).resolve().parents[5]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_功能覆盖率建模/frontend_bt_functional_coverage_pilot.csv"
    )
    with pilot_path.open(encoding="utf-8-sig", newline="") as f:
        pilot_rows = [row for row in csv.DictReader(f) if row["Bin_ID"].startswith("BIN-5")]
    assert len(pilot_rows) == 41
    assert not [
        row["Bin_ID"]
        for row in pilot_rows
        if not any(
            group == row["Coverage_Group"] and bin_name == row["Bin_Name"]
            for group, _point, bin_name in model
        )
    ]

    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_测试点分解/Frontend_testpoint_0525_coverage_backannotated.csv"
    )
    reference_re = re.compile(
        r"covergroup ([^,;]+), coverpoint ([^,;]+), bins ([^ (;]+) \((BIN-\d+)\)"
    )
    references = []
    with testpoint_path.open(encoding="utf-8-sig", newline="") as f:
        for row in csv.reader(f):
            if len(row) < 9:
                continue
            references.extend(reference_re.findall(row[8]))

    two_fetch_references = [ref for ref in references if ref[0].startswith("two_")]
    assert two_fetch_references
    assert not [
        ref
        for ref in two_fetch_references
        if (ref[0], ref[1], ref[2]) not in model
        or not any(
            pilot["Bin_ID"] == ref[3]
            and pilot["Coverage_Group"] == ref[0]
            and pilot["Bin_Name"] == ref[2]
            for pilot in pilot_rows
        )
    ]
