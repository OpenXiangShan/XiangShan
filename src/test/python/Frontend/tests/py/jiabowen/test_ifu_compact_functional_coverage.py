from pathlib import Path
from types import SimpleNamespace

from env.funcov.py.ifu.sampler import sample_cfvec_coverage
from env.funcov.recorder import FunctionalCoverageRecorder, default_pilot_csv_path
from env.support.rvc_decoder import expand_rvc


_PREFIX = "Frontend_top.Frontend.inner_ifu.__Vtogcov__"


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


class _Memory:
    def __init__(self):
        self._bytes = {}

    def write16(self, address, value):
        self._write(address, value, 2)

    def write32(self, address, value):
        self._write(address, value, 4)

    def _write(self, address, value, width):
        for offset in range(int(width)):
            self._bytes[int(address) + offset] = (int(value) >> (8 * offset)) & 0xFF

    def read_u8(self, address):
        return self._bytes.get(int(address), 0)

    @staticmethod
    def is_mmio(_address):
        return False


def _make_recorder(tmp_path):
    dut = _FakeDut()
    memory = _Memory()
    env = SimpleNamespace(dut=dut, memory=memory)
    recorder = FunctionalCoverageRecorder.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="ifu_compact_unit",
        artifact_tag="ifu_compact_unit",
        output_dir=tmp_path,
    )
    recorder.attach(env)
    return recorder, env, dut, memory


def _set_ifu_output(dut, entries, *, exception_type=0, valid_mask_extra=0):
    enq_enable = 0
    valid_mask = 0
    dut.set(_PREFIX + "io_toIBuffer_ready", 1)
    dut.set(_PREFIX + "io_toIBuffer_valid", 1)
    dut.set(_PREFIX + "io_toIBuffer_bits_exceptionType_value", exception_type)
    for slot, pc, instr, is_rvc, end_offset, ftq_flag, ftq_value, exception_mask in entries:
        slot = int(slot)
        enq_enable |= 1 << slot
        valid_mask |= 1 << slot
        # IFU's PrunedAddr omits bit 0, matching the generated DUT signal.
        dut.set(_PREFIX + f"io_toIBuffer_bits_pc_{slot}_addr", int(pc) >> 1)
        dut.set(_PREFIX + f"io_toIBuffer_bits_instrs_{slot}", instr)
        dut.set(_PREFIX + f"io_toIBuffer_bits_isRvc_{slot}", is_rvc)
        dut.set(_PREFIX + f"io_toIBuffer_bits_instrEndOffset_{slot}_offset", end_offset)
        dut.set(_PREFIX + f"io_toIBuffer_bits_instrEndOffset_{slot}_predTaken", slot == entries[-1][0])
        dut.set(_PREFIX + f"io_toIBuffer_bits_instrEndOffset_{slot}_fixedTaken", 0)
        dut.set(_PREFIX + f"io_toIBuffer_bits_isLastInFtqEntry_{slot}", slot == entries[-1][0])
        dut.set(_PREFIX + f"io_toIBuffer_bits_ftqPtr_{slot}_flag", ftq_flag)
        dut.set(_PREFIX + f"io_toIBuffer_bits_ftqPtr_{slot}_value", ftq_value)
        dut.set(_PREFIX + f"io_toIBuffer_bits_exceptionMask_{slot}", exception_mask)
    dut.set(_PREFIX + "io_toIBuffer_bits_enqEnable", enq_enable)
    dut.set(_PREFIX + "io_toIBuffer_bits_valid", valid_mask | int(valid_mask_extra))


def test_ifu_compact_and_expander_bins_use_to_ibuffer_fire(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path)
    base = 0x80000000
    compressed = 0x0001
    memory.write16(base, compressed)
    memory.write16(base + 2, compressed)
    memory.write32(base + 4, 0x00000013)
    memory.write32(base + 8, 0x00000013)
    expanded = expand_rvc(compressed)
    _set_ifu_output(
        dut,
        [
            (0, base, expanded, 1, 0, 0, 1, 0),
            (1, base + 2, expanded, 1, 1, 0, 1, 0),
            (2, base + 4, 0x00000013, 0, 3, 0, 1, 0),
            (3, base + 8, 0x00000013, 0, 5, 0, 1, 0),
        ],
    )

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_instr_compact", "contiguous_slots")
    assert recorder.key_hit("ifu_instr_compact", "rvi_single_slot")
    assert recorder.key_hit("ifu_instr_compact", "rvc_multi_slot")
    assert recorder.key_hit("ifu_instr_end_offset", "rvc_rvi_end_offset")
    assert recorder.key_hit("ifu_rvc_expander", "legal_rvc_expanded")
    assert recorder.key_hit("ifu_rvc_expander", "rvi_passthrough")
    assert recorder.key_hit("ifu_ibuffer_output", "instr_pc_isrvc_observed")
    assert recorder.key_hit("ifu_ibuffer_output", "ftq_offset_observed")
    assert recorder.key_hit("ifu_ibuffer_output", "last_in_ftq_entry")
    assert recorder.key_hit("ifu_ibuffer_output", "taken_end_metadata")


def test_ifu_ibuffer_output_range_clipping_requires_valid_slot_not_enabled(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path)
    base = 0x80000000
    memory.write32(base, 0x00000013)
    _set_ifu_output(
        dut,
        [(0, base, 0x00000013, 0, 1, 0, 1, 0)],
        valid_mask_extra=1 << 1,
    )

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_ibuffer_output", "fixed_range_clipped")


def test_ifu_compact_two_fetch_source_requires_expected_ftq_order(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    recorder._two_fetch_expected_cfvec = {"tags": ((0, 3), (0, 4)), "cycle": 1}
    _set_ifu_output(
        dut,
        [
            (0, 0x80000000, 0x00000013, 0, 1, 0, 3, 0),
            (1, 0x80000004, 0x00000013, 0, 3, 0, 4, 0),
        ],
    )

    sample_cfvec_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_instr_compact_source", "two_fetch_select_block")


def test_ifu_illegal_rvc_and_fetch_exception_priority_bins(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path / "illegal")
    base = 0x80000000
    memory.write16(base, 0x0000)
    _set_ifu_output(dut, [(0, base, 0x00000000, 1, 0, 0, 1, 1)], exception_type=4)

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_rvc_exception", "illegal_rvc")
    assert not recorder.key_hit("ifu_rvc_exception", "fetch_exception_over_illegal_rvc")

    priority, priority_env, priority_dut, priority_memory = _make_recorder(tmp_path / "fetch_priority")
    priority_memory.write16(base, 0x0000)
    _set_ifu_output(
        priority_dut,
        [(0, base, 0x00000000, 1, 0, 0, 1, 1)],
        exception_type=1,
    )

    sample_cfvec_coverage(priority, priority_env, 1)

    assert priority.key_hit("ifu_rvc_exception", "fetch_exception_over_illegal_rvc")
    assert not priority.key_hit("ifu_rvc_exception", "illegal_rvc")


def test_invalid_taken_fetch_exception_cross_is_stimulus_coverage(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    dut.set(_PREFIX + "s1_valid", 1)
    dut.set(_PREFIX + "s1_invalidTaken_0", 1)
    dut.set(_PREFIX + "s1_icacheMeta_0_exception_value", 0)
    dut.set(_PREFIX + "s1_instrCount", 1)
    dut.set(_PREFIX + "s1_flush", 0)

    sample_cfvec_coverage(recorder, env, 1)
    assert not recorder.key_hit("ifu_invalid_taken_exception", "observed")

    dut.set(_PREFIX + "s1_icacheMeta_0_exception_value", 3)
    # Coverage records the stimulus even if a pre-fix DUT computes the wrong
    # count. A checker must independently require instrCount == 1.
    dut.set(_PREFIX + "s1_instrCount", 4)
    sample_cfvec_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_invalid_taken_exception", "observed")


def test_ifu_compact_sampler_signals_are_present_in_generated_contract():
    root = Path(__file__).resolve().parents[7]
    offset = root / "build-frontend/pylib-verilator/Frontend/Frontend_offset.yaml"
    names = {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }
    required = {
        _PREFIX + "io_toIBuffer_ready",
        _PREFIX + "io_toIBuffer_valid",
        _PREFIX + "io_toIBuffer_bits_enqEnable",
        _PREFIX + "io_toIBuffer_bits_valid",
        _PREFIX + "io_toIBuffer_bits_pc_0_addr",
        _PREFIX + "io_toIBuffer_bits_instrs_0",
        _PREFIX + "io_toIBuffer_bits_isRvc_0",
        _PREFIX + "io_toIBuffer_bits_instrEndOffset_0_offset",
        _PREFIX + "io_toIBuffer_bits_instrEndOffset_0_predTaken",
        _PREFIX + "io_toIBuffer_bits_instrEndOffset_0_fixedTaken",
        _PREFIX + "io_toIBuffer_bits_isLastInFtqEntry_0",
        _PREFIX + "io_toIBuffer_bits_exceptionType_value",
        _PREFIX + "io_toIBuffer_bits_exceptionMask_0",
        _PREFIX + "io_toIBuffer_bits_ftqPtr_0_flag",
        _PREFIX + "io_toIBuffer_bits_ftqPtr_0_value",
        _PREFIX + "s1_valid",
        _PREFIX + "s1_invalidTaken_0",
        _PREFIX + "s1_icacheMeta_0_exception_value",
        _PREFIX + "s1_instrCount",
        _PREFIX + "s1_flush",
    }
    assert required <= names
