"""Exercise the actual baremode testcase pressure/witness sequence offline."""

from types import SimpleNamespace

import pytest

from tests.py.jiabowen import test_functional_coverage_baremode as actual


_SIGNAL_PATHS = {
    "io_toIBuffer_valid": "Frontend_top.Frontend.inner_ifu.io_toIBuffer_valid",
    "io_toIBuffer_ready": "inner_ifu.io_toIBuffer_ready",
    "io_toIBuffer_bits_enqEnable": "inner_ifu.io_toIBuffer_bits_enqEnable",
    "io_toIBuffer_bits_valid": "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_valid",
    "s2_useUncacheFetch": "Frontend_top.Frontend.inner_ifu.s2_useUncacheFetch",
    "io_toIBuffer_bits_exceptionMask": (
        "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_exceptionMask"
    ),
    "s2_alignedInstrPcVec_0_addr": (
        "Frontend_top.Frontend.inner_ifu.s2_alignedInstrPcVec_0_addr"
    ),
    "s2_alignedInstrPcVec_1_addr": (
        "Frontend_top.Frontend.inner_ifu.s2_alignedInstrPcVec_1_addr"
    ),
    "io_toIBuffer_bits_instrs_0": "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_instrs_0",
    "io_toIBuffer_bits_instrs_1": "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_instrs_1",
    "io_toIBuffer_bits_isRvc_0": "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_isRvc_0",
    "io_toIBuffer_bits_isRvc_1": "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_isRvc_1",
}
_PATH_TO_STEM = {path: stem for stem, path in _SIGNAL_PATHS.items()}


class _Backend:
    def __init__(self, env):
        self.env = env
        self.can_accept = 1
        self.transitions = []
    def get_stats(self):
        return {"can_accept": self.can_accept,
                "commit_count": int(self.env.premature_commit and self.env.current_cycle >= 100)}
    def set_can_accept(self, value):
        self.can_accept = int(value)
        self.transitions.append((self.env.current_cycle, self.can_accept))


class _Coverage:
    def __init__(self, env):
        self.env = env
    def key_hit(self, group, bin_name):
        self.env.checked_bins.append((group, bin_name))
        return True
    def _try_read_dut_signal(self, dut, path):
        del dut
        env = self.env
        if path == "Frontend_top.io_backend_toIBuf_decodeCanAccept":
            return 1 if env.bad_accept and env.current_cycle == 5 else env.native_accept
        field = _PATH_TO_STEM.get(path)
        if field is None:
            return None
        signals = {
            "io_toIBuffer_valid": int(
                env.native_accept == 0 and env.current_cycle >= env.payload_cycle
            ),
            "io_toIBuffer_ready": 1,
            "io_toIBuffer_bits_enqEnable": 3,
            "io_toIBuffer_bits_valid": 3,
            "s2_useUncacheFetch": 0,
            "io_toIBuffer_bits_exceptionMask": 0,
            "s2_alignedInstrPcVec_0_addr": actual._BASE >> 1,
            "s2_alignedInstrPcVec_1_addr": (actual._BASE + 2) >> 1,
            "io_toIBuffer_bits_instrs_0": 0x13,
            "io_toIBuffer_bits_instrs_1": 0x13,
            "io_toIBuffer_bits_isRvc_0": 1,
            "io_toIBuffer_bits_isRvc_1": 1,
        }
        if field in env.overrides:
            return env.overrides[field]
        return signals.get(field)


class _Env:
    def __init__(self, payload_cycle=200, native_accept_delay=1):
        self.current_cycle = 0
        self.payload_cycle = payload_cycle
        self.native_accept_delay = native_accept_delay
        self.native_accept = 1
        self.drive_started_cycle = None
        self.premature_commit = self.bad_accept = False
        self.overrides = {}
        self.dut = SimpleNamespace(
            io_backend_toIBuf_decodeCanAccept=SimpleNamespace(value=1)
        )
        self.backend_model = _Backend(self)
        self.functional_coverage = _Coverage(self)
        self.monitor = SimpleNamespace(get_errors=lambda: [])
        self.events = []
        self.checked_bins = []
        self.replayed = False
    def initialize(self, **kwargs):
        assert kwargs == {"reset_vector": actual._BASE, "bare_mode": True, "reset_cycles": 20}
    def step(self, cycles):
        assert cycles == 1
        self.current_cycle += cycles
        if self.backend_model.can_accept == 0:
            if self.dut.io_backend_toIBuf_decodeCanAccept.value == 1:
                self.dut.io_backend_toIBuf_decodeCanAccept.value = 0
                self.drive_started_cycle = self.current_cycle
            if (
                self.drive_started_cycle is not None
                and self.current_cycle - self.drive_started_cycle
                >= self.native_accept_delay
            ):
                self.native_accept = 0
    def _emit_event(self, name, payload):
        self.events.append((name, payload))


def _invoke(monkeypatch, env):
    class Load:
        def __init__(self, *, image, step_cycles):
            assert image.payload[:4] == b"\x01\x00\x01\x00" and step_cycles == 0
        def run(self, target):
            assert target is env
    def replay(target, *, target_count, max_cycles):
        assert target is env and target_count == 14 and max_cycles == 5000
        assert env.backend_model.can_accept == 1
        assert env.current_cycle >= env.payload_cycle + 32
        env.replayed = True
        return 14
    monkeypatch.setattr(actual, "LoadProgramSequence", Load)
    monkeypatch.setattr(actual, "_warmup_commits", replay)
    monkeypatch.delenv("TB_FUNCOV_SAMPLER_DOMAINS", raising=False)
    actual.test_baremode_cacheable_stream_delivery_pilot(env)


@pytest.mark.parametrize("payload_cycle", [2, 200, 568])
def test_actual_pressure_waits_for_physical_payload_and_keeps_all_assertions(monkeypatch, payload_cycle):
    env = _Env(payload_cycle)
    _invoke(monkeypatch, env)
    assert env.backend_model.transitions == [(0, 0), (payload_cycle + 32, 1)]
    assert env.replayed and len(env.checked_bins) == 7
    event, proof = env.events[-1]
    assert event == "ifu.cacheable_stream_loaded_backpressure"
    assert proof["source"]["cycle"] == payload_cycle
    assert proof["loaded_hold_cycles"] == 32
    assert proof["release_cycle"] - proof["source"]["cycle"] == 32
    assert proof["commits_before_release"] == 0
    assert proof["pressure_effective_cycle"] == 2
    assert proof["pending_drive_cycles"] == 1
    assert proof["source"]["signal_paths"] == [
        _SIGNAL_PATHS["io_toIBuffer_bits_enqEnable"],
        _SIGNAL_PATHS["io_toIBuffer_bits_valid"],
        _SIGNAL_PATHS["io_toIBuffer_bits_exceptionMask"],
    ]
    assert [entry["signal_paths"] for entry in proof["source"]["payload"]] == [
        [
            _SIGNAL_PATHS["s2_alignedInstrPcVec_0_addr"],
            _SIGNAL_PATHS["io_toIBuffer_bits_instrs_0"],
            _SIGNAL_PATHS["io_toIBuffer_bits_isRvc_0"],
        ],
        [
            _SIGNAL_PATHS["s2_alignedInstrPcVec_1_addr"],
            _SIGNAL_PATHS["io_toIBuffer_bits_instrs_1"],
            _SIGNAL_PATHS["io_toIBuffer_bits_isRvc_1"],
        ],
    ]


@pytest.mark.parametrize("failure", [
    "never_loaded", "premature_commit", "bad_accept", "bad_pc", "bad_data",
    "missing_data", "active_exception", "uncache", "enq_outside_valid",
    "stuck_pending_drive",
])
def test_actual_pressure_sequence_fails_closed_without_releasing_backend(monkeypatch, failure):
    env = _Env()
    if failure == "never_loaded":
        env.payload_cycle = 10000
    elif failure == "premature_commit":
        env.premature_commit = True
    elif failure == "bad_accept":
        env.bad_accept = True
    elif failure == "stuck_pending_drive":
        env.native_accept_delay = 10
    else:
        field, value = {
            "bad_pc": ("s2_alignedInstrPcVec_0_addr", 0),
            "bad_data": ("io_toIBuffer_bits_instrs_0", 0),
            "missing_data": ("io_toIBuffer_bits_instrs_0", None),
            "active_exception": ("io_toIBuffer_bits_exceptionMask", 1),
            "uncache": ("s2_useUncacheFetch", 1),
            "enq_outside_valid": ("io_toIBuffer_bits_valid", 1),
        }[failure]
        env.overrides[field] = value
    with pytest.raises(AssertionError):
        _invoke(monkeypatch, env)
    assert env.backend_model.transitions == [(0, 0)]
    assert not env.replayed
