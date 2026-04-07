from dataclasses import replace

import env.api as frontend_api
from env.api import (
    api_Frontend_check_pc_sequence,
    api_Frontend_enable_fst_dump,
    api_Frontend_flush_fst_dump,
    api_Frontend_get_branch_stats,
    api_Frontend_load_program,
    api_Frontend_pause_fst_dump,
    api_Frontend_set_bp_ctrl_enable,
    api_Frontend_set_log_level,
)
from env import api_Frontend_prepare_program_and_nemu_trace
from env.env_config import DEFAULT_ENV_CONFIG, SequenceConfig, TraceConfig


def test_api_load_program_writes_memory(env):
    n = api_Frontend_load_program(env, [0x00000013, 0x00100093], 0x80000000)
    assert n == 8
    assert env.memory.read_u32(0x80000000) == 0x00000013
    assert env.memory.read_u32(0x80000004) == 0x00100093


def test_api_load_program_routes_through_sequence_layer(env, monkeypatch):
    seen = {}

    def fake_run(self, target_env):
        seen["env"] = target_env
        seen["image"] = self.image
        return len(self.image.payload)

    monkeypatch.setattr("env.request_apis.LoadProgramSequence.run", fake_run)

    n = api_Frontend_load_program(env, [0x00000013], 0x80000000)

    assert n == 4
    assert seen["env"] is env
    assert seen["image"].payload == b"\x13\x00\x00\x00"
    assert seen["image"].base_addr == 0x80000000


def test_api_load_program_file_writes_memory(env, tmp_path):
    bin_path = tmp_path / "program.bin"
    bin_path.write_bytes(bytes([0x13, 0x00, 0x00, 0x00, 0x93, 0x00, 0x10, 0x00]))

    assert hasattr(frontend_api, "api_Frontend_load_program_file")
    n = frontend_api.api_Frontend_load_program_file(env, str(bin_path), 0x80000000)

    assert n == 8
    assert env.memory.read_u32(0x80000000) == 0x00000013
    assert env.memory.read_u32(0x80000004) == 0x00100093


def test_api_get_branch_stats_shape(env):
    stats = api_Frontend_get_branch_stats(env)
    assert "mpki" in stats
    assert "by_type" in stats


def test_api_check_pc_sequence_empty(env):
    assert api_Frontend_check_pc_sequence(env, []) is True


def test_api_fst_dump_controls(env, tmp_path):
    fst = tmp_path / "wave.fst"
    out = api_Frontend_enable_fst_dump(env, str(fst), max_cycles=1)
    assert out == str(fst)
    assert env.waveform_path == str(fst)
    assert api_Frontend_flush_fst_dump(env, max_cycles=1) is True
    assert api_Frontend_pause_fst_dump(env, max_cycles=1) is True


def test_api_set_log_level(env):
    value = api_Frontend_set_log_level(env, "DEBUG")
    assert isinstance(value, int)


def test_api_set_bp_ctrl_enable(env):
    cfg = api_Frontend_set_bp_ctrl_enable(
        env,
        ubtb_enable=0,
        abtb_enable=1,
        mbtb_enable=0,
        tage_enable=1,
        sc_enable=0,
        ittage_enable=1,
    )
    assert cfg == {
        "ubtb_enable": 0,
        "abtb_enable": 1,
        "mbtb_enable": 0,
        "tage_enable": 1,
        "sc_enable": 0,
        "ittage_enable": 1,
    }

    if hasattr(env.dut, "io_csrCtrl_bp_ctrl_ubtbEnable"):
        assert int(env.dut.io_csrCtrl_bp_ctrl_ubtbEnable.value) == 0
    if hasattr(env.dut, "io_csrCtrl_bp_ctrl_abtbEnable"):
        assert int(env.dut.io_csrCtrl_bp_ctrl_abtbEnable.value) == 1
    if hasattr(env.dut, "io_csrCtrl_bp_ctrl_mbtbEnable"):
        assert int(env.dut.io_csrCtrl_bp_ctrl_mbtbEnable.value) == 0
    if hasattr(env.dut, "io_csrCtrl_bp_ctrl_tageEnable"):
        assert int(env.dut.io_csrCtrl_bp_ctrl_tageEnable.value) == 1
    if hasattr(env.dut, "io_csrCtrl_bp_ctrl_scEnable"):
        assert int(env.dut.io_csrCtrl_bp_ctrl_scEnable.value) == 0
    if hasattr(env.dut, "io_csrCtrl_bp_ctrl_ittageEnable"):
        assert int(env.dut.io_csrCtrl_bp_ctrl_ittageEnable.value) == 1


def test_api_set_bp_ctrl_enable_default_all_one(env):
    cfg = api_Frontend_set_bp_ctrl_enable(env)
    assert cfg == {
        "ubtb_enable": 1,
        "abtb_enable": 1,
        "mbtb_enable": 1,
        "tage_enable": 1,
        "sc_enable": 1,
        "ittage_enable": 1,
    }


def test_package_reexports_prepare_program_and_nemu_trace():
    assert callable(api_Frontend_prepare_program_and_nemu_trace)


class _FakeRedirectBackend:
    def __init__(self):
        self.calls = []

    def inject_redirect(self, target_pc, reason, delay_cycles=0):
        self.calls.append((int(target_pc), str(reason), int(delay_cycles)))


class _FakeRedirectMonitor:
    def recent_pcs(self, limit=16):
        return [0x80000000]


class _FakeRedirectEnv:
    def __init__(self, config):
        self.config = config
        self.backend_model = _FakeRedirectBackend()
        self.monitor = _FakeRedirectMonitor()
        self.steps = 0

    def step(self, cycles=1):
        self.steps += int(cycles)
        return self.steps


class _FakeTraceEnv:
    def __init__(self, config):
        self.config = config
        self.program_load = None
        self.total_steps = 0

    def load_program_file(self, path, base_addr):
        self.program_load = (str(path), int(base_addr))
        return 4

    def step(self, cycles=1):
        self.total_steps += int(cycles)
        return self.total_steps


def test_api_inject_redirect_uses_env_config_delay():
    config = replace(
        DEFAULT_ENV_CONFIG,
        sequence=SequenceConfig(redirect_delay_cycles=9),
    )
    env = _FakeRedirectEnv(config)

    ok = frontend_api.api_Frontend_inject_redirect(env, 0x80000000, "unit", max_cycles=1)

    assert ok is True
    assert env.backend_model.calls == [(0x80000000, "unit", 9)]


def test_api_prepare_program_and_nemu_trace_uses_env_config_base(tmp_path, monkeypatch):
    config = replace(
        DEFAULT_ENV_CONFIG,
        trace=TraceConfig(nemu_image_base_addr=0x90000000),
    )
    env = _FakeTraceEnv(config)
    bin_path = tmp_path / "program.bin"
    trace_path = tmp_path / "trace.jsonl"
    bin_path.write_bytes(b"\x13\x00\x00\x00")

    monkeypatch.setattr(
        frontend_api,
        "generate_nemu_trace_from_bin",
        lambda **kwargs: {
            "trace_entries": 0,
            "nemu_exec_path": kwargs.get("nemu_exec_path") or "fake-nemu",
            "nemu_log_path": kwargs.get("nemu_log_path") or "fake.log",
            "trace_output_path": kwargs["trace_output_path"],
        },
    )

    out = frontend_api.api_Frontend_prepare_program_and_nemu_trace(
        env=env,
        bin_path=str(bin_path),
        base_addr=0x90000000,
        trace_output_path=str(trace_path),
        load_trace=0,
    )

    assert int(out["base_addr"]) == 0x90000000
    assert env.program_load == (str(bin_path), 0x90000000)
    assert env.total_steps == 1
