from types import SimpleNamespace

from env.funcov.py.ifu.mmio_v3_funcov import sample_mmio_v3_coverage
from env.funcov.recorder import FunctionalCoverageRecorder, default_pilot_csv_path


_IFU = "Frontend_top.Frontend.inner_ifu."


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


def _make_recorder(tmp_path):
    dut = _FakeDut()
    env = SimpleNamespace(dut=dut)
    recorder = FunctionalCoverageRecorder.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="ifu_mmio_v3_unit",
        artifact_tag="ifu_mmio_v3_unit",
        output_dir=tmp_path,
    )
    recorder.attach(env)
    return recorder, env, dut


def _set_common_signals(dut):
    dut.set("auto_inner_instrUncache_client_out_a_valid", 0)
    dut.set("auto_inner_instrUncache_client_out_a_ready", 1)
    dut.set("auto_inner_instrUncache_client_out_a_bits_address", 0)
    dut.set("Frontend_top.io_backend_toFtq_redirect_valid", 0)
    dut.set(_IFU + "s2_flush", 0)
    dut.set(_IFU + "s2_fetchBlock_0_startVAddr_addr", 0)
    dut.set(_IFU + "s2_icacheMeta_0_pmpMmio", 1)
    dut.set(_IFU + "s2_icacheMeta_0_itlbPbmt", 0)
    dut.set(_IFU + "s2_icacheMeta_0_exception_value", 0)


def test_tl_a_stall_requires_full_context_stability(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_common_signals(dut)
    dut.set("auto_inner_instrUncache_client_out_a_valid", 1)
    dut.set("auto_inner_instrUncache_client_out_a_ready", 0)
    dut.set("auto_inner_instrUncache_client_out_a_bits_address", 0x80001000)
    dut.set(_IFU + "s2_fetchBlock_0_startVAddr_addr", 0x40000800)
    dut.set(_IFU + "s2_icacheMeta_0_itlbPbmt", 2)

    sample_mmio_v3_coverage(recorder, env, 1)
    assert not recorder.key_hit("ifu_mmio_tl_a_stall", "stable_until_accept")

    dut.set(_IFU + "s2_icacheMeta_0_itlbPbmt", 1)
    sample_mmio_v3_coverage(recorder, env, 2)
    assert not recorder.key_hit("ifu_mmio_tl_a_stall", "stable_until_accept")
    assert any(
        item.get("event") == "mmio_tl_a_stall_context_changed"
        for item in recorder.risk_observations
    )

    sample_mmio_v3_coverage(recorder, env, 3)
    sample_mmio_v3_coverage(recorder, env, 4)
    assert recorder.key_hit("ifu_mmio_tl_a_stall", "stable_until_accept")
