from __future__ import annotations

from types import SimpleNamespace

from env.runtime import fixtures


class _FakeVcsDut:
    def __init__(self) -> None:
        self.reset = SimpleNamespace(value=None)
        self.clock = SimpleNamespace(value=None)
        self.waveforms: list[str] = []
        self.coverage_paths: list[str] = []
        self.finish_calls = 0

    def GetWaveFormat(self) -> str:
        return "fsdb"

    def SetWaveform(self, path: str) -> None:
        self.waveforms.append(path)

    def SetCoverage(self, path: str) -> None:
        self.coverage_paths.append(path)

    def Finish(self) -> None:
        self.finish_calls += 1


def _request(name: str):
    return SimpleNamespace(node=SimpleNamespace(name=name))


def test_vcs_batch_reuses_dut_and_finalizes_one_run_coverage(monkeypatch, tmp_path):
    dut = _FakeVcsDut()
    monkeypatch.setattr(fixtures, "_VCS_BATCH_DUT", None)
    monkeypatch.setattr(fixtures, "create_frontend_dut", lambda **_kwargs: dut)
    monkeypatch.setattr(fixtures, "is_fake_frontend_dut", lambda _dut: False)
    monkeypatch.setenv("TB_FRONTEND_SIM", "vcs")
    monkeypatch.setenv("TB_SKIP_DUT_FINISH", "1")
    monkeypatch.setenv("TB_RUN_ID", "short_vcs_suite")
    monkeypatch.setenv("TB_ARTIFACT_DIR", str(tmp_path))
    monkeypatch.setenv("TB_ENABLE_CASE_LOG", "0")

    first = fixtures.create_dut(_request("test_first"))
    second = fixtures.create_dut(_request("test_second"))

    assert first is dut
    assert second is dut
    assert dut.coverage_paths == [str(tmp_path / "short_vcs_suite_vcs_batch.dat")]
    assert dut.waveforms == [
        str(tmp_path / "test_first.fsdb"),
        str(tmp_path / "test_second.fsdb"),
    ]

    fixtures.finish_vcs_batch_dut()
    fixtures.finish_vcs_batch_dut()

    assert dut.finish_calls == 1
    assert fixtures._VCS_BATCH_DUT is None


def test_vcs_batch_finalizer_is_inactive_outside_batch_mode(monkeypatch):
    dut = _FakeVcsDut()
    monkeypatch.setattr(fixtures, "_VCS_BATCH_DUT", dut)
    monkeypatch.setenv("TB_FRONTEND_SIM", "vcs")
    monkeypatch.setenv("TB_SKIP_DUT_FINISH", "0")

    fixtures.finish_vcs_batch_dut()

    assert dut.finish_calls == 0
    assert fixtures._VCS_BATCH_DUT is dut
