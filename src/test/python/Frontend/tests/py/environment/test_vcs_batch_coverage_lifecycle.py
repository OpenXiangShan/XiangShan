from __future__ import annotations

import sys
import types
from types import SimpleNamespace

from env.runtime import fixtures
from env.runtime import dut_factory


class _FakeVcsDut:
    def __init__(self) -> None:
        self.reset = SimpleNamespace(value=None)
        self.clock = SimpleNamespace(value=None)
        self.waveforms: list[str] = []
        self.coverage_paths: list[str] = []
        self.finish_calls = 0

    def GetWaveFormat(self) -> str:
        return ""

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
    factory_kwargs = []
    monkeypatch.setattr(fixtures, "_VCS_BATCH_DUT", None)
    monkeypatch.setattr(
        fixtures,
        "create_frontend_dut",
        lambda **kwargs: (factory_kwargs.append(kwargs), dut)[1],
    )
    monkeypatch.setattr(fixtures, "is_fake_frontend_dut", lambda _dut: False)
    monkeypatch.setenv("TB_FRONTEND_SIM", "vcs")
    monkeypatch.setenv("TB_SKIP_DUT_FINISH", "1")
    monkeypatch.setenv("TB_RUN_ID", "short_vcs_suite")
    monkeypatch.setenv("TB_ARTIFACT_DIR", str(tmp_path))
    monkeypatch.setenv("TB_ENABLE_CASE_LOG", "0")
    source_vdb = tmp_path / "pylib-vcs" / "Frontend" / "Frontend.vdb"
    source_vdb.mkdir(parents=True)
    monkeypatch.setattr(fixtures, "frontend_pylib_path", lambda: tmp_path / "pylib-vcs")

    first = fixtures.create_dut(_request("test_first"))
    second = fixtures.create_dut(_request("test_second"))

    assert first is dut
    assert second is dut
    assert dut.coverage_paths == [str(tmp_path / "short_vcs_suite_vcs_batch.dat")]
    assert dut.waveforms == [
        str(tmp_path / "test_first.fsdb"),
        str(tmp_path / "test_second.fsdb"),
    ]
    assert factory_kwargs[0]["vcs_coverage_vdb"] == str(tmp_path / "Frontend.vdb")
    assert factory_kwargs[0]["vcs_coverage_name"] == "short_vcs_suite"

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


def test_vcs_factory_injects_run_local_coverage_arguments(monkeypatch):
    captured = {}

    class _FrontendDut:
        def __init__(self, *args):
            captured["args"] = args

    monkeypatch.setitem(sys.modules, "Frontend", types.SimpleNamespace(DUTFrontend=_FrontendDut))
    monkeypatch.setenv("TB_FRONTEND_SIM", "vcs")

    dut_factory.create_frontend_dut(
        vcs_coverage_vdb="/tmp/frontend-run/Frontend.vdb",
        vcs_coverage_name="frontend_run",
    )

    assert captured["args"] == (
        [
            "Frontend",
            "-cm_dir",
            "/tmp/frontend-run/Frontend.vdb",
            "-cm_name",
            "frontend_run",
        ],
    )
