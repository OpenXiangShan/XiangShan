from __future__ import annotations

import ast
from copy import deepcopy
from dataclasses import dataclass
from pathlib import Path

import pytest
from toffee.funcov import CovGroup
from env.funcov.recorder import default_pilot_csv_path
from env.funcov.toffee_bridge import ToffeeCoverageSink
from env.funcov.recorder import FunctionalCoverageRecorder
from env.funcov.sample_hub import FrontendFuncovSampleHub
from env.funcov.py.icache.icache_hitmiss_funcov import (
    ICACHE_HITMISS_COVERPOINTS,
    ICACHE_HITMISS_SAMPLER_BIN_KEYS,
)
from env.funcov.py.icache.icache_mainpipe_funcov import (
    ICACHE_MAINPIPE_COVERPOINTS,
    ICACHE_MAINPIPE_SAMPLER_BIN_KEYS,
)
from env.funcov.py.icache.icache_mainpipe_toffee import ICacheMainpipeToffeeCoverage
from env.funcov.py.icache.icache_prefetchpipe_funcov import (
    ICACHE_PREFETCHPIPE_COVERPOINTS,
    ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS,
)
from env.funcov.py.icache.icache_prefetchpipe_toffee import ICachePrefetchpipeToffeeCoverage
from env.funcov.py.icache.icache_missunit_funcov import (
    ICACHE_MISSUNIT_COVERPOINTS,
    ICACHE_MISSUNIT_SAMPLER_BIN_KEYS,
)
from env.funcov.py.icache.icache_missunit_toffee import ICacheMissunitToffeeCoverage
from env.funcov.py.icache.icache_waylookup_funcov import (
    ICACHE_WAYLOOKUP_COVERPOINTS,
    ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS,
)
from env.funcov.py.icache.icache_waylookup_toffee import ICacheWaylookupToffeeCoverage
from env.funcov.py.ftq.two_fetch_funcov import (
    TWO_FETCH_COVERPOINTS,
    TWO_FETCH_SAMPLER_BIN_KEYS,
)
from env.funcov.py.ftq.two_fetch_toffee import TwoFetchToffeeCoverage
from env.funcov.py.ifu.mmio_v3_funcov import (
    MMIO_V3_COVERPOINTS,
    MMIO_V3_SAMPLER_BIN_KEYS,
)
from env.funcov.py.ifu.cacheable_pipeline_funcov import (
    IFU_CACHEABLE_PIPELINE_COVERPOINTS,
    IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS,
)
from env.funcov.py.ifu.cfvec_toffee import CFVEC_COVERPOINTS
from env.funcov.py.ifu.cfvec_funcov import CFVEC_SAMPLER_BIN_KEYS
from env.funcov.py.ifu.owner_v3_funcov import (
    OWNER_V3_COVERPOINTS,
    OWNER_V3_SAMPLER_BIN_KEYS,
)
from env.funcov.py.ifu.mmio_nc_owner_funcov import (
    MMIO_NC_OWNER_COVERPOINTS,
    MMIO_NC_OWNER_SAMPLER_BIN_KEYS,
)
from env.funcov.py.ifu.instr_uncache_owner_funcov import (
    INSTR_UNCACHE_OWNER_COVERPOINTS,
    INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS,
)
from env.funcov.py.ifu.mmio_v3_toffee import MmioV3ToffeeCoverage
from env.funcov.py.ifu.mmio_nc_owner_toffee import MmioNcOwnerToffeeCoverage
from env.funcov.py.ifu.owner_v3_toffee import OwnerV3ToffeeCoverage
from env.funcov.py.ifu.uncache_event_toffee import (
    UNCACHE_EVENT_COVERPOINTS,
    UncacheEventToffeeCoverage,
)
from env.funcov.py.ifu.cfvec_toffee import IfuCfvecToffeeCoverage
from env.funcov.py.ifu.cacheable_pipeline_toffee import IfuCacheablePipelineToffeeCoverage
from env.funcov.recorder import UNCACHE_EVENT_SAMPLER_BIN_KEYS
from env.funcov.toffee_artifact import merge_toffee_artifacts
from env.funcov.toffee_runtime import create_toffee_runtime
from env.funcov.native_toffee import EvaluateFlagRecorder
from tools.backannotate_funcov import normalize_artifact


@dataclass
class _Sample:
    value: str = "none"


def _group() -> tuple[CovGroup, _Sample]:
    sample = _Sample()
    group = CovGroup("contract_group", disable_sample_when_point_hinted=False)
    group.add_watch_point(
        sample,
        {
            "first": lambda target: target.value == "first",
            "second": lambda target: target.value == "second",
        },
        name="contract_point",
        once=False,
    )
    return group, sample


def test_toffee_funcov_report_preserves_group_point_bin_names() -> None:
    group, sample = _group()

    sample.value = "first"
    group.sample()
    report = group.as_dict()

    assert report["name"] == "contract_group"
    assert report["points"][0]["name"] == "contract_point"
    assert report["points"][0]["bins"] == [
        {"name": "first", "hints": 1},
        {"name": "second", "hints": 0},
    ]
    assert report["bin_num_total"] == 2
    assert report["bin_num_hints"] == 1
    assert group.is_point_covered("contract_point") is False


def test_toffee_sink_key_hit_uses_toffee_hints(tmp_path) -> None:
    sink = ToffeeCoverageSink({("group", "point"): ("bin",)})
    sink.attach_audit_backend(object(), tmp_path / "case.toffee.funcov.json")
    assert not sink.key_hit("group", "bin")
    sink.mark("group", "bin", cycle=1, coverpoint="point")
    assert sink.key_hit("group", "bin")
    assert sink.raw_path() == tmp_path / "case.toffee.funcov.json"


def test_toffee_sink_samples_each_group_once_per_cycle() -> None:
    sink = ToffeeCoverageSink(
        {("group", "point"): ("first", "second")}
    )
    sink.mark("group", "first", cycle=4, coverpoint="point")
    sink.mark("group", "second", cycle=4, coverpoint="point")
    sink.flush_cycle(4)

    report = sink.report()[0]
    assert report["__sample_count__"] == 1
    assert {item["name"]: item["hints"] for item in report["points"][0]["bins"]} == {
        "first": 1,
        "second": 1,
    }


def test_toffee_audit_separates_covered_mismatch_from_count_difference() -> None:
    from types import SimpleNamespace

    sink = ToffeeCoverageSink({("group", "point"): ("bin", "other")})
    sink.mark("group", "bin", cycle=1, coverpoint="point")
    sink.flush_cycle(1)
    legacy = SimpleNamespace(
        hits={
            ("group", "point", "bin"): SimpleNamespace(hits=8),
            ("group", "point", "other"): SimpleNamespace(hits=1),
        }
    )

    comparison = sink.compare_legacy(legacy)
    assert comparison["count_differences"] == {
        "group::point::bin": {"legacy": 8, "toffee": 1}
    }
    assert comparison["covered_mismatches"] == {
        "group::point::other": {"legacy": 1, "toffee": 0}
    }


def test_toffee_funcov_continues_sampling_after_point_is_covered() -> None:
    group, sample = _group()

    sample.value = "first"
    group.sample()
    sample.value = "second"
    group.sample()
    sample.value = "first"
    group.sample()

    report = group.as_dict()
    bins = {
        item["name"]: item["hints"] for item in report["points"][0]["bins"]
    }
    assert report["hinted"] is True
    assert report["points"][0]["hinted"] is True
    assert report["__sample_count__"] == 3
    assert bins == {"first": 2, "second": 1}


def test_toffee_funcov_registry_model_contains_all_python_recorder_bins() -> None:
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    keys = {
        (group["name"], point["name"], item["name"])
        for group in sink.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(keys) == 573
    assert len(sink.bin_ids) == 573


def test_toffee_funcov_artifact_has_compact_summary_and_stable_bin_ids(tmp_path) -> None:
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    artifact = sink.write_artifact(tmp_path / "toffee.funcov.json")

    import json

    payload = json.loads(artifact.read_text(encoding="utf-8"))
    assert payload["schema_version"] == 1
    assert payload["collector"]["toffee-test"] != "unavailable"
    assert payload["collector"]["pytoffee"] != "unavailable"
    assert payload["summary"]["bin_num_total"] == 573
    assert payload["summary"]["bin_num_hints"] == 0
    assert len(payload["bin_ids"]) == 573
    assert set(payload["coverage"]) == {"groups"}
    assert payload["metadata"] == {}


def test_toffee_artifact_normalizes_to_frontend_gate_schema(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="case",
        artifact_tag="case",
        output_dir=tmp_path,
        target_bin_ids=["BIN-759"],
    )
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    key = next(key for key, value in sink.bin_ids.items() if value == "BIN-759")
    group_name, point_name, bin_name = key
    sink.mark(group_name, bin_name, cycle=3, coverpoint=point_name)
    sink.flush_cycle(3)
    sink.record_native_hits(
        {(group_name, bin_name): True},
        {group_name: point_name},
        cycle=3,
        evidence={"event": "unit"},
    )
    artifact_path = tmp_path / "case.toffee.funcov.json"
    metadata = {
        "mode": "formal",
        "artifact_tag": hub.artifact_tag,
        "testcase_name": hub.testcase_name,
        "source_csv": hub.source_csv,
        "coverage_targets": hub.coverage_targets,
        "definitions": [vars(item) for item in hub.definitions],
        "run": {
            "outcome": "passed",
            "exit_code": 0,
            "checker": {"status": "pass", "error_count": 0, "errors": []},
            "run_id": "unit-run",
        },
        "execution": {
            "testcase_nodeid": "tests/test_case.py::test_case",
            "funcov_path": str(artifact_path),
            "waveform_path": str(tmp_path / "case.fst"),
            "line_coverage_path": str(tmp_path / "case.dat"),
        },
        "stats": {"monitor": {"cycles_total": 4, "error_count": 0}},
        "errors": [],
        "provenance": hub.provenance,
    }
    sink.write_artifact(artifact_path, metadata=metadata)

    import json

    normalized = normalize_artifact(
        json.loads(artifact_path.read_text(encoding="utf-8")),
        artifact_path=artifact_path,
    )
    hit_key = "::".join(key)
    assert normalized["artifact_schema_version"] == 2
    assert normalized["schema_source"] == "toffee"
    assert normalized["coverage_targets"]["bin_ids"] == ["BIN-759"]
    assert normalized["hits"][hit_key]["bin_id"] == "BIN-759"
    assert normalized["hits"][hit_key]["hits"] == 1
    assert normalized["hits"][hit_key]["first_cycle"] == 3
    assert normalized["hits"][hit_key]["last_cycle"] == 3
    assert normalized["hits"][hit_key]["evidence"] == [{"event": "unit"}]
    assert normalized["run"]["pytest_outcome"] == "passed"


def test_toffee_evidence_skips_serialization_without_active_hits() -> None:
    class UnexpectedEvidenceRead(dict):
        def keys(self):
            raise AssertionError("inactive cycle must not serialize evidence")

    sink = ToffeeCoverageSink({("group", "point"): ("bin",)})
    sink.record_native_hits(
        {("group", "bin"): False},
        {("group", "bin"): "point"},
        cycle=1,
        evidence=UnexpectedEvidenceRead(event="inactive"),
    )
    assert sink._hit_details == {}


def test_formal_fixture_uses_sample_hub_toffee_only() -> None:
    from pathlib import Path

    source = (
        Path(__file__).resolve().parents[3] / "env" / "runtime" / "fixtures.py"
    ).read_text(encoding="utf-8")
    assert 'funcov_dir / f"{tag}.toffee.funcov.json"' in source
    assert "FrontendFuncovSampleHub.from_pilot_csv" in source
    assert "ToffeeCoverageSink.from_registry" in source
    assert "create_toffee_runtime(" in source
    assert "audit_recorder=None" in source
    assert '"mode": "formal"' in source
    # B1 removed legacy fallback / audit / pilot wiring from the formal fixture.
    assert "FunctionalCoverageRecorder" not in source
    assert "TB_ENABLE_TOFFEE_FUNCOV" not in source
    assert "TB_ENABLE_FUNCOV_AUDIT" not in source
    assert "TB_ENABLE_TOFFEE_FUNCOV_PILOT" not in source
    assert 'funcov_dir.parent / "audit" / "legacy-funcov"' not in source
    assert 'metadata["execution"]["legacy_funcov_audit_path"]' not in source
    assert "legacy_recorder" not in source
    assert "runtime_context.write_artifacts()" not in source
    assert 'execution["line_coverage_path"] = str(Path(coverage).resolve())' in source
    assert 'execution["waveform_path"] = str(Path(waveform).resolve())' in source
    assert '"testcase_nodeid": str(execution.get("testcase_nodeid") or "").strip()' in source
    assert '"stats": {' in source
    assert '"coverage_targets": runtime_context.coverage_targets' in source
    assert '"definitions": [asdict(item) for item in runtime_context.definitions]' in source
    assert '"source_csv": runtime_context.source_csv' in source


class _CountingSignal:
    def __init__(self, value: int) -> None:
        self._value = int(value)
        self.reads = 0

    @property
    def value(self) -> int:
        self.reads += 1
        return self._value


class _CountingDut:
    def __init__(self, signal: _CountingSignal) -> None:
        self.shared_signal = signal


def test_recorder_cycle_snapshot_reads_each_dut_value_once() -> None:
    hub = FrontendFuncovSampleHub.__new__(FrontendFuncovSampleHub)
    hub._dut_signal_cache = {}
    hub._missing_dut_signals = set()
    hub._cycle_snapshot_cycle = None
    hub._cycle_snapshot_values = {}
    signal = _CountingSignal(7)
    dut = _CountingDut(signal)

    hub.begin_cycle_snapshot(12)
    assert hub._try_read_dut_signal(dut, "shared_signal") == 7
    assert hub._try_read_dut_signal(dut, "shared_signal") == 7
    assert hub._read_first_dut_signal(dut, ("shared_signal",)) == 7
    assert signal.reads == 1

    hub.begin_cycle_snapshot(13)
    assert hub._try_read_dut_signal(dut, "shared_signal") == 7
    assert signal.reads == 2


def test_funcov_modules_do_not_bypass_snapshot_dut_reads() -> None:
    funcov_root = Path(__file__).resolve().parents[3] / "env" / "funcov"
    allowed = {funcov_root / "recorder.py"}
    violations: list[str] = []

    def _base_name(node: ast.AST) -> str:
        if isinstance(node, ast.Name):
            return node.id
        if isinstance(node, ast.Attribute):
            return f"{_base_name(node.value)}.{node.attr}"
        return type(node).__name__

    for path in sorted(funcov_root.rglob("*.py")):
        if "__pycache__" in path.parts or path in allowed:
            continue
        tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
        for node in ast.walk(tree):
            if isinstance(node, ast.Attribute) and node.attr == "value":
                base = _base_name(node.value)
                if base == "self":
                    continue
                violations.append(f"{path.relative_to(funcov_root)}:{node.lineno}:{base}.value")
            if isinstance(node, ast.Call):
                func = node.func
                if isinstance(func, ast.Attribute) and func.attr == "GetInternalSignal":
                    violations.append(
                        f"{path.relative_to(funcov_root)}:{node.lineno}:GetInternalSignal"
                    )
                if (
                    isinstance(func, ast.Name)
                    and func.id == "getattr"
                    and node.args
                    and isinstance(node.args[0], ast.Name)
                    and node.args[0].id == "dut"
                ):
                    violations.append(
                        f"{path.relative_to(funcov_root)}:{node.lineno}:getattr(dut, ...)"
                    )

    assert violations == []


class _AutoCountingDut:
    def __init__(self) -> None:
        self.signals: dict[str, _CountingSignal] = {
            "reset": _CountingSignal(0),
            "clock": _CountingSignal(0),
        }

    def __getattr__(self, name: str):
        if name.startswith("_"):
            raise AttributeError(name)
        signal = self.signals.get(name)
        if signal is None:
            signal = _CountingSignal(0)
            self.signals[name] = signal
        return signal

    def GetInternalSignal(self, name: str):
        return getattr(self, str(name))


def test_direct_runtime_cycle_models_read_each_dut_signal_at_most_once(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-read-once",
        artifact_tag="runtime-read-once",
        output_dir=tmp_path,
    )
    dut = _AutoCountingDut()
    env = type(
        "Env",
        (),
        {
            "dut": dut,
            "memory": type("Memory", (), {"is_mmio": staticmethod(lambda _addr: False)})(),
            "page_table": None,
        },
    )()
    hub.env = env
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)

    hub.begin_cycle_snapshot(21)
    hub._read_dut_signal(dut, "reset", 0)
    for model in runtime.cycle_models:
        model.on_cycle(21)
    sink.flush_cycle(21)

    overread = {
        name: signal.reads
        for name, signal in sorted(dut.signals.items())
        if signal.reads > 1
    }
    assert overread == {}, overread


def test_direct_runtime_rejects_dut_reads_without_cycle_snapshot(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-no-snapshot",
        artifact_tag="runtime-no-snapshot",
        output_dir=tmp_path,
    )
    dut = _AutoCountingDut()
    hub.env = type("Env", (), {"dut": dut})()
    create_toffee_runtime(hub, ToffeeCoverageSink.from_registry(default_pilot_csv_path()))

    with pytest.raises(RuntimeError, match="active cycle snapshot"):
        hub._try_read_dut_signal(dut, "reset")


def test_formal_fixture_starts_snapshot_before_direct_models() -> None:
    source = (
        Path(__file__).resolve().parents[3] / "env" / "runtime" / "fixtures.py"
    ).read_text(encoding="utf-8")
    callback_start = source.index("def sample_functional_coverage(cycle):")
    callback = source[callback_start : source.index("dut.StepRis(sample_functional_coverage)")]
    assert "runtime_context.on_cycle(cycle, tb)" in callback
    assert "begin_cycle_snapshot" in (
        Path(__file__).resolve().parents[3] / "env" / "funcov" / "recorder.py"
    ).read_text(encoding="utf-8")
    assert callback.index("runtime_context.on_cycle(cycle, tb)") < callback.index(
        "for model in toffee_direct_models:"
    )


def test_formal_runtime_context_rejects_legacy_artifact_output(tmp_path) -> None:
    from env.funcov.runtime_context import FrontendFuncovRuntimeContext

    assert FrontendFuncovRuntimeContext is FrontendFuncovSampleHub
    assert not issubclass(FrontendFuncovSampleHub, FunctionalCoverageRecorder)
    assert issubclass(FunctionalCoverageRecorder, FrontendFuncovSampleHub)

    context = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="formal-context",
        artifact_tag="formal-context",
        output_dir=tmp_path,
    )

    with pytest.raises(RuntimeError, match="cannot write legacy funcov artifacts"):
        context.write_artifacts()

    legacy = FunctionalCoverageRecorder.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="legacy-context",
        artifact_tag="legacy-context",
        output_dir=tmp_path / "legacy",
    )
    written = legacy.write_artifacts()
    assert Path(written["raw_path"]).is_file()


def test_toffee_mainpipe_model_contains_all_54_bins() -> None:
    class Runtime:
        env = type("Env", (), {"dut": object()})()

    model = ICacheMainpipeToffeeCoverage(Runtime())
    keys = {
        (group["name"], point["name"], item["name"])
        for group in model.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(keys) == 54


def test_toffee_prefetchpipe_model_contains_all_37_bins() -> None:
    class Runtime:
        env = type("Env", (), {"dut": object()})()

    model = ICachePrefetchpipeToffeeCoverage(Runtime())
    keys = {
        (group["name"], point["name"], item["name"])
        for group in model.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(keys) == 37


def test_toffee_missunit_model_contains_all_34_bins() -> None:
    class Runtime:
        env = type("Env", (), {"dut": object()})()

    model = ICacheMissunitToffeeCoverage(Runtime())
    keys = {
        (group["name"], point["name"], item["name"])
        for group in model.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(keys) == 34


def test_toffee_waylookup_model_contains_all_41_bins() -> None:
    class Runtime:
        env = type("Env", (), {"dut": object()})()

    model = ICacheWaylookupToffeeCoverage(Runtime())
    keys = {
        (group["name"], point["name"], item["name"])
        for group in model.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(keys) == 41


def test_toffee_two_fetch_model_contains_all_41_bins() -> None:
    class Runtime:
        env = type("Env", (), {"dut": object()})()

    model = TwoFetchToffeeCoverage(Runtime())
    keys = {
        (group["name"], point["name"], item["name"])
        for group in model.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(keys) == 41


def test_toffee_mmio_v3_model_contains_all_4_bins() -> None:
    class Runtime:
        env = type("Env", (), {"dut": object()})()

    model = MmioV3ToffeeCoverage(Runtime())
    keys = {
        (group["name"], point["name"], item["name"])
        for group in model.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(keys) == 4


def test_toffee_mmio_nc_owner_model_contains_all_116_bins() -> None:
    class Runtime:
        env = type("Env", (), {"dut": object()})()

    model = MmioNcOwnerToffeeCoverage(Runtime())
    keys = {
        (group["name"], point["name"], item["name"])
        for group in model.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(keys) == 116


def test_toffee_owner_v3_model_contains_all_106_bins() -> None:
    class Runtime:
        env = type("Env", (), {"dut": object()})()

    model = OwnerV3ToffeeCoverage(Runtime())
    keys = {
        (group["name"], point["name"], item["name"])
        for group in model.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(keys) == 106


def test_toffee_uncache_event_model_contains_all_6_bins() -> None:
    class Runtime:
        env = type("Env", (), {"dut": object()})()

    model = UncacheEventToffeeCoverage(Runtime())
    keys = {
        (group["name"], point["name"], item["name"])
        for group in model.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(keys) == 6


def test_toffee_remaining_ifu_models_contain_all_124_bins() -> None:
    class Runtime:
        env = type("Env", (), {"dut": object()})()

    models = (
        IfuCfvecToffeeCoverage(Runtime()),
        IfuCacheablePipelineToffeeCoverage(Runtime()),
    )
    keys = {
        (group["name"], point["name"], item["name"])
        for model in models
        for group in model.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(keys) == 124




def test_all_573_bins_are_native_installed(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-all-native",
        artifact_tag="runtime-all-native",
        output_dir=tmp_path,
    )
    hub.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)
    native_bins = {
        (group["name"], point["name"], item["name"])
        for group in sink.report()
        for point in group["points"]
        for item in point["bins"]
        if group["name"] in sink._native_group_names
    }
    all_bins = {
        (group["name"], point["name"], item["name"])
        for group in sink.report()
        for point in group["points"]
        for item in point["bins"]
    }
    assert len(all_bins) == 573
    assert native_bins == all_bins
    assert all_bins == set(sink.bin_ids)
    assert len(runtime.cycle_models) == 12

def test_toffee_artifact_merge_sums_hints_and_rejects_failed_runs(
    tmp_path, monkeypatch
) -> None:
    from tools import backannotate_funcov

    monkeypatch.setattr(
        backannotate_funcov,
        "evaluate_artifact",
        lambda _raw, **_kwargs: {"eligible": True, "reasons": []},
    )
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    key = next(iter(sink.bin_ids))
    group_name, point_name, bin_name = key
    sink.mark(group_name, bin_name, cycle=1, coverpoint=point_name)
    metadata = {
        "mode": "formal",
        "run": {
            "outcome": "passed",
            "exit_code": 0,
            "checker": {"status": "pass"},
        },
        "provenance": {"compatibility_signature": "same"},
    }
    first = sink.write_artifact(tmp_path / "first.json", metadata=metadata)
    second = sink.write_artifact(tmp_path / "second.json", metadata=metadata)
    merged = merge_toffee_artifacts(
        (first, second), tmp_path / "merged.json"
    )

    import json

    payload = json.loads(merged.read_text(encoding="utf-8"))
    merged_bins = {
        (group["name"], point["name"], item["name"]): item["hints"]
        for group in payload["coverage"]["groups"]
        for point in group["points"]
        for item in point["bins"]
    }
    assert merged_bins[key] == 2

    failed_metadata = deepcopy(metadata)
    failed_metadata["run"]["outcome"] = "failed"
    failed = sink.write_artifact(tmp_path / "failed.json", metadata=failed_metadata)
    with pytest.raises(ValueError, match="did not pass"):
        merge_toffee_artifacts((first, failed), tmp_path / "rejected.json")


def test_toffee_merge_rejects_artifact_without_frontend_gate_metadata(tmp_path) -> None:
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    artifact = sink.write_artifact(
        tmp_path / "incomplete.json",
        metadata={
            "mode": "formal",
            "run": {
                "outcome": "passed",
                "exit_code": 0,
                "checker": {"status": "pass"},
            },
            "provenance": {"compatibility_signature": "same"},
        },
    )

    with pytest.raises(ValueError, match="failed signoff gate"):
        merge_toffee_artifacts((artifact,), tmp_path / "rejected.json")


def test_toffee_merge_cli_is_available() -> None:
    from pathlib import Path

    tool = Path(__file__).resolve().parents[3] / "tools" / "merge_toffee_funcov.py"
    assert tool.is_file()


def test_backannotation_cli_is_read_only() -> None:
    tool = Path(__file__).resolve().parents[3] / "tools" / "backannotate_funcov.py"
    source = tool.read_text(encoding="utf-8")
    main_source = source[source.index("def main() -> int:") :]

    assert "apply=False" in main_source
    assert "apply=not args.check" not in main_source


def test_direct_runtime_models_partition_all_573_bins(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-contract",
        artifact_tag="runtime-contract",
        output_dir=tmp_path,
    )
    hub.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)
    keys = []
    for model in runtime.cycle_models:
        keys.extend(model.hit_counts())

    assert len(keys) == 573
    assert len(set(keys)) == 573
    assert set(keys) == set(sink.hit_counts())


def test_direct_runtime_defaults_to_no_audit_transfer(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-no-audit",
        artifact_tag="runtime-no-audit",
        output_dir=tmp_path,
    )
    hub.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)
    # All direct domains are now native CovGroup models.
    assert runtime.cycle_models
    assert all(getattr(model, "_audit_recorder", None) is None for model in runtime.cycle_models)
    assert set(sink._native_group_names)
    # Shared sink mark-bridge is reserved for non-native groups only; with full
    # native install it should reject every active domain group.
    sample_group = sorted(sink._native_group_names)[0]
    sample_point = next(
        point["name"]
        for group in sink.report()
        if group["name"] == sample_group
        for point in group["points"]
    )
    sample_bin = next(
        item["name"]
        for group in sink.report()
        if group["name"] == sample_group
        for point in group["points"]
        if point["name"] == sample_point
        for item in point["bins"]
    )
    with pytest.raises(RuntimeError, match="native Toffee group rejects mark-bridge"):
        sink.mark(sample_group, sample_bin, cycle=1, coverpoint=sample_point)


def test_native_evaluator_dispatches_checked_hits_to_owner_model(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-cross-domain",
        artifact_tag="runtime-cross-domain",
        output_dir=tmp_path,
    )
    hub.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)
    cfvec = next(
        model
        for model in runtime.cycle_models
        if model.__class__.__name__ == "IfuCfvecToffeeCoverage"
    )
    wrapped = EvaluateFlagRecorder(cfvec, {}, {})

    wrapped.mark(
        "ifu_v3_boundary_owner_model",
        "owner_leaf_076",
        cycle=754,
        evidence={"event": "checked-owner"},
    )

    assert sink.key_hit("ifu_v3_boundary_owner_model", "owner_leaf_076")
    detail = sink._hit_details[
        (
            "ifu_v3_boundary_owner_model",
            "verified_leaf_event",
            "owner_leaf_076",
        )
    ]
    assert detail["first_cycle"] == 754
    assert detail["evidence"] == [{"event": "checked-owner"}]

    wrapped.mark(
        "two_fetch_ifu_source",
        "two_ftq_sources",
        cycle=751,
        evidence={"event": "cross-domain-source"},
    )
    assert sink.key_hit("two_fetch_ifu_source", "two_ftq_sources")


def test_native_source_derivation_uses_toffee_hints_not_legacy_hits(tmp_path) -> None:
    context = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-owner-derivation",
        artifact_tag="runtime-owner-derivation",
        output_dir=tmp_path,
    )
    context.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    create_toffee_runtime(context, sink)
    source_group, _source_point, source_bin = sink._key_by_bin_id["BIN-831"]

    sink.mark_native(
        source_group,
        source_bin,
        cycle=17,
        evidence={"event": "canonical-source"},
    )

    assert not hasattr(context, "hits")
    assert sink.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_001")
    assert sink.hit_count_by_bin_id("BIN-899") == 1


def test_hitmiss_native_groups_are_installed_and_not_mark_bridged(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-hitmiss-native",
        artifact_tag="runtime-hitmiss-native",
        output_dir=tmp_path,
    )
    hub.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)
    hitmiss = next(
        item
        for item in runtime.cycle_models
        if item.__class__.__name__ == "ICacheHitMissToffeeCoverage"
    )

    assert "icache_hit_path" in sink._native_group_names
    assert "icache_miss_path" in sink._native_group_names
    assert {
        (group["name"], point["name"], item["name"])
        for group in sink.report()
        for point in group["points"]
        for item in point["bins"]
        if group["name"] in {"icache_hit_path", "icache_miss_path"}
    } == {
        (group, ICACHE_HITMISS_COVERPOINTS[group], bin_name)
        for group, bin_name in ICACHE_HITMISS_SAMPLER_BIN_KEYS
    }
    with pytest.raises(RuntimeError, match="native Toffee group rejects mark-bridge"):
        sink.mark("icache_hit_path", "continuous_same_line_sram_hit", cycle=1)
    # Native model samples its own groups; shared flush must not touch them.
    before = hitmiss.hit_counts()[
        "icache_hit_path",
        "hit_behavior",
        "continuous_same_line_sram_hit",
    ]
    sink._pending_cycle = 99
    sink._dirty_groups.add("icache_hit_path")
    sink.flush_cycle(99)
    assert (
        hitmiss.hit_counts()[
            "icache_hit_path",
            "hit_behavior",
            "continuous_same_line_sram_hit",
        ]
        == before
    )


def test_mainpipe_native_groups_are_installed_and_not_mark_bridged(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-mainpipe-native",
        artifact_tag="runtime-mainpipe-native",
        output_dir=tmp_path,
    )
    hub.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)
    mainpipe = next(
        item
        for item in runtime.cycle_models
        if item.__class__.__name__ == "ICacheMainpipeToffeeCoverage"
    )

    native_groups = set(ICACHE_MAINPIPE_COVERPOINTS)
    assert native_groups <= sink._native_group_names
    assert {
        (group["name"], point["name"], item["name"])
        for group in sink.report()
        for point in group["points"]
        for item in point["bins"]
        if group["name"] in native_groups
    } == {
        (group, ICACHE_MAINPIPE_COVERPOINTS[group], bin_name)
        for group, bin_name in ICACHE_MAINPIPE_SAMPLER_BIN_KEYS
    }
    sample_group, sample_bin = next(iter(ICACHE_MAINPIPE_SAMPLER_BIN_KEYS))
    with pytest.raises(RuntimeError, match="native Toffee group rejects mark-bridge"):
        sink.mark(sample_group, sample_bin, cycle=1)
    before = mainpipe.hit_counts()[
        sample_group,
        ICACHE_MAINPIPE_COVERPOINTS[sample_group],
        sample_bin,
    ]
    sink._pending_cycle = 77
    sink._dirty_groups.add(sample_group)
    sink.flush_cycle(77)
    assert (
        mainpipe.hit_counts()[
            sample_group,
            ICACHE_MAINPIPE_COVERPOINTS[sample_group],
            sample_bin,
        ]
        == before
    )


def test_prefetchpipe_native_groups_are_installed_and_not_mark_bridged(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-prefetch-native",
        artifact_tag="runtime-prefetch-native",
        output_dir=tmp_path,
    )
    hub.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)
    prefetch = next(
        item
        for item in runtime.cycle_models
        if item.__class__.__name__ == "ICachePrefetchpipeToffeeCoverage"
    )

    native_groups = set(ICACHE_PREFETCHPIPE_COVERPOINTS)
    assert native_groups <= sink._native_group_names
    assert {
        (group["name"], point["name"], item["name"])
        for group in sink.report()
        for point in group["points"]
        for item in point["bins"]
        if group["name"] in native_groups
    } == {
        (group, ICACHE_PREFETCHPIPE_COVERPOINTS[group], bin_name)
        for group, bin_name in ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS
    }
    sample_group, sample_bin = next(iter(ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS))
    with pytest.raises(RuntimeError, match="native Toffee group rejects mark-bridge"):
        sink.mark(sample_group, sample_bin, cycle=1)
    before = prefetch.hit_counts()[
        sample_group,
        ICACHE_PREFETCHPIPE_COVERPOINTS[sample_group],
        sample_bin,
    ]
    sink._pending_cycle = 55
    sink._dirty_groups.add(sample_group)
    sink.flush_cycle(55)
    assert (
        prefetch.hit_counts()[
            sample_group,
            ICACHE_PREFETCHPIPE_COVERPOINTS[sample_group],
            sample_bin,
        ]
        == before
    )


def test_missunit_native_groups_are_installed_and_not_mark_bridged(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-missunit-native",
        artifact_tag="runtime-missunit-native",
        output_dir=tmp_path,
    )
    hub.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)
    missunit = next(
        item
        for item in runtime.cycle_models
        if item.__class__.__name__ == "ICacheMissunitToffeeCoverage"
    )

    native_groups = set(ICACHE_MISSUNIT_COVERPOINTS)
    assert native_groups <= sink._native_group_names
    assert {
        (group["name"], point["name"], item["name"])
        for group in sink.report()
        for point in group["points"]
        for item in point["bins"]
        if group["name"] in native_groups
    } == {
        (group, ICACHE_MISSUNIT_COVERPOINTS[group], bin_name)
        for group, bin_name in ICACHE_MISSUNIT_SAMPLER_BIN_KEYS
    }
    sample_group, sample_bin = next(iter(ICACHE_MISSUNIT_SAMPLER_BIN_KEYS))
    with pytest.raises(RuntimeError, match="native Toffee group rejects mark-bridge"):
        sink.mark(sample_group, sample_bin, cycle=1)
    before = missunit.hit_counts()[
        sample_group,
        ICACHE_MISSUNIT_COVERPOINTS[sample_group],
        sample_bin,
    ]
    sink._pending_cycle = 44
    sink._dirty_groups.add(sample_group)
    sink.flush_cycle(44)
    assert (
        missunit.hit_counts()[
            sample_group,
            ICACHE_MISSUNIT_COVERPOINTS[sample_group],
            sample_bin,
        ]
        == before
    )


def test_waylookup_native_groups_are_installed_and_not_mark_bridged(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-waylookup-native",
        artifact_tag="runtime-waylookup-native",
        output_dir=tmp_path,
    )
    hub.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)
    waylookup = next(
        item
        for item in runtime.cycle_models
        if item.__class__.__name__ == "ICacheWaylookupToffeeCoverage"
    )

    native_groups = set(ICACHE_WAYLOOKUP_COVERPOINTS)
    assert native_groups <= sink._native_group_names
    assert {
        (group["name"], point["name"], item["name"])
        for group in sink.report()
        for point in group["points"]
        for item in point["bins"]
        if group["name"] in native_groups
    } == {
        (group, ICACHE_WAYLOOKUP_COVERPOINTS[group], bin_name)
        for group, bin_name in ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS
    }
    sample_group, sample_bin = next(iter(ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS))
    with pytest.raises(RuntimeError, match="native Toffee group rejects mark-bridge"):
        sink.mark(sample_group, sample_bin, cycle=1)
    before = waylookup.hit_counts()[
        sample_group,
        ICACHE_WAYLOOKUP_COVERPOINTS[sample_group],
        sample_bin,
    ]
    sink._pending_cycle = 33
    sink._dirty_groups.add(sample_group)
    sink.flush_cycle(33)
    assert (
        waylookup.hit_counts()[
            sample_group,
            ICACHE_WAYLOOKUP_COVERPOINTS[sample_group],
            sample_bin,
        ]
        == before
    )


def test_two_fetch_native_groups_are_installed_and_not_mark_bridged(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-two-fetch-native",
        artifact_tag="runtime-two-fetch-native",
        output_dir=tmp_path,
    )
    hub.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)
    two_fetch = next(
        item
        for item in runtime.cycle_models
        if item.__class__.__name__ == "TwoFetchToffeeCoverage"
    )

    native_groups = set(TWO_FETCH_COVERPOINTS)
    assert native_groups <= sink._native_group_names
    assert {
        (group["name"], point["name"], item["name"])
        for group in sink.report()
        for point in group["points"]
        for item in point["bins"]
        if group["name"] in native_groups
    } == {
        (group, TWO_FETCH_COVERPOINTS[group], bin_name)
        for group, bin_name in TWO_FETCH_SAMPLER_BIN_KEYS
    }
    sample_group, sample_bin = next(iter(TWO_FETCH_SAMPLER_BIN_KEYS))
    with pytest.raises(RuntimeError, match="native Toffee group rejects mark-bridge"):
        sink.mark(sample_group, sample_bin, cycle=1)
    before = two_fetch.hit_counts()[
        sample_group,
        TWO_FETCH_COVERPOINTS[sample_group],
        sample_bin,
    ]
    sink._pending_cycle = 22
    sink._dirty_groups.add(sample_group)
    sink.flush_cycle(22)
    assert (
        two_fetch.hit_counts()[
            sample_group,
            TWO_FETCH_COVERPOINTS[sample_group],
            sample_bin,
        ]
        == before
    )

def test_uncache_native_groups_are_installed_and_not_mark_bridged(tmp_path) -> None:
    hub = FrontendFuncovSampleHub.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="runtime-uncache-native",
        artifact_tag="runtime-uncache-native",
        output_dir=tmp_path,
    )
    hub.env = type("Env", (), {"dut": object()})()
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    runtime = create_toffee_runtime(hub, sink)
    uncache = next(
        item
        for item in runtime.cycle_models
        if item.__class__.__name__ == "UncacheEventToffeeCoverage"
    )

    native_groups = set(UNCACHE_EVENT_COVERPOINTS)
    assert native_groups <= sink._native_group_names
    assert {
        (group["name"], point["name"], item["name"])
        for group in sink.report()
        for point in group["points"]
        for item in point["bins"]
        if group["name"] in native_groups
    } == {
        (group, UNCACHE_EVENT_COVERPOINTS[group], bin_name)
        for group, bin_name in UNCACHE_EVENT_SAMPLER_BIN_KEYS
    }
    sample_group, sample_bin = next(iter(UNCACHE_EVENT_SAMPLER_BIN_KEYS))
    with pytest.raises(RuntimeError, match="native Toffee group rejects mark-bridge"):
        sink.mark(sample_group, sample_bin, cycle=1)
    before = uncache.hit_counts()[
        sample_group,
        UNCACHE_EVENT_COVERPOINTS[sample_group],
        sample_bin,
    ]
    sink._pending_cycle = 11
    sink._dirty_groups.add(sample_group)
    sink.flush_cycle(11)
    assert (
        uncache.hit_counts()[
            sample_group,
            UNCACHE_EVENT_COVERPOINTS[sample_group],
            sample_bin,
        ]
        == before
    )
    # Event-driven path samples native groups directly without shared mark-bridge.
    uncache.mark(sample_group, sample_bin, cycle=12, evidence={"event": "path_switch"})
    assert (
        uncache.hit_counts()[
            sample_group,
            UNCACHE_EVENT_COVERPOINTS[sample_group],
            sample_bin,
        ]
        == before + 1
    )
