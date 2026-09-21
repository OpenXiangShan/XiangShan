from __future__ import annotations

import ast
from dataclasses import dataclass
from pathlib import Path

import pytest
from toffee.funcov import CovGroup
from env.funcov.recorder import default_pilot_csv_path
from env.funcov.toffee_bridge import (
    ToffeeCoverageSink,
    ToffeeSessionCoverage,
    merge_native_toffee_reports,
)
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
from env.funcov.toffee_runtime import create_toffee_runtime
from env.funcov.native_toffee import EvaluateFlagRecorder


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


def test_toffee_sink_key_hit_uses_toffee_hints() -> None:
    sink = ToffeeCoverageSink({("group", "point"): ("bin",)})
    assert not sink.key_hit("group", "bin")
    sink.mark("group", "bin", cycle=1, coverpoint="point")
    assert sink.key_hit("group", "bin")


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


def test_toffee_session_coverage_merges_native_groups_without_frontend_metadata(tmp_path) -> None:
    first = ToffeeCoverageSink({("group", "point"): ("bin",)})
    first.mark("group", "bin", cycle=1, coverpoint="point")
    first.flush_cycle(1)
    second = ToffeeCoverageSink({("group", "point"): ("bin",)})
    second.mark("group", "bin", cycle=2, coverpoint="point")
    second.flush_cycle(2)

    collector = ToffeeSessionCoverage()
    collector.add(first.cov_groups)
    collector.add(second.cov_groups)
    output = collector.write(tmp_path / "toffee.funcov.json")

    import json

    payload = json.loads(output.read_text(encoding="utf-8"))
    assert "metadata" not in payload
    assert payload["groups"][0]["points"][0]["bins"] == [
        {"name": "bin", "hints": 2}
    ]


def test_native_toffee_report_merge_counts_every_input(tmp_path) -> None:
    reports = []
    for cycle in (1, 2):
        sink = ToffeeCoverageSink({("group", "point"): ("bin",)})
        sink.mark("group", "bin", cycle=cycle, coverpoint="point")
        sink.flush_cycle(cycle)
        collector = ToffeeSessionCoverage()
        collector.add(sink.cov_groups)
        reports.append(collector.write(tmp_path / f"case_{cycle}.json"))

    output = merge_native_toffee_reports(reports, tmp_path / "merged.json")

    import json

    payload = json.loads(output.read_text(encoding="utf-8"))
    assert payload["groups"][0]["points"][0]["bins"] == [
        {"name": "bin", "hints": 2}
    ]


def test_formal_fixture_uses_sample_hub_toffee_only() -> None:
    from pathlib import Path

    source = (
        Path(__file__).resolve().parents[3] / "env" / "runtime" / "fixtures.py"
    ).read_text(encoding="utf-8")
    assert "FrontendFuncovSampleHub.from_pilot_csv" in source
    assert "ToffeeCoverageSink.from_registry" in source
    assert "create_toffee_runtime(" in source
    assert "_session_toffee_coverage(request).add(toffee_sink.cov_groups)" in source
    assert "audit_recorder=None" in source
    assert 'collector.write(_funcov_dir() / "toffee.funcov.json")' in source
    assert "FunctionalCoverageRecorder" not in source
    assert "TB_ENABLE_TOFFEE_FUNCOV" not in source
    assert "TB_ENABLE_FUNCOV_AUDIT" not in source
    assert "TB_ENABLE_TOFFEE_FUNCOV_PILOT" not in source
    assert 'funcov_dir.parent / "audit" / "legacy-funcov"' not in source
    assert 'metadata["execution"]["legacy_funcov_audit_path"]' not in source
    assert "legacy_recorder" not in source


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
