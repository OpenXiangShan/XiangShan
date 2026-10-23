"""Transaction/negative contracts for the four input-only parity scenarios."""

import copy
import csv
from dataclasses import replace
from pathlib import Path
from types import SimpleNamespace

import pytest

from env.funcov.py.icache.icache_data_ecc_ibuffer_funcov import (
    BIN_POINTS, GROUP, SIGNALS, DataEccScenarioCoverage,
)
from env.funcov.py.icache.icache_data_ecc_ibuffer_toffee import ICacheDataEccIBufferToffeeCoverage
from env.funcov.py.icache.signal_contract import validate_target_probes
from env.funcov.recorder import FrontendFuncovSampleHub
from env.funcov.registry import default_pilot_csv_path
from env.funcov.toffee_bridge import ToffeeCoverageSink
from env.checkers.icache_data_ecc_checker import DataEccRequestChecker
from env.support.icache_data_ecc import (
    BankRead, SCENARIO_CASES, SCENARIO_POINTS, fault_banks, parity, scenario_bin_name,
)


BASE = 0x80040000
IDENTITY = (1, 12)
CASES = [(kind, banks) for kind, cases in SCENARIO_CASES.items() for banks in cases]


def frames(kind="line1", banks=(2,)):
    masks = (255, 0) if kind == "single" else (240, 15)
    pc = BASE + (0 if kind == "single" else 32)
    faults = fault_banks(kind, banks)
    words, mutations, metas, refills = {}, {}, [0, 0], []
    for line, mask in enumerate(masks):
        if not mask:
            continue
        data = 0
        for bank in range(8):
            value = 0x1300000013 + 2 * bank + 64 * line
            clean = (value << 2) | (parity(value) << 1)
            key = line, bank
            words[key] = clean ^ (2 if key in faults else 0)
            data |= value << (bank * 64)
            if key in faults:
                mutations[key] = dict(before=clean, after=clean ^ 2, mask=2,
                                      paddr=BASE + 64 * line, way=line,
                                      row=((BASE >> 6) + line) & 255)
        payload = ((BASE + line * 64) >> 12) << 32 | 0xFFFF
        metas[line] = (payload << 1) | (payload.bit_count() & 1)
        refills.append((line + 1, dict(refill=dict(paddr=BASE + line * 64,
            vset=((BASE >> 6) + line) & 255, waymask=1 << line, data=data,
            corrupt=0, denied=0, meta_write=1, data_write=1, ready=1))))
    physical = []
    data, codes = [0] * 8, [0] * 8
    for bank in range(8):
        line = next(line for line, mask in enumerate(masks) if mask & (1 << bank))
        physical.append(BankRead(1, 1, 1 << line, (line,) * 4))
        raw = words[line, bank]
        data[bank], codes[bank] = raw >> 2, (raw >> 1) & 1
    read = dict(identity=IDENTITY, start_pc=pc, end_position=31, request_masks=masks,
                req1_valid=0, ecc_enable=1, flush=0, ways=(0, 1), metas=metas,
                words=words, mutations=mutations, banks=physical)
    response = dict(identity=IDENTITY, data=data, codes=codes, sram_valid=(1,) * 8, mshr_valid=(0, 0))
    handoff = dict(identity=IDENTITY, start_pc=pc, paddr=pc, exception=0, mmio=0, pbmt=0,
                   backend=0, mshr_valid=(0, 0))
    metadata = dict(identity=IDENTITY, start_pc=pc, cross=int(bool(masks[1])), hits=(1, 1),
                    waymasks=(1, 2), metas=list(metas))
    return [(cycle, {"global_flush": 0, **f}) for cycle, f in refills + [
        (10, dict(read=read)), (11, dict(response=response, handoff=handoff)),
        (12, dict(metadata=metadata)), (13, dict(delivery=dict(identity=IDENTITY, previous_half=0)))]]


def run(sequence):
    state, hits = DataEccScenarioCoverage(), []
    for cycle, frame in sequence:
        hits.extend(state.observe(cycle, frame))
    return state, hits


@pytest.mark.parametrize("kind,banks", CASES)
def test_all_47_bank_sets_have_one_request_owned_input_hit(kind, banks):
    state, hits = run(frames(kind, banks))
    assert not state.diagnostics
    assert len(hits) == 1
    name, evidence = hits[0]
    assert name == scenario_bin_name(kind, fault_banks(kind, banks))
    assert BIN_POINTS[name] == SCENARIO_POINTS[kind]
    assert evidence["response"]["identity"] == evidence["delivery"]["identity"] == IDENTITY
    assert not state.pending


@pytest.mark.parametrize("phase,key,value", [
    ("read", "ecc_enable", 0), ("read", "req1_valid", 1), ("read", "flush", 1),
    ("read", "request_masks", (240, 11)),
    ("response", "mshr_valid", (0, 1)), ("response", "sram_valid", (0,) * 8),
    ("handoff", "exception", 5), ("handoff", "exception", 3),
    ("handoff", "mmio", 1), ("handoff", "pbmt", 1), ("handoff", "backend", 1),
    ("handoff", "paddr", BASE + 0x100),
    ("metadata", "waymasks", (1, 3)), ("metadata", "hits", (1, 0)),
    ("metadata", "cross", 0), ("delivery", "previous_half", 1),
])
def test_each_excluded_source_or_invalid_input_prevents_a_hit(phase, key, value):
    sequence = frames()
    next(f[phase] for _, f in sequence if phase in f)[key] = value
    state, hits = run(sequence)
    assert not hits
    assert state.diagnostics


@pytest.mark.parametrize("change", ["omitted", "set", "way", "ready"])
def test_bank2_cannot_be_omitted_or_read_from_another_sram_word(change):
    sequence = frames()
    read = next(f["read"] for _, f in sequence if "read" in f)
    read["banks"][2] = replace(read["banks"][2], **{
        "omitted": dict(valid=0), "set": dict(sets=(0,) * 4),
        "way": dict(waymask=1), "ready": dict(ready=0),
    }[change])
    assert not run(sequence)[1]


@pytest.mark.parametrize("change", ["refill", "refill_error", "other_bank", "meta", "before", "overwritten"])
def test_refill_and_injection_evidence_is_required(change):
    sequence = frames()
    read = next(f["read"] for _, f in sequence if "read" in f)
    if change == "refill":
        sequence = [(cycle, f) for cycle, f in sequence if "refill" not in f]
    elif change == "refill_error":
        sequence[1][1]["refill"]["corrupt"] = 1
    elif change == "other_bank":
        read["words"][1, 3] ^= 4
    elif change == "meta":
        read["metas"][1] ^= 1
    elif change == "before":
        read["mutations"][1, 2]["before"] ^= 4
    else:
        next(f["response"] for _, f in sequence if "response" in f)["codes"][2] ^= 1
    assert not run(sequence)[1]


@pytest.mark.parametrize("phase", ["response", "handoff", "metadata", "delivery"])
def test_wrong_ftq_wrap_flag_or_index_cannot_complete_another_request(phase):
    sequence = frames()
    next(f[phase] for _, f in sequence if phase in f)["identity"] = (0, 12)
    assert not run(sequence)[1]


@pytest.mark.parametrize("cancellation", ["reset", "global_flush", "stage_flush", "missing_flush"])
def test_cancelled_requests_do_not_contribute_and_reset_discards_refill_witnesses(cancellation):
    sequence = frames()
    f = next(f for _, f in sequence if "metadata" in f)
    if cancellation == "stage_flush":
        f["stages"] = [(IDENTITY, 1)]
    elif cancellation == "missing_flush":
        f["global_flush"] = None
    else:
        f[cancellation] = 1
    state, hits = run(sequence)
    assert not hits
    if cancellation == "reset":
        assert not state.refills


def test_handshake_is_counted_once_and_both_lines_must_belong_to_one_request():
    sequence = frames("both", ((6, 7), (0, 1, 2)))
    sequence.append(copy.deepcopy(sequence[-1]))
    assert len(run(sequence)[1]) == 1
    sequence = frames("both", ((6,), (2,)))
    response = next(f["response"] for _, f in sequence if "response" in f)
    response["identity"] = (1, 13)
    assert not run(sequence)[1]


@pytest.mark.parametrize("result", ["corrupt", "hwe"])
def test_incorrect_results_do_not_erase_input_coverage_but_checker_rejects_them(result):
    sequence = frames()
    # These are output payloads, intentionally absent from the coverage inputs.
    sequence[-2][1]["metadata"]["corrupt"] = (0, 0)
    sequence[-1][1]["delivery"].update(exception=0, exception_mask=0)
    assert len(run(sequence)[1]) == 1
    read = next(f["read"] for _, f in sequence if "read" in f)
    response = next(f["response"] for _, f in sequence if "response" in f)
    checker = DataEccRequestChecker(read["start_pc"], read["request_masks"],
                                    read["ways"], read["words"], read["mutations"])
    checker.check_read(cycle=10, **{k: read[k] for k in (
        "start_pc", "end_position", "identity", "req1_valid", "ecc_enable", "flush", "request_masks", "banks")})
    checker.check_response(cycle=11, start_pc=read["start_pc"], **response)
    checker.check_handoff(cycle=11, identity=IDENTITY, exception=0, mmio=0, pbmt=0, mshr_valid=(0, 0))
    ecc = dict(cycle=12, identity=IDENTITY, start_pc=read["start_pc"], cross_line=1, hits=(1, 1))
    if result == "corrupt":
        with pytest.raises(AssertionError, match="corrupt differs"):
            checker.check_ecc(corrupt=(0, 0), **ecc)
    else:
        checker.check_ecc(corrupt=(0, 1), **ecc)
        with pytest.raises(AssertionError, match="did not carry Hwe"):
            checker.check_delivery(cycle=13, identity=IDENTITY, valid=1, ready=1, exception=0,
                exception_mask=0, valid_mask=1, enq_mask=1, previous_half=0,
                slots={0: dict(identity=IDENTITY, pc=read["start_pc"])})


def test_native_groups_registry_mapping_and_parameter_targets_use_the_same_47_bins(tmp_path):
    hub = FrontendFuncovSampleHub.from_pilot_csv(default_pilot_csv_path(), testcase_name="data_ecc_contract", artifact_tag="unit",
                                          output_dir=tmp_path)
    hub.attach(SimpleNamespace(dut=SimpleNamespace()))
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    hub.attach_toffee_sink(sink)
    model = ICacheDataEccIBufferToffeeCoverage(hub, sink=sink)
    groups = model.report()
    points = {p["name"]: p for p in groups[0]["points"]}
    assert {p: len(v["bins"]) for p, v in points.items()} == {
        "single_line": 11, "cross_line0": 6, "cross_line1": 6, "cross_both": 24}
    definitions = [d for d in hub.definitions if d.coverage_group == GROUP]
    assert len(definitions) == len(BIN_POINTS) == 47
    for d in definitions:
        assert d.coverpoint == BIN_POINTS[d.bin_name]
    from tests.py.icache.directed_dut import test_icache_data_ecc_ibuffer_dut as directed
    targets = []
    for name in ("single_line", "cross_line0", "cross_line1", "cross_both"):
        fn = getattr(directed, "test_icache_data_ecc_ibuffer_" + name)
        params = next(m.args[1] for m in fn.pytestmark if m.name == "parametrize")
        targets.extend(p.marks[0].args[0] for p in params)
    assert set(targets) == {d.bin_id for d in definitions}
    tp = Path(__file__).resolve().parents[4] / "docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    with tp.open(encoding="utf-8-sig", newline="") as handle:
        mapped = [r[8] for r in csv.reader(handle) if len(r) > 8 and GROUP in r[8]]
    assert set(mapped) == {f"covergroup {GROUP}, coverpoint {p}" for p in points}


def test_native_cycle_model_uses_one_hub_state_and_samples_real_native_bins(tmp_path, monkeypatch):
    from env.funcov.py.icache.icache_data_ecc_ibuffer_funcov import Probes

    hub = FrontendFuncovSampleHub.from_pilot_csv(default_pilot_csv_path(),
        testcase_name="data_ecc_native", artifact_tag="unit", output_dir=tmp_path)
    hub.attach(SimpleNamespace(dut=SimpleNamespace()))
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    hub.attach_toffee_sink(sink)
    model = ICacheDataEccIBufferToffeeCoverage(hub, sink=sink)
    sequence = dict(frames("both", ((6, 7), (0, 1, 2))))
    monkeypatch.setattr(Probes, "frame", lambda self, env, state, cycle: sequence[cycle])
    for cycle in sequence:
        hub.begin_cycle_snapshot(cycle)
        model.sample(cycle)
    name = scenario_bin_name("both", ((0, 6), (0, 7), (1, 0), (1, 1), (1, 2)))
    assert sink.key_hit(GROUP, name, coverpoint="cross_both")
    assert not hub._icache_data_ecc_ibuffer_state.pending
    assert "_icache_data_ecc_ibuffer_state" not in model.__dict__


def test_coverage_probes_use_shared_read_once_snapshot_and_record_resolved_alias(tmp_path):
    from env.funcov.py.icache.icache_data_ecc_ibuffer_funcov import Probes

    class Pin:
        reads = 0

        @property
        def value(self):
            self.reads += 1
            return self.reads

    hub = FrontendFuncovSampleHub.from_pilot_csv(default_pilot_csv_path(),
        testcase_name="data_ecc_snapshot", artifact_tag="unit", output_dir=tmp_path)
    pin = Pin()
    hub.attach(SimpleNamespace(dut=SimpleNamespace(reset=pin)))
    probes = Probes(hub)
    hub.begin_cycle_snapshot(10)
    assert probes.read("reset") == probes.read("reset") == 1
    assert pin.reads == 1
    assert probes.paths["reset"] == "reset"
    hub.begin_cycle_snapshot(11)
    assert probes.read("reset") == 2


def test_missing_required_probe_is_not_defaulted_to_a_clean_condition():
    definitions = [SimpleNamespace(bin_id="BIN-1140", coverage_group=GROUP)]
    values = {names[0]: 0 for names in SIGNALS.values()}
    recorder = SimpleNamespace(definitions=definitions, coverage_targets={"bin_ids": ["BIN-1140"]},
        env=SimpleNamespace(dut=None), _read_first_dut_signal=lambda dut, names: values.get(names[0]))
    validate_target_probes(recorder)
    del values[SIGNALS["ecc_enable"][0]]
    with pytest.raises(AssertionError, match="ecc_enable"):
        validate_target_probes(recorder)
