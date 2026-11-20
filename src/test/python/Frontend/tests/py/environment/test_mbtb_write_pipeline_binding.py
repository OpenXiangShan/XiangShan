"""Register-edge binding is exact; stale or invalid payloads never qualify."""
from dataclasses import replace
import importlib.util
import json
from pathlib import Path
from types import SimpleNamespace

import pytest

from env.support.bpu_v3_contract import (
    BpuV3SignalUnavailable, MbtbWriteBufferEvent, MbtbWriteEntry,
)
from tests.py.support.mbtb_training_request import (
    TRAINING_REQUEST_FIELDS, sample_mbtb_write_launch,
)
from tests.py.support.mbtb_write_program import build_mbtb_write_compare_program


_ROOT = "Frontend_top.Frontend.inner_bpu.mbtb.alignBanks_0."
_BANK = _ROOT + "internalBanks_1."


def _sample_inputs(*, target=None, port=3):
    _, _, facts = build_mbtb_write_compare_program()
    target = facts["initial_target"] if target is None else target
    fields = dict(valid=1, need_write=1, mispredict_valid=1,
                  start_pc=facts["source_pc"] >> 1, position=facts["position"],
                  target=target >> 1, branch_type=3, ras_action=0)
    values = {_ROOT + TRAINING_REQUEST_FIELDS[key]: value for key, value in fields.items()}
    values[_BANK + "io_writeEntry_req_valid"] = 1
    for index in range(4):
        suffix = "" if index == 0 else f"_{index}"
        values[_BANK + "writeValid" + suffix] = int(index == port)
        values[_BANK + "valid" + suffix] = int(index == port)
    incoming = MbtbWriteEntry(facts["set_idx"], facts["tag"], 3, 0, facts["position"],
                             1, (target >> 1) & ((1 << 20) - 1))
    event = MbtbWriteBufferEvent(0, 1, port, 3, 0, 0, incoming, incoming, {})
    return facts, values, event


def _launch(values, cycle=10):
    return sample_mbtb_write_launch(values.get, cycle=cycle, align_bank=0, internal_bank=1)


@pytest.mark.parametrize("port", range(4))
def test_exact_preceding_valid_request_qualifies_each_registered_port(port):
    facts, values, event = _sample_inputs(port=port)
    launch = _launch(values)
    request = launch.bind_event(event, facts, event_cycle=11)
    assert request.target == facts["initial_target"] and launch.cycle == 10
    assert launch.source_values == values
    assert launch.signal_paths["bank_write_valid"] == _BANK + "io_writeEntry_req_valid"


def test_current_invalid_payload_is_never_substituted_for_the_real_previous_request():
    facts, values, event = _sample_inputs()
    previous = _launch(values)
    current_values = dict(values)
    current_values[_ROOT + "io_write_req_valid"] = 0
    current_values[_BANK + "io_writeEntry_req_valid"] = 0
    current_values[_BANK + "writeValid_3"] = current_values[_BANK + "valid_3"] = 0
    current = _launch(current_values, cycle=11)
    assert previous.bind_event(event, facts, event_cycle=11).valid == 1
    with pytest.raises(AssertionError):
        current.bind_event(event, facts, event_cycle=12)


@pytest.mark.parametrize("event_cycle", [9, 10, 12, 30])
def test_matching_payload_at_wrong_edge_never_becomes_a_launch(event_cycle):
    facts, values, event = _sample_inputs()
    with pytest.raises(AssertionError, match="exactly the previous"):
        _launch(values).bind_event(event, facts, event_cycle=event_cycle)


@pytest.mark.parametrize("suffix", ["io_write_req_valid", "io_write_req_bits_needWrite",
                                    "io_write_req_bits_mispredictInfo_valid"])
def test_invalid_upstream_request_cannot_qualify_even_when_payload_matches(suffix):
    facts, values, event = _sample_inputs()
    values[_ROOT + suffix] = 0
    with pytest.raises(AssertionError):
        _launch(values).bind_event(event, facts, event_cycle=11)


@pytest.mark.parametrize("suffix", ["io_writeEntry_req_valid", "writeValid_3", "valid_3"])
def test_wrong_bank_gating_or_flush_only_cannot_supply_training_payload(suffix):
    facts, values, event = _sample_inputs()
    values[_BANK + suffix] = 0
    with pytest.raises(AssertionError):
        _launch(values).bind_event(event, facts, event_cycle=11)


@pytest.mark.parametrize("field,value", [("align_bank", 1), ("internal_bank", 2), ("port", 2)])
def test_event_cannot_mix_a_different_bank_or_port(field, value):
    facts, values, event = _sample_inputs()
    with pytest.raises(AssertionError):
        _launch(values).bind_event(replace(event, **{field: value}), facts, event_cycle=11)


@pytest.mark.parametrize("field,value", [("set_idx", 1), ("tag", 32769), ("position", 2),
                                        ("target_lower", 2050), ("target_carry", 0),
                                        ("branch_type", 2), ("ras_action", 1)])
def test_registered_entry_cannot_mix_a_different_source_payload(field, value):
    facts, values, event = _sample_inputs()
    mixed = replace(event, incoming=replace(event.incoming, **{field: value}))
    with pytest.raises(AssertionError):
        _launch(values).bind_event(mixed, facts, event_cycle=11)


def test_payload_from_newer_request_cannot_be_borrowed_for_previous_launch():
    facts, values, equal_event = _sample_inputs()
    _, changed_values, changed_event = _sample_inputs(target=facts["changed_target"])
    previous, newer = _launch(values, 10), _launch(changed_values, 11)
    with pytest.raises(AssertionError):
        previous.bind_event(changed_event, facts, event_cycle=11)
    with pytest.raises(AssertionError):
        newer.bind_event(equal_event, facts, event_cycle=12)


def test_full_target_alias_cannot_reuse_the_same_stored_low_bits():
    facts, values, event = _sample_inputs()
    values[_ROOT + TRAINING_REQUEST_FIELDS["target"]] += 1 << 20
    with pytest.raises(AssertionError):
        _launch(values).bind_event(event, facts, event_cycle=11)


@pytest.mark.parametrize("suffix", ["io_writeEntry_req_valid", "writeValid_3", "valid_3"])
def test_native_launch_gates_cannot_fall_back_to_toggle_history(suffix):
    facts, values, event = _sample_inputs()
    previous = values.pop(_BANK + suffix)
    values[_BANK + "__Vtogcov__" + suffix] = previous
    with pytest.raises(BpuV3SignalUnavailable):
        _launch(values)


def test_native_launch_probes_are_present_in_the_frozen_inventory():
    from env.runtime.pylib import frontend_pylib_path
    _, values, _ = _sample_inputs()
    names = set()
    for line in (frontend_pylib_path() / "Frontend/Frontend_offset.yaml").read_text().splitlines():
        if line.startswith("  - name: "):
            value = line[10:]
            names.add(json.loads(value) if value.startswith('"') else value)
    assert set(values) <= names


def test_launch_delay_and_payload_mux_match_frozen_scala_and_emitted_rtl():
    from env.runtime.pylib import frontend_build_root_path
    root = frontend_build_root_path().parent
    scala = (root / "src/main/scala/xiangshan/frontend/bpu/mbtb/MainBtbInternalBank.scala").read_text()
    rtl = (root / "build-frontend/rtl/MainBtbInternalBank.sv").read_text()
    assert "bufWrite.valid := RegNext(valid, false.B)" in scala
    assert "val writeValid = writeEntry.req.valid && writeEntry.req.bits.wayMask(i)" in scala
    assert "bufWrite.bits.entry := RegEnable(" in scala
    for port in range(4):
        suffix = "" if port == 0 else f"_{port}"
        assert f"entryWriteBuffer_io_write_{port}_valid_REG <= valid{suffix};" in rtl
        assert f".io_write_{port}_valid" in rtl
        assert f"writeValid{suffix} ? io_writeEntry_req_bits_entry_targetLowerBits : 20'h0;" in rtl


def _run_real_canary_body(monkeypatch, *, invalidate_previous=False):
    """Only exercise orchestration with explicit synthetic samples, no DUT."""
    path = Path(__file__).parents[1] / "jiabowen/test_bpu_v3_negative_dut.py"
    spec = importlib.util.spec_from_file_location("staged_registered_mbtb_canary", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    facts, initial_values, equal_event = _sample_inputs()
    _, changed_values, changed_event = _sample_inputs(target=facts["changed_target"])
    changed_event = replace(changed_event, stored=equal_event.incoming)
    idle = dict(initial_values)
    idle[_ROOT + "io_write_req_valid"] = idle[_BANK + "io_writeEntry_req_valid"] = 0
    idle[_BANK + "writeValid_3"] = idle[_BANK + "valid_3"] = 0
    first = dict(initial_values)
    if invalidate_previous:
        first[_ROOT + "io_write_req_valid"] = 0
    launch_values = {1: first, 2: idle, 3: changed_values, 4: idle, 5: idle}
    trace = SimpleNamespace(cursor=0, entries=[
        SimpleNamespace(instr=facts["source_instruction"], kind="jump_indirect", target_pc=target)
        for target in (facts["initial_target"], facts["initial_target"], facts["changed_target"])
    ])
    backend = SimpleNamespace(_cfvec_queue=[], _ftq_start_pc_cache={1: facts["source_pc"]},
                              set_golden_trace=lambda value: None,
                              set_can_accept=lambda value: None, get_stats=lambda: {})
    monitor = SimpleNamespace(clear=lambda: None, set_expected_pc=lambda pc: None,
                              get_errors=lambda: [])
    recorder = SimpleNamespace(risk_observations=[], _try_read_dut_signal=lambda dut, name: None)
    outputs = []
    env = SimpleNamespace(load_program=lambda payload, base: None,
                          initialize=lambda **kwargs: None, monitor=monitor,
                          set_bp_ctrl_enable=lambda **kwargs: None, backend_model=backend,
                          functional_coverage=recorder, dut=None, current_cycle=0,
                          get_errors=lambda: [], _emit_event=lambda key, value: outputs.append(value))
    recorder.env = env

    def step(count):
        env.current_cycle += count
        trace.cursor = min(env.current_cycle, 3)
        index = trace.cursor - 1
        backend._cfvec_queue = [SimpleNamespace(
            pc=facts["source_pc"], instr=facts["source_instruction"],
            golden_match_state="matched", golden_index=index,
            is_rvc=False, is_cfi=True, golden_target_pc=trace.entries[index].target_pc,
            ftq_flag=0, ftq_value=1, ftq_offset=1, cycle=env.current_cycle)]

    env.step = step
    monkeypatch.setattr(module, "build_mbtb_write_compare_program", lambda base: (b"", trace, facts))
    monkeypatch.setattr(module, "sample_mbtb_write_launch", lambda reader, **args: _launch(
        launch_values[args["cycle"]], args["cycle"]))
    monkeypatch.setattr(module, "sample_mbtb_write_buffer_events", lambda reader: (
        (equal_event,) if env.current_cycle == 2 else (changed_event,) if env.current_cycle == 4 else ()))
    monkeypatch.setattr(module, "read_mbtb_write_buffer_dirty", lambda reader, event: (
        int(event.semantic_changed), "synthetic.orchestration_only.dirty"))
    return module, env, outputs


def test_real_canary_uses_valid_previous_launch_when_current_request_is_invalid(monkeypatch):
    module, env, outputs = _run_real_canary_body(monkeypatch)
    module.test_mbtb_write_buffer_compare_bits_semantics_canary(env)
    assert outputs[-1]["semantic_equal_cycles"] == [2]
    assert outputs[-1]["semantic_change_cycles"] == [4]
    assert [item["native_launch"]["cycle"] for item in outputs[-1]["observations"]] == [1, 3]
    assert outputs[-1]["exact_ftq_occurrence_match_proven"] is False


def test_real_canary_rejects_invalid_previous_request_despite_identical_payload(monkeypatch):
    module, env, outputs = _run_real_canary_body(monkeypatch, invalidate_previous=True)
    with pytest.raises(AssertionError):
        module.test_mbtb_write_buffer_compare_bits_semantics_canary(env)
    assert outputs == []
