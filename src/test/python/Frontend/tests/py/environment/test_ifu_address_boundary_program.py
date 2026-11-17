"""Decode actual fetched bytes independently of the generated GoldenTrace."""

from types import SimpleNamespace

import pytest

from env.agents.backend_agent import BackendAgent
from env.funcov.py.ifu.compact_funcov import _decode_pruned_pc
from tests.py.jiabowen import test_ifu_address_boundary_v3_dut as actual
from tests.py.environment.test_ifu_address_native_witnesses import _runtime, _cycle, _output, _PREFIX


_MASK64 = (1 << 64) - 1


def _execute_two_rv64_instructions(payload, initial_pc):
    # Independent ISA decoding: U-immediate sign extension and JALR register
    # dataflow. No testcase constants or GoldenTrace targets feed execution.
    regs = [0] * 32
    pc = int(initial_pc)
    steps = []
    for offset in (0, 4):
        instr = int.from_bytes(payload[offset:offset + 4], "little")
        opcode = instr & 0x7f
        rd = (instr >> 7) & 31
        if opcode in (0x37, 0x17):
            immediate = instr & 0xfffff000
            if immediate & 0x80000000:
                immediate -= 1 << 32
            value = immediate if opcode == 0x37 else pc + immediate
            if rd:
                regs[rd] = value & _MASK64
            next_pc = (pc + 4) & _MASK64
        elif opcode == 0x67 and ((instr >> 12) & 7) == 0:
            rs1 = (instr >> 15) & 31
            immediate = instr >> 20
            if immediate & 0x800:
                immediate -= 1 << 12
            next_pc = ((regs[rs1] + immediate) & _MASK64) & ~1
            if rd:
                regs[rd] = (pc + 4) & _MASK64
        else:
            raise AssertionError(f"unsupported source instruction: {instr:08x}")
        steps.append({"pc": pc, "instr": instr, "x6": regs[6], "next_pc": next_pc})
        regs[0] = 0
        pc = next_pc
    return regs, steps


def _physical_page(monkeypatch):
    loaded = []
    class Load:
        def __init__(self, *, image, step_cycles):
            assert step_cycles == 0
            loaded.append(image)
        def run(self, _env):
            pass
    monkeypatch.setattr(actual, "LoadProgramSequence", Load)
    actual._load_nop_page(object(), actual._LOW_PHYS_BASE, low_source=True)
    assert len(loaded) == 1 and len(loaded[0].payload) == 4096
    assert loaded[0].base_addr == actual._LOW_PHYS_BASE
    return loaded[0].payload


def test_actual_source_bytes_compute_full_high_architectural_target(monkeypatch):
    payload = _physical_page(monkeypatch)
    regs, steps = _execute_two_rv64_instructions(payload, actual._LOW_BASE)
    assert payload[:8].hex() == "37e3ffff67000300"
    assert [item["pc"] for item in steps] == [0x1000, 0x1004]
    assert regs[6] == steps[-1]["next_pc"] == 0xffffffffffffe000
    assert regs[0] == 0
    assert actual._HIGH_ARCH_PC == steps[-1]["next_pc"]


def test_golden_target_and_eight_high_instructions_follow_real_register_result(monkeypatch):
    payload = _physical_page(monkeypatch)
    _, execution = _execute_two_rv64_instructions(payload, actual._LOW_BASE)
    guarded = BackendAgent._encode_guarded_pc(execution[-1]["next_pc"])
    pruned = BackendAgent._encode_backend_addr(guarded)
    normalized = _decode_pruned_pc(pruned)
    trace = actual._boundary_trace().entries
    assert trace[0].pc == 0x1000 and trace[1].pc == 0x1004
    assert [entry.instr for entry in trace[:2]] == [item["instr"] for item in execution]
    assert trace[1].target_pc == trace[2].pc == normalized == actual._HIGH_BASE
    assert [entry.pc for entry in trace[2:10]] == [normalized + 4 * i for i in range(8)]
    assert all(entry.instr == 0x13 and entry.size == 4 for entry in trace[2:10])


def test_full_guarded_pruned_and_vaddr_representations_use_official_helpers():
    assert BackendAgent._PC_BITS == actual._ADDRESS_BITS == 50
    assert actual._HIGH_ARCH_PC == 0xffffffffffffe000
    assert actual._HIGH_GUARDED_PC == 0x7ffffffffe000
    assert actual._HIGH_PRUNED_ADDR == 0x3fffffffff000
    assert actual._HIGH_BASE == 0x3ffffffffe000
    assert actual._HIGH_PRUNED_ADDR.bit_length() == 50
    assert actual._HIGH_GUARDED_PC.bit_length() == 51
    assert _decode_pruned_pc(actual._HIGH_PRUNED_ADDR) == actual._HIGH_BASE
    assert actual._HIGH_BASE > (1 << 35)


def test_native_witness_preserves_full_guarded_and_pruned_source_probes(tmp_path):
    hub, env, dut, model = _runtime(tmp_path)
    _output(dut, actual._HIGH_BASE)
    dut.set(_PREFIX + "s2_fetchBlock_0_startVAddr_addr", actual._HIGH_PRUNED_ADDR)
    dut.set(_PREFIX + "s2_alignedInstrPcVec_0_addr", actual._HIGH_PRUNED_ADDR)
    _cycle(hub, env, model, 1)
    witness = hub.toffee_sink._ifu_owner_address_boundary_witnesses["high"]
    assert witness["start_pc_pruned_addr"] == 0x3fffffffff000
    assert witness["start_pc_guarded"] == 0x7ffffffffe000
    assert witness["start_pc"] == 0x3ffffffffe000
    assert witness["slots"][0]["pc"] == 0x3ffffffffe000
    assert all(witness["checks"].values())


def test_old_auipc_source_computes_low_target_and_cannot_justify_high_golden():
    payload = bytes.fromhex("1703000067000300")
    regs, steps = _execute_two_rv64_instructions(payload, 0x1000)
    assert regs[6] == steps[-1]["next_pc"] == 0x1000
    assert steps[-1]["next_pc"] != actual._HIGH_ARCH_PC


@pytest.mark.parametrize("lui", [0xffffd337, 0xffffe3b7, 0x00000337])
def test_altered_source_bytes_cannot_reuse_high_golden_target(lui):
    payload = int(lui).to_bytes(4, "little") + int(actual._JALR_X0_X6_ZERO).to_bytes(4, "little")
    _, steps = _execute_two_rv64_instructions(payload, 0x1000)
    assert steps[-1]["next_pc"] != actual._HIGH_ARCH_PC


@pytest.mark.parametrize("condition", ["mismatch", "no_cacheable_request", "timeout"])
def test_actual_witness_waiter_preserves_failure_instead_of_skipping(condition):
    events = []
    class Coverage:
        def _try_read_dut_signal(self, _dut, _name):
            return None
    def step(_cycles):
        if condition == "mismatch":
            raise AssertionError("first mismatch has no attributable CFI")
    env = SimpleNamespace(
        functional_coverage=Coverage(), dut=object(), step=step,
        monitor=SimpleNamespace(get_errors=lambda: []),
        icache_agent=SimpleNamespace(get_stats=lambda: {"req_count": int(condition != "no_cacheable_request")}),
        backend_model=SimpleNamespace(get_stats=lambda: {}),
        ptw_agent=SimpleNamespace(get_stats=lambda: {}),
        _emit_event=lambda name, payload: events.append((name, payload)),
    )
    with pytest.raises(AssertionError):
        actual._wait_for_boundary_witness(env, "low", max_cycles=1)
    assert events and events[-1][0] == "ifu.bin900_address_boundary_blocked"
