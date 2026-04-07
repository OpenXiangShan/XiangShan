from env.backend_model import BackendModel
from env.trace import GoldenTrace, TraceEntry


class _Pin:
    def __init__(self, value=0):
        self.value = int(value)


class _DummyDut:
    def __init__(self):
        self._pins = {}

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        pin = self._pins.get(name)
        if pin is None:
            pin = _Pin(0)
            self._pins[name] = pin
        return pin


def _beq(rs1: int, rs2: int, offset: int) -> int:
    imm = offset & 0x1FFF
    return (
        (((imm >> 12) & 0x1) << 31)
        | (((imm >> 5) & 0x3F) << 25)
        | (rs2 << 20)
        | (rs1 << 15)
        | (((imm >> 1) & 0xF) << 8)
        | (((imm >> 11) & 0x1) << 7)
        | 0x63
    )


def _drive_one_cfvec_branch(dut, pc: int, instr: int, pred_taken: int) -> None:
    dut.io_backend_cfVec_0_valid.value = 1
    dut.io_backend_cfVec_0_bits_pc.value = int(pc)
    dut.io_backend_cfVec_0_bits_instr.value = int(instr)
    dut.io_backend_cfVec_0_bits_isRvc.value = 0
    dut.io_backend_cfVec_0_bits_predTaken.value = int(pred_taken)
    dut.io_backend_cfVec_0_bits_ftqPtr_flag.value = 0
    dut.io_backend_cfVec_0_bits_ftqPtr_value.value = 0
    dut.io_backend_cfVec_0_bits_ftqOffset.value = 0
    dut.io_backend_cfVec_0_bits_isLastInFtqEntry.value = 1


def _advance_to_cycle(bm: BackendModel, cycle: int) -> None:
    for cur_cycle in range(1, int(cycle) + 1):
        bm.on_clock_edge(cur_cycle)


def test_resolve_mispredict_from_golden_trace_branch_taken_mismatch():
    base = 0x80000000
    instr = _beq(0, 0, 8)  # always taken

    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0)
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(
                    index=0,
                    pc=base,
                    instr=instr,
                    size=4,
                    kind="branch",
                    taken=True,
                    target_pc=base + 8,
                )
            ]
        )
    )
    dut.io_backend_fromFtq_wen.value = 1
    dut.io_backend_fromFtq_ftqIdx.value = 0
    dut.io_backend_fromFtq_startPc_addr.value = base >> 1

    _drive_one_cfvec_branch(dut, pc=base, instr=instr, pred_taken=0)
    bm.on_clock_edge(0)
    dut.io_backend_cfVec_0_valid.value = 0

    resolve_ready_cycle = bm._pending_resolves[0].ready_cycle
    _advance_to_cycle(bm, resolve_ready_cycle)

    assert int(dut.io_backend_toFtq_resolve_0_valid.value) == 1
    assert int(dut.io_backend_toFtq_resolve_0_bits_taken.value) == 1
    assert int(dut.io_backend_toFtq_resolve_0_bits_pc_addr.value) == (base >> 1)
    assert int(dut.io_backend_toFtq_resolve_0_bits_target_addr.value) == ((base + 8) >> 1)
    assert int(dut.io_backend_toFtq_resolve_0_bits_mispredict.value) == 1


def test_resolve_mispredict_defaults_to_zero_without_golden_trace():
    base = 0x80000000
    instr = _beq(0, 0, 8)

    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0)
    bm.bind(dut)

    _drive_one_cfvec_branch(dut, pc=base, instr=instr, pred_taken=0)
    bm.on_clock_edge(0)

    assert int(dut.io_backend_toFtq_resolve_0_valid.value) == 1
    assert int(dut.io_backend_toFtq_resolve_0_bits_mispredict.value) == 0


def test_redirect_keeps_pc_and_target_as_full_byte_addresses():
    base = 0x80000000
    target = base + 0x40

    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)

    bm.inject_redirect(target, "unit_mispredict", delay_cycles=0)
    bm.on_clock_edge(0)

    redirect_ready_cycle = bm.pending_events[0].ready_cycle
    _advance_to_cycle(bm, redirect_ready_cycle)

    assert int(dut.io_backend_toFtq_redirect_valid.value) == 1
    assert int(dut.io_backend_toFtq_redirect_bits_pc.value) == target
    assert int(dut.io_backend_toFtq_redirect_bits_target.value) == target


def test_inject_redirect_clears_pending_resolves():
    base = 0x80000000
    instr = _beq(0, 0, 8)

    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=5, resolve_max_delay=5)
    bm.bind(dut)

    _drive_one_cfvec_branch(dut, pc=base, instr=instr, pred_taken=1)
    bm.on_clock_edge(0)

    assert len(bm._pending_resolves) == 1

    bm.inject_redirect(base + 0x40, "unit_mispredict", delay_cycles=0)

    assert len(bm._pending_resolves) == 0


def test_resolve_pc_addr_preserves_cached_ftq_start_pc_byte_address():
    base = 0x80000000
    branch_pc = base + 0x20
    instr = _beq(0, 0, 8)

    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0)
    bm.bind(dut)

    dut.io_backend_fromFtq_wen.value = 1
    dut.io_backend_fromFtq_ftqIdx.value = 0
    dut.io_backend_fromFtq_startPc_addr.value = base >> 1

    dut.io_backend_cfVec_0_valid.value = 1
    dut.io_backend_cfVec_0_bits_pc.value = int(branch_pc)
    dut.io_backend_cfVec_0_bits_instr.value = int(instr)
    dut.io_backend_cfVec_0_bits_isRvc.value = 0
    dut.io_backend_cfVec_0_bits_predTaken.value = 1
    dut.io_backend_cfVec_0_bits_ftqPtr_flag.value = 0
    dut.io_backend_cfVec_0_bits_ftqPtr_value.value = 0
    dut.io_backend_cfVec_0_bits_ftqOffset.value = 0x10
    dut.io_backend_cfVec_0_bits_isLastInFtqEntry.value = 1

    bm.on_clock_edge(0)

    assert int(dut.io_backend_toFtq_resolve_0_valid.value) == 1
    assert int(dut.io_backend_toFtq_resolve_0_bits_pc_addr.value) == (base >> 1)
