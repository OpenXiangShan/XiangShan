"""Real-DUT proof for the cumulative correct taken JAL/JALR/RET leaf."""

from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.funcov.py.ifu.compact_funcov import _read_ifu_internal_with_path, _read_predchecker_with_path
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.sequences import LoadProgramSequence
from env.support.pc_utils import fold_pc
from tests.py.jiabowen.test_ifu_predchecker_v3_dut import _BASE, _CNOP, _jal_x0


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_AUIPC_X6_ZERO = 0x00000317
_JALR_X0_X6_ZERO = 0x00030067
_JALR_X0_X1_ZERO = 0x00008067


def _jal(rd: int, offset: int) -> int:
    assert 0 <= int(rd) < 32
    assert int(offset) % 2 == 0
    assert -(1 << 20) <= int(offset) < (1 << 20)
    imm = int(offset) & 0x1FFFFF
    return (
        (((imm >> 20) & 1) << 31)
        | (((imm >> 1) & 0x3FF) << 21)
        | (((imm >> 11) & 1) << 20)
        | (((imm >> 12) & 0xFF) << 12)
        | (int(rd) << 7)
        | 0x6F
    )


def _program() -> tuple[bytes, dict[str, int]]:
    """A -> B (JALR) -> C (CALL) -> D (RET) -> C+4 -> A.

    The x6 base is not a link register. No injected predictor state or
    coverage-dependent path change is used to create a taken observation.
    """
    a = _BASE + 0x100
    b = _BASE + 0x140
    c = _BASE + 0x300
    d = _BASE + 0x340
    payload = bytearray(_CNOP.to_bytes(2, "little") * 512)

    def put32(address: int, instruction: int) -> None:
        offset = int(address) - _BASE
        payload[offset : offset + 4] = int(instruction).to_bytes(4, "little")

    put32(_BASE, _AUIPC_X6_ZERO)
    put32(_BASE + 4, 0x30030313)  # addi x6,x6,0x300
    put32(_BASE + 8, _jal_x0(a - (_BASE + 8)))
    put32(a, _jal_x0(b - a))
    put32(b, _JALR_X0_X6_ZERO)
    put32(c, _jal(1, d - c))
    put32(c + 4, _jal_x0(a - (c + 4)))
    put32(d, _JALR_X0_X1_ZERO)
    return bytes(payload), {"a": a, "b": b, "c": c, "d": d}


def _architectural_trace(laps: int = 1024) -> GoldenTrace:
    """Fixed ISA oracle: x6=base+0x300, CALL writes x1=C+4, RET uses x1.

    Never derive a golden target or path from the DUT or its coverage state.
    A separate mini-interpreter contract test checks this oracle against bytes.
    """
    entries = [TraceEntry(0, _BASE, _AUIPC_X6_ZERO, 4),
               TraceEntry(1, _BASE + 4, 0x30030313, 4),
               TraceEntry(2, _BASE + 8, _jal(0, 0xF8), 4, "jump", True, _BASE + 0x100)]
    for _ in range(laps):
        for offset, instruction, kind, target in (
            (0x100, _jal(0, 0x40), "jump", 0x140),
            (0x140, _JALR_X0_X6_ZERO, "jump_indirect", 0x300),
            (0x300, _jal(1, 0x40), "call", 0x340),
            (0x340, _JALR_X0_X1_ZERO, "ret", 0x304),
            (0x304, _jal(0, -0x204), "jump", 0x100),
        ):
            entries.append(TraceEntry(len(entries), _BASE + offset, instruction,
                                      4, kind, True, _BASE + target))
    return GoldenTrace(entries)


@pytest.mark.funcov_bins("BIN-932")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_ifu_correct_taken_jal_jalr_ret_accumulate_in_one_run(env) -> None:
    payload, addresses = _program()
    LoadProgramSequence(
        image=ProgramImage(payload=payload, base_addr=_BASE),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    recorder = env.functional_coverage
    trace = _architectural_trace()
    env.backend_model.set_golden_trace(trace)
    paths, checkpoints, checked_arch = {}, {}, set()
    indirect_resolves = []
    queued_indirect_resolves = {}
    jalr_requests = []
    pending = []

    def read(stem, *, pred=False):
        reader = _read_predchecker_with_path if pred else _read_ifu_internal_with_path
        value, path = reader(recorder, env.dut, stem)
        assert value is not None, {"missing_probe": stem, "predchecker": pred}
        paths[("pred." if pred else "ifu.") + stem] = path
        return int(value)

    def observe(cycle, _env):
        nonlocal pending
        if pending:
            assert all(cycle == item["cycle"] + 1 for item in pending)
            assert read("io_resp_stage2Out_checkerRedirect_valid", pred=True) == 0
            assert read("io_toFtq_wbRedirect_valid") == 0
            for item in pending:
                checkpoints.setdefault(item["kind"], {**item, "no_redirect_cycle": cycle})
            pending = []
        for entry in env.backend_model._cfvec_queue:
            if entry.golden_index is not None and entry.path_state == "correct":
                golden = trace.entries[entry.golden_index]
                assert entry.pc == golden.pc and entry.instr == golden.instr
                checked_arch.add(entry.golden_index)
        if read("s2_fire") != 1 or read("s2_flush") != 0:
            return
        entries = []
        for slot in range(35):
            prefix = f"s2_alignedInstrVec_{slot}_"
            valid, invalid = read(prefix + "valid"), read(prefix + "invalidTaken")
            if valid or invalid:
                entries.append(dict(slot=slot, valid=valid, invalid=invalid,
                    taken=read(prefix + "isPredTaken"),
                    branch=read(f"s2_alignedPdInfoVec_{slot}_brAttribute_branchType"),
                    pc=read(f"s2_alignedInstrPcVec_{slot}_addr") << 1))
        if len(jalr_requests) < 16:
            for entry in entries:
                if entry["pc"] == addresses["b"]:
                    jalr_requests.append({"cycle": cycle, **entry})
        # No fault anywhere in this request: not merely a good slot alongside
        # a younger fault. The next cycle checks the actual registered output.
        if not entries or any(e["invalid"] or
                (e["valid"] and e["branch"] in (2, 3) and not e["taken"]) or
                (e["valid"] and e["branch"] == 0 and e["taken"]) for e in entries):
            return
        for e in entries:
            expected = {
                addresses["a"]: ("jal", _jal(0, 0x40), 2, 0),
                addresses["b"]: ("jalr", _JALR_X0_X6_ZERO, 3, 0),
                addresses["d"]: ("ret", _JALR_X0_X1_ZERO, 3, 1),
            }.get(e["pc"])
            if not (expected and e["valid"] and e["taken"]):
                continue
            kind, instruction, branch, ras = expected
            slot = e["slot"]
            prefix = f"s2_alignedInstrVec_{slot}_"
            assert read(prefix + "data") == instruction and e["branch"] == branch
            assert read(f"s2_alignedPdInfoVec_{slot}_brAttribute_rasAction") == ras
            owner = read(prefix + "blockSel") | read(prefix + "isCrossBlockInstr")
            ftq = [read(f"s2_fetchBlock_{owner}_ftqIdx_{field}") for field in ("flag", "value")]
            assert read("io_toIBuffer_valid") == read("io_toIBuffer_ready") == 1
            assert (read("io_toIBuffer_bits_enqEnable") >> slot) & 1
            assert (read("s2_fixedInstrValid") >> slot) & 1
            assert read(f"io_toIBuffer_bits_instrs_{slot}") == instruction
            assert read(f"io_toIBuffer_bits_foldpc_{slot}") == fold_pc(e["pc"])
            assert [read(f"io_toIBuffer_bits_ftqPtr_{slot}_{field}")
                    for field in ("flag", "value")] == ftq
            assert read(f"io_toIBuffer_bits_instrEndOffset_{slot}_offset") == read(prefix + "endOffset")
            pending.append(dict(cycle=cycle, kind=kind, pc=e["pc"], instr=instruction,
                                slot=slot, ftq=ftq, effective_owner=owner))

    def observe_resolve(cycle, _env):
        for entry in env.backend_model._pending_resolves:
            if entry.branch_type != 3:
                continue
            identity = f"{entry.queued_cycle}:{entry.ftq_flag}:{entry.ftq_value}:{entry.ftq_offset}"
            if identity not in queued_indirect_resolves and len(queued_indirect_resolves) >= 32:
                continue
            record = queued_indirect_resolves.setdefault(identity, {
                "inst_pc": entry.inst_pc, "pc": entry.pc, "target": entry.target,
                "queued_cycle": entry.queued_cycle, "ready_cycle": entry.ready_cycle,
                "ftq_flag": entry.ftq_flag, "ftq_value": entry.ftq_value,
                "ftq_offset": entry.ftq_offset, "ras_action": entry.ras_action,
                "first_mispredict": bool(entry.mispredict),
            })
            record.update(last_seen_cycle=cycle, last_mispredict=bool(entry.mispredict),
                          golden_frontier=env.backend_model.current_golden_pc(),
                          target_progressed=env.backend_model._target_path_progressed_after_cycle(
                              entry.target, entry.queued_cycle))
        for channel in range(3):
            def port(stem):
                name = f"io_backend_toFtq_resolve_{channel}_" + stem
                signal = getattr(env.dut, name, None)
                assert signal is not None, {"missing_port": name}
                paths[name] = name
                return int(signal.value)

            if port("valid") and port("bits_attribute_branchType") == 3 and len(indirect_resolves) < 32:
                record = {stem: port("bits_" + stem) for stem in (
                    "pc_addr", "target_addr", "mispredict", "taken", "ftqIdx_flag",
                    "ftqIdx_value", "ftqOffset", "attribute_rasAction",
                )}
                record["cycle"] = cycle
                indirect_resolves.append(record)

    env.register_cycle_observer(observe)
    env.register_pre_drive_cycle_observer(observe_resolve)
    for index in range(int(os.getenv("IFU_BIN932_MAX_CYCLES", "3000"))):
        env.step(1)
        if index % 512 == 511:
            env.logger.info("BIN-932 cycle=%d cursor=%d kinds=%s checked=%s", env.current_cycle,
                trace.cursor, sorted(getattr(recorder, "_ifu_owner_correct_cfi_kinds", set())),
                sorted(checkpoints))
        if (set(checkpoints) == {"jal", "jalr", "ret"} and len(checked_arch) >= 103
                and recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_034")):
            break
    evidence = {
        "event": "ifu_bin932_oracle_checkpoint", "cycle": env.current_cycle,
        "checkpoints": checkpoints, "signal_paths": paths, "trace_cursor": trace.cursor,
        "checked_arch_count": len(checked_arch),
        "indirect_resolves": indirect_resolves,
        "queued_indirect_resolves": queued_indirect_resolves,
        "jalr_requests": jalr_requests,
        "correct_cfi_kinds": sorted(
            getattr(recorder, "_ifu_owner_correct_cfi_kinds", set())
        ),
        "correct_jalr_forms": sorted(
            getattr(recorder, "_ifu_owner_correct_jalr_forms", set())
        ),
        "branch_stats": env.backend_model.get_stats(),
    }
    recorder.risk_observations.append(evidence)
    env.logger.info("BIN-932 final checkpoint: %s", {k: v for k, v in evidence.items() if k != "signal_paths"})
    assert recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_034"), evidence
    assert set(checkpoints) == {"jal", "jalr", "ret"} and len(checked_arch) >= 103, evidence
    assert not env.monitor.get_errors()
    assert not env.get_errors()
