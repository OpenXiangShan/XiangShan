from __future__ import annotations

from typing import Iterable, Sequence

from .sequences import (
    CheckPcSequence,
    InjectRedirectSequence,
    LoadGoldenTraceSequence,
    LoadProgramFileSequence,
    LoadProgramSequence,
    RunUntilCommitSequence,
)
from .transactions import (
    BpCtrlConfig,
    CommitTarget,
    GoldenTraceSource,
    PcSequenceExpectation,
    ProgramImage,
    RedirectTxn,
)


def _instructions_to_bytes(instructions: Iterable[int]) -> bytes:
    buf = bytearray()
    for instr in instructions:
        val = int(instr) & 0xFFFFFFFF
        buf.extend(val.to_bytes(4, "little"))
    return bytes(buf)


def normalize_program_image(bin_data, base_addr) -> ProgramImage:
    if isinstance(bin_data, (bytes, bytearray)):
        payload = bytes(bin_data)
    else:
        payload = _instructions_to_bytes(bin_data)
    return ProgramImage(payload=payload, base_addr=int(base_addr))


def normalize_golden_trace_source(path) -> GoldenTraceSource:
    return GoldenTraceSource(path=str(path))


def normalize_commit_target(target_count, max_cycles) -> CommitTarget:
    return CommitTarget(target_count=int(target_count), max_cycles=int(max_cycles))


def normalize_redirect_txn(target_pc, reason, max_cycles) -> RedirectTxn:
    return RedirectTxn(target_pc=int(target_pc), reason=str(reason), max_cycles=int(max_cycles))


def normalize_pc_sequence_expectation(expected_pcs: Sequence[int], max_cycles) -> PcSequenceExpectation:
    return PcSequenceExpectation(expected_pcs=tuple(int(x) for x in expected_pcs), max_cycles=int(max_cycles))


def normalize_bp_ctrl_config(
    ubtb_enable=1,
    abtb_enable=1,
    mbtb_enable=1,
    tage_enable=1,
    sc_enable=1,
    ittage_enable=1,
) -> BpCtrlConfig:
    return BpCtrlConfig(
        ubtb_enable=1 if int(ubtb_enable) else 0,
        abtb_enable=1 if int(abtb_enable) else 0,
        mbtb_enable=1 if int(mbtb_enable) else 0,
        tage_enable=1 if int(tage_enable) else 0,
        sc_enable=1 if int(sc_enable) else 0,
        ittage_enable=1 if int(ittage_enable) else 0,
    )


def load_program(env, image: ProgramImage) -> int:
    return LoadProgramSequence(image).run(env)


def load_program_file(env, path, base_addr) -> int:
    return LoadProgramFileSequence(path=str(path), base_addr=int(base_addr)).run(env)


def load_golden_trace(env, source: GoldenTraceSource, step_cycles=0) -> int:
    return LoadGoldenTraceSequence(source=source, step_cycles=int(step_cycles)).run(env)


def run_until_commit(env, target: CommitTarget) -> int:
    return RunUntilCommitSequence(target).run(env)


def inject_redirect(env, txn: RedirectTxn, redirect_delay_cycles=0) -> bool:
    return InjectRedirectSequence(
        txn=txn,
        redirect_delay_cycles=int(redirect_delay_cycles),
    ).run(env)


def check_pc_sequence(env, expectation: PcSequenceExpectation) -> bool:
    return CheckPcSequence(expectation).run(env)
