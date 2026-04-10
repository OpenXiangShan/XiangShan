from __future__ import annotations

from typing import Iterable, Sequence

from .sequences import (
    CheckPcSequence,
    InitializeFrontendSequence,
    InjectRedirectSequence,
    LoadGoldenTraceSequence,
    LoadProgramFileSequence,
    LoadProgramSequence,
    ResetFrontendSequence,
    RunUntilCommitSequence,
    RunUntilGoldenTraceCompleteSequence,
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


def initialize_frontend(env, reset_vector=0x80000000, bare_mode=True, reset_cycles=20, step_cycles=0):
    return InitializeFrontendSequence(
        reset_vector=int(reset_vector),
        bare_mode=bool(bare_mode),
        reset_cycles=int(reset_cycles),
        step_cycles=int(step_cycles),
    ).run(env)


def reset_frontend(env, reset_cycles=1, step_cycles=0):
    return ResetFrontendSequence(
        reset_cycles=int(reset_cycles),
        step_cycles=int(step_cycles),
    ).run(env)


def load_program(env, image: ProgramImage, step_cycles=0) -> int:
    return LoadProgramSequence(image, step_cycles=int(step_cycles)).run(env)


def load_program_file(env, path, base_addr, step_cycles=0) -> int:
    return LoadProgramFileSequence(
        path=str(path),
        base_addr=int(base_addr),
        step_cycles=int(step_cycles),
    ).run(env)


def load_golden_trace(env, source: GoldenTraceSource, step_cycles=0) -> int:
    return LoadGoldenTraceSequence(source=source, step_cycles=int(step_cycles)).run(env)


def run_until_commit(env, target: CommitTarget) -> int:
    return RunUntilCommitSequence(target).run(env)


def run_until_golden_trace_complete(
    env,
    max_cycles=10000,
    progress_interval=0,
    stall_snapshot_interval=0,
    logger=None,
    current_golden_pc_getter=None,
    format_optional_pc=None,
    stall_snapshot_capture=None,
    stall_snapshot_formatter=None,
) -> bool:
    return RunUntilGoldenTraceCompleteSequence(
        max_cycles=int(max_cycles),
        progress_interval=int(progress_interval),
        stall_snapshot_interval=int(stall_snapshot_interval),
        logger=logger,
        current_golden_pc_getter=current_golden_pc_getter,
        format_optional_pc=format_optional_pc,
        stall_snapshot_capture=stall_snapshot_capture,
        stall_snapshot_formatter=stall_snapshot_formatter,
    ).run(env)


def inject_redirect(env, txn: RedirectTxn) -> bool:
    return InjectRedirectSequence(txn=txn).run(env)


def check_pc_sequence(env, expectation: PcSequenceExpectation) -> bool:
    return CheckPcSequence(expectation).run(env)
