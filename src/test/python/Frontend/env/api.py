from __future__ import annotations

import logging
from typing import Sequence

from .env_config import DEFAULT_ENV_CONFIG
from .logging_utils import configure_env_logging, parse_log_level
from .nemu_trace_pipeline import generate_nemu_trace_from_bin
from .request_apis import (
    check_pc_sequence,
    inject_redirect,
    load_golden_trace,
    load_program,
    load_program_file,
    normalize_bp_ctrl_config,
    normalize_commit_target,
    normalize_golden_trace_source,
    normalize_pc_sequence_expectation,
    normalize_program_image,
    normalize_redirect_txn,
    run_until_commit,
)


logger = logging.getLogger("env.api")


def _env_config(env):
    return getattr(env, "config", DEFAULT_ENV_CONFIG)


def api_Frontend_load_program(env, bin_data, base_addr, max_cycles=1000):
    """Load a program image into the env memory model."""
    configure_env_logging()
    image = normalize_program_image(bin_data=bin_data, base_addr=base_addr)
    written = int(load_program(env, image))
    logger.info("api load program: base=0x%x size=%d", image.base_addr, len(image.payload))
    env.step(1)
    return written


def api_Frontend_load_program_file(env, path, base_addr, max_cycles=1000):
    """Load a program image from a binary file into the env memory model."""
    configure_env_logging()
    size = int(load_program_file(env, path, base_addr))
    logger.info("api load program file: path=%s base=0x%x size=%d", str(path), int(base_addr), size)
    env.step(1)
    return size


def api_Frontend_load_golden_trace(env, path, max_cycles=0) -> int:
    """Load a golden trace file and attach it to backend model comparison."""
    configure_env_logging()
    source = normalize_golden_trace_source(path)
    count = int(load_golden_trace(env, source, step_cycles=max_cycles))
    logger.info("api load golden trace: path=%s entries=%d", source.path, count)
    return count


def api_Frontend_prepare_program_and_nemu_trace(
    env,
    bin_path,
    base_addr,
    trace_output_path,
    nemu_exec_path=None,
    nemu_log_path=None,
    nemu_max_instr=0,
    trace_limit=0,
    load_trace=1,
    max_cycles=0,
) -> dict:
    """Load real bin into DUT env, run NEMU to build trace json/jsonl, optionally load trace."""
    configure_env_logging()
    env_config = _env_config(env)
    nemu_image_base_addr = int(env_config.trace.nemu_image_base_addr)
    if int(base_addr) != nemu_image_base_addr:
        raise ValueError(
            "api_Frontend_prepare_program_and_nemu_trace requires "
            f"base_addr=0x{nemu_image_base_addr:x} to match the current NEMU image base, "
            f"got 0x{int(base_addr):x}"
        )
    bin_size = int(api_Frontend_load_program_file(env, str(bin_path), int(base_addr), max_cycles=0))
    trace_out = generate_nemu_trace_from_bin(
        bin_path=str(bin_path),
        trace_output_path=str(trace_output_path),
        nemu_exec_path=(None if not nemu_exec_path else str(nemu_exec_path)),
        nemu_log_path=(None if not nemu_log_path else str(nemu_log_path)),
        nemu_max_instr=int(nemu_max_instr),
        trace_limit=int(trace_limit),
    )
    trace_entries = int(trace_out["trace_entries"])

    loaded_trace_entries = 0
    if int(load_trace):
        loaded_trace_entries = int(load_golden_trace(env, normalize_golden_trace_source(trace_output_path), step_cycles=0))

    if int(max_cycles) > 0:
        env.step(int(max_cycles))

    out = {
        "bin_path": str(bin_path),
        "bin_size": int(bin_size),
        "base_addr": int(base_addr),
        "nemu_exec_path": str(trace_out["nemu_exec_path"]),
        "nemu_log_path": str(trace_out["nemu_log_path"]),
        "trace_output_path": str(trace_out["trace_output_path"]),
        "trace_entries": int(trace_entries),
        "loaded_trace_entries": int(loaded_trace_entries),
    }
    logger.info(
        "api prepare program and nemu trace: bin=%s base=0x%x bin_size=%d trace_entries=%d loaded_trace=%d",
        str(bin_path),
        int(base_addr),
        int(bin_size),
        int(trace_entries),
        int(loaded_trace_entries),
    )
    return out


def api_Frontend_run_until_commit(env, target_count, max_cycles=10000) -> int:
    """Run simulation until target commits are observed."""
    configure_env_logging()
    target = normalize_commit_target(target_count=target_count, max_cycles=max_cycles)
    if target.target_count < 0:
        raise ValueError("target_count must be >= 0")
    got = int(run_until_commit(env, target))
    logger.info("api run until commit: target=%d got=%d max_cycles=%d", target.target_count, got, target.max_cycles)
    return got


def api_Frontend_inject_redirect(env, target_pc, reason, max_cycles=1000) -> bool:
    """Inject a redirect event and wait until monitor sees target PC."""
    configure_env_logging()
    env_config = _env_config(env)
    txn = normalize_redirect_txn(target_pc=target_pc, reason=reason, max_cycles=max_cycles)
    logger.info(
        "api inject redirect: target=0x%x reason=%s max_cycles=%d",
        txn.target_pc,
        txn.reason,
        txn.max_cycles,
    )
    ok = inject_redirect(env, txn, redirect_delay_cycles=env_config.sequence.redirect_delay_cycles)
    if ok:
        logger.info("api inject redirect success: target=0x%x", txn.target_pc)
    else:
        logger.warning("api inject redirect timeout: target=0x%x", txn.target_pc)
    return ok


def api_Frontend_check_pc_sequence(env, expected_pcs: Sequence[int], max_cycles=5000) -> bool:
    """Check if expected PC sequence appears in monitor observations in order."""
    configure_env_logging()
    expectation = normalize_pc_sequence_expectation(expected_pcs=expected_pcs, max_cycles=max_cycles)
    ok = check_pc_sequence(env, expectation)
    if ok:
        logger.info("api check pc sequence success: count=%d", len(expectation.expected_pcs))
    else:
        logger.warning("api check pc sequence timeout: expected=%d", len(expectation.expected_pcs))
    return ok


def api_Frontend_get_branch_stats(env) -> dict:
    """Return branch stats from branch checker."""
    return env.branch_checker.get_stats()


def api_Frontend_set_log_level(env, level, max_cycles=0) -> int:
    """Set testbench env logging level, e.g. DEBUG/INFO/WARNING/ERROR."""
    value = parse_log_level(str(level))
    configure_env_logging(str(level))
    if hasattr(env, "set_log_level"):
        env.set_log_level(str(level))
    if int(max_cycles) > 0:
        env.step(int(max_cycles))
    logger.info("api set log level: %s", str(level).upper())
    return int(value)


def api_Frontend_set_bp_ctrl_enable(
    env,
    ubtb_enable=1,
    abtb_enable=1,
    mbtb_enable=1,
    tage_enable=1,
    sc_enable=1,
    ittage_enable=1,
    max_cycles=0,
) -> dict:
    """Set io_csrCtrl_bp_ctrl_*Enable bits."""
    configure_env_logging()
    bp_cfg = normalize_bp_ctrl_config(
        ubtb_enable=ubtb_enable,
        abtb_enable=abtb_enable,
        mbtb_enable=mbtb_enable,
        tage_enable=tage_enable,
        sc_enable=sc_enable,
        ittage_enable=ittage_enable,
    )
    cfg = env.set_bp_ctrl_enable(
        ubtb_enable=bp_cfg.ubtb_enable,
        abtb_enable=bp_cfg.abtb_enable,
        mbtb_enable=bp_cfg.mbtb_enable,
        tage_enable=bp_cfg.tage_enable,
        sc_enable=bp_cfg.sc_enable,
        ittage_enable=bp_cfg.ittage_enable,
    )
    if int(max_cycles) > 0:
        env.step(int(max_cycles))
    logger.info("api set bp ctrl enable: %s", cfg)
    return cfg


def api_Frontend_enable_fst_dump(env, fst_path=None, max_cycles=1):
    """Enable FST waveform dump and optionally set waveform path."""
    path = env.enable_fst_dump(fst_path)
    if int(max_cycles) > 0:
        env.step(1)
    return path


def api_Frontend_pause_fst_dump(env, max_cycles=1) -> bool:
    """Pause FST waveform dump."""
    ok = env.pause_waveform_dump()
    if int(max_cycles) > 0:
        env.step(1)
    return ok


def api_Frontend_flush_fst_dump(env, max_cycles=1) -> bool:
    """Flush FST waveform file buffers to disk."""
    ok = env.flush_waveform()
    if int(max_cycles) > 0:
        env.step(1)
    return ok
