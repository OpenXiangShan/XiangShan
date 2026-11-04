"""Uncache response scenarios shared by directed tests."""

from __future__ import annotations

from typing import Sequence

from tests.py.support.dut_support import _assert_observed_instrs, _wait_for_resp_delta
from tests.py.support.uncache_scenarios import (
    _ADDI_X0_X0_0,
    _CROSS_PAGE_PC,
    _FETCH_BLOCK_SIZE,
    _IFU_UNCACHE_TO_UNCACHE_VALID_SIGNALS,
    _JAL_X0_PLUS_4,
    _MMIO_BASE,
    _NORMAL_BASE,
    _NORMAL_PHYS_BASE,
    _SV39_PAGE_SIZE,
    _UNCACHE_BEAT_BYTES,
    _configure_exec_cacheable_pma_4k,
    _configure_exec_pmp_4k,
    _force_redirect_to,
    _initialize_mmio_fetch,
    _initialize_sv39_fetch,
    _pending_uncache_samples,
    _prepare_cross_page_rvi_stream,
    _prepare_sv39_mapped_pbmt_nc_cfi_stream,
    _register_prev_half_rvi_observer,
    _require_first_dut_signal,
    _wait_for_observed_pc,
    _wait_for_observed_pc_sequence,
    _wait_for_request_addr,
    _wait_for_resp_count,
    _wait_for_uncache_a_valid_addr,
)


def _half_rvi_state_is_quiescent(sample: dict) -> bool:
    return (
        int(sample["s0"]) == 0
        and int(sample["s1_data"]) == 0
        and int(sample["s1_pc"]) == 0
        and (int(sample["s1"]) == 0 or int(sample["s1_pipeline_valid"]) == 0)
        and (int(sample["s2"]) == 0 or int(sample["s2_valid"]) == 0)
    )


def _wait_for_first_active_s1_after(
    env,
    samples: Sequence[dict],
    cycle: int,
    *,
    max_cycles: int = 64,
) -> dict | None:
    for _ in range(int(max_cycles)):
        active = next(
            (
                sample
                for sample in samples
                if int(sample["cycle"]) > int(cycle)
                and int(sample["s1_pipeline_valid"]) == 1
            ),
            None,
        )
        if active is not None:
            return active
        env.step(1)
    return None


def _run_uncache_pbmt_nc_non_mmio_uses_uncache_path(env):
    expected_block, mapping = _prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=_NORMAL_BASE,
        paddr=_NORMAL_PHYS_BASE,
    )
    expected_block_pcs = [pc for pc, _, _ in expected_block]
    cfi_pc = mapping.vaddr + 0x10
    _initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    _configure_exec_pmp_4k(env, base_addr=mapping.paddr)
    _configure_exec_cacheable_pma_4k(env, base_addr=mapping.paddr)
    env.backend_model.set_can_accept(0)
    env.uncache_agent.set_a_ready(0)
    _force_redirect_to(env, mapping.vaddr)

    assert _wait_for_resp_delta(env, env.ptw_agent, max_cycles=6000), env.ptw_agent.get_stats()
    assert _wait_for_uncache_a_valid_addr(env, mapping.paddr, max_cycles=6000), {
        "ptw": env.ptw_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
        "mapping": mapping,
        "a_valid": int(env.uncache_if.a_valid.value),
        "a_addr": hex(int(env.uncache_if.a_bits_address.value)),
    }
    assert _require_first_dut_signal(env, _IFU_UNCACHE_TO_UNCACHE_VALID_SIGNALS) == 1
    env.uncache_agent.set_a_ready(None)
    assert _wait_for_request_addr(env, mapping.paddr, max_cycles=6000), env.uncache_agent.get_stats()
    assert _wait_for_resp_count(env, 1, max_cycles=6000), env.uncache_agent.get_stats()
    env.backend_model.set_can_accept(1)
    assert _wait_for_observed_pc_sequence(env, expected_block_pcs, max_cycles=12000), {
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-16:]],
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
        "mapping": mapping,
    }
    stats = env.uncache_agent.get_stats()
    expected_beat_addrs = [mapping.paddr + _UNCACHE_BEAT_BYTES * idx for idx in range(_FETCH_BLOCK_SIZE // _UNCACHE_BEAT_BYTES)]

    assert not env.memory.is_mmio(mapping.paddr)
    assert mapping.paddr in stats.get("request_addrs", [])
    assert all(addr in stats.get("request_addrs", []) for addr in expected_beat_addrs), stats
    _assert_observed_instrs(env, expected_block)
    observed_by_pc = {int(obs.pc): obs for obs in env.monitor.observations if int(obs.pc) in expected_block_pcs}
    assert int(observed_by_pc[cfi_pc].instr) == _JAL_X0_PLUS_4
    assert not bool(observed_by_pc[cfi_pc].is_rvc)
    assert int(env.ptw_agent.get_stats().get("resp_count", 0)) >= 1
    assert not env.monitor.get_errors()


def _run_uncache_page_tail_rvi_need_resend_rechecks_next_page(
    env,
    *,
    second_latency: int = 16,
    redirect_target: int = _MMIO_BASE + 0x40,
):
    _prepare_cross_page_rvi_stream(env)
    # Keep the second-page response outstanding long enough to observe the
    # retimed half-RVI state, rather than proving only the final cfVec result.
    env.uncache_agent.configure(latency=2, mmio_latency=second_latency)
    prev_half_samples = _register_prev_half_rvi_observer(env)
    _initialize_mmio_fetch(env, reset_vector=_CROSS_PAGE_PC)

    first_beat = _CROSS_PAGE_PC & ~(_UNCACHE_BEAT_BYTES - 1)
    next_page = _MMIO_BASE + _SV39_PAGE_SIZE
    assert _wait_for_request_addr(env, first_beat, max_cycles=5000), env.uncache_agent.get_stats()
    assert _wait_for_request_addr(env, next_page, max_cycles=5000), env.uncache_agent.get_stats()
    assert _wait_for_observed_pc(env, _CROSS_PAGE_PC, max_cycles=8000), {
        "observed": [(int(obs.pc), int(obs.instr), bool(obs.is_rvc)) for obs in env.monitor.observations[-16:]],
        "uncache": env.uncache_agent.get_stats(),
    }
    stats = env.uncache_agent.get_stats()
    observed = next(obs for obs in env.monitor.observations if int(obs.pc) == _CROSS_PAGE_PC)

    assert stats.get("request_addrs", []).count(first_beat) == 1
    assert stats.get("request_addrs", []).count(next_page) == 1
    assert int(observed.instr) == _ADDI_X0_X0_0
    assert not bool(observed.is_rvc)

    recovery_samples = list(prev_half_samples)
    expected_half_data = _ADDI_X0_X0_0 & 0xFFFF
    expected_half_pc = _CROSS_PAGE_PC >> 1
    pending_samples = _pending_uncache_samples(recovery_samples, next_page)
    assert len(pending_samples) >= 2, {
        "reason": "no multi-cycle next-page response stall was observed",
        "pending_samples": pending_samples,
        "uncache": stats,
    }
    pending_cycles = [int(sample["cycle"]) for sample in pending_samples]
    assert any(
        current == previous + 1
        for previous, current in zip(pending_cycles, pending_cycles[1:])
    ), {"pending_cycles": pending_cycles}

    need_resend_samples = [
        sample
        for sample in recovery_samples
        if sample["last_request_addr"] == first_beat and int(sample["need_resend"]) == 1
    ]
    assert need_resend_samples, {
        "reason": "first-page RVI response did not expose needResend",
        "samples": recovery_samples[-16:],
    }

    transaction_samples = [
        sample
        for sample in recovery_samples
        if sample["last_request_addr"] in {first_beat, next_page}
    ]
    assert any(sample["s0"] == 1 for sample in transaction_samples), {
        "reason": "s0 never recorded the first-page RVI half",
        "samples": transaction_samples[-16:],
    }
    first_need_resend_cycle = min(int(sample["cycle"]) for sample in need_resend_samples)
    s1_recovery = [
        sample
        for sample in recovery_samples
        if int(sample["cycle"]) >= first_need_resend_cycle
        and sample["s1"] == 1
        and int(sample["s1_data"]) == expected_half_data
        and int(sample["s1_pc"]) == expected_half_pc
    ]
    assert s1_recovery, {
        "reason": "s1 did not capture the first-page half-RVI payload",
        "samples": recovery_samples[-16:],
        "expected_half_data": expected_half_data,
        "expected_half_pc": expected_half_pc,
    }
    s2_recovery = [sample for sample in pending_samples if sample["s2"] == 1]
    assert s2_recovery, {
        "reason": "s2 never carried prevHalfRvi during the next-page response",
        "pending_samples": pending_samples,
    }

    # The delayed next-page response must hold the assembled instruction's
    # source half and PC on every cycle until the response is consumed.
    for sample in pending_samples:
        assert int(sample["s2"]) == 1, {"sample": sample, "reason": "s2 half-RVI flag dropped while stalled"}
        assert int(sample["s2_data"]) == expected_half_data, {
            "sample": sample,
            "expected_half_data": expected_half_data,
        }
        assert int(sample["s2_pc"]) == expected_half_pc, {
            "sample": sample,
            "expected_half_pc": expected_half_pc,
        }

    # Exercise backend redirect after recovery and make sure the old saved
    # half cannot leak into the redirected path.
    observations_before_redirect = len(env.monitor.observations)
    redirect_queued_cycle = int(env.current_cycle)
    _force_redirect_to(env, redirect_target)
    assert _wait_for_observed_pc(env, redirect_target, max_cycles=5000)
    redirect_samples = [
        sample for sample in prev_half_samples if int(sample["cycle"]) >= redirect_queued_cycle
    ]
    redirect_cycles = [
        int(sample["cycle"])
        for sample in redirect_samples
        if int(sample["backend_redirect"]) == 1
    ]
    assert redirect_cycles, {
        "reason": "backend redirect was not observable",
        "samples": redirect_samples[-16:],
    }
    first_redirect_cycle = min(redirect_cycles)
    target_delivery_cycle = min(
        int(obs.cycle)
        for obs in env.monitor.observations[observations_before_redirect:]
        if int(obs.pc) == redirect_target
    )
    redirect_clear_window = [
        sample
        for sample in redirect_samples
        if first_redirect_cycle <= int(sample["cycle"]) <= target_delivery_cycle
    ]
    # S2 payload registers are RegEnable state; redirect invalidates them via
    # s2_valid rather than requiring the physically stale bits to be zero.
    assert any(_half_rvi_state_is_quiescent(sample) for sample in redirect_clear_window), {
        "reason": "backend redirect did not clear saved half-RVI payload",
        "redirect_clear_window": redirect_clear_window,
    }
    first_recovery_s1 = _wait_for_first_active_s1_after(
        env,
        prev_half_samples,
        first_redirect_cycle,
    )
    assert first_recovery_s1 is not None, {
        "reason": "redirect target never reached an active S1 transaction",
        "samples": prev_half_samples[-16:],
    }
    assert int(first_recovery_s1["s1"]) == 0, {
        "reason": "redirect target inherited the old half-RVI valid state",
        "sample": first_recovery_s1,
    }
    assert not any(
        int(obs.pc) == _CROSS_PAGE_PC
        for obs in env.monitor.observations[observations_before_redirect:]
    )
    assert not env.monitor.get_errors()
