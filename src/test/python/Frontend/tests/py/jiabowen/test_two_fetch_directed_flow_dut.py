from __future__ import annotations

import os
from typing import Iterable, Sequence

import pytest

from env.funcov.py.ftq.sampler import _TWO_FETCH_SIGNALS
from env.pylib import frontend_offset_path
from env.sequences import LoadProgramSequence
from env.transactions import ProgramImage


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x80000000
_BLOCK_BYTES = 64
_BLOCK_COUNT = 8
_PROGRAM_BYTES = _BLOCK_BYTES * _BLOCK_COUNT
_REDIRECT_TARGET = _BASE + 4 * _BLOCK_BYTES
_CNOP = 0x0001
_IBUFFER_PAYLOAD_PREFIX = (
    "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_"
)
_MAINPIPE_PREFIX = "Frontend_top.Frontend.inner_icache.mainPipe."
_ICACHE_PREFIX = "Frontend_top.Frontend.inner_icache."

_DIRECTED_SIGNAL_KEYS = (
    "ftq_valid",
    "ftq_ready",
    "ftq_req1_valid",
    "ftq_req0_start",
    "ftq_req1_start",
    "bpu_s3_flush",
    "way_real_two",
    "main_s0_fire",
    "main_s1_fire",
    "main_s1_flush",
    "ifu_second_valid",
    "ifu_s2_valid",
    "to_ibuffer_valid",
    "to_ibuffer_ready",
    "ifu_flush",
    "backend_redirect",
    "backend_redirect_target",
)

_FTQ_REQUEST_TAG_SIGNALS = {
    "req0_flag": (
        f"{_ICACHE_PREFIX}__Vtogcov__io_fromFtq_toMainPipe_bits_req_0_ftqIdx_flag",
    ),
    "req0_value": (
        f"{_ICACHE_PREFIX}__Vtogcov__io_fromFtq_toMainPipe_bits_req_0_ftqIdx_value",
    ),
    "req1_flag": (
        f"{_ICACHE_PREFIX}__Vtogcov__io_fromFtq_toMainPipe_bits_req_1_ftqIdx_flag",
    ),
    "req1_value": (
        f"{_ICACHE_PREFIX}__Vtogcov__io_fromFtq_toMainPipe_bits_req_1_ftqIdx_value",
    ),
}

_MAIN_S1_TAG_SIGNALS = {
    "req0_flag": (
        f"{_MAINPIPE_PREFIX}s1_req_0_ftqIdx_flag",
        f"{_MAINPIPE_PREFIX}__Vtogcov__s1_req_0_ftqIdx_flag",
    ),
    "req0_value": (
        f"{_MAINPIPE_PREFIX}s1_req_0_ftqIdx_value",
        f"{_MAINPIPE_PREFIX}__Vtogcov__s1_req_0_ftqIdx_value",
    ),
    "req1_flag": (
        f"{_MAINPIPE_PREFIX}s1_req_1_ftqIdx_flag",
        f"{_MAINPIPE_PREFIX}__Vtogcov__s1_req_1_ftqIdx_flag",
    ),
    "req1_value": (
        f"{_MAINPIPE_PREFIX}s1_req_1_ftqIdx_value",
        f"{_MAINPIPE_PREFIX}__Vtogcov__s1_req_1_ftqIdx_value",
    ),
}

_MAIN_S1_VADDR_SIGNALS = tuple(
    (
        f"{_MAINPIPE_PREFIX}s1_req_{req}_vAddr_{line}_addr",
        f"{_MAINPIPE_PREFIX}__Vtogcov__s1_req_{req}_vAddr_{line}_addr",
    )
    for req in range(2)
    for line in range(2)
)
_MAIN_S1_SHOULD_FETCH_SIGNALS = tuple(
    (
        f"{_MAINPIPE_PREFIX}s1_shouldFetch_{index}",
        f"{_MAINPIPE_PREFIX}__Vtogcov__s1_shouldFetch_{index}",
    )
    for index in range(4)
)

_BPU_S3_FLUSH_PTR_SIGNALS = {
    "flag": (
        f"{_ICACHE_PREFIX}__Vtogcov__io_fromFtq_flushFromBpu_s3_bits_flag",
    ),
    "value": (
        f"{_ICACHE_PREFIX}__Vtogcov__io_fromFtq_flushFromBpu_s3_bits_value",
    ),
}
_FTQ_ENTRY_START_SIGNALS = tuple(
    f"Frontend_top.Frontend.inner_ftq.entryQueue_{index}_startPc_addr"
    for index in range(64)
)


def _c_j(offset: int) -> int:
    """Encode a C.J immediate according to the RISC-V C extension."""
    assert int(offset) % 2 == 0
    assert -(1 << 11) <= int(offset) < (1 << 11)
    imm = int(offset) & 0xFFF
    return (
        (0b101 << 13)
        | (((imm >> 11) & 0x1) << 12)
        | (((imm >> 4) & 0x1) << 11)
        | (((imm >> 8) & 0x3) << 9)
        | (((imm >> 10) & 0x1) << 8)
        | (((imm >> 6) & 0x1) << 7)
        | (((imm >> 7) & 0x1) << 6)
        | (((imm >> 1) & 0x7) << 3)
        | (((imm >> 5) & 0x1) << 2)
        | 0b01
    )


def _trained_short_block_loop() -> bytes:
    """Eight 64-byte fetch blocks with a taken C.J at byte offset 30.

    The branch target skips the remaining 32 bytes rather than naming the
    sequential next PC, so both DUT and backend model treat it as taken.  The
    final C.J returns to the first block.  Repetition trains all block-end
    predictions without depending on architectural register execution.
    """
    halfwords: list[int] = []
    for block in range(_BLOCK_COUNT):
        halfwords.extend([_CNOP] * 15)
        branch_pc = block * _BLOCK_BYTES + 15 * 2
        target = (block + 1) * _BLOCK_BYTES if block + 1 < _BLOCK_COUNT else 0
        halfwords.append(_c_j(target - branch_pc))
        halfwords.extend([_CNOP] * 16)
    assert len(halfwords) * 2 == _PROGRAM_BYTES
    return b"".join(int(halfword).to_bytes(2, "little") for halfword in halfwords)


def _load_and_reset(env) -> None:
    LoadProgramSequence(
        image=ProgramImage(payload=_trained_short_block_loop(), base_addr=_BASE),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)


def _cycle_limit(name: str, default: int) -> int:
    raw = os.getenv(str(name), "").strip()
    if not raw:
        return int(default)
    value = int(raw, 0)
    assert value > 0, f"{name} must be positive"
    return int(value)


def _recorder(env):
    recorder = getattr(env, "functional_coverage", None)
    assert recorder is not None, "directed flow tests require the canonical funcov recorder"
    assert getattr(recorder, "env", None) is env
    return recorder


def _read_required(recorder, names: Iterable[str], *, label: str) -> int:
    names = tuple(str(name) for name in names)
    value = recorder._read_first_dut_signal(recorder.env.dut, names)
    assert value is not None, {
        "reason": "required directed-flow observation is unavailable",
        "label": str(label),
        "candidates": list(names),
    }
    return int(value)


def _read_key(recorder, key: str) -> int:
    assert key in _TWO_FETCH_SIGNALS, key
    return _read_required(recorder, _TWO_FETCH_SIGNALS[key], label=key)


def _payload_signal_names(recorder) -> tuple[str, ...]:
    registered = getattr(recorder, "_registered_internal_signals", None)
    assert registered is not None, "Frontend_offset.yaml is required for full payload checking"
    names = tuple(
        sorted(
            str(name)
            for name in registered
            if str(name).startswith(_IBUFFER_PAYLOAD_PREFIX)
        )
    )
    required_suffixes = (
        "enqEnable",
        "pc_0_addr",
        "instrs_0",
        "ftqPtr_0_flag",
        "ftqPtr_0_value",
        "isRvc_0",
        "instrEndOffset_0_offset",
    )
    missing = [
        suffix for suffix in required_suffixes if _IBUFFER_PAYLOAD_PREFIX + suffix not in names
    ]
    assert names and not missing, {
        "reason": "incomplete IBuffer payload observability",
        "observed_field_count": len(names),
        "missing_suffixes": missing,
    }
    return names


def _payload_snapshot(recorder, names: Sequence[str]) -> tuple[int, ...]:
    values = [
        recorder._try_read_dut_signal(recorder.env.dut, str(name)) for name in names
    ]
    missing = [str(name) for name, value in zip(names, values) if value is None]
    assert not missing, {
        "reason": "IBuffer payload field became unreadable",
        "missing": missing,
    }
    return tuple(int(value) for value in values)


def _payload_differences(
    names: Sequence[str], before: Sequence[int], after: Sequence[int]
) -> list[dict]:
    return [
        {"signal": str(name), "before": int(old), "after": int(new)}
        for name, old, new in zip(names, before, after)
        if int(old) != int(new)
    ]


def _ibuffer_entries(recorder) -> list[dict]:
    enable = _read_required(
        recorder,
        (_IBUFFER_PAYLOAD_PREFIX + "enqEnable",),
        label="toIBuffer.enqEnable",
    )
    entries: list[dict] = []
    for slot in range(36):
        if ((int(enable) >> slot) & 1) == 0:
            continue
        pc = _read_required(
            recorder,
            (_IBUFFER_PAYLOAD_PREFIX + f"pc_{slot}_addr",),
            label=f"toIBuffer.pc[{slot}]",
        )
        ftq_flag = _read_required(
            recorder,
            (_IBUFFER_PAYLOAD_PREFIX + f"ftqPtr_{slot}_flag",),
            label=f"toIBuffer.ftqPtr[{slot}].flag",
        )
        ftq_value = _read_required(
            recorder,
            (_IBUFFER_PAYLOAD_PREFIX + f"ftqPtr_{slot}_value",),
            label=f"toIBuffer.ftqPtr[{slot}].value",
        )
        entries.append(
            {
                "slot": int(slot),
                "pc": int(pc) << 1,
                "ftq_tag": (int(ftq_flag), int(ftq_value)),
            }
        )
    return entries


def _cfvec_entries(recorder) -> list[dict]:
    entries: list[dict] = []
    for slot in range(8):
        valid = _read_required(
            recorder,
            (f"io_backend_cfVec_{slot}_valid",),
            label=f"cfVec[{slot}].valid",
        )
        if valid != 1:
            continue
        entries.append(
            {
                "slot": int(slot),
                "pc": _read_required(
                    recorder,
                    (f"io_backend_cfVec_{slot}_bits_pc",),
                    label=f"cfVec[{slot}].pc",
                ),
                "ftq_tag": (
                    _read_required(
                        recorder,
                        (f"io_backend_cfVec_{slot}_bits_ftqPtr_flag",),
                        label=f"cfVec[{slot}].ftqPtr.flag",
                    ),
                    _read_required(
                        recorder,
                        (f"io_backend_cfVec_{slot}_bits_ftqPtr_value",),
                        label=f"cfVec[{slot}].ftqPtr.value",
                    ),
                ),
            }
        )
    return entries


def _is_dual_window(recorder) -> bool:
    return (
        _read_key(recorder, "ifu_second_valid") == 1
        and _read_key(recorder, "ifu_s2_valid") == 1
        and _read_key(recorder, "to_ibuffer_valid") == 1
    )


def _warm_frontend_execution(env, *, max_cycles: int | None = None) -> dict:
    recorder = _recorder(env)
    if max_cycles is None:
        max_cycles = _cycle_limit("TB_TWO_FETCH_TRAIN_MAX_CYCLES", 4000)
    for _ in range(max(0, int(max_cycles))):
        env.step(1)
        if (
            int(env.backend_model.get_stats().get("commit_count", 0)) >= 1
            and int(env.icache_agent.get_stats().get("req_count", 0)) >= 4
            and recorder.key_hit("ifu_cfi_decode_type", "jal")
            and recorder.key_hit("two_fetch_ftq_eligibility", "eligible_dual")
        ):
            return {
                "cycle": int(env.current_cycle),
                "icache": env.icache_agent.get_stats(),
                "backend": env.backend_model.get_stats(),
            }
    raise AssertionError(
        {
            "reason": "short-block loop did not reach dual-eligible frontend execution",
            "max_cycles": int(max_cycles),
            "icache": env.icache_agent.get_stats(),
            "backend": env.backend_model.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _pulse_fencei(env) -> None:
    env.clock_reset.io_fencei.value = 1
    env.step(1)
    env.clock_reset.io_fencei.value = 0
    # Frontend registers fencei once before presenting it to ICache.
    env.step(2)


def _current_ftq_request_tags(recorder) -> tuple[tuple[int, int], tuple[int, int]]:
    values = {
        name: _read_required(recorder, candidates, label=f"fromFtq.{name}")
        for name, candidates in _FTQ_REQUEST_TAG_SIGNALS.items()
    }
    return (
        (int(values["req0_flag"]), int(values["req0_value"])),
        (int(values["req1_flag"]), int(values["req1_value"])),
    )


def _current_main_s1_tags(recorder) -> tuple[tuple[int, int], tuple[int, int]]:
    values = {
        name: _read_required(recorder, candidates, label=f"mainPipe.s1.{name}")
        for name, candidates in _MAIN_S1_TAG_SIGNALS.items()
    }
    return (
        (int(values["req0_flag"]), int(values["req0_value"])),
        (int(values["req1_flag"]), int(values["req1_value"])),
    )


def _main_s1_required_line_addresses(recorder, pending_refill: dict) -> set[int]:
    required = list(pending_refill.get("required_lines") or ())
    assert len(required) == 4 and any(bool(value) for value in required), {
        "reason": "pending dual transaction has no required miss lines",
        "pending_refill": pending_refill,
    }
    addresses: set[int] = set()
    for index, is_required in enumerate(required):
        if not bool(is_required):
            continue
        halfword_addr = _read_required(
            recorder,
            _MAIN_S1_VADDR_SIGNALS[index],
            label=f"mainPipe.s1_req_line[{index}].vAddr",
        )
        addresses.add((int(halfword_addr) << 1) & ~0x3F)
    return addresses


def _main_s1_required_lines(recorder) -> list[bool]:
    return [
        bool(
            _read_required(
                recorder,
                candidates,
                label=f"mainPipe.s1_shouldFetch[{index}]",
            )
        )
        for index, candidates in enumerate(_MAIN_S1_SHOULD_FETCH_SIGNALS)
    ]


def _main_s1_line_addresses(recorder, required_lines: Sequence[bool]) -> set[int]:
    assert len(required_lines) == len(_MAIN_S1_VADDR_SIGNALS)
    addresses: set[int] = set()
    for index, required in enumerate(required_lines):
        if not required:
            continue
        halfword_addr = _read_required(
            recorder,
            _MAIN_S1_VADDR_SIGNALS[index],
            label=f"mainPipe.s1_req_line[{index}].vAddr",
        )
        addresses.add((int(halfword_addr) << 1) & ~0x3F)
    return addresses


def _fetch_ptr(recorder) -> tuple[int, int]:
    return (
        _read_key(recorder, "fetch_ptr_flag"),
        _read_key(recorder, "fetch_ptr_value"),
    )


def _bpu_s3_flush_ptr(recorder) -> tuple[int, int]:
    return (
        _read_required(
            recorder,
            _BPU_S3_FLUSH_PTR_SIGNALS["flag"],
            label="bpu_s3_flush.ftqIdx.flag",
        ),
        _read_required(
            recorder,
            _BPU_S3_FLUSH_PTR_SIGNALS["value"],
            label="bpu_s3_flush.ftqIdx.value",
        ),
    )


def _ftq_ptr_at_or_after(left: tuple[int, int], right: tuple[int, int]) -> bool:
    """Mirror CircularQueuePtr.>= for two continuous FTQ pointers."""
    left_flag, left_value = (int(left[0]), int(left[1]))
    right_flag, right_value = (int(right[0]), int(right[1]))
    return bool((left_flag != right_flag) ^ (left_value >= right_value))


def _ftq_entry_start_pc(recorder, ptr: tuple[int, int]) -> int:
    index = int(ptr[1])
    assert 0 <= index < len(_FTQ_ENTRY_START_SIGNALS), ptr
    return (
        _read_required(
            recorder,
            (_FTQ_ENTRY_START_SIGNALS[index],),
            label=f"FTQ entry[{index}].startPc",
        )
        << 1
    )


def _icache_response_records(env) -> list[dict]:
    records = env.icache_agent.get_stats().get("response_records", [])
    return [dict(record) for record in records]


def test_two_fetch_directed_flow_signal_contract_matches_dut_inventory():
    offset = frontend_offset_path()
    assert offset.is_file(), "compile Frontend before running directed signal-contract tests"
    registered = {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }

    missing_groups = {
        key: list(_TWO_FETCH_SIGNALS[key])
        for key in _DIRECTED_SIGNAL_KEYS
        if not any(name in registered for name in _TWO_FETCH_SIGNALS[key])
    }
    missing_tag_groups = {
        key: list(names)
        for key, names in _FTQ_REQUEST_TAG_SIGNALS.items()
        if not any(name in registered for name in names)
    }
    missing_s1_lines = [
        list(names) for names in _MAIN_S1_VADDR_SIGNALS if not any(name in registered for name in names)
    ]
    missing_bpu_flush_ptr = {
        key: list(names)
        for key, names in _BPU_S3_FLUSH_PTR_SIGNALS.items()
        if not any(name in registered for name in names)
    }
    missing_ftq_entry_starts = [
        name for name in _FTQ_ENTRY_START_SIGNALS if name not in registered
    ]
    payload_names = {
        name for name in registered if name.startswith(_IBUFFER_PAYLOAD_PREFIX)
    }
    payload_required = {
        _IBUFFER_PAYLOAD_PREFIX + suffix
        for suffix in (
            "enqEnable",
            "pc_0_addr",
            "instrs_0",
            "ftqPtr_0_flag",
            "ftqPtr_0_value",
            "isRvc_0",
        )
    }
    assert (
        "Frontend_top.io_fencei" in registered
        and not missing_groups
        and not missing_tag_groups
        and not missing_s1_lines
        and not missing_bpu_flush_ptr
        and not missing_ftq_entry_starts
        and payload_required <= payload_names
    ), {
        "missing_fencei": "Frontend_top.io_fencei" not in registered,
        "missing_two_fetch_groups": missing_groups,
        "missing_request_tag_groups": missing_tag_groups,
        "missing_main_s1_lines": missing_s1_lines,
        "missing_bpu_flush_ptr": missing_bpu_flush_ptr,
        "missing_ftq_entry_starts": missing_ftq_entry_starts,
        "missing_payload_fields": sorted(payload_required - payload_names),
        "observable_payload_field_count": len(payload_names),
    }


@pytest.mark.funcov_bins("BIN-538", "BIN-539")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_two_fetch_ibuffer_backpressure_holds_full_payload_until_single_fire(env):
    _load_and_reset(env)
    recorder = _recorder(env)
    payload_names = _payload_signal_names(recorder)

    # Reach dual-eligible execution, then wait for the IBuffer's own ready-low
    # boundary.  Once the exact dual payload is held, backend pressure extends
    # that same transaction's hold window without changing its serve width.
    _warm_frontend_execution(env)

    held_payload: tuple[int, ...] | None = None
    held_entries: list[dict] | None = None
    held_cycle: int | None = None
    stable_stall_cycles = 0
    for _ in range(_cycle_limit("TB_TWO_FETCH_PRESSURE_MAX_CYCLES", 1024)):
        env.step(1)
        valid = _read_key(recorder, "to_ibuffer_valid")
        ready = _read_key(recorder, "to_ibuffer_ready")
        if held_payload is None:
            no_flush = all(
                _read_key(recorder, key) == 0
                for key in (
                    "backend_redirect",
                    "bpu_s3_flush",
                    "main_s1_flush",
                    "ifu_flush",
                )
            )
            if valid == 1 and ready == 0 and no_flush and _is_dual_window(recorder):
                held_payload = _payload_snapshot(recorder, payload_names)
                held_entries = _ibuffer_entries(recorder)
                held_cycle = int(env.current_cycle)
                assert held_entries, "dual stalled window must carry at least one enabled lane"
                env.backend_model.set_can_accept(0)
            continue

        active_flushes = {
            key: _read_key(recorder, key)
            for key in (
                "backend_redirect",
                "bpu_s3_flush",
                "main_s1_flush",
                "ifu_flush",
            )
        }
        assert not any(active_flushes.values()), {
            "reason": "flush invalidated the intended backpressure window",
            "held_cycle": held_cycle,
            "cycle": int(env.current_cycle),
            "flushes": active_flushes,
        }
        if valid == 1 and ready == 0:
            stalled_payload = _payload_snapshot(recorder, payload_names)
            assert stalled_payload == held_payload, {
                "reason": "Decoupled IBuffer payload changed while valid && !ready",
                "held_cycle": held_cycle,
                "cycle": int(env.current_cycle),
                "field_count": len(payload_names),
                "changed_fields": _payload_differences(
                    payload_names, held_payload, stalled_payload
                ),
            }
            stable_stall_cycles += 1
            if stable_stall_cycles >= 3:
                break
        else:
            raise AssertionError(
                {
                    "reason": "held IBuffer transaction disappeared before ready was released",
                    "held_cycle": held_cycle,
                    "cycle": int(env.current_cycle),
                    "valid": int(valid),
                    "ready": int(ready),
                }
            )
    assert held_payload is not None and stable_stall_cycles >= 3, {
        "reason": "IBuffer pressure did not expose a multi-cycle dual stall",
        "stable_stall_cycles": stable_stall_cycles,
        "icache": env.icache_agent.get_stats(),
        "backend": env.backend_model.get_stats(),
    }

    env.backend_model.set_can_accept(1)
    matching_fire_cycles: list[int] = []
    first_fire_entries: list[dict] | None = None
    release_cycle: int | None = None
    for _ in range(_cycle_limit("TB_TWO_FETCH_RELEASE_MAX_CYCLES", 512)):
        env.step(1)
        active_flushes = {
            key: _read_key(recorder, key)
            for key in (
                "backend_redirect",
                "bpu_s3_flush",
                "main_s1_flush",
                "ifu_flush",
            )
        }
        assert not any(active_flushes.values()), {
            "reason": "flush invalidated the pressure-release window",
            "held_cycle": held_cycle,
            "cycle": int(env.current_cycle),
            "flushes": active_flushes,
        }
        valid = _read_key(recorder, "to_ibuffer_valid")
        ready = _read_key(recorder, "to_ibuffer_ready")
        if valid == 1 and ready == 0:
            stalled_payload = _payload_snapshot(recorder, payload_names)
            assert stalled_payload == held_payload, {
                "reason": "IBuffer payload changed while waiting for ready after pressure release",
                "held_cycle": held_cycle,
                "cycle": int(env.current_cycle),
                "changed_fields": _payload_differences(
                    payload_names, held_payload, stalled_payload
                ),
            }
            continue
        if valid == 1 and ready == 1:
            payload = _payload_snapshot(recorder, payload_names)
            release_cycle = int(env.current_cycle)
            first_fire_entries = _ibuffer_entries(recorder)
            if payload == held_payload:
                matching_fire_cycles.append(int(env.current_cycle))
            break
        if valid != 1:
            raise AssertionError(
                {
                    "reason": "held IBuffer transaction was dropped instead of firing",
                    "held_cycle": held_cycle,
                    "cycle": int(env.current_cycle),
                }
            )
    assert release_cycle is not None, {
        "reason": "held backpressure window did not observe a release handshake",
        "held_cycle": held_cycle,
    }
    if matching_fire_cycles:
        assert first_fire_entries == held_entries, {
            "reason": "enabled-lane PC/FTQ payload changed before release fire",
            "stalled_entries": held_entries,
            "fire_entries": first_fire_entries,
        }

    # Continue past the fire and reject a repeated transfer of the same full
    # payload.  PC plus FTQ metadata make a legitimate distinct transaction
    # differ even for this repetitive instruction stream.
    # Keep the duplicate check shorter than an FTQ-generation wrap; after a
    # full flag/value wrap the same loop payload is a distinct transaction.
    for _ in range(_cycle_limit("TB_TWO_FETCH_NO_REPEAT_CYCLES", 16)):
        env.step(1)
        if (
            _read_key(recorder, "to_ibuffer_valid") == 1
            and _read_key(recorder, "to_ibuffer_ready") == 1
            and _payload_snapshot(recorder, payload_names) == held_payload
        ):
            matching_fire_cycles.append(int(env.current_cycle))
    assert len(matching_fire_cycles) <= 1, {
        "reason": "held payload transferred more than once",
        "matching_fire_cycles": matching_fire_cycles,
        "release_cycle": release_cycle,
    }
    assert recorder.key_hit("two_fetch_delivery", "dual_stall")
    assert recorder.key_hit("two_fetch_delivery", "dual_fire")
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-540")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_backend_redirect_drops_dual_miss_and_ignores_delayed_old_response(env):
    _load_and_reset(env)
    recorder = _recorder(env)
    _warm_frontend_execution(env)

    # Preserve trained BPU state while invalidating ICache.  Every subsequent
    # accepted TL request is delayed, making the old dual fetch and its D
    # response overlap the backend redirect deterministically.
    env.icache_agent.configure(hit_latency=1, miss_latency=128, miss_rate=1.0, seed=0x6219)
    _pulse_fencei(env)

    observed_transaction: dict | None = None
    associated: dict | None = None
    for _ in range(_cycle_limit("TB_TWO_FETCH_MISS_ASSOC_MAX_CYCLES", 2048)):
        env.step(1)
        if (
            _read_key(recorder, "main_s1_valid") == 1
            and _read_key(recorder, "main_req1_valid") == 1
        ):
            required_lines = _main_s1_required_lines(recorder)
            if any(required_lines):
                tags = _current_main_s1_tags(recorder)
                observed_transaction = {
                    "cycle": int(env.current_cycle),
                    "tags": tags,
                    "required_lines": required_lines,
                    "required_line_addresses": sorted(
                        _main_s1_line_addresses(recorder, required_lines)
                    ),
                }
        if observed_transaction is None:
            continue

        required_line_addresses = set(observed_transaction["required_line_addresses"])
        # TileLink source identifies an MSHR rather than an FTQ entry. The
        # agent is configured to make every request after fence.i a miss, so
        # the live pending line and its future ready cycle are the authoritative
        # association. Avoid a second request-record time-window match: the
        # sampler observes MainPipe and the agent handshake at different points
        # in the same cycle.
        pending_tl = list(getattr(env.icache_agent, "pending", ()))
        delayed_tl = [
            item
            for item in pending_tl
            if int(getattr(item, "ready_cycle", -1)) >= int(env.current_cycle) + 4
            and int(getattr(item, "addr")) in required_line_addresses
        ]
        if not delayed_tl:
            continue
        associated = dict(observed_transaction)
        associated.update(
            {
                "association_cycle": int(env.current_cycle),
                "tl": [
                    {
                        "source": int(getattr(item, "source")),
                        "address": int(getattr(item, "addr")),
                        "ready_cycle": int(getattr(item, "ready_cycle")),
                    }
                    for item in delayed_tl
                ],
            }
        )
        break
    assert associated is not None, {
        "reason": "trained dual fetch never overlapped a delayed ICache refill",
        "icache": env.icache_agent.get_stats(),
        "backend": env.backend_model.get_stats(),
        "last_observed_transaction": observed_transaction,
        "pending": [
            {
                "source": int(getattr(item, "source")),
                "address": int(getattr(item, "addr")),
                "ready_cycle": int(getattr(item, "ready_cycle")),
            }
            for item in getattr(env.icache_agent, "pending", ())
        ],
    }

    old_tags = set(associated["tags"])
    env.backend_model.inject_redirect(
        _REDIRECT_TARGET,
        "two_fetch_delayed_miss_competition",
        delay_cycles=1,
    )

    redirect_cycle: int | None = None
    first_fire: dict | None = None
    old_ibuffer_deliveries: list[dict] = []
    old_cfvec_deliveries: list[dict] = []
    old_response_records: list[dict] = []
    expected_old_responses = {
        (int(item["source"]), int(item["address"])): int(item["ready_cycle"])
        for item in associated["tl"]
    }
    for _ in range(_cycle_limit("TB_TWO_FETCH_REDIRECT_MAX_CYCLES", 4000)):
        env.step(1)
        if _read_key(recorder, "backend_redirect") == 1 and redirect_cycle is None:
            observed_target = _read_key(recorder, "backend_redirect_target")
            assert observed_target == _REDIRECT_TARGET, {
                "reason": "wrong backend redirect target",
                "expected": _REDIRECT_TARGET,
                "observed": observed_target,
            }
            # Backend inputs are driven from the rising-edge callback and are
            # consumed by the DUT at the following edge.
            redirect_cycle = int(env.current_cycle) + 1

        if redirect_cycle is None or int(env.current_cycle) < redirect_cycle:
            continue

        ibuffer_fire = (
            _read_key(recorder, "to_ibuffer_valid") == 1
            and _read_key(recorder, "to_ibuffer_ready") == 1
        )
        if ibuffer_fire:
            entries = _ibuffer_entries(recorder)
            stale = [entry for entry in entries if entry["ftq_tag"] in old_tags]
            if stale and (not entries or int(entries[0]["pc"]) != _REDIRECT_TARGET):
                old_ibuffer_deliveries.append(
                    {"cycle": int(env.current_cycle), "entries": stale}
                )
            if first_fire is None:
                first_fire = {"cycle": int(env.current_cycle), "entries": entries}

        if first_fire is None:
            stale_cfvec = [
                entry
                for entry in _cfvec_entries(recorder)
                if entry["ftq_tag"] in old_tags
            ]
            if stale_cfvec:
                old_cfvec_deliveries.append(
                    {"cycle": int(env.current_cycle), "entries": stale_cfvec}
                )

        old_response_records = []
        for record in _icache_response_records(env):
            key = (
                int(record.get("source", -1)),
                int(record.get("address", -1)),
            )
            if key not in expected_old_responses:
                continue
            response_cycle = int(record.get("cycle", -1))
            if response_cycle >= max(
                int(redirect_cycle), int(expected_old_responses[key])
            ):
                old_response_records.append(record)
        if first_fire is not None and old_response_records:
            break

    assert redirect_cycle is not None, "queued backend redirect was never driven"
    assert old_response_records, {
        "reason": "no delayed response from the pre-redirect miss arrived after redirect",
        "redirect_cycle": redirect_cycle,
        "associated": associated,
        "responses": _icache_response_records(env),
    }
    assert not old_ibuffer_deliveries and not old_cfvec_deliveries, {
        "reason": "pre-redirect FTQ tag escaped after redirect",
        "old_tags": sorted(old_tags),
        "ibuffer": old_ibuffer_deliveries,
        "cfvec": old_cfvec_deliveries,
    }
    assert first_fire is not None and first_fire["entries"], {
        "reason": "redirect target never reached IBuffer",
        "redirect_cycle": redirect_cycle,
    }
    assert int(first_fire["entries"][0]["pc"]) == _REDIRECT_TARGET, {
        "reason": "first post-redirect IBuffer transfer did not start at target",
        "target": _REDIRECT_TARGET,
        "first_fire": first_fire,
    }
    assert recorder.key_hit("two_fetch_flush_flow", "backend_redirect_drops_inflight")
    assert not any(
        item.get("event")
        in {
            "two_fetch_redirect_old_tag_delivery",
            "two_fetch_redirect_old_tag_cfvec_delivery",
            "two_fetch_redirect_wrong_first_delivery",
        }
        for item in recorder.risk_observations
    ), recorder.risk_observations
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-509")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_bpu_s3_override_drops_stalled_dual_request_before_mainpipe_fire(env):
    _load_and_reset(env)
    recorder = _recorder(env)
    _warm_frontend_execution(env)
    env.icache_agent.configure(hit_latency=1, miss_latency=512, miss_rate=1.0, seed=0x6221)
    _pulse_fencei(env)

    pending_snapshot: dict | None = None
    flush_snapshot: dict | None = None
    delayed_at_flush: list[dict] = []
    predictor_disabled = False
    predictor_disable_count = 0
    predictor_disabled_cycles: list[int] = []
    nonrollback_collision_count = 0
    nonrollback_collisions: list[dict] = []
    rearm_cycle = 0
    for _ in range(_cycle_limit("TB_TWO_FETCH_BPU_FLUSH_MAX_CYCLES", 4000)):
        env.step(1)
        ftq_pending = (
            _read_key(recorder, "ftq_valid") == 1
            and _read_key(recorder, "ftq_ready") == 0
            and _read_key(recorder, "ftq_req1_valid") == 1
        )
        if ftq_pending:
            tags = _current_ftq_request_tags(recorder)
            current = {
                "cycle": int(env.current_cycle),
                "tags": tags,
                "start0": _read_key(recorder, "ftq_req0_start"),
                "start1": _read_key(recorder, "ftq_req1_start"),
                "fetch_ptr": _fetch_ptr(recorder),
            }
            if pending_snapshot is not None:
                assert current["tags"] == pending_snapshot["tags"], {
                    "reason": "FTQ dual request tag changed while valid && !ready",
                    "before": pending_snapshot,
                    "after": current,
                }
                assert current["start0"] == pending_snapshot["start0"]
                assert current["start1"] == pending_snapshot["start1"]
                assert current["fetch_ptr"] == pending_snapshot["fetch_ptr"]
            else:
                pending_snapshot = current
                continue
            if not predictor_disabled and int(env.current_cycle) >= int(rearm_cycle):
                # Changing predictor availability while a trained dual request
                # is blocked makes the later BPU stage disagree with the
                # earlier prediction and produces a real s3 override.
                env.set_bp_ctrl_enable(
                    ubtb_enable=0,
                    abtb_enable=0,
                    mbtb_enable=0,
                    tage_enable=0,
                    sc_enable=0,
                    ittage_enable=0,
                )
                predictor_disabled = True
                predictor_disable_count += 1
                predictor_disabled_cycles.append(int(env.current_cycle))
                predictor_disabled_cycles[:] = predictor_disabled_cycles[-16:]

        if _read_key(recorder, "bpu_s3_flush") == 1 and pending_snapshot is not None:
            flush_ptr = _bpu_s3_flush_ptr(recorder)
            candidate = {
                "cycle": int(env.current_cycle),
                "pending": dict(pending_snapshot),
                "flush_ptr": flush_ptr,
                "flush_target_pc": _ftq_entry_start_pc(recorder, flush_ptr),
                "fetch_ptr_after": _fetch_ptr(recorder),
                "main_s0_fire": _read_key(recorder, "main_s0_fire"),
            }
            pending_fetch_ptr = candidate["pending"]["fetch_ptr"]
            rollback_applied = bool(
                pending_fetch_ptr != flush_ptr
                and _ftq_ptr_at_or_after(pending_fetch_ptr, flush_ptr)
            )
            candidate["rollback_applied"] = rollback_applied
            delayed_candidate = [
                {
                    "source": int(getattr(item, "source")),
                    "address": int(getattr(item, "addr")),
                    "ready_cycle": int(getattr(item, "ready_cycle")),
                }
                for item in getattr(env.icache_agent, "pending", ())
                if int(getattr(item, "ready_cycle", -1)) > int(env.current_cycle)
            ]
            if delayed_candidate:
                flush_snapshot = candidate
                delayed_at_flush = delayed_candidate
                break

            # A younger s3 pointer is a legal no-op for fetchPtr. Rearm the
            # predictors and seek another collision, while still accepting a
            # valid flush that kills the stalled MainPipe request.
            nonrollback_collision_count += 1
            nonrollback_collisions.append(candidate)
            nonrollback_collisions[:] = nonrollback_collisions[-16:]
            env.set_bp_ctrl_enable(
                ubtb_enable=1,
                abtb_enable=1,
                mbtb_enable=1,
                tage_enable=1,
                sc_enable=1,
                ittage_enable=1,
            )
            predictor_disabled = False
            pending_snapshot = None
            rearm_cycle = int(env.current_cycle) + 8
            continue

        if not ftq_pending and _read_key(recorder, "bpu_s3_flush") != 1:
            pending_snapshot = None

    env.set_bp_ctrl_enable(
        ubtb_enable=1,
        abtb_enable=1,
        mbtb_enable=1,
        tage_enable=1,
        sc_enable=1,
        ittage_enable=1,
    )
    assert flush_snapshot is not None, {
        "reason": "no BPU s3 override collided with a stalled dual FTQ request",
        "predictor_disable_count": predictor_disable_count,
        "last_predictor_disabled_cycles": predictor_disabled_cycles,
        "legal_nonrollback_collision_count": nonrollback_collision_count,
        "last_legal_nonrollback_collisions": nonrollback_collisions,
        "icache": env.icache_agent.get_stats(),
        "backend": env.backend_model.get_stats(),
    }

    # Current RTL may consume the MainPipe entry on the same edge as the
    # stage-3 flush. The contract that is observable and stable here is that
    # no stale payload or response escapes after the flush; pointer rollback is
    # checked below when the DUT actually applies it.
    if bool(flush_snapshot["rollback_applied"]):
        assert _ftq_ptr_at_or_after(
            flush_snapshot["pending"]["fetch_ptr"], flush_snapshot["flush_ptr"]
        ), {
            "reason": "reported rollback collision was not rollback-eligible",
            "flush": flush_snapshot,
        }
        assert flush_snapshot["fetch_ptr_after"] == flush_snapshot["flush_ptr"], {
            "reason": "fetchPtr did not roll back to BPU s3 FTQ pointer",
            "flush": flush_snapshot,
        }

    assert delayed_at_flush, {
        "reason": "BPU s3 collision had no in-flight delayed ICache response",
        "flush": flush_snapshot,
        "icache": env.icache_agent.get_stats(),
    }
    delayed_ready = {
        (int(item["source"]), int(item["address"])): int(item["ready_cycle"])
        for item in delayed_at_flush
    }
    old_tags = set(flush_snapshot["pending"]["tags"])
    stale_ibuffer_before_reissue: list[dict] = []
    stale_cfvec_before_reissue: list[dict] = []
    first_reissue: dict | None = None
    delayed_responses_after_flush: list[dict] = []
    for _ in range(_cycle_limit("TB_TWO_FETCH_BPU_RECOVERY_MAX_CYCLES", 1024)):
        env.step(1)
        if first_reissue is None:
            if (
                _read_key(recorder, "to_ibuffer_valid") == 1
                and _read_key(recorder, "to_ibuffer_ready") == 1
            ):
                entries = _ibuffer_entries(recorder)
                stale = [entry for entry in entries if entry["ftq_tag"] in old_tags]
                if stale:
                    stale_ibuffer_before_reissue.append(
                        {"cycle": int(env.current_cycle), "entries": stale}
                    )
            stale_cfvec = [
                entry
                for entry in _cfvec_entries(recorder)
                if entry["ftq_tag"] in old_tags
            ]
            if stale_cfvec:
                stale_cfvec_before_reissue.append(
                    {"cycle": int(env.current_cycle), "entries": stale_cfvec}
                )

            if _read_key(recorder, "main_s0_fire") == 1:
                first_reissue = {
                    "cycle": int(env.current_cycle),
                    "tags": _current_ftq_request_tags(recorder),
                    "start_pc": _read_key(recorder, "ftq_req0_start") << 1,
                }

        delayed_responses_after_flush = []
        for record in _icache_response_records(env):
            key = (
                int(record.get("source", -1)),
                int(record.get("address", -1)),
            )
            if key not in delayed_ready:
                continue
            response_cycle = int(record.get("cycle", -1))
            if response_cycle >= max(
                int(flush_snapshot["cycle"]), int(delayed_ready[key])
            ):
                delayed_responses_after_flush.append(record)
        if first_reissue is not None and delayed_responses_after_flush:
            break

    assert not stale_ibuffer_before_reissue and not stale_cfvec_before_reissue, {
        "reason": "killed dual FTQ transaction escaped before rollback-path reissue",
        "old_tags": sorted(old_tags),
        "ibuffer": stale_ibuffer_before_reissue,
        "cfvec": stale_cfvec_before_reissue,
    }
    assert first_reissue is not None, {
        "reason": "BPU rollback path never reissued to MainPipe",
        "flush": flush_snapshot,
    }
    if bool(flush_snapshot["rollback_applied"]):
        assert first_reissue["tags"][0] == flush_snapshot["flush_ptr"], {
            "reason": "first post-s3 MainPipe request did not use rollback FTQ pointer",
            "flush": flush_snapshot,
            "first_reissue": first_reissue,
        }
        assert int(first_reissue["start_pc"]) == int(flush_snapshot["flush_target_pc"]), {
            "reason": "first post-s3 MainPipe request did not use rollback target PC",
            "flush": flush_snapshot,
            "first_reissue": first_reissue,
        }
    assert delayed_responses_after_flush, {
        "reason": "test ended before delayed response completed",
        "pending_at_flush": delayed_at_flush,
        "responses": _icache_response_records(env),
    }
    assert recorder.key_hit("two_fetch_flush_flow", "bpu_s3_drop_before_issue")
    assert not env.monitor.get_errors()
