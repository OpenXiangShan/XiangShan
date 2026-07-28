from __future__ import annotations

from typing import Any


UNCACHE_EVENT_SAMPLER_BIN_KEYS = frozenset(
    {
        ("uncache_ordering", "pbmt_nc_pmp_mmio_wait_commit"),
        ("uncache_ordering", "pbmt_nc_non_mmio_no_commit_gate"),
        ("uncache_path_switch", "uncache_to_icache_clean"),
        ("fetch_path_switch", "icache_to_mmio_clean"),
        ("uncache_ordering", "pbmt_io_wait_commit"),
        ("uncache_path_switch", "icache_to_nc_clean"),
    }
)


def initialize_fetch_path_coverage_state(recorder) -> None:
    recorder._last_fetch_path = "icache_seq"
    recorder._last_fetch_cycle = -1
    recorder._redirected_fetch_path = None
    recorder._uncache_page_tail_requests = {}
    recorder._uncache_active_nc = False
    recorder._last_uncache_was_nc = False


def reset_fetch_path_coverage_state(recorder) -> None:
    initialize_fetch_path_coverage_state(recorder)


def handle_fetch_path_event(recorder, event: dict[str, Any]) -> None:
    event_type = str(event.get("type", ""))
    cycle = int(event.get("cycle", 0))
    payload = event.get("payload", {}) or {}

    if event_type == "handshake.icache_a":
        if (
            recorder._redirected_fetch_path is not None
            and recorder._redirected_fetch_path.get("path") == "mmio_uncache"
            and recorder._redirected_fetch_path.get("pbmt_nc") is True
        ):
            recorder.mark(
                "uncache_path_switch",
                "uncache_to_icache_clean",
                cycle,
                {"event": event_type, **recorder._redirected_fetch_path},
            )
            recorder._redirected_fetch_path = None
        if (
            recorder._redirected_fetch_path is None
            or recorder._redirected_fetch_path.get("path") != "icache_seq"
        ):
            recorder._redirected_fetch_path = None
        recorder._last_fetch_path = "icache_seq"
        recorder._last_fetch_cycle = cycle
    elif event_type == "handshake.uncache_a":
        address = int(payload.get("address", 0))
        if (
            recorder._redirected_fetch_path is not None
            and recorder._redirected_fetch_path.get("path") == "icache_seq"
            and recorder._uncache_active_nc
        ):
            recorder.mark(
                "uncache_path_switch",
                "icache_to_nc_clean",
                cycle,
                {
                    "event": event_type,
                    "address": address,
                    "new_pbmt_nc": True,
                    **recorder._redirected_fetch_path,
                },
            )
        if (
            recorder._redirected_fetch_path is not None
            and recorder._redirected_fetch_path.get("path") == "icache_seq"
            and recorder.env is not None
            and recorder.env.memory.is_mmio(address)
        ):
            recorder.mark(
                "fetch_path_switch",
                "icache_to_mmio_clean",
                cycle,
                {"event": event_type, "address": address, **recorder._redirected_fetch_path},
            )
        recorder._redirected_fetch_path = None
        recorder._last_fetch_path = "mmio_uncache"
        recorder._last_fetch_cycle = cycle
        sample_uncache_a_event(recorder, cycle, payload)
    elif event_type == "backend.redirect":
        recorder._redirected_fetch_path = {
            "path": recorder._last_fetch_path,
            "pbmt_nc": bool(recorder._last_uncache_was_nc),
        }
        recorder._uncache_page_tail_requests.clear()


def sample_uncache_a_event(recorder, cycle: int, payload: dict[str, Any]) -> None:
    addr = int(payload.get("address", 0))
    recorder._last_uncache_was_nc = bool(recorder._uncache_active_nc)
    recorder._uncache_active_nc = False

    for page, tail in recorder._uncache_page_tail_requests.items():
        if addr == int(page) + 0x1000:
            tail["next_page_requested"] = True
    if addr & 0xFFF == 0xFF8:
        recorder._uncache_page_tail_requests[addr & ~0xFFF] = {
            "request_addr": addr,
            "request_cycle": cycle,
            "next_page_requested": False,
        }


def sample_uncache_cycle_state(recorder, dut, cycle: int) -> None:
    pbmt = recorder._try_read_dut_signal(
        dut, "Frontend_top.Frontend.inner_ifu.s1_icacheMetaIn_0_itlbPbmt"
    )
    pmp_mmio = recorder._try_read_dut_signal(
        dut, "Frontend_top.Frontend.inner_ifu.s1_icacheMetaIn_0_pmpMmio"
    )
    state = recorder._try_read_dut_signal(
        dut, "Frontend_top.Frontend.inner_ifu.uncacheUnit.uncacheState"
    )
    latched_pbmt = recorder._try_read_dut_signal(
        dut, "Frontend_top.Frontend.inner_ifu.uncacheUnit.itlbPbmt"
    )
    active_pbmt = latched_pbmt if state in {1, 2, 3} and latched_pbmt is not None else pbmt
    can_accept = recorder._try_read_dut_signal(dut, "Frontend_top.io_backend_canAccept")
    if active_pbmt == 1 and pmp_mmio == 0 and state in {2, 3}:
        recorder._uncache_active_nc = True
    if active_pbmt == 1 and pmp_mmio == 1 and state == 1:
        recorder.mark(
            "uncache_ordering",
            "pbmt_nc_pmp_mmio_wait_commit",
            cycle,
            {"event": "ifu_uncache_state", "pbmt": active_pbmt, "pmp_mmio": pmp_mmio, "state": state},
        )
    if active_pbmt == 2 and pmp_mmio == 0 and state == 1:
        recorder.mark(
            "uncache_ordering",
            "pbmt_io_wait_commit",
            cycle,
            {"event": "ifu_uncache_state", "pbmt": active_pbmt, "pmp_mmio": pmp_mmio, "state": state},
        )
    if active_pbmt == 1 and pmp_mmio == 0 and state == 2 and can_accept == 0:
        recorder.mark(
            "uncache_ordering",
            "pbmt_nc_non_mmio_no_commit_gate",
            cycle,
            {"event": "ifu_uncache_state", "pbmt": active_pbmt, "pmp_mmio": pmp_mmio, "state": state, "can_accept": can_accept},
        )
