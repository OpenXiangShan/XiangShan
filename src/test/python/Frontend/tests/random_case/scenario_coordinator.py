from __future__ import annotations

import hashlib
import json
import random
import sys
from collections import Counter, defaultdict
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Callable

if not __package__:  # Direct CLI imports must resolve this worktree's public env.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

try:
    from env.support.signal_utils import read_internal_signal
except ImportError:
    read_internal_signal = None

from env.support.pc_utils import fold_pc

try:
    from .scenario_contract import RealizedEvent, write_json_contract
except ImportError:  # Support run_random_campaign.sh invoking this file directly.
    from scenario_contract import RealizedEvent, write_json_contract


TIMING_PLAN_SCHEMA_VERSION = 1
TIMELINE_SCHEMA_VERSION = 1
BIN951_EVENT_SCHEMA_VERSION = 2
BIN951_REDIRECT_SIGNAL_NAMES = {
    "valid": "Frontend_top.io_backend_toFtq_redirect_valid",
    "pc": "Frontend_top.io_backend_toFtq_redirect_bits_pc",
    "target": "Frontend_top.io_backend_toFtq_redirect_bits_target",
    "ftq_flag": "Frontend_top.io_backend_toFtq_redirect_bits_ftqIdx_flag",
    "ftq_value": "Frontend_top.io_backend_toFtq_redirect_bits_ftqIdx_value",
    "ftq_offset": "Frontend_top.io_backend_toFtq_redirect_bits_ftqOffset",
}
BIN951_FTQ_SIGNAL_NAMES = {
    "ftq_redirect_valid": "Frontend_top.Frontend.inner_ftq.redirect_1_valid",
    "ftq_redirect_flag": "Frontend_top.Frontend.inner_ftq.redirect_1_bits_ftqIdx_flag",
    "ftq_redirect_value": "Frontend_top.Frontend.inner_ftq.redirect_1_bits_ftqIdx_value",
    "ftq_redirect_offset": "Frontend_top.Frontend.inner_ftq.redirect_1_bits_ftqOffset",
    "ftq_redirect_target": "Frontend_top.Frontend.inner_ftq.redirectReg_bits_target",
}
BIN951_IFU_SIGNAL_NAMES = {
    "ifu_redirect_valid": "io_fromFtq_redirect_valid",
    "s0_prev_end_is_half_rvi": "s0_prevEndIsHalfRvi",
    "s1_half_valid": "s1_prevEndHalfRviInfo_valid",
    "s1_half_data": "s1_prevEndHalfRviInfo_bits_data",
    "s1_half_pc": "s1_prevEndHalfRviInfo_bits_pc_addr",
    "s1_enqueue_ptr": "s1_prevIBufEnqPtrDup_dup_0_value",
    "s0_fire": "s0_fire",
    "s1_fire": "s1_fire",
    "s2_fire": "s2_fire",
    "s0_flush": "s0_flush",
    "s1_valid": "s1_valid",
    "s1_fetch_block0_start_pc": "s1_fetchBlock_0_startVAddr_addr",
    "s1_flush": "s1_flush",
    "s2_valid": "s2_valid_valid",
    "s2_flush": "s2_flush",
    "to_ibuffer_valid": "io_toIBuffer_valid",
    "to_ibuffer_instr0": "io_toIBuffer_bits_instrs_0",
    "to_ibuffer_enq0": "io_toIBuffer_bits_enqEnable_0",
    "to_ibuffer_foldpc0": "io_toIBuffer_bits_foldpc_0",
    "s2_aligned_pc0": "s2_alignedInstrPcVec_0_addr",
}
BIN951_EVENT_NAME = "dut.bin951_redirect_half_cleanup"
BIN951_MAX_POST_SAMPLES = 64


def validate_bin951_event(event: dict[str, Any]) -> list[str]:
    """Accept only a complete observed redirect, cleanup, and target recovery."""
    failures: list[str] = []
    redirect = event.get("redirect") or {}
    state = event.get("redirect_state") or {}
    pre = event.get("pre_redirect_half_state") or []
    post = event.get("post_redirect_samples") or []

    if event.get("schema_version") != BIN951_EVENT_SCHEMA_VERSION:
        failures.append("schema_version")
    if event.get("runtime_probe_status") != "RAW_FST_CONFIRMED":
        failures.append("runtime_probe_paths_not_raw_fst_confirmed")
    if event.get("redirect_sampling_phase") != "pre_drive_edge":
        failures.append("redirect_edge_identity_not_sampled")
    for key in BIN951_REDIRECT_SIGNAL_NAMES:
        if type(redirect.get(key)) is not int:
            failures.append(f"redirect.{key}_missing")
    if redirect.get("valid") != 1:
        failures.append("redirect_not_valid")

    half = next(
        (sample for sample in reversed(pre) if sample.get("s1_half_valid") == 1),
        None,
    )
    if half is None:
        failures.append("pre_redirect_half_not_observed")
    else:
        for key in ("s1_half_data", "s1_half_pc"):
            if type(half.get(key)) is not int:
                failures.append(f"pre_redirect_half.{key}_missing")
            if type(state.get(key)) is not int:
                failures.append(f"redirect_state.{key}_missing")
            elif type(half.get(key)) is int and state[key] != half[key]:
                failures.append(f"half_{key}_changed_before_redirect")
    if event.get("half_program_map_matches") is not True:
        failures.append("half_program_map_not_matched")
    if state.get("ifu_redirect_valid") != 1:
        failures.append("ifu_redirect_not_observed")
    if state.get("ftq_redirect_valid") != 1:
        failures.append("ftq_redirect_not_observed")
    for field, source in (
        ("ftq_redirect_flag", "ftq_flag"),
        ("ftq_redirect_value", "ftq_value"),
        ("ftq_redirect_offset", "ftq_offset"),
        ("ftq_redirect_target", "target"),
    ):
        if type(state.get(field)) is not int or state[field] != redirect.get(source):
            failures.append(f"{field}_missing_or_mismatched")
    if state.get("s1_half_valid") != 1:
        failures.append("redirect_half_not_valid")
    for key in ("s0_flush", "s1_flush", "s2_flush"):
        if state.get(key) != 1:
            failures.append(f"redirect_state.{key}_missing_or_low")

    recovery = next((sample for sample in post if sample.get("s1_valid") == 1), None)
    if recovery is None:
        failures.append("recovery_s1_not_observed")
        isolated = post
    else:
        isolated = [sample for sample in post if sample["cycle"] < recovery["cycle"]]
    if not isolated:
        failures.append("post_redirect_isolation_not_observed")
    for sample in isolated:
        for key in ("s0_prev_end_is_half_rvi", "s1_valid", "s2_valid", "to_ibuffer_valid"):
            if sample.get(key) != 0:
                failures.append(f"post_redirect.{key}_missing_or_active")
    if recovery is not None:
        if not any(sample.get("s0_fire") == 1 for sample in post if sample["cycle"] < recovery["cycle"]):
            failures.append("recovery_s0_fire_not_observed")
        if recovery.get("s1_half_valid") != 0:
            failures.append("recovery_half_not_cleared")
        start_pc = recovery.get("s1_fetch_block0_start_pc")
        target = redirect.get("target")
        if type(start_pc) is not int or type(target) is not int or start_pc << 1 != target:
            failures.append("recovery_target_missing_or_mismatched")

    target = redirect.get("target")
    delivered = next(
        (
            sample for sample in post
            if recovery is not None
            and sample["cycle"] >= recovery["cycle"]
            and sample.get("to_ibuffer_valid") == 1
            and sample.get("to_ibuffer_enq0") == 1
        ),
        None,
    )
    if delivered is None:
        failures.append("recovery_ibuffer_delivery_not_observed")
    elif (
        type(target) is not int
        or type(delivered.get("s2_aligned_pc0")) is not int
        or type(delivered.get("to_ibuffer_foldpc0")) is not int
        or type(delivered.get("to_ibuffer_instr0")) is not int
        or delivered["s2_aligned_pc0"] << 1 != target
        or delivered["to_ibuffer_foldpc0"] != fold_pc(target)
    ):
        failures.append("recovery_ibuffer_identity_missing_or_mismatched")
    return sorted(set(failures))
UINT64_LIMIT = 1 << 64
FETCH_ADDRESS_EVENTS = frozenset({
    "handshake.icache_a", "handshake.icache_d",
    "handshake.uncache_a", "handshake.uncache_d",
})
INTERESTING_EVENT_TYPES = frozenset(
    {
        "handshake.icache_a",
        "handshake.icache_d",
        "handshake.uncache_a",
        "handshake.uncache_d",
        "backend.redirect_queued",
        "backend.redirect",
        "backend.resolve",
        "backend.commit",
        "backend.can_accept",
        "handshake.ptw_req",
        "handshake.ptw_resp",
        "control.translation_context",
        "control.translation_pbmte",
        "control.sfence",
        "control.pmp_entry_write",
        "control.pma_entry_write",
        "audit.native_ftq_write",
        "audit.native_redirect",
        "audit.native_cfvec_exception",
        "translation.speculative_cfvec_exception",
        "translation.cfvec_exception",
        "translation.speculative_ptw_request",
        "translation.stale_speculative_ptw_response",
    }
)


@dataclass(frozen=True)
class TimingPlan:
    timing_seed: int
    icache_seed: int
    icache_hit_latency: int
    icache_miss_latency: int
    icache_miss_rate: float
    warmup_response_lines: int
    periodic_response_stride: int
    maximum_backpressure_windows: int
    backpressure_durations: tuple[int, ...]
    relation_window_cycles: int = 2
    profile: str = "default"
    uncache_a_stall_cycles: int = 0

    @classmethod
    def from_seed(cls, timing_seed: int, profile: str = "default") -> "TimingPlan":
        if timing_seed < 0 or timing_seed >= UINT64_LIMIT:
            raise ValueError("timing seed must fit in an unsigned 64-bit integer")
        profile = str(profile)
        if profile not in {"default", "ibuffer_pressure_cross", "translation_timing_cross"}:
            raise ValueError(f"unsupported timing profile: {profile}")
        rng = random.Random(int(timing_seed))
        if profile == "ibuffer_pressure_cross":
            # IBuffer full is an emergent state.  A short backend stall only
            # creates pressure; the DUT needs a sustained window to fill its
            # queue while fetch responses continue arriving.
            maximum_windows = rng.randint(4, 8)
            # The profile requires a real redirect/backpressure intersection.
            # Response-to-redirect latency can exceed 1,000 cycles on long CFG
            # bursts. Keep the window long enough to make the requested
            # redirect/backpressure intersection observable while retaining a
            # bounded, replayable timing disturbance.
            backpressure_durations = tuple(rng.randint(1200, 1500) for _ in range(maximum_windows))
            warmup_response_lines = rng.randint(1, 4)
            periodic_response_stride = rng.randint(7, 15)
        else:
            maximum_windows = rng.randint(8, 14)
            backpressure_durations = tuple(rng.randint(2, 7) for _ in range(maximum_windows))
            warmup_response_lines = rng.randint(2, 8)
            periodic_response_stride = rng.randint(13, 31)
        plan = cls(
            timing_seed=int(timing_seed),
            icache_seed=int(timing_seed) ^ 0xA0761D6478BD642F,
            icache_hit_latency=rng.choice((1, 1, 2, 3)),
            icache_miss_latency=rng.choice((12, 16, 20, 24, 32)),
            icache_miss_rate=rng.choice((0.02, 0.04, 0.06, 0.08, 0.10)),
            warmup_response_lines=warmup_response_lines,
            periodic_response_stride=periodic_response_stride,
            maximum_backpressure_windows=maximum_windows,
            backpressure_durations=backpressure_durations,
            profile=profile,
        )
        plan.validate()
        return plan

    @classmethod
    def from_dict(cls, payload: dict[str, Any]) -> "TimingPlan":
        if not isinstance(payload, dict):
            raise ValueError("timing plan must be an object")
        durations = payload.get("backpressure_durations")
        if not isinstance(durations, (list, tuple)):
            raise ValueError("timing plan backpressure_durations must be a list")
        plan = cls(
            timing_seed=int(payload["timing_seed"]),
            icache_seed=int(payload["icache_seed"]),
            icache_hit_latency=int(payload["icache_hit_latency"]),
            icache_miss_latency=int(payload["icache_miss_latency"]),
            icache_miss_rate=float(payload["icache_miss_rate"]),
            warmup_response_lines=int(payload["warmup_response_lines"]),
            periodic_response_stride=int(payload["periodic_response_stride"]),
            maximum_backpressure_windows=int(payload["maximum_backpressure_windows"]),
            backpressure_durations=tuple(int(value) for value in durations),
            relation_window_cycles=int(payload.get("relation_window_cycles", 2)),
            profile=str(payload.get("profile") or "default"),
            uncache_a_stall_cycles=int(payload.get("uncache_a_stall_cycles", 0)),
        )
        plan.validate()
        return plan

    def validate(self) -> None:
        if self.timing_seed < 0 or self.timing_seed >= UINT64_LIMIT:
            raise ValueError("timing seed must fit in an unsigned 64-bit integer")
        if self.profile not in {"default", "ibuffer_pressure_cross", "translation_timing_cross"}:
            raise ValueError(f"unsupported timing profile: {self.profile}")
        if self.icache_hit_latency < 0 or self.icache_miss_latency < 0:
            raise ValueError("ICache latencies must be >= 0")
        if not 0.0 <= self.icache_miss_rate <= 1.0:
            raise ValueError("ICache miss rate must be in [0, 1]")
        if self.warmup_response_lines <= 0 or self.periodic_response_stride <= 0:
            raise ValueError("response trigger thresholds must be > 0")
        if self.maximum_backpressure_windows <= 0:
            raise ValueError("maximum_backpressure_windows must be > 0")
        if len(self.backpressure_durations) != self.maximum_backpressure_windows:
            raise ValueError("each backpressure window must have a duration")
        if any(duration <= 0 for duration in self.backpressure_durations):
            raise ValueError("backpressure durations must be > 0")
        if self.relation_window_cycles < 0:
            raise ValueError("relation_window_cycles must be >= 0")
        if not 0 <= self.uncache_a_stall_cycles <= 1024:
            raise ValueError("uncache_a_stall_cycles must be in [0, 1024]")

    def to_dict(self) -> dict[str, Any]:
        self.validate()
        payload = {"schema_version": TIMING_PLAN_SCHEMA_VERSION, **asdict(self)}
        payload["backpressure_durations"] = list(self.backpressure_durations)
        if self.uncache_a_stall_cycles == 0:
            # Preserve byte-for-byte historical timing-plan serialization.
            payload.pop("uncache_a_stall_cycles")
        payload["policy"] = {
            "random_draws_resolved_before_run": True,
            "event_triggered": True,
            "internal_dut_force": False,
            "trigger_source": (
                "observed_backend_redirect_queue"
                if self.profile == "ibuffer_pressure_cross"
                else "observed_top_level_handshake"
            ),
        }
        if self.uncache_a_stall_cycles:
            payload["policy"]["uncache_a_stall"] = {
                "trigger_source": "observed_uncache_a_valid_pre_drive",
                "release_requires_handshake": True,
                "payload_stability_fields": [
                    "address",
                    "mem_back_type_mm",
                    "mem_page_type_nc",
                ],
            }
        return payload


def _stable_payload_sha256(payload: dict[str, Any]) -> str:
    encoded = json.dumps(payload, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return hashlib.sha256(encoded).hexdigest()


def _read_signal(signal: Any, default: int = 0) -> int:
    try:
        value = getattr(signal, "value", None)
        return default if value is None else int(value)
    except Exception:
        return default


def _read_optional_binary(signal: Any) -> int | None:
    """Missing/unreadable pins never become a measured zero."""
    try:
        value = signal.value
        return int(value) if value is not None and value in (0, 1) else None
    except Exception:
        return None


def _read_ibuffer_full(env: Any) -> int | None:
    """Return unknown if neither the raw allowEnq nor optional wrapper is readable."""
    dut = getattr(env, "dut", None)
    if dut is not None and callable(read_internal_signal):
        for name in (
            "Frontend_top.Frontend.inner_ibuffer.allowEnq",
            "Frontend_top.Frontend._inner_ibuffer_allowEnq",
        ):
            allow_enq = read_internal_signal(dut, name)
            if allow_enq in (0, 1):
                return 1 - int(allow_enq)
    return _read_optional_binary(
        getattr(getattr(env, "frontend_info_if", None), "io_frontendInfo_ibufFull", None)
    )


def _record_pressure_sample(action: dict[str, Any], cycle: int, can_accept: int | None) -> None:
    """Record sampled post-drive input windows; never infer a held interval."""
    samples = action.setdefault("pressure_samples", [])
    samples.append({"cycle": int(cycle), "can_accept": can_accept})
    action["pressure_sampling_phase"] = "post_drive_input; not edge handshake proof"
    counts = Counter(sample["cycle"] for sample in samples)
    observed = {sample["cycle"]: sample["can_accept"] for sample in samples}
    low = sorted(c for c, value in observed.items() if value == 0 and counts[c] == 1)
    windows = []
    for c in low:
        if windows and windows[-1]["end_cycle_inclusive"] == c - 1:
            windows[-1]["end_cycle_inclusive"] = c
            windows[-1]["cycles"] += 1
        else:
            windows.append({"start_cycle": c, "end_cycle_inclusive": c, "cycles": 1})
    action["observed_low_windows"] = windows
    action["confirmed_low_cycles"] = len(low)
    expected = list(range(int(action["start_cycle"]), int(action["end_cycle_inclusive"]) + 1))
    action["pressure_window_confirmed"] = (
        [sample["cycle"] for sample in samples] == expected
        and all(sample["can_accept"] == 0 for sample in samples)
    )


def _program_map_index(
    program_map: dict[str, Any],
) -> tuple[dict[int, set[str]], dict[int, list[dict[str, Any]]], list[int]]:
    episodes_by_line: dict[int, set[str]] = defaultdict(set)
    instructions_by_line: dict[int, list[dict[str, Any]]] = defaultdict(list)
    boundary_lines: set[int] = set()
    for instruction in program_map.get("instructions") or []:
        pc = int(instruction["pc"])
        size = int(instruction.get("size") or 0)
        if size not in {2, 4, 6, 8}:
            raise ValueError(f"invalid ProgramMap instruction size at 0x{pc:x}: {size}")
        episode_id = str(instruction.get("episode_id") or "")
        boundary_features = tuple(
            name
            for name in (
                "crosses_fetch_block",
                "crosses_cacheline",
                "crosses_page",
                "conditional_branch",
                "call",
                "return",
            )
            if bool(instruction.get(name))
        )
        first_line = pc & ~0x3F
        last_line = (pc + size - 1) & ~0x3F
        for line in range(first_line, last_line + 0x40, 0x40):
            instructions_by_line[line].append(
                {
                    "pc": pc,
                    "size": size,
                    "episode_id": episode_id,
                    "boundary_features": boundary_features,
                }
            )
            if episode_id:
                episodes_by_line[line].add(episode_id)
            if boundary_features:
                boundary_lines.add(line)
    return dict(episodes_by_line), dict(instructions_by_line), sorted(boundary_lines)


class ScenarioCoordinator:
    def __init__(
        self,
        *,
        scenario_id: str,
        timing_plan: TimingPlan,
        program_map: dict[str, Any],
        maximum_timeline_events: int = 20000,
    ) -> None:
        timing_plan.validate()
        if not scenario_id.strip():
            raise ValueError("scenario_id is required")
        if not program_map.get("binary_text_verified"):
            raise ValueError("ScenarioCoordinator requires a byte-verified ProgramMap")
        self.scenario_id = str(scenario_id)
        self.timing_plan = timing_plan
        self.program_map = program_map
        self.maximum_timeline_events = max(1, int(maximum_timeline_events))
        self.episodes_by_line, self.instructions_by_line, candidate_lines = _program_map_index(
            program_map
        )
        line_rng = random.Random(int(timing_plan.timing_seed) ^ 0xE7037ED1A0B428DB)
        line_rng.shuffle(candidate_lines)
        self.selected_boundary_lines = tuple(
            sorted(candidate_lines[: timing_plan.maximum_backpressure_windows])
        )
        self.env = None
        self._previous_event_sink: Callable[[dict], None] | None = None
        self._installed = False
        self._active_action: dict[str, Any] | None = None
        self._uncache_stall_action: dict[str, Any] | None = None
        self._acted_boundary_lines: set[int] = set()
        self._next_periodic_response = int(timing_plan.warmup_response_lines)
        self._response_line_count = 0
        self._event_counts: Counter[str] = Counter()
        self._event_cycles: dict[str, list[int]] = defaultdict(list)
        self._timeline: list[dict[str, Any]] = []
        self._dropped_timeline_events = 0
        self._actions: list[dict[str, Any]] = []
        self._observations: list[dict[str, Any]] = []
        self._realized_counts: Counter[str] = Counter()
        self._realized_first: dict[str, dict[str, int]] = {}
        self._last_icache_response_cycle: int | None = None
        self._last_icache_request_cycle: int | None = None
        self._last_redirect_cycle: int | None = None
        self._last_can_accept: int | None = None
        self._last_ibuffer_full: int | None = None
        self._last_ibuffer_probe_available: bool | None = None
        self._translation_pages: dict[int, int] | None = None
        self._translation_mapping_valid = False
        self._bin951_history: list[dict[str, Any]] = []
        self._bin951_redirect_edges: dict[int, dict[str, int | None]] = {}
        self._bin951_active: dict[str, Any] | None = None
        self._bin951_events: list[dict[str, Any]] = []

    def configure_translation_pages(self, outcomes: list[dict] | tuple[dict, ...]) -> None:
        """Index configured successful mappings, not inferred DUT translations.

        The current random family has a static mapping. A later context change
        invalidates this index; physical aliases are never assigned one guessed VA.
        """
        if self._translation_pages is not None:
            raise RuntimeError("translation program mapping is already configured")
        pages: dict[int, int] = {}
        for outcome in outcomes:
            if not outcome.get("ok"):
                continue
            va, pa = int(outcome["va"]), int(outcome["pa"])
            if va < 0 or pa < 0 or va % 4096 != pa % 4096:
                raise ValueError("translation program mapping requires matching page offsets")
            virtual_page, physical_page = va & ~4095, pa & ~4095
            if virtual_page in pages and pages[virtual_page] != physical_page:
                raise ValueError("conflicting translation program page mapping")
            pages[virtual_page] = physical_page
        required_pages = {int(item["pc"]) & ~4095 for item in self.program_map.get("instructions", [])}
        if not required_pages.issubset(pages):
            raise ValueError("translation program mapping does not cover ProgramMap pages")
        self._translation_pages = pages
        self._translation_mapping_valid = True

    @property
    def translation_mapping_valid(self) -> bool:
        return self._translation_mapping_valid

    @property
    def action_count(self) -> int:
        return len(self._actions)

    @property
    def has_active_action(self) -> bool:
        return any(
            action is not None
            and str(action.get("status") or "") in {"ACTIVE", "RELEASE_PENDING"}
            for action in (self._active_action, self._uncache_stall_action)
        )

    @property
    def unrealized_action_ids(self) -> tuple[str, ...]:
        return tuple(
            str(action.get("action_id") or "unnamed")
            for action in self._actions
            if str(action.get("status") or "") != "REALIZED"
        )

    def _append_timeline(self, item: dict[str, Any]) -> None:
        if len(self._timeline) < self.maximum_timeline_events:
            self._timeline.append(item)
        else:
            self._dropped_timeline_events += 1

    def _observe_realized(self, event: str, cycle: int, *, details: dict[str, Any] | None = None) -> None:
        self._realized_counts[event] += 1
        self._realized_first.setdefault(
            event,
            {"first_cycle": int(cycle), **dict(details or {})},
        )

    def install(self, env: Any) -> None:
        if self._installed:
            raise RuntimeError("ScenarioCoordinator is already installed")
        self.env = env
        self._previous_event_sink = getattr(env, "event_sink", None)

        def combined_sink(event: dict) -> None:
            if self._previous_event_sink is not None:
                self._previous_event_sink(event)
            self.handle_event(event)

        env.set_event_sink(combined_sink)
        if self.timing_plan.profile == "translation_timing_cross":
            env.register_pre_drive_cycle_observer(self.observe_translation_edge)
        if self.timing_plan.profile == "ibuffer_pressure_cross":
            env.register_pre_drive_cycle_observer(self.observe_bin951_redirect_edge)
        if self.timing_plan.uncache_a_stall_cycles:
            env.register_pre_drive_cycle_observer(self.observe_uncache_a_edge)
        env.register_cycle_observer(self.observe_cycle)
        env.icache_agent.configure(
            hit_latency=self.timing_plan.icache_hit_latency,
            miss_latency=self.timing_plan.icache_miss_latency,
            miss_rate=self.timing_plan.icache_miss_rate,
            seed=self.timing_plan.icache_seed,
        )
        self._installed = True

    @staticmethod
    def _uncache_a_payload(env: Any) -> dict[str, int]:
        interface = getattr(env, "uncache_if", None)
        fields = {
            "address": "a_bits_address",
            "mem_back_type_mm": "a_bits_user_mem_back_type_mm",
            "mem_page_type_nc": "a_bits_user_mem_page_type_nc",
        }
        payload: dict[str, int] = {}
        for key, name in fields.items():
            signal = getattr(interface, name, None)
            if signal is None or getattr(signal, "value", None) is None:
                raise ValueError(f"uncache A payload signal is unavailable: {name}")
            payload[key] = int(signal.value)
        return payload

    def _finish_uncache_stall_failure(
        self,
        cycle: int,
        reason: str,
        *,
        observed_payload: dict[str, int] | None = None,
    ) -> None:
        action = self._uncache_stall_action
        if action is None:
            return
        action["status"] = "FAIL"
        action["failure_reason"] = str(reason)
        action["completion_cycle"] = int(cycle)
        if observed_payload is not None:
            action["failure_payload"] = dict(observed_payload)
        self._append_timeline(
            {
                "type": "scenario.action_fail",
                "cycle": int(cycle),
                "payload": dict(action),
            }
        )
        if self.env is not None:
            self.env.uncache_agent.set_a_ready(None)

    def observe_uncache_a_edge(self, cycle: int, env: Any) -> None:
        if _read_signal(
            getattr(getattr(env, "clock_reset", None), "reset", None),
            default=0,
        ):
            return
        stall_cycles = int(self.timing_plan.uncache_a_stall_cycles)
        action = self._uncache_stall_action
        if stall_cycles == 0:
            return
        if action is not None and str(action.get("status") or "") != "ACTIVE":
            return
        interface = getattr(env, "uncache_if", None)
        valid = _read_signal(getattr(interface, "a_valid", None), default=0)
        ready = _read_signal(getattr(interface, "a_ready", None), default=1)
        if action is None:
            if valid != 1:
                return
            try:
                payload = self._uncache_a_payload(env)
            except (TypeError, ValueError) as exc:
                action = {
                    "action_id": "uncache-a-stall-000",
                    "kind": "uncache_a_ready_low",
                    "trigger_event": "dut.uncache_a_valid",
                    "trigger_cycle": int(cycle),
                    "start_cycle": int(cycle),
                    "duration_cycles": stall_cycles,
                    "status": "FAIL",
                    "confirmed_low_cycles": 0,
                    "failure_reason": str(exc),
                    "completion_cycle": int(cycle),
                }
                self._actions.append(action)
                self._uncache_stall_action = action
                self._append_timeline(
                    {
                        "type": "scenario.action_fail",
                        "cycle": int(cycle),
                        "payload": dict(action),
                    }
                )
                env.uncache_agent.set_a_ready(None)
                return
            line_context = self._event_line_context(
                {
                    "type": "handshake.uncache_a",
                    "cycle": int(cycle),
                    "payload": payload,
                }
            )
            action = {
                "action_id": "uncache-a-stall-000",
                "kind": "uncache_a_ready_low",
                "trigger_event": "dut.uncache_a_valid",
                "trigger_cycle": int(cycle),
                "start_cycle": int(cycle) + 1,
                "duration_cycles": stall_cycles,
                "captured_payload": payload,
                **line_context,
                "status": "ACTIVE",
                "confirmed_low_cycles": 0,
            }
            self._actions.append(action)
            self._uncache_stall_action = action
            env.uncache_agent.set_a_ready(0)
            self._append_timeline(
                {
                    "type": "scenario.action_start",
                    "cycle": int(cycle),
                    "payload": dict(action),
                }
            )
            return
        if valid != 1:
            self._finish_uncache_stall_failure(
                cycle, "uncache_a_valid_deasserted_before_release"
            )
            return
        try:
            payload = self._uncache_a_payload(env)
        except (TypeError, ValueError) as exc:
            self._finish_uncache_stall_failure(cycle, str(exc))
            return
        if payload != action["captured_payload"]:
            self._finish_uncache_stall_failure(
                cycle,
                "uncache_a_payload_changed_while_stalled",
                observed_payload=payload,
            )
            return
        if ready != 0:
            self._finish_uncache_stall_failure(
                cycle,
                "uncache_a_ready_low_not_observed",
                observed_payload=payload,
            )
            return
        action["confirmed_low_cycles"] = int(action["confirmed_low_cycles"]) + 1
        self._observe_realized(
            "dut.uncache_a_stall_cycle",
            cycle,
            details=dict(payload),
        )
        if int(action["confirmed_low_cycles"]) < stall_cycles:
            return
        action["status"] = "RELEASE_PENDING"
        action["release_cycle"] = int(cycle)
        env.uncache_agent.set_a_ready(None)
        self._append_timeline(
            {
                "type": "scenario.action_release",
                "cycle": int(cycle),
                "payload": dict(action),
            }
        )

    def observe_translation_edge(self, cycle: int, env: Any) -> None:
        """Retain native identities before agents drive next-cycle inputs.

        This is raw evidence only: valid and canAccept do not establish path
        ownership, and repeated held packets are not independent transactions.
        Required native signals must be present; no guessed PC or zero fallback.
        """
        def read(signal: Any) -> int:
            return int(signal.value)

        if read(env.clock_reset.reset):
            return
        control = env.backend_to_ftq_if
        observation = env.backend_cfvec_if
        from_ftq = env.backend_from_ftq_if

        def record(kind: str, payload: dict[str, Any]) -> None:
            self.handle_event({
                "type": "audit.native_" + kind,
                "source": "pre_drive_top_level_observer",
                "cycle": int(cycle),
                "payload": {"sampling_phase": "pre_drive", **payload},
            })

        if read(from_ftq.io_backend_fromFtq_wen):
            record("ftq_write", {
                "ftq_idx_raw": read(from_ftq.io_backend_fromFtq_ftqIdx),
                "start_pc_addr_raw": read(from_ftq.io_backend_fromFtq_startPc_addr),
            })
        redirect_valid = read(control.redirect_valid)
        can_accept = read(env.backend_to_ibuf_if.decode_can_accept)
        if redirect_valid:
            record("redirect", {
                "ftq_flag": read(control.redirect_bits_ftq_idx_flag),
                "ftq_value": read(control.redirect_bits_ftq_idx_value),
                "ftq_offset": read(control.redirect_bits_ftq_offset),
                "target_raw": read(control.redirect_bits_target),
                "pc_raw": read(control.redirect_bits_pc),
                "level": read(control.redirect_bits_level),
                "can_accept": can_accept,
            })
        for slot in range(8):
            if not read(observation.cfvec_valid[slot]):
                continue
            bits = {
                bit: read(getattr(observation, f"cfvec_exception_vec_{bit}")[slot])
                for bit in (1, 2, 12, 19, 20)
            }
            if not any(bits.values()):
                continue
            is_last_signal = getattr(observation, "cfvec_is_last_in_ftq_entry", None)
            is_last = None
            if is_last_signal is not None and "cfvec_is_last_in_ftq_entry" not in getattr(observation, "_missing_optional_attrs", ()):
                is_last = read(is_last_signal[slot])
            record("cfvec_exception", {
                "slot": slot,
                "folded_pc": read(observation.cfvec_foldpc[slot]),
                "ftq_flag": read(observation.cfvec_ftq_ptr_flag[slot]),
                "ftq_value": read(observation.cfvec_ftq_ptr_value[slot]),
                "ftq_offset": read(observation.cfvec_ftq_offset[slot]),
                "is_rvc": read(observation.cfvec_is_rvc[slot]),
                # Full-core ABI may omit this optional boundary bit. Preserve
                # that fact; BackendModel infers the boundary from FTQ
                # pointer transitions and path metadata. Never substitute 0.
                "is_last": is_last,
                "is_last_available": is_last is not None,
                "cross_page": read(observation.cfvec_cross_page_ipf_fix[slot]),
                "exception_bits": bits,
                "can_accept": can_accept,
                "redirect_valid": redirect_valid,
            })

    def _line_context(self, address: int) -> dict[str, Any]:
        line = int(address) & ~0x3F
        line_instructions = self.instructions_by_line.get(line, ())
        boundary_instructions = [
            instruction
            for instruction in line_instructions
            if instruction["boundary_features"]
        ]
        exact_instructions = [
            instruction
            for instruction in line_instructions
            if int(instruction["pc"]) <= int(address)
            < int(instruction["pc"]) + int(instruction["size"])
        ]
        return {
            "cacheline_address": line,
            "episode_ids": sorted(self.episodes_by_line.get(line, set())),
            "line_episode_ids": sorted(self.episodes_by_line.get(line, set())),
            "boundary_episode_ids": sorted(
                {
                    str(instruction["episode_id"])
                    for instruction in boundary_instructions
                    if instruction["episode_id"]
                }
            ),
            "boundary_instruction_pcs": sorted(
                int(instruction["pc"]) for instruction in boundary_instructions
            ),
            "exact_address_instruction_pcs": sorted(
                int(instruction["pc"]) for instruction in exact_instructions
            ),
            "attribution_scope": "cacheline",
            "exact_instruction_attribution": False,
            "selected_boundary_line": line in self.selected_boundary_lines,
        }

    def _event_line_context(self, event: dict[str, Any]) -> dict[str, Any]:
        payload = event.get("payload") or {}
        address = payload.get("address", payload.get("from_pc", payload.get("target_pc")))
        if address is None:
            return {"cacheline_address": None, "episode_ids": [], "selected_boundary_line": False,
                    "line_episode_ids": [], "boundary_episode_ids": [],
                    "boundary_instruction_pcs": [], "exact_address_instruction_pcs": [],
                    "attribution_scope": "none", "exact_instruction_attribution": False,
                    "mapping_status": "missing_address"}
        address = int(address)
        physical = str(event.get("type")) in FETCH_ADDRESS_EVENTS
        translated = self.timing_plan.profile == "translation_timing_cross"
        if not physical or not translated:
            return {**self._line_context(address), "address_space": "program",
                    "mapping_status": "identity"}
        candidates = []
        if self._translation_mapping_valid:
            candidates = sorted(
                va + (address & 4095) for va, pa in (self._translation_pages or {}).items()
                if pa == address & ~4095
            )
        status = ("unconfigured_or_invalidated" if not self._translation_mapping_valid else
                  "unmapped" if not candidates else "ambiguous_alias" if len(candidates) > 1 else "mapped")
        context = (self._line_context(candidates[0]) if len(candidates) == 1 else
                   {"cacheline_address": None, "episode_ids": [], "line_episode_ids": [],
                    "boundary_episode_ids": [], "boundary_instruction_pcs": [],
                    "exact_address_instruction_pcs": [], "attribution_scope": "none",
                    "exact_instruction_attribution": False, "selected_boundary_line": False})
        return {**context, "address_space": "physical", "physical_address": address,
                "program_address_candidates": candidates, "mapping_status": status,
                "mapping_evidence": "configured_page_model; not DUT translation proof"}

    def _start_backpressure(self, cycle: int, event: dict[str, Any]) -> None:
        if self.env is None or self._active_action is not None:
            return
        ordinal = sum(
            str(action.get("kind") or "") == "backend_can_accept_low"
            for action in self._actions
        )
        if ordinal >= self.timing_plan.maximum_backpressure_windows:
            return
        duration = int(self.timing_plan.backpressure_durations[ordinal])
        payload = dict(event.get("payload") or {})
        line_context = self._event_line_context(event)
        action = {
            "action_id": f"backend-backpressure-{ordinal:03d}",
            "kind": "backend_can_accept_low",
            "trigger_event": str(event.get("type") or ""),
            "trigger_cycle": int(cycle),
            "start_cycle": int(cycle),
            "end_cycle_inclusive": int(cycle) + duration - 1,
            "duration_cycles": duration,
            "trigger_payload": payload,
            **line_context,
            "status": "ACTIVE",
            "confirmed_low_cycles": 0,
        }
        self._actions.append(action)
        self._active_action = action
        self.env.backend_model.set_can_accept(0)
        self._append_timeline(
            {
                "type": "scenario.action_start",
                "cycle": int(cycle),
                "payload": dict(action),
            }
        )

    def handle_event(self, event: dict[str, Any]) -> None:
        event_type = str(event.get("type") or "")
        cycle = int(event.get("cycle") or 0)
        payload = dict(event.get("payload") or {})
        if self._translation_pages is not None and event_type in {"control.translation_context", "control.sfence"}:
            self._translation_mapping_valid = False
        self._event_counts[event_type] += 1
        if len(self._event_cycles[event_type]) < 4096:
            self._event_cycles[event_type].append(cycle)
        if event_type in INTERESTING_EVENT_TYPES:
            item = {
                "type": event_type,
                "source": str(event.get("source") or ""),
                "cycle": cycle,
                "payload": payload,
            }
            if "address" in payload:
                item["program_context"] = self._event_line_context(event)
            self._append_timeline(item)

        uncache_stall = self._uncache_stall_action
        if event_type == "handshake.uncache_a" and uncache_stall is not None:
            status = str(uncache_stall.get("status") or "")
            if status == "ACTIVE":
                self._finish_uncache_stall_failure(
                    cycle,
                    "uncache_a_fired_before_release",
                    observed_payload=payload,
                )
            elif status == "RELEASE_PENDING":
                expected_payload = dict(uncache_stall["captured_payload"])
                observed_payload = {
                    key: int(payload.get(key, -1)) for key in expected_payload
                }
                if observed_payload != expected_payload:
                    self._finish_uncache_stall_failure(
                        cycle,
                        "uncache_a_release_payload_mismatch",
                        observed_payload=observed_payload,
                    )
                else:
                    uncache_stall["status"] = "REALIZED"
                    uncache_stall["completion_cycle"] = int(cycle)
                    uncache_stall["release_fire_payload"] = observed_payload
                    self._observe_realized(
                        "dut.uncache_a_stall_release",
                        cycle,
                        details=observed_payload,
                    )
                    self._append_timeline(
                        {
                            "type": "scenario.action_complete",
                            "cycle": int(cycle),
                            "payload": dict(uncache_stall),
                        }
                    )
                    self.env.uncache_agent.set_a_ready(None)

        # A response-triggered stall can postpone the redirect that would
        # otherwise exercise the intended cross.  For the IBuffer pressure
        # profile, begin the stall when redirect is queued, before the same
        # cycle drives redirect_valid, so the DUT samples both together.
        if (
            self.timing_plan.profile == "ibuffer_pressure_cross"
            and event_type == "backend.redirect_queued"
        ):
            self._start_backpressure(cycle, event)
            return

        if event_type in {"handshake.icache_a", "handshake.uncache_a"}:
            self._last_icache_request_cycle = cycle
            path = "icache" if event_type == "handshake.icache_a" else "uncache"
            self._observe_realized(
                f"dut.{path}_request",
                cycle,
                details={"first_pc": int(payload.get("address", 0))},
            )
            self._observe_realized(
                "dut.fetch_request",
                cycle,
                details={"path": path, "first_pc": int(payload.get("address", 0))},
            )
            return
        if event_type == "backend.redirect":
            self._last_redirect_cycle = cycle
            self._observe_realized(
                "dut.backend_redirect",
                cycle,
                details={"first_pc": int(payload.get("from_pc", 0))},
            )
            return
        if event_type == "handshake.ptw_req":
            self._observe_realized(
                "dut.ptw_request",
                cycle,
                details={
                    "first_pc": int(payload.get("vpn", 0)) << 12,
                    "s2xlate": int(payload.get("s2xlate", 0)),
                },
            )
            return
        if event_type == "handshake.ptw_resp":
            self._observe_realized(
                "dut.ptw_response",
                cycle,
                details={
                    "first_pc": int(payload.get("vpn", 0)) << 12,
                    "s2xlate": int(payload.get("s2xlate", 0)),
                },
            )
            return
        if event_type not in {"handshake.icache_d", "handshake.uncache_d"}:
            return

        self._last_icache_response_cycle = cycle
        path = "icache" if event_type == "handshake.icache_d" else "uncache"
        self._observe_realized(
            f"dut.{path}_response",
            cycle,
            details={"first_pc": int(payload.get("address", 0))},
        )
        self._observe_realized(
            "dut.fetch_response",
            cycle,
            details={"path": path, "first_pc": int(payload.get("address", 0))},
        )
        if event_type == "handshake.icache_d" and int(payload.get("beat_idx", 0)) != 0:
            return
        self._response_line_count += 1
        context = self._event_line_context(event)
        line = context["cacheline_address"]
        boundary_trigger = (
            context["selected_boundary_line"] and line not in self._acted_boundary_lines
        )
        periodic_trigger = self._response_line_count >= self._next_periodic_response
        if self._active_action is None and (boundary_trigger or periodic_trigger):
            if self.timing_plan.profile == "ibuffer_pressure_cross":
                return
            if line is not None:
                self._acted_boundary_lines.add(line)
            self._start_backpressure(cycle, event)
            self._next_periodic_response = (
                self._response_line_count + self.timing_plan.periodic_response_stride
            )

    def _record_signal_transition(self, event: str, cycle: int, value: int) -> None:
        self._observations.append({"event": event, "cycle": int(cycle), "value": int(value)})
        self._append_timeline(
            {"type": event, "source": "top_level_signal", "cycle": int(cycle), "payload": {"value": int(value)}}
        )

    @staticmethod
    def _read_required_dut_signal(env: Any, name: str, *, snapshot: bool = False) -> int | None:
        dut = getattr(env, "dut", None)
        if dut is None:
            return None
        candidates = (
            name,
            f"Frontend_top.Frontend.inner_ifu.{name}",
            f"Frontend_top.Frontend._inner_ifu_{name}",
            f"inner_ifu.{name}",
        )
        coverage = getattr(env, "functional_coverage", None)
        snapshot_reader = getattr(coverage, "_try_read_dut_signal", None)
        if snapshot and getattr(coverage, "native_toffee_runtime_enabled", False):
            if not callable(snapshot_reader):
                raise RuntimeError("native random sampling requires the recorder snapshot reader")
            for candidate in candidates:
                value = snapshot_reader(dut, candidate)
                if value is not None:
                    return int(value)
            return None
        for candidate in candidates:
            signal = getattr(dut, candidate, None)
            value = _read_signal(signal, default=-1)
            if value >= 0:
                return value
            if callable(read_internal_signal):
                value = read_internal_signal(dut, candidate)
                if value is not None:
                    return int(value)
        return None

    def observe_bin951_redirect_edge(self, cycle: int, env: Any) -> None:
        if _read_signal(getattr(getattr(env, "clock_reset", None), "reset", None)):
            return
        redirect = {
            key: self._read_required_dut_signal(env, name)
            for key, name in BIN951_REDIRECT_SIGNAL_NAMES.items()
        }
        if redirect["valid"] == 1:
            self._bin951_redirect_edges[int(cycle)] = redirect

    def _sample_bin951(self, cycle: int, env: Any) -> None:
        if self.timing_plan.profile != "ibuffer_pressure_cross":
            return
        dut = getattr(env, "dut", None)
        if dut is None:
            return

        coverage = getattr(env, "functional_coverage", None)
        if getattr(coverage, "native_toffee_runtime_enabled", False) and getattr(coverage, "_cycle_snapshot_cycle", None) != int(cycle):
            raise RuntimeError("random sampling requires the current post-drive native snapshot")

        state = {
            key: self._read_required_dut_signal(env, name, snapshot=True)
            for key, name in {**BIN951_IFU_SIGNAL_NAMES, **BIN951_FTQ_SIGNAL_NAMES}.items()
        }
        state["cycle"] = int(cycle)
        self._bin951_history.append(state)
        if len(self._bin951_history) > 8:
            del self._bin951_history[:-8]

        redirect = self._bin951_redirect_edges.pop(int(cycle), None)
        redirect_valid = redirect is not None
        if self._bin951_active is not None:
            self._bin951_active["post_redirect_samples"].append(state)
            target = self._bin951_active["redirect"]["target"]
            if (
                target is not None
                and state.get("s1_valid") == 1
                and self._bin951_active["recovery_cycle"] is None
            ):
                self._bin951_active["recovery_cycle"] = int(cycle)
                start_pc = state.get("s1_fetch_block0_start_pc")
                self._bin951_active["recovery_target_match"] = (
                    None if start_pc is None else (int(start_pc) << 1) == int(target)
                )
            if (
                redirect_valid
                or len(self._bin951_active["post_redirect_samples"]) >= BIN951_MAX_POST_SAMPLES
                or (state.get("to_ibuffer_valid") == 1 and state.get("to_ibuffer_enq0") == 1)
            ):
                self._finish_bin951_event()
        if not redirect_valid:
            return
        pre_state = self._bin951_history[:-1]
        half = next(
            (sample for sample in reversed(pre_state) if sample.get("s1_half_valid") == 1),
            None,
        )
        self._bin951_active = {
            "schema_version": BIN951_EVENT_SCHEMA_VERSION,
            "status": "PENDING",
            "runtime_probe_status": getattr(self, "_bin951_runtime_probe_status", "UNVERIFIED"),
            "redirect_cycle": int(cycle),
            "redirect_sampling_phase": "pre_drive_edge",
            "redirect": redirect,
            "pre_redirect_half_state": pre_state,
            "redirect_state": state,
            "post_redirect_samples": [],
            "recovery_cycle": None,
            "recovery_target_match": None,
            "source_program_map_matches": self._bin951_source_matches_program_map(redirect),
            "half_program_map_matches": (
                None if half is None or half.get("s1_half_pc") is None
                else self._bin951_pc_crosses_fetch_block(int(half["s1_half_pc"]) << 1)
            ),
            "missing_fields": sorted(
                [
                    f"redirect.{key}"
                    for key, value in redirect.items()
                    if value is None
                ]
                + [
                    f"pre_redirect_half_state[{index}].{key}"
                    for index, sample in enumerate(pre_state)
                    for key, value in sample.items()
                    if key != "cycle" and value is None
                ]
                + [
                    f"redirect_state.{key}"
                    for key, value in state.items()
                    if key != "cycle" and value is None
                ]
            ),
        }

    def _bin951_source_matches_program_map(self, redirect: dict[str, int | None]) -> bool | None:
        pc = redirect.get("pc")
        if pc is None:
            return None
        return self._bin951_pc_crosses_fetch_block(int(pc))

    def _bin951_pc_crosses_fetch_block(self, pc: int) -> bool:
        matches = [
            instruction
            for records in self.instructions_by_line.values()
            for instruction in records
            if int(instruction["pc"]) == pc
        ]
        if not matches:
            return False
        return any(
            "crosses_fetch_block" in instruction.get("boundary_features", [])
            for instruction in matches
        )

    def _finish_bin951_event(self) -> None:
        if self._bin951_active is None:
            return
        active = self._bin951_active
        redirect = active["redirect"]
        recovery = next(
            (
                sample
                for sample in active["post_redirect_samples"]
                if sample.get("s1_valid") == 1
            ),
            None,
        )
        target = redirect.get("target")
        if recovery is not None and target is not None:
            start_pc = recovery.get("s1_fetch_block0_start_pc")
            active["recovery_target_match"] = (
                None if start_pc is None else (int(start_pc) << 1) == int(target)
            )
        half_present = any(
            sample.get("s0_prev_end_is_half_rvi") == 1
            or sample.get("s1_half_valid") == 1
            for sample in active["pre_redirect_half_state"]
        ) or active["redirect_state"].get("s0_prev_end_is_half_rvi") == 1 or active[
            "redirect_state"
        ].get("s1_half_valid") == 1
        isolated = [
            sample for sample in active["post_redirect_samples"]
            if recovery is None or sample["cycle"] < recovery["cycle"]
        ]
        old_payload_suppressed = bool(isolated) and all(
            sample.get("s0_prev_end_is_half_rvi") == 0
            and sample.get("s1_valid") == 0
            and sample.get("s2_valid") == 0
            and sample.get("to_ibuffer_valid") == 0
            for sample in isolated
        )
        active["summary"] = {
            "pre_redirect_half_present": bool(half_present),
            "old_payload_suppressed_until_recovery": bool(old_payload_suppressed),
            "recovery_observed": recovery is not None,
            "recovery_target_match": active["recovery_target_match"],
            "source_program_map_matches": active["source_program_map_matches"],
            "half_program_map_matches": active["half_program_map_matches"],
            "redirect_identity_complete": all(
                redirect.get(key) is not None
                for key in ("pc", "target", "ftq_flag", "ftq_value", "ftq_offset")
            ),
        }
        active["missing_fields"] = sorted(set(active["missing_fields"]) | {
            f"post_redirect_samples[{index}].{key}"
            for index, sample in enumerate(active["post_redirect_samples"])
            for key, value in sample.items()
            if key != "cycle" and value is None
        })
        active["validation_failures"] = validate_bin951_event(active)
        active["status"] = "REALIZED" if not active["validation_failures"] else "UNCONFIRMED"
        self._bin951_events.append(active)
        self._append_timeline(
            {
                "type": BIN951_EVENT_NAME,
                "cycle": active["redirect_cycle"],
                "payload": active,
            }
        )
        if active["status"] == "REALIZED":
            self._observe_realized(
                BIN951_EVENT_NAME,
                active["redirect_cycle"],
                details={
                    "redirect": dict(redirect),
                    **active["summary"],
                    "missing_fields": list(active["missing_fields"]),
                    "status": active["status"],
                },
            )
        self._bin951_active = None

    def observe_cycle(self, cycle: int, env: Any) -> None:
        if _read_signal(getattr(getattr(env, "clock_reset", None), "reset", None)):
            self._discard_reset_pending(cycle, env)
            return
        # The environment drives the next-cycle backend inputs after the
        # clock-edge callbacks.  Use BackendModel's edge snapshot for any
        # relation involving DUT state sampled on this edge; retain the live
        # value only to confirm that a timing action actually held ready low.
        live_can_accept = _read_optional_binary(env.backend_to_ibuf_if.decode_can_accept)
        backend_model = getattr(env, "backend_model", None)
        edge_snapshot = getattr(backend_model, "can_accept_at_edge", None)
        if callable(edge_snapshot):
            try:
                can_accept_at_edge = 1 if int(edge_snapshot(1)) else 0
            except Exception:
                can_accept_at_edge = live_can_accept
        else:
            can_accept_at_edge = live_can_accept
        live_redirect_valid = _read_signal(env.backend_to_ftq_if.redirect_valid, default=0)
        redirect_snapshot = getattr(backend_model, "redirect_valid_at_edge", None)
        if callable(redirect_snapshot):
            try:
                redirect_valid_at_edge = 1 if int(redirect_snapshot(0)) else 0
            except Exception:
                redirect_valid_at_edge = live_redirect_valid
        else:
            redirect_valid_at_edge = live_redirect_valid
        ibuffer_full = _read_ibuffer_full(env)
        self._sample_bin951(cycle, env)
        if live_can_accept is not None and self._last_can_accept != live_can_accept:
            self._record_signal_transition("dut.backend_can_accept", cycle, live_can_accept)
            self._last_can_accept = live_can_accept
        available = ibuffer_full is not None
        if self._last_ibuffer_probe_available != available:
            self._append_timeline({"type": "audit.ibuffer_full_probe", "cycle": int(cycle),
                                   "payload": {"available": available, "value": ibuffer_full}})
            self._last_ibuffer_probe_available = available
        if available and self._last_ibuffer_full != ibuffer_full:
            self._record_signal_transition("dut.ibuffer_full", cycle, ibuffer_full)
        self._last_ibuffer_full = ibuffer_full

        active = self._active_action
        if active is not None:
            _record_pressure_sample(active, cycle, live_can_accept)
        if active is not None and live_can_accept == 0:
            self._observe_realized("dut.backend_backpressure_cycle", cycle)
            if self._last_icache_response_cycle == int(cycle):
                self._observe_realized(
                    "dut.icache_response_x_backend_backpressure",
                    cycle,
                )
        if redirect_valid_at_edge == 1:
            self._observe_realized("dut.backend_redirect_valid", cycle)
            if can_accept_at_edge == 0:
                self._observe_realized("dut.redirect_x_backend_backpressure", cycle)
            if self._last_icache_response_cycle is not None:
                delta = int(cycle) - int(self._last_icache_response_cycle)
                if abs(delta) <= self.timing_plan.relation_window_cycles:
                    self._observe_realized(
                        "dut.icache_response_near_backend_redirect",
                        cycle,
                        details={"delta_cycles": delta},
                    )
        if ibuffer_full == 1 and can_accept_at_edge == 0:
            self._observe_realized("dut.ibuffer_full_x_backend_backpressure", cycle)

        if active is not None and int(cycle) >= int(active["end_cycle_inclusive"]):
            active["status"] = "REALIZED" if active["pressure_window_confirmed"] else "NOT_REALIZED"
            if active["status"] != "REALIZED":
                active["failure_reason"] = "requested_pressure_window_not_fully_observed"
            active["completion_cycle"] = int(cycle)
            if active["status"] == "REALIZED":
                self._observe_realized("dut.backend_backpressure_window", cycle)
            self._append_timeline(
                {
                    "type": "scenario.action_complete",
                    "cycle": int(cycle),
                    "payload": dict(active),
                }
            )
            self._active_action = None
            env.backend_model.set_can_accept(1)

    def _discard_reset_pending(self, cycle: int, env: Any) -> None:
        """Do not connect an old half/redirect/timing action across DUT reset."""
        for action in (self._active_action, self._uncache_stall_action):
            if action is None or action.get("status") not in {"ACTIVE", "RELEASE_PENDING"}:
                continue
            action.update(status="FAIL", failure_reason="dut_reset_interrupted_action", completion_cycle=int(cycle))
            self._append_timeline({"type": "scenario.action_fail", "cycle": int(cycle), "payload": dict(action)})
        if self._active_action is not None:
            env.backend_model.set_can_accept(1)
        if self._uncache_stall_action is not None:
            env.uncache_agent.set_a_ready(None)
        if self._bin951_active is not None:
            self._bin951_active.update(status="UNCONFIRMED", validation_failures=["dut_reset_interrupted_event"])
            self._bin951_events.append(self._bin951_active)
            self._append_timeline({"type": BIN951_EVENT_NAME, "cycle": int(cycle), "payload": self._bin951_active})
        self._bin951_active = None
        self._bin951_history.clear()
        self._bin951_redirect_edges.clear()
        self._active_action = None
        self._uncache_stall_action = None
        self._last_icache_request_cycle = None
        self._last_icache_response_cycle = None
        self._last_redirect_cycle = None
        self._last_can_accept = None
        self._last_ibuffer_full = None
        self._last_ibuffer_probe_available = None
        self._translation_mapping_valid = False

    def _relation_summary(self) -> list[dict[str, Any]]:
        responses = [
            *self._event_cycles.get("handshake.icache_d", []),
            *self._event_cycles.get("handshake.uncache_d", []),
        ]
        redirects = self._event_cycles.get("backend.redirect", [])
        result: list[dict[str, Any]] = []
        window = int(self.timing_plan.relation_window_cycles)
        for redirect_cycle in redirects:
            nearby = [cycle for cycle in responses if abs(int(cycle) - int(redirect_cycle)) <= window]
            if nearby:
                result.append(
                    {
                        "anchor": "backend.redirect",
                        "anchor_cycle": int(redirect_cycle),
                        "event": "handshake.icache_d",
                        "event_cycles": nearby,
                        "deltas": [int(cycle) - int(redirect_cycle) for cycle in nearby],
                    }
                )
        return result

    def realized_events(self) -> tuple[RealizedEvent, ...]:
        result: list[RealizedEvent] = []
        for event, count in sorted(self._realized_counts.items()):
            first = self._realized_first.get(event, {})
            result.append(
                RealizedEvent(
                    event=event,
                    count=int(count),
                    evidence_source="dut_top_level_timeline",
                    first_cycle=(int(first["first_cycle"]) if "first_cycle" in first else None),
                    first_pc=(int(first["first_pc"]) if "first_pc" in first else None),
                    details={
                        key: value
                        for key, value in first.items()
                        if key not in {"first_cycle", "first_pc"}
                    },
                )
            )
        return tuple(result)

    def build_timeline(self, *, execution_status: str, details: dict[str, Any] | None = None) -> dict[str, Any]:
        if self.has_active_action:
            for action in (self._active_action, self._uncache_stall_action):
                if action is None or str(action.get("status") or "") not in {
                    "ACTIVE", "RELEASE_PENDING",
                }:
                    continue
                action["status"] = "FAIL"
                action["failure_reason"] = "run_ended_before_window_completed"
            if self.env is not None and self._uncache_stall_action is not None:
                self.env.uncache_agent.set_a_ready(None)
        plan = self.timing_plan.to_dict()
        return {
            "schema_version": TIMELINE_SCHEMA_VERSION,
            "scenario_id": self.scenario_id,
            "execution_status": str(execution_status),
            "timing_plan": plan,
            "timing_plan_sha256": _stable_payload_sha256(plan),
            "program_map_binary_sha256": str(self.program_map.get("binary_sha256") or ""),
            "selected_boundary_lines": [f"0x{line:x}" for line in self.selected_boundary_lines],
            "program_address_mapping": {
                "schema_version": 1,
                "mode": "static_translation" if self.timing_plan.profile == "translation_timing_cross" else "identity",
                "valid": self._translation_mapping_valid if self.timing_plan.profile == "translation_timing_cross" else True,
                "pages": [{"va": va, "pa": pa} for va, pa in sorted((self._translation_pages or {}).items())],
                "policy": "configured mapping only; aliases unattributed; context/sfence invalidates",
            },
            "event_counts": dict(sorted(self._event_counts.items())),
            "realized_events": [asdict(event) for event in self.realized_events()],
            "actions": self._actions,
            "top_level_signal_transitions": self._observations,
            "bin951_event_contract": {
                "schema_version": BIN951_EVENT_SCHEMA_VERSION,
                "event": "dut.bin951_redirect_half_cleanup",
                "redirect_identity_fields": sorted(BIN951_REDIRECT_SIGNAL_NAMES),
                "ifu_state_fields": sorted(BIN951_IFU_SIGNAL_NAMES),
                "ftq_identity_fields": sorted(BIN951_FTQ_SIGNAL_NAMES),
                "sampled_events": self._bin951_events,
                "active_event_at_end": self._bin951_active,
                "evidence_policy": "missing signals remain null; no inferred zero values",
                "runtime_probe_policy": "getter-alias and __Vtogcov__ probes require raw FST confirmation",
            },
            "relative_event_observations": self._relation_summary(),
            "timeline": self._timeline,
            "maximum_timeline_events": self.maximum_timeline_events,
            "dropped_timeline_events": self._dropped_timeline_events,
            "details": dict(details or {}),
            "evidence_policy": {
                "program_events": "NEMU committed trace",
                "timing_events": "DUT-facing top-level handshakes and inputs",
                "internal_dut_force": False,
                "configured_intent_is_not_realization": True,
            },
        }

    def write_timeline(
        self,
        output: Path,
        *,
        execution_status: str,
        details: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        timeline = self.build_timeline(execution_status=execution_status, details=details)
        write_json_contract(output, timeline)
        return timeline
