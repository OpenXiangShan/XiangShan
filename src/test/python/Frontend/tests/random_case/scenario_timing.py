"""Seeded, replayable timing plans for random scenarios."""
from __future__ import annotations

import random
from dataclasses import asdict, dataclass
from typing import Any

TIMING_PLAN_SCHEMA_VERSION = 1
UINT64_LIMIT = 1 << 64


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
