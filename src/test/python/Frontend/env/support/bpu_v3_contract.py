from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass


_BPU = "Frontend_top.Frontend.inner_bpu."
_ICACHE = "Frontend_top.Frontend.inner_icache."
_MAINPIPE = _ICACHE + "mainPipe."
_WAYLOOKUP = _ICACHE + "wayLookup."

PREFETCH_DEPTH = 32

BPU_V3_SIGNAL_GROUPS: dict[str, tuple[str, ...]] = {
    "s3_valid": (
        _BPU + "s3_valid",
        _BPU + "__Vtogcov__s3_valid",
    ),
    "s3_s1_prediction_taken": (
        _BPU + "s3_s1Prediction_taken",
        _BPU + "__Vtogcov__s3_s1Prediction_taken",
    ),
    "s3_override": (
        _ICACHE + "__Vtogcov__io_fromFtq_flushFromBpu_s3_valid",
        _MAINPIPE + "io_flushFromBpu_s3_valid",
        _WAYLOOKUP + "bpuS3FlushValid",
        _WAYLOOKUP + "__Vtogcov__bpuS3FlushValid",
    ),
    **{
        f"s3_taken_mask_{index}": (
            _BPU + f"s3_takenMask_{index}",
            _BPU + f"__Vtogcov__s3_takenMask_{index}",
        )
        for index in range(8)
    },
}

PREFETCH_DEPTH_SIGNAL_GROUPS: dict[str, tuple[str, ...]] = {
    "num_valid_entries": (
        _WAYLOOKUP + "numValidEntries",
        _WAYLOOKUP + "__Vtogcov__numValidEntries",
    ),
    "read_ptr_flag": (
        _WAYLOOKUP + "readPtr_flag",
        _WAYLOOKUP + "__Vtogcov__readPtr_flag",
    ),
    "read_ptr_value": (
        _WAYLOOKUP + "readPtr_value",
        _WAYLOOKUP + "__Vtogcov__readPtr_value",
    ),
    "write_ptr_flag": (
        _WAYLOOKUP + "writePtr_flag",
        _WAYLOOKUP + "__Vtogcov__writePtr_flag",
    ),
    "write_ptr_value": (
        _WAYLOOKUP + "writePtr_value",
        _WAYLOOKUP + "__Vtogcov__writePtr_value",
    ),
    "write0_valid": (
        _ICACHE + "prefetcher.io_wayLookupWrite_0_valid",
        _ICACHE + "prefetcher.__Vtogcov__io_wayLookupWrite_0_valid",
    ),
    "shared_write_ready": (
        _ICACHE + "prefetcher.io_wayLookupWrite_0_ready",
        _ICACHE + "prefetcher.__Vtogcov__io_wayLookupWrite_0_ready",
    ),
    "write1_valid": (
        _ICACHE + "prefetcher.io_wayLookupWrite_1_valid",
        _ICACHE + "prefetcher.__Vtogcov__io_wayLookupWrite_1_valid",
    ),
    "to_mainpipe_valid": (
        _MAINPIPE + "io_fromWayLookup_valid",
        _MAINPIPE + "__Vtogcov__io_fromWayLookup_valid",
    ),
    "to_mainpipe_ready": (
        _MAINPIPE + "io_fromWayLookup_ready",
        _MAINPIPE + "__Vtogcov__io_fromWayLookup_ready",
    ),
    "global_flush": (
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",
    ),
    "bpu_flush_match": (
        _WAYLOOKUP + "bpuS3FlushValid",
        _WAYLOOKUP + "__Vtogcov__bpuS3FlushValid",
    ),
}


class BpuV3SignalUnavailable(AssertionError):
    def __init__(self, key: str, candidates: tuple[str, ...]) -> None:
        self.key = str(key)
        self.candidates = tuple(str(name) for name in candidates)
        super().__init__(
            f"required BPU V3 signal group {self.key!r} is unavailable; "
            f"candidates={list(self.candidates)!r}"
        )


@dataclass(frozen=True)
class BpuV3Cycle:
    s3_valid: int
    s3_s1_prediction_taken: int
    s3_taken_mask: tuple[int, ...]
    s3_override: int
    signal_paths: dict[str, str]

    @property
    def is_all_not_taken_candidate(self) -> bool:
        return (
            self.s3_valid == 1
            and self.s3_s1_prediction_taken == 0
            and not any(self.s3_taken_mask)
        )


@dataclass(frozen=True)
class PrefetchDepthCycle:
    num_valid_entries: int
    read_ptr: tuple[int, int]
    write_ptr: tuple[int, int]
    write0_valid: int
    write1_valid: int
    shared_write_ready: int
    to_mainpipe_valid: int
    to_mainpipe_ready: int
    global_flush: int
    bpu_flush_match: int
    signal_paths: dict[str, str]

    @property
    def full(self) -> bool:
        return self.num_valid_entries == PREFETCH_DEPTH

    @property
    def one_slot_left(self) -> bool:
        return self.num_valid_entries == PREFETCH_DEPTH - 1

    @property
    def dual_write_fire(self) -> bool:
        return bool(
            self.write0_valid
            and self.write1_valid
            and self.shared_write_ready
        )


def _read_required(
    reader: Callable[[str], int | None],
    key: str,
    candidates: tuple[str, ...],
) -> tuple[int, str]:
    for name in candidates:
        try:
            value = reader(str(name))
        except Exception:
            value = None
        if value is not None:
            return int(value), str(name)
    raise BpuV3SignalUnavailable(key, candidates)


def sample_bpu_v3_cycle(reader: Callable[[str], int | None]) -> BpuV3Cycle:
    values: dict[str, int] = {}
    paths: dict[str, str] = {}
    for key, candidates in BPU_V3_SIGNAL_GROUPS.items():
        values[key], paths[key] = _read_required(reader, key, candidates)

    return BpuV3Cycle(
        s3_valid=int(values["s3_valid"]),
        s3_s1_prediction_taken=int(values["s3_s1_prediction_taken"]),
        s3_taken_mask=tuple(int(values[f"s3_taken_mask_{index}"]) for index in range(8)),
        s3_override=int(values["s3_override"]),
        signal_paths=paths,
    )


def sample_prefetch_depth_cycle(
    reader: Callable[[str], int | None],
) -> PrefetchDepthCycle:
    values: dict[str, int] = {}
    paths: dict[str, str] = {}
    for key, candidates in PREFETCH_DEPTH_SIGNAL_GROUPS.items():
        values[key], paths[key] = _read_required(reader, key, candidates)

    return PrefetchDepthCycle(
        num_valid_entries=int(values["num_valid_entries"]),
        read_ptr=(int(values["read_ptr_flag"]), int(values["read_ptr_value"])),
        write_ptr=(int(values["write_ptr_flag"]), int(values["write_ptr_value"])),
        write0_valid=int(values["write0_valid"]),
        write1_valid=int(values["write1_valid"]),
        shared_write_ready=int(values["shared_write_ready"]),
        to_mainpipe_valid=int(values["to_mainpipe_valid"]),
        to_mainpipe_ready=int(values["to_mainpipe_ready"]),
        global_flush=int(values["global_flush"]),
        bpu_flush_match=int(values["bpu_flush_match"]),
        signal_paths=paths,
    )


__all__ = [
    "BPU_V3_SIGNAL_GROUPS",
    "PREFETCH_DEPTH",
    "PREFETCH_DEPTH_SIGNAL_GROUPS",
    "BpuV3Cycle",
    "BpuV3SignalUnavailable",
    "PrefetchDepthCycle",
    "sample_bpu_v3_cycle",
    "sample_prefetch_depth_cycle",
]
