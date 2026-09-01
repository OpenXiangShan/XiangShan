from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass


_BPU = "Frontend_top.Frontend.inner_bpu."
_FTQ = "Frontend_top.Frontend.inner_ftq."
_ICACHE = "Frontend_top.Frontend.inner_icache."
_MAINPIPE = _ICACHE + "mainPipe."
_WAYLOOKUP = _ICACHE + "wayLookup."

PREFETCH_DEPTH = 32
BTB_TARGET_COMPARE_WIDTH = 22
MBTB_TARGET_WIDTH = 20
MBTB_ALIGN_BANKS = 2
MBTB_INTERNAL_BANKS = 4
MBTB_WRITE_PORTS = 4
MBTB_WRITE_BUFFER_ROWS = 4
MBTB_WRITE_BUFFER_ENTRIES = 4

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

BPU_TARGET_DIFF_SIGNAL_GROUPS: dict[str, tuple[str, ...]] = {
    "s3_valid": BPU_V3_SIGNAL_GROUPS["s3_valid"],
    "s3_taken": (_BPU + "__Vtogcov__s3_taken",),
    "s1_taken": (
        _BPU + "s3_s1Prediction_taken",
        _BPU + "__Vtogcov__s3_s1Prediction_taken",
    ),
    "s1_cfi_position": (
        _BPU + "s3_s1Prediction_cfiPosition",
        _BPU + "__Vtogcov__s3_s1Prediction_cfiPosition",
    ),
    "s1_branch_type": (
        _BPU + "s3_s1Prediction_attribute_branchType",
        _BPU + "__Vtogcov__s3_s1Prediction_attribute_branchType",
    ),
    "s1_ras_action": (
        _BPU + "s3_s1Prediction_attribute_rasAction",
        _BPU + "__Vtogcov__s3_s1Prediction_attribute_rasAction",
    ),
    "s1_target": (
        _BPU + "s3_s1Prediction_target_addr",
        _BPU + "__Vtogcov__s3_s1Prediction_target_addr",
    ),
    "s3_cfi_position": (
        _BPU + "s3_firstTakenBranch_bits_cfiPosition",
        _BPU + "__Vtogcov__s3_firstTakenBranch_bits_cfiPosition",
    ),
    "s3_branch_type": (
        _BPU + "s3_firstTakenBranch_bits_attribute_branchType",
        _BPU + "__Vtogcov__s3_firstTakenBranch_bits_attribute_branchType",
    ),
    "s3_ras_action": (
        _BPU + "s3_firstTakenBranch_bits_attribute_rasAction",
        _BPU + "__Vtogcov__s3_firstTakenBranch_bits_attribute_rasAction",
    ),
    "s3_target": (
        _BPU + "s3_prediction_target_addr",
        _BPU + "__Vtogcov__s3_prediction_target_addr",
    ),
    "ittage_hit": (
        _BPU + "ittage.io_prediction_hit",
        _BPU + "ittage.__Vtogcov__io_prediction_hit",
    ),
    "s3_override": (
        _FTQ + "bpuS3Redirect",
        _FTQ + "__Vtogcov__bpuS3Redirect",
        *BPU_V3_SIGNAL_GROUPS["s3_override"],
    ),
}


def _mbtb_parent_candidates(
    align_bank: int,
    internal_bank: int,
    suffix: str,
) -> tuple[str, ...]:
    base = _BPU + f"mbtb.alignBanks_{align_bank}.internalBanks_{internal_bank}."
    return (base + suffix, base + "__Vtogcov__" + suffix)


def _mbtb_buffer_candidates(
    align_bank: int,
    internal_bank: int,
    suffix: str,
) -> tuple[str, ...]:
    base = (
        _BPU
        + f"mbtb.alignBanks_{align_bank}.internalBanks_{internal_bank}."
        + "entryWriteBuffer."
    )
    return (base + suffix, base + "__Vtogcov__" + suffix)


def _mbtb_key(
    align_bank: int,
    internal_bank: int,
    component: str,
) -> str:
    return f"mbtb_a{align_bank}_i{internal_bank}_{component}"


def _build_mbtb_write_buffer_signal_groups() -> dict[str, tuple[str, ...]]:
    groups: dict[str, tuple[str, ...]] = {}
    input_fields = {
        "setIdx": "setIdx_r",
        "entry_tag": "entry_r_tag",
        "entry_attribute_branchType": "entry_r_attribute_branchType",
        "entry_attribute_rasAction": "entry_r_attribute_rasAction",
        "entry_position": "entry_r_position",
        "entry_targetCarry_value": "entry_r_targetCarry_value",
        "entry_targetLowerBits": "entry_r_targetLowerBits",
    }
    stored_fields = (
        "setIdx",
        "entry_tag",
        "entry_attribute_branchType",
        "entry_attribute_rasAction",
        "entry_position",
        "entry_targetCarry_value",
        "entry_targetLowerBits",
    )
    for align_bank in range(MBTB_ALIGN_BANKS):
        for internal_bank in range(MBTB_INTERNAL_BANKS):
            for port in range(MBTB_WRITE_PORTS):
                port_prefix = f"p{port}_"
                groups[_mbtb_key(align_bank, internal_bank, port_prefix + "write_valid")] = (
                    _mbtb_parent_candidates(
                        align_bank,
                        internal_bank,
                        f"entryWriteBuffer_io_write_{port}_valid_REG",
                    )
                )
                hit_suffix = "" if port == 0 else f"_{port}"
                groups[_mbtb_key(align_bank, internal_bank, port_prefix + "hit_written")] = (
                    _mbtb_buffer_candidates(
                        align_bank,
                        internal_bank,
                        "hitWritten" + hit_suffix,
                    )
                )
                groups[_mbtb_key(align_bank, internal_bank, port_prefix + "hit_not_written")] = (
                    _mbtb_buffer_candidates(
                        align_bank,
                        internal_bank,
                        "hitNotWritten" + hit_suffix,
                    )
                )
                for field, rtl_field in input_fields.items():
                    groups[_mbtb_key(align_bank, internal_bank, port_prefix + "input_" + field)] = (
                        _mbtb_parent_candidates(
                            align_bank,
                            internal_bank,
                            f"entryWriteBuffer_io_write_{port}_bits_{rtl_field}",
                        )
                    )
                for row in range(MBTB_WRITE_BUFFER_ROWS):
                    row_suffix = f"_{row}" if port == 0 else f"_{port}_{row}"
                    groups[_mbtb_key(align_bank, internal_bank, port_prefix + f"hit_row_{row}")] = (
                        _mbtb_buffer_candidates(
                            align_bank,
                            internal_bank,
                            "hitRowsVec" + row_suffix,
                        )
                    )
                    groups[_mbtb_key(align_bank, internal_bank, port_prefix + f"hit_index_{row}")] = (
                        _mbtb_buffer_candidates(
                            align_bank,
                            internal_bank,
                            "hitRowIdxVec" + row_suffix,
                        )
                    )
            for row in range(MBTB_WRITE_BUFFER_ROWS):
                for entry in range(MBTB_WRITE_BUFFER_ENTRIES):
                    entry_prefix = f"r{row}_e{entry}_"
                    groups[_mbtb_key(align_bank, internal_bank, entry_prefix + "dirty")] = (
                        _mbtb_buffer_candidates(
                            align_bank,
                            internal_bank,
                            f"dirty_{row}_{entry}",
                        )
                    )
                    for field in stored_fields:
                        groups[_mbtb_key(align_bank, internal_bank, entry_prefix + field)] = (
                            _mbtb_buffer_candidates(
                                align_bank,
                                internal_bank,
                                f"entries_{row}_{entry}_{field}",
                            )
                        )
    return groups


MBTB_WRITE_BUFFER_SIGNAL_GROUPS = _build_mbtb_write_buffer_signal_groups()


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


@dataclass(frozen=True)
class BpuTargetDiffCycle:
    s3_valid: int
    s3_taken: int
    s1_taken: int
    s1_cfi_position: int
    s1_branch_type: int
    s1_ras_action: int
    s1_target: int
    s3_cfi_position: int
    s3_branch_type: int
    s3_ras_action: int
    s3_target: int
    ittage_hit: int
    s3_override: int
    signal_paths: dict[str, str]

    @property
    def target_source(self) -> str:
        if self.s3_ras_action == 1:
            return "ras"
        has_pop = bool(self.s3_ras_action & 0x1)
        if self.s3_branch_type == 3 and not has_pop and self.ittage_hit == 1:
            return "ittage"
        return "btb"

    @property
    def target_only_candidate(self) -> bool:
        return bool(
            self.s3_valid == 1
            and self.s3_taken == 1
            and self.s1_taken == 1
            and self.s3_cfi_position == self.s1_cfi_position
            and self.s3_branch_type == self.s1_branch_type
            and self.s3_ras_action == self.s1_ras_action
        )

    @property
    def target_diff(self) -> bool:
        if self.target_source in {"ras", "ittage"}:
            return self.s3_target != self.s1_target
        mask = (1 << BTB_TARGET_COMPARE_WIDTH) - 1
        return (self.s3_target & mask) != (self.s1_target & mask)

    @property
    def full_target_diff(self) -> bool:
        return self.s3_target != self.s1_target

    @property
    def btb_high_only_diff(self) -> bool:
        return bool(
            self.target_source == "btb"
            and self.full_target_diff
            and not self.target_diff
        )


@dataclass(frozen=True)
class MbtbWriteEntry:
    set_idx: int
    tag: int
    branch_type: int
    ras_action: int
    position: int
    target_carry: int
    target_lower: int

    @property
    def identity(self) -> tuple[int, int, int]:
        return (self.set_idx, self.tag, self.position)

    @property
    def compare_bits(self) -> tuple[int, int, int, int]:
        return (
            self.branch_type,
            self.ras_action,
            self.target_lower,
            self.target_carry,
        )


@dataclass(frozen=True)
class MbtbWriteBufferEvent:
    align_bank: int
    internal_bank: int
    port: int
    row: int
    entry: int
    dirty: int
    incoming: MbtbWriteEntry
    stored: MbtbWriteEntry
    signal_paths: dict[str, str]

    @property
    def identity_matches(self) -> bool:
        return self.incoming.identity == self.stored.identity

    @property
    def semantic_changed(self) -> bool:
        return self.incoming.compare_bits != self.stored.compare_bits


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


def sample_bpu_target_diff_cycle(
    reader: Callable[[str], int | None],
) -> BpuTargetDiffCycle:
    values: dict[str, int] = {}
    paths: dict[str, str] = {}
    for key, candidates in BPU_TARGET_DIFF_SIGNAL_GROUPS.items():
        values[key], paths[key] = _read_required(reader, key, candidates)

    return BpuTargetDiffCycle(
        s3_valid=int(values["s3_valid"]),
        s3_taken=int(values["s3_taken"]),
        s1_taken=int(values["s1_taken"]),
        s1_cfi_position=int(values["s1_cfi_position"]),
        s1_branch_type=int(values["s1_branch_type"]),
        s1_ras_action=int(values["s1_ras_action"]),
        s1_target=int(values["s1_target"]),
        s3_cfi_position=int(values["s3_cfi_position"]),
        s3_branch_type=int(values["s3_branch_type"]),
        s3_ras_action=int(values["s3_ras_action"]),
        s3_target=int(values["s3_target"]),
        ittage_hit=int(values["ittage_hit"]),
        s3_override=int(values["s3_override"]),
        signal_paths=paths,
    )


def _read_mbtb_entry(
    reader: Callable[[str], int | None],
    *,
    align_bank: int,
    internal_bank: int,
    component_prefix: str,
    paths: dict[str, str],
) -> MbtbWriteEntry:
    field_names = {
        "set_idx": "setIdx",
        "tag": "entry_tag",
        "branch_type": "entry_attribute_branchType",
        "ras_action": "entry_attribute_rasAction",
        "position": "entry_position",
        "target_carry": "entry_targetCarry_value",
        "target_lower": "entry_targetLowerBits",
    }
    values: dict[str, int] = {}
    for attribute, field in field_names.items():
        key = _mbtb_key(
            align_bank,
            internal_bank,
            component_prefix + field,
        )
        values[attribute], paths[key] = _read_required(
            reader,
            key,
            MBTB_WRITE_BUFFER_SIGNAL_GROUPS[key],
        )
    return MbtbWriteEntry(**values)


def sample_mbtb_write_buffer_events(
    reader: Callable[[str], int | None],
) -> tuple[MbtbWriteBufferEvent, ...]:
    events: list[MbtbWriteBufferEvent] = []
    for align_bank in range(MBTB_ALIGN_BANKS):
        for internal_bank in range(MBTB_INTERNAL_BANKS):
            for port in range(MBTB_WRITE_PORTS):
                prefix = f"p{port}_"
                paths: dict[str, str] = {}
                values: dict[str, int] = {}
                for field in ("write_valid", "hit_written", "hit_not_written"):
                    key = _mbtb_key(
                        align_bank,
                        internal_bank,
                        prefix + field,
                    )
                    values[field], paths[key] = _read_required(
                        reader,
                        key,
                        MBTB_WRITE_BUFFER_SIGNAL_GROUPS[key],
                    )
                if values["hit_written"] != 1:
                    continue
                if values["write_valid"] != 1 or values["hit_not_written"] != 0:
                    raise AssertionError(
                        "inconsistent MBTB WriteBuffer hitWritten observation: "
                        f"a={align_bank} i={internal_bank} p={port} values={values!r}"
                    )

                hit_rows: list[int] = []
                hit_indices: dict[int, int] = {}
                for row in range(MBTB_WRITE_BUFFER_ROWS):
                    row_key = _mbtb_key(
                        align_bank,
                        internal_bank,
                        prefix + f"hit_row_{row}",
                    )
                    row_value, paths[row_key] = _read_required(
                        reader,
                        row_key,
                        MBTB_WRITE_BUFFER_SIGNAL_GROUPS[row_key],
                    )
                    index_key = _mbtb_key(
                        align_bank,
                        internal_bank,
                        prefix + f"hit_index_{row}",
                    )
                    hit_indices[row], paths[index_key] = _read_required(
                        reader,
                        index_key,
                        MBTB_WRITE_BUFFER_SIGNAL_GROUPS[index_key],
                    )
                    if row_value == 1:
                        hit_rows.append(row)
                if len(hit_rows) != 1:
                    raise AssertionError(
                        "MBTB WriteBuffer hitWritten did not identify exactly one row: "
                        f"a={align_bank} i={internal_bank} p={port} rows={hit_rows!r}"
                    )
                row = hit_rows[0]
                entry = int(hit_indices[row])
                if not 0 <= entry < MBTB_WRITE_BUFFER_ENTRIES:
                    raise AssertionError(
                        "MBTB WriteBuffer hit index is out of range: "
                        f"a={align_bank} i={internal_bank} p={port} entry={entry}"
                    )

                incoming = _read_mbtb_entry(
                    reader,
                    align_bank=align_bank,
                    internal_bank=internal_bank,
                    component_prefix=prefix + "input_",
                    paths=paths,
                )
                stored = _read_mbtb_entry(
                    reader,
                    align_bank=align_bank,
                    internal_bank=internal_bank,
                    component_prefix=f"r{row}_e{entry}_",
                    paths=paths,
                )
                dirty_key = _mbtb_key(
                    align_bank,
                    internal_bank,
                    f"r{row}_e{entry}_dirty",
                )
                dirty, paths[dirty_key] = _read_required(
                    reader,
                    dirty_key,
                    MBTB_WRITE_BUFFER_SIGNAL_GROUPS[dirty_key],
                )
                events.append(
                    MbtbWriteBufferEvent(
                        align_bank=align_bank,
                        internal_bank=internal_bank,
                        port=port,
                        row=row,
                        entry=entry,
                        dirty=int(dirty),
                        incoming=incoming,
                        stored=stored,
                        signal_paths=paths,
                    )
                )
    return tuple(events)


def read_mbtb_write_buffer_dirty(
    reader: Callable[[str], int | None],
    event: MbtbWriteBufferEvent,
) -> tuple[int, str]:
    key = _mbtb_key(
        event.align_bank,
        event.internal_bank,
        f"r{event.row}_e{event.entry}_dirty",
    )
    return _read_required(reader, key, MBTB_WRITE_BUFFER_SIGNAL_GROUPS[key])


__all__ = [
    "BPU_V3_SIGNAL_GROUPS",
    "BPU_TARGET_DIFF_SIGNAL_GROUPS",
    "BTB_TARGET_COMPARE_WIDTH",
    "MBTB_ALIGN_BANKS",
    "MBTB_INTERNAL_BANKS",
    "MBTB_TARGET_WIDTH",
    "MBTB_WRITE_BUFFER_ENTRIES",
    "MBTB_WRITE_BUFFER_ROWS",
    "MBTB_WRITE_BUFFER_SIGNAL_GROUPS",
    "MBTB_WRITE_PORTS",
    "PREFETCH_DEPTH",
    "PREFETCH_DEPTH_SIGNAL_GROUPS",
    "BpuTargetDiffCycle",
    "BpuV3Cycle",
    "BpuV3SignalUnavailable",
    "MbtbWriteBufferEvent",
    "MbtbWriteEntry",
    "PrefetchDepthCycle",
    "read_mbtb_write_buffer_dirty",
    "sample_bpu_target_diff_cycle",
    "sample_bpu_v3_cycle",
    "sample_mbtb_write_buffer_events",
    "sample_prefetch_depth_cycle",
]
