from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass, field


_BPU = "Frontend_top.Frontend.inner_bpu."
_FTQ = "Frontend_top.Frontend.inner_ftq."
_ICACHE = "Frontend_top.Frontend.inner_icache."
_MAINPIPE = _ICACHE + "mainPipe."
_WAYLOOKUP = _ICACHE + "wayLookup."

PREFETCH_DEPTH = 32
BTB_TARGET_COMPARE_WIDTH = 22
MBTB_TARGET_WIDTH = 20
MBTB_WAYS = 4
MBTB_ALIGN_BANKS = 2
VBTB_WAYS_PER_ALIGN_BANK = 16
SC_STORAGE_WAYS = 8
# Physical bank order: four SRAM results followed by one VBTB result.
BTB_RESULT_ENTRIES = MBTB_ALIGN_BANKS * (MBTB_WAYS + 1)
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
        _BPU + "s2_s1Prediction_taken",
        _BPU + "__Vtogcov__s2_s1Prediction_taken",
    ),
    "s3_override": (
        _FTQ + "bpuS3Redirect",
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
        for index in range(BTB_RESULT_ENTRIES)
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
        # Ftq drives ICache redirectFlush directly from redirect_1_valid.
        _FTQ + "redirect_1_valid",
    ),
    "bpu_flush_match": (
        _WAYLOOKUP + "bpuS3FlushValid",
        _WAYLOOKUP + "__Vtogcov__bpuS3FlushValid",
    ),
}

BPU_TARGET_DIFF_SIGNAL_GROUPS: dict[str, tuple[str, ...]] = {
    # All operands belong to the same registered S3 transaction.
    "s3_valid": (_BPU + "s3_valid",),
    "s3_taken": (_BPU + "s3_taken",),
    "s2_taken": (_BPU + "s3_s2Prediction_taken",),
    "s2_override": (_BPU + "s3_s2Override",),
    "s1_target": (_BPU + "s3_s1Prediction_target_addr",),
    "s2_target": (_BPU + "s3_s2Prediction_target_addr",),
    "s3_branch_type": (_BPU + "s3_prediction_attribute_branchType",),
    "s3_ras_action": (_BPU + "s3_prediction_attribute_rasAction",),
    "s3_target": (_BPU + "s3_prediction_target_addr",),
    "s3_start_pc": (_BPU + "s3_startPc_dup_0_addr",),
    "ittage_hit": (_BPU + "ittage.io_prediction_hit",),
    "ras_enable": (_BPU + "ras.io_enable",),
    "s3_override": (_FTQ + "bpuS3Redirect",),
    **{f"s3_first_{i}": (_BPU + f"s3_firstTakenBranchOH_{i}",)
       for i in range(BTB_RESULT_ENTRIES)},
    **{f"s2_first_{i}": (_BPU + f"s3_s2FirstTakenBranchOH_{i}",)
       for i in range(BTB_RESULT_ENTRIES)},
}


def _mbtb_parent_candidates(
    align_bank: int,
    internal_bank: int,
    suffix: str,
) -> tuple[str, ...]:
    base = _BPU + f"mbtb.alignBanks_{align_bank}.internalBanks_{internal_bank}."
    return (base + suffix,)


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
    return (base + suffix,)


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
                suffix = "" if port == 0 else f"_{port}"
                for name, rtl_name in (
                    ("hit_mask", "hitMaskUInt" + suffix),
                    ("dirty_mask", "dirtyUInt"),
                ):
                    # Toggle coverage records transitions, not the current signal.
                    groups[_mbtb_key(align_bank, internal_bank, port_prefix + name)] = (
                        _mbtb_buffer_candidates(align_bank, internal_bank, rtl_name)[0],
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
    s2_taken: int
    s2_override: int
    s1_target: int
    s2_target: int
    s3_branch_type: int
    s3_ras_action: int
    s3_target: int
    s3_start_pc: int
    ittage_hit: int
    ras_enable: int
    s3_first_taken_mask: int
    s2_first_taken_mask: int
    s3_override: int
    signal_paths: dict[str, str]

    @property
    def target_source(self) -> str:
        if self.s3_ras_action == 1 and self.ras_enable:
            return "ras"
        if self.s3_branch_type == 3 and not (self.s3_ras_action & 1) and self.ittage_hit:
            return "ittage"
        return "btb"

    @property
    def target_only_candidate(self) -> bool:
        return bool(self.s3_valid == self.s3_taken == self.s2_taken == 1
                    and self.s3_first_taken_mask == self.s2_first_taken_mask
                    and self.s3_first_taken_mask.bit_count() == 1)

    @property
    def comparison_target(self) -> int:
        return self.s2_target if self.s2_override else self.s1_target

    @property
    def target_diff(self) -> bool:
        if self.target_source in {"ras", "ittage"}:
            return self.s3_target != self.comparison_target
        if self.s2_override:
            return False  # S2 already selected the MBTB target; exact RTL policy.
        mask = (1 << BTB_TARGET_COMPARE_WIDTH) - 1
        return (self.s3_target & mask) != (self.s1_target & mask)

    @property
    def expected_override(self) -> int:
        return int(bool(self.s3_valid and (
            self.s3_taken != self.s2_taken
            or self.s3_first_taken_mask != self.s2_first_taken_mask
            or self.target_diff)))

    @property
    def full_target_diff(self) -> bool:
        return self.s3_target != self.comparison_target

    @property
    def btb_high_only_diff(self) -> bool:
        return bool(self.target_source == "btb" and not self.s2_override
                    and self.full_target_diff and not self.target_diff)


    @property
    def target_comparison_kind(self) -> str:
        """Distinguish full equality from policy-suppressed target differences."""
        if not self.full_target_diff:
            return "equal"
        if self.target_source == "btb" and self.s2_override:
            return "suppressed"
        if self.btb_high_only_diff:
            return "high_only"
        return "diff"


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
    source_values: dict[str, int] = field(default_factory=dict)

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
    *, native_only: bool = False,
) -> tuple[int, str]:
    for name in candidates:
        if native_only and "__Vtogcov__" in name:
            continue
        try:
            value = reader(str(name))
        except Exception:
            value = None
        if value is not None:
            return int(value), str(name)
    raise BpuV3SignalUnavailable(key, candidates)


def sample_bpu_v3_cycle(
    reader: Callable[[str], int | None], *, require_native: bool = False,
) -> BpuV3Cycle:
    """Sample all ten results; require_native=True is the new strict contract.

    Default alias behavior is retained for existing public API consumers;
    legacy/toggle-only sampling does not qualify current canonical evidence.
    """
    values: dict[str, int] = {}
    paths: dict[str, str] = {}
    for key, candidates in BPU_V3_SIGNAL_GROUPS.items():
        values[key], paths[key] = _read_required(reader, key, candidates, native_only=require_native)
        if require_native and values[key] not in (0, 1):
            raise AssertionError(f"BPU single-bit contract exceeded for {key}: {values[key]}")

    return BpuV3Cycle(
        s3_valid=int(values["s3_valid"]),
        s3_s1_prediction_taken=int(values["s3_s1_prediction_taken"]),
        s3_taken_mask=tuple(int(values[f"s3_taken_mask_{index}"]) for index in range(BTB_RESULT_ENTRIES)),
        s3_override=int(values["s3_override"]),
        signal_paths=paths,
    )


def sample_prefetch_depth_cycle(
    reader: Callable[[str], int | None], *, require_native: bool = False,
) -> PrefetchDepthCycle:
    values: dict[str, int] = {}
    paths: dict[str, str] = {}
    for key, candidates in PREFETCH_DEPTH_SIGNAL_GROUPS.items():
        values[key], paths[key] = _read_required(
            reader, key, candidates, native_only=require_native
        )

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
    values, paths = {}, {}
    for key, candidates in BPU_TARGET_DIFF_SIGNAL_GROUPS.items():
        values[key], paths[key] = _read_required(reader, key, candidates, native_only=True)
        assert values[key] >= 0, f"negative native BPU value: {key}"
        if key.startswith(("s3_first_", "s2_first_")) or key in {
            "s3_valid", "s3_taken", "s2_taken", "s2_override", "ittage_hit", "ras_enable", "s3_override"
        }:
            assert values[key] in (0, 1), f"non-bit native BPU value: {key}"
    masks = {stage: sum(values[f"{stage}_first_{i}"] << i for i in range(BTB_RESULT_ENTRIES))
             for stage in ("s3", "s2")}
    if values["s3_valid"]:
        assert all(mask.bit_count() <= 1 for mask in masks.values()), "BPU first-taken mask is not one-hot"
    return BpuTargetDiffCycle(
        **{key: values[key] for key in (
            "s3_valid", "s3_taken", "s2_taken", "s2_override", "s1_target", "s2_target",
            "s3_branch_type", "s3_ras_action", "s3_target", "ittage_hit", "ras_enable", "s3_override")},
        s3_start_pc=values["s3_start_pc"] << 1,
        s3_first_taken_mask=masks["s3"], s2_first_taken_mask=masks["s2"], signal_paths=paths,
    )


def _read_mbtb_hit_signal(reader, *, align_bank, internal_bank, port, field, paths, sources):
    """Read an exact native result, or the frozen WriteBuffer equation's parents.

    This is not an inference from training intent: both bitmasks are observed
    on the DUT in this cycle. Missing/unreadable parents never default to zero.
    """
    key = _mbtb_key(align_bank, internal_bank, f"p{port}_" + field)
    try:
        value, path = _read_required(reader, key, MBTB_WRITE_BUFFER_SIGNAL_GROUPS[key], native_only=True)
    except BpuV3SignalUnavailable:
        base = _BPU + f"mbtb.alignBanks_{align_bank}.internalBanks_{internal_bank}.entryWriteBuffer."
        suffix = "" if port == 0 else f"_{port}"
        hit_path, dirty_path = base + "hitMaskUInt" + suffix, base + "dirtyUInt"
        hit, _ = _read_required(reader, key + "_hit_mask", (hit_path,), native_only=True)
        dirty, _ = _read_required(reader, key + "_dirty_mask", (dirty_path,), native_only=True)
        limit = (1 << (MBTB_WRITE_BUFFER_ROWS * MBTB_WRITE_BUFFER_ENTRIES)) - 1
        assert 0 <= hit <= limit and 0 <= dirty <= limit, "MBTB native mask width"
        assert hit.bit_count() <= 1, "MBTB hit mask must be zero or one-hot"
        sources.update({hit_path: hit, dirty_path: dirty})
        if field == "hit_written":
            value, expression = int(bool(hit & (~dirty & limit))), f"bool({hit_path} & ~{dirty_path})"
        elif field == "hit_not_written":
            value, expression = int(bool(hit & dirty)), f"bool({hit_path} & {dirty_path})"
        else:
            row = int(field.rsplit("_", 1)[1])
            row_mask = (hit >> (row * MBTB_WRITE_BUFFER_ENTRIES)) & ((1 << MBTB_WRITE_BUFFER_ENTRIES) - 1)
            if field.startswith("hit_row_"):
                value, expression = int(bool(row_mask)), f"bool(({hit_path} >> {row * MBTB_WRITE_BUFFER_ENTRIES}) & 15)"
            else:
                # Chisel OHToUInt(0)=0; nonzero mask was checked one-hot above.
                value = row_mask.bit_length() - 1 if row_mask else 0
                expression = f"OHToUInt(({hit_path} >> {row * MBTB_WRITE_BUFFER_ENTRIES}) & 15)"
        path = expression
    else:
        sources[path] = value
    paths[key] = path
    return value


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
            native_only=True,
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
                sources: dict[str, int] = {}
                values: dict[str, int] = {}
                for field in ("write_valid", "hit_mask", "dirty_mask"):
                    key = _mbtb_key(align_bank, internal_bank, prefix + field)
                    values[field], paths[key] = _read_required(
                        reader, key, MBTB_WRITE_BUFFER_SIGNAL_GROUPS[key]
                    )
                    sources[paths[key]] = values[field]
                valid = values["write_valid"]
                hit = values["hit_mask"]
                dirty_mask = values["dirty_mask"]
                width = MBTB_WRITE_BUFFER_ROWS * MBTB_WRITE_BUFFER_ENTRIES
                location = f"a={align_bank} i={internal_bank} p={port}"
                if (valid not in (0, 1) or not 0 <= hit < (1 << width)
                        or not 0 <= dirty_mask < (1 << width)):
                    raise AssertionError(f"invalid MBTB native mask/valid: {location} {values!r}")
                if hit.bit_count() > 1 or (hit and not valid):
                    raise AssertionError(f"invalid MBTB hit mask: {location} {values!r}")
                if not hit or (hit & dirty_mask):
                    continue
                bit = hit.bit_length() - 1
                row, entry = divmod(bit, MBTB_WRITE_BUFFER_ENTRIES)

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
                    native_only=True,
                )
                if dirty != 0:
                    raise AssertionError(
                        f"MBTB dirty bit disagrees with native mask: {dirty_key}={dirty}"
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
                        source_values=sources,
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
    return _read_required(reader, key, MBTB_WRITE_BUFFER_SIGNAL_GROUPS[key], native_only=True)


__all__ = [
    "BPU_V3_SIGNAL_GROUPS",
    "BPU_TARGET_DIFF_SIGNAL_GROUPS",
    "BTB_TARGET_COMPARE_WIDTH",
    "BTB_RESULT_ENTRIES",
    "MBTB_WAYS",
    "VBTB_WAYS_PER_ALIGN_BANK",
    "SC_STORAGE_WAYS",
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
