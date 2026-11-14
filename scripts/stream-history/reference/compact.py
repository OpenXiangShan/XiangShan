"""Sub-1KiB integrated history/current-segment policy, software reference.

Only already accepted training requests and native candidate geometry are
inputs. Python integers/debug identities are not hardware storage; capacity()
enumerates every finite architectural field and the two-entry S0->S2 context.
"""
from dataclasses import asdict, dataclass
from .common import PLRU


@dataclass(frozen=True)
class CompactConfig:
    name: str = "compact64"
    entries: int = 64
    ways: int = 4
    tag_bits: int = 16
    length_bits: int = 8
    block_boundary: int = 0
    confidence_l1: int = 2
    confidence_l2: int = 2
    slack: int = 0  # regions, including in block-boundary mode
    learn_rule: int = 0  #0 exact,1 max,2 distinct pair,3 min/max,4 EWMA,5 recent pair,6 hysteretic,7 last complete,8 two-short shrink
    quantize: int = 0  # 0 exact,1 ceiling-power-of-two,2/4/8 ceiling multiple
    overrun: int = 1  # 0 keep fixed prediction,1 evict/relearn,2 fail-open segment
    backtrack: int = 1
    span_revisit: int = 0
    forward_gap: int = 1
    confirm_blocks: int = 2
    gate_blocks: int = 2
    eligible_only: int = 0
    min_length: int = 1  # regions
    max_length_l1: int = 0  # regions;0 disables this safety classifier
    max_length_l2: int = 0
    slack_l1: int = -1  # -1 reuses common slack
    slack_l2: int = -1
    frontier_back_regions: int = -1  # -1 disables;0 requires current frontier region
    require_native_direction: int = 0
    end_gap_regions: int = 0  # smaller ambiguous gaps bypass intervention/learning
    gate_fraction_shift: int = 0  #1/2/3 require half/quarter/eighth observed progress
    max_range_ratio: int = 0  # min/max envelope only;0 disables variation gate
    density_mode: int = 0  #0 off,1 front-region bitmap,2 consecutive-block run
    density_threshold: int = 0
    min_native_count: int = 0  # reuses original Stream bitmap cnt, no added state
    context_tracking_bits: int = 0  #195 in final designs;0 retains legacy sweep encoding
    length_unit_shift: int = 0  # stored linear length unit is2**shift regions/blocks
    log_length: int = 0  # code0=empty; code1..max decode1,2,4,... regions/blocks
    index_hash: int = 0  #1 XOR folds upper PC portions into the set index
    tracker_span_bits: int = 0  #0 full start;else full frontier plus bounded span
    trackers: int = 0  #0 integrated;else separate fully-associative current table
    direction_mode: int = 0  #1 infer only from adjacent region movement
    allocate_eligible_only: int = 0  #0 all;1 history admission;2 history/current admission;hits observe all
    mismatch_budget: int = 0  #1..3 trusted underestimates quarantine until replacement
    pc_transport_bits: int = 0  #500 in audited designs;0 is legacy capacity accounting
    end_confirm_regions: int = 0  #1/2 defer old length until new segment advances this many regions
    gate_regions: int = 0  #minimum signed trigger-region progress from the observed start
    require_uncensored_start: int = 0  #do not filter after a mid-stream current-table allocation
    stateless_fail_open: int = 0  #derive sticky overrun bypass from monotone span; no deferred history change
    omit_initial_offset: int = 0  #region-confirmed direction and region progress require no first-block offset
    min_history_l1: int = 0  #minimum predicted regions to intervene; does not change learning
    min_history_l2: int = 0
    history_evidence: int = 0  #0 learner default,1 exact bound,2 upper half,3 remembered pattern

    def __post_init__(self):
        if self.entries not in (8, 16, 32, 64, 128, 256) or self.ways not in (1, 2, 4, 8):
            raise ValueError("unsupported finite table geometry")
        if self.entries < self.ways or not 1 <= self.tag_bits <= 49 - self.set_bits:
            raise ValueError("invalid PC tag")
        if not 4 <= self.length_bits <= 14 or self.learn_rule not in (0, 1, 2, 3, 4, 5, 6, 7, 8):
            raise ValueError("invalid length encoding/learning rule")
        if any(v not in (0, 1, 2, 3) for v in (self.confidence_l1, self.confidence_l2)):
            raise ValueError("confidence0 disables filtering at that level")
        if self.overrun not in (0, 1, 2) or self.quantize not in (0, 1, 2, 4, 8, 16, 32):
            raise ValueError("invalid learning controls")
        if self.forward_gap < 1 or self.backtrack < 0 or self.confirm_blocks < 1:
            raise ValueError("invalid segmentation controls")
        if self.density_mode not in (0, 1, 2) or not 0 <= self.density_threshold <= 15:
            raise ValueError("invalid density classifier")
        if self.context_tracking_bits not in (0, 195):
            raise ValueError("context state is195bits, or0 for legacy replay only")
        if not 0 <= self.length_unit_shift <= 6:
            raise ValueError("invalid length unit")
        if self.log_length and (self.length_bits not in (4, 5) or self.learn_rule == 4 or self.length_unit_shift or self.quantize not in (0, 1)):
            raise ValueError("log lengths use4/5-bit exponents and non-EWMA learners")
        if self.index_hash not in (0, 1) or self.tracker_span_bits not in (0, 4, 5, 6, 8, 10, 12, 16):
            raise ValueError("invalid index/span representation")
        if self.trackers not in (0, 4, 8, 16, 32):
            raise ValueError("invalid separate current-tracker geometry")
        if self.direction_mode not in (0, 1):
            raise ValueError("invalid direction confirmation")
        if self.allocate_eligible_only not in (0, 1, 2) or self.mismatch_budget not in (0, 1, 2, 3):
            raise ValueError("invalid admission/mismatch guard")
        if self.mismatch_budget and self.overrun != 1:
            raise ValueError("trusted-underestimate guard requires demand-overrun eviction")
        if self.pc_transport_bits not in (0, 500):
            raise ValueError("audited conservative PC transport is500bits")
        if self.end_confirm_regions not in (0, 1, 2):
            raise ValueError("restart confirmation is0/1/2 regions")
        if self.gate_regions not in (0, 1, 2) or self.require_uncensored_start not in (0, 1):
            raise ValueError("invalid observed-start qualification")
        if self.stateless_fail_open not in (0, 1) or (self.stateless_fail_open and (self.overrun != 2 or self.end_confirm_regions)):
            raise ValueError("stateless fail-open requires segment bypass and history fixed inside the segment")
        if self.omit_initial_offset not in (0, 1) or (self.omit_initial_offset and (
                self.block_boundary or self.direction_mode != 1 or self.gate_regions < 1 or
                self.gate_blocks or self.gate_fraction_shift)):
            raise ValueError("offset elision requires region-confirmed direction and region-only progress")
        if any(not 0 <= v <= 65535 for v in (self.min_history_l1, self.min_history_l2)):
            raise ValueError("invalid per-level minimum history length")
        if self.history_evidence not in (0, 1, 2, 3) or (self.history_evidence and self.learn_rule in (3, 4, 6)):
            raise ValueError("independent history evidence requires a non-coupled learner")
        if self.capacity()["total_bits"] > 8192:
            raise ValueError("total added hardware state exceeds1KiB")

    @property
    def set_bits(self):
        return (self.entries // self.ways).bit_length() - 1

    def capacity(self):
        if self.trackers:
            return self.split_capacity()
        units = 44 if self.block_boundary else 40
        fields = dict(pc_tag=self.tag_bits, segment_start=units, frontier=units,
                      history_length=self.length_bits, history_direction=1,
                      confidence=2, current_direction=2, entry_valid=1,
                      left_censored=1, segment_epoch=2, overflow=1)
        if self.tracker_span_bits:
            del fields["segment_start"]
            fields["segment_span"] = self.tracker_span_bits
        if not self.block_boundary and not self.omit_initial_offset:
            fields["initial_block_offset"] = 4
        if self.learn_rule in (2, 3, 4, 5, 8):
            fields["second_length"] = self.length_bits
        if self.overrun == 2 and not self.stateless_fail_open:
            fields["segment_fail_open"] = 1
        if self.mismatch_budget:
            fields["trusted_underestimates"] = self.mismatch_budget.bit_length()
        if self.end_confirm_regions:
            fields.update(pending_length=self.length_bits, pending_direction=1, restart_state=2)
        if self.density_mode == 1:
            fields.update(front_region_bitmap=16, density_qualified=1)
        elif self.density_mode == 2:
            fields.update(last_front_block_offset=4, consecutive_run_count=4, density_qualified=1)
        entry = sum(fields.values())
        replacement = (self.entries // self.ways) * (self.ways - 1)
        # One accepted eligible train/cycle, exactly two S0->S2 snapshots.
        # PC transport and geometry qualifications are explicitly charged below;
        # existing debug-only S2 PC/VA registers are not assumed free.
        pipeline = 2 * ((self.entries - 1).bit_length() + 2 + 1)
        reserve = 16  # reset/control pipeline and conservative implementation headroom
        auxiliary = self.auxiliary_pipeline_bits() if self.pc_transport_bits else 0
        total = self.entries * entry + replacement + pipeline + reserve + self.context_tracking_bits + self.pc_transport_bits + auxiliary
        return dict(fields_per_entry=fields, bits_per_entry=entry,
                    table_bits=self.entries * entry, replacement_bits=replacement,
                    pipeline_bits=pipeline, control_reserve_bits=reserve,
                    context_tracking_bits=self.context_tracking_bits,
                    pc_transport_bits=self.pc_transport_bits, auxiliary_pipeline_bits=auxiliary,
                    total_bits=total, total_bytes=(total + 7) // 8,
                    comparisons_per_lookup=self.ways,
                    note="storage estimate; comparator/mux/add/sub logic area is separate")

    def split_capacity(self):
        units = 44 if self.block_boundary else 40
        history = dict(pc_tag=self.tag_bits, history_length=self.length_bits,
                       history_direction=1, confidence=2, valid=1)
        if self.learn_rule in (2, 3, 4, 5, 8):
            history["second_length"] = self.length_bits
        if self.mismatch_budget:
            history["trusted_underestimates"] = self.mismatch_budget.bit_length()
        current = dict(frontier=units, current_direction=2, left_censored=1,
                       segment_epoch=2, overflow=1, valid=1,
                       history_index=(self.entries-1).bit_length())
        current["segment_span" if self.tracker_span_bits else "segment_start"] = self.tracker_span_bits or units
        if not self.block_boundary and not self.omit_initial_offset:
            current["initial_block_offset"] = 4
        if self.overrun == 2 and not self.stateless_fail_open:
            current["segment_fail_open"] = 1
        if self.end_confirm_regions:
            current.update(pending_length=self.length_bits, pending_direction=1, restart_state=2)
        if self.density_mode == 1:
            current.update(front_region_bitmap=16, density_qualified=1)
        elif self.density_mode == 2:
            current.update(last_front_block_offset=4, consecutive_run_count=4, density_qualified=1)
        hb, cb = sum(history.values()), sum(current.values())
        replacement = (self.entries//self.ways)*(self.ways-1) + self.trackers-1
        pipeline = 2*((self.trackers-1).bit_length()+3)
        auxiliary = self.auxiliary_pipeline_bits() if self.pc_transport_bits else 0
        total = self.entries*hb+self.trackers*cb+replacement+pipeline+16+self.context_tracking_bits+self.pc_transport_bits+auxiliary
        return dict(history_fields=history, current_fields=current, history_bits_per_entry=hb,
                    current_bits_per_entry=cb, history_bits=self.entries*hb,
                    current_bits=self.trackers*cb, table_bits=self.entries*hb+self.trackers*cb,
                    replacement_bits=replacement, pipeline_bits=pipeline,
                    control_reserve_bits=16, context_tracking_bits=self.context_tracking_bits,
                    pc_transport_bits=self.pc_transport_bits, auxiliary_pipeline_bits=auxiliary,
                    total_bits=total, total_bytes=(total+7)//8,
                    comparisons_per_lookup=self.ways, current_pointer_comparisons=self.trackers,
                    note="history eviction invalidates all matching current pointers; no unstored generation signature")

    def auxiliary_pipeline_bits(self):
        # Existing debug trigger-VA/PC registers cannot be assumed free.
        # Combined progress/fraction qualification is immutable within an epoch
        # and can be computed at S0 using the historical direction; two1b stages.
        bits = 2
        if self.frontier_back_regions >= 0:
            bits += 2*(self.tracker_span_bits+1) if self.tracker_span_bits else 80
        if self.min_native_count:
            bits += 8  # two4b original Stream entry-index stages for live cnt lookup
        return bits

    def maximum_length(self):
        return (1 << ((1 << self.length_bits)-2)) if self.log_length else (((1 << self.length_bits)-1) << self.length_unit_shift)


@dataclass(slots=True)
class CompactEntry:
    valid: bool = False
    tag: int = 0
    start: int = 0
    frontier: int = 0
    span: int = 0  # architectural bounded counter when tracker_span_bits>0
    offset: int = 0
    direction: int = 0
    epoch: int = 0
    censored: bool = True
    overflow: bool = False
    fail_open: bool = False
    length: int = 0
    second: int = 0
    hist_direction: int = 0
    confidence: int = 0
    # Diagnostics only, never used for lookup or policy decisions.
    debug_pc: int = 0
    density_bitmap: int = 0
    density_last: int = 0
    density_run: int = 0
    density_qualified: bool = False
    history_index: int = 0
    underestimates: int = 0
    pending_length: int = 0
    pending_direction: int = 0
    restart_state: int = 0  #0 none;1 no old length;2 valid old length;3 interrupted restart


class CompactPolicy:
    def __init__(self, config):
        self.cfg = config
        self.array = [CompactEntry() for _ in range(config.entries)]
        self.replacement = [PLRU(config.ways) if config.ways > 1 else None
                            for _ in range(config.entries // config.ways)]
        self.pipeline = {}
        self.stats = {}
        self.trackers = [CompactEntry() for _ in range(config.trackers)]
        self.tracker_replacement = PLRU(config.trackers) if config.trackers else None

    def count(self, key):
        self.stats[key] = self.stats.get(key, 0) + 1

    def address(self, pc):
        c = self.cfg
        index = pc >> 1
        if c.index_hash:
            index ^= (pc >> (1+c.set_bits)) ^ (pc >> (12+c.set_bits))
        return index & ((1 << c.set_bits) - 1), (pc >> (1 + c.set_bits)) & ((1 << c.tag_bits) - 1)

    def round_length(self, length):
        if not length:
            return 0
        q = self.cfg.quantize
        if self.cfg.log_length:
            return 1 << (length - 1).bit_length()
        value = (1 << (length - 1).bit_length()) if q == 1 else (((length + q - 1) // q) * q if q else length)
        unit = 1 << self.cfg.length_unit_shift
        return ((value+unit-1)//unit)*unit

    def close(self, e):
        c = self.cfg
        self.count("closed")
        valid = not (e.censored or e.overflow or not e.direction)
        length = self.round_length(abs(e.frontier - e.start) + 1) if valid else 0
        scale = 16 if c.block_boundary else 1
        valid &= c.min_length * scale <= length <= c.maximum_length()
        if c.end_confirm_regions:
            interrupted = bool(e.restart_state)
            e.pending_length, e.pending_direction = (length, e.direction) if valid else (0, 0)
            e.restart_state = 3 if interrupted else 2 if valid else 1
            self.count("restart_pending")
            if not valid:
                self.count("censored_close")
            return
        if e.censored or e.overflow or not e.direction:
            self.count("censored_close")
            return
        if length < c.min_length * scale or length > c.maximum_length():
            self.count("unrepresentable_or_short")
            return
        self.learn(e, length, e.direction)

    def learn(self, e, length, direction):
        c = self.cfg
        old_conf, old_dir = e.confidence, e.hist_direction
        old_pred, old_length, old_second = self.predicted_length(e), e.length, e.second
        if not e.confidence or e.hist_direction != direction:
            e.length, e.second, e.hist_direction, e.confidence = length, length if c.learn_rule in (3, 5) else 0, direction, 1
            self.count("seed")
        elif c.learn_rule == 1:
            e.length = max(e.length, length)
            e.confidence = min(3, e.confidence + 1)
            self.count("envelope_confirm")
        elif c.learn_rule == 2:
            if length not in (e.length, e.second):
                e.second, e.length = e.length, length
                e.confidence = 1
                self.count("second_length_seed")
            else:
                e.confidence = min(3, e.confidence + 1)
                self.count("confirm")
        elif c.learn_rule == 3:
            e.length, e.second = max(e.length, length), min(e.second, length)
            e.confidence = min(3, e.confidence + 1)
            self.count("envelope_confirm")
        elif c.learn_rule == 5:
            # A complete, noncensored segment replaces the oldest of two
            # samples even when it matches the most recent length. Old outliers
            # therefore disappear after two subsequent valid observations.
            e.second, e.length = e.length, length
            e.confidence = min(3, e.confidence + 1)
            self.count("recent_pair_update")
        elif c.learn_rule == 7:
            e.length = length
            e.confidence = min(3, e.confidence + 1)
            self.count("last_complete_update")
        elif c.learn_rule == 8:
            # second=0 means no shorter sample pending; valid lengths are >=1.
            if length >= e.length:
                e.length, e.second = length, 0
            elif length == e.second:
                e.length, e.second = length, 0
                self.count("two_short_commit")
            else:
                e.second = length
                self.count("short_pending")
            e.confidence = min(3, e.confidence + 1)
        elif c.learn_rule == 6:
            # One complete length and a two-bit saturating confidence counter.
            # An isolated differing close cannot immediately replace a trusted
            # length; repeated differences exhaust confidence and then reseed.
            if length == e.length:
                e.confidence = min(3, e.confidence + 1)
                self.count("confirm")
            elif e.confidence > 1:
                e.confidence -= 1
                self.count("hysteresis_disagree")
            else:
                e.length, e.confidence = length, 1
                self.count("hysteresis_reseed")
        elif c.learn_rule == 4:
            deviation = abs(length-e.length)
            e.length += (length-e.length) // 4
            e.second += (deviation-e.second) // 4
            e.length = min(c.maximum_length(), self.round_length(e.length))
            e.second = min(c.maximum_length(), self.round_length(e.second))
            e.confidence = min(3, e.confidence + 1)
            self.count("ewma_confirm")
        elif length == e.length:
            e.confidence = min(3, e.confidence + 1)
            self.count("confirm")
        elif c.overrun == 0:
            self.count("fixed_mismatch_ignored")
        else:
            e.length, e.confidence = length, 1
            self.count("reseed_end_mismatch")
        if c.history_evidence:
            supported = (length == old_pred if c.history_evidence == 1 else
                         (old_pred + 1)//2 <= length <= old_pred if c.history_evidence == 2 else
                         length == old_length or (c.learn_rule in (2, 5, 8) and old_second != 0 and length == old_second))
            e.confidence = min(3, old_conf + 1) if old_conf and old_dir == direction and supported else 1
            self.count("evidence_confirm" if e.confidence > 1 else "evidence_restart")

    def observe(self, cycle, train, pc, block, eligible):
        if self.cfg.trackers:
            return self.observe_split(cycle, train, pc, block, eligible)
        c = self.cfg
        if c.eligible_only and not eligible:
            return
        index, tag = self.address(pc)
        base = index * c.ways
        way = next((w for w in range(c.ways) if self.array[base+w].valid and self.array[base+w].tag == tag), None)
        if way is None:
            if c.allocate_eligible_only and not eligible:
                self.prune_pipeline(cycle)
                return
            way = next((w for w in range(c.ways) if not self.array[base+w].valid), None)
            if way is None:
                way = self.replacement[index].victim() if c.ways > 1 else 0
            old = self.array[base+way]
            if old.valid:
                self.count("capacity_censor")
            e = CompactEntry(valid=True, tag=tag, epoch=(old.epoch+1)&3, debug_pc=pc)
            self.array[base+way] = e
            self.start(e, block, True)
            self.count("allocation")
        else:
            e = self.array[base+way]
            if e.debug_pc != pc:
                self.count("pc_tag_alias")
            if not self.advance(e, block):
                if eligible:
                    self.pipeline[cycle] = train, -1, 0
                self.prune_pipeline(cycle)
                if c.ways > 1:
                    self.replacement[index].access(way)
                return
        if c.ways > 1:
            self.replacement[index].access(way)
        self.check_mismatch(e, block)
        if eligible:
            self.pipeline[cycle] = train, base+way, e.epoch
        self.prune_pipeline(cycle)

    def prune_pipeline(self, cycle):
        for old in list(self.pipeline):
            if old <= cycle-2:
                del self.pipeline[old]

    @staticmethod
    def bind_history(current, history):
        # A combinational view, not a duplicate bank of architectural registers.
        for field in ("length", "second", "hist_direction", "confidence", "underestimates"):
            setattr(current, field, getattr(history, field))

    def observe_split(self, cycle, train, pc, block, eligible):
        c = self.cfg
        if c.eligible_only and not eligible:
            return
        index, tag = self.address(pc)
        base = index*c.ways
        way = next((w for w in range(c.ways) if self.array[base+w].valid and self.array[base+w].tag == tag), None)
        if way is None:
            if c.allocate_eligible_only and not eligible:
                self.prune_pipeline(cycle)
                return
            way = next((w for w in range(c.ways) if not self.array[base+w].valid), None)
            if way is None:
                way = self.replacement[index].victim() if c.ways > 1 else 0
                self.count("history_capacity_evict")
            hindex = base+way
            # Invalidate by pointer at the same edge as history replacement.
            # This avoids any hidden unbounded history-generation tag.
            for t in self.trackers:
                if t.valid and t.history_index == hindex:
                    t.valid = False
            self.array[hindex] = CompactEntry(valid=True, tag=tag, debug_pc=pc)
            self.count("allocation")
        else:
            hindex = base+way
            if self.array[hindex].debug_pc != pc:
                self.count("pc_tag_alias")
        if c.ways > 1:
            self.replacement[index].access(way)
        history = self.array[hindex]
        tindex = next((i for i, t in enumerate(self.trackers) if t.valid and t.history_index == hindex), None)
        if tindex is None:
            if c.allocate_eligible_only == 2 and not eligible:
                self.prune_pipeline(cycle)
                return
            tindex = next((i for i, t in enumerate(self.trackers) if not t.valid), None)
            if tindex is None:
                tindex = self.tracker_replacement.victim()
                self.count("current_capacity_censor")
            old = self.trackers[tindex]
            current = CompactEntry(valid=True, epoch=(old.epoch+1)&3, history_index=hindex)
            self.trackers[tindex] = current
            self.start(current, block, True)
            self.bind_history(current, history)
        else:
            current = self.trackers[tindex]
            self.bind_history(current, history)
            if not self.advance(current, block):
                if eligible:
                    self.pipeline[cycle] = train, -1, 0
                self.prune_pipeline(cycle)
                self.tracker_replacement.access(tindex)
                return
        self.check_mismatch(current, block)
        self.bind_history(history, current)  # Commit only the four history fields.
        self.tracker_replacement.access(tindex)
        if eligible:
            self.pipeline[cycle] = train, tindex, current.epoch
        self.prune_pipeline(cycle)

    def advance(self, e, block):
        c = self.cfg
        reg = block >> 4
        start_reg = e.start >> 4 if c.block_boundary else e.start
        front_reg = e.frontier >> 4 if c.block_boundary else e.frontier
        if e.direction > 0:
            within = front_reg-c.backtrack <= reg <= front_reg+c.forward_gap
            within |= bool(c.span_revisit and not (c.tracker_span_bits and e.overflow) and start_reg <= reg <= front_reg)
        elif e.direction < 0:
            within = front_reg-c.forward_gap <= reg <= front_reg+c.backtrack
            within |= bool(c.span_revisit and not (c.tracker_span_bits and e.overflow) and front_reg <= reg <= start_reg)
        else:
            within = start_reg-c.forward_gap <= reg <= start_reg+c.forward_gap
        if not within:
            if abs(reg-front_reg) < c.end_gap_regions:
                self.count("ambiguous_gap_bypass")
                return False
            self.close(e)
            e.epoch = (e.epoch + 1) & 3
            self.start(e, block, bool(c.end_confirm_regions))
            self.count("split")
        else:
            first_block = e.start if c.block_boundary else (e.start << 4) | e.offset
            if not e.direction:
                if c.direction_mode:
                    if reg != start_reg:
                        e.direction = 1 if reg > start_reg else -1
                elif abs(block-first_block) >= c.confirm_blocks:
                    e.direction = 1 if block > first_block else -1
            self.density(e, block, front_reg)
            value = block if c.block_boundary else reg
            previous_front = e.frontier
            if e.direction > 0:
                e.frontier = max(e.frontier, value)
            elif e.direction < 0:
                e.frontier = min(e.frontier, value)
            e.overflow |= abs(e.frontier-e.start) + 1 > c.maximum_length()
            if c.tracker_span_bits:
                # start is reconstructed from the architectural full-width
                # frontier, direction and finite unsigned span. On overflow
                # it loses its original value and every intervention bypasses.
                # With region tracking and forward_gap=1, the only nonzero
                # increment is1: implement as an increment/hold counter.
                span = e.span + abs(e.frontier-previous_front)
                maximum_span = (1 << c.tracker_span_bits)-1
                e.overflow |= span > maximum_span
                e.span = min(span, maximum_span)
                e.start = e.frontier-e.direction*e.span
            if e.restart_state and not e.overflow and abs(e.frontier-e.start) >= c.end_confirm_regions*(16 if c.block_boundary else 1):
                if e.restart_state == 2:
                    self.learn(e, e.pending_length, e.pending_direction)
                    self.count("restart_commit")
                e.censored = e.restart_state == 3
                e.restart_state = 0
                # History can change inside this segment, so invalidate earlier
                # S0 progress qualifications instead of assuming a stable length.
                e.epoch = (e.epoch + 1) & 3
        return True

    def check_mismatch(self, e, block):
        c = self.cfg
        value = block if c.block_boundary else block >> 4
        length = self.predicted_length(e)
        slack = c.slack * (16 if c.block_boundary else 1)
        mismatch = e.confidence and (e.hist_direction * (value-e.start) >= length+slack or
                                     (e.direction and e.direction != e.hist_direction))
        if mismatch and c.overrun == 1:
            threshold = min((v for v in (c.confidence_l1, c.confidence_l2) if v), default=4)
            if c.mismatch_budget and e.confidence >= threshold and e.hist_direction*(value-e.start) >= length+slack:
                before = e.underestimates
                e.underestimates = min(c.mismatch_budget, before+1)
                self.count("trusted_underestimate")
                if before < c.mismatch_budget and e.underestimates == c.mismatch_budget:
                    self.count("quarantine")
            e.confidence = 0
            self.count("overrun_or_direction_evict")
        elif mismatch and c.overrun == 2:
            if not c.stateless_fail_open:
                e.fail_open = True
            self.count("overrun_or_direction_fail_open")

    def start(self, e, block, censored):
        e.start = e.frontier = block if self.cfg.block_boundary else block >> 4
        e.span = 0
        e.offset, e.direction = (0 if self.cfg.omit_initial_offset else block & 15), 0
        e.censored, e.overflow, e.fail_open = censored, False, False
        e.density_bitmap, e.density_last, e.density_run = 1 << (block & 15), block & 15, 1
        e.density_qualified = self.cfg.density_threshold <= 1

    def density(self, e, block, old_front_region):
        c = self.cfg
        if not c.density_mode:
            return
        region, offset = block >> 4, block & 15
        front = region == old_front_region
        forward = bool(e.direction and e.direction*(region-old_front_region) > 0)
        if not front and not forward:
            return
        if c.density_mode == 1:
            e.density_bitmap = (e.density_bitmap if front else 0) | (1 << offset)
            e.density_qualified |= e.density_bitmap.bit_count() >= c.density_threshold
        else:
            last = (old_front_region << 4) | e.density_last
            delta = block-last
            if delta:
                continuous = delta == e.direction if e.direction else abs(delta) == 1
                e.density_run = min(15, e.density_run+1) if continuous else 1
            e.density_last = offset
            e.density_qualified |= e.density_run >= c.density_threshold

    def predicted_length(self, e):
        if self.cfg.learn_rule in (2, 5):
            return max(e.length, e.second)
        if self.cfg.learn_rule == 4:
            return min(self.cfg.maximum_length(), e.length+2*e.second)
        return e.length

    def decide(self, cycle, train, pc, target_block, bitmap, level, native, trigger_block=None, native_decr=None, native_count=0):
        if not native:
            return 0
        c = self.cfg
        threshold = c.confidence_l1 if level == 1 else c.confidence_l2
        if not threshold:
            return bitmap
        if native_count < c.min_native_count:
            return bitmap
        context = self.pipeline.get(cycle-2)
        if context is None or context[0] != train:
            self.count("candidate_missing_pipeline")
            return bitmap
        if context[1] < 0:
            return bitmap
        if c.trackers:
            e = self.trackers[context[1]]
            self.bind_history(e, self.array[e.history_index])
        else:
            e = self.array[context[1]]
        if not e.valid or e.epoch != context[2] or e.confidence < threshold or not e.direction or e.direction != e.hist_direction or e.fail_open or e.restart_state:
            return bitmap
        if c.tracker_span_bits and e.overflow:
            return bitmap
        if c.mismatch_budget and e.underestimates >= c.mismatch_budget:
            return bitmap
        if c.density_mode and not e.density_qualified:
            return bitmap
        if c.require_uncensored_start and e.censored:
            return bitmap
        if c.stateless_fail_open and abs(e.frontier-e.start) >= self.predicted_length(e) + c.slack*(16 if c.block_boundary else 1):
            return bitmap
        first_block = e.start if c.block_boundary else (e.start << 4) | e.offset
        progress = (e.direction * (trigger_block-first_block)) if trigger_block is not None else abs(e.frontier-e.start) * (1 if c.block_boundary else 16)
        if progress < c.gate_blocks:
            return bitmap
        if c.gate_regions:
            start_region = e.start >> 4 if c.block_boundary else e.start
            trigger_region = trigger_block >> 4 if trigger_block is not None else (e.frontier >> 4 if c.block_boundary else e.frontier)
            if e.direction * (trigger_region-start_region) < c.gate_regions:
                return bitmap
        length = self.predicted_length(e)
        scale = 16 if c.block_boundary else 1
        maximum = c.max_length_l1 if level == 1 else c.max_length_l2
        minimum = c.min_history_l1 if level == 1 else c.min_history_l2
        if length < minimum*scale:
            return bitmap
        if maximum and length > maximum*scale:
            return bitmap
        if c.require_native_direction and native_decr != (e.hist_direction < 0):
            return bitmap
        if c.frontier_back_regions >= 0 and trigger_block is not None:
            front_reg = e.frontier >> 4 if c.block_boundary else e.frontier
            distance = e.direction*(front_reg-(trigger_block >> 4))
            if distance < 0 or distance > c.frontier_back_regions:
                return bitmap
        if c.gate_fraction_shift and progress < ((length if c.block_boundary else length*16) >> c.gate_fraction_shift):
            return bitmap
        if c.learn_rule == 3 and c.max_range_ratio and e.length > e.second*c.max_range_ratio:
            return bitmap
        if c.learn_rule == 5 and c.max_range_ratio and max(e.length, e.second) > min(e.length, e.second)*c.max_range_ratio:
            return bitmap
        slack = c.slack_l1 if level == 1 else c.slack_l2
        length += (c.slack if slack < 0 else slack)*scale
        endpoint = e.start + e.direction * (length - 1)
        low, high = min(e.start, endpoint), max(e.start, endpoint)
        self.count("applied_L" + str(level))
        if not c.block_boundary:
            return bitmap if low <= target_block >> 4 <= high else 0
        region_block = target_block & ~15
        return sum((1 << bit) for bit in range(16) if bitmap >> bit & 1 and low <= region_block+bit <= high)

    def flush(self, purge_history=True):
        # Hardware always clears integrated entries; a tracker-only flush would
        # require extra valid bits and a distinct partial-reset policy.
        for e in self.array:
            e.valid = False
            e.epoch = (e.epoch + 1) & 3
        for e in self.trackers:
            e.valid = False
            e.epoch = (e.epoch + 1) & 3
        self.pipeline.clear()
        self.count("flush")

    def result(self):
        return dict(config=asdict(self.cfg), capacity=self.cfg.capacity(), events=self.stats)
