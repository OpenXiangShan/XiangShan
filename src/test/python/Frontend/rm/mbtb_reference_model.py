"""Transaction-level mBTB model from XiangShan-Design-Doc BPU/mbtb.md.

Design document snapshot: 92428b64be5a7e016621e74203bcce104d36635c.
Addresses are bytes. ``touch`` accepts the BPU's final taken prediction;
pipeline timing and the write buffer are outside this functional model.
"""

from dataclasses import dataclass, field


_REGION_SIZE = 32
_FETCH_SIZE = 64
_NUM_WAYS = 4
_TARGET_LOW_MASK = (1 << 21) - 1  # TargetWidth=20 half-word bits.
_SET_MASK = (1 << 11) - 1  # align bank, internal bank, and set index.


@dataclass(frozen=True)
class MbtbEntry:
    tag: int
    position: int
    target_low: int
    target_carry: int
    branch_type: int
    ras_action: int
    taken_count: int
    outcome_streak: int
    last_taken: bool


@dataclass(frozen=True)
class MbtbPrediction:
    cfi_pc: int
    cfi_position: int
    target: int
    branch_type: int
    ras_action: int
    taken: bool = True
    direction_known: bool = field(default=False, compare=False)


class MbtbReferenceModel:
    def __init__(self) -> None:
        self.entries: dict[tuple[int, int], MbtbEntry] = {}
        self.lru: dict[tuple[int, int], list[int]] = {}
        self.pending_metadata: dict[tuple[int, int], list[MbtbEntry]] = {}

    def reset(self) -> None:
        self.entries.clear()
        self.lru.clear()
        self.pending_metadata.clear()

    @staticmethod
    def set_index(cfi_pc: int) -> int:
        return (int(cfi_pc) >> 5) & _SET_MASK

    @staticmethod
    def _replacer_index(cfi_pc: int) -> tuple[int, int]:
        # Each align bank has its own LRU; its index is address bits 13:6.
        return ((int(cfi_pc) >> 5) & 1, (int(cfi_pc) >> 6) & 0xFF)

    @staticmethod
    def _tag_position(cfi_pc: int) -> tuple[int, int]:
        return ((int(cfi_pc) >> 16) & 0xFFFF, (int(cfi_pc) >> 1) & 0xF)

    def _find_way(self, cfi_pc: int) -> int | None:
        set_index = self.set_index(cfi_pc)
        tag, position = self._tag_position(cfi_pc)
        for way in range(_NUM_WAYS):
            entry = self.entries.get((set_index, way))
            if entry is not None and (entry.tag, entry.position) == (tag, position):
                return way
        return None

    def _touch_way(self, cfi_pc: int, way: int) -> None:
        order = self.lru.setdefault(self._replacer_index(cfi_pc), list(range(_NUM_WAYS)))
        order.remove(way)
        order.append(way)

    def touch(self, cfi_pc: int) -> bool:
        """Mark a final taken prediction as recently used; misses do nothing."""
        way = self._find_way(cfi_pc)
        if way is None:
            return False
        self._touch_way(cfi_pc, way)
        return True

    @staticmethod
    def update_counter(count: int, taken: bool) -> int:
        return min(3, count + 1) if taken else max(0, count - 1)

    def train(
        self, cfi_pc: int, *, target: int, branch_type: int, ras_action: int,
        taken: bool = True, start_pc: int | None = None,
    ) -> None:
        if branch_type not in (1, 2, 3):
            return
        set_index = self.set_index(cfi_pc)
        way = self._find_way(cfi_pc)
        previous = self.entries.get((set_index, way)) if way is not None else None
        if way is None:
            if not taken:
                return
            way = self.lru.setdefault(
                self._replacer_index(cfi_pc), list(range(_NUM_WAYS))
            )[0]
            self.pending_metadata.pop((set_index, way), None)

        # The document specifies a 2-bit counter but not its allocation value.
        # Initialize weakly toward the training outcome.
        if previous is not None and previous.branch_type == 1 and branch_type == 1:
            count = self.update_counter(previous.taken_count, taken)
            streak = min(3, previous.outcome_streak + 1) if previous.last_taken == taken else 1
        else:
            count = 2 if taken else 1
            streak = 1
        target_upper = int(target) & ~_TARGET_LOW_MASK
        train_upper = int(cfi_pc if start_pc is None else start_pc) & ~_TARGET_LOW_MASK
        target_carry = 1 if target_upper > train_upper else 2 if target_upper < train_upper else 0
        target_low = int(target) & _TARGET_LOW_MASK
        if previous is not None and not taken:
            # A fall-through training target is not the branch's jump target.
            target_low = previous.target_low
            target_carry = previous.target_carry
        tag, position = self._tag_position(cfi_pc)
        entry = MbtbEntry(
            tag, position, target_low, target_carry,
            int(branch_type), int(ras_action), count, streak, bool(taken),
        )
        if previous is not None and self._metadata(previous) != self._metadata(entry):
            versions = self.pending_metadata.setdefault((set_index, way), [previous])
            versions.append(entry)
        self.entries[(set_index, way)] = entry
        self._touch_way(cfi_pc, way)

    @staticmethod
    def _metadata(entry: MbtbEntry) -> tuple[int, int, int, int]:
        return (entry.target_low, entry.target_carry, entry.branch_type, entry.ras_action)

    @staticmethod
    def _prediction_metadata(entry: MbtbEntry, start_pc: int) -> tuple[int, int, int]:
        upper = int(start_pc) & ~_TARGET_LOW_MASK
        if entry.target_carry == 1:
            upper += 1 << 21
        elif entry.target_carry == 2:
            upper -= 1 << 21
        return (upper | entry.target_low, entry.branch_type, entry.ras_action)

    def allowed_metadata(self, cfi_pc: int, start_pc: int) -> tuple[tuple[int, int, int], ...]:
        way = self._find_way(cfi_pc)
        if way is None:
            return ()
        key = (self.set_index(cfi_pc), way)
        versions = self.pending_metadata.get(key, [self.entries[key]])
        return tuple(self._prediction_metadata(entry, start_pc) for entry in versions)

    def observe_metadata(self, cfi_pc: int, start_pc: int, actual: tuple[int, int, int]) -> None:
        way = self._find_way(cfi_pc)
        if way is None:
            return
        key = (self.set_index(cfi_pc), way)
        versions = self.pending_metadata.get(key)
        if versions is None:
            return
        for index in range(len(versions) - 1, -1, -1):
            if self._prediction_metadata(versions[index], start_pc) == actual:
                remaining = versions[index:]
                if len(remaining) == 1:
                    self.pending_metadata.pop(key, None)
                else:
                    self.pending_metadata[key] = remaining
                return

    def predict(self, start_pc: int) -> tuple[MbtbPrediction, ...]:
        start = int(start_pc)
        end = (start + _FETCH_SIZE) & ~(_REGION_SIZE - 1)
        if (start >> 12) != ((start + _FETCH_SIZE) >> 12):
            end = min(end, (start + _FETCH_SIZE) & ~0xFFF)
        predictions = []
        for region in range(start & ~(_REGION_SIZE - 1), end, _REGION_SIZE):
            set_index = self.set_index(region)
            tag, _ = self._tag_position(region)
            for way in range(_NUM_WAYS):
                entry = self.entries.get((set_index, way))
                if entry is None or entry.tag != tag:
                    continue
                cfi_pc = region + (entry.position << 1)
                if not start <= cfi_pc < end:
                    continue
                predictions.append(MbtbPrediction(
                    cfi_pc=cfi_pc,
                    cfi_position=(cfi_pc - (start & ~(_REGION_SIZE - 1))) >> 1,
                    target=self._prediction_metadata(entry, start)[0],
                    branch_type=entry.branch_type,
                    ras_action=entry.ras_action,
                    taken=entry.taken_count >= 2,
                    direction_known=entry.branch_type == 1 and entry.outcome_streak >= 3,
                ))
        return tuple(sorted(predictions, key=lambda prediction: prediction.cfi_pc))
