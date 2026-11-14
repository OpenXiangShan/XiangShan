"""Bit-exact primitives from L1PrefetchComponent / rocket-chip Replacement."""
from collections import Counter
from dataclasses import dataclass


@dataclass(frozen=True)
class RTLConfig:
    vaddr_bits: int = 50  # DefaultConfig: H extension + Sv48x4
    entries: int = 16
    blocks: int = 16
    threshold: int = 12
    queue_entries: int = 16
    train_filter_size: int = 4
    strict_active: bool = True
    decrement: bool = True

    def __post_init__(self):
        if self.entries != 16 or self.blocks != 16 or self.queue_entries != 16:
            raise ValueError("This RTL revision has 16 Stream slots, 16 blocks/region and 16 queue slots/level")
        if not 35 <= self.vaddr_bits <= 63 or not 1 <= self.threshold <= 16:
            raise ValueError("Invalid address width or activation threshold")

    @property
    def va_mask(self):
        return (1 << self.vaddr_bits) - 1

    @property
    def region_mask(self):
        return (1 << (self.vaddr_bits - 10)) - 1


def hash_tag(address):
    """Input is a region number, block number, or unshifted PC (as appropriate)."""
    return (address & 1023) | ((((address >> 10) ^ (address >> 15) ^ (address >> 20)) & 31) << 10)


def bitmap_for(target, width, decrement):
    bit = (target >> 6) & 15
    if decrement:
        return sum(1 << (bit - i) for i in range(min(width, bit + 1)))
    return (((1 << min(width, 16 - bit)) - 1) << bit) & 65535


def bits(mask):
    while mask:
        one = mask & -mask
        yield one.bit_length() - 1
        mask ^= one


class PLRU:
    """Bit layout and zero reset of rocket-chip PseudoLRU, including masked victim."""
    def __init__(self, ways=16):
        if ways < 2 or ways & (ways - 1):
            raise ValueError("PLRU requires a power of two >= 2")
        self.ways, self.state = ways, 0

    def access(self, way):
        size, base, local = self.ways, 0, way
        while size >= 2:
            half, root = size // 2, base + size - 2
            high = local >= half
            self.state = (self.state & ~(1 << root)) | (int(not high) << root)
            if high:
                base += half - 1
                local -= half
            size = half

    def victim(self, allowed=None):
        allowed = (1 << self.ways) - 1 if allowed is None else allowed
        if not allowed:
            raise ValueError("ValidPseudoLRU result is undefined for an empty allowed mask")
        size, base, offset = self.ways, 0, 0
        while size >= 2:
            half, root = size // 2, base + size - 2
            low = bool(allowed & (((1 << half) - 1) << offset))
            high = bool(allowed & (((1 << half) - 1) << (offset + half)))
            choose_high = bool((self.state >> root) & 1) if low and high else high
            if choose_high:
                offset += half
                base += half - 1
            size = half
        return offset


class FastArbiter:
    """utility/FastArbiter.scala pendingMask and rrGrantMask, NOT ordinary RR."""
    def __init__(self, size):
        self.size, self.pending, self.grant = size, 0, 0

    def choose(self, valid):
        if not valid:
            return None
        rr = self.pending & self.grant
        rr_one = rr & -rr
        one = rr_one if rr_one & valid else valid & -valid
        return one.bit_length() - 1

    def fire(self, valid, chosen):
        self.pending = valid & ~(1 << chosen)
        self.grant = ((1 << self.size) - 1) & ~((1 << (chosen + 1)) - 1)


class TwoLevelRR:
    def __init__(self, size=16):
        self.half = size // 2
        self.low, self.high = FastArbiter(self.half), FastArbiter(self.half)
        self.sel_low = False

    def choose(self, valid, advance=True):
        lo = valid & ((1 << self.half) - 1)
        hi = valid >> self.half
        if not (lo or hi):
            return None
        low = self.sel_low if lo and hi else bool(lo)
        arb, mask = (self.low, lo) if low else (self.high, hi)
        selected = arb.choose(mask)
        if advance:
            arb.fire(mask, selected)
            self.sel_low = not bool(hi) if low else bool(lo)
        return selected if low else self.half + selected


class Audit:
    def __init__(self, max_examples=30):
        self.comparisons, self.errors, self.examples = Counter(), Counter(), []
        self.max_examples = max_examples

    def compare(self, group, cycle, expected, actual, fields=None, identity=None):
        fields = expected.keys() if fields is None else fields
        for key in fields:
            self.comparisons[group] += 1
            if expected[key] != actual.get(key):
                self.errors[group + "." + key] += 1
                if len(self.examples) < self.max_examples:
                    self.examples.append(dict(group=group, cycle=cycle, identity=identity, field=key,
                                              replay=expected[key], trace=actual.get(key)))

    def check(self, group, cycle, condition, detail):
        self.comparisons[group] += 1
        if not condition:
            self.errors[group] += 1
            if len(self.examples) < self.max_examples:
                self.examples.append(dict(group=group, cycle=cycle, detail=detail))

    def result(self):
        return dict(status="PASS" if not self.errors else "FAIL", comparisons=dict(self.comparisons),
                    mismatches=dict(self.errors), examples=self.examples)
