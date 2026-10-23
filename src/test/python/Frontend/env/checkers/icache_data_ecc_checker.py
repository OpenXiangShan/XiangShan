"""Request-owned DataArray reads and parity faults delivered to IBuffer.

The oracle uses request addresses, physical SRAM reads and returned data/code.
S2 ECC selection, corrupt, BEU and coverage hits do not determine the oracle.
This adapter targets V3's 64-byte line, 32-byte fetch alignment and bank parity.
"""

from __future__ import annotations

from dataclasses import dataclass


def request_bank_masks(start_pc: int, end_position: int) -> tuple[int, int]:
    """endPosition names a halfword relative to the 32-byte aligned block."""
    assert start_pc >= 0 and start_pc % 2 == 0, "unaligned request PC"
    assert 0 <= end_position < 32, "invalid endPosition"
    end_pc = (start_pc & ~31) + 2 * end_position
    assert start_pc <= end_pc < start_pc + 64, "invalid fetch range"
    base = start_pc & ~63
    masks = [0, 0]
    for pc in range(start_pc, end_pc + 1, 2):
        line, offset = divmod(pc - base, 64)
        masks[line] |= 1 << (offset // 8)
    assert not masks[0] & masks[1], "one bank cannot read two lines in one request"
    return tuple(masks)


def parity(data: int) -> int:
    assert 0 <= data < 1 << 64, "data must fit one 64-bit bank"
    return data.bit_count() & 1


@dataclass(frozen=True)
class BankRead:
    valid: int
    ready: int
    waymask: int
    sets: tuple[int, ...]


class DataEccRequestChecker:
    """One measured request; a clean instance also checks the pre-fault baseline."""

    def __init__(self, start_pc, masks, ways, words, faults=()):
        self.start_pc = int(start_pc)
        self.masks = tuple(masks)
        self.ways = tuple(ways)
        # Raw words use the injection agent's {data[63:0], parity, padding} ABI.
        self.words = dict(words)
        self.faults = frozenset(faults)
        self.identity = None
        self.read_cycle = None
        self.response_cycle = None
        self.handoff_cycle = None
        self.ecc_cycle = None
        self.delivery = None
        self.expected_hwe = None
        self.evidence = {}
        selected = {(line, bank) for line, mask in enumerate(self.masks)
                    for bank in range(8) if mask & (1 << bank)}
        assert self.faults <= selected, "injection outside intended request"
        assert selected <= self.words.keys(), "missing SRAM readback"
        assert len(self.ways) == 2 and all(0 <= way < 4 for way in self.ways)
        for key in selected:
            raw = self.words[key]
            bad = parity(raw >> 2) != ((raw >> 1) & 1)
            assert bad == (key in self.faults), ("unexpected SRAM parity", key)

    def check_read(self, *, cycle, start_pc, end_position, identity, req1_valid,
                   ecc_enable, flush, request_masks, banks):
        assert self.identity is None, "duplicate measured request"
        assert start_pc == self.start_pc, "wrong request PC"
        expected = request_bank_masks(start_pc, end_position)
        assert expected == self.masks, ("scenario fetch range differs", expected, self.masks)
        assert tuple(request_masks) == expected, "request bankSel differs from address range"
        assert req1_valid == 0, "scenario requires only req0"
        assert ecc_enable == 1 and flush == 0, "ECC disabled or request flushed"
        assert len(banks) == 8, "missing physical bank read probes"
        base_set = (self.start_pc >> 6) & 255
        for bank, actual in enumerate(banks):
            lines = [line for line, mask in enumerate(expected) if mask & (1 << bank)]
            if not lines:
                assert not actual.valid, ("unexpected bank read", bank)
                continue
            line = lines[0]
            way = self.ways[line]
            assert actual.valid == actual.ready == 1, ("required bank not read", line, bank)
            assert actual.waymask == 1 << way, ("wrong SRAM way", line, bank)
            assert actual.sets[way] == ((base_set + line) & 255), ("wrong SRAM set", line, bank)
        self.identity = tuple(identity)
        self.read_cycle = int(cycle)
        self.evidence["read"] = dict(cycle=cycle, identity=self.identity,
                                     start_pc=start_pc, end_position=end_position,
                                     masks=expected, banks=banks)

    def check_response(self, *, cycle, identity, start_pc, data, codes,
                       sram_valid, mshr_valid):
        assert tuple(identity) == self.identity, "SRAM response belongs to another request"
        assert start_pc == self.start_pc, "SRAM response PC mismatch"
        assert self.response_cycle is None, "duplicate SRAM response"
        assert cycle == self.read_cycle + 1, "SRAM response is not one cycle after read"
        bad_banks = []
        for (line, bank), raw in self.words.items():
            if not self.masks[line] & (1 << bank):
                continue
            assert sram_valid[bank] == 1 and mshr_valid[line] == 0, "bank was supplied by refill"
            assert data[bank] == raw >> 2, ("SRAM data changed", line, bank)
            assert codes[bank] == ((raw >> 1) & 1), ("parity injection not sampled", line, bank)
            if parity(data[bank]) != codes[bank]:
                bad_banks.append((line, bank))
        assert set(bad_banks) == self.faults, "sampled faults differ from injected faults"
        self.expected_hwe = bool(bad_banks)
        self.response_cycle = int(cycle)
        self.evidence["response"] = dict(cycle=cycle, bad_banks=bad_banks,
                                         data=tuple(data), codes=tuple(codes))

    def check_handoff(self, *, cycle, identity, exception, mmio, pbmt, mshr_valid):
        assert tuple(identity) == self.identity, "ICache handoff identity mismatch"
        assert self.response_cycle is not None, "handoff without checked SRAM response"
        assert self.handoff_cycle is None, "duplicate ICache handoff"
        assert exception == 0 and mmio == 0 and pbmt == 0, "another exception or uncache source"
        assert not any(mshr_valid), "refill bypass in SRAM-only scenario"
        self.handoff_cycle = int(cycle)
        self.evidence["handoff"] = dict(cycle=cycle, identity=self.identity,
                                        exception=exception, mmio=mmio, pbmt=pbmt)

    def check_ecc(self, *, cycle, identity, start_pc, cross_line, hits, corrupt):
        """Check the delayed ECC output against independently sampled bank parity."""
        self.evidence["ecc"] = dict(cycle=cycle, identity=tuple(identity),
                                    start_pc=start_pc, cross_line=cross_line,
                                    hits=tuple(hits), corrupt=tuple(corrupt))
        assert tuple(identity) == self.identity, "ECC response identity mismatch"
        assert start_pc == self.start_pc, "ECC response PC mismatch"
        assert self.handoff_cycle is not None and cycle == self.handoff_cycle + 1, "ECC response timing mismatch"
        assert self.ecc_cycle is None, "duplicate ECC response"
        assert cross_line == int(bool(self.masks[1])), "wrong ECC cross-line context"
        assert all(hits[line] == 1 for line, mask in enumerate(self.masks) if mask), "ECC context is not an SRAM hit"
        expected = tuple(int(any(item[0] == line for item in self.faults)) for line in range(2))
        assert tuple(corrupt) == expected, ("ECC corrupt differs from sampled parity", corrupt, expected)
        self.ecc_cycle = int(cycle)

    def check_delivery(self, *, cycle, identity, valid, ready, exception,
                       exception_mask, valid_mask, enq_mask, slots, previous_half):
        # Keep the actual output even when an assertion rejects it, so a missed
        # Hwe has its PC, FTQ identity and zero exception recorded in evidence.
        self.evidence["observed_delivery"] = dict(
            cycle=cycle, identity=tuple(identity), valid=valid, ready=ready,
            exception=exception, exception_mask=exception_mask,
            valid_mask=valid_mask, enq_mask=enq_mask, slots=slots,
            previous_half=previous_half,
        )
        assert tuple(identity) == self.identity, "IBuffer request identity mismatch"
        assert self.handoff_cycle is not None, "IBuffer delivery without ICache handoff"
        assert self.ecc_cycle is not None, "IBuffer delivery without checked ECC response"
        assert self.delivery is None, "duplicate IBuffer delivery"
        assert valid == ready == 1, "IBuffer did not handshake"
        assert previous_half == 0, "previous request left half an instruction"
        assert cycle > self.handoff_cycle, "IBuffer delivery precedes ICache handoff"
        active = valid_mask & enq_mask
        owned = [slot for slot, info in slots.items()
                 if active & (1 << slot) and tuple(info["identity"]) == self.identity]
        assert owned, "no valid enqueued instruction belongs to target request"
        assert slots[min(owned)]["pc"] == self.start_pc, "IBuffer target PC mismatch"
        if self.expected_hwe:
            assert exception == 5, "target request did not carry Hwe"
            assert exception_mask and exception_mask & (exception_mask - 1) == 0, "Hwe mask is not one-hot"
            lane = exception_mask.bit_length() - 1
            assert lane in owned, "Hwe assigned to invalid or unrelated instruction"
            assert slots[lane]["pc"] == self.start_pc, "Hwe assigned to wrong PC"
        else:
            # IBuffer gates exceptionMask with valid/enqEnable. IFU can retain
            # an exception for invalid req1 at the slot after req0's last
            # instruction; those raw bits do not give req0 an exception.
            assert not (exception and exception_mask & active), "clean baseline raised an exception"
        self.delivery = dict(cycle=cycle, identity=tuple(identity),
                             exception=exception, exception_mask=exception_mask,
                             valid_mask=valid_mask, enq_mask=enq_mask, slots=slots)
        self.evidence["delivery"] = self.delivery

    def assert_complete(self):
        assert self.read_cycle is not None, "target SRAM read never occurred"
        assert self.response_cycle is not None, "target SRAM response never occurred"
        assert self.handoff_cycle is not None, "target ICache handoff never occurred"
        assert self.ecc_cycle is not None, "target ECC response never occurred"
        assert self.delivery is not None, "target IBuffer delivery timed out"
