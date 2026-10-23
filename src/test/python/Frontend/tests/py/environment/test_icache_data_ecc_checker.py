"""Negative contracts: omitted reads, missed parity and wrong request ownership."""

from dataclasses import replace

import pytest

from env.checkers.icache_data_ecc_checker import (
    BankRead, DataEccRequestChecker, parity, request_bank_masks,
)


_PC = 0x800A0034
_ID = (1, 10)


def _fixture(faults=((1, 2),)):
    masks = (0xC0, 7)
    words = {}
    banks = []
    for bank in range(8):
        lines = [line for line, mask in enumerate(masks) if mask & (1 << bank)]
        if not lines:
            banks.append(BankRead(0, 1, 0, (0, 0, 0, 0)))
            continue
        line = lines[0]
        data = 0x1300000013 + 2 * bank
        words[line, bank] = (data << 2) | ((parity(data) ^ int((line, bank) in faults)) << 1)
        banks.append(BankRead(1, 1, 1 << line, (line, line, line, line)))
    checker = DataEccRequestChecker(_PC, masks, (0, 1), words, faults)
    read = dict(cycle=10, start_pc=_PC, end_position=26, identity=_ID,
                req1_valid=0, ecc_enable=1, flush=0, request_masks=masks, banks=banks)
    return checker, read


def _response(checker):
    data, codes = [0] * 8, [0] * 8
    for (_, bank), raw in checker.words.items():
        data[bank], codes[bank] = raw >> 2, (raw >> 1) & 1
    return dict(cycle=11, identity=_ID, start_pc=_PC, data=data, codes=codes,
                sram_valid=[1] * 8, mshr_valid=(0, 0))


def _ready(faults=((1, 2),)):
    checker, read = _fixture(faults)
    checker.check_read(**read)
    checker.check_response(**_response(checker))
    checker.check_handoff(cycle=11, identity=_ID, exception=0, mmio=0, pbmt=0, mshr_valid=(0, 0))
    checker.check_ecc(**_ecc(checker))
    return checker


def _ecc(checker):
    return dict(cycle=12, identity=_ID, start_pc=_PC, cross_line=1, hits=(1, 1),
                corrupt=tuple(int(any(line == i for line, bank in checker.faults)) for i in range(2)))


def _delivery(checker):
    return dict(cycle=13, identity=_ID, valid=1, ready=1,
                exception=5 if checker.expected_hwe else 0,
                exception_mask=8 if checker.expected_hwe else 0,
                valid_mask=8, enq_mask=8, previous_half=0,
                slots={3: {"identity": _ID, "pc": _PC}})


@pytest.mark.parametrize("pc,end,masks", [
    (0x800A0000, 31, (255, 0)),
    (0x800A0020, 31, (0xF0, 0x0F)),
    (_PC, 26, (0xC0, 7)),
    (0x800A003C, 16, (0x80, 1)),
    (0x800A0034, 15, (0xC0, 0)),
])
def test_address_range_oracle_handles_half_aligned_end_position(pc, end, masks):
    assert request_bank_masks(pc, end) == masks


@pytest.mark.parametrize("faults", [(), ((1, 1),), ((1, 2),), ((0, 6), (1, 2))])
def test_clean_and_faulted_requests_deliver_on_a_nonzero_lane(faults):
    checker = _ready(faults)
    assert checker.expected_hwe == bool(faults)
    checker.check_delivery(**_delivery(checker))
    checker.assert_complete()


@pytest.mark.parametrize("exception,mask", [(5, 16), (0, 8)])
def test_clean_baseline_uses_ibuffer_exception_validity_gates(exception, mask):
    checker = _ready(())
    delivery = _delivery(checker)
    delivery.update(exception=exception, exception_mask=mask)
    checker.check_delivery(**delivery)
    checker.assert_complete()


def test_clean_baseline_rejects_an_exception_on_an_active_lane():
    checker = _ready(())
    delivery = _delivery(checker)
    delivery.update(exception=5, exception_mask=8)
    with pytest.raises(AssertionError, match="clean baseline raised"):
        checker.check_delivery(**delivery)


def test_bank2_omitted_from_physical_reads_is_a_failure_before_hwe_check():
    checker, read = _fixture()
    read["banks"][2] = replace(read["banks"][2], valid=0)
    with pytest.raises(AssertionError, match="required bank not read"):
        checker.check_read(**read)
    assert checker.expected_hwe is None
    with pytest.raises(AssertionError, match="read never occurred"):
        checker.assert_complete()


def test_bank2_omitted_from_request_bank_sel_does_not_shrink_the_oracle():
    checker, read = _fixture()
    read["request_masks"] = (0xC0, 1)
    with pytest.raises(AssertionError, match="bankSel differs"):
        checker.check_read(**read)


@pytest.mark.parametrize("change,error", [
    ({"waymask": 1}, "wrong SRAM way"),
    ({"sets": (0, 0, 0, 0)}, "wrong SRAM set"),
    ({"ready": 0}, "required bank not read"),
])
def test_bank2_read_must_use_the_injected_set_way_and_handshake(change, error):
    checker, read = _fixture()
    read["banks"][2] = replace(read["banks"][2], **change)
    with pytest.raises(AssertionError, match=error):
        checker.check_read(**read)


def test_bank2_read_with_missing_hwe_is_detected_without_corrupt_or_beu_inputs():
    checker = _ready()
    delivery = _delivery(checker)
    delivery["exception"] = 0
    delivery["exception_mask"] = 0
    with pytest.raises(AssertionError, match="did not carry Hwe"):
        checker.check_delivery(**delivery)


@pytest.mark.parametrize("change,error", [
    ({"identity": (0, 10)}, "identity mismatch"),
    ({"exception_mask": 16}, "unrelated instruction"),
    ({"exception_mask": 24}, "not one-hot"),
    ({"valid_mask": 0}, "no valid enqueued"),
    ({"enq_mask": 0}, "no valid enqueued"),
    ({"ready": 0}, "did not handshake"),
    ({"previous_half": 1}, "half an instruction"),
    ({"slots": {3: {"identity": _ID, "pc": _PC + 4}}}, "PC mismatch"),
    ({"slots": {3: {"identity": (1, 11), "pc": _PC}}}, "no valid enqueued"),
])
def test_an_unowned_or_unaccepted_hwe_cannot_satisfy_the_request(change, error):
    checker = _ready()
    delivery = _delivery(checker)
    delivery.update(change)
    with pytest.raises(AssertionError, match=error):
        checker.check_delivery(**delivery)


@pytest.mark.parametrize("change,error", [
    ({"identity": (1, 11)}, "another request"),
    ({"cycle": 12}, "one cycle"),
    ({"mshr_valid": (0, 1)}, "supplied by refill"),
])
def test_refill_and_stale_responses_cannot_establish_the_parity_expectation(change, error):
    checker, read = _fixture()
    checker.check_read(**read)
    response = _response(checker)
    response.update(change)
    with pytest.raises(AssertionError, match=error):
        checker.check_response(**response)


def test_injection_overwritten_before_response_is_rejected():
    checker, read = _fixture()
    checker.check_read(**read)
    response = _response(checker)
    response["codes"][2] ^= 1
    with pytest.raises(AssertionError, match="injection not sampled"):
        checker.check_response(**response)


def test_missing_delivery_and_duplicate_delivery_fail():
    checker = _ready()
    with pytest.raises(AssertionError, match="timed out"):
        checker.assert_complete()
    checker.check_delivery(**_delivery(checker))
    with pytest.raises(AssertionError, match="duplicate IBuffer"):
        checker.check_delivery(**_delivery(checker))


@pytest.mark.parametrize("change,error", [
    ({"corrupt": (0, 0)}, "corrupt differs"),
    ({"corrupt": (1, 1)}, "corrupt differs"),
    ({"identity": (0, 10)}, "identity mismatch"),
    ({"start_pc": _PC + 2}, "PC mismatch"),
    ({"cycle": 13}, "timing mismatch"),
    ({"cross_line": 0}, "cross-line context"),
    ({"hits": (1, 0)}, "not an SRAM hit"),
])
def test_delayed_ecc_matches_request_and_independently_sampled_parity(change, error):
    checker, read = _fixture()
    checker.check_read(**read)
    checker.check_response(**_response(checker))
    checker.check_handoff(cycle=11, identity=_ID, exception=0, mmio=0, pbmt=0, mshr_valid=(0, 0))
    ecc = _ecc(checker)
    ecc.update(change)
    with pytest.raises(AssertionError, match=error):
        checker.check_ecc(**ecc)


@pytest.mark.parametrize("change,error", [
    ({"req1_valid": 1}, "only req0"),
    ({"ecc_enable": 0}, "ECC disabled"),
    ({"flush": 1}, "request flushed"),
])
def test_invalid_scenario_preconditions_do_not_establish_a_read(change, error):
    checker, read = _fixture()
    read.update(change)
    with pytest.raises(AssertionError, match=error):
        checker.check_read(**read)
