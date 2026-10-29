"""Offline checks of effective fault scope; no DUT fixture is instantiated."""

import pytest

from tests.py.support.ifu_exception_scope import _SCOPE_FIELDS, assert_no_effective_ifu_exception


def _window():
    # Saved old failure values, used only as a synthetic diagnostic regression.
    # They do not qualify this source, or change the original FAIL outcome.
    return {
        "io_toIBuffer_valid": 1, "io_toIBuffer_ready": 1,
        "io_toIBuffer_bits_valid": 0xffff, "io_toIBuffer_bits_enqEnable": 0xffff,
        "io_toIBuffer_bits_exceptionType_value": 5, "io_toIBuffer_bits_exceptionMask": 0x10000,
        "s2_fetchBlock_0_valid": 1, "s2_fetchBlock_1_valid": 0,
        "s2_icacheMeta_0_exception_value": 0, "s2_icacheMeta_1_exception_value": 5,
    }


def test_inactive_error_payload_is_outside_effective_entries():
    result = assert_no_effective_ifu_exception(_window())
    assert result == {
        "valid_blocks": [0], "effective_entry_mask": 0xffff,
        "exception_masked_enq": 0, "exception_type_payload": 5, "effective_exception": False,
    }


@pytest.mark.parametrize("changes,reason", [
    ({"s2_icacheMeta_0_exception_value": 5}, "exception on a valid fetch block"),
    ({"s2_fetchBlock_1_valid": 1}, "exception on a valid fetch block"),
    ({"io_toIBuffer_bits_exceptionMask": 1}, "exception on an enqueued entry"),
    ({"io_toIBuffer_bits_exceptionMask": 0x8000}, "exception on an enqueued entry"),
    ({"io_toIBuffer_bits_valid": 0xff}, "nonempty enqueue"),
    ({"io_toIBuffer_bits_enqEnable": 0}, "nonempty enqueue"),
    ({"io_toIBuffer_valid": 0}, "accepted window"),
    ({"io_toIBuffer_ready": 0}, "accepted window"),
    ({"s2_fetchBlock_0_valid": 0}, "no valid fetch block"),
    ({"s2_fetchBlock_1_valid": 2}, "invalid block valid bit"),
])
def test_effective_errors_and_unaccepted_windows_fail(changes, reason):
    record = {**_window(), **changes}
    with pytest.raises(AssertionError, match=reason):
        assert_no_effective_ifu_exception(record)


@pytest.mark.parametrize("field", _SCOPE_FIELDS)
@pytest.mark.parametrize("invalid", ["absent", None, "x", -1])
def test_missing_or_unreadable_scope_probe_fails(field, invalid):
    record = _window()
    if invalid == "absent":
        del record[field]
    else:
        record[field] = invalid
    with pytest.raises(AssertionError, match="missing_or_invalid_scope_probe"):
        assert_no_effective_ifu_exception(record)


def test_no_exception_type_does_not_interpret_exception_mask_payload():
    record = {**_window(), "io_toIBuffer_bits_exceptionType_value": 0,
              "io_toIBuffer_bits_exceptionMask": 1}
    assert_no_effective_ifu_exception(record)


def test_valid_but_not_enqueued_entry_does_not_report_exception():
    record = {**_window(), "io_toIBuffer_bits_valid": 0x1ffff}
    assert_no_effective_ifu_exception(record)


def test_valid_dual_block_without_fault_is_accepted():
    record = {**_window(), "s2_fetchBlock_1_valid": 1, "s2_icacheMeta_1_exception_value": 0,
              "io_toIBuffer_bits_exceptionType_value": 0}
    assert assert_no_effective_ifu_exception(record)["valid_blocks"] == [0, 1]


def test_satp_diagnostic_uses_scope_and_preserves_context_checks(monkeypatch):
    from tests.py.jiabowen import test_ifu_exception_metadata_v3_dut as diagnostic

    record = {**_window(), "io_toIBuffer_bits_hasSatpFlush": 1,
              "s2_icacheMeta_0_hasSatpFlush": 1, "io_toIBuffer_bits_isBackendException": 0,
              "s2_fetchBlock_0_startVAddr_addr": diagnostic.context._NEW_VA >> 1}
    called = []
    monkeypatch.setattr(diagnostic, "_capture_metadata", lambda env: [record])
    monkeypatch.setattr(diagnostic.context, "test_satp_asid_switch_restarts_translation_from_live_cfvec",
                        lambda env: called.append("context"))
    monkeypatch.setattr(diagnostic, "_save_review", lambda env, records, component: called.append(component))
    diagnostic.test_ifu_exception_metadata_satp_without_fault_diagnostic(object())
    assert called == ["context", "satp_without_fault"]
    assert record["effective_exception_scope"]["effective_exception"] is False
    record["s2_fetchBlock_0_startVAddr_addr"] += 1
    with pytest.raises(AssertionError):
        diagnostic.test_ifu_exception_metadata_satp_without_fault_diagnostic(object())
    assert called.count("satp_without_fault") == 1


@pytest.mark.parametrize("stem", [
    "io_toIBuffer_bits_valid", "io_toIBuffer_bits_exceptionMask",
    "s2_fetchBlock_0_valid", "s2_fetchBlock_1_valid", "s2_icacheMeta_1_exception_value",
    "s2_alignedInstrVec_0_blockSel", "s2_alignedInstrVec_0_isCrossBlockInstr",
    "io_toIBuffer_bits_ftqPtr_0_flag", "io_toIBuffer_bits_ftqPtr_0_value",
])
def test_capture_requires_new_scope_probes_before_interpreting_payload(stem):
    from types import SimpleNamespace
    from tests.py.jiabowen import test_ifu_exception_metadata_v3_dut as diagnostic

    observers = []

    class Coverage:
        def _try_read_dut_signal(self, dut, path):
            if path.endswith(stem):
                return None
            return 1

    env = SimpleNamespace(
        dut=SimpleNamespace(reset=SimpleNamespace(value=0)),
        functional_coverage=Coverage(), register_cycle_observer=observers.append,
    )
    # The collector must fail if every native alias returns unreadable.
    # _read_ifu_internal_with_path must not synthesize a default zero.
    records = diagnostic._capture_metadata(env)
    with pytest.raises(AssertionError, match="missing_metadata_probe"):
        observers[0](10, env)
    assert records == []
