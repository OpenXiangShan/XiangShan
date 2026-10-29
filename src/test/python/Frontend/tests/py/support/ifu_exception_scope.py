"""Check a captured, accepted IFU window without interpreting inactive payload."""

from collections.abc import Mapping


_SCOPE_FIELDS = (
    "io_toIBuffer_valid", "io_toIBuffer_ready",
    "io_toIBuffer_bits_valid", "io_toIBuffer_bits_enqEnable",
    "io_toIBuffer_bits_exceptionType_value", "io_toIBuffer_bits_exceptionMask",
    "s2_fetchBlock_0_valid", "s2_fetchBlock_1_valid",
    "s2_icacheMeta_0_exception_value", "s2_icacheMeta_1_exception_value",
)


def assert_no_effective_ifu_exception(record: Mapping) -> dict:
    """Require no fault on valid blocks or accepted entries; fail on missing data.

    Ifu.scala merges ECC payload even for an invalid block. IBuffer qualifies
    exceptionType with exceptionMask & enqEnable (and with valid on bypass).
    Keep both boundaries checked, so masking cannot conceal a valid block fault.
    This is one accepted IFU window only. Type/priority, subsequent queue or
    bypass delivery and recovery require separate evidence; this function
    cannot qualify a whole satp scenario or reclassify prior FAIL results.
    """
    values = {}
    for field in _SCOPE_FIELDS:
        value = record.get(field)
        assert type(value) is int and value >= 0, {"missing_or_invalid_scope_probe": field, "value": value}
        values[field] = value
    assert values["io_toIBuffer_valid"] == values["io_toIBuffer_ready"] == 1, {
        "reason": "scope requires an accepted window", "values": values,
    }
    valid = values["io_toIBuffer_bits_valid"]
    enq = values["io_toIBuffer_bits_enqEnable"]
    assert enq and (enq & ~valid) == 0, {"reason": "nonempty enqueue must be within valid entries", "values": values}
    valid_blocks = []
    for block in range(2):
        block_valid = values[f"s2_fetchBlock_{block}_valid"]
        assert block_valid in (0, 1), {"reason": "invalid block valid bit", "block": block}
        if block_valid:
            valid_blocks.append(block)
            assert values[f"s2_icacheMeta_{block}_exception_value"] == 0, {
                "reason": "exception on a valid fetch block", "block": block, "values": values,
            }
    assert valid_blocks, {"reason": "accepted window has no valid fetch block", "values": values}
    exception = values["io_toIBuffer_bits_exceptionType_value"]
    masked_enq = values["io_toIBuffer_bits_exceptionMask"] & enq
    assert exception == 0 or masked_enq == 0, {
        "reason": "exception on an enqueued entry", "values": values, "masked_enq": masked_enq,
    }
    return {
        "valid_blocks": valid_blocks, "effective_entry_mask": valid & enq,
        "exception_masked_enq": masked_enq,
        "exception_type_payload": exception, "effective_exception": False,
    }
