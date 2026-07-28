from __future__ import annotations


def sample_ibuffer_contract(recorder, dut, cycle: int) -> None:
    """Capture alignment/ownership facts without turning them into hits."""
    valid = recorder._try_read_dut_signal(dut, "Frontend_top.Frontend.inner_ifu.io_toIBuffer_valid")
    enq = recorder._try_read_dut_signal(
        dut, "Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_enqEnable_0"
    )
    if valid is None or enq is None:
        return

    masks: list[int] = []
    for index in range(35):
        value = recorder._read_first_dut_signal(
            dut,
            (
                f"Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_exceptionMask_{index}",
                f"Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_exceptionMask_{index}",
                f"Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_exceptionMask_{index}",
            ),
        )
        if value is None:
            break
        masks.append(int(value) & 1)
    if not masks:
        return

    enq_bits = int(enq)
    mask_bits = sum(bit << index for index, bit in enumerate(masks))
    invalid_mask = mask_bits & ~enq_bits
    recorder.risk_observations.append(
        {
            "cycle": int(cycle),
            "risk": "ibuffer_exception_mask_enq_alignment",
            "valid": int(valid),
            "enq_enable": enq_bits,
            "exception_mask": mask_bits,
            "mask_without_enq": int(invalid_mask),
            "aligned": invalid_mask == 0,
        }
    )
