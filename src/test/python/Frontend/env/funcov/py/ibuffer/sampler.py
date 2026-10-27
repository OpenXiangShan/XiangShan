from __future__ import annotations

def sample_ibuffer_contract(recorder, dut, cycle: int) -> None:
    """Capture alignment/ownership facts without turning them into hits."""
    valid = recorder._read_first_dut_signal(
        dut,
        (
            "Frontend_top.Frontend._inner_ifu_io_toIBuffer_valid",
            "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_valid",
        ),
    )
    enq = recorder._read_first_dut_signal(
        dut,
        (
            "Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_enqEnable",
            "inner_ifu.io_toIBuffer_bits_enqEnable",
            "Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_enqEnable_0",
            "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_enqEnable",
            "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_enqEnable",
        ),
    )
    if valid is None or enq is None:
        return

    # Current V3 exports a packed [35:0] exceptionMask.
    packed = recorder._read_first_dut_signal(
        dut,
        (
            "Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_exceptionMask",
            "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_exceptionMask",
            "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_exceptionMask",
        ),
    )
    if packed is None:
        return

    enq_bits = int(enq)
    mask_bits = int(packed)
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
