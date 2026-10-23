"""Cycle-equivalent aliases for the current V3 ICache simulation interface."""

ICACHE = "Frontend_top.Frontend.inner_icache."
MAIN = ICACHE + "mainPipe."
PREFETCH = ICACHE + "prefetcher."

# Request-local ITLB/PMP exceptions, before registered TileLink errors are merged.
MAIN_EXCEPTIONS = tuple((MAIN + f"s1_exception_{req}_value",) for req in range(2))
# Final to-IFU exceptions include TileLink errors. The generated ICache aliases
# remain readable when the simulator optimizes away MainPipe's output ports.
TO_IFU_EXCEPTIONS = tuple(
    (ICACHE + f"__Vtogcov__io_toIfu_req_bits_info_{req}_icacheMeta_exception_value",)
    for req in range(2)
)

MAIN_S2_END_OFFSETS = tuple((MAIN + f"s2_blkEndOffset_{req}",) for req in range(2))

# Frontend.scala registers io.fencei before delivering it to ICache.
ICACHE_FENCEI = (
    "Frontend_top.Frontend.inner_icache_io_fencei_REG",
    "Frontend_top.Frontend.__Vtogcov__inner_icache_io_fencei_REG",
    ICACHE + "io_fencei",
)
MAIN_PMP_INSTR = (MAIN + "io_pmp_resp_instr", ICACHE + "__Vtogcov__io_pmp_0_resp_instr")
MAIN_PMP_MMIO = (
    MAIN + "io_pmp_resp_mmio",
    # ICacheMainPipe.scala: req.icacheMeta.pmpMmio := s1_pmpMmio.
    ICACHE + "__Vtogcov__io_toIfu_req_bits_info_0_icacheMeta_pmpMmio",
)
TO_IFU_VALID = (MAIN + "io_toIfu_req_valid", ICACHE + "__Vtogcov__io_toIfu_req_valid")
TO_IFU_READY = (MAIN + "io_toIfu_req_ready", ICACHE + "__Vtogcov__io_toIfu_req_ready")
TO_IFU_MAP = (
    MAIN + "io_toIfu_req_bits_maybeRvcMap",
    "Frontend_top.Frontend._inner_icache_io_toIfu_req_bits_maybeRvcMap",
    ICACHE + "__Vtogcov__io_toIfu_req_bits_maybeRvcMap",
)
PREFETCH_S1_FLUSH = (
    PREFETCH + "io_itlbFlushPipe",
    # Includes both global and BPU flush; io.flush alone is not equivalent.
    ICACHE + "__Vtogcov__io_itlbFlushPipe",
)
ITLB_REQ_VALID = (PREFETCH + "io_itlb_req_valid", ICACHE + "__Vtogcov__io_itlb_req_valid")
ITLB_RESP_MISS = (PREFETCH + "io_itlb_resp_bits_miss", ICACHE + "__Vtogcov__io_itlb_resp_bits_miss")


def half_aligned_cross_line(pruned_start, end_position):
    """Helpers.isCrossLine for V3's 64B block / 32B half-align configuration.

    vAddr.addr omits the instruction byte-offset bit: addr[4] is PC[5].
    Unknown inputs must not imply a non-crossing request.
    """
    if pruned_start is None or end_position is None:
        return None
    return (int(pruned_start) >> 4) & (int(end_position) >> 4) & 1


def validate_target_probes(recorder):
    """Fail early for mandatory scalars of explicitly targeted affected bins."""
    targets = set(recorder.coverage_targets.get("bin_ids", ()))
    from .icache_data_ecc_ibuffer_funcov import GROUP, SIGNALS

    data_ecc_targets = {d.bin_id for d in getattr(recorder, "definitions", ())
                        if d.coverage_group == GROUP} & targets
    if data_ecc_targets:
        missing_ecc = {key: candidates for key, candidates in SIGNALS.items()
                       if recorder._read_first_dut_signal(recorder.env.dut, candidates) is None}
        if missing_ecc:
            raise AssertionError({"reason": "required Data ECC probes are unobservable",
                                  "bins": sorted(data_ecc_targets), "missing": missing_ecc})
    requirements = (
        ({720, 726, 731, 734, 737, 758}, "icache_fencei", ICACHE_FENCEI),
        ({608, 634, 759, 760, 761, 763, 768}, "main_pmp_instr", MAIN_PMP_INSTR),
        ({616, 635, 638}, "main_pmp_mmio", MAIN_PMP_MMIO),
        ({613, 616, 635, 638}, "main_exception_0", MAIN_EXCEPTIONS[0]),
        ({613, 616}, "main_exception_1", MAIN_EXCEPTIONS[1]),
        ({642, 773, 774, 775}, "main_s2_end_offset_0", MAIN_S2_END_OFFSETS[0]),
        ({642, 773, 774, 775}, "main_s2_end_offset_1", MAIN_S2_END_OFFSETS[1]),
        ({609, 612, 613, 614, 1139}, "to_ifu_valid", TO_IFU_VALID),
        ({613, 614}, "to_ifu_ready", TO_IFU_READY),
        ({612, 1139}, "to_ifu_maybe_rvc_map", TO_IFU_MAP),
        ({663, 678, 679, 681, 682}, "prefetch_s1_flush", PREFETCH_S1_FLUSH),
        ({678, 679}, "itlb_req_valid", ITLB_REQ_VALID),
        ({678, 679}, "itlb_resp_miss", ITLB_RESP_MISS),
    )
    missing = {}
    for bins, key, candidates in requirements:
        affected = targets.intersection(f"BIN-{number}" for number in bins)
        if affected and recorder._read_first_dut_signal(recorder.env.dut, candidates) is None:
            missing[key] = {"bins": sorted(affected), "candidates": candidates}
    if missing:
        raise AssertionError({"reason": "required ICache probes are unobservable", "missing": missing})
