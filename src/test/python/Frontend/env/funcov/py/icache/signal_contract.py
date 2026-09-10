"""Cycle-equivalent aliases for the current V3 ICache simulation interface."""

ICACHE = "Frontend_top.Frontend.inner_icache."
MAIN = ICACHE + "mainPipe."
PREFETCH = ICACHE + "prefetcher."

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
    requirements = (
        ({720, 726, 731, 734, 737, 758}, "icache_fencei", ICACHE_FENCEI),
        ({608, 634, 759, 760, 761, 763, 768}, "main_pmp_instr", MAIN_PMP_INSTR),
        ({616, 635, 638}, "main_pmp_mmio", MAIN_PMP_MMIO),
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
