"""Compatibility exports for the modular frontend functional coverage samplers.

New code should import samplers from their module directories.  These exports
keep existing tests and callers working while the old monolithic implementation
is retired.
"""

from .py.ifu.sampler import (
    CFVEC_SAMPLER_BIN_KEYS as IFU_CFVEC_SAMPLER_BIN_KEYS,
    _classify_cfi_kind,
    sample_cfvec_coverage,
)
from .py.ftq.sampler import (
    TWO_FETCH_COVERPOINTS,
    TWO_FETCH_SAMPLER_BIN_KEYS,
    _TWO_FETCH_SIGNALS,
    sample_two_fetch_coverage,
)

# Legacy registry compatibility: these 17 keys remain part of the existing
# pilot contract while the modular IFU sampler exposes its complete 26-key set.
CFVEC_SAMPLER_BIN_KEYS = frozenset(
    {
        ("ifu_instr_size_type", "rvi_seen"),
        ("ifu_instr_size_type", "rvc_seen"),
        ("ifu_instr_size_type", "mixed_rvi_rvc_seen"),
        ("ifu_pc_step_type", "step_4b_rvi"),
        ("ifu_pc_step_type", "step_2b_rvc"),
        ("ifu_pc_step_type", "mixed_no_gap_no_dup"),
        ("ifu_boundary_event", "rvc_start"),
        ("ifu_boundary_event", "rvi_start"),
        ("ifu_boundary_event", "rvi_high_half_suppressed"),
        ("ifu_fetch_block_position", "head"),
        ("ifu_fetch_block_position", "mid"),
        ("ifu_fetch_block_position", "tail"),
        ("ifu_cfi_decode_type", "non_cfi"),
        ("ifu_cfi_decode_type", "branch"),
        ("ifu_cfi_decode_type", "jal"),
        ("uncache_page_boundary", "rvc_tail_no_resend_before_delivery"),
        ("uncache_page_boundary", "rvi_tail_resend_next_page"),
    }
)

__all__ = [
    "CFVEC_SAMPLER_BIN_KEYS",
    "IFU_CFVEC_SAMPLER_BIN_KEYS",
    "TWO_FETCH_COVERPOINTS",
    "TWO_FETCH_SAMPLER_BIN_KEYS",
    "sample_cfvec_coverage",
    "sample_two_fetch_coverage",
]
