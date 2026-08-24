"""Compatibility exports for the modular frontend functional coverage samplers.

This is a Python import-compatibility layer for recorder consumers and
existing tests that import symbols from ``env.funcov``. The actual group
implementations now live below ``env.funcov.py.ifu`` and
``env.funcov.py.ftq``; this module re-exports their public symbols so those
callers do not need a flag-day import rewrite.  It does not bridge DUT
versions, simulators, or two different coverage semantics.
"""

from .py.ifu.sampler import (
    CFVEC_SAMPLER_BIN_KEYS as IFU_CFVEC_SAMPLER_BIN_KEYS,
    IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS,
    OWNER_V3_BIN_SPECS,
    OWNER_V3_COVERPOINT,
    OWNER_V3_COVERPOINTS,
    OWNER_V3_EVENT_TYPE,
    OWNER_V3_SAMPLER_BIN_KEYS,
    MMIO_V3_COVERPOINTS,
    MMIO_V3_SAMPLER_BIN_KEYS,
    _classify_cfi_kind,
    handle_owner_v3_event,
    initialize_mmio_v3_coverage_state,
    reset_mmio_v3_coverage_state,
    sample_mmio_v3_coverage,
    initialize_ifu_cacheable_pipeline_state,
    reset_ifu_cacheable_pipeline_state,
    sample_ifu_cacheable_pipeline_coverage,
    sample_cfvec_coverage,
)
from .py.ftq.sampler import (
    TWO_FETCH_COVERPOINTS,
    TWO_FETCH_SAMPLER_BIN_KEYS,
    _TWO_FETCH_SIGNALS,
    sample_two_fetch_coverage,
)

# Compatibility names used by the pre-refactor contract tests.  These signals
# are intentionally optional because the generated Verilator contract does
# not expose the historical way-data conflict internals.
TWO_FETCH_OPTIONAL_SIGNAL_KEYS = frozenset({"way_data_conflict"})
_WAY_DATA_CONFLICT_SIGNALS = frozenset()

# Legacy registry compatibility: these 17 keys remain part of the existing
# pilot contract while the modular IFU sampler exports the additional models.
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
    "IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS",
    "OWNER_V3_BIN_SPECS",
    "OWNER_V3_COVERPOINT",
    "OWNER_V3_COVERPOINTS",
    "OWNER_V3_EVENT_TYPE",
    "OWNER_V3_SAMPLER_BIN_KEYS",
    "MMIO_V3_COVERPOINTS",
    "MMIO_V3_SAMPLER_BIN_KEYS",
    "TWO_FETCH_COVERPOINTS",
    "TWO_FETCH_SAMPLER_BIN_KEYS",
    "_TWO_FETCH_SIGNALS",
    "TWO_FETCH_OPTIONAL_SIGNAL_KEYS",
    "_WAY_DATA_CONFLICT_SIGNALS",
    "_classify_cfi_kind",
    "handle_owner_v3_event",
    "initialize_mmio_v3_coverage_state",
    "reset_mmio_v3_coverage_state",
    "sample_mmio_v3_coverage",
    "sample_cfvec_coverage",
    "initialize_ifu_cacheable_pipeline_state",
    "reset_ifu_cacheable_pipeline_state",
    "sample_ifu_cacheable_pipeline_coverage",
    "sample_two_fetch_coverage",
]
