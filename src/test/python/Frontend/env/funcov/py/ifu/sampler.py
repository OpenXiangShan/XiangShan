"""Compatibility and aggregation entry point for IFU functional coverage."""

from .cfvec_funcov import (
    CFVEC_SAMPLER_BIN_KEYS,
    _classify_cfi_kind,
    handle_ifu_event,
    initialize_ifu_coverage_state,
    reset_ifu_coverage_state,
    sample_cfvec_coverage,
)
from .compact_funcov import COMPACT_COVERPOINTS, COMPACT_SAMPLER_BIN_KEYS, sample_compact_coverage

IFU_COVERPOINTS = {
    "ifu_cfvec": "instruction_stream",
    **COMPACT_COVERPOINTS,
}

__all__ = [
    "CFVEC_SAMPLER_BIN_KEYS",
    "COMPACT_COVERPOINTS",
    "COMPACT_SAMPLER_BIN_KEYS",
    "IFU_COVERPOINTS",
    "_classify_cfi_kind",
    "handle_ifu_event",
    "initialize_ifu_coverage_state",
    "reset_ifu_coverage_state",
    "sample_cfvec_coverage",
    "sample_compact_coverage",
]
