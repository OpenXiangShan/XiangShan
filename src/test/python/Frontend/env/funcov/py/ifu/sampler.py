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
from .owner_v3_funcov import (
    OWNER_V3_BIN_SPECS,
    OWNER_V3_COVERPOINT,
    OWNER_V3_COVERPOINTS,
    OWNER_V3_EVENT_TYPE,
    OWNER_V3_SAMPLER_BIN_KEYS,
    handle_owner_v3_event,
)
from .mmio_v3_funcov import (
    MMIO_V3_CHECKED_EVENT_TYPE,
    MMIO_V3_COVERPOINTS,
    MMIO_V3_SAMPLER_BIN_KEYS,
    handle_mmio_v3_checked_event,
    initialize_mmio_v3_coverage_state,
    reset_mmio_v3_coverage_state,
    sample_mmio_v3_coverage,
)
from .cacheable_pipeline_funcov import (
    IFU_CACHEABLE_PIPELINE_COVERPOINTS,
    IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS,
    initialize_ifu_cacheable_pipeline_state,
    reset_ifu_cacheable_pipeline_state,
    sample_ifu_cacheable_pipeline_coverage,
)

IFU_COVERPOINTS = {
    "ifu_cfvec": "instruction_stream",
    "ifu_cacheable_delivery": "stream_shape",
    "ifu_cacheable_cfi_flow": "next_pc",
    **COMPACT_COVERPOINTS,
    **OWNER_V3_COVERPOINTS,
    **MMIO_V3_COVERPOINTS,
    **IFU_CACHEABLE_PIPELINE_COVERPOINTS,
}

__all__ = [
    "CFVEC_SAMPLER_BIN_KEYS",
    "COMPACT_COVERPOINTS",
    "COMPACT_SAMPLER_BIN_KEYS",
    "OWNER_V3_BIN_SPECS",
    "OWNER_V3_COVERPOINT",
    "OWNER_V3_COVERPOINTS",
    "OWNER_V3_EVENT_TYPE",
    "OWNER_V3_SAMPLER_BIN_KEYS",
    "MMIO_V3_CHECKED_EVENT_TYPE",
    "MMIO_V3_COVERPOINTS",
    "MMIO_V3_SAMPLER_BIN_KEYS",
    "IFU_CACHEABLE_PIPELINE_COVERPOINTS",
    "IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS",
    "IFU_COVERPOINTS",
    "_classify_cfi_kind",
    "handle_ifu_event",
    "initialize_ifu_coverage_state",
    "reset_ifu_coverage_state",
    "sample_cfvec_coverage",
    "sample_compact_coverage",
    "initialize_ifu_cacheable_pipeline_state",
    "handle_owner_v3_event",
    "handle_mmio_v3_checked_event",
    "initialize_mmio_v3_coverage_state",
    "reset_mmio_v3_coverage_state",
    "sample_mmio_v3_coverage",
    "reset_ifu_cacheable_pipeline_state",
    "sample_ifu_cacheable_pipeline_coverage",
]
