"""IFU functional coverage groups and compatibility sampler."""

from .cfvec_funcov import CFVEC_SAMPLER_BIN_KEYS, sample_cfvec_coverage
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
from .mmio_nc_owner_funcov import (
    MMIO_NC_OWNER_COVERPOINT,
    MMIO_NC_OWNER_COVERPOINTS,
    MMIO_NC_OWNER_SAMPLER_BIN_KEYS,
    initialize_mmio_nc_owner_coverage_state,
    reset_mmio_nc_owner_coverage_state,
    sample_mmio_nc_owner_coverage,
)
from .instr_uncache_owner_funcov import (
    INSTR_UNCACHE_OWNER_COVERPOINT,
    INSTR_UNCACHE_OWNER_COVERPOINTS,
    INSTR_UNCACHE_OWNER_GROUP,
    INSTR_UNCACHE_OWNER_LEAF_COUNT,
    INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS,
)
from .cacheable_pipeline_funcov import (
    IFU_CACHEABLE_PIPELINE_COVERPOINTS,
    IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS,
    initialize_ifu_cacheable_pipeline_state,
    reset_ifu_cacheable_pipeline_state,
    sample_ifu_cacheable_pipeline_coverage,
)
from .sampler import IFU_COVERPOINTS

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
    "MMIO_NC_OWNER_COVERPOINT",
    "MMIO_NC_OWNER_COVERPOINTS",
    "MMIO_NC_OWNER_SAMPLER_BIN_KEYS",
    "INSTR_UNCACHE_OWNER_COVERPOINT",
    "INSTR_UNCACHE_OWNER_COVERPOINTS",
    "INSTR_UNCACHE_OWNER_GROUP",
    "INSTR_UNCACHE_OWNER_LEAF_COUNT",
    "INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS",
    "IFU_CACHEABLE_PIPELINE_COVERPOINTS",
    "IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS",
    "IFU_COVERPOINTS",
    "sample_cfvec_coverage",
    "sample_compact_coverage",
    "handle_owner_v3_event",
    "handle_mmio_v3_checked_event",
    "initialize_mmio_v3_coverage_state",
    "reset_mmio_v3_coverage_state",
    "sample_mmio_v3_coverage",
    "initialize_mmio_nc_owner_coverage_state",
    "reset_mmio_nc_owner_coverage_state",
    "sample_mmio_nc_owner_coverage",
    "initialize_ifu_cacheable_pipeline_state",
    "reset_ifu_cacheable_pipeline_state",
    "sample_ifu_cacheable_pipeline_coverage",
]
