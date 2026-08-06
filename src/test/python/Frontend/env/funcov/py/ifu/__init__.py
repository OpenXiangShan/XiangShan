"""IFU functional coverage groups and compatibility sampler."""

from .cfvec_funcov import CFVEC_SAMPLER_BIN_KEYS, sample_cfvec_coverage
from .compact_funcov import COMPACT_COVERPOINTS, COMPACT_SAMPLER_BIN_KEYS, sample_compact_coverage
from .sampler import IFU_COVERPOINTS

__all__ = [
    "CFVEC_SAMPLER_BIN_KEYS",
    "COMPACT_COVERPOINTS",
    "COMPACT_SAMPLER_BIN_KEYS",
    "IFU_COVERPOINTS",
    "sample_cfvec_coverage",
    "sample_compact_coverage",
]
