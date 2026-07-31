"""ICache functional-coverage samplers."""

from .icache_mainpipe_funcov import (
    ICACHE_MAINPIPE_COVERPOINTS,
    ICACHE_MAINPIPE_SAMPLER_BIN_KEYS,
    reset_icache_mainpipe_coverage_state,
    sample_icache_mainpipe_coverage,
)
from .icache_prefetchpipe_funcov import (
    ICACHE_PREFETCHPIPE_COVERPOINTS,
    ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS,
    reset_icache_prefetchpipe_coverage_state,
    sample_icache_prefetchpipe_coverage,
)
from .icache_missunit_funcov import (
    ICACHE_MISSUNIT_COVERPOINTS,
    ICACHE_MISSUNIT_SAMPLER_BIN_KEYS,
    reset_icache_missunit_coverage_state,
    sample_icache_missunit_coverage,
)
from .icache_waylookup_funcov import (
    ICACHE_WAYLOOKUP_COVERPOINTS,
    ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS,
    reset_icache_waylookup_coverage_state,
    sample_icache_waylookup_coverage,
)
from .icache_hitmiss_funcov import (
    ICACHE_HITMISS_COVERPOINTS,
    ICACHE_HITMISS_SAMPLER_BIN_KEYS,
    reset_icache_hitmiss_coverage_state,
    sample_icache_hitmiss_coverage,
)

__all__ = (
    "ICACHE_MAINPIPE_COVERPOINTS",
    "ICACHE_MAINPIPE_SAMPLER_BIN_KEYS",
    "reset_icache_mainpipe_coverage_state",
    "sample_icache_mainpipe_coverage",
    "ICACHE_PREFETCHPIPE_COVERPOINTS",
    "ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS",
    "reset_icache_prefetchpipe_coverage_state",
    "sample_icache_prefetchpipe_coverage",
    "ICACHE_MISSUNIT_COVERPOINTS",
    "ICACHE_MISSUNIT_SAMPLER_BIN_KEYS",
    "reset_icache_missunit_coverage_state",
    "sample_icache_missunit_coverage",
    "ICACHE_WAYLOOKUP_COVERPOINTS",
    "ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS",
    "reset_icache_waylookup_coverage_state",
    "sample_icache_waylookup_coverage",
    "ICACHE_HITMISS_COVERPOINTS",
    "ICACHE_HITMISS_SAMPLER_BIN_KEYS",
    "reset_icache_hitmiss_coverage_state",
    "sample_icache_hitmiss_coverage",
)
