"""Compatibility and aggregation entry point for FTQ functional coverage."""

from .two_fetch_funcov import (
    TWO_FETCH_COVERPOINTS,
    TWO_FETCH_SAMPLER_BIN_KEYS,
    _TWO_FETCH_SIGNALS,
    initialize_ftq_coverage_state,
    reset_ftq_coverage_state,
    sample_two_fetch_coverage,
)
from .checker_funcov import COVERAGE_GROUPS as CHECKER_COVERAGE_GROUPS, sample_checker_coverage
from .ftq_request_funcov import COVERAGE_GROUPS as REQUEST_COVERAGE_GROUPS, sample_ftq_request_coverage
from .ifu_delivery_funcov import COVERAGE_GROUPS as IFU_DELIVERY_COVERAGE_GROUPS, sample_ifu_delivery_coverage
from .mainpipe_funcov import COVERAGE_GROUPS as MAINPIPE_COVERAGE_GROUPS, sample_mainpipe_coverage
from .waylookup_funcov import COVERAGE_GROUPS as WAYLOOKUP_COVERAGE_GROUPS, sample_waylookup_coverage

FTQ_COVERAGE_GROUPS = {
    **REQUEST_COVERAGE_GROUPS,
    **WAYLOOKUP_COVERAGE_GROUPS,
    **MAINPIPE_COVERAGE_GROUPS,
    **IFU_DELIVERY_COVERAGE_GROUPS,
    **CHECKER_COVERAGE_GROUPS,
}

__all__ = [
    "FTQ_COVERAGE_GROUPS",
    "TWO_FETCH_COVERPOINTS",
    "TWO_FETCH_SAMPLER_BIN_KEYS",
    "_TWO_FETCH_SIGNALS",
    "sample_checker_coverage",
    "sample_ftq_request_coverage",
    "sample_ifu_delivery_coverage",
    "sample_mainpipe_coverage",
    "initialize_ftq_coverage_state",
    "reset_ftq_coverage_state",
    "sample_two_fetch_coverage",
    "sample_waylookup_coverage",
]
