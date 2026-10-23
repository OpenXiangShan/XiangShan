from ...native_toffee import NativeDomainToffeeCoverage
from .icache_data_ecc_ibuffer_funcov import (
    GROUP, ICACHE_DATA_ECC_IBUFFER_SAMPLER_BIN_KEYS,
    evaluate_icache_data_ecc_ibuffer_coverage, reset_icache_data_ecc_ibuffer_state,
)


class ICacheDataEccIBufferToffeeCoverage(NativeDomainToffeeCoverage):
    """The canonical registry resolves each bin to one of four coverpoints."""

    def __init__(self, runtime, *, sink):
        super().__init__(runtime, keys=ICACHE_DATA_ECC_IBUFFER_SAMPLER_BIN_KEYS,
                         coverpoints={GROUP: "single_line"},
                         evaluate=evaluate_icache_data_ecc_ibuffer_coverage,
                         reset=reset_icache_data_ecc_ibuffer_state, sink=sink)
