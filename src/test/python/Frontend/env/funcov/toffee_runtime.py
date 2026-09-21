from __future__ import annotations

from dataclasses import dataclass

from .py.ftq.two_fetch_toffee import TwoFetchToffeeCoverage
from .py.icache.icache_hitmiss_toffee import ICacheHitMissToffeeCoverage
from .py.icache.icache_mainpipe_toffee import ICacheMainpipeToffeeCoverage
from .py.icache.icache_missunit_toffee import ICacheMissunitToffeeCoverage
from .py.icache.icache_prefetchpipe_toffee import ICachePrefetchpipeToffeeCoverage
from .py.icache.icache_waylookup_toffee import ICacheWaylookupToffeeCoverage
from .py.ifu.cacheable_pipeline_toffee import IfuCacheablePipelineToffeeCoverage
from .py.ifu.cfvec_toffee import IfuCfvecToffeeCoverage
from .py.ifu.mmio_nc_owner_toffee import MmioNcOwnerToffeeCoverage
from .py.ifu.mmio_v3_toffee import MmioV3ToffeeCoverage
from .py.ifu.owner_v3_toffee import OwnerV3ToffeeCoverage
from .py.ifu.uncache_event_toffee import UncacheEventToffeeCoverage


@dataclass(frozen=True)
class ToffeeRuntime:
    cycle_models: tuple
    event_models: tuple
    owner_model: object
    uncache_model: object


def create_toffee_runtime(recorder, sink, *, audit_recorder=None) -> ToffeeRuntime:
    def create(model_type):
        return model_type(recorder, sink=sink, audit_recorder=audit_recorder)

    hitmiss = create(ICacheHitMissToffeeCoverage)
    mainpipe = create(ICacheMainpipeToffeeCoverage)
    prefetchpipe = create(ICachePrefetchpipeToffeeCoverage)
    missunit = create(ICacheMissunitToffeeCoverage)
    waylookup = create(ICacheWaylookupToffeeCoverage)
    two_fetch = create(TwoFetchToffeeCoverage)
    mmio_v3 = create(MmioV3ToffeeCoverage)
    mmio_nc_owner = create(MmioNcOwnerToffeeCoverage)
    owner_v3 = create(OwnerV3ToffeeCoverage)
    sink.attach_owner_model(owner_v3)
    uncache_event = create(UncacheEventToffeeCoverage)
    cfvec = create(IfuCfvecToffeeCoverage)
    cacheable = create(IfuCacheablePipelineToffeeCoverage)

    domains = (
        "icache_hitmiss",
        "icache_mainpipe",
        "icache_prefetchpipe",
        "icache_missunit",
        "icache_waylookup",
        "ftq_two_fetch",
        "ifu_mmio_v3",
        "ifu_mmio_nc_owner",
        "uncache_event",
        "ifu_cfvec",
        "ifu_cacheable_pipeline",
    )
    for domain in domains:
        recorder.enable_toffee_direct_domain(domain)

    return ToffeeRuntime(
        cycle_models=(
            hitmiss,
            mainpipe,
            prefetchpipe,
            missunit,
            waylookup,
            two_fetch,
            mmio_v3,
            mmio_nc_owner,
            owner_v3,
            uncache_event,
            cfvec,
            cacheable,
        ),
        event_models=(mmio_v3, owner_v3),
        owner_model=owner_v3,
        uncache_model=uncache_event,
    )
