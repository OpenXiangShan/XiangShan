package top

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import system._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, XLen}
import sifive.blocks.inclusivecache.{InclusiveCache, InclusiveCacheMicroParameters, CacheParameters}
import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.cache.{DCacheParameters, ICacheParameters, L1plusCacheParameters}
import xiangshan.cache.prefetch.{BOPParameters, L1plusPrefetcherParameters, L2PrefetcherParameters, StreamPrefetchParameters}

class DefaultConfig(n: Int) extends Config((site, here, up) => {
  case XLen => 64
  case DebugOptionsKey => DebugOptions()
  case SoCParamsKey => SoCParameters(
    cores = List.tabulate(n){ i => XSCoreParameters(HartId = i) }
  )
})

// TODO: disable L2 and L3
class MinimalConfig(n: Int = 1) extends Config(
  new DefaultConfig(n).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(
      cores = up(SoCParamsKey).cores.map(_.copy(
        IssQueSize = 8,
        NRPhyRegs = 80,
        LoadQueueSize = 16,
        StoreQueueSize = 16,
        RoqSize = 32,
        BrqSize = 8,
        FtqSize = 16,
        IBufSize = 16,
        dpParams = DispatchParameters(
          IntDqSize = 8,
          FpDqSize = 8,
          LsDqSize = 8,
          IntDqDeqWidth = 4,
          FpDqDeqWidth = 4,
          LsDqDeqWidth = 4
        ),
        EnableBPD = false, // disable TAGE
        EnableLoop = false,
        TlbEntrySize = 4,
        TlbSPEntrySize = 2,
        PtwL1EntrySize = 2,
        PtwL2EntrySize = 2,
        PtwL3EntrySize = 4,
        PtwSPEntrySize = 2,
        useFakeDCache = true,
        useFakePTW = true,
        useFakeL1plusCache = true,
      )),
      useFakeL3Cache = true
    )
  })
)