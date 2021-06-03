/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

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

// Synthesizable minimal XiangShan
// * It is still an out-of-order, super-scalaer arch
// * L1 cache included
// * L2 cache NOT included
// * L3 cache included
class MinimalConfig(n: Int = 1) extends Config(
  new DefaultConfig(n).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(
      cores = up(SoCParamsKey).cores.map(_.copy(
        DecodeWidth = 2,
        RenameWidth = 2,
        FetchWidth = 4,
        IssQueSize = 8,
        NRPhyRegs = 80,
        LoadQueueSize = 16,
        StoreQueueSize = 16,
        RoqSize = 32,
        BrqSize = 8,
        FtqSize = 16,
        IBufSize = 16,
        StoreBufferSize = 4,
        StoreBufferThreshold = 3,
        dpParams = DispatchParameters(
          IntDqSize = 8,
          FpDqSize = 8,
          LsDqSize = 8,
          IntDqDeqWidth = 4,
          FpDqDeqWidth = 4,
          LsDqDeqWidth = 4
        ),
        icacheParameters = ICacheParameters(
          nSets = 8, // 4KB ICache
          tagECC = Some("parity"),
          dataECC = Some("parity"),
          replacer = Some("setplru"),
          nMissEntries = 2
        ),
        dcacheParameters = DCacheParameters(
          nSets = 8, // 4KB DCache
          nWays = 4,
          tagECC = Some("secded"),
          dataECC = Some("secded"),
          replacer = Some("setplru"),
          nMissEntries = 4,
          nProbeEntries = 4,
          nReleaseEntries = 4,
          nStoreReplayEntries = 4,
        ),
        L2Size = 16 * 1024, // 16KB
        L2NWays = 8,
        EnableBPD = false, // disable TAGE
        EnableLoop = false,
        TlbEntrySize = 4,
        TlbSPEntrySize = 2,
        PtwL1EntrySize = 2,
        PtwL2EntrySize = 64,
        PtwL3EntrySize = 128,
        PtwSPEntrySize = 2,
        useFakeL2Cache = true,
      )),
      L3Size = 32 * 1024, // 32KB
    )
  })
)

// Non-synthesizable MinimalConfig, for fast simulation only
class MinimalSimConfig(n: Int = 1) extends Config(
  new MinimalConfig(n).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(
      cores = up(SoCParamsKey).cores.map(_.copy(
        useFakeDCache = true,
        useFakePTW = true,
        useFakeL1plusCache = true,
      )),
      useFakeL3Cache = true
    )
  })
)
