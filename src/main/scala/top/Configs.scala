/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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
import utility._
import system._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, XLen}
import xiangshan.frontend.icache.ICacheParameters
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.tile.{MaxHartIdBits, XLen}
import system._
import utility._
import utils._
import huancun._
import xiangshan._
import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.backend.regfile.{IntPregParams, VfPregParams}
import xiangshan.cache.DCacheParameters
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import device.{EnableJtag, XSDebugModuleParams}
import huancun._
import coupledL2._
import xiangshan.frontend.icache.ICacheParameters

class BaseConfig(n: Int) extends Config((site, here, up) => {
  case XLen => 64
  case DebugOptionsKey => DebugOptions()
  case SoCParamsKey => SoCParameters()
  case PMParameKey => PMParameters()
  case XSTileKey => Seq.tabulate(n){ i => XSCoreParameters(HartId = i) }
  case ExportDebug => DebugAttachParams(protocols = Set(JTAG))
  case DebugModuleKey => Some(XSDebugModuleParams(site(XLen)))
  case JtagDTMKey => JtagDTMKey
  case MaxHartIdBits => 2
  case EnableJtag => true.B
})

// Synthesizable minimal XiangShan
// * It is still an out-of-order, super-scalaer arch
// * L1 cache included
// * L2 cache NOT included
// * L3 cache included
class MinimalConfig(n: Int = 1) extends Config(
  new BaseConfig(n).alter((site, here, up) => {
    case XSTileKey => up(XSTileKey).map(
      _.copy(
        DecodeWidth = 2,
        RenameWidth = 2,
        CommitWidth = 2,
        FetchWidth = 4,
        VirtualLoadQueueSize = 16,
        LoadQueueRARSize = 16,
        LoadQueueRAWSize = 12,
        LoadQueueReplaySize = 12,
        LoadUncacheBufferSize = 8,
        LoadQueueNWriteBanks = 4, // NOTE: make sure that LoadQueue{RAR, RAW, Replay}Size is divided by LoadQueueNWriteBanks.
        RollbackGroupSize = 8,
        StoreQueueSize = 12,
        StoreQueueNWriteBanks = 4, // NOTE: make sure that StoreQueueSize is divided by StoreQueueNWriteBanks
        StoreQueueForwardWithMask = true,
        RobSize = 32,
        RabSize = 32,
        FtqSize = 8,
        IBufSize = 16,
        StoreBufferSize = 4,
        StoreBufferThreshold = 3,
        IssueQueueSize = 8,
        dpParams = DispatchParameters(
          IntDqSize = 12,
          FpDqSize = 12,
          LsDqSize = 12,
          IntDqDeqWidth = 4,
          FpDqDeqWidth = 4,
          LsDqDeqWidth = 6
        ),
        intPreg = IntPregParams(
          numEntries = 64,
          numRead = None,
          numWrite = None,
        ),
        vfPreg = VfPregParams(
          numEntries = 192,
          numRead = None,
          numWrite = None,
        ),
        icacheParameters = ICacheParameters(
          nSets = 64, // 16KB ICache
          tagECC = Some("parity"),
          dataECC = Some("parity"),
          replacer = Some("setplru"),
          nMissEntries = 2,
          nReleaseEntries = 1,
          nProbeEntries = 2,
          // fdip
          enableICachePrefetch = true,
          prefetchToL1 = false,
        ),
        dcacheParametersOpt = Some(DCacheParameters(
          nSets = 64, // 32KB DCache
          nWays = 8,
          tagECC = Some("secded"),
          dataECC = Some("secded"),
          replacer = Some("setplru"),
          nMissEntries = 4,
          nProbeEntries = 4,
          nReleaseEntries = 8,
          nMaxPrefetchEntry = 2,
        )),
        EnableBPD = false, // disable TAGE
        EnableLoop = false,
        itlbParameters = TLBParameters(
          name = "itlb",
          fetchi = true,
          useDmode = false,
          NWays = 4,
        ),
        ldtlbParameters = TLBParameters(
          name = "ldtlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false
        ),
        sttlbParameters = TLBParameters(
          name = "sttlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false
        ),
        hytlbParameters = TLBParameters(
          name = "hytlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false
        ),
        pftlbParameters = TLBParameters(
          name = "pftlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false
        ),
        btlbParameters = TLBParameters(
          name = "btlb",
          NWays = 4,
        ),
        l2tlbParameters = L2TLBParameters(
          l1Size = 4,
          l2nSets = 4,
          l2nWays = 4,
          l3nSets = 4,
          l3nWays = 8,
          spSize = 2,
        ),
        L2CacheParamsOpt = Some(L2Param(
          name = "L2",
          ways = 8,
          sets = 128,
          echoField = Seq(huancun.DirtyField()),
          prefetch = None
        )),
        L2NBanks = 2,
        prefetcher = None // if L2 pf_recv_node does not exist, disable SMS prefetcher
      )
    )
    case SoCParamsKey =>
      val tiles = site(XSTileKey)
      up(SoCParamsKey).copy(
        L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
          sets = 1024,
          inclusive = false,
          clientCaches = tiles.map{ core =>
            val clientDirBytes = tiles.map{ t =>
              t.L2NBanks * t.L2CacheParamsOpt.map(_.toCacheParams.capacity).getOrElse(0)
            }.sum
            val l2params = core.L2CacheParamsOpt.get.toCacheParams
            l2params.copy(sets = 2 * clientDirBytes / core.L2NBanks / l2params.ways / 64)
          },
          simulation = !site(DebugOptionsKey).FPGAPlatform,
          prefetch = None
        )),
        L3NBanks = 1
      )
  })
)

// Non-synthesizable MinimalConfig, for fast simulation only
class MinimalSimConfig(n: Int = 1) extends Config(
  new MinimalConfig(n).alter((site, here, up) => {
    case XSTileKey => up(XSTileKey).map(_.copy(
      dcacheParametersOpt = None,
      softPTW = true
    ))
    case SoCParamsKey => up(SoCParamsKey).copy(
      L3CacheParamsOpt = None
    )
  })
)

class WithNKBL1D(n: Int, ways: Int = 8) extends Config((site, here, up) => {
  case XSTileKey =>
    val sets = n * 1024 / ways / 64
    up(XSTileKey).map(_.copy(
      dcacheParametersOpt = Some(DCacheParameters(
        nSets = sets,
        nWays = ways,
        tagECC = Some("secded"),
        dataECC = Some("secded"),
        replacer = Some("setplru"),
        nMissEntries = 16,
        nProbeEntries = 8,
        nReleaseEntries = 18,
        nMaxPrefetchEntry = 6,
      ))
    ))
})

class WithNKBL2
(
  n: Int,
  ways: Int = 8,
  inclusive: Boolean = true,
  banks: Int = 1
) extends Config((site, here, up) => {
  case XSTileKey =>
    val upParams = up(XSTileKey)
    val l2sets = n * 1024 / banks / ways / 64
    upParams.map(p => p.copy(
      L2CacheParamsOpt = Some(L2Param(
        name = "L2",
        ways = ways,
        sets = l2sets,
        clientCaches = Seq(L1Param(
          "dcache",
          sets = 2 * p.dcacheParametersOpt.get.nSets / banks,
          ways = p.dcacheParametersOpt.get.nWays + 2,
          aliasBitsOpt = p.dcacheParametersOpt.get.aliasBitsOpt,
          vaddrBitsOpt = Some(p.VAddrBits - log2Up(p.dcacheParametersOpt.get.blockBytes))
        )),
        reqField = Seq(utility.ReqSourceField()),
        echoField = Seq(huancun.DirtyField()),
        prefetch = Some(coupledL2.prefetch.PrefetchReceiverParams())
      )),
      L2NBanks = banks
    ))
})

class WithNKBL3(n: Int, ways: Int = 8, inclusive: Boolean = true, banks: Int = 1) extends Config((site, here, up) => {
  case SoCParamsKey =>
    val sets = n * 1024 / banks / ways / 64
    val tiles = site(XSTileKey)
    val clientDirBytes = tiles.map{ t =>
      t.L2NBanks * t.L2CacheParamsOpt.map(_.toCacheParams.capacity).getOrElse(0)
    }.sum
    up(SoCParamsKey).copy(
      L3NBanks = banks,
      L3CacheParamsOpt = Some(HCCacheParameters(
        name = "L3",
        level = 3,
        ways = ways,
        sets = sets,
        inclusive = inclusive,
        clientCaches = tiles.map{ core =>
          val l2params = core.L2CacheParamsOpt.get.toCacheParams
          l2params.copy(sets = 2 * clientDirBytes / core.L2NBanks / l2params.ways / 64, ways = l2params.ways + 2)
        },
        enablePerf = true,
        ctrl = Some(CacheCtrl(
          address = 0x39000000,
          numCores = tiles.size
        )),
        reqField = Seq(utility.ReqSourceField()),
        sramClkDivBy2 = true,
        sramDepthDiv = 4,
        tagECC = Some("secded"),
        dataECC = Some("secded"),
        simulation = !site(DebugOptionsKey).FPGAPlatform,
        prefetch = Some(huancun.prefetch.L3PrefetchReceiverParams())
      ))
    )
})

class WithL3DebugConfig extends Config(
  new WithNKBL3(256, inclusive = false) ++ new WithNKBL2(64)
)

class MinimalL3DebugConfig(n: Int = 1) extends Config(
  new WithL3DebugConfig ++ new MinimalConfig(n)
)

class DefaultL3DebugConfig(n: Int = 1) extends Config(
  new WithL3DebugConfig ++ new BaseConfig(n)
)

class WithFuzzer extends Config((site, here, up) => {
  case DebugOptionsKey => up(DebugOptionsKey).copy(
    EnablePerfDebug = false,
  )
  case SoCParamsKey => up(SoCParamsKey).copy(
    L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
      enablePerf = false,
    )),
  )
  case XSTileKey => up(XSTileKey).zipWithIndex.map{ case (p, i) =>
    p.copy(
      L2CacheParamsOpt = Some(up(XSTileKey)(i).L2CacheParamsOpt.get.copy(
        enablePerf = false,
      )),
    )
  }
})

class MinimalAliasDebugConfig(n: Int = 1) extends Config(
  new WithNKBL3(512, inclusive = false) ++
    new WithNKBL2(256, inclusive = false) ++
    new WithNKBL1D(128) ++
    new MinimalConfig(n)
)

class MediumConfig(n: Int = 1) extends Config(
  new WithNKBL3(4096, inclusive = false, banks = 4)
    ++ new WithNKBL2(512, inclusive = false)
    ++ new WithNKBL1D(128)
    ++ new BaseConfig(n)
)

class FuzzConfig(dummy: Int = 0) extends Config(
  new WithFuzzer
    ++ new DefaultConfig(1)
)

class DefaultConfig(n: Int = 1) extends Config(
  new WithNKBL3(6 * 1024, inclusive = false, banks = 4, ways = 6)
    ++ new WithNKBL2(2 * 512, inclusive = false, banks = 4)
    ++ new WithNKBL1D(128)
    ++ new BaseConfig(n)
)
