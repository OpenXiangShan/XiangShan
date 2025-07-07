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
import system._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, XLen}
import xiangshan.frontend.icache.ICacheParameters
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.tile.MaxHartIdBits
import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.backend.exu.ExuParameters
import xiangshan.cache.DCacheParameters
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import device.{EnableJtag, XSDebugModuleParams}
import huancun._

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
        FetchWidth = 4,
        CommitWidth = 4,
        IssQueSize = 8,
        NRPhyRegs = 64,
        LoadQueueSize = 16,
        LoadQueueNWriteBanks = 4,
        StoreQueueSize = 12,
        StoreQueueNWriteBanks = 4,
        RobSize = 32,
        FtqSize = 8,
        IBufSize = 16,
        StoreBufferSize = 4,
        StoreBufferThreshold = 3,
        dpParams = DispatchParameters(
          IntDqSize = 12,
          FpDqSize = 12,
          LsDqSize = 12,
          IntDqDeqWidth = 4,
          FpDqDeqWidth = 4,
          LsDqDeqWidth = 4
        ),
        exuParameters = ExuParameters(
          JmpCnt = 1,
          AluCnt = 2,
          MulCnt = 0,
          MduCnt = 1,
          FmacCnt = 1,
          FmiscCnt = 1,
          FmiscDivSqrtCnt = 0,
          LduCnt = 2,
          StuCnt = 2
        ),
        prefetcher = None,
        icacheParameters = ICacheParameters(
          nSets = 64, // 16KB ICache
          tagECC = Some("parity"),
          dataECC = Some("parity"),
          replacer = Some("setplru"),
          nMissEntries = 2,
          nReleaseEntries = 1,
          nProbeEntries = 2,
          nPrefetchEntries = 2,
          hasPrefetch = false
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
        )),
        EnableBPD = false, // disable TAGE
        EnableLoop = false,
        itlbParameters = TLBParameters(
          name = "itlb",
          fetchi = true,
          useDmode = false,
          sameCycle = false,
          missSameCycle = true,
          normalReplacer = Some("plru"),
          superReplacer = Some("plru"),
          normalNWays = 4,
          normalNSets = 1,
          superNWays = 2,
          shouldBlock = true
        ),
        ldtlbParameters = TLBParameters(
          name = "ldtlb",
          normalNSets = 16, // 6when da or sa
          normalNWays = 1, // when fa or sa
          normalAssociative = "sa",
          normalReplacer = Some("setplru"),
          superNWays = 4,
          normalAsVictim = true,
          partialStaticPMP = true,
          outReplace = false
        ),
        sttlbParameters = TLBParameters(
          name = "sttlb",
          normalNSets = 16, // when da or sa
          normalNWays = 1, // when fa or sa
          normalAssociative = "sa",
          normalReplacer = Some("setplru"),
          normalAsVictim = true,
          superNWays = 4,
          partialStaticPMP = true,
          outReplace = false
        ),
        btlbParameters = TLBParameters(
          name = "btlb",
          normalNSets = 1,
          normalNWays = 8,
          superNWays = 2
        ),
        l2tlbParameters = L2TLBParameters(
          l1Size = 4,
          l2nSets = 4,
          l2nWays = 4,
          l3nSets = 4,
          l3nWays = 8,
          spSize = 2,
        ),
        L2CacheParamsOpt = None // remove L2 Cache
      )
    )
    case SoCParamsKey =>
      val tiles = site(XSTileKey)
      up(SoCParamsKey).copy(
        L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
          sets = 1024,
          inclusive = false,
          clientCaches = tiles.map{ p =>
            CacheParameters(
              "dcache",
              sets = 2 * p.dcacheParametersOpt.get.nSets,
              ways = p.dcacheParametersOpt.get.nWays + 2,
              blockGranularity = log2Ceil(2 * p.dcacheParametersOpt.get.nSets),
              aliasBitsOpt = None
            )
          },
          sramClkDivBy2 = false,
          tagECC = None,
          dataECC = None,
          simulation = !site(DebugOptionsKey).FPGAPlatform,
          FPGAPlatform = site(DebugOptionsKey).FPGAPlatform,
          elaboratedTopDown = !site(DebugOptionsKey).FPGAPlatform
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

class WithNKBL1D(n: Int, ways: Int = 4) extends Config((site, here, up) => {
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
        nReleaseEntries = 18
      ))
    ))
})

class WithNKBL2
(
  n: Int,
  ways: Int = 8,
  inclusive: Boolean = true,
  banks: Int = 1,
  alwaysReleaseData: Boolean = false
) extends Config((site, here, up) => {
  case XSTileKey =>
    val upParams = up(XSTileKey)
    val l2sets = n * 1024 / banks / ways / 64
    upParams.map(p => p.copy(
      L2CacheParamsOpt = Some(HCCacheParameters(
        name = "L2",
        level = 2,
        ways = ways,
        sets = l2sets,
        inclusive = inclusive,
        alwaysReleaseData = alwaysReleaseData,
        clientCaches = Seq(CacheParameters(
          "dcache",
          sets = 2 * p.dcacheParametersOpt.get.nSets / banks,
          ways = p.dcacheParametersOpt.get.nWays + 2,
          blockGranularity = log2Ceil(2 * p.dcacheParametersOpt.get.nSets / banks),
          aliasBitsOpt = p.dcacheParametersOpt.get.aliasBitsOpt
        )),
        reqField = Seq(PreferCacheField()),
        echoField = Seq(DirtyField()),
        prefetch = Some(huancun.prefetch.PrefetchReceiverParams()),
        enablePerf = true,
        sramDepthDiv = 2,
        tagECC = None,
        dataECC = None,
        simulation = !site(DebugOptionsKey).FPGAPlatform,
        FPGAPlatform = site(DebugOptionsKey).FPGAPlatform,
        elaboratedTopDown = !site(DebugOptionsKey).FPGAPlatform
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
          l2params.copy(
            sets = 2 * clientDirBytes / core.L2NBanks / l2params.ways / 64,
            blockGranularity = log2Ceil(clientDirBytes / core.L2NBanks / l2params.ways / 64 / tiles.size)
          )
        },
        enablePerf = true,
        ctrl = Some(CacheCtrl(
          address = 0x39000000,
          numCores = tiles.size
        )),
        sramClkDivBy2 = true,
        sramDepthDiv = 4,
        tagECC = None,
        dataECC = None,
        simulation = !site(DebugOptionsKey).FPGAPlatform,
        FPGAPlatform = site(DebugOptionsKey).FPGAPlatform,
        elaboratedTopDown = !site(DebugOptionsKey).FPGAPlatform
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

class MediumConfig(n: Int = 1) extends Config(
  new WithNKBL3(4096, inclusive = false, banks = 4)
    ++ new WithNKBL2(512, inclusive = false, alwaysReleaseData = true)
    ++ new WithNKBL1D(128)
    ++ new BaseConfig(n)
)

class DefaultConfig(n: Int = 1) extends Config(
  new WithNKBL3(6 * 1024, inclusive = false, banks = 4, ways = 6)
    ++ new WithNKBL2(2 * 512, inclusive = false, banks = 4, alwaysReleaseData = true)
    ++ new WithNKBL1D(64)
    ++ new BaseConfig(n)
)

/*** Nanhu General Config ( Nanhu-G Config) ***/

// XSCore Config: 
// * Including Frontend/Backend/MMU/MemBlock
// * Including DebugOptions
// * Not Including L1D/L2/L3 Cache
class NanHuGCoreConfig(n: Int = 1) extends Config(
  new BaseConfig(n).alter((site, here, up) => {
    case XSTileKey => up(XSTileKey).map(
      _.copy(
        DecodeWidth = 4,
        RenameWidth = 4,
        CommitWidth = 4,
        FetchWidth = 8,
        IssQueSize = 8,
        NRPhyRegs = 128,
        LoadQueueSize = 64,
        LoadQueueNWriteBanks = 4,
        StoreQueueSize = 64,
        StoreQueueNWriteBanks = 4,
        RobSize = 128,
        FtqSize = 64,
        IBufSize = 32,
        StoreBufferSize = 4,
        StoreBufferThreshold = 3,
        dpParams = DispatchParameters(
          IntDqSize = 16,
          FpDqSize = 16,
          LsDqSize = 16,
          IntDqDeqWidth = 4,
          FpDqDeqWidth = 4,
          LsDqDeqWidth = 4
        ),
        exuParameters = ExuParameters(
          JmpCnt = 1,
          AluCnt = 2,
          MulCnt = 0,
          MduCnt = 1,
          FmacCnt = 1,
          FmiscCnt = 1,
          FmiscDivSqrtCnt = 0,
          LduCnt = 2,
          StuCnt = 2
        ),
        //prefetcher = None,
        EnableSC = false,
        EnableLoop = false,
        FtbSize = 1024,
        UbtbSize = 128,
        // 4-way 16KB DCache        
        icacheParameters = ICacheParameters(
          nSets = 64, 
          nWays = 4,
          tagECC = None,
          dataECC = None,
          replacer = Some("setplru"),
          nMissEntries = 2,
          nReleaseEntries = 1,
          nProbeEntries = 2,
          nPrefetchEntries = 2,
          hasPrefetch = false
        ),
        itlbParameters = TLBParameters(
          name = "itlb",
          fetchi = true,
          useDmode = false,
          sameCycle = false,
          missSameCycle = true,
          normalReplacer = Some("plru"),
          superReplacer = Some("plru"),
          normalNWays = 4,
          normalNSets = 1,
          superNWays = 2,
          shouldBlock = true
        ),
        ldtlbParameters = TLBParameters(
          name = "ldtlb",
          normalNSets = 16, // 6when da or sa
          normalNWays = 1, // when fa or sa
          normalAssociative = "sa",
          normalReplacer = Some("setplru"),
          superNWays = 4,
          normalAsVictim = true,
          partialStaticPMP = true,
          outReplace = false
        ),
        sttlbParameters = TLBParameters(
          name = "sttlb",
          normalNSets = 16, // when da or sa
          normalNWays = 1, // when fa or sa
          normalAssociative = "sa",
          normalReplacer = Some("setplru"),
          normalAsVictim = true,
          superNWays = 4,
          partialStaticPMP = true,
          outReplace = false
        ),
        btlbParameters = TLBParameters(
          name = "btlb",
          normalNSets = 1,
          normalNWays = 8,
          superNWays = 2
        ),
        l2tlbParameters = L2TLBParameters(
          l1Size = 4,
          l2nSets = 4,
          l2nWays = 4,
          l3nSets = 4,
          l3nWays = 8,
          spSize = 2,
        )
      )
    )
  })
)

// Cache Hierarchy Config: 
// * Including L1D/L2/L3 Cache
class NanHuGCacheConfig extends Config(
  new WithNKBL3(6 * 256, inclusive = false, banks = 4, ways = 6)
  ++ new WithNKBL2(256,inclusive = false, banks = 4, alwaysReleaseData = true) 
  ++ new WithNKBL1D(32) 
)

// XSSoC Config:
class NanHuGConfig(n: Int = 1) extends Config(
  new NanHuGCacheConfig ++ new NanHuGCoreConfig(n)
)

// FPGA Config:
// * Dissable BasicDiff
class NanHuGFPGAConfig(n: Int = 1) extends Config(
  new NanHuGConfig(n).alter((site, here, up) => {
    case DebugOptionsKey => up(DebugOptionsKey).copy(
      AlwaysBasicDiff = false
    )
    case SoCParamsKey => up(SoCParamsKey).copy(
      L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
        sramClkDivBy2 = false,
      ))
    )
  })
)

class MinimalFPGAConfig(n: Int = 1) extends Config(
  new MinimalConfig(n).alter((site, here, up) => {
    case DebugOptionsKey => up(DebugOptionsKey).copy(
      AlwaysBasicDiff = false
    )
  })
)

class NanHuGServeConfig(n: Int = 1) extends Config(
  new NanHuGFPGAConfig(n).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(
      L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
        sramClkDivBy2 = false,
      ))
    )
  })
)
