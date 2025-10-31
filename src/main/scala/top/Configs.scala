/***************************************************************************************
* Copyright (c) 2021-2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
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
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, MaxHartIdBits, XLen}
import xiangshan.frontend.FrontendParameters
import xiangshan.frontend.bpu.BpuParameters
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.IttageTableInfo
import xiangshan.frontend.bpu.mbtb.MainBtbParameters
import xiangshan.frontend.bpu.tage.TageParameters
import xiangshan.frontend.bpu.sc.ScParameters
import xiangshan.frontend.bpu.ittage.IttageParameters
import xiangshan.frontend.bpu.ras.RasParameters
import xiangshan.frontend.ftq.FtqParameters
import xiangshan.frontend.icache.ICacheParameters
import xiangshan.frontend.ibuffer.IBufferParameters
import freechips.rocketchip.devices.debug._
import openLLC.OpenLLCParam
import freechips.rocketchip.diplomacy._
import xiangshan.backend.regfile.{IntPregParams, VfPregParams}
import xiangshan.cache.DCacheParameters
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import device.EnableJtag
import huancun._
import coupledL2._
import coupledL2.prefetch._

class BaseConfig(n: Int) extends Config((site, here, up) => {
  case XLen => 64
  case DebugOptionsKey => DebugOptions()
  case SoCParamsKey => SoCParameters()
  case CVMParamsKey => CVMParameters()
  case PMParameKey => PMParameters()
  case XSTileKey => Seq.tabulate(n){ i => XSCoreParameters(HartId = i) }
  case ExportDebug => DebugAttachParams(protocols = Set(JTAG))
  case DebugModuleKey => Some(DebugModuleParams(
    nAbstractDataWords = (if (site(XLen) == 32) 1 else if (site(XLen) == 64) 2 else 4),
    maxSupportedSBAccess = site(XLen),
    hasBusMaster = true,
    baseAddress = BigInt(0x38020000),
    nScratch = 2,
    crossingHasSafeReset = false,
    hasHartResets = true
  ))
  case JtagDTMKey => JtagDTMKey
  case MaxHartIdBits => log2Up(n) max 6
  case EnableJtag => true.B
  case DFTOptionsKey => DFTOptions()
})

// Synthesizable minimal XiangShan
// * It is still an out-of-order, super-scalaer arch
// * L1 cache included
// * L2 cache NOT included
// * L3 cache included
class MinimalConfig(n: Int = 1) extends Config(
  new BaseConfig(n).alter((site, here, up) => {
    case XSTileKey => up(XSTileKey).map(
      p => p.copy(
        DecodeWidth = 8,
        RenameWidth = 8,
        RobCommitWidth = 8,
        // FetchWidth = 4, // NOTE: make sure that FTQ SRAM width is not a prime number bigger than 256.
        VirtualLoadQueueSize = 24,
        LoadQueueRARSize = 24,
        LoadQueueRAWSize = 12,
        LoadQueueReplaySize = 24,
        LoadUncacheBufferSize = 8,
        LoadQueueNWriteBanks = 4, // NOTE: make sure that LoadQueue{RAR, RAW, Replay}Size is divided by LoadQueueNWriteBanks.
        RollbackGroupSize = 8,
        StoreQueueSize = 20,
        StoreQueueNWriteBanks = 4, // NOTE: make sure that StoreQueueSize is divided by StoreQueueNWriteBanks
        StoreQueueForwardWithMask = true,
        // ============ VLSU ============
        VlMergeBufferSize = 16,
        VsMergeBufferSize = 8,
        UopWritebackWidth = 2,
        // ==============================
        RobSize = 48,
        RabSize = 96,
        frontendParameters = FrontendParameters(
          FetchBlockSize = 32, // in bytes
          bpuParameters = BpuParameters(
            // FIXME: these are from V2 Ftb(Size=512, Way=2), may not correct
            mbtbParameters = MainBtbParameters(
              // NumEntries = 512,
              // NumWay = 2
            ),
            tageParameters = TageParameters(
              TableInfos = Seq(
                new TageTableInfo(512, 4),
                new TageTableInfo(512, 9),
                new TageTableInfo(512, 17),
                new TageTableInfo(1024, 31)
              ),
            ),
            // FIXME: these are from V2 SC, we don't have equivalent parameters now
            scParameters = ScParameters(
              // NumRows = 128,
              // NumTables = 2,
              // HistLens = Seq(0, 5),
            ),
            ittageParameters = IttageParameters(
              TableInfos = Seq(
                new IttageTableInfo(256, 4),
                new IttageTableInfo(256, 8),
                new IttageTableInfo(512, 16)
              ),
              TagWidth = 7
            ),
            rasParameters = RasParameters(
              CommitStackSize = 8,
              SpecQueueSize = 16
            ),
          ),
          ftqParameters = FtqParameters(
            FtqSize = 8,
          ),
          icacheParameters = ICacheParameters( // default 64B blockBytes, 4way, 256set (64KB ICache)
            nSets = 64, // override to 64set in MinimalConfig (16KB ICache)
          ),
          ibufferParameters = IBufferParameters(
            Size = 24,
          ),
        ),
        StoreBufferSize = 4,
        StoreBufferThreshold = 3,
        IssueQueueSize = 10,
        IssueQueueCompEntrySize = 4,
        intPreg = IntPregParams(
          numEntries = 64,
          numRead = None,
          numWrite = None,
        ),
        vfPreg = VfPregParams(
          numEntries = 160,
          numRead = None,
          numWrite = None,
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
          enableTagEcc = true,
          enableDataEcc = true,
          cacheCtrlAddressOpt = Some(AddressSet(0x38022000, 0x7f))
        )),
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
          outReplace = false,
          lgMaxSize = 4
        ),
        sttlbParameters = TLBParameters(
          name = "sttlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false,
          lgMaxSize = 4
        ),
        hytlbParameters = TLBParameters(
          name = "hytlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false,
          lgMaxSize = 4
        ),
        pftlbParameters = TLBParameters(
          name = "pftlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false,
          lgMaxSize = 4
        ),
        btlbParameters = TLBParameters(
          name = "btlb",
          NWays = 4,
        ),
        l2tlbParameters = L2TLBParameters(
          l3Size = 4,
          l2Size = 4,
          l1nSets = 4,
          l1nWays = 4,
          l1ReservedBits = 1,
          l0nSets = 4,
          l0nWays = 8,
          l0ReservedBits = 0,
          spSize = 4,
        ),
        L2CacheParamsOpt = Some(L2Param(
          name = "L2",
          ways = 8,
          sets = 128,
          echoField = Seq(huancun.DirtyField()),
          prefetch = Nil,
          clientCaches = Seq(L1Param(
            "dcache",
            isKeywordBitsOpt = p.dcacheParametersOpt.get.isKeywordBitsOpt
          )),
        )),
        L2NBanks = 2,
        prefetcher = Nil // if L2 pf_recv_node does not exist, disable SMS prefetcher
      )
    )
    case SoCParamsKey =>
      val tiles = site(XSTileKey)
      up(SoCParamsKey).copy(
        L3CacheParamsOpt = Option.when(!up(EnableCHI))(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
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
        OpenLLCParamsOpt = Option.when(up(EnableCHI))(OpenLLCParam(
          name = "LLC",
          ways = 8,
          sets = 2048,
          banks = 4,
          clientCaches = Seq(L2Param())
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
      L3CacheParamsOpt = None,
      OpenLLCParamsOpt = None
    )
  })
)

case class WithNKBL1D(n: Int, ways: Int = 8) extends Config((site, here, up) => {
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
        enableTagEcc = true,
        enableDataEcc = true,
        cacheCtrlAddressOpt = Some(AddressSet(0x38022000, 0x7f))
      ))
    ))
})

case class L2CacheConfig
(
  size: String,
  ways: Int = 8,
  inclusive: Boolean = true,
  banks: Int = 1,
  tp: Boolean = true,
  enableFlush: Boolean = false
) extends Config((site, here, up) => {
  case XSTileKey =>
    require(inclusive, "L2 must be inclusive")
    val nKB = size.toUpperCase() match {
      case s"${k}KB" => k.trim().toInt
      case s"${m}MB" => (m.trim().toDouble * 1024).toInt
    }
    val upParams = up(XSTileKey)
    val l2sets = nKB * 1024 / banks / ways / 64
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
          vaddrBitsOpt = Some(p.GPAddrBitsSv48x4 - log2Up(p.dcacheParametersOpt.get.blockBytes)),
          isKeywordBitsOpt = p.dcacheParametersOpt.get.isKeywordBitsOpt
        )),
        reqField = Seq(utility.ReqSourceField()),
        echoField = Seq(huancun.DirtyField()),
        tagECC = Some("secded"),
        dataECC = Some("secded"),
        enableTagECC = true,
        enableDataECC = true,
        dataCheck = Some("oddparity"),
        enablePoison = true,
        prefetch = Seq(BOPParameters()) ++
          (if (tp) Seq(TPParameters()) else Nil) ++
          (if (p.prefetcher.nonEmpty) Seq(PrefetchReceiverParams()) else Nil),
        enableL2Flush = enableFlush,
        enablePerf = !site(DebugOptionsKey).FPGAPlatform && site(DebugOptionsKey).EnablePerfDebug,
        enableRollingDB = site(DebugOptionsKey).EnableRollingDB,
        enableMonitor = site(DebugOptionsKey).AlwaysBasicDB,
        elaboratedTopDown = !site(DebugOptionsKey).FPGAPlatform,
        hasMbist = site(DFTOptionsKey).EnableMbist,
        hasSramCtl = site(DFTOptionsKey).EnableSramCtl,
      )),
      L2NBanks = banks
    ))
})

case class L3CacheConfig(size: String, ways: Int = 8, inclusive: Boolean = true, banks: Int = 1) extends Config((site, here, up) => {
  case SoCParamsKey =>
    val nKB = size.toUpperCase() match {
      case s"${k}KB" => k.trim().toInt
      case s"${m}MB" => (m.trim().toDouble * 1024).toInt
    }
    val sets = nKB * 1024 / banks / ways / 64
    val tiles = site(XSTileKey)
    val clientDirBytes = tiles.map{ t =>
      t.L2NBanks * t.L2CacheParamsOpt.map(_.toCacheParams.capacity).getOrElse(0)
    }.sum
    up(SoCParamsKey).copy(
      L3NBanks = banks,
      L3CacheParamsOpt = Option.when(!up(EnableCHI))(HCCacheParameters(
        name = "L3",
        level = 3,
        ways = ways,
        sets = sets,
        inclusive = inclusive,
        clientCaches = tiles.map{ core =>
          val l2params = core.L2CacheParamsOpt.get.toCacheParams
          l2params.copy(sets = 2 * clientDirBytes / core.L2NBanks / l2params.ways / 64, ways = l2params.ways + 2)
        },
        enablePerf = !site(DebugOptionsKey).FPGAPlatform && site(DebugOptionsKey).EnablePerfDebug,
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
        prefetch = Some(huancun.prefetch.L3PrefetchReceiverParams()),
        tpmeta = Some(huancun.prefetch.DefaultTPmetaParameters())
      )),
      OpenLLCParamsOpt = Option.when(up(EnableCHI))(OpenLLCParam(
        name = "LLC",
        ways = ways,
        sets = sets,
        banks = banks,
        fullAddressBits = 48,
        clientCaches = tiles.map { core =>
          val l2params = core.L2CacheParamsOpt.get
          l2params.copy(sets = 2 * clientDirBytes / core.L2NBanks / l2params.ways / 64, ways = l2params.ways + 2)
        },
        enablePerf = !site(DebugOptionsKey).FPGAPlatform && site(DebugOptionsKey).EnablePerfDebug,
        elaboratedTopDown = !site(DebugOptionsKey).FPGAPlatform
      ))
    )
})

class WithL3DebugConfig extends Config(
  L3CacheConfig("256KB", inclusive = false) ++ L2CacheConfig("64KB")
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
    L3CacheParamsOpt = up(SoCParamsKey).L3CacheParamsOpt.map(_.copy(
      enablePerf = false,
    )),
    OpenLLCParamsOpt = up(SoCParamsKey).OpenLLCParamsOpt.map(_.copy(
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

class CVMCompile extends Config((site, here, up) => {
  case CVMParamsKey => up(CVMParamsKey).copy(
    KeyIDBits = 5,
    HasMEMencryption = true,
    HasDelayNoencryption = false
  )
  case XSTileKey => up(XSTileKey).map(_.copy(
    HasBitmapCheck = true,
    HasBitmapCheckDefault = false))
})

class CVMTestCompile extends Config((site, here, up) => {
  case CVMParamsKey => up(CVMParamsKey).copy(
    KeyIDBits = 5,
    HasMEMencryption = true,
    HasDelayNoencryption = true
  )
  case XSTileKey => up(XSTileKey).map(_.copy(
    HasBitmapCheck =true,
    HasBitmapCheckDefault = true))
})

class MinimalAliasDebugConfig(n: Int = 1) extends Config(
  L3CacheConfig("512KB", inclusive = false)
    ++ L2CacheConfig("256KB", inclusive = true)
    ++ WithNKBL1D(128)
    ++ new MinimalConfig(n)
)

class MediumConfig(n: Int = 1) extends Config(
  L3CacheConfig("4MB", inclusive = false, banks = 4)
    ++ L2CacheConfig("512KB", inclusive = true)
    ++ WithNKBL1D(128)
    ++ new BaseConfig(n)
)

class FuzzConfig(dummy: Int = 0) extends Config(
  new WithFuzzer
    ++ new KunminghuV2Config(1)
)

class DefaultConfig(n: Int = 1) extends Config(
  L3CacheConfig("16MB", inclusive = false, banks = 4, ways = 16)
    ++ L2CacheConfig("1MB", inclusive = true, banks = 4)
    ++ WithNKBL1D(64, ways = 4)
    ++ new BaseConfig(n)
)

class CVMConfig(n: Int = 1) extends Config(
  new CVMCompile
    ++ new DefaultConfig(n)
)

class CVMTestConfig(n: Int = 1) extends Config(
  new CVMTestCompile
    ++ new DefaultConfig(n)
)

class WithCHI extends Config((_, _, _) => {
  case EnableCHI => true
})

class KunminghuV2Config(n: Int = 1) extends Config(
  L2CacheConfig("1MB", inclusive = true, banks = 4, tp = false)
    ++ new DefaultConfig(n)
    ++ new WithCHI
)

class KunminghuV2MinimalConfig(n: Int = 1) extends Config(
  L2CacheConfig("128KB", inclusive = true, banks = 1, tp = false)
    ++ WithNKBL1D(32, ways = 4)
    ++ new MinimalConfig(n)
    ++ new WithCHI
)

class XSNoCTopConfig(n: Int = 1) extends Config(
  (new KunminghuV2Config(n)).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(UseXSNoCTop = true)
  })
)

class XSNoCTopMinimalConfig(n: Int = 1) extends Config(
  (new KunminghuV2MinimalConfig(n)).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(UseXSNoCTop = true)
  })
)

class XSNoCDiffTopConfig(n: Int = 1) extends Config(
  (new XSNoCTopConfig(n)).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(UseXSNoCDiffTop = true)
  })
)

class XSNoCDiffTopMinimalConfig(n: Int = 1) extends Config(
  (new XSNoCTopConfig(n)).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(UseXSNoCDiffTop = true)
  })
)

class FpgaDefaultConfig(n: Int = 1) extends Config(
  (L3CacheConfig("3MB", inclusive = false, banks = 1, ways = 6)
    ++ L2CacheConfig("1MB", inclusive = true, banks = 4)
    ++ WithNKBL1D(64, ways = 4)
    ++ new BaseConfig(n)).alter((site, here, up) => {
    case DebugOptionsKey => up(DebugOptionsKey).copy(
      AlwaysBasicDiff = false,
      AlwaysBasicDB = false
    )
    case SoCParamsKey => up(SoCParamsKey).copy(
      L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
        sramClkDivBy2 = false,
      )),
    )
  })
)

class FpgaDiffDefaultConfig(n: Int = 1) extends Config(
  (L3CacheConfig("3MB", inclusive = false, banks = 1, ways = 6)
    ++ L2CacheConfig("1MB", inclusive = true, banks = 4)
    ++ WithNKBL1D(64, ways = 4)
    ++ new BaseConfig(n)).alter((site, here, up) => {
    case DebugOptionsKey => up(DebugOptionsKey).copy(
      AlwaysBasicDiff = true,
      AlwaysBasicDB = false
    )
    case SoCParamsKey => up(SoCParamsKey).copy(
      UseXSTileDiffTop = true,
      L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
        sramClkDivBy2 = false,
      )),
    )
  })
)

class FpgaDiffMinimalConfig(n: Int = 1) extends Config(
  (new MinimalConfig(n)).alter((site, here, up) => {
    case DebugOptionsKey => up(DebugOptionsKey).copy(
      AlwaysBasicDiff = true,
      AlwaysBasicDB = false
    )
    case SoCParamsKey => up(SoCParamsKey).copy(
      UseXSTileDiffTop = true,
      L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
        sramClkDivBy2 = false,
      )),
    )
  })
)
