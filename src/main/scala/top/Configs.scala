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
import chipsalliance.rocketchip.config._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, XLen}
import xiangshan.frontend.ICacheParameters
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
  case SoCParamsKey => SoCParameters(
    cores = List.tabulate(n){ i => XSCoreParameters(HartId = i) }
  )
  case ExportDebug => DebugAttachParams(protocols = Set(JTAG))
  case DebugModuleKey => Some(XSDebugModuleParams(site(XLen)))
  case JtagDTMKey => JtagDTMKey
  case MaxHartIdBits => 2
  case EnableJtag => false.B
})

// Synthesizable minimal XiangShan
// * It is still an out-of-order, super-scalaer arch
// * L1 cache included
// * L2 cache NOT included
// * L3 cache included
class MinimalConfig(n: Int = 1) extends Config(
  new BaseConfig(n).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(
      cores = up(SoCParamsKey).cores.map(_.copy(
        DecodeWidth = 2,
        RenameWidth = 2,
        FetchWidth = 4,
        IssQueSize = 8,
        NRPhyRegs = 64,
        LoadQueueSize = 16,
        StoreQueueSize = 12,
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
        icacheParameters = ICacheParameters(
          nSets = 64, // 16KB ICache
          tagECC = Some("parity"),
          dataECC = Some("parity"),
          replacer = Some("setplru"),
          nMissEntries = 2
        ),
        dcacheParametersOpt = Some(DCacheParameters(
          nSets = 64, // 32KB DCache
          nWays = 8,
          tagECC = Some("secded"),
          dataECC = Some("secded"),
          replacer = Some("setplru"),
          nMissEntries = 4,
          nProbeEntries = 4,
          nReleaseEntries = 4,
          nStoreReplayEntries = 4,
        )),
        EnableBPD = false, // disable TAGE
        EnableLoop = false,
        itlbParameters = TLBParameters(
          name = "itlb",
          fetchi = true,
          useDmode = false,
          sameCycle = true,
          normalReplacer = Some("plru"),
          superReplacer = Some("plru"),
          normalNWays = 4,
          normalNSets = 1,
          superNWays = 2,
          shouldBlock = true
        ),
        ldtlbParameters = TLBParameters(
          name = "ldtlb",
          normalNSets = 4, // when da or sa
          normalNWays = 1, // when fa or sa
          normalAssociative = "sa",
          normalReplacer = Some("setplru"),
          superNWays = 4,
          normalAsVictim = true,
          outReplace = true
        ),
        sttlbParameters = TLBParameters(
          name = "sttlb",
          normalNSets = 4, // when da or sa
          normalNWays = 1, // when fa or sa
          normalAssociative = "sa",
          normalReplacer = Some("setplru"),
          normalAsVictim = true,
          superNWays = 4,
          outReplace = true
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
          missQueueSize = 8
        ),
        L2CacheParamsOpt = None // remove L2 Cache
      )),
      L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
        sets = 1024
      )),
      L3NBanks = 1
    )
  })
)

// Non-synthesizable MinimalConfig, for fast simulation only
class MinimalSimConfig(n: Int = 1) extends Config(
  new MinimalConfig(n).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(
      cores = up(SoCParamsKey).cores.map(_.copy(
        dcacheParametersOpt = None,
        softPTW = true
      )),
      L3CacheParamsOpt = None
    )
  })
)

class WithNKBL1D(n: Int, ways: Int = 8) extends Config((site, here, up) => {
  case SoCParamsKey =>
    val upParams = up(SoCParamsKey)
    val sets = n * 1024 / ways / 64
    upParams.copy(cores = upParams.cores.map(p => p.copy(
      dcacheParametersOpt = Some(DCacheParameters(
        nSets = sets,
        nWays = ways,
        tagECC = Some("secded"),
        dataECC = Some("secded"),
        replacer = Some("setplru"),
        nMissEntries = 16,
        nProbeEntries = 16,
        nReleaseEntries = 16,
        nStoreReplayEntries = 16
      ))
    )))
})

class WithNKBL2(n: Int, ways: Int = 8, inclusive: Boolean = true, alwaysReleaseData: Boolean = false) extends Config((site, here, up) => {
  case SoCParamsKey =>
    val upParams = up(SoCParamsKey)
    val l2sets = n * 1024 / ways / 64
    upParams.copy(
      cores = upParams.cores.map(p => p.copy(
        L2CacheParamsOpt = Some(HCCacheParameters(
          name = "L2",
          level = 2,
          ways = ways,
          sets = l2sets,
          inclusive = inclusive,
          alwaysReleaseData = alwaysReleaseData,
          clientCaches = Seq(CacheParameters(
            "dcache",
            sets = 2 * p.dcacheParametersOpt.get.nSets,
            ways = p.dcacheParametersOpt.get.nWays + 2,
            aliasBitsOpt = p.dcacheParametersOpt.get.aliasBitsOpt
          )),
          reqField = Seq(PreferCacheField()),
          echoField = Seq(DirtyField()),
          prefetch = Some(huancun.prefetch.BOPParameters()),
          enablePerf = true
        )
      )))
    )
})

class WithNKBL3(n: Int, ways: Int = 8, inclusive: Boolean = true, banks: Int = 1) extends Config((site, here, up) => {
  case SoCParamsKey =>
    val upParams = up(SoCParamsKey)
    val sets = n * 1024 / banks / ways / 64
    upParams.copy(
      L3NBanks = banks,
      L3CacheParamsOpt = Some(HCCacheParameters(
        name = "L3",
        level = 3,
        ways = ways,
        sets = sets,
        inclusive = inclusive,
        clientCaches = upParams.cores.map{ core =>
          val l2params = core.L2CacheParamsOpt.get.toCacheParams
          l2params.copy(sets = 2 * l2params.sets, ways = l2params.ways + 1)
        },
        enablePerf = true
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

class MinimalAliasDebugConfig(n: Int = 1) extends Config(
  new WithNKBL3(512, inclusive = false) ++
    new WithNKBL2(256, inclusive = false, alwaysReleaseData = true) ++
    new WithNKBL1D(128) ++
    new MinimalConfig(n)
)

class DefaultConfig(n: Int = 1) extends Config(
  new WithNKBL3(4096, inclusive = false, banks = 4)
    ++ new WithNKBL2(512, inclusive = false, alwaysReleaseData = true)
    ++ new WithNKBL1D(128)
    ++ new BaseConfig(n)
)
