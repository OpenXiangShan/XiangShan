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
import xiangshan.frontend.{ICacheParameters}
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.tile.MaxHartIdBits
import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.backend.exu.ExuParameters
import xiangshan.cache.{DCacheParameters, L1plusCacheParameters}
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import xiangshan.cache.prefetch._
import device.{EnableJtag, XSDebugModuleParams}
import huancun.{CacheParameters, HCCacheParameters}

class DefaultConfig(n: Int) extends Config((site, here, up) => {
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
  new DefaultConfig(n).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(
      cores = up(SoCParamsKey).cores.map(_.copy(
        DecodeWidth = 2,
        RenameWidth = 2,
        FetchWidth = 4,
        IssQueSize = 8,
        NRPhyRegs = 64,
        LoadQueueSize = 16,
        StoreQueueSize = 12,
        RoqSize = 32,
        BrqSize = 8,
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
        dcacheParameters = DCacheParameters(
          nSets = 64, // 32KB DCache
          nWays = 8,
          tagECC = Some("secded"),
          dataECC = Some("secded"),
          replacer = Some("setplru"),
          nMissEntries = 4,
          nProbeEntries = 4,
          nReleaseEntries = 4,
          nStoreReplayEntries = 4,
        ),
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
        useFakeL2Cache = true, // disable L2 Cache
      )),
      L3CacheParams = up(SoCParamsKey).L3CacheParams.copy(
        sets = 1024
      ),
      L3NBanks = 1
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

class WithNKBL2(n: Int, ways: Int = 8, inclusive: Boolean = true) extends Config((site, here, up) => {
  case SoCParamsKey =>
    val upParams = up(SoCParamsKey)
    val l2sets = n * 1024 / ways / 64
    upParams.copy(
      cores = upParams.cores.map(p => p.copy(
        L2CacheParams = HCCacheParameters(
          name = "L2",
          level = 2,
          ways = ways,
          sets = l2sets,
          inclusive = inclusive,
          prefetch = Some(huancun.prefetch.BOPParameters())
        ),
        useFakeL2Cache = false,
        useFakeDCache = false,
        useFakePTW = false,
        useFakeL1plusCache = false
      ))
    )
})

class WithNKBL3(n: Int, ways: Int = 8, inclusive: Boolean = true, banks: Int = 1) extends Config((site, here, up) => {
  case SoCParamsKey =>
    val upParams = up(SoCParamsKey)
    val sets = n * 1024 / banks / ways / 64
    upParams.copy(
      L3NBanks = banks,
      L3CacheParams = HCCacheParameters(
        name = "L3",
        level = 3,
        ways = ways,
        sets = sets,
        inclusive = inclusive,
        clientCaches = upParams.cores.map{ core =>
          val l2params = core.L2CacheParams.toCacheParams
          l2params.copy(ways = 2 * l2params.ways)
        }
      )
    )
})

class WithL3DebugConfig extends Config(
  new WithNKBL3(256, inclusive = false) ++ new WithNKBL2(64)
)

class MinimalL3DebugConfig(n: Int = 1) extends Config(
  new WithL3DebugConfig ++ new MinimalConfig(n)
)

class DefaultL3DebugConfig(n: Int = 1) extends Config(
  new WithL3DebugConfig ++ new DefaultConfig(n)
)

class NonInclusiveL3Config(n: Int = 1) extends Config(
  new WithNKBL3(4096, inclusive = false, banks = 4) ++ new WithNKBL2(512) ++ new DefaultConfig(n)
)
