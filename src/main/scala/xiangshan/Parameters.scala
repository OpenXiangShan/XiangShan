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

package xiangshan

import org.chipsalliance.cde.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import huancun._
import system.SoCParamsKey
import system.CVMParamskey
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.issue.{FpScheduler, IntScheduler, IssueBlockParams, MemScheduler, SchdBlockParams, SchedulerType, VfScheduler}
import xiangshan.backend.regfile._
import xiangshan.backend.BackendParams
import xiangshan.backend.trace._
import xiangshan.cache.DCacheParameters
import xiangshan.frontend.bpu.BpuParameters
import xiangshan.frontend.icache.ICacheParameters
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import xiangshan.frontend._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tile.MaxHartIdBits
import system.SoCParamsKey
import huancun._
import huancun.debug._
import xiangshan.cache.wpu.WPUParameters
import coupledL2._
import coupledL2.tl2chi._
import xiangshan.backend.datapath.WakeUpConfig
import xiangshan.frontend.ftq.FtqParameters
import xiangshan.mem.prefetch.{PrefetcherParams, SMSParams, StreamStrideParams, TLBPlace}

import scala.math.{max, min, pow}

case object XSTileKey extends Field[Seq[XSCoreParameters]]

case object XSCoreParamsKey extends Field[XSCoreParameters]

case class XSCoreParameters
(
  HasPrefetch: Boolean = false,
  HartId: Int = 0,
  XLEN: Int = 64,
  VLEN: Int = 128,
  ELEN: Int = 64,
  HSXLEN: Int = 64,
  HasBitmapCheck: Boolean = false,
  HasBitmapCheckDefault: Boolean = false,
  HasMExtension: Boolean = true,
  HasCExtension: Boolean = true,
  HasHExtension: Boolean = true,
  HasDiv: Boolean = true,
  HasDCache: Boolean = true,
  AddrBits: Int = 64,
  PAddrBitsMax: Int = 56,   // The bits of physical address from Sv39/Sv48/Sv57 virtual address translation.
  VAddrBitsSv39: Int = 39,
  GPAddrBitsSv39x4: Int = 41,
  VAddrBitsSv48: Int = 48,
  GPAddrBitsSv48x4: Int = 50,
  HasFPU: Boolean = true,
  HasVPU: Boolean = true,
  HasCustomCSRCacheOp: Boolean = true,
  AsidLength: Int = 16,
  VmidLength: Int = 14,
  EnbaleTlbDebug: Boolean = false,
  EnableClockGate: Boolean = true,
  EnableJal: Boolean = false,
  EnableSv48: Boolean = true,
  EnableCommitGHistDiff: Boolean = true,
  CacheLineSize: Int = 512,
  DecodeWidth: Int = 8,
  RenameWidth: Int = 8,
  CommitWidth: Int = 8,
  RobCommitWidth: Int = 8,
  RabCommitWidth: Int = 8,
  MaxUopSize: Int = 65,
  EnableRenameSnapshot: Boolean = true,
  RenameSnapshotNum: Int = 4,
  // TODO: New frontend parameters system below. Replace the old parameters above during development.
  frontendParameters: FrontendParameters = FrontendParameters(),
  EnableLoadFastWakeUp: Boolean = true, // NOTE: not supported now, make it false
  IntLogicRegs: Int = 32,
  FpLogicRegs: Int = 32 + 1 + 1, // 1: I2F, 1: stride
  VecLogicRegs: Int = 32 + 15, // 15: tmp
  V0LogicRegs: Int = 1, // V0
  VlLogicRegs: Int = 1, // Vl
  V0_IDX: Int = 0,
  Vl_IDX: Int = 0,
  NRPhyRegs: Int = 192,
  VirtualLoadQueueSize: Int = 72,
  LoadQueueRARSize: Int = 72,
  LoadQueueRAWSize: Int = 32, // NOTE: make sure that LoadQueueRAWSize is power of 2.
  RollbackGroupSize: Int = 8,
  LoadQueueReplaySize: Int = 72,
  LoadUncacheBufferSize: Int = 4,
  LoadQueueNWriteBanks: Int = 8, // NOTE: make sure that LoadQueueRARSize/LoadQueueRAWSize is divided by LoadQueueNWriteBanks
  StoreQueueSize: Int = 56,
  StoreQueueNWriteBanks: Int = 8, // NOTE: make sure that StoreQueueSize is divided by StoreQueueNWriteBanks
  StoreQueueForwardWithMask: Boolean = true,
  VlsQueueSize: Int = 8,
  RobSize: Int = 224,
  RabSize: Int = 256,
  VTypeBufferSize: Int = 64, // used to reorder vtype
  IssueQueueSize: Int = 20,
  IssueQueueCompEntrySize: Int = 12,
  intPreg: PregParams = IntPregParams(
    numEntries = 224,
    numRead = None,
    numWrite = None,
  ),
  fpPreg: PregParams = FpPregParams(
    numEntries = 192,
    numRead = None,
    numWrite = None,
  ),
  vfPreg: VfPregParams = VfPregParams(
    numEntries = 128,
    numRead = None,
    numWrite = None,
  ),
  v0Preg: V0PregParams = V0PregParams(
    numEntries = 22,
    numRead = None,
    numWrite = None,
  ),
  vlPreg: VlPregParams = VlPregParams(
    numEntries = 32,
    numRead = None,
    numWrite = None,
  ),
  IntRegCacheSize: Int = 16,
  MemRegCacheSize: Int = 12,
  intSchdVlWbPort: Int = 0,
  vfSchdVlWbPort: Int = 1,
  prefetcher: Seq[PrefetcherParams] = Seq(StreamStrideParams(), SMSParams()),
  IfuRedirectNum: Int = 1,
  LoadPipelineWidth: Int = 3,
  StorePipelineWidth: Int = 2,
  VecLoadPipelineWidth: Int = 2,
  VecStorePipelineWidth: Int = 2,
  VecMemSrcInWidth: Int = 2,
  VecMemInstWbWidth: Int = 1,
  VecMemDispatchWidth: Int = 1,
  VecMemDispatchMaxNumber: Int = 16,
  VecMemUnitStrideMaxFlowNum: Int = 2,
  VecMemLSQEnqIteratorNumberSeq: Seq[Int] = Seq(16, 16, 16, 16, 16, 16),
  StoreBufferSize: Int = 16,
  StoreBufferThreshold: Int = 7,
  EnsbufferWidth: Int = 2,
  LoadDependencyWidth: Int = 2,
  // ============ VLSU ============
  VlMergeBufferSize: Int = 16,
  VsMergeBufferSize: Int = 16,
  UopWritebackWidth: Int = 2,
  VLUopWritebackWidth: Int = 2,
  VSUopWritebackWidth: Int = 1,
  VSegmentBufferSize: Int = 8,
  // ==============================
  UncacheBufferSize: Int = 4,
  EnableLoadToLoadForward: Boolean = false,
  EnableFastForward: Boolean = true,
  EnableLdVioCheckAfterReset: Boolean = true,
  EnableSoftPrefetchAfterReset: Boolean = true,
  EnableCacheErrorAfterReset: Boolean = true,
  EnableAccurateLoadError: Boolean = true,
  EnableUncacheWriteOutstanding: Boolean = false,
  EnableHardwareStoreMisalign: Boolean = true,
  EnableHardwareLoadMisalign: Boolean = true,
  EnableStorePrefetchAtIssue: Boolean = false,
  EnableStorePrefetchAtCommit: Boolean = false,
  EnableAtCommitMissTrigger: Boolean = true,
  EnableStorePrefetchSMS: Boolean = false,
  EnableStorePrefetchSPB: Boolean = false,
  HasCMO: Boolean = true,
  MMUAsidLen: Int = 16, // max is 16, 0 is not supported now
  MMUVmidLen: Int = 14,
  ReSelectLen: Int = 7, // load replay queue replay select counter len
  iwpuParameters: WPUParameters = WPUParameters(
    enWPU = false,
    algoName = "mmru",
    isICache = true,
  ),
  dwpuParameters: WPUParameters = WPUParameters(
    enWPU = false,
    algoName = "mmru",
    enCfPred = false,
    isICache = false,
  ),
  itlbParameters: TLBParameters = TLBParameters(
    name = "itlb",
    fetchi = true,
    useDmode = false,
    NWays = 48,
  ),
  itlbPortNum: Int = 1 + 1, // ICache + Ifu
  ipmpPortNum: Int = 2 + 1, // ICache(PrefetchPipe + MainPipe) + Ifu
  ldtlbParameters: TLBParameters = TLBParameters(
    name = "ldtlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = false,
    lgMaxSize = 4
  ),
  sttlbParameters: TLBParameters = TLBParameters(
    name = "sttlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = false,
    lgMaxSize = 4
  ),
  hytlbParameters: TLBParameters = TLBParameters(
    name = "hytlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = false,
    lgMaxSize = 4
  ),
  pftlbParameters: TLBParameters = TLBParameters(
    name = "pftlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = false,
    lgMaxSize = 4
  ),
  l2ToL1tlbParameters: TLBParameters = TLBParameters(
    name = "l2tlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = false
  ),
  refillBothTlb: Boolean = false,
  btlbParameters: TLBParameters = TLBParameters(
    name = "btlb",
    NWays = 48,
  ),
  l2tlbParameters: L2TLBParameters = L2TLBParameters(),
  NumPerfCounters: Int = 16,
  dcacheParametersOpt: Option[DCacheParameters] = Some(DCacheParameters(
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
  )),
  L2CacheParamsOpt: Option[L2Param] = Some(L2Param(
    name = "l2",
    ways = 8,
    sets = 1024, // default 512KB L2
    prefetch = Seq(coupledL2.prefetch.PrefetchReceiverParams(), coupledL2.prefetch.BOPParameters(),
      coupledL2.prefetch.TPParameters()),
  )),
  L2NBanks: Int = 1,
  usePTWRepeater: Boolean = false,
  softTLB: Boolean = false, // dpi-c l1tlb debug only
  softPTW: Boolean = false, // dpi-c l2tlb debug only
  softPTWDelay: Int = 1,
  wfiResume: Boolean = true,
){
  def ISABase = "rv64i"
  def ISAExtensions = Seq(
    // single letter extensions, in canonical order
    "i", "m", "a", "f", "d", "c", /* "b", */ "v", "h",
    // multi-letter extensions, sorted alphanumerically
    "sdtrig", "sha", "shcounterenw", "shgatpa", "shlcofideleg", "shtvala", "shvsatpa", "shvstvala",
    "shvstvecd", "smaia", "smcsrind", "smdbltrp", "smmpm", "smnpm", "smrnmi", "smstateen",
    "ss1p13", "ssaia", "ssccptr", "sscofpmf", "sscounterenw", "sscsrind", "ssdbltrp", "ssnpm",
    "sspm", "ssstateen", "ssstrict", "sstc", "sstvala", "sstvecd", "ssu64xl", "supm", "sv39",
    "sv48", "svade", "svbare", "svinval", "svnapot", "svpbmt", "za64rs", "zacas", "zawrs", "zba",
    "zbb", "zbc", "zbkb", "zbkc", "zbkx", "zbs", "zcb", "zcmop", "zfa", "zfh", "zfhmin", "zic64b",
    "zicbom", "zicbop", "zicboz", "ziccamoa", "ziccif", "zicclsm", "ziccrse", "zicntr", "zicond",
    "zicsr", "zifencei", "zihintntl", "zihintpause", "zihpm", "zimop", "zkn", "zknd", "zkne", "zknh",
    "zksed", "zksh", "zkt", "zvbb", "zvfh", "zvfhmin", "zvkt", "zvl128b", "zvl32b", "zvl64b"
  )

  def vlWidth = log2Up(VLEN) + 1

  /**
   * the minimum element length of vector elements
   */
  val minVecElen: Int = 8

  /**
   * the maximum number of elements in vector register
   */
  val maxElemPerVreg: Int = VLEN / minVecElen

  val RegCacheSize = IntRegCacheSize + MemRegCacheSize
  val RegCacheIdxWidth = log2Up(RegCacheSize)

  val intSchdParams = {
    implicit val schdType: SchedulerType = IntScheduler()
    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams("ALU0", Seq(AluCfg, BrhCfg, JmpCfg, CsrCfg, FenceCfg), Seq(IntWB(port = 0, 0)), Seq(Seq(IntRD(0, 0)), Seq(IntRD(12, 0))), true, 2)
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("ALU1", Seq(AluCfg, BrhCfg, JmpCfg, DivCfg), Seq(IntWB(port = 1, 0)), Seq(Seq(IntRD(1, 0)), Seq(IntRD(13, 0))), true, 2)
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("ALU2", Seq(AluCfg, BrhCfg, JmpCfg, I2fCfg, VSetRiWiCfg, VSetRiWvfCfg, I2vCfg), Seq(IntWB(port = 2, 0), VfWB(4, 0), V0WB(port = 2, 0), VlWB(port = intSchdVlWbPort, 0), FpWB(port = 0, 1)), Seq(Seq(IntRD(2, 0)), Seq(IntRD(14, 0))), true, 2)
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("ALU3", Seq(AluCfg, BkuCfg), Seq(IntWB(port = 3, 0)), Seq(Seq(IntRD(3, 0)), Seq(IntRD(6, 0))), true, 2)
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("ALU4", Seq(AluCfg, MulCfg), Seq(IntWB(port = 4, 0)), Seq(Seq(IntRD(4, 0)), Seq(IntRD(7, 0))), true, 2)
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("ALU5", Seq(AluCfg, MulCfg), Seq(IntWB(port = 5, 0)), Seq(Seq(IntRD(5, 0)), Seq(IntRD(8, 0))), true, 2)
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
    ),
      numPregs = intPreg.numEntries,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = intPreg.dataCfg.dataWidth,
    )
  }

  val fpSchdParams = {
    implicit val schdType: SchedulerType = FpScheduler()
    SchdBlockParams(Seq(
      // FcmpCfg and FcvtCfg must be in the same ExuUnit because they both need to write to the integer register file.
      IssueBlockParams(Seq(
        ExeUnitParams("FEX0", Seq(FaluCfg, FmacCfg, FcvtCfg, FcmpCfg, F2vCfg), Seq(FpWB(port = 0, 0), IntWB(port = 3, 1), VfWB(port = 5, 0), V0WB(port = 3, 0)), Seq(Seq(FpRD(0, 0)), Seq(FpRD(1, 0)), Seq(FpRD(2, 0)))),
      ), numEntries = 20, numEnq = 2, numComp = 16),
      IssueBlockParams(Seq(
        ExeUnitParams("FEX1", Seq(FaluCfg, FmacCfg, FdivCfg), Seq(FpWB(port = 1, 0)), Seq(Seq(FpRD(3, 0)), Seq(FpRD(4, 0)), Seq(FpRD(5, 0)))),
      ), numEntries = 20, numEnq = 2, numComp = 16),
      IssueBlockParams(Seq(
        ExeUnitParams("FEX2", Seq(FaluCfg, FmacCfg, FdivCfg), Seq(FpWB(port = 2, 0)), Seq(Seq(FpRD(6, 0)), Seq(FpRD(7, 0)), Seq(FpRD(8, 0)))),
      ), numEntries = 20, numEnq = 2, numComp = 16),
    ),
      numPregs = fpPreg.numEntries,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = fpPreg.dataCfg.dataWidth,
    )
  }

  val vfSchdParams = {
    implicit val schdType: SchedulerType = VfScheduler()
    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams("VFEX0", Seq(VialuCfg, VfaluCfg, VfmaCfg, VimacCfg, VppuCfg, VipuCfg, VfcvtCfg, VSetRvfWvfCfg), Seq(VfWB(port = 0, 0), V0WB(port = 0, 0), VlWB(port = vfSchdVlWbPort, 0), IntWB(port = 4, 1), FpWB(port = 6, 0)), Seq(Seq(VfRD(0, 0)), Seq(VfRD(1, 0)), Seq(VfRD(2, 0)), Seq(V0RD(0, 0)), Seq(VlRD(0, 0)))),
      ), numEntries = 16, numEnq = 2, numComp = 12),
      IssueBlockParams(Seq(
        ExeUnitParams("VFEX1", Seq(VialuCfg, VfaluCfg, VfmaCfg, VfdivCfg, VidivCfg), Seq(VfWB(port = 1, 0), V0WB(port = 1, 0), FpWB(port = 7, 0)), Seq(Seq(VfRD(3, 0)), Seq(VfRD(4, 0)), Seq(VfRD(5, 0)), Seq(V0RD(1, 0)), Seq(VlRD(1, 0)))),
      ), numEntries = 16, numEnq = 2, numComp = 12),
    ),
      numPregs = vfPreg.numEntries,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = vfPreg.dataCfg.dataWidth,
    )
  }

  val memSchdParams = {
    implicit val schdType: SchedulerType = MemScheduler()
    val rfDataWidth = 64

    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams("STA0", Seq(StaCfg, MouCfg), Seq(FakeIntWB()), Seq(Seq(IntRD(0, 1)))),
      ), numEntries = 16, numEnq = 2, numComp = 12),
      IssueBlockParams(Seq(
        ExeUnitParams("STA1", Seq(StaCfg, MouCfg), Seq(FakeIntWB()), Seq(Seq(IntRD(1, 1)))),
      ), numEntries = 16, numEnq = 2, numComp = 12),
      IssueBlockParams(Seq(
        ExeUnitParams("LDU0", Seq(LduCfg), Seq(IntWB(6, 0), FpWB(3, 0)), Seq(Seq(IntRD(9, 0))), true, 2),
      ), numEntries = 20, numEnq = 2, numComp = 12),
      IssueBlockParams(Seq(
        ExeUnitParams("LDU1", Seq(LduCfg), Seq(IntWB(7, 0), FpWB(4, 0)), Seq(Seq(IntRD(10, 0))), true, 2),
      ), numEntries = 20, numEnq = 2, numComp = 12),
      IssueBlockParams(Seq(
        ExeUnitParams("LDU2", Seq(LduCfg), Seq(IntWB(8, 0), FpWB(5, 0)), Seq(Seq(IntRD(11, 0))), true, 2),
      ), numEntries = 20, numEnq = 2, numComp = 12),
      IssueBlockParams(Seq(
        ExeUnitParams("VLSU0", Seq(VlduCfg, VstuCfg, VseglduSeg, VsegstuCfg), Seq(VfWB(2, 0), V0WB(2, 0), VlWB(port = 2, 0)), Seq(Seq(VfRD(6, 0)), Seq(VfRD(7, 0)), Seq(VfRD(8, 0)), Seq(V0RD(2, 0)), Seq(VlRD(2, 0)))),
      ), numEntries = 16, numEnq = 2, numComp = 12),
      IssueBlockParams(Seq(
        ExeUnitParams("VLSU1", Seq(VlduCfg, VstuCfg), Seq(VfWB(3, 0), V0WB(3, 0), VlWB(port = 3, 0)), Seq(Seq(VfRD(9, 0)), Seq(VfRD(10, 0)), Seq(VfRD(11, 0)), Seq(V0RD(3, 0)), Seq(VlRD(3, 0)))),
      ), numEntries = 16, numEnq = 2, numComp = 12),
      IssueBlockParams(Seq(
        ExeUnitParams("STD0", Seq(StdCfg, MoudCfg), Seq(), Seq(Seq(IntRD(2, 1), FpRD(9, 0)))),
      ), numEntries = 16, numEnq = 2, numComp = 12),
      IssueBlockParams(Seq(
        ExeUnitParams("STD1", Seq(StdCfg, MoudCfg), Seq(), Seq(Seq(IntRD(3, 1), FpRD(10, 0)))),
      ), numEntries = 16, numEnq = 2, numComp = 12),
    ),
      numPregs = intPreg.numEntries max vfPreg.numEntries,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = rfDataWidth,
    )
  }

  def PregIdxWidthMax = intPreg.addrWidth max vfPreg.addrWidth

  def iqWakeUpParams = {
    Seq(
      WakeUpConfig(
        Seq("ALU0", "ALU1", "ALU2", "ALU3", "ALU4", "ALU5", "LDU0", "LDU1", "LDU2") ->
        Seq("ALU0", "ALU1", "ALU2", "ALU3", "ALU4", "ALU5", "LDU0", "LDU1", "LDU2", "STA0", "STA1", "STD0", "STD1")
      ),
      WakeUpConfig(
        Seq("FEX0", "FEX1", "FEX2") ->
        Seq("FEX0", "FEX1", "FEX2")
      ),
      // TODO load wakeup to fp delay 1 cycle
      WakeUpConfig(
        Seq("LDU0", "LDU1", "LDU2") ->
        Seq("FEX0", "FEX1", "FEX2")
      ),
      // TODO fp wakeup to std delay 1 cycle
      WakeUpConfig(
        Seq("FEX0", "FEX1", "FEX2") ->
        Seq("STD0", "STD1")
      ),
    ).flatten
  }

  def fakeIntPreg = FakeIntPregParams(intPreg.numEntries, intPreg.numRead, intPreg.numWrite)

  val backendParams: BackendParams = backend.BackendParams(
    Map(
      IntScheduler() -> intSchdParams,
      FpScheduler() -> fpSchdParams,
      VfScheduler() -> vfSchdParams,
      MemScheduler() -> memSchdParams,
    ),
    Seq(
      intPreg,
      fpPreg,
      vfPreg,
      v0Preg,
      vlPreg,
      fakeIntPreg
    ),
    iqWakeUpParams,
  )

  // Parameters for trace extension.
  // Trace parameters is useful for XSTOP.
  val traceParams: TraceParams = new TraceParams(
    TraceGroupNum  = 3,
    IaddrWidth     = GPAddrBitsSv48x4,
    PrivWidth      = 3,
    ItypeWidth     = 4,
    IlastsizeWidth = 1,
  )
}

case object DebugOptionsKey extends Field[DebugOptions]

case class DebugOptions
(
  FPGAPlatform: Boolean = false,
  ResetGen: Boolean = false,
  EnableDifftest: Boolean = false,
  AlwaysBasicDiff: Boolean = true,
  EnableDebug: Boolean = false,
  EnablePerfDebug: Boolean = true,
  PerfLevel: String = "VERBOSE",
  UseDRAMSim: Boolean = false,
  EnableConstantin: Boolean = false,
  EnableChiselDB: Boolean = false,
  AlwaysBasicDB: Boolean = true,
  EnableRollingDB: Boolean = false
)

case object DFTOptionsKey extends Field[DFTOptions]

case class DFTOptions
(
  EnableMbist: Boolean = true, // enable mbist default
  EnableSramCtl: Boolean = false,
)

trait HasXSParameter {

  implicit val p: Parameters

  def PAddrBits = p(SoCParamsKey).PAddrBits // PAddrBits is Phyical Memory addr bits
  def PmemRanges = p(SoCParamsKey).PmemRanges
  def KeyIDBits = p(CVMParamskey).KeyIDBits
  final val PageOffsetWidth = 12
  def NodeIDWidth = p(SoCParamsKey).NodeIDWidthList(p(CHIIssue)) // NodeID width among NoC

  def coreParams = p(XSCoreParamsKey)
  def env = p(DebugOptionsKey)

  def ISABase = coreParams.ISABase
  def ISAExtensions = coreParams.ISAExtensions
  def XLEN = coreParams.XLEN
  def VLEN = coreParams.VLEN
  def ELEN = coreParams.ELEN
  def HSXLEN = coreParams.HSXLEN
  val minFLen = 32
  val fLen = 64
  def hartIdLen = p(MaxHartIdBits)
  val xLen = XLEN

  def HasBitmapCheck = coreParams.HasBitmapCheck
  def HasBitmapCheckDefault = coreParams.HasBitmapCheckDefault
  
  /** prefetch config */
  def prefetcherSeq = coreParams.prefetcher
  def prefetcherNum = max(prefetcherSeq.size, 1) //TODO lyq: 1 for simpler code generation, but it's also ugly
  def PfNumInDtlbLD = prefetcherSeq.count(_.tlbPlace == TLBPlace.dtlb_ld)
  def PfNumInDtlbPF = prefetcherSeq.count(_.tlbPlace == TLBPlace.dtlb_pf) + 1 // 1 for l2 prefetch

  def HasMExtension = coreParams.HasMExtension
  def HasCExtension = coreParams.HasCExtension
  def HasHExtension = coreParams.HasHExtension
  def EnableSv48 = coreParams.EnableSv48
  def HasDiv = coreParams.HasDiv
  def HasDcache = coreParams.HasDCache
  def AddrBits = coreParams.AddrBits // AddrBits is used in some cases
  def PAddrBitsMax = coreParams.PAddrBitsMax
  def GPAddrBitsSv39x4 = coreParams.GPAddrBitsSv39x4
  def GPAddrBitsSv48x4 = coreParams.GPAddrBitsSv48x4
  def GPAddrBits = {
    if (EnableSv48)
      coreParams.GPAddrBitsSv48x4
    else
      coreParams.GPAddrBitsSv39x4
  }
  def VAddrBits = {
    if (HasHExtension) {
      if (EnableSv48)
        coreParams.GPAddrBitsSv48x4
      else
        coreParams.GPAddrBitsSv39x4
    } else {
      if (EnableSv48)
        coreParams.VAddrBitsSv48
      else
        coreParams.VAddrBitsSv39
    }
  } // VAddrBits is Virtual Memory addr bits

  def VAddrMaxBits = {
    if(EnableSv48) {
      coreParams.VAddrBitsSv48 max coreParams.GPAddrBitsSv48x4
    } else {
      coreParams.VAddrBitsSv39 max coreParams.GPAddrBitsSv39x4
    }
  }

  def AsidLength = coreParams.AsidLength
  def VmidLength = coreParams.VmidLength
  def ReSelectLen = coreParams.ReSelectLen
  def AddrBytes = AddrBits / 8 // unused
  def DataBits = XLEN
  def DataBytes = DataBits / 8
  def QuadWordBits = DataBits * 2
  def QuadWordBytes = QuadWordBits / 8
  def VDataBytes = VLEN / 8
  def HasFPU = coreParams.HasFPU
  def HasVPU = coreParams.HasVPU
  def HasCustomCSRCacheOp = coreParams.HasCustomCSRCacheOp
  def EnbaleTlbDebug = coreParams.EnbaleTlbDebug
  def EnableCommitGHistDiff = coreParams.EnableCommitGHistDiff
  def EnableClockGate = coreParams.EnableClockGate

  def CacheLineSize = coreParams.CacheLineSize
  def CacheLineHalfWord = CacheLineSize / 16
  def FtqSize = coreParams.frontendParameters.ftqParameters.FtqSize
  def FetchBlockInstOffsetWidth: Int = log2Ceil(coreParams.frontendParameters.FetchBlockSize / instBytes)
  def backendParams: BackendParams = coreParams.backendParams
  def DecodeWidth = coreParams.DecodeWidth
  def RenameWidth = coreParams.RenameWidth
  def CommitWidth = coreParams.CommitWidth
  def RobCommitWidth = coreParams.RobCommitWidth
  def RabCommitWidth = coreParams.RabCommitWidth
  def MaxUopSize = coreParams.MaxUopSize
  def EnableRenameSnapshot = coreParams.EnableRenameSnapshot
  def RenameSnapshotNum = coreParams.RenameSnapshotNum
  def EnableLoadFastWakeUp = coreParams.EnableLoadFastWakeUp
  def IntLogicRegs = coreParams.IntLogicRegs
  def FpLogicRegs = coreParams.FpLogicRegs
  def VecLogicRegs = coreParams.VecLogicRegs
  def V0LogicRegs = coreParams.V0LogicRegs
  def VlLogicRegs = coreParams.VlLogicRegs
  def MaxLogicRegs = Set(IntLogicRegs, FpLogicRegs, VecLogicRegs, V0LogicRegs, VlLogicRegs).max
  def LogicRegsWidth = log2Ceil(MaxLogicRegs)
  def V0_IDX = coreParams.V0_IDX
  def Vl_IDX = coreParams.Vl_IDX
  def IntPhyRegs = coreParams.intPreg.numEntries
  def FpPhyRegs = coreParams.fpPreg.numEntries
  def VfPhyRegs = coreParams.vfPreg.numEntries
  def V0PhyRegs = coreParams.v0Preg.numEntries
  def VlPhyRegs = coreParams.vlPreg.numEntries
  def MaxPhyRegs = Seq(IntPhyRegs, FpPhyRegs, VfPhyRegs, V0PhyRegs, VlPhyRegs).max
  def IntPhyRegIdxWidth = log2Up(IntPhyRegs)
  def FpPhyRegIdxWidth = log2Up(FpPhyRegs)
  def VfPhyRegIdxWidth = log2Up(VfPhyRegs)
  def V0PhyRegIdxWidth = log2Up(V0PhyRegs)
  def VlPhyRegIdxWidth = log2Up(VlPhyRegs)
  def PhyRegIdxWidth = Seq(IntPhyRegIdxWidth, FpPhyRegIdxWidth, VfPhyRegIdxWidth, V0PhyRegIdxWidth, VlPhyRegIdxWidth).max
  def RobSize = coreParams.RobSize
  def RabSize = coreParams.RabSize
  def VTypeBufferSize = coreParams.VTypeBufferSize
  def IntRegCacheSize = coreParams.IntRegCacheSize
  def MemRegCacheSize = coreParams.MemRegCacheSize
  def RegCacheSize = coreParams.RegCacheSize
  def RegCacheIdxWidth = coreParams.RegCacheIdxWidth
  /**
   * the minimum element length of vector elements
   */
  def minVecElen: Int = coreParams.minVecElen

  /**
   * the maximum number of elements in vector register
   */
  def maxElemPerVreg: Int = coreParams.maxElemPerVreg

  def IntRefCounterWidth = log2Ceil(RobSize)
  def LSQEnqWidth = RenameWidth
  def LSQLdEnqWidth = LSQEnqWidth min backendParams.numLoadDp
  def LSQStEnqWidth = LSQEnqWidth min backendParams.numStoreDp
  def VirtualLoadQueueSize = coreParams.VirtualLoadQueueSize
  def LoadQueueRARSize = coreParams.LoadQueueRARSize
  def LoadQueueRAWSize = coreParams.LoadQueueRAWSize
  def RollbackGroupSize = coreParams.RollbackGroupSize
  val RAWlgSelectGroupSize = log2Ceil(RollbackGroupSize)
  val RAWTotalDelayCycles = scala.math.ceil(log2Ceil(LoadQueueRAWSize).toFloat / RAWlgSelectGroupSize).toInt + 1 - 2
  def LoadQueueReplaySize = coreParams.LoadQueueReplaySize
  def LoadUncacheBufferSize = coreParams.LoadUncacheBufferSize
  def LoadQueueNWriteBanks = coreParams.LoadQueueNWriteBanks
  def StoreQueueSize = coreParams.StoreQueueSize
  def StoreQueueForceWriteSbufferUpper = coreParams.StoreQueueSize - 4
  def StoreQueueForceWriteSbufferLower = StoreQueueForceWriteSbufferUpper - 5
  def VirtualLoadQueueMaxStoreQueueSize = VirtualLoadQueueSize max StoreQueueSize
  def StoreQueueNWriteBanks = coreParams.StoreQueueNWriteBanks
  def StoreQueueForwardWithMask = coreParams.StoreQueueForwardWithMask
  def VlsQueueSize = coreParams.VlsQueueSize

  def MemIQSizeMax = backendParams.memSchdParams.get.issueBlockParams.map(_.numEntries).max
  def IQSizeMax = backendParams.allSchdParams.map(_.issueBlockParams.map(_.numEntries).max).max

  def NumRedirect = backendParams.numRedirect
  def BackendRedirectNum = NumRedirect + 2 //2: ldReplay + Exception
  def FtqRedirectAheadNum = NumRedirect
  def IfuRedirectNum = coreParams.IfuRedirectNum
  def LoadPipelineWidth = coreParams.LoadPipelineWidth
  def StorePipelineWidth = coreParams.StorePipelineWidth
  def VecLoadPipelineWidth = coreParams.VecLoadPipelineWidth
  def VecStorePipelineWidth = coreParams.VecStorePipelineWidth
  def VecMemSrcInWidth = coreParams.VecMemSrcInWidth
  def VecMemInstWbWidth = coreParams.VecMemInstWbWidth
  def VecMemDispatchWidth = coreParams.VecMemDispatchWidth
  def VecMemDispatchMaxNumber = coreParams.VecMemDispatchMaxNumber
  def VecMemUnitStrideMaxFlowNum = coreParams.VecMemUnitStrideMaxFlowNum
  def VecMemLSQEnqIteratorNumberSeq = coreParams.VecMemLSQEnqIteratorNumberSeq
  def StoreBufferSize = coreParams.StoreBufferSize
  def StoreBufferThreshold = coreParams.StoreBufferThreshold
  def EnsbufferWidth = coreParams.EnsbufferWidth
  def LoadDependencyWidth = coreParams.LoadDependencyWidth
  def VlMergeBufferSize = coreParams.VlMergeBufferSize
  def VsMergeBufferSize = coreParams.VsMergeBufferSize
  def UopWritebackWidth = coreParams.UopWritebackWidth
  def VLUopWritebackWidth = coreParams.VLUopWritebackWidth
  def VSUopWritebackWidth = coreParams.VSUopWritebackWidth
  def VSegmentBufferSize = coreParams.VSegmentBufferSize
  def UncacheBufferSize = coreParams.UncacheBufferSize
  def UncacheBufferIndexWidth = log2Up(UncacheBufferSize)
  def EnableLoadToLoadForward = coreParams.EnableLoadToLoadForward
  def EnableFastForward = coreParams.EnableFastForward
  def EnableLdVioCheckAfterReset = coreParams.EnableLdVioCheckAfterReset
  def EnableSoftPrefetchAfterReset = coreParams.EnableSoftPrefetchAfterReset
  def EnableCacheErrorAfterReset = coreParams.EnableCacheErrorAfterReset
  def EnableAccurateLoadError = coreParams.EnableAccurateLoadError
  def EnableUncacheWriteOutstanding = coreParams.EnableUncacheWriteOutstanding
  def EnableHardwareStoreMisalign = coreParams.EnableHardwareStoreMisalign
  def EnableHardwareLoadMisalign = coreParams.EnableHardwareLoadMisalign
  def EnableStorePrefetchAtIssue = coreParams.EnableStorePrefetchAtIssue
  def EnableStorePrefetchAtCommit = coreParams.EnableStorePrefetchAtCommit
  def EnableAtCommitMissTrigger = coreParams.EnableAtCommitMissTrigger
  def EnableStorePrefetchSMS = coreParams.EnableStorePrefetchSMS
  def EnableStorePrefetchSPB = coreParams.EnableStorePrefetchSPB
  def HasCMO = coreParams.HasCMO && p(EnableCHI)
  require(LoadPipelineWidth == backendParams.LdExuCnt, "LoadPipelineWidth must be equal exuParameters.LduCnt!")
  require(StorePipelineWidth == backendParams.StaCnt, "StorePipelineWidth must be equal exuParameters.StuCnt!")
  def Enable3Load3Store = (LoadPipelineWidth == 3 && StorePipelineWidth == 3)
  def asidLen = coreParams.MMUAsidLen
  def vmidLen = coreParams.MMUVmidLen
  def BTLBWidth = coreParams.LoadPipelineWidth + coreParams.StorePipelineWidth
  def refillBothTlb = coreParams.refillBothTlb
  def iwpuParam = coreParams.iwpuParameters
  def dwpuParam = coreParams.dwpuParameters
  def itlbParams = coreParams.itlbParameters
  def ldtlbParams = coreParams.ldtlbParameters
  def sttlbParams = coreParams.sttlbParameters
  def hytlbParams = coreParams.hytlbParameters
  def pftlbParams = coreParams.pftlbParameters
  def l2ToL1Params = coreParams.l2ToL1tlbParameters
  def btlbParams = coreParams.btlbParameters
  def l2tlbParams = coreParams.l2tlbParameters
  def NumPerfCounters = coreParams.NumPerfCounters

  def instBytes = if (HasCExtension) 2 else 4
  def instOffsetBits = log2Ceil(instBytes)

  def icacheCtrlEnabled = coreParams.frontendParameters.icacheParameters.EnableCtrlUnit
  def icacheCtrlAddress = coreParams.frontendParameters.icacheParameters.ctrlUnitParameters.Address // valid only when icacheCtrlEnabled is true

  def dcacheParameters = coreParams.dcacheParametersOpt.getOrElse(DCacheParameters())

  // dcache block cacheline when lr for LRSCCycles - LRSCBackOff cycles
  // for constrained LR/SC loop
  def LRSCCycles = 64
  // for lr storm
  def LRSCBackOff = 8

  // cache hierarchy configurations
  def l1BusDataWidth = 256

  // load violation predict
  def ResetTimeMax2Pow = 20 //1078576
  def ResetTimeMin2Pow = 10 //1024
  // wait table parameters
  def WaitTableSize = 1024
  def MemPredPCWidth = log2Up(WaitTableSize)
  def LWTUse2BitCounter = true
  // store set parameters
  def SSITSize = WaitTableSize
  def LFSTSize = 32
  def SSIDWidth = log2Up(LFSTSize)
  def LFSTWidth = 4
  def StoreSetEnable = true // LWT will be disabled if SS is enabled
  def LFSTEnable = true

  def PCntIncrStep: Int = 6
  def numPCntHc: Int = 12
  def numPCntPtw: Int = 19

  def numCSRPCntFrontend = 8
  def numCSRPCntCtrl     = 8
  def numCSRPCntLsu      = 8
  def numCSRPCntHc       = 5
  def printEventCoding   = true
  def printCriticalError = false
  def maxCommitStuck = pow(2, 21).toInt

  // Vector load exception
  def maxMergeNumPerCycle = 4

  // Parameters for Sdtrig extension
  protected def TriggerNum = 4
  protected def TriggerChainMaxLength = 2

  // Parameters for Trace extension
  def TraceGroupNum          = coreParams.traceParams.TraceGroupNum
  def CauseWidth             = XLEN
  def TvalWidth              = coreParams.traceParams.IaddrWidth
  def PrivWidth              = coreParams.traceParams.PrivWidth
  def IaddrWidth             = coreParams.traceParams.IaddrWidth
  def ItypeWidth             = coreParams.traceParams.ItypeWidth
  def IretireWidthEncoded    = ((2 + RenameWidth + 1) * RenameWidth / 2).U.getWidth // 2 + 3 + ... + (RenameWidth + 1)
  def IretireWidthCommited   = (RenameWidth * 2).U.getWidth
  def IretireWidthCompressed = (RenameWidth * CommitWidth * 2).U.getWidth
  def IlastsizeWidth         = coreParams.traceParams.IlastsizeWidth

  def wfiResume              = coreParams.wfiResume
  def hasMbist               = p(DFTOptionsKey).EnableMbist
  def hasSramCtl             = p(DFTOptionsKey).EnableSramCtl
  def hasDFT                 = hasMbist || hasSramCtl
}
