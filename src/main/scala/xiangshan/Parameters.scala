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

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import xiangshan.backend.exu._
import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.cache.DCacheParameters
import xiangshan.cache.prefetch._
import huancun.{CacheParameters, HCCacheParameters}
import xiangshan.frontend.{BIM, BasePredictor, BranchPredictionResp, FTB, FakePredictor, ICacheParameters, MicroBTB, RAS, Tage, ITTage, Tage_SC}
import xiangshan.cache.mmu.{TLBParameters, L2TLBParameters}
import freechips.rocketchip.diplomacy.AddressSet

case object XSCoreParamsKey extends Field[XSCoreParameters]

case class XSCoreParameters
(
  HasPrefetch: Boolean = false,
  HartId: Int = 0,
  XLEN: Int = 64,
  HasMExtension: Boolean = true,
  HasCExtension: Boolean = true,
  HasDiv: Boolean = true,
  HasICache: Boolean = true,
  HasDCache: Boolean = true,
  AddrBits: Int = 64,
  VAddrBits: Int = 39,
  PAddrBits: Int = 40,
  HasFPU: Boolean = true,
  HasCustomCSRCacheOp: Boolean = true,
  FetchWidth: Int = 8,
  EnableBPU: Boolean = true,
  EnableBPD: Boolean = true,
  EnableRAS: Boolean = true,
  EnableLB: Boolean = false,
  EnableLoop: Boolean = true,
  EnableSC: Boolean = true,
  EnbaleTlbDebug: Boolean = false,
  EnableJal: Boolean = false,
  EnableUBTB: Boolean = true,
  HistoryLength: Int = 64,
  PathHistoryLength: Int = 16,
  BtbSize: Int = 2048,
  JbtacSize: Int = 1024,
  JbtacBanks: Int = 8,
  RasSize: Int = 32,
  CacheLineSize: Int = 512,
  UBtbWays: Int = 16,
  BtbWays: Int = 2,
  branchPredictor: Function3[BranchPredictionResp, Parameters, Boolean, Tuple2[Seq[BasePredictor], BranchPredictionResp]] =
    ((resp_in: BranchPredictionResp, p: Parameters, enableSC: Boolean) => {
      // val loop = Module(new LoopPredictor)
      // val tage = (if(EnableBPD) { if (EnableSC) Module(new Tage_SC)
      //                             else          Module(new Tage) }
      //             else          { Module(new FakeTage) })
      val ftb = Module(new FTB()(p))
      val ubtb = Module(new MicroBTB()(p))
      val bim = Module(new BIM()(p))
      val tage = if (enableSC) { Module(new Tage_SC()(p)) } else { Module(new Tage()(p)) }
      val ras = Module(new RAS()(p))
      val ittage = Module(new ITTage()(p))
      // val tage = Module(new Tage()(p))
      // val fake = Module(new FakePredictor()(p))

      // val preds = Seq(loop, tage, btb, ubtb, bim)
      val preds = Seq(bim, ubtb, tage, ftb, ittage, ras)
      preds.map(_.io := DontCare)

      // ubtb.io.resp_in(0)  := resp_in
      // bim.io.resp_in(0)   := ubtb.io.resp
      // btb.io.resp_in(0)   := bim.io.resp
      // tage.io.resp_in(0)  := btb.io.resp
      // loop.io.resp_in(0)  := tage.io.resp
      bim.io.in.bits.resp_in(0)  := resp_in
      ubtb.io.in.bits.resp_in(0) := bim.io.out.resp
      tage.io.in.bits.resp_in(0) := ubtb.io.out.resp
      ftb.io.in.bits.resp_in(0)  := tage.io.out.resp
      ittage.io.in.bits.resp_in(0)  := ftb.io.out.resp
      ras.io.in.bits.resp_in(0) := ittage.io.out.resp

      (preds, ras.io.out.resp)
    }),
  IBufSize: Int = 48,
  DecodeWidth: Int = 6,
  RenameWidth: Int = 6,
  CommitWidth: Int = 6,
  FtqSize: Int = 64,
  EnableLoadFastWakeUp: Boolean = true, // NOTE: not supported now, make it false
  IssQueSize: Int = 16,
  NRPhyRegs: Int = 192,
  NRIntReadPorts: Int = 14,
  NRIntWritePorts: Int = 8,
  NRFpReadPorts: Int = 14,
  NRFpWritePorts: Int = 8,
  LoadQueueSize: Int = 80,
  StoreQueueSize: Int = 64,
  RobSize: Int = 256,
  dpParams: DispatchParameters = DispatchParameters(
    IntDqSize = 16,
    FpDqSize = 16,
    LsDqSize = 16,
    IntDqDeqWidth = 4,
    FpDqDeqWidth = 4,
    LsDqDeqWidth = 4
  ),
  exuParameters: ExuParameters = ExuParameters(
    JmpCnt = 1,
    AluCnt = 4,
    MulCnt = 0,
    MduCnt = 2,
    FmacCnt = 4,
    FmiscCnt = 2,
    FmiscDivSqrtCnt = 0,
    LduCnt = 2,
    StuCnt = 2
  ),
  LoadPipelineWidth: Int = 2,
  StorePipelineWidth: Int = 2,
  StoreBufferSize: Int = 16,
  StoreBufferThreshold: Int = 7,
  EnableFastForward: Boolean = true,
  RefillSize: Int = 512,
  itlbParameters: TLBParameters = TLBParameters(
    name = "itlb",
    fetchi = true,
    useDmode = false,
    sameCycle = true,
    normalNWays = 32,
    normalReplacer = Some("plru"),
    superNWays = 4,
    superReplacer = Some("plru"),
    shouldBlock = true
  ),
  ldtlbParameters: TLBParameters = TLBParameters(
    name = "ldtlb",
    normalNSets = 128,
    normalNWays = 1,
    normalAssociative = "sa",
    normalReplacer = Some("setplru"),
    superNWays = 8,
    normalAsVictim = true,
    outReplace = true
  ),
  sttlbParameters: TLBParameters = TLBParameters(
    name = "sttlb",
    normalNSets = 128,
    normalNWays = 1,
    normalAssociative = "sa",
    normalReplacer = Some("setplru"),
    superNWays = 8,
    normalAsVictim = true,
    outReplace = true
  ),
  refillBothTlb: Boolean = false,
  btlbParameters: TLBParameters = TLBParameters(
    name = "btlb",
    normalNSets = 1,
    normalNWays = 64,
    superNWays = 4,
  ),
  useBTlb: Boolean = false,
  l2tlbParameters: L2TLBParameters = L2TLBParameters(),
  NumPMP: Int = 16, // 0 or 16 or 64
  NumPerfCounters: Int = 16,
  icacheParameters: ICacheParameters = ICacheParameters(
    tagECC = Some("parity"),
    dataECC = Some("parity"),
    replacer = Some("setplru"),
    nMissEntries = 2
  ),
  dcacheParametersOpt: Option[DCacheParameters] = Some(DCacheParameters(
    tagECC = Some("secded"),
    dataECC = Some("secded"),
    replacer = Some("setplru"),
    nMissEntries = 16,
    nProbeEntries = 16,
    nReleaseEntries = 32
  )),
  L2CacheParamsOpt: Option[HCCacheParameters] = Some(HCCacheParameters(
    name = "l2",
    level = 2,
    ways = 8,
    sets = 1024, // default 512KB L2
    prefetch = Some(huancun.prefetch.BOPParameters())
  )),
  L2NBanks: Int = 1,
  usePTWRepeater: Boolean = false,
  softPTW: Boolean = false // dpi-c debug only
){
  val loadExuConfigs = Seq.fill(exuParameters.LduCnt)(LdExeUnitCfg)
  val storeExuConfigs = Seq.fill(exuParameters.StuCnt)(StaExeUnitCfg) ++ Seq.fill(exuParameters.StuCnt)(StdExeUnitCfg)

  val intExuConfigs = (Seq.fill(exuParameters.AluCnt)(AluExeUnitCfg) ++
    Seq.fill(exuParameters.MduCnt)(MulDivExeUnitCfg) :+ JumpCSRExeUnitCfg)

  val fpExuConfigs =
    Seq.fill(exuParameters.FmacCnt)(FmacExeUnitCfg) ++
      Seq.fill(exuParameters.FmiscCnt)(FmiscExeUnitCfg)

  val exuConfigs: Seq[ExuConfig] = intExuConfigs ++ fpExuConfigs ++ loadExuConfigs ++ storeExuConfigs
}

case object DebugOptionsKey extends Field[DebugOptions]

case class DebugOptions
(
  FPGAPlatform: Boolean = true,
  EnableDebug: Boolean = true,
  EnablePerfDebug: Boolean = true,
  UseDRAMSim: Boolean = false
)

trait HasXSParameter {

  implicit val p: Parameters

  val coreParams = p(XSCoreParamsKey)
  val env = p(DebugOptionsKey)

  val XLEN = coreParams.XLEN
  val hardId = coreParams.HartId
  val minFLen = 32
  val fLen = 64
  def xLen = XLEN

  val HasMExtension = coreParams.HasMExtension
  val HasCExtension = coreParams.HasCExtension
  val HasDiv = coreParams.HasDiv
  val HasIcache = coreParams.HasICache
  val HasDcache = coreParams.HasDCache
  val AddrBits = coreParams.AddrBits // AddrBits is used in some cases
  val VAddrBits = coreParams.VAddrBits // VAddrBits is Virtual Memory addr bits
  val PAddrBits = coreParams.PAddrBits // PAddrBits is Phyical Memory addr bits
  val AddrBytes = AddrBits / 8 // unused
  val DataBits = XLEN
  val DataBytes = DataBits / 8
  val HasFPU = coreParams.HasFPU
  val HasCustomCSRCacheOp = coreParams.HasCustomCSRCacheOp
  val FetchWidth = coreParams.FetchWidth
  val PredictWidth = FetchWidth * (if (HasCExtension) 2 else 1)
  val EnableBPU = coreParams.EnableBPU
  val EnableBPD = coreParams.EnableBPD // enable backing predictor(like Tage) in BPUStage3
  val EnableRAS = coreParams.EnableRAS
  val EnableLB = coreParams.EnableLB
  val EnableLoop = coreParams.EnableLoop
  val EnableSC = coreParams.EnableSC
  val EnbaleTlbDebug = coreParams.EnbaleTlbDebug
  val HistoryLength = coreParams.HistoryLength
  val PathHistoryLength = coreParams.PathHistoryLength
  val BtbSize = coreParams.BtbSize
  // val BtbWays = 4
  val BtbBanks = PredictWidth
  // val BtbSets = BtbSize / BtbWays
  val JbtacSize = coreParams.JbtacSize
  val JbtacBanks = coreParams.JbtacBanks
  val RasSize = coreParams.RasSize

  def getBPDComponents(resp_in: BranchPredictionResp, p: Parameters, enableSC: Boolean) = {
    coreParams.branchPredictor(resp_in, p, enableSC)
  }

  val CacheLineSize = coreParams.CacheLineSize
  val CacheLineHalfWord = CacheLineSize / 16
  val ExtHistoryLength = HistoryLength + 64
  val UBtbWays = coreParams.UBtbWays
  val BtbWays = coreParams.BtbWays
  val IBufSize = coreParams.IBufSize
  val DecodeWidth = coreParams.DecodeWidth
  val RenameWidth = coreParams.RenameWidth
  val CommitWidth = coreParams.CommitWidth
  val FtqSize = coreParams.FtqSize
  val IssQueSize = coreParams.IssQueSize
  val EnableLoadFastWakeUp = coreParams.EnableLoadFastWakeUp
  val NRPhyRegs = coreParams.NRPhyRegs
  val PhyRegIdxWidth = log2Up(NRPhyRegs)
  val RobSize = coreParams.RobSize
  val IntRefCounterWidth = log2Ceil(RobSize)
  val StdFreeListSize = NRPhyRegs - 32
  val MEFreeListSize = NRPhyRegs
  val LoadQueueSize = coreParams.LoadQueueSize
  val StoreQueueSize = coreParams.StoreQueueSize
  val dpParams = coreParams.dpParams
  val exuParameters = coreParams.exuParameters
  val NRMemReadPorts = exuParameters.LduCnt + 2 * exuParameters.StuCnt
  val NRIntReadPorts = 2 * exuParameters.AluCnt + NRMemReadPorts
  val NRIntWritePorts = exuParameters.AluCnt + exuParameters.MduCnt + exuParameters.LduCnt
  val NRFpReadPorts = 3 * exuParameters.FmacCnt + exuParameters.StuCnt
  val NRFpWritePorts = exuParameters.FpExuCnt + exuParameters.LduCnt
  val LoadPipelineWidth = coreParams.LoadPipelineWidth
  val StorePipelineWidth = coreParams.StorePipelineWidth
  val StoreBufferSize = coreParams.StoreBufferSize
  val StoreBufferThreshold = coreParams.StoreBufferThreshold
  val EnableFastForward = coreParams.EnableFastForward
  val RefillSize = coreParams.RefillSize
  val BTLBWidth = coreParams.LoadPipelineWidth + coreParams.StorePipelineWidth
  val refillBothTlb = coreParams.refillBothTlb
  val useBTlb = coreParams.useBTlb
  val itlbParams = coreParams.itlbParameters
  val ldtlbParams = coreParams.ldtlbParameters
  val sttlbParams = coreParams.sttlbParameters
  val btlbParams = coreParams.btlbParameters
  val l2tlbParams = coreParams.l2tlbParameters
  val NumPMP = coreParams.NumPMP
  val PlatformGrain: Int = log2Up(coreParams.RefillSize/8) // set PlatformGrain to avoid itlb, dtlb, ptw size conflict
  val NumPerfCounters = coreParams.NumPerfCounters

  val instBytes = if (HasCExtension) 2 else 4
  val instOffsetBits = log2Ceil(instBytes)

  val icacheParameters = coreParams.icacheParameters
  val dcacheParameters = coreParams.dcacheParametersOpt.getOrElse(DCacheParameters())

  val LRSCCycles = 100

  // cache hierarchy configurations
  val l1BusDataWidth = 256

  // load violation predict
  val ResetTimeMax2Pow = 20 //1078576
  val ResetTimeMin2Pow = 10 //1024
  // wait table parameters
  val WaitTableSize = 1024
  val MemPredPCWidth = log2Up(WaitTableSize)
  val LWTUse2BitCounter = true
  // store set parameters
  val SSITSize = WaitTableSize
  val LFSTSize = 32
  val SSIDWidth = log2Up(LFSTSize)
  val LFSTWidth = 4
  val StoreSetEnable = true // LWT will be disabled if SS is enabled

  val loadExuConfigs = coreParams.loadExuConfigs
  val storeExuConfigs = coreParams.storeExuConfigs

  val intExuConfigs = coreParams.intExuConfigs

  val fpExuConfigs = coreParams.fpExuConfigs

  val exuConfigs = coreParams.exuConfigs

}
