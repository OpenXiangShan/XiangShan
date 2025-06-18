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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.tile.HasFPUParameters
import freechips.rocketchip.tilelink._
import utils._
import utility._
import utility.mbist.{MbistInterface, MbistPipeline}
import utility.sram.{SramBroadcastBundle, SramHelper}
import system.{HasSoCParameter, SoCParamsKey}
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfoBundle, LsTopdownInfo}
import xiangshan.backend.exu.MemExeUnit
import xiangshan.backend.fu._
import xiangshan.backend.fu.FuType._
import xiangshan.backend.fu.NewCSR.{CsrTriggerBundle, TriggerUtil, PFEvent}
import xiangshan.backend.fu.util.{CSRConst, SdtrigExt}
import xiangshan.backend.{BackendToTopBundle, TopToBackendBundle}
import xiangshan.backend.rob.{RobDebugRollingIO, RobPtr, RobLsqIO}
import xiangshan.backend.datapath.NewPipelineConnect
import xiangshan.backend.trace.{Itype, TraceCoreInterface}
import xiangshan.backend.Bundles._
import xiangshan.mem._
import xiangshan.mem.mdp._
import xiangshan.mem.Bundles._
import xiangshan.mem.prefetch.{BasePrefecher, L1Prefetcher, SMSParams, SMSPrefetcher}
import xiangshan.cache._
import xiangshan.cache.mmu._
import coupledL2.PrefetchRecv
import utility.mbist.{MbistInterface, MbistPipeline}
import utility.sram.{SramBroadcastBundle, SramHelper}
import system.HasSoCParameter
import xiangshan.frontend.instruncache.HasInstrUncacheConst

trait HasMemBlockParameters extends HasXSParameter {
  // number of memory units
  val LduCnt  = backendParams.LduCnt
  val StaCnt  = backendParams.StaCnt
  val StdCnt  = backendParams.StdCnt
  val HyuCnt  = backendParams.HyuCnt
  val VlduCnt = backendParams.VlduCnt
  val VstuCnt = backendParams.VstuCnt

  val LdExuCnt  = LduCnt + HyuCnt
  val StAddrCnt = StaCnt + HyuCnt
  val StDataCnt = StdCnt
  val MemExuCnt = LduCnt + HyuCnt + StaCnt + StdCnt
  val MemAddrExtCnt = LdExuCnt + StaCnt
  val MemVExuCnt = VlduCnt + VstuCnt

  val AtomicWBPort   = 0
  val MisalignWBPort = 1
  val UncacheWBPort  = 2
  val NCWBPorts = Seq(1, 2)
}

abstract class MemBlockBundle(implicit val p: Parameters) extends Bundle with HasMemBlockParameters

class ooo_to_mem(implicit p: Parameters) extends MemBlockBundle {
  val backendToTopBypass = Flipped(new BackendToTopBundle)

  val loadFastMatch = Vec(LdExuCnt, Input(UInt(LdExuCnt.W)))
  val loadFastFuOpType = Vec(LdExuCnt, Input(FuOpType()))
  val loadFastImm = Vec(LdExuCnt, Input(UInt(12.W)))
  val sfence = Input(new SfenceBundle)
  val tlbCsr = Input(new TlbCsrBundle)
  val lsqio = new Bundle {
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val scommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val pendingMMIOld = Input(Bool())
    val pendingld = Input(Bool())
    val pendingst = Input(Bool())
    val pendingVst = Input(Bool())
    val commit = Input(Bool())
    val pendingPtr = Input(new RobPtr)
    val pendingPtrNext = Input(new RobPtr)
  }

  val isStoreException = Input(Bool())
  val isVlsException = Input(Bool())
  val csrCtrl = Flipped(new CustomCSRCtrlIO)
  val enqLsq = new LsqEnqIO
  val flushSb = Input(Bool())

  val storePc = Vec(StaCnt, Input(UInt(VAddrBits.W))) // for hw prefetch
  val hybridPc = Vec(HyuCnt, Input(UInt(VAddrBits.W))) // for hw prefetch

  val issueLda = MixedVec(Seq.fill(LduCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueSta = MixedVec(Seq.fill(StaCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueStd = MixedVec(Seq.fill(StdCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueHya = MixedVec(Seq.fill(HyuCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueVldu = MixedVec(Seq.fill(VlduCnt)(Flipped(DecoupledIO(new MemExuInput(isVector=true)))))

  def issueUops = issueLda ++ issueSta ++ issueStd ++ issueHya ++ issueVldu
}

class mem_to_ooo(implicit p: Parameters) extends MemBlockBundle {
  val topToBackendBypass = new TopToBackendBundle

  val otherFastWakeup = Vec(LdExuCnt, ValidIO(new DynInst))
  val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
  val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
  val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
  // used by VLSU issue queue, the vector store would wait all store before it, and the vector load would wait all load
  val sqDeqPtr = Output(new SqPtr)
  val lqDeqPtr = Output(new LqPtr)
  val stIn = Vec(StAddrCnt, ValidIO(new MemExuInput))
  val stIssuePtr = Output(new SqPtr())

  val memoryViolation = ValidIO(new Redirect)
  val sbIsEmpty = Output(Bool())

  val lsTopdownInfo = Vec(LdExuCnt, Output(new LsTopdownInfo))

  val lsqio = new Bundle {
    val vaddr = Output(UInt(XLEN.W))
    val vstart = Output(UInt((log2Up(VLEN) + 1).W))
    val vl = Output(UInt((log2Up(VLEN) + 1).W))
    val gpaddr = Output(UInt(XLEN.W))
    val isForVSnonLeafPTE = Output(Bool())
    val mmio = Output(Vec(LoadPipelineWidth, Bool()))
    val uop = Output(Vec(LoadPipelineWidth, new DynInst))
    val lqCanAccept = Output(Bool())
    val sqCanAccept = Output(Bool())
  }

  val storeDebugInfo = Vec(EnsbufferWidth, new Bundle {
    val robidx = Output(new RobPtr)
    val pc     = Input(UInt(VAddrBits.W))
  })

  val writebackLda = Vec(LduCnt, DecoupledIO(new MemExuOutput))
  val writebackSta = Vec(StaCnt, DecoupledIO(new MemExuOutput))
  val writebackStd = Vec(StdCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuLda = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuSta = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackVldu = Vec(VlduCnt, DecoupledIO(new MemExuOutput(isVector = true)))
  def writeBack: Seq[DecoupledIO[MemExuOutput]] = {
    writebackSta ++
      writebackHyuLda ++ writebackHyuSta ++
      writebackLda ++
      writebackVldu ++
      writebackStd
  }

  val ldaIqFeedback = Vec(LduCnt, new MemRSFeedbackIO)
  val staIqFeedback = Vec(StaCnt, new MemRSFeedbackIO)
  val hyuIqFeedback = Vec(HyuCnt, new MemRSFeedbackIO)
  val vstuIqFeedback= Vec(VstuCnt, new MemRSFeedbackIO(isVector = true))
  val vlduIqFeedback= Vec(VlduCnt, new MemRSFeedbackIO(isVector = true))
  val ldCancel = Vec(backendParams.LdExuCnt, new LoadCancelIO)
  val wakeup = Vec(backendParams.LdExuCnt, Valid(new DynInst))

  val s3_delayed_load_error = Vec(LdExuCnt, Output(Bool()))
}

class MemCoreTopDownIO extends Bundle {
  val robHeadMissInDCache = Output(Bool())
  val robHeadTlbReplay = Output(Bool())
  val robHeadTlbMiss = Output(Bool())
  val robHeadLoadVio = Output(Bool())
  val robHeadLoadMSHR = Output(Bool())
}

class fetch_to_mem(implicit p: Parameters) extends XSBundle{
  val itlb = Flipped(new TlbPtwIO())
}

// triple buffer applied in i-mmio path (two at MemBlock, one at L2Top)
class InstrUncacheBuffer()(implicit p: Parameters) extends LazyModule with HasInstrUncacheConst {
  val node = new TLBufferNode(BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default)
  lazy val module = new InstrUncacheBufferImpl

  class InstrUncacheBufferImpl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.a <> BufferParams.default(BufferParams.default(in.a))
      in.d <> BufferParams.default(BufferParams.default(out.d))

      // only a.valid, a.ready, a.address can change
      // hoping that the rest would be optimized to keep MemBlock port unchanged after adding buffer
      out.a.bits.data := 0.U
      out.a.bits.mask := Fill(MmioBusBytes, 1.U(1.W))
      out.a.bits.opcode := 4.U // Get
      out.a.bits.size := log2Ceil(MmioBusBytes).U
      out.a.bits.source := 0.U
    }
  }
}

// triple buffer applied in L1I$-L2 path (two at MemBlock, one at L2Top)
class ICacheBuffer()(implicit p: Parameters) extends LazyModule {
  val node = new TLBufferNode(BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default)
  lazy val module = new ICacheBufferImpl

  class ICacheBufferImpl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.a <> BufferParams.default(BufferParams.default(in.a))
      in.d <> BufferParams.default(BufferParams.default(out.d))
    }
  }
}

class ICacheCtrlBuffer()(implicit p: Parameters) extends LazyModule {
  val node = new TLBufferNode(BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default)
  lazy val module = new ICacheCtrlBufferImpl

  class ICacheCtrlBufferImpl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.a <> BufferParams.default(BufferParams.default(in.a))
      in.d <> BufferParams.default(BufferParams.default(out.d))
    }
  }
}

// Frontend bus goes through MemBlock
class FrontendBridge()(implicit p: Parameters) extends LazyModule {
  val icache_node = LazyModule(new ICacheBuffer()).suggestName("icache").node// to keep IO port name
  val icachectrl_node = LazyModule(new ICacheCtrlBuffer()).suggestName("icachectrl").node
  val instr_uncache_node = LazyModule(new InstrUncacheBuffer()).suggestName("instr_uncache").node
  lazy val module = new LazyModuleImp(this) {
  }
}

class MemBlockInlined()(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  override def shouldBeInlined: Boolean = true

  val dcache = LazyModule(new DCacheWrapper())
  val uncache = LazyModule(new Uncache())
  val uncache_port = TLTempNode()
  val uncache_xbar = TLXbar()
  val ptw = LazyModule(new L2TLBWrapper())
  val ptw_to_l2_buffer = if (!coreParams.softPTW) LazyModule(new TLBuffer) else null
  val l1d_to_l2_buffer = if (coreParams.dcacheParametersOpt.nonEmpty) LazyModule(new TLBuffer) else null
  val dcache_port = TLNameNode("dcache_client") // to keep dcache-L2 port name
  val l2_pf_sender_opt = coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new PrefetchRecv)
  )
  val l3_pf_sender_opt = if (p(SoCParamsKey).L3CacheParamsOpt.nonEmpty) coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new huancun.PrefetchRecv)
  ) else None
  val frontendBridge = LazyModule(new FrontendBridge)
  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val debug_int_sink = IntSinkNode(IntSinkPortSimple(1, 1))
  val plic_int_sink = IntSinkNode(IntSinkPortSimple(2, 1))
  val nmi_int_sink = IntSinkNode(IntSinkPortSimple(1, (new NonmaskableInterruptIO).elements.size))
  val beu_local_int_sink = IntSinkNode(IntSinkPortSimple(1, 1))

  if (!coreParams.softPTW) {
    ptw_to_l2_buffer.node := ptw.node
  }
  uncache_xbar := TLBuffer() := uncache.clientNode
  if (dcache.uncacheNode.isDefined) {
    dcache.uncacheNode.get := TLBuffer.chainNode(2) := uncache_xbar
  }
  uncache_port := TLBuffer.chainNode(2) := uncache_xbar

  val stdUnits = stdUnitParams.map(param => LazyModule(new MemUnit(param)))
  val staUnits = staUnitParams.map(param => LazyModule(new MemUnit(param)))
  val loadUnits = ldUnitParams.map(param => LazyModule(new MemUnit(param)))
  val hyUnits = hyUnitParams.map(param => LazyModule(new MemUnit(param)))
  val amoUnits = amoUnitParams.map(param => LazyModule(new MemUnit(param)))

  require(amoUnits.length == 1, "Atomics must only one instantiate!")

  lazy val module = new MemBlockInlinedImp(this)
}

class MemBlockInlinedImp(outer: MemBlockInlined) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasFPUParameters
  with HasPerfEvents
  with HasSoCParameter
  with HasL1PrefetchSourceParameter
  with HasCircularQueuePtrHelper
  with HasMemBlockParameters
  with HasTlbConst
  with SdtrigExt
{
  val io = IO(new Bundle {
    val hartId = Input(UInt(hartIdLen.W))
    val redirect = Flipped(ValidIO(new Redirect))

    val ooo_to_mem = new ooo_to_mem
    val mem_to_ooo = new mem_to_ooo
    val fetch_to_mem = new fetch_to_mem

    val ifetchPrefetch = Vec(LduCnt, ValidIO(new SoftIfetchPrefetchBundle))

    // misc
    val error = ValidIO(new L1CacheErrorInfo)
    val memInfo = new Bundle {
      val sqFull = Output(Bool())
      val lqFull = Output(Bool())
      val dcacheMSHRFull = Output(Bool())
    }
    val debug_ls = new DebugLSIO
    val l2_hint = Input(Valid(new L2ToL1Hint()))
    val l2PfqBusy = Input(Bool())
    val l2_tlb_req = Flipped(new TlbRequestIO(nRespDups = 2))
    val l2_pmp_resp = new PMPRespBundle
    val l2_flush_done = Input(Bool())

    val debugTopDown = new Bundle {
      val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
      val toCore = new MemCoreTopDownIO
    }
    val debugRolling = Flipped(new RobDebugRollingIO)

    // All the signals from/to frontend/backend to/from bus will go through MemBlock
    val fromTopToBackend = Input(new Bundle {
      val msiInfo   = ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W))
      val clintTime = ValidIO(UInt(64.W))
    })
    val inner_hartId = Output(UInt(hartIdLen.W))
    val inner_reset_vector = Output(UInt(PAddrBits.W))
    val outer_reset_vector = Input(UInt(PAddrBits.W))
    val outer_cpu_halt = Output(Bool())
    val outer_l2_flush_en = Output(Bool())
    val outer_power_down_en = Output(Bool())
    val outer_cpu_critical_error = Output(Bool())
    val outer_msi_ack = Output(Bool())
    val inner_beu_errors_icache = Input(new L1BusErrorUnitInfo)
    val outer_beu_errors_icache = Output(new L1BusErrorUnitInfo)
    val inner_hc_perfEvents = Output(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))
    val outer_hc_perfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))

    // reset signals of frontend & backend are generated in memblock
    val reset_backend = Output(Reset())
    // Reset singal from frontend.
    val resetInFrontendBypass = new Bundle{
      val fromFrontend = Input(Bool())
      val toL2Top      = Output(Bool())
    }
    val traceCoreInterfaceBypass = new Bundle{
      val fromBackend = Flipped(new TraceCoreInterface(hasOffset = true))
      val toL2Top     = new TraceCoreInterface
    }

    val topDownInfo = new Bundle {
      val fromL2Top = Input(new TopDownFromL2Top)
      val toBackend = Flipped(new TopDownInfo)
    }
    val dft = Option.when(hasDFT)(Input(new SramBroadcastBundle))
    val dft_reset = Option.when(hasMbist)(Input(new DFTResetSignals()))
    val dft_frnt = Option.when(hasDFT)(Output(new SramBroadcastBundle))
    val dft_reset_frnt = Option.when(hasMbist)(Output(new DFTResetSignals()))
    val dft_bcknd = Option.when(hasDFT)(Output(new SramBroadcastBundle))
    val dft_reset_bcknd = Option.when(hasMbist)(Output(new DFTResetSignals()))
  })

  dontTouch(io.inner_hartId)
  dontTouch(io.inner_reset_vector)
  dontTouch(io.outer_reset_vector)
  dontTouch(io.outer_cpu_halt)
  dontTouch(io.outer_l2_flush_en)
  dontTouch(io.outer_power_down_en)
  dontTouch(io.outer_cpu_critical_error)
  dontTouch(io.inner_beu_errors_icache)
  dontTouch(io.outer_beu_errors_icache)
  dontTouch(io.inner_hc_perfEvents)
  dontTouch(io.outer_hc_perfEvents)

  val redirect = RegNextWithEnable(io.redirect)
  val csrCtrl = DelayN(io.ooo_to_mem.csrCtrl, 2)
  // trigger
  val triggerCtrl = RegInit(0.U.asTypeOf(new CsrTriggerBundle))
  triggerCtrl.tEnableVec := csrCtrl.mem_trigger.tEnableVec
  when(csrCtrl.mem_trigger.tUpdate.valid) {
    triggerCtrl.tdataVec(csrCtrl.mem_trigger.tUpdate.bits.addr) := csrCtrl.mem_trigger.tUpdate.bits.tdata
  }
  triggerCtrl.triggerCanRaiseBpExp := csrCtrl.mem_trigger.triggerCanRaiseBpExp
  triggerCtrl.debugMode := csrCtrl.mem_trigger.debugMode

  XSDebug(triggerCtrl.tEnableVec.asUInt.orR, "Debug Mode: At least one store trigger is enabled\n")
  for (j <- 0 until TriggerNum)
    PrintTriggerInfo(triggerCtrl.tEnableVec(j), triggerCtrl.tdataVec(j))

  // L2 Hint
  val l2_hint = RegNext(io.l2_hint)

  /**
    * --------------------------------------------------------------------
    * Modules
    * --------------------------------------------------------------------
    */
  val stdUnitImps = outer.stdUnits.map(_.module.asInstanceOf[StdImp])
  val staUnitImps = outer.staUnits.map(_.module.asInstanceOf[HyuImp])
  val ldUnitImps = outer.loadUnits.map(_.module.asInstanceOf[HyuImp])
  val hyUnitImps = outer.hyUnits.map(_.module.asInstanceOf[HyuImp])
  val amoUnitImps = outer.amoUnits.map(_.module.asInstanceOf[AmoImp])
  val uncache = outer.uncache.module
  private val dcache = outer.dcache.module

  val lduMods = ldUnitImps ++ hyUnitImps
  val staMods = staUnitImps ++ hyUnitImps

  // The number of vector load/store units is decoupled with the number of load/store units
  val vlSplit = Seq.fill(VlduCnt)(Module(new VLSplitImp))
  val vsSplit = Seq.fill(VstuCnt)(Module(new VSSplitImp))
  val vlMergeBuffer = Module(new VLMergeBufferImp)
  val vsMergeBuffer = Seq.fill(VstuCnt)(Module(new VSMergeBufferImp))
  val vSegmentUnit = Module(new VSegmentUnit)
  val vfofBuffer = Module(new VfofBuffer)
  val vSegmentFlag = RegInit(false.B)

  // misalign Buffer
  val loadMisalignBuffer = Module(new LoadMisalignBuffer)
  val storeMisalignBuffer = Module(new StoreMisalignBuffer)

  // Lsq
  val lsq = Module(new LsqWrapper)
  val sbuffer = Module(new Sbuffer)

  // Tlbs
  val ld_dtlb = Module(new TLBNonBlock(LduCnt + HyuCnt + 1, 2, ldtlbParams))
  val st_dtlb = Module(new TLBNonBlock(StaCnt, 2, sttlbParams))
  val pf_dtlb = Module(new TLBNonBlock(2, 2, pftlbParams))

  val dtlbs = Seq(ld_dtlb, st_dtlb, pf_dtlb)
  val (ld_dtlb_idx, st_dtlb_idx, pf_dtlb_idx) = (0, 1, 2)
  val TlbSubSizeVec = Seq(LduCnt + HyuCnt + 1, StaCnt, 2) // (load + hyu + stream pf, store, sms+l2bop)
  val DTlbSize = TlbSubSizeVec.sum
  val TlbStartVec = TlbSubSizeVec.scanLeft(0)(_ + _).dropRight(1)
  val TlbEndVec = TlbSubSizeVec.scanLeft(0)(_ + _).drop(1)

  val pmp = Module(new PMP())
  val pmp_checkers = Seq.fill(DTlbSize)(Module(new PMPChecker(4, leaveHitMux = true)))
  val pmp_check = pmp_checkers.map(_.io)
  private val ptw = outer.ptw.module
  private val ptw_to_l2_buffer = outer.ptw_to_l2_buffer.module
  private val l1d_to_l2_buffer = outer.l1d_to_l2_buffer.module

  val tlbreplay = WireInit(VecInit(Seq.fill(LdExuCnt)(false.B)))
  val tlbreplay_reg = GatedValidRegNext(tlbreplay)
  val dtlb_ld0_tlbreplay_reg = GatedValidRegNext(ld_dtlb.io.tlbreplay)

  val dtlb_reqs = dtlbs.map(_.io.requestor).flatten
  val dtlb_pmps = dtlbs.map(_.io.pmp).flatten

  // Prefetcher
  val smsPrefetcherOpt = coreParams.prefetcher.map { case _: SMSParams =>
    val mod = Module(new SMSPrefetcher())
    mod.io := DontCare
    mod
  }
  val l1PrefetcherOpt = coreParams.prefetcher.map { case _ =>
    val mod = Module(new L1Prefetcher())
    mod.io := DontCare
    mod
  }
  val l1_pf_req = Wire(Decoupled(new L1PrefetchReq()))

  // Performance
  val pfevent = Module(new PFEvent)

  /**
    * --------------------------------------------------------------------
    * MMU
    * --------------------------------------------------------------------
    */
  val sfence = RegNext(RegNext(io.ooo_to_mem.sfence))
  val tlbcsr = RegNext(RegNext(io.ooo_to_mem.tlbCsr))
  val ptwio = Wire(new VectorTlbPtwIO(DTlbSize))
  ptw.io.hartId := io.hartId
  ptw.io.sfence <> sfence
  ptw.io.csr.tlb <> tlbcsr
  ptw.io.csr.distribute_csr <> csrCtrl.distribute_csr
  ptwio.resp.ready := true.B
  val ptw_resp_next = RegEnable(ptwio.resp.bits, ptwio.resp.valid)
  val ptw_resp_v = RegNext(ptwio.resp.valid && !(sfence.valid && tlbcsr.satp.changed && tlbcsr.vsatp.changed && tlbcsr.hgatp.changed), init = false.B)
  val perfEventsPTW = Option.when(!coreParams.softPTW)(ptw.getPerfEvents)
  val dtlbRepeater  = PTWNewFilter(ldtlbParams.fenceDelay, ptwio, ptw.io.tlb(1), sfence, tlbcsr, l2tlbParams.dfilterSize)
  val itlbRepeater3 = PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, io.fetch_to_mem.itlb, ptw.io.tlb(0), sfence, tlbcsr)

  /**
    * --------------------------------------------------------------------
    * DCache
    * --------------------------------------------------------------------
    */
  dcache.io.hartId := io.hartId
  dcache.io.l2_hint <> l2_hint
  dcache.io.lqEmpty := lsq.io.lqEmpty
  dcache.io.l2_pf_store_only := csrCtrl.pf_ctrl.l2_pf_store_only
  dcache.io.force_write := lsq.io.force_write
  dcache.io.sms_agt_evict_req.ready := false.B
  dcache.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  dcache.io.debugTopDown.robHeadOtherReplay := lsq.io.debugTopDown.robHeadOtherReplay
  dcache.io.debugRolling := io.debugRolling
  io.error <> DelayNWithValid(dcache.io.error, 2)
  when(!csrCtrl.cache_error_enable){
    io.error.bits.report_to_beu := false.B
    io.error.valid := false.B
  }

  //
  val correctMissTrain = Constantin.createRecord(s"CorrectMissTrain$hartId", initValue = false)
  val misalignAllowSpec = RegInit(true.B)
  val rollbackWithMisalignNAck = lduMods.map { case impl: HyuImp =>
    impl.io.toLsq.writeback.bits.isMisAlignBuf &&
    ReplayCauseNO.hasRARF(impl.io.toLsq.writeback.bits.cause) &&
    impl.io.toBackend.rollback.valid
  }.reduce(_|_)

  val misalignSpecThreshold = Constantin.createRecord(s"MisalignSpecThreshold$hartId", initValue = LoadQueueRARSize - 4)
  when (rollbackWithMisalignNAck) {
    misalignAllowSpec := false.B
  } .elsewhen(lsq.io.rarValidCount < misalignSpecThreshold) {
    misalignAllowSpec := true.B
  }

  io.debug_ls := DontCare

  /**
    * --------------------------------------------------------------------
    * Std Units
    * --------------------------------------------------------------------
    */
  stdUnitImps.zipWithIndex.foreach { case (impl: StdImp, i) =>
    impl.io.fromCtrl.redirect <> redirect
    impl.io.fromCtrl.hartId <> io.hartId
    impl.io.fromCtrl.csrCtrl <> csrCtrl
    impl.io.fromCtrl.trigger <> triggerCtrl
    // from
    impl.io.fromBackend.issue.valid <> io.ooo_to_mem.issueStd(i).valid
    impl.io.fromBackend.issue.ready <> io.ooo_to_mem.issueStd(i).ready
    impl.io.fromBackend.issue.bits.fromMemExuInputBundle(io.ooo_to_mem.issueStd(i).bits, isStore = true)

    // Write data to Lsq
    val isNotAtomics = impl.io.toBackend.writeback.valid &&
      !FuType.storeIsAMO(impl.io.toBackend.writeback.bits.uop.fuType)
    if (i < VstuCnt) {
      // from MemExuOutput
      val vstdIn = Wire(new LsPipelineBundle)
      vstdIn.fromMemExuOutputBundle(vsSplit(i).io.vstd.get.bits)
      //
      lsq.io.std.storeDataIn(i).valid := vsSplit(i).io.vstd.get.valid || isNotAtomics
      lsq.io.std.storeDataIn(i).bits := ParallelPriorityMux(Seq(
        vsSplit(i).io.vstd.get.valid -> vstdIn,
        impl.io.toBackend.writeback.valid -> impl.io.toBackend.writeback.bits
      ))
      impl.io.toBackend.writeback.ready := !vsSplit(i).io.vstd.get.valid
    } else {
      lsq.io.std.storeDataIn(i).valid := isNotAtomics
      lsq.io.std.storeDataIn(i).bits := impl.io.toBackend.writeback.bits
      impl.io.toBackend.writeback.ready := true.B
    }
  }

  /**
    * --------------------------------------------------------------------
    * Sta Units
    * --------------------------------------------------------------------
    */
  staUnitImps.zipWithIndex.foreach { case (impl: HyuImp, i) =>
    val dtlb = st_dtlb.io.requestor(i)
    impl.io := DontCare // remove useless io
    // from
    impl.io.fromCtrl.redirect <> redirect
    impl.io.fromCtrl.hartId <> io.hartId
    impl.io.fromCtrl.csrCtrl <> csrCtrl
    impl.io.fromCtrl.trigger <> triggerCtrl
    impl.io.fromTlb.resp <> dtlb.resp
    // TODO: FIX StorePipe IO
    // connectSamePort(impl.io.fromDCache, dcache.io.lsu.sta(i))
    dcache.io.lsu.sta(i) := DontCare
    require(!StorePrefetchL1Enabled, "Store prefetch enabled, please connect sta.io.fromDCache (dcache.io.lsu.sta)")
    impl.io.fromBackend.issue.valid <> io.ooo_to_mem.issueSta(i).valid
    impl.io.fromBackend.issue.ready <> io.ooo_to_mem.issueSta(i).ready
    impl.io.fromBackend.issue.bits.fromMemExuInputBundle(io.ooo_to_mem.issueSta(i).bits, isStore = true)
    impl.io.fromPmp.resp <> pmp_check.drop(LduCnt + HyuCnt + 1)(i).resp
    if (i == 0) {
      impl.io.fromMisalign.issue <> storeMisalignBuffer.io.splitStoreReq
    }
    // to
    dtlb.req <> impl.io.toTlb.req
    dtlb.req_kill <> impl.io.toTlb.req_kill
    // TODO: FIX StorePipe IO
    // connectSamePort(dcache.io.lsu.sta(i), impl.io.toDCache)
    impl.io.toDCache.req.ready := true.B
    lsq.io.sta.storeMaskIn(i) <> impl.io.toLsq.maskOut
    lsq.io.sta.storeAddrIn(i) <> impl.io.toLsq.addrUpdate
    lsq.io.sta.storeAddrInRe(i) <> impl.io.toLsq.excpUpdate
    storeMisalignBuffer.io.enq(i).req <> impl.io.toStoreMisalign.enq
    storeMisalignBuffer.io.enq(i).revoke <> impl.io.toStoreMisalign.revoke
    if (i == 0) {
      storeMisalignBuffer.io.splitStoreResp <> impl.io.toStoreMisalign.writeback
    }

    io.mem_to_ooo.stIn(i) <> impl.io.toBackend.stIssued
    io.mem_to_ooo.staIqFeedback(i) <> impl.io.toBackend.iqFeedback
    //  Writeback
    io.mem_to_ooo.writebackSta(i).valid <> impl.io.toBackend.staWriteback.valid
    io.mem_to_ooo.writebackSta(i).ready <> impl.io.toBackend.staWriteback.ready
    io.mem_to_ooo.writebackSta(i).bits <> impl.io.toBackend.staWriteback.bits.toMemExuOutputBundle()
    //
    io.debug_ls.debugLsInfo(i) <> impl.io.debugLsInfo
  }

  /**
    * --------------------------------------------------------------------
    * Load Units
    * --------------------------------------------------------------------
    */
  ldUnitImps.zipWithIndex.foreach { case (impl: HyuImp, i) =>
    val ldIdx = i
    val dtlb = ld_dtlb.io.requestor(ldIdx)

    impl.io := DontCare // remove useless io
    // from
    impl.io.fromCtrl.redirect <> redirect
    impl.io.fromCtrl.hartId <> io.hartId
    impl.io.fromCtrl.csrCtrl <> csrCtrl
    impl.io.fromCtrl.trigger <> triggerCtrl
    impl.io.fromBackend.issue.valid <> io.ooo_to_mem.issueLda(ldIdx).valid
    impl.io.fromBackend.issue.ready <> io.ooo_to_mem.issueLda(ldIdx).ready
    impl.io.fromBackend.issue.bits.fromMemExuInputBundle(io.ooo_to_mem.issueLda(ldIdx).bits)
    if (ldIdx == MisalignWBPort) {
      impl.io.fromMisalign.issue <> loadMisalignBuffer.io.splitLoadReq
      impl.io.fromMisalign.misalignAllowSpec := misalignAllowSpec
    }
    impl.io.fromTlb.resp <> dtlb.resp
    impl.io.fromTlb.hint.id := dtlbRepeater.io.hint.get.req(ldIdx).id
    impl.io.fromTlb.hint.full := dtlbRepeater.io.hint.get.req(ldIdx).full || tlbreplay_reg(ldIdx) || dtlb_ld0_tlbreplay_reg(ldIdx)
    impl.io.fromPmp.resp <> pmp_check(ldIdx).resp
    impl.io.fromLdu.replay <> impl.io.toLdu.replay
    impl.io.fromSta.nukeQuery <> VecInit(staUnitImps.map(_.io.toLdu.nukeQuery) ++ hyUnitImps.map(_.io.toLdu.nukeQuery))
    impl.io.fromLsq.forward <> lsq.io.forward(ldIdx).resp
    impl.io.fromLsq.rarQuery <> lsq.io.ldu.ldld_nuke_query(ldIdx).resp
    impl.io.fromLsq.replay <> lsq.io.replay(ldIdx)
    impl.io.fromLsq.nc <> lsq.io.ncOut(ldIdx)
    impl.io.fromLsq.mmio <> lsq.io.ldout(ldIdx)
    impl.io.fromLsq.lqDeqPtr <> lsq.io.lqDeqPtr
    impl.io.fromSbuffer.forward <> sbuffer.io.forward(ldIdx).resp
    impl.io.fromUbuffer.forward <> uncache.io.forward(ldIdx).resp
    impl.io.fromMissQueue.forward <> dcache.io.lsu.forward_mshr(ldIdx).resp
    impl.io.fromBus <> dcache.io.lsu.forward_D(ldIdx)
    impl.io.fromPrefetch.issue.valid <> l1_pf_req.valid
    impl.io.fromPrefetch.issue.bits.fromL1PrefetchReqBundle(l1_pf_req.bits)
    impl.io.fromMisalign.misalignAllowSpec := misalignAllowSpec

    // dcache read port arb, vSegmentUnit will atomic execute
    connectSameIOPort(impl.io.fromDCache, dcache.io.lsu.load(ldIdx))

    // to
    dtlb.req <> impl.io.toTlb.req
    dtlb.req_kill <> impl.io.toTlb.req_kill
    // Forward
    sbuffer.io.forward(ldIdx).req <> impl.io.toSbuffer.forward
    uncache.io.forward(ldIdx).req <> impl.io.toUbuffer.forward
    dcache.io.lsu.forward_mshr(ldIdx).req <> impl.io.toMissQueue.forward
    lsq.io.forward(ldIdx).req <> impl.io.toLsq.forward
    //
    lsq.io.ldu.ldin(ldIdx) <> impl.io.toLsq.writeback
    lsq.io.ldu.ldld_nuke_query(ldIdx).req <> impl.io.toLsq.rarQuery.req
    lsq.io.ldu.ldld_nuke_query(ldIdx).revoke <> impl.io.toLsq.rarQuery.revoke
    lsq.io.ldu.stld_nuke_query(ldIdx).req <> impl.io.toLsq.rawQuery.req
    lsq.io.ldu.stld_nuke_query(ldIdx).revoke <> impl.io.toLsq.rawQuery.revoke
    // Misalign
    loadMisalignBuffer.io.enq(ldIdx).req <> impl.io.toLoadMisalign.enq
    loadMisalignBuffer.io.enq(ldIdx).revoke <> impl.io.toLoadMisalign.revoke
    loadMisalignBuffer.io.enq(ldIdx).req <> impl.io.toLoadMisalign.enq
    loadMisalignBuffer.io.enq(ldIdx).revoke <> impl.io.toLoadMisalign.revoke
    if (ldIdx == MisalignWBPort) {
      loadMisalignBuffer.io.splitLoadResp <> impl.io.toLoadMisalign.writeback
    }
    // Dcache requests must also be preempted by the segment.
    connectSameIOPort(dcache.io.lsu.load(ldIdx), impl.io.toDCache)
    when (vSegmentFlag) {
      impl.io.toDCache.req.ready := false.B // Dcache is preempted.
    }
    if (ldIdx == 0) {
      // DTlb Query
      val vSegmentDtlbReqValid = RegNext(vSegmentUnit.io.dtlb.req.valid) // segment tlb resquest need to delay 1 cycle
      dtlb_reqs(ldIdx).req.valid := impl.io.toTlb.req.valid || vSegmentDtlbReqValid
      vSegmentUnit.io.dtlb.req.ready := dtlb_reqs(ldIdx).req.ready
      dtlb_reqs(ldIdx).req.bits := ParallelPriorityMux(Seq(
        impl.io.toTlb.req.valid -> impl.io.toTlb.req.bits,
        vSegmentDtlbReqValid -> RegEnable(vSegmentUnit.io.dtlb.req.bits, vSegmentUnit.io.dtlb.req.valid)
      ))
      // DCache query
      connectSameIOPort(dcache.io.lsu.load(ldIdx), vSegmentUnit.io.rdcache)
      dcache.io.lsu.load(ldIdx).req.valid := impl.io.toDCache.req.valid || vSegmentUnit.io.rdcache.req.valid
      dcache.io.lsu.load(ldIdx).req.bits := ParallelPriorityMux(Seq(
        impl.io.toDCache.req.valid -> impl.io.toDCache.req.bits,
        vSegmentUnit.io.rdcache.req.valid -> vSegmentUnit.io.rdcache.req.bits
      ))
    }
    // Wakeup & Cancel
    io.mem_to_ooo.wakeup(ldIdx) <> impl.io.toBackend.wakeup
    io.mem_to_ooo.ldCancel(ldIdx) <> impl.io.toBackend.ldCancel
    // Feedback
    io.mem_to_ooo.ldaIqFeedback(i).feedbackFast := DontCare // load no need feedback
    io.mem_to_ooo.ldaIqFeedback(i).feedbackSlow := DontCare // load no need feedback
    //
    io.mem_to_ooo.s3_delayed_load_error(ldIdx) := false.B
    io.mem_to_ooo.lsTopdownInfo(ldIdx) <> impl.io.lsTopdownInfo
    // I prefetch
    io.ifetchPrefetch(ldIdx) <> impl.io.toPrefetch.ifetch
    io.debug_ls.debugLsInfo(StaCnt + ldIdx) <> impl.io.debugLsInfo
  }

  /**
    * --------------------------------------------------------------------
    * Hybrid Units
    * --------------------------------------------------------------------
    */
  hyUnitImps.zipWithIndex.foreach { case (impl: HyuImp, i) =>
    val hyLduIdx = LduCnt + i
    val hyStaIdx = StaCnt + i
    val dtlb = ld_dtlb.io.requestor(hyLduIdx)

    impl.io := DontCare // remove useless io
    // from
    impl.io.fromCtrl.redirect <> redirect
    impl.io.fromCtrl.hartId <> io.hartId
    impl.io.fromCtrl.csrCtrl <> csrCtrl
    impl.io.fromCtrl.trigger <> triggerCtrl
    impl.io.fromBackend.issue.valid <> io.ooo_to_mem.issueHya(i).valid
    impl.io.fromBackend.issue.ready <> io.ooo_to_mem.issueHya(i).ready
    impl.io.fromBackend.issue.bits.fromMemExuInputBundle(io.ooo_to_mem.issueHya(i).bits)
    impl.io.fromTlb.resp <> dtlb.resp
    impl.io.fromTlb.hint.id := dtlbRepeater.io.hint.get.req(hyLduIdx).id
    impl.io.fromTlb.hint.full := dtlbRepeater.io.hint.get.req(hyLduIdx).full || tlbreplay_reg(hyLduIdx) || dtlb_ld0_tlbreplay_reg(hyLduIdx)
    impl.io.fromLdu.replay <> impl.io.toLdu.replay
    impl.io.fromSta.nukeQuery <> staMods.map(_.io.toLdu.nukeQuery)
    impl.io.fromLsq.forward <> lsq.io.forward(hyLduIdx).resp
    impl.io.fromLsq.rarQuery <> lsq.io.ldu.ldld_nuke_query(hyLduIdx).resp
    impl.io.fromLsq.replay <> lsq.io.replay(hyLduIdx)
    impl.io.fromLsq.nc <> lsq.io.ncOut(hyLduIdx)
    impl.io.fromLsq.mmio <> lsq.io.ldout(hyLduIdx)
    impl.io.fromLsq.lqDeqPtr <> lsq.io.lqDeqPtr
    impl.io.fromSbuffer.forward <> sbuffer.io.forward(hyLduIdx).resp
    impl.io.fromUbuffer.forward <> uncache.io.forward(hyLduIdx).resp
    impl.io.fromMissQueue.forward <> dcache.io.lsu.forward_mshr(hyLduIdx).resp
    impl.io.fromBus <> dcache.io.lsu.forward_D(hyLduIdx)
    impl.io.fromPrefetch.issue.valid <> l1_pf_req.valid
    impl.io.fromPrefetch.issue.bits <> l1_pf_req.bits
    //
    connectSameIOPort(impl.io.fromDCache, dcache.io.lsu.load(hyLduIdx))
    // Misalign
    if (hyLduIdx == MisalignWBPort) {
      loadMisalignBuffer.io.splitLoadReq.ready := false.B
      storeMisalignBuffer.io.splitStoreReq.ready := false.B
      when (loadMisalignBuffer.io.splitLoadReq.valid) {
        impl.io.fromMisalign.issue <> loadMisalignBuffer.io.splitLoadReq
      } .otherwise {
        impl.io.fromMisalign.issue <> storeMisalignBuffer.io.splitStoreReq
      }
      impl.io.fromMisalign.misalignAllowSpec := misalignAllowSpec
    }

    // to
    dtlb.req <> impl.io.toTlb.req
    dtlb.req_kill <> impl.io.toTlb.req_kill
    // Forward
    sbuffer.io.forward(hyLduIdx).req <> impl.io.toSbuffer.forward
    uncache.io.forward(hyLduIdx).req <> impl.io.toUbuffer.forward
    dcache.io.lsu.forward_mshr(hyLduIdx).req <> impl.io.toMissQueue.forward
    lsq.io.forward(hyLduIdx).req <> impl.io.toLsq.forward
    // Lsq
    lsq.io.ldu.ldld_nuke_query(hyLduIdx).req <> impl.io.toLsq.rarQuery.req
    lsq.io.ldu.ldld_nuke_query(hyLduIdx).revoke <> impl.io.toLsq.rarQuery.revoke
    lsq.io.ldu.stld_nuke_query(hyLduIdx).req <> impl.io.toLsq.rawQuery.req
    lsq.io.ldu.stld_nuke_query(hyLduIdx).revoke <> impl.io.toLsq.rawQuery.revoke
    lsq.io.ldu.ldin(hyLduIdx) <> impl.io.toLsq.writeback
    lsq.io.sta.storeMaskIn(hyStaIdx) <> impl.io.toLsq.maskOut
    lsq.io.sta.storeAddrIn(hyStaIdx) <> impl.io.toLsq.addrUpdate
    lsq.io.sta.storeAddrInRe(hyStaIdx) <> impl.io.toLsq.excpUpdate
    // Misalign
    storeMisalignBuffer.io.enq(hyStaIdx).req <> impl.io.toStoreMisalign.enq
    storeMisalignBuffer.io.enq(hyStaIdx).revoke <> impl.io.toStoreMisalign.revoke
    loadMisalignBuffer.io.enq(hyLduIdx).req <> impl.io.toLoadMisalign.enq
    loadMisalignBuffer.io.enq(hyLduIdx).revoke <> impl.io.toLoadMisalign.revoke
    loadMisalignBuffer.io.enq(hyLduIdx) <> impl.io.toLoadMisalign.enq
    if (hyLduIdx == MisalignWBPort) {
      loadMisalignBuffer.io.splitLoadResp <> impl.io.toLoadMisalign.writeback
      storeMisalignBuffer.io.splitStoreResp <> impl.io.toStoreMisalign.writeback
    }
    //
    connectSameIOPort(dcache.io.lsu.load(hyLduIdx), impl.io.toDCache)

    // Wakeup & Cancel
    io.mem_to_ooo.wakeup(hyLduIdx) <> impl.io.toBackend.wakeup
    io.mem_to_ooo.ldCancel(hyLduIdx) <> impl.io.toBackend.ldCancel
    io.mem_to_ooo.hyuIqFeedback(i).feedbackFast := DontCare
    io.mem_to_ooo.hyuIqFeedback(i).feedbackSlow := DontCare
    // Load writeback
    io.mem_to_ooo.writebackHyuLda(i).valid <> impl.io.toBackend.lduWriteback.valid
    io.mem_to_ooo.writebackHyuLda(i).ready <> impl.io.toBackend.lduWriteback.ready
    io.mem_to_ooo.writebackHyuLda(i).bits <> impl.io.toBackend.lduWriteback.bits.toMemExuOutputBundle()
    // Store writeback
    io.mem_to_ooo.writebackHyuSta(i).valid <> impl.io.toBackend.staWriteback.valid
    io.mem_to_ooo.writebackHyuSta(i).ready <> impl.io.toBackend.staWriteback.ready
    io.mem_to_ooo.writebackHyuSta(i).bits <> impl.io.toBackend.staWriteback.bits.toMemExuOutputBundle()
    //
    io.mem_to_ooo.s3_delayed_load_error(hyLduIdx) := false.B
    // I Prefetch
    io.ifetchPrefetch(hyLduIdx) <> impl.io.toPrefetch.ifetch
    //
    io.mem_to_ooo.lsTopdownInfo(hyLduIdx) <> impl.io.lsTopdownInfo
    io.debug_ls.debugLsInfo(StaCnt + hyLduIdx) <> impl.io.debugLsInfo
  }

  /**
    * --------------------------------------------------------------------
    * Atomics Units
    * --------------------------------------------------------------------
    */
  val s_normal +: s_atomics = Enum(StaCnt+ 1)
  val st_dataAtomics = stdUnitImps.map { case std =>
    std.io.toBackend.writeback.valid && FuType.storeIsAMO(std.io.toBackend.writeback.bits.uop.fuType)
  }
  val st_isAtomics = io.ooo_to_mem.issueSta.map { case issue =>
    issue.valid && FuType.storeIsAMO(issue.bits.uop.fuType)
  }
  amoUnitImps.zipWithIndex.foreach { case (impl: AmoImp, i) =>
    val state = RegInit(s_normal)
    val amoTlb = ld_dtlb.io.requestor(i)
    val amoPmp = pmp_check(i)

    impl.io := DontCare // remove useless io
    // from
    impl.io.fromCtrl.redirect <> redirect
    impl.io.fromCtrl.hartId <> io.hartId
    impl.io.fromCtrl.csrCtrl <> csrCtrl
    impl.io.fromCtrl.trigger := triggerCtrl
    // Issue
    impl.io.fromBackend.issue.valid := st_isAtomics.reduce(_|_)
    impl.io.fromBackend.issue.bits.fromMemExuInputBundle(Mux1H(st_isAtomics.zip(io.ooo_to_mem.issueSta).map(x => x._1 -> x._2.bits)))
    impl.io.fromBackend.storeDataIn.zipWithIndex.foreach { case (stdIn, j) =>
      stdIn.valid := st_dataAtomics(j)
      stdIn.bits := stdUnitImps(j).io.toBackend.writeback.bits
    }
    for (j <- 0 until StaCnt) { // FIXME: If memblock has HybridUnit(s), please fix this.
      when (st_isAtomics(j)) {
        io.ooo_to_mem.issueSta(j).ready := impl.io.fromBackend.issue.ready
        staUnitImps(j).io.fromBackend.issue.valid := false.B
        state := s_atomics(j)
      }
    }
    impl.io.fromTlb.resp <> amoTlb.resp
    impl.io.fromDCache.resp <> dcache.io.lsu.atomics.resp
    impl.io.fromDCache.blockLR <> dcache.io.lsu.atomics.block_lr
    impl.io.fromPmp.resp <> amoPmp.resp
    // to
    dcache.io.lsu.atomics.req <> impl.io.toDCache.req
    impl.io.toTlb.req <> amoTlb.req
    impl.io.toTlb.req_kill <> amoTlb.req_kill
    // Feedback
    for (j <- 0 until StaCnt) {
      when (state === s_atomics(j)) {
        io.mem_to_ooo.staIqFeedback(j) <> impl.io.toBackend.iqFeedback
      }
    }
  }

  val atomicsUnit = amoUnitImps.head
  /**
    * --------------------------------------------------------------------
    * Misalign Buffer
    * --------------------------------------------------------------------
    */
  loadMisalignBuffer.io.redirect <> redirect
  loadMisalignBuffer.io.rob.lcommit := io.ooo_to_mem.lsqio.lcommit
  loadMisalignBuffer.io.rob.scommit := io.ooo_to_mem.lsqio.scommit
  loadMisalignBuffer.io.rob.pendingMMIOld := io.ooo_to_mem.lsqio.pendingMMIOld
  loadMisalignBuffer.io.rob.pendingld := io.ooo_to_mem.lsqio.pendingld
  loadMisalignBuffer.io.rob.pendingst := io.ooo_to_mem.lsqio.pendingst
  loadMisalignBuffer.io.rob.pendingVst := io.ooo_to_mem.lsqio.pendingVst
  loadMisalignBuffer.io.rob.commit := io.ooo_to_mem.lsqio.commit
  loadMisalignBuffer.io.rob.pendingPtr := io.ooo_to_mem.lsqio.pendingPtr
  loadMisalignBuffer.io.rob.pendingPtrNext := io.ooo_to_mem.lsqio.pendingPtrNext

  storeMisalignBuffer.io.redirect <> redirect
  storeMisalignBuffer.io.rob.lcommit := io.ooo_to_mem.lsqio.lcommit
  storeMisalignBuffer.io.rob.scommit := io.ooo_to_mem.lsqio.scommit
  storeMisalignBuffer.io.rob.pendingMMIOld := io.ooo_to_mem.lsqio.pendingMMIOld
  storeMisalignBuffer.io.rob.pendingld := io.ooo_to_mem.lsqio.pendingld
  storeMisalignBuffer.io.rob.pendingst := io.ooo_to_mem.lsqio.pendingst
  storeMisalignBuffer.io.rob.pendingVst := io.ooo_to_mem.lsqio.pendingVst
  storeMisalignBuffer.io.rob.commit := io.ooo_to_mem.lsqio.commit
  storeMisalignBuffer.io.rob.pendingPtr := io.ooo_to_mem.lsqio.pendingPtr
  storeMisalignBuffer.io.rob.pendingPtrNext := io.ooo_to_mem.lsqio.pendingPtrNext

  /**
    * --------------------------------------------------------------------
    * Writeback
    * --------------------------------------------------------------------
    */
  val ldaExeWbReqs = Wire(Vec(LduCnt, Decoupled(new MemExuOutput)))

  // atomicsUnit will overwrite the source from ldu if it is about to writeback
  val atomicWritebackOverride = ParallelPriorityMux(
    amoUnitImps.map(mod => mod.io.toBackend.writeback.valid -> mod.io.toBackend.writeback.bits) :+
    (true.B -> ldUnitImps(AtomicWBPort).io.toBackend.lduWriteback.bits)
  )
  ldaExeWbReqs(AtomicWBPort).valid := amoUnitImps.map(_.io.toBackend.writeback.valid).reduce(_|_) ||
    ldUnitImps(AtomicWBPort).io.toBackend.lduWriteback.valid
  ldaExeWbReqs(AtomicWBPort).bits := atomicWritebackOverride.toMemExuOutputBundle()
  amoUnitImps.foreach { case mod: AmoImp =>
    mod.io.toBackend.writeback.ready := ldaExeWbReqs(AtomicWBPort).ready
  }
  ldUnitImps(AtomicWBPort).io.toBackend.lduWriteback.ready := ldaExeWbReqs(AtomicWBPort).ready

  // misalignBuffer will overwrite the source from ldu if it is about to writeback
  val misalignWritebackOverride = Mux(
    ldUnitImps(MisalignWBPort).io.toBackend.lduWriteback.valid,
    ldUnitImps(MisalignWBPort).io.toBackend.lduWriteback.bits.toMemExuOutputBundle(),
    loadMisalignBuffer.io.writeBack.bits
  )
  ldaExeWbReqs(MisalignWBPort).valid := ldUnitImps(MisalignWBPort).io.toBackend.lduWriteback.valid ||
    loadMisalignBuffer.io.writeBack.valid
  ldaExeWbReqs(MisalignWBPort).bits := misalignWritebackOverride
  ldUnitImps(MisalignWBPort).io.toBackend.lduWriteback.ready := ldaExeWbReqs(MisalignWBPort).ready
  loadMisalignBuffer.io.writeBack.ready := ldaExeWbReqs(MisalignWBPort).ready &&
    !ldUnitImps(MisalignWBPort).io.toBackend.lduWriteback.valid
  loadMisalignBuffer.io.loadOutValid := ldUnitImps(MisalignWBPort).io.toBackend.lduWriteback.valid
  loadMisalignBuffer.io.loadVecOutValid := ldUnitImps(MisalignWBPort).io.toVex.lduWriteback.valid

  // loadUnit will overwrite the source from uncache if it is about to writeback
  ldaExeWbReqs(UncacheWBPort).valid <> ldUnitImps(UncacheWBPort).io.toBackend.lduWriteback.valid
  ldaExeWbReqs(UncacheWBPort).ready <> ldUnitImps(UncacheWBPort).io.toBackend.lduWriteback.ready
  ldaExeWbReqs(UncacheWBPort).bits <> ldUnitImps(UncacheWBPort).io.toBackend.lduWriteback.bits.toMemExuOutputBundle()

  //
  io.mem_to_ooo.writebackStd.zip(stdUnitImps).foreach { case (sink, source) =>
    sink.bits  := source.io.toBackend.writeback.bits.toMemExuOutputBundle()
    // AMOs do not need to write back std now.
    sink.valid := source.io.toBackend.writeback.fire && !FuType.storeIsAMO(source.io.toBackend.writeback.bits.uop.fuType)
  }
  io.mem_to_ooo.writebackLda <> ldaExeWbReqs
  val stOut = io.mem_to_ooo.writebackSta ++ io.mem_to_ooo.writebackHyuSta

  io.mem_to_ooo.otherFastWakeup := DontCare

  // miss align buffer will overwrite stOut(0)
  val otherStout = WireInit(0.U.asTypeOf(lsq.io.mmioStout))
  val storeMisalignCanWriteBack = !otherStout.valid &&
    !staUnitImps.head.io.toBackend.staWriteback.valid &&
    !staUnitImps.head.io.toVex.staWriteback.valid
  storeMisalignBuffer.io.writeBack.ready := storeMisalignCanWriteBack
  storeMisalignBuffer.io.storeOutValid := staUnitImps.head.io.toBackend.staWriteback.valid
  storeMisalignBuffer.io.storeVecOutValid := staUnitImps.head.io.toVex.staWriteback.valid
  when (storeMisalignBuffer.io.writeBack.valid && storeMisalignCanWriteBack) {
    stOut.head.valid := true.B
    stOut.head.bits := storeMisalignBuffer.io.writeBack.bits
  }

  /**
    * --------------------------------------------------------------------
    * Prefetcher
    * --------------------------------------------------------------------
    */
  val hartId = p(XSCoreParamsKey).HartId
  smsPrefetcherOpt.foreach { case mod =>
      val enableSMS = Constantin.createRecord(s"enableSMS$hartId", initValue = true)
      // constantinCtrl && master switch csrCtrl && single switch csrCtrl
      mod.io.enable := enableSMS && csrCtrl.pf_ctrl.l1D_pf_enable && csrCtrl.pf_ctrl.l2_pf_recv_enable
      mod.io_agt_en := csrCtrl.pf_ctrl.l1D_pf_enable_agt
      mod.io_pht_en := csrCtrl.pf_ctrl.l1D_pf_enable_pht
      mod.io_act_threshold := csrCtrl.pf_ctrl.l1D_pf_active_threshold
      mod.io_act_stride := csrCtrl.pf_ctrl.l1D_pf_active_stride
      mod.io_stride_en := false.B
      mod.io_dcache_evict <> dcache.io.sms_agt_evict_req
      mod.io.l1_req.ready := false.B
      val mbistSmsPl = MbistPipeline.PlaceMbistPipeline(1, "MbistPipeSms", hasMbist)

      val sources = lduMods.map(_.io.toPrefetch.train)
      val trainPcs = (io.ooo_to_mem.issueLda ++ io.ooo_to_mem.issueHya).map(x => RegNext(x.bits.uop.pc))
      sources.zip(trainPcs).zipWithIndex.foreach { case ((source, trainPc), i) =>
        mod.io.ld_in(i).valid := Mux(
          csrCtrl.pf_ctrl.l1D_pf_train_on_hit,
          source.req.valid,
          source.req.valid && source.req.bits.isFirstIssue && source.req.bits.miss
        )

        //
        mod.io.ld_in(i).bits := source.req.bits
        mod.io.ld_in(i).bits.uop.pc := RegEnable(RegEnable(trainPc, source.s1PrefetchSpec), source.s2PrefetchSpec)
      }
  }

  // load prefetch to l1 Dcache
  l1_pf_req.valid := false.B
  l1_pf_req.bits := DontCare
  l1PrefetcherOpt.foreach { case mod =>
    val enableL1StreamPrefetcher = Constantin.createRecord(s"enableL1StreamPrefetcher$hartId", initValue = true)
    // constantinCtrl && master switch csrCtrl && single switch csrCtrl
    mod.io.enable := enableL1StreamPrefetcher && csrCtrl.pf_ctrl.l1D_pf_enable && csrCtrl.pf_ctrl.l1D_pf_enable_stride
    mod.pf_ctrl <> dcache.io.pf_ctrl
    mod.l2PfqBusy := io.l2PfqBusy

    // stride will train on miss or prefetch hit
    val sources = lduMods.map(_.io.toPrefetch.trainL1)
    val trainPcs = (io.ooo_to_mem.issueLda ++ io.ooo_to_mem.issueHya).map(x => RegNext(x.bits.uop.pc))
    sources.zip(trainPcs).zipWithIndex.foreach { case ((source, trainPc), i) =>
      mod.stride_train(i).valid := source.req.valid && source.req.bits.isFirstIssue && (
        source.req.bits.miss || isFromStride(source.req.bits.metaPrefetch)
      )
      mod.stride_train(i).bits := source.req.bits
      mod.stride_train(i).bits.uop.pc := RegEnable(RegEnable(trainPc, source.s1PrefetchSpec), source.s2PrefetchSpec)

      //
      mod.io.ld_in(i).valid := source.req.valid && source.req.bits.isFirstIssue
      mod.io.ld_in(i).bits := source.req.bits
    }

    l1_pf_req <> Pipeline(in = mod.io.l1_req, depth = 1, pipe = false, name = Some("pf_queue_to_ldu_reg"))
  }

  // load/store prefetch to l2 cache
  smsPrefetcherOpt.foreach { case smsMod =>
    l1PrefetcherOpt.foreach { case l1PfMod =>
      val sms_pf_to_l2 = DelayNWithValid(smsMod.io.l2_req, 2)
      val l1_pf_to_l2 = DelayNWithValid(l1PfMod.io.l2_req, 2)

      outer.l2_pf_sender_opt.get.out.head._1.addr_valid := sms_pf_to_l2.valid || l1_pf_to_l2.valid
      outer.l2_pf_sender_opt.get.out.head._1.addr := Mux(l1_pf_to_l2.valid, l1_pf_to_l2.bits.addr, sms_pf_to_l2.bits.addr)
      outer.l2_pf_sender_opt.get.out.head._1.pf_source := Mux(l1_pf_to_l2.valid, l1_pf_to_l2.bits.source, sms_pf_to_l2.bits.source)
      outer.l2_pf_sender_opt.get.out.head._1.l2_pf_en := csrCtrl.pf_ctrl.l2_pf_enable

      val l2_trace = Wire(new LoadPfDbBundle)
      l2_trace.paddr := outer.l2_pf_sender_opt.get.out.head._1.addr
      val table = ChiselDB.createTable(s"L2PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
      table.log(l2_trace, l1_pf_to_l2.valid, "StreamPrefetchTrace", clock, reset)
      table.log(l2_trace, !l1_pf_to_l2.valid && sms_pf_to_l2.valid, "L2PrefetchTrace", clock, reset)

      val l1_pf_to_l3 = ValidIODelay(l1PfMod.io.l3_req, 4)
      outer.l3_pf_sender_opt.foreach(_.out.head._1.addr_valid := l1_pf_to_l3.valid)
      outer.l3_pf_sender_opt.foreach(_.out.head._1.addr := l1_pf_to_l3.bits)
      outer.l3_pf_sender_opt.foreach(_.out.head._1.l2_pf_en := RegNextN(io.ooo_to_mem.csrCtrl.pf_ctrl.l2_pf_enable, 4, Some(true.B)))

      val l3_trace = Wire(new LoadPfDbBundle)
      l3_trace.paddr := outer.l3_pf_sender_opt.map(_.out.head._1.addr).getOrElse(0.U)
      val l3_table = ChiselDB.createTable(s"L3PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
      l3_table.log(l3_trace, l1_pf_to_l3.valid, "StreamPrefetchTrace", clock, reset)

      XSPerfAccumulate("prefetch_fire_l2", outer.l2_pf_sender_opt.get.out.head._1.addr_valid)
      XSPerfAccumulate("prefetch_fire_l3", outer.l3_pf_sender_opt.map(_.out.head._1.addr_valid).getOrElse(false.B))
      XSPerfAccumulate("l1pf_fire_l2", l1_pf_to_l2.valid)
      XSPerfAccumulate("sms_fire_l2", !l1_pf_to_l2.valid && sms_pf_to_l2.valid)
      XSPerfAccumulate("sms_block_by_l1pf", l1_pf_to_l2.valid && sms_pf_to_l2.valid)
    }
  }

  /**
    * NOTE: loadUnits(0) has higher bank conflict and miss queue arb priority than loadUnits(1) and loadUnits(2)
    * when loadUnits(1)/loadUnits(2) stage 0 is busy, hw prefetch will never use that pipeline
    */
  val LowConfPorts = if (LduCnt == 2) Seq(1) else if (LduCnt == 3) Seq(1, 2) else Seq(0)
  LowConfPorts.map{ case i => ldUnitImps(i).io.fromPrefetch.issue.bits.confidence := 0.U }
  hyUnitImps.foreach { case impl: HyuImp => impl.io.fromPrefetch.issue.bits.confidence := 0.U }

  val canAcceptHighConfPrefetch = lduMods.map(_.io.toPrefetch.trainL1.canAcceptHighConfPrefetch)
  val canAcceptLowConfPrefetch = lduMods.map(_.io.toPrefetch.trainL1.canAcceptLowConfPrefetch)
  l1_pf_req.ready := lduMods.zipWithIndex.map{ case (_, i) =>
    if (LowConfPorts.contains(i)) {
      canAcceptLowConfPrefetch.take(LduCnt)(i)
    } else {
      Mux(l1_pf_req.bits.confidence === 1.U, canAcceptHighConfPrefetch(i), canAcceptLowConfPrefetch(i))
    }
  }.reduce(_ || _)

  /**
    * --------------------------------------------------------------------
    * TLB
    * --------------------------------------------------------------------
    */
  /* tlb vec && constant variable */
  dtlbs.map(_.io.hartId := io.hartId)
  dtlbs.map(_.io.sfence := sfence)
  dtlbs.map(_.io.csr := tlbcsr)
  dtlbs.map(_.io.flushPipe.map(a => a := false.B)) // non-block doesn't need
  dtlbs.map(_.io.redirect := redirect)
  if (refillBothTlb) {
    require(ldtlbParams.outReplace == sttlbParams.outReplace)
    require(ldtlbParams.outReplace == hytlbParams.outReplace)
    require(ldtlbParams.outReplace == pftlbParams.outReplace)
    require(ldtlbParams.outReplace)

    val replace = Module(new TlbReplace(DTlbSize, ldtlbParams))
    replace.io.apply_sep(
      Seq(ld_dtlb.io.replace, st_dtlb.io.replace, pf_dtlb.io.replace),
      ptwio.resp.bits.data.s1.entry.tag
    )
  } else {
    // TODO: there will be bugs in TlbReplace when outReplace enable, since the order of Hyu is not right.
    if (ldtlbParams.outReplace) {
      val replace_ld = Module(new TlbReplace(LduCnt + 1, ldtlbParams))
      replace_ld.io.apply_sep(Seq(ld_dtlb.io.replace), ptwio.resp.bits.data.s1.entry.tag)
    }
    if (hytlbParams.outReplace) {
      val replace_hy = Module(new TlbReplace(HyuCnt, hytlbParams))
      replace_hy.io.apply_sep(Seq(ld_dtlb.io.replace), ptwio.resp.bits.data.s1.entry.tag)
    }
    if (sttlbParams.outReplace) {
      val replace_st = Module(new TlbReplace(StaCnt, sttlbParams))
      replace_st.io.apply_sep(Seq(st_dtlb.io.replace), ptwio.resp.bits.data.s1.entry.tag)
    }
    if (pftlbParams.outReplace) {
      val replace_pf = Module(new TlbReplace(2, pftlbParams))
      replace_pf.io.apply_sep(Seq(pf_dtlb.io.replace), ptwio.resp.bits.data.s1.entry.tag)
    }
  }

  if (backendParams.debugEn){ dontTouch(tlbreplay) }
  for (i <- 0 until LdExuCnt) {
    tlbreplay(i) := ld_dtlb.io.ptw.req(i).valid && ptw_resp_next.vector(0) && ptw_resp_v &&
      ptw_resp_next.data.hit(ld_dtlb.io.ptw.req(i).bits.vpn, tlbcsr.satp.asid, tlbcsr.vsatp.asid, tlbcsr.hgatp.vmid, allType = true, ignoreAsid = true)
  }

  dtlbs.foreach(_.io.ptw.resp.bits := ptw_resp_next.data)
  dtlbs.flatMap(mod => mod.io.ptw.req).zipWithIndex.foreach{ case (req, i) =>
    req.ready := ptwio.req(i).ready
    ptwio.req(i).bits := req.bits
    val vector_hit = if (refillBothTlb) Cat(ptw_resp_next.vector).orR
      else if (i < TlbEndVec(ld_dtlb_idx)) Cat(ptw_resp_next.vector.slice(TlbStartVec(ld_dtlb_idx), TlbEndVec(ld_dtlb_idx))).orR
      else if (i < TlbEndVec(st_dtlb_idx)) Cat(ptw_resp_next.vector.slice(TlbStartVec(st_dtlb_idx), TlbEndVec(st_dtlb_idx))).orR
      else                                 Cat(ptw_resp_next.vector.slice(TlbStartVec(pf_dtlb_idx), TlbEndVec(pf_dtlb_idx))).orR
    ptwio.req(i).valid := req.valid &&
      !(ptw_resp_v && vector_hit && ptw_resp_next.data.hit(req.bits.vpn, tlbcsr.satp.asid, tlbcsr.vsatp.asid, tlbcsr.hgatp.vmid, allType = true, ignoreAsid = true))
  }

  if (refillBothTlb) {
    dtlbs.foreach(_.io.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector).orR)
  } else {
    ld_dtlb.io.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.slice(TlbStartVec(ld_dtlb_idx), TlbEndVec(ld_dtlb_idx))).orR
    st_dtlb.io.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.slice(TlbStartVec(st_dtlb_idx), TlbEndVec(st_dtlb_idx))).orR
    pf_dtlb.io.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.slice(TlbStartVec(pf_dtlb_idx), TlbEndVec(pf_dtlb_idx))).orR
  }
  ld_dtlb.io.ptw.resp.bits.getGpa := Cat(ptw_resp_next.getGpa.take(LduCnt + HyuCnt + 1)).orR
  st_dtlb.io.ptw.resp.bits.getGpa := Cat(ptw_resp_next.getGpa.slice(LduCnt + HyuCnt + 1, LduCnt + HyuCnt + 1 + StaCnt)).orR
  pf_dtlb.io.ptw.resp.bits.getGpa := Cat(ptw_resp_next.getGpa.drop(LduCnt + HyuCnt + 1 + StaCnt)).orR

  dtlbRepeater.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr

  // pmp
  pmp.io.distribute_csr <> csrCtrl.distribute_csr
  for ((p,d) <- pmp_check zip dtlb_pmps) {
    require(p.req.bits.size.getWidth == d.bits.size.getWidth)
    if (HasBitmapCheck) {
      p.apply(tlbcsr.mbmc.CMODE.asBool, tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
    } else {
      p.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
    }
  }

  // Prefetcher
  val StreamDTLBPortIndex = TlbStartVec(ld_dtlb_idx) + LduCnt + HyuCnt
  val PrefetcherDTLBPortIndex = TlbStartVec(pf_dtlb_idx)
  val L2toL1DLBPortIndex = TlbStartVec(pf_dtlb_idx) + 1

  // sms prefetcher
  dtlb_reqs(PrefetcherDTLBPortIndex) := DontCare
  dtlb_reqs(PrefetcherDTLBPortIndex).req.valid := false.B
  dtlb_reqs(PrefetcherDTLBPortIndex).resp.ready := true.B
  smsPrefetcherOpt.foreach { case mod =>
    dtlb_reqs(PrefetcherDTLBPortIndex) <> mod.io.tlb_req
    mod.io.pmp_resp := pmp_check(PrefetcherDTLBPortIndex).resp
  }

  // l1 prefetcher
  dtlb_reqs(StreamDTLBPortIndex) := DontCare
  dtlb_reqs(StreamDTLBPortIndex).req.valid := false.B
  dtlb_reqs(StreamDTLBPortIndex).resp.ready := true.B
  l1PrefetcherOpt.foreach { case mod =>
    dtlb_reqs(StreamDTLBPortIndex) <> mod.io.tlb_req
    mod.io.pmp_resp := pmp_check(StreamDTLBPortIndex).resp
  }

  // L2
  dtlb_reqs(L2toL1DLBPortIndex) <> io.l2_tlb_req
  dtlb_reqs(L2toL1DLBPortIndex).resp.ready := true.B
  io.l2_pmp_resp := pmp_check(L2toL1DLBPortIndex).resp

  /**
    * --------------------------------------------------------------------
    * Lsq
    * --------------------------------------------------------------------
    */
  lsq.io.hartId := io.hartId
  lsq.io.brqRedirect <> redirect
  lsq.io.enq <> io.ooo_to_mem.enqLsq
  lsq.io.maControl <> storeMisalignBuffer.io.sqControl
  lsq.io.loadMisalignFull := loadMisalignBuffer.io.loadMisalignFull
  lsq.io.misalignAllowSpec := misalignAllowSpec
  lsq.io.cmoOpReq <> dcache.io.cmoOpReq
  lsq.io.cmoOpResp <> dcache.io.cmoOpResp
  lsq.io.uncacheOutstanding := io.ooo_to_mem.csrCtrl.uncache_write_outstanding_enable
  lsq.io.rob.lcommit := io.ooo_to_mem.lsqio.lcommit
  lsq.io.rob.scommit := io.ooo_to_mem.lsqio.scommit
  lsq.io.rob.pendingMMIOld := io.ooo_to_mem.lsqio.pendingMMIOld
  lsq.io.rob.pendingld := io.ooo_to_mem.lsqio.pendingld
  lsq.io.rob.pendingst := io.ooo_to_mem.lsqio.pendingst
  lsq.io.rob.pendingVst := io.ooo_to_mem.lsqio.pendingVst
  lsq.io.rob.commit := io.ooo_to_mem.lsqio.commit
  lsq.io.rob.pendingPtr := io.ooo_to_mem.lsqio.pendingPtr
  lsq.io.rob.pendingPtrNext := io.ooo_to_mem.lsqio.pendingPtrNext
  lsq.io.release := dcache.io.lsu.release
  lsq.io.lqCancelCnt <> io.mem_to_ooo.lqCancelCnt
  lsq.io.sqCancelCnt <> io.mem_to_ooo.sqCancelCnt
  lsq.io.lqDeq <> io.mem_to_ooo.lqDeq
  lsq.io.sqDeq <> io.mem_to_ooo.sqDeq
  lsq.io.debugTopDown.robHeadMissInDTlb := dtlbRepeater.io.rob_head_miss_in_tlb
  lsq.io.tl_d_channel <> dcache.io.lsu.tl_d_channel
  lsq.io.sqEmpty <> sbuffer.io.sqempty
  lsq.io.l2_hint <> l2_hint
  lsq.io.tlb_hint <> dtlbRepeater.io.hint.get
  lsq.io.sbufferVecDifftestInfo := DontCare

  /**
    * Store writeback by StoreQueue:
    *   1. cbo Zero
    *   2. mmio
    * Currently, the two should not be present at the same time, so simply make cbo zero a higher priority.
    */
  val sqOtherStout = WireInit(0.U.asTypeOf(DecoupledIO(new MemExuOutput)))
  sqOtherStout.valid := lsq.io.mmioStout.valid || lsq.io.cboZeroStout.valid
  sqOtherStout.bits  := Mux(lsq.io.cboZeroStout.valid, lsq.io.cboZeroStout.bits, lsq.io.mmioStout.bits)
  assert(!(lsq.io.mmioStout.valid && lsq.io.cboZeroStout.valid), "Cannot writeback to mmio and cboZero at the same time.")

  NewPipelineConnect(
    left = sqOtherStout,
    right = otherStout,
    rightOutFire = otherStout.fire,
    isFlush = false.B,
    moduleName = Option("otherStoutConnect")
  )
  otherStout.ready := false.B
  when (otherStout.valid && !staUnitImps.head.io.toBackend.staWriteback.valid) {
    stOut.head.valid := true.B
    stOut.head.bits := otherStout.bits
    otherStout.ready := true.B
  }
  lsq.io.mmioStout.ready := sqOtherStout.ready
  lsq.io.cboZeroStout.ready := sqOtherStout.ready
  lsq.io.vecmmioStout.ready := false.B
  lsq.io.flushSbuffer.empty := sbuffer.io.sbempty
  lsq.io.exceptionAddr.isStore := io.ooo_to_mem.isStoreException
  lsq.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  lsq.io.noUopsIssued := io.topDownInfo.toBackend.noUopsIssued
  io.mem_to_ooo.lsqio.mmio := lsq.io.rob.mmio
  io.mem_to_ooo.lsqio.uop := lsq.io.rob.uop
  io.mem_to_ooo.lsqio.lqCanAccept := lsq.io.lqCanAccept
  io.mem_to_ooo.lsqio.sqCanAccept := lsq.io.sqCanAccept
  io.mem_to_ooo.sqDeqPtr := lsq.io.sqDeqPtr
  io.mem_to_ooo.lqDeqPtr := lsq.io.lqDeqPtr
  io.mem_to_ooo.stIssuePtr := lsq.io.issuePtrExt

  /**
    * --------------------------------------------------------------------
    * Uncache
    * --------------------------------------------------------------------
    */
  uncache.io.enableOutstanding := csrCtrl.uncache_write_outstanding_enable
  uncache.io.hartId := io.hartId

  // lsq.io.uncache        <> uncache.io.lsq
  val s_idle :: s_scalar_uncache :: s_vector_uncache :: Nil = Enum(3)
  val uncacheState = RegInit(s_idle)
  val uncacheReq = Wire(Decoupled(new UncacheWordReq))
  val uncacheIdResp = uncache.io.lsq.idResp
  val uncacheResp = Wire(Decoupled(new UncacheWordResp))

  uncacheReq.bits := DontCare
  uncacheReq.valid := false.B
  uncacheReq.ready := false.B
  uncacheResp.bits := DontCare
  uncacheResp.valid := false.B
  uncacheResp.ready := false.B
  lsq.io.uncache.req.ready := false.B
  lsq.io.uncache.idResp.valid := false.B
  lsq.io.uncache.idResp.bits := DontCare
  lsq.io.uncache.resp.valid := false.B
  lsq.io.uncache.resp.bits := DontCare

  switch (uncacheState) {
    is (s_idle) {
      when (uncacheReq.fire) {
        when (lsq.io.uncache.req.valid) {
          when (!lsq.io.uncache.req.bits.nc || !csrCtrl.uncache_write_outstanding_enable) {
            uncacheState := s_scalar_uncache
          }
        }.otherwise {
          // val isStore = vsFlowQueue.io.uncache.req.bits.cmd === MemoryOpConstants.M_XWR
          when (!csrCtrl.uncache_write_outstanding_enable) {
            uncacheState := s_vector_uncache
          }
        }
      }
    }

    is (s_scalar_uncache) {
      when (uncacheResp.fire) {
        uncacheState := s_idle
      }
    }

    is (s_vector_uncache) {
      when (uncacheResp.fire) {
        uncacheState := s_idle
      }
    }
  }

  when (lsq.io.uncache.req.valid) {
    uncacheReq <> lsq.io.uncache.req
  }
  when (csrCtrl.uncache_write_outstanding_enable) {
    lsq.io.uncache.resp <> uncacheResp
    lsq.io.uncache.idResp <> uncacheIdResp
  }.otherwise {
    when (uncacheState === s_scalar_uncache) {
      lsq.io.uncache.resp <> uncacheResp
      lsq.io.uncache.idResp <> uncacheIdResp
    }
  }
  // delay dcache refill for 1 cycle for better timing
  AddPipelineReg(uncacheReq, uncache.io.lsq.req, false.B)
  AddPipelineReg(uncache.io.lsq.resp, uncacheResp, false.B)

  /**
    * --------------------------------------------------------------------
    * Sbuffer
    * --------------------------------------------------------------------
    */
  sbuffer.io.hartId := io.hartId
  sbuffer.io.csrCtrl <> csrCtrl
  sbuffer.io.in <> lsq.io.sbuffer
  sbuffer.io.in(0).valid := lsq.io.sbuffer(0).valid || vSegmentUnit.io.sbuffer.valid
  sbuffer.io.in(0).bits  := ParallelPriorityMux(Seq(
    vSegmentUnit.io.sbuffer.valid -> vSegmentUnit.io.sbuffer.bits,
    lsq.io.sbuffer(0).valid -> lsq.io.sbuffer(0).bits
  ))
  vSegmentUnit.io.sbuffer.ready := sbuffer.io.in(0).ready
  sbuffer.io.dcache <> dcache.io.lsu.store
  sbuffer.io.memSetPattenDetected := dcache.io.memSetPattenDetected
  sbuffer.io.force_write <> lsq.io.force_write
  sbuffer.io.store_prefetch := DontCare // FIXME: please connect store prefetch later
  require(!StorePrefetchL1Enabled, "Store prefetch enabled, please connect sbuffer.io.store_prefetch!")

  // flush sbuffer
  val cmoFlush = lsq.io.flushSbuffer.valid
  val fenceFlush = io.ooo_to_mem.flushSb
  val atomicsFlush = amoUnitImps.map(_.io.flushSbuffer.valid).reduce(_|_) || vSegmentUnit.io.flush_sbuffer.valid
  val stIsEmpty = sbuffer.io.flush.empty && uncache.io.flush.empty
  io.mem_to_ooo.sbIsEmpty := RegNext(stIsEmpty)

  // if both of them tries to flush sbuffer at the same time something must have gone wrong
  assert(!(fenceFlush && atomicsFlush && cmoFlush))
  sbuffer.io.flush.valid := RegNext(fenceFlush || atomicsFlush || cmoFlush)
  uncache.io.flush.valid := sbuffer.io.flush.valid

  // Initialize when unenabled difftest.
  sbuffer.io.vecDifftestInfo      := DontCare

  /**
  * --------------------------------------------------------------------
  * Vector Execution Block
  * --------------------------------------------------------------------
  */
  // The segment instruction is executed atomically.
  // After the segment instruction directive starts executing, no other instructions should be executed.
  when(GatedValidRegNext(vSegmentUnit.io.in.fire)) {
    vSegmentFlag := true.B
  }.elsewhen(GatedValidRegNext(vSegmentUnit.io.uopwriteback.valid)) {
    vSegmentFlag := false.B
  }

  vfofBuffer.io.redirect <> redirect

  // vector
  val vLoadCanAccept  = (0 until VlduCnt).map(i =>
    vlSplit(i).io.in.ready && VlduType.isVecLd(io.ooo_to_mem.issueVldu(i).bits.uop.fuOpType)
  )
  val vStoreCanAccept = (0 until VstuCnt).map(i =>
    vsSplit(i).io.in.ready && VstuType.isVecSt(io.ooo_to_mem.issueVldu(i).bits.uop.fuOpType)
  )
  (0 until VlduCnt).foreach {  i =>
    vfofBuffer.io.in(i).valid := io.ooo_to_mem.issueVldu(i).valid
    vfofBuffer.io.in(i).bits  := io.ooo_to_mem.issueVldu(i).bits
    io.ooo_to_mem.issueVldu(i).ready := vLoadCanAccept(i) || vStoreCanAccept(i)
  }

  val isSegment     = io.ooo_to_mem.issueVldu.head.valid && isVsegls(io.ooo_to_mem.issueVldu.head.bits.uop.fuType)
  val isFixVlUop    = io.ooo_to_mem.issueVldu.map{x =>
    x.bits.uop.vpu.isVleff && x.bits.uop.vpu.lastUop && x.valid
  }

  /**
   * TODO: splited vsMergebuffer maybe remove, if one RS can accept two feedback, or don't need RS replay uop
   * for now:
   *  RS0 -> VsSplit0 -> stu0 -> vsMergebuffer0 -> feedback -> RS0
   *  RS1 -> VsSplit1 -> stu1 -> vsMergebuffer1 -> feedback -> RS1
   *
   * vector load don't need feedback
   *
   *  RS0 -> VlSplit0  -> ldu0 -> |
   *  RS1 -> VlSplit1  -> ldu1 -> |  -> vlMergebuffer
   *        replayIO   -> ldu3 -> |
   * */
  // Vector Store
  vsMergeBuffer.zipWithIndex.foreach { case (mod, i) =>
    mod.io.redirect <> redirect
    mod.io.fromPipeline := DontCare
    mod.io.fromSplit := DontCare
    mod.io.fromMisalignBuffer.get.flush := storeMisalignBuffer.io.toVecStoreMergeBuffer(i).flush
    mod.io.fromMisalignBuffer.get.mbIndex := storeMisalignBuffer.io.toVecStoreMergeBuffer(i).mbIndex

    // send to Lsq
    mod.io.toLsq.head <> lsq.io.stvecFeedback(i)

    // send to RS
    io.mem_to_ooo.vstuIqFeedback(i).feedbackFast := DontCare
    if (i == 0){
      io.mem_to_ooo.vstuIqFeedback(i).feedbackSlow.valid := mod.io.feedback.head.valid || vSegmentUnit.io.feedback.valid
      io.mem_to_ooo.vstuIqFeedback(i).feedbackSlow.bits := ParallelPriorityMux(Seq(
        vSegmentUnit.io.feedback.valid -> vSegmentUnit.io.feedback.bits,
        mod.io.feedback.head.valid -> mod.io.feedback.head.bits
      ))
    } else {
      mod.io.feedback.head <> io.mem_to_ooo.vstuIqFeedback(i).feedbackSlow
    }
  }

  vsSplit.zipWithIndex.foreach { case (mod, i) =>
    mod.io.redirect <> redirect
    mod.io.in <> io.ooo_to_mem.issueVldu(i)
    mod.io.in.valid := io.ooo_to_mem.issueVldu(i).valid && vStoreCanAccept(i) && !isSegment
    mod.io.toMergeBuffer <> vsMergeBuffer(i).io.fromSplit.head
    mod.io.vstd.get := DontCare // Todo: Discuss how to pass vector store data
    mod.io.vstdMisalign.get.storeMisalignBufferEmpty := !storeMisalignBuffer.io.full
    mod.io.vstdMisalign.get.storePipeEmpty := !staMods(i).io.s0_s1_valid

    val issue = Wire(DecoupledIO(new LsPipelineBundle))
    issue.valid <> mod.io.out.valid
    issue.ready <> mod.io.out.ready
    issue.bits.fromVecPipeBundle(mod.io.out.bits, isVStore = true)
    NewPipelineConnect(
      left = issue,
      right = staMods(i).io.fromVex.issue,
      rightOutFire = staMods(i).io.fromVex.issue.fire,
      isFlush = Mux(
        mod.io.out.fire,
        mod.io.out.bits.uop.robIdx.needFlush(io.redirect),
        staMods(i).io.fromVex.issue.bits.uop.robIdx.needFlush(io.redirect)
      ),
      moduleName = Option("VsSplitConnectStu")
    )
  }

  staMods.zipWithIndex.foreach { case (mod: HyuImp, i) =>
    if(i < VstuCnt){
      mod.io.toVex.staWriteback.ready := true.B
      storeMisalignBuffer.io.vecWriteBack(i).ready := vsMergeBuffer(i).io.fromPipeline.head.ready

      when(mod.io.toVex.staWriteback.valid) {
        vsMergeBuffer(i).io.fromPipeline.head.valid := mod.io.toVex.staWriteback.valid
        vsMergeBuffer(i).io.fromPipeline.head.bits := mod.io.toVex.staWriteback.bits
      } .otherwise {
        vsMergeBuffer(i).io.fromPipeline.head.valid := storeMisalignBuffer.io.vecWriteBack(i).valid
        vsMergeBuffer(i).io.fromPipeline.head.bits := storeMisalignBuffer.io.vecWriteBack(i).bits
      }
    }
  }

  // Vector Load
  vlSplit.zipWithIndex.foreach { case (mod, i) =>
    mod.io.redirect <> redirect
    mod.io.in <> io.ooo_to_mem.issueVldu(i)
    mod.io.in.valid := io.ooo_to_mem.issueVldu(i).valid && vLoadCanAccept(i) && !isSegment && !isFixVlUop(i)
    mod.io.toMergeBuffer <> vlMergeBuffer.io.fromSplit(i)
    mod.io.threshold.get.valid := vlMergeBuffer.io.toSplit.get.threshold
    mod.io.threshold.get.bits := lsq.io.lqDeqPtr

    val issue = Wire(DecoupledIO(new LsPipelineBundle))
    issue.valid <> mod.io.out.valid
    issue.ready <> mod.io.out.ready
    issue.bits.fromVecPipeBundle(mod.io.out.bits, isVStore = true)
    NewPipelineConnect(
      left = issue,
      right = lduMods(i).io.fromVex.issue,
      rightOutFire = lduMods(i).io.fromVex.issue.fire,
      isFlush = Mux(
        mod.io.out.fire,
        mod.io.out.bits.uop.robIdx.needFlush(io.redirect),
        lduMods(i).io.fromVex.issue.bits.uop.robIdx.needFlush(io.redirect)
      ),
      moduleName = Option("VlSplitConnectLdu")
    )
  }

  lduMods.zipWithIndex.foreach { case (mod: HyuImp, i) =>
    mod.io.toVex.lduWriteback.ready := vlMergeBuffer.io.fromPipeline(i).ready
    loadMisalignBuffer.io.vecWriteBack.ready := true.B

    if (i == MisalignWBPort) {
      when(mod.io.toVex.lduWriteback.valid) {
        vlMergeBuffer.io.fromPipeline(i).valid := mod.io.toVex.lduWriteback.valid
        vlMergeBuffer.io.fromPipeline(i).bits  := mod.io.toVex.lduWriteback.bits
      } .otherwise {
        vlMergeBuffer.io.fromPipeline(i).valid   := loadMisalignBuffer.io.vecWriteBack.valid
        vlMergeBuffer.io.fromPipeline(i).bits    := loadMisalignBuffer.io.vecWriteBack.bits
      }
    } else {
      vlMergeBuffer.io.fromPipeline(i).valid := mod.io.toVex.lduWriteback.valid
      vlMergeBuffer.io.fromPipeline(i).bits  := mod.io.toVex.lduWriteback.bits
    }
  }

  (0 until VlduCnt).foreach{i=>
    vlMergeBuffer.io.redirect <> redirect
    // send to Lsq
    vlMergeBuffer.io.toLsq(i) <> lsq.io.ldvecFeedback(i)
    // send to RS
    io.mem_to_ooo.vlduIqFeedback(i).feedbackFast := DontCare
    vlMergeBuffer.io.feedback(i) <> io.mem_to_ooo.vlduIqFeedback(i).feedbackSlow
  }

  //
  val vlMergeBufferWriteback = vlMergeBuffer.io.uopWriteback
  val vsMergeBufferWriteback = vsMergeBuffer.map(_.io.uopWriteback.head)
  val vfofBufferWriteback    = vfofBuffer.io.mergeUopWriteback
  io.mem_to_ooo.writebackVldu.zip(vlMergeBufferWriteback.zip(vsMergeBufferWriteback).zip(vfofBufferWriteback))
    .zipWithIndex.foreach { case ((sink, ((vlMod, vsMod), vfofMod)), port) =>
      vfofMod.valid := vlMod.valid
      vfofMod.bits := vlMod.bits

      // Arbitor
      port match {
        case 0 =>
          sink.valid := vlMod.valid || vsMod.valid || vSegmentUnit.io.uopwriteback.valid
          sink.bits := ParallelPriorityMux(Seq(
            vSegmentUnit.io.uopwriteback.valid  -> vSegmentUnit.io.uopwriteback.bits,
            vlMod.valid -> vlMod.bits,
            vsMod.valid -> vsMod.bits,
          ))
          vSegmentUnit.io.uopwriteback.ready := sink.ready
          vlMod.ready := sink.ready && !vSegmentUnit.io.uopwriteback.valid
          vsMod.ready := sink.ready && !vSegmentUnit.io.uopwriteback.valid && !vlMod.valid
        case 1 =>
          sink.valid := vlMod.valid || vsMod.valid || vfofBuffer.io.uopWriteback.valid
          sink.bits := ParallelPriorityMux(Seq(
            vfofBuffer.io.uopWriteback.valid  -> vfofBuffer.io.uopWriteback.bits,
            vlMod.valid -> vlMod.bits,
            vsMod.valid -> vsMod.bits,
          ))
          vfofBuffer.io.uopWriteback.ready := sink.ready
          vlMod.ready := sink.ready && !vfofBuffer.io.uopWriteback.valid
          vsMod.ready := sink.ready && !vfofBuffer.io.uopWriteback.valid && !vlMod.valid
        case _ =>
          sink.valid := vlMod.valid || vsMod.valid
          sink.bits := ParallelPriorityMux(Seq(
            vlMod.valid -> vlMod.bits,
            vsMod.valid -> vsMod.bits,
          ))
          vlMod.ready := sink.ready
          vsMod.ready := sink.ready && !vlMod.valid
      }
  }

  // Vector segmentUnit
  vSegmentUnit.io.redirect <> redirect
  vSegmentUnit.io.fromCsrTrigger := triggerCtrl
  vSegmentUnit.io.in.valid := isSegment && io.ooo_to_mem.issueVldu.head.valid// is segment instruction
  vSegmentUnit.io.in.bits <> io.ooo_to_mem.issueVldu.head.bits
  vSegmentUnit.io.dtlb.resp.valid <> dtlb_reqs.take(LduCnt).head.resp.valid
  vSegmentUnit.io.dtlb.resp.bits <> dtlb_reqs.take(LduCnt).head.resp.bits
  vSegmentUnit.io.pmpResp <> pmp_check.head.resp
  vSegmentUnit.io.rdcache.resp.valid := dcache.io.lsu.load(0).resp.valid
  vSegmentUnit.io.rdcache.resp.bits := dcache.io.lsu.load(0).resp.bits
  vSegmentUnit.io.rdcache.s2_bank_conflict := dcache.io.lsu.load(0).s2_bank_conflict
  vSegmentUnit.io.flush_sbuffer.empty := stIsEmpty
  vSegmentUnit.io.vecDifftestInfo := DontCare

  /**
    * --------------------------------------------------------------------
    * Rollback
    * --------------------------------------------------------------------
    */
  val allRedirect = lduMods.map(_.io.toBackend.rollback)  ++ lsq.io.nack_rollback ++ lsq.io.nuke_rollback
  val oldestOneHot = Redirect.selectOldestRedirect(allRedirect)
  val oldestRedirect = WireDefault(Mux1H(oldestOneHot, allRedirect))
  // memory replay would not cause IAF/IPF/IGPF
  oldestRedirect.bits.cfiUpdate.backendIAF := false.B
  oldestRedirect.bits.cfiUpdate.backendIPF := false.B
  oldestRedirect.bits.cfiUpdate.backendIGPF := false.B
  io.mem_to_ooo.memoryViolation := oldestRedirect

  /**
    * --------------------------------------------------------------------
    * Exception Info Generation
    * --------------------------------------------------------------------
    */
  // Exception address is used several cycles after flush.
  // We delay it by 10 cycles to ensure its flush safety.
  val atomicsException = RegInit(false.B)
  when (DelayN(redirect.valid, 10) && atomicsException) {
    atomicsException := false.B
  }.elsewhen (atomicsUnit.io.exceptionInfo.valid) {
    atomicsException := true.B
  }

  val misalignBufExceptionOverwrite = loadMisalignBuffer.io.overwriteExpBuf.valid || storeMisalignBuffer.io.overwriteExpBuf.valid
  val misalignBufExceptionVaddr = Mux(loadMisalignBuffer.io.overwriteExpBuf.valid,
    loadMisalignBuffer.io.overwriteExpBuf.vaddr,
    storeMisalignBuffer.io.overwriteExpBuf.vaddr
  )
  val misalignBufExceptionIsHyper = Mux(loadMisalignBuffer.io.overwriteExpBuf.valid,
    loadMisalignBuffer.io.overwriteExpBuf.isHyper,
    storeMisalignBuffer.io.overwriteExpBuf.isHyper
  )
  val misalignBufExceptionGpaddr = Mux(loadMisalignBuffer.io.overwriteExpBuf.valid,
    loadMisalignBuffer.io.overwriteExpBuf.gpaddr,
    storeMisalignBuffer.io.overwriteExpBuf.gpaddr
  )
  val misalignBufExceptionIsForVSnonLeafPTE = Mux(loadMisalignBuffer.io.overwriteExpBuf.valid,
    loadMisalignBuffer.io.overwriteExpBuf.isForVSnonLeafPTE,
    storeMisalignBuffer.io.overwriteExpBuf.isForVSnonLeafPTE
  )

  val vSegmentException = RegInit(false.B)
  when (DelayN(redirect.valid, 10) && vSegmentException) {
    vSegmentException := false.B
  }.elsewhen (vSegmentUnit.io.exceptionInfo.valid) {
    vSegmentException := true.B
  }
  val atomicsExceptionAddress = RegEnable(atomicsUnit.io.exceptionInfo.bits.vaddr, atomicsUnit.io.exceptionInfo.valid)
  val vSegmentExceptionVstart = RegEnable(vSegmentUnit.io.exceptionInfo.bits.vstart, vSegmentUnit.io.exceptionInfo.valid)
  val vSegmentExceptionVl     = RegEnable(vSegmentUnit.io.exceptionInfo.bits.vl, vSegmentUnit.io.exceptionInfo.valid)
  val vSegmentExceptionAddress = RegEnable(vSegmentUnit.io.exceptionInfo.bits.vaddr, vSegmentUnit.io.exceptionInfo.valid)
  val atomicsExceptionGPAddress = RegEnable(atomicsUnit.io.exceptionInfo.bits.gpaddr, atomicsUnit.io.exceptionInfo.valid)
  val vSegmentExceptionGPAddress = RegEnable(vSegmentUnit.io.exceptionInfo.bits.gpaddr, vSegmentUnit.io.exceptionInfo.valid)
  val atomicsExceptionIsForVSnonLeafPTE = RegEnable(atomicsUnit.io.exceptionInfo.bits.isForVSnonLeafPTE, atomicsUnit.io.exceptionInfo.valid)
  val vSegmentExceptionIsForVSnonLeafPTE = RegEnable(vSegmentUnit.io.exceptionInfo.bits.isForVSnonLeafPTE, vSegmentUnit.io.exceptionInfo.valid)

  val exceptionVaddr = ParallelPriorityMux(Seq(
    atomicsException -> atomicsExceptionAddress,
    misalignBufExceptionOverwrite -> misalignBufExceptionVaddr,
    vSegmentException -> vSegmentExceptionAddress,
    true.B -> lsq.io.exceptionAddr.vaddr
  ))

  // whether vaddr need ext or is hyper inst:
  // VaNeedExt: atomicsException -> false; misalignBufExceptionOverwrite -> true; vSegmentException -> false
  // IsHyper: atomicsException -> false; vSegmentException -> false
  val exceptionVaNeedExt = !atomicsException &&
    (misalignBufExceptionOverwrite || (!vSegmentException && lsq.io.exceptionAddr.vaNeedExt))
  val exceptionIsHyper = !atomicsException &&
    (misalignBufExceptionOverwrite && misalignBufExceptionIsHyper ||
      (!vSegmentException && lsq.io.exceptionAddr.isHyper && !misalignBufExceptionOverwrite))

  def GenExceptionVa(
    mode: UInt, isVirt: Bool, vaNeedExt: Bool, satp: TlbSatpBundle, vsatp: TlbSatpBundle,
    hgatp: TlbHgatpBundle, vaddr: UInt
  ) = {
    require(VAddrBits >= 50)

    val satpNone = satp.mode === 0.U
    val satpSv39 = satp.mode === 8.U
    val satpSv48 = satp.mode === 9.U

    val vsatpNone = vsatp.mode === 0.U
    val vsatpSv39 = vsatp.mode === 8.U
    val vsatpSv48 = vsatp.mode === 9.U

    val hgatpNone = hgatp.mode === 0.U
    val hgatpSv39x4 = hgatp.mode === 8.U
    val hgatpSv48x4 = hgatp.mode === 9.U

    // For !isVirt, mode check is necessary, as we don't want virtual memory in M-mode.
    // For isVirt, mode check is unnecessary, as virt won't be 1 in M-mode.
    // Also, isVirt includes Hyper Insts, which don't care mode either.
    val useBareAddr =
      (isVirt && vsatpNone && hgatpNone) ||
      (!isVirt && (mode === CSRConst.ModeM)) ||
      (!isVirt && (mode =/= CSRConst.ModeM) && satpNone)
    val useSv39Addr =
      (isVirt && vsatpSv39) ||
      (!isVirt && (mode =/= CSRConst.ModeM) && satpSv39)
    val useSv48Addr =
      (isVirt && vsatpSv48) ||
      (!isVirt && (mode =/= CSRConst.ModeM) && satpSv48)
    val useSv39x4Addr = isVirt && vsatpNone && hgatpSv39x4
    val useSv48x4Addr = isVirt && vsatpNone && hgatpSv48x4

    val bareAddr   = ZeroExt(vaddr(PAddrBits - 1, 0), XLEN)
    val sv39Addr   = SignExt(vaddr.take(39), XLEN)
    val sv39x4Addr = ZeroExt(vaddr.take(39 + 2), XLEN)
    val sv48Addr   = SignExt(vaddr.take(48), XLEN)
    val sv48x4Addr = ZeroExt(vaddr.take(48 + 2), XLEN)

    val exceptionVAddr = Wire(UInt(XLEN.W))
    when (vaNeedExt) {
      exceptionVAddr := Mux1H(Seq(
        (useBareAddr)   -> bareAddr,
        (useSv39Addr)   -> sv39Addr,
        (useSv48Addr)   -> sv48Addr,
        (useSv39x4Addr) -> sv39x4Addr,
        (useSv48x4Addr) -> sv48x4Addr,
      ))
    } .otherwise {
      exceptionVAddr := vaddr
    }
    exceptionVAddr
  }

  io.mem_to_ooo.lsqio.vaddr := RegNext(
    GenExceptionVa(tlbcsr.priv.dmode, tlbcsr.priv.virt || exceptionIsHyper, exceptionVaNeedExt,
    tlbcsr.satp, tlbcsr.vsatp, tlbcsr.hgatp, exceptionVaddr)
  )

  // vsegment instruction is executed atomic, which mean atomicsException and vSegmentException should not raise at the same time.
  XSError(atomicsException && vSegmentException, "atomicsException and vSegmentException raise at the same time!")
  io.mem_to_ooo.lsqio.vstart := RegNext(
    Mux(
      vSegmentException,
      vSegmentExceptionVstart,
      lsq.io.exceptionAddr.vstart
    )
  )
  io.mem_to_ooo.lsqio.vl := RegNext(
    Mux(
      vSegmentException,
      vSegmentExceptionVl,
      lsq.io.exceptionAddr.vl
    )
  )
  XSError(atomicsException && atomicsUnit.io.fromBackend.issue.valid, "new instruction before exception triggers\n")

  io.mem_to_ooo.lsqio.gpaddr := RegNext(ParallelPriorityMux(Seq(
    atomicsException -> atomicsExceptionGPAddress,
    misalignBufExceptionOverwrite -> misalignBufExceptionGpaddr,
    vSegmentException -> vSegmentExceptionGPAddress,
    true.B -> lsq.io.exceptionAddr.gpaddr
  )))

  io.mem_to_ooo.lsqio.isForVSnonLeafPTE := RegNext(ParallelPriorityMux(Seq(
    atomicsException -> atomicsExceptionIsForVSnonLeafPTE,
    misalignBufExceptionOverwrite -> misalignBufExceptionIsForVSnonLeafPTE,
    vSegmentException -> vSegmentExceptionIsForVSnonLeafPTE,
    true.B -> lsq.io.exceptionAddr.isForVSnonLeafPTE
  )))

  /**
    * --------------------------------------------------------------------
    * DFT (Mbist & Sram control)
    * --------------------------------------------------------------------
    */
  val mbistPl = MbistPipeline.PlaceMbistPipeline(Int.MaxValue, "MbistPipeMemBlk", hasMbist)
  val mbistIntf = Option.when(hasMbist)({
    val params = mbistPl.get.nodeParams
    val intf = Some(Module(new MbistInterface(
      params = Seq(params),
      ids = Seq(mbistPl.get.childrenIds),
      name = s"MbistIntfMemBlk",
      pipelineNum = 1
    )))
    intf.get.toPipeline.head <> mbistPl.get.mbist
    mbistPl.get.registerCSV(intf.get.info, "MbistMemBlk")
    intf.get.mbist := DontCare
    dontTouch(intf.get.mbist)
    //TODO: add mbist controller connections here
    intf
  })
  val sigFromSrams = Option.when(hasDFT)(SramHelper.genBroadCastBundleTop())
  val cg = ClockGate.genTeSrc
  dontTouch(cg)

  if (hasMbist) {
    cg.cgen := io.dft.get.cgen
  } else {
    cg.cgen := false.B
  }

  // sram debug
  sigFromSrams.foreach({ case mod => mod := DontCare })
  sigFromSrams.zip(io.dft).foreach { case (mod, dft) =>
      if (hasMbist) {
        mod.ram_hold := dft.ram_hold
        mod.ram_bypass := dft.ram_bypass
        mod.ram_bp_clken := dft.ram_bp_clken
        mod.ram_aux_clk := dft.ram_aux_clk
        mod.ram_aux_ckbp := dft.ram_aux_ckbp
        mod.ram_mcp_hold := dft.ram_mcp_hold
        mod.cgen := dft.cgen
      }
      if (hasSramCtl) {
        mod.ram_ctl := RegNext(dft.ram_ctl)
      }
  }
  io.dft_frnt.zip(sigFromSrams).foreach({ case (a, b) => a := b })
  io.dft_reset_frnt.zip(io.dft_reset).foreach({ case (a, b) => a := b })
  io.dft_bcknd.zip(sigFromSrams).foreach({ case (a, b) => a := b })
  io.dft_reset_bcknd.zip(io.dft_reset).foreach({ case (a, b) => a := b })

  /**
    * --------------------------------------------------------------------
    * Bypass group
    * --------------------------------------------------------------------
    */
  io.mem_to_ooo.topToBackendBypass match { case sink =>
    sink.hartId := io.hartId
    sink.l2FlushDone := RegNext(io.l2_flush_done)
    sink.externalInterrupt.msip := outer.clint_int_sink.in.head._1(0)
    sink.externalInterrupt.mtip := outer.clint_int_sink.in.head._1(1)
    sink.externalInterrupt.meip := outer.plic_int_sink.in.head._1(0)
    sink.externalInterrupt.seip := outer.plic_int_sink.in.last._1(0)
    sink.externalInterrupt.debug := outer.debug_int_sink.in.head._1(0)
    sink.externalInterrupt.nmi.nmi_31 := outer.nmi_int_sink.in.head._1(0) | outer.beu_local_int_sink.in.head._1(0)
    sink.externalInterrupt.nmi.nmi_43 := outer.nmi_int_sink.in.head._1(1)
    sink.msiInfo := DelayNWithValid(io.fromTopToBackend.msiInfo, 1)
    sink.clintTime := DelayNWithValid(io.fromTopToBackend.clintTime, 1)
  }

  io.memInfo.sqFull := RegNext(lsq.io.sqFull)
  io.memInfo.lqFull := RegNext(lsq.io.lqFull)
  io.memInfo.dcacheMSHRFull := RegNext(dcache.io.mshrFull)

  io.inner_hartId := io.hartId
  io.inner_reset_vector := RegNext(io.outer_reset_vector)
  io.outer_cpu_halt := io.ooo_to_mem.backendToTopBypass.cpuHalted
  io.outer_l2_flush_en := io.ooo_to_mem.csrCtrl.flush_l2_enable
  io.outer_power_down_en := io.ooo_to_mem.csrCtrl.power_down_enable
  io.outer_cpu_critical_error := io.ooo_to_mem.backendToTopBypass.cpuCriticalError
  io.outer_msi_ack := io.ooo_to_mem.backendToTopBypass.msiAck
  io.outer_beu_errors_icache := RegNext(io.inner_beu_errors_icache)
  io.inner_hc_perfEvents <> RegNext(io.outer_hc_perfEvents)

  /**
    * --------------------------------------------------------------------
    * Performance and Debug
    * --------------------------------------------------------------------
    */
  io.debugTopDown.toCore.robHeadMissInDCache := dcache.io.debugTopDown.robHeadMissInDCache
  io.debugTopDown.toCore.robHeadTlbReplay := lsq.io.debugTopDown.robHeadTlbReplay
  io.debugTopDown.toCore.robHeadTlbMiss := lsq.io.debugTopDown.robHeadTlbMiss
  io.debugTopDown.toCore.robHeadLoadVio := lsq.io.debugTopDown.robHeadLoadVio
  io.debugTopDown.toCore.robHeadLoadMSHR := lsq.io.debugTopDown.robHeadLoadMSHR

  io.topDownInfo.toBackend.lqEmpty := lsq.io.lqEmpty
  io.topDownInfo.toBackend.sqEmpty := lsq.io.sqEmpty
  io.topDownInfo.toBackend.l1Miss := dcache.io.l1Miss
  io.topDownInfo.toBackend.l2TopMiss.l2Miss := RegNext(io.topDownInfo.fromL2Top.l2Miss)
  io.topDownInfo.toBackend.l2TopMiss.l3Miss := RegNext(io.topDownInfo.fromL2Top.l3Miss)

  io.mem_to_ooo.writeBack.zipWithIndex.foreach{ case (wb, i) =>
    PerfCCT.updateInstPos(wb.bits.uop.debug_seqNum, PerfCCT.InstPos.AtBypassVal.id.U, wb.valid, clock, reset)
  }

  val hyLdDeqCount = PopCount(io.ooo_to_mem.issueHya.map(x => x.valid && FuType.isLoad(x.bits.uop.fuType)))
  val hyStDeqCount = PopCount(io.ooo_to_mem.issueHya.map(x => x.valid && FuType.isStore(x.bits.uop.fuType)))
  val ldDeqCount = PopCount(io.ooo_to_mem.issueLda.map(_.valid)) +& hyLdDeqCount
  val stDeqCount = PopCount(io.ooo_to_mem.issueSta.take(StaCnt).map(_.valid)) +& hyStDeqCount
  val iqDeqCount = ldDeqCount +& stDeqCount
  XSPerfAccumulate("load_iq_deq_count", ldDeqCount)
  XSPerfHistogram("load_iq_deq_count", ldDeqCount, true.B, 0, LdExuCnt + 1)
  XSPerfAccumulate("store_iq_deq_count", stDeqCount)
  XSPerfHistogram("store_iq_deq_count", stDeqCount, true.B, 0, StAddrCnt + 1)
  XSPerfAccumulate("ls_iq_deq_count", iqDeqCount)

  // Performance event
  pfevent.io.distribute_csr := csrCtrl.distribute_csr
  val csrevents = pfevent.io.hpmevent.slice(16,24)

  val perfFromUnits = Seq(sbuffer, lsq, dcache).flatMap(_.getPerfEvents)
  val perfFromPTW = perfEventsPTW.getOrElse(Seq()).map(x => ("PTW_" + x._1, x._2))
  val perfBlock = Seq(("ldDeqCount", ldDeqCount), ("stDeqCount", stDeqCount))
  // let index = 0 be no event
  val allPerfEvents = Seq(("noEvent", 0.U)) ++ perfFromUnits ++ perfFromPTW ++ perfBlock

  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("MemBlock perfEvents Set", name, inc, i)
    }
  }

  val allPerfInc = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  val perfEvents = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
  generatePerfEvent()

  /**
    * --------------------------------------------------------------------
    * Trace Interface
    * --------------------------------------------------------------------
    */
  val traceToL2Top = io.traceCoreInterfaceBypass.toL2Top
  val traceFromBackend = io.traceCoreInterfaceBypass.fromBackend
  traceFromBackend.fromEncoder := RegNext(traceToL2Top.fromEncoder)
  traceToL2Top.toEncoder.trap  := RegEnable(
    traceFromBackend.toEncoder.trap,
    traceFromBackend.toEncoder.groups(0).valid && Itype.isTrap(traceFromBackend.toEncoder.groups(0).bits.itype)
  )
  traceToL2Top.toEncoder.priv := RegEnable(
    traceFromBackend.toEncoder.priv,
    traceFromBackend.toEncoder.groups(0).valid
  )
  (0 until TraceGroupNum).foreach { i =>
    traceToL2Top.toEncoder.groups(i).valid := RegNext(traceFromBackend.toEncoder.groups(i).valid)
    traceToL2Top.toEncoder.groups(i).bits.iretire := RegNext(traceFromBackend.toEncoder.groups(i).bits.iretire)
    traceToL2Top.toEncoder.groups(i).bits.itype := RegNext(traceFromBackend.toEncoder.groups(i).bits.itype)
    traceToL2Top.toEncoder.groups(i).bits.ilastsize := RegEnable(
      traceFromBackend.toEncoder.groups(i).bits.ilastsize,
      traceFromBackend.toEncoder.groups(i).valid
    )
    traceToL2Top.toEncoder.groups(i).bits.iaddr := RegEnable(
      traceFromBackend.toEncoder.groups(i).bits.iaddr,
      traceFromBackend.toEncoder.groups(i).valid
    ) + (RegEnable(
      traceFromBackend.toEncoder.groups(i).bits.ftqOffset.getOrElse(0.U),
      traceFromBackend.toEncoder.groups(i).valid
    ) << instOffsetBits)
  }

  /**
    * --------------------------------------------------------------------
    * Reset Tree of MemBlock
    * --------------------------------------------------------------------
    */
  if (p(DebugOptionsKey).ResetGen) {
    val leftResetTree = ResetGenNode(
      Seq(
        ModuleNode(ptw),
        ModuleNode(ptw_to_l2_buffer),
        ModuleNode(lsq),
        ModuleNode(st_dtlb),
        ModuleNode(pf_dtlb),
        ModuleNode(pmp)
      )
      ++ pmp_checkers.map(ModuleNode(_))
      ++ (if (smsPrefetcherOpt.isDefined) Seq(ModuleNode(smsPrefetcherOpt.get)) else Nil)
      ++ (if (l1PrefetcherOpt.isDefined) Seq(ModuleNode(l1PrefetcherOpt.get)) else Nil)
    )
    val rightResetTree = ResetGenNode(
      Seq(
        ModuleNode(sbuffer),
        ModuleNode(ld_dtlb),
        ModuleNode(dcache),
        ModuleNode(l1d_to_l2_buffer),
        CellNode(io.reset_backend)
      )
    )
    ResetGen(leftResetTree, reset, sim = false, io.dft_reset)
    ResetGen(rightResetTree, reset, sim = false, io.dft_reset)
  } else {
    io.reset_backend := DontCare
  }
  io.resetInFrontendBypass.toL2Top := io.resetInFrontendBypass.fromFrontend

  /**
    * --------------------------------------------------------------------
    * Difftest
    * --------------------------------------------------------------------
    */
  io.mem_to_ooo.storeDebugInfo := DontCare
  if (env.EnableDifftest) {
    (0 until EnsbufferWidth).foreach{i =>
        io.mem_to_ooo.storeDebugInfo(i).robidx := sbuffer.io.vecDifftestInfo(i).bits.robIdx
        sbuffer.io.vecDifftestInfo(i).bits.pc := io.mem_to_ooo.storeDebugInfo(i).pc
    }
  }

  if (env.EnableDifftest) {
    sbuffer.io.vecDifftestInfo .zipWithIndex.map{ case (sbufferPort, index) =>
      if (index == 0) {
        val vSegmentDifftestValid = vSegmentUnit.io.vecDifftestInfo.valid
        sbufferPort.valid := Mux(vSegmentDifftestValid, vSegmentUnit.io.vecDifftestInfo.valid, lsq.io.sbufferVecDifftestInfo(0).valid)
        sbufferPort.bits  := Mux(vSegmentDifftestValid, vSegmentUnit.io.vecDifftestInfo.bits, lsq.io.sbufferVecDifftestInfo(0).bits)

        vSegmentUnit.io.vecDifftestInfo.ready  := sbufferPort.ready
        lsq.io.sbufferVecDifftestInfo(0).ready := sbufferPort.ready
      } else {
         sbufferPort <> lsq.io.sbufferVecDifftestInfo(index)
      }
    }
  }

}

class MemBlock()(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val inner = LazyModule(new MemBlockInlined())

  lazy val module = new MemBlockImp(this)
}

class MemBlockImp(wrapper: MemBlock) extends LazyModuleImp(wrapper) {
  val io = IO(wrapper.inner.module.io.cloneType)
  val io_perf = IO(wrapper.inner.module.io_perf.cloneType)
  io <> wrapper.inner.module.io
  io_perf <> wrapper.inner.module.io_perf

  if (p(DebugOptionsKey).ResetGen) {
    ResetGen(
      ResetGenNode(Seq(ModuleNode(wrapper.inner.module))),
      reset, sim = false, io.dft_reset
    )
  }
}
