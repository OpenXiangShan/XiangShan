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

import chisel3._
import chisel3.util._
import coupledL2.{PrefetchCtrlFromCore, PrefetchRecv}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.tile.HasFPUParameters
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import system.{HasSoCParameter, SoCParamsKey}
import utility._
import utility.mbist.{MbistInterface, MbistPipeline}
import utility.sram.{SramBroadcastBundle, SramHelper}
import utils._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.ctrlblock.{DebugLSIO, LsTopdownInfo}
import xiangshan.backend.datapath.NewPipelineConnect
import xiangshan.backend.fu.FuType._
import xiangshan.backend.fu.NewCSR.PFEvent
import xiangshan.backend.fu._
import xiangshan.backend.fu.util.{CSRConst, SdtrigExt}
import xiangshan.backend.rob.{RobDebugRollingIO, RobPtr}
import xiangshan.backend.trace.{Itype, TraceCoreInterface}
import xiangshan.backend.{BackendToTopBundle, TopToBackendBundle}
import xiangshan.backend.Bundles._
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.frontend.instruncache.HasInstrUncacheConst
import xiangshan.mem.prefetch.{PrefetcherWrapper, TLBPlace}

trait HasMemBlockParameters extends HasXSParameter {
  val intSchdParams = backendParams.intSchdParams.get
  val vecSchdParams = backendParams.vecSchdParams.get

  val intMemExeUnitParams = intSchdParams.issueBlockParams.filter(_.isMemBlockIQ).map(_.allExuParams).flatten
  val vecMemExeUnitParams = vecSchdParams.issueBlockParams.filter(_.isMemBlockIQ).map(_.allExuParams).flatten

  val memExeUnitParams = intMemExeUnitParams ++ vecMemExeUnitParams
  val ldaParams = intMemExeUnitParams.filter(_.hasLoadFu)
  val staParams = intMemExeUnitParams.filter(_.hasStoreAddrFu)
  val stdParams = intMemExeUnitParams.filter(_.hasStdFu)
  val hyaParams = intMemExeUnitParams.filter(_.hasHyldaFu)
  val mouParam = intMemExeUnitParams.filter(_.hasMouFu).head
  val moudParam = intMemExeUnitParams.filter(_.hasMoudFu).head
  val vlduParams = vecMemExeUnitParams.filter(_.hasVLoadFu)
  val vstuParams = vecMemExeUnitParams.filter(_.hasVStoreFu)
  val vsegParam = vecMemExeUnitParams.filter(_.hasVSegFu).head

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

  def arbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  def arbiter_with_pipereg[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    AddPipelineReg(arb.io.out, out, false.B)
  }

  def arbiter_with_pipereg_N_dup[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    dups: Seq[DecoupledIO[T]],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    for (dup <- dups) {
      AddPipelineReg(arb.io.out, dup, false.B)
    }
    AddPipelineReg(arb.io.out, out, false.B)
  }

  def rrArbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new RRArbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  def fastArbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new FastArbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  def oneHotArbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    OneHot.checkOneHot(in.map(_.valid))
    arbiter(in, out, name)
    in.foreach(_.ready := out.ready)
  }
}

abstract class MemBlockBundle(implicit val p: Parameters) extends Bundle with HasMemBlockParameters

class Std(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits := 0.U.asTypeOf(io.out.bits)
  io.out.bits.res.data := io.in.bits.data.src(0)
  io.out.bits.ctrl.robIdx := io.in.bits.ctrl.robIdx
}

class ooo_to_mem(implicit p: Parameters) extends MemBlockBundle {
  val backendToTopBypass = Flipped(new BackendToTopBundle)

  val sfence = Input(new SfenceBundle)
  val tlbCsr = Input(new TlbCsrBundle)
  val lsqio = new Bundle {
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val scommit = Input(UInt(log2Up(CommitWidth + 1).W))
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

  val intIssue: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(intSchdParams.genExuInputCopySrcBundleMemBlock)
  val vecIssue: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(vecSchdParams.genExuInputCopySrcBundleMemBlock)
}

class mem_to_ooo(implicit p: Parameters) extends MemBlockBundle {
  val topToBackendBypass = new TopToBackendBundle

  val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
  val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
  val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
  // used by VLSU issue queue, the vector store would wait all store before it, and the vector load would wait all load
  val sqDeqPtr = Output(new SqPtr)
  val lqDeqPtr = Output(new LqPtr)
  val updateLFST = Vec(StAddrCnt, ValidIO(new StoreUnitToLFST))
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

  val intWriteback: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = intSchdParams.genExuOutputDecoupledBundleMemBlock
  val vecWriteback: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = vecSchdParams.genExuOutputDecoupledBundleMemBlock

  val ldaIqFeedback = Vec(LduCnt, new MemRSFeedbackIO)
  val staIqFeedback = Vec(StaCnt, new MemRSFeedbackIO)
  val hyuIqFeedback = Vec(HyuCnt, new MemRSFeedbackIO)
  val vstuIqFeedback= Vec(VstuCnt, new MemRSFeedbackIO(isVector = true))
  val vlduIqFeedback= Vec(VlduCnt, new MemRSFeedbackIO(isVector = true))
  val ldCancel = Vec(backendParams.LdExuCnt, new LoadCancelIO)
  val wakeup = Vec(backendParams.LdExuCnt, Valid(new MemWakeUpBundle))
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
  // NOTE: we currently only use one output port to L2 and L3 prefetch sender respectively
  val l2_pf_sender_opt = if (coreParams.prefetcher.nonEmpty)
    Some(BundleBridgeSource(() => new PrefetchRecv)) else None
  val l3_pf_sender_opt = if (p(SoCParamsKey).L3CacheParamsOpt.nonEmpty && coreParams.prefetcher.nonEmpty)
    Some(BundleBridgeSource(() => new huancun.PrefetchRecv)) else None
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
    val dcacheError = Output(new L1BusErrorUnitInfo())
    val uncacheError = Output(new L1BusErrorUnitInfo())
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
    val outer_l2PfCtrl = Output(new PrefetchCtrlFromCore)

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

    val wfi = Flipped(new WfiReqBundle)

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

  require(HyuCnt == 0) // HybridUnit is not supported for now

  val intIssue: Seq[DecoupledIO[ExuInput]] = io.ooo_to_mem.intIssue.flatten
  val vecIssue: Seq[DecoupledIO[ExuInput]] = io.ooo_to_mem.vecIssue.flatten
  val issueLda = intIssue.filter(_.bits.params.hasLoadFu)
  val issueSta = intIssue.filter(_.bits.params.hasStoreAddrFu)
  val issueStd = intIssue.filter(_.bits.params.hasStdFu)
  val issueVldu = vecIssue.filter(_.bits.params.hasVLoadFu)

  val intWriteback: Seq[DecoupledIO[ExuOutput]] = io.mem_to_ooo.intWriteback.flatten
  val vecWriteback: Seq[DecoupledIO[ExuOutput]] = io.mem_to_ooo.vecWriteback.flatten
  val writeback = intWriteback ++ vecWriteback
  val writebackLda = intWriteback.filter(_.bits.params.hasLoadFu)
  val writebackSta = intWriteback.filter(_.bits.params.hasStoreAddrFu)
  val writebackStd = intWriteback.filter(_.bits.params.hasStdFu)
  val writebackVldu = vecWriteback.filter(_.bits.params.hasVLoadFu)

  writeback.zipWithIndex.foreach{ case (wb, i) =>
    wb.bits.debug_seqNum.foreach(x => PerfCCT.updateInstPos(x, PerfCCT.InstPos.AtBypassVal.id.U, wb.valid, clock, reset))
  }

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

  val hartId = p(XSCoreParamsKey).HartId
  val redirect = RegNextWithEnable(io.redirect)

  private val dcache = outer.dcache.module
  val uncache = outer.uncache.module

  //val delayedDcacheRefill = RegNext(dcache.io.lsu.lsq)

  val csrCtrl = DelayN(io.ooo_to_mem.csrCtrl, 2)
  dcache.io.l2_pf_store_only := RegNext(io.ooo_to_mem.csrCtrl.pf_ctrl.l2_pf_store_only, false.B)
  val dcacheError = DelayNWithValid(dcache.io.error, 2)
  io.dcacheError <> dcacheError.bits.toL1BusErrorUnitInfo(dcacheError.valid)
  io.uncacheError.ecc_error <> DelayNWithValid(uncache.io.busError.ecc_error, 2)
  when(!csrCtrl.cache_error_enable){
    io.dcacheError.ecc_error.valid := false.B
    io.uncacheError.ecc_error.valid := false.B
  }

  val loadUnits = Seq.tabulate(LduCnt)(i => Module(new LoadUnit(ldaParams(i))))
  val storeUnits = Seq.tabulate(StaCnt)(i => Module(new StoreUnit(staParams(i))))
  val stdExeUnits = Seq.tabulate(StdCnt)(i => Module(new StdExeUnit(stdParams(i))))
  val atomicsUnit = Module(new AtomicsUnit(mouParam))

  // The number of vector load/store units is decoupled with the number of load/store units
  val vlSplit = Seq.tabulate(VlduCnt)(i => Module(new VLSplitImp(vlduParams(i))))
  val vsSplit = Seq.tabulate(VstuCnt)(i => Module(new VSSplitImp(vstuParams(i))))
  val vlMergeBuffer = Module(new VLMergeBufferImp)
  val vsMergeBuffer = Seq.fill(VstuCnt)(Module(new VSMergeBufferImp))
  val vSegmentUnit  = Module(new VSegmentUnit(vsegParam))
  val vfofBuffer    = Module(new VfofBuffer(vlduParams.head))

  // misalign Buffer
  val loadMisalignBuffer = Module(new LoadMisalignBuffer(ldaParams.head))
  val storeMisalignBuffer = Module(new StoreMisalignBuffer)

  loadUnits.zipWithIndex.map(x => x._1.suggestName("LoadUnit_"+x._2))
  storeUnits.zipWithIndex.map(x => x._1.suggestName("StoreUnit_"+x._2))

  writebackLda.zipWithIndex.foreach { case (wb, i) =>
    if (i == AtomicWBPort) {
      // atomicsUnit writeback
      oneHotArbiter(Seq(atomicsUnit.io.out, loadUnits(i).io.ldout), wb, Some("writebackLdaAtomic"))
    } else if (i == MisalignWBPort) {
      // misalign writeback
      arbiter(Seq(loadUnits(i).io.ldout, loadMisalignBuffer.io.writeBack), wb, Some("writebackLdaMisalign"))
    } else if (i == UncacheWBPort) {
      // uncache writeback
      wb <> loadUnits(i).io.ldout
    } else {
      // normal load writeback
      wb <> loadUnits(i).io.ldout
    }
  }

  writebackStd.zipWithIndex.foreach { case (wb, i) =>
    wb <> stdExeUnits(i).io.out
  }

  val lsq     = Module(new LsqWrapper)
  val sbuffer = Module(new Sbuffer)

  io.mem_to_ooo.stIssuePtr := lsq.io.issuePtrExt

  dcache.io.hartId := io.hartId
  lsq.io.hartId := io.hartId
  sbuffer.io.hartId := io.hartId
  atomicsUnit.io.hartId := io.hartId

  dcache.io.lqEmpty := lsq.io.lqEmpty
  dcache.io.wfi.wfiReq := io.wfi.wfiReq
  lsq.io.wfi.wfiReq := io.wfi.wfiReq

  // ptw
  val sfence = RegNext(RegNext(io.ooo_to_mem.sfence))
  val tlbcsr = RegNext(RegNext(io.ooo_to_mem.tlbCsr))
  private val ptw = outer.ptw.module
  private val ptw_to_l2_buffer = outer.ptw_to_l2_buffer.module
  private val l1d_to_l2_buffer = outer.l1d_to_l2_buffer.module
  ptw.io.hartId := io.hartId
  ptw.io.sfence <> sfence
  ptw.io.csr.tlb <> tlbcsr
  ptw.io.csr.distribute_csr <> csrCtrl.distribute_csr
  ptw.io.wfi.wfiReq := io.wfi.wfiReq

  io.wfi.wfiSafe := dcache.io.wfi.wfiSafe && uncache.io.wfi.wfiSafe && lsq.io.wfi.wfiSafe && ptw.io.wfi.wfiSafe

  val perfEventsPTW = if (!coreParams.softPTW) {
    ptw.getPerfEvents
  } else {
    Seq()
  }

  // dtlb parameters
  var pf2tlbIndexMap: Seq[Int] = Seq()
  var tmp_pf_num_in_dtlb_ld: Int = 0
  var tmp_pf_num_in_dtlb_pf: Int = 1 // 1 for l2 prefetch
  prefetcherSeq foreach { x =>
    if(x.tlbPlace == TLBPlace.dtlb_ld){
      pf2tlbIndexMap = pf2tlbIndexMap :+ (LduCnt + tmp_pf_num_in_dtlb_ld)
      tmp_pf_num_in_dtlb_ld += 1
    }else if(x.tlbPlace == TLBPlace.dtlb_pf){
      // 1 for l2 prefetch
      pf2tlbIndexMap = pf2tlbIndexMap :+ (LduCnt + PfNumInDtlbLD + StaCnt + tmp_pf_num_in_dtlb_pf)
      tmp_pf_num_in_dtlb_pf += 1
    }
  }
  val (dtlb_ld_idx, dtlb_st_idx, dtlb_pf_idx) = (0, 1, 2)
  val TlbSubSizeVec = Seq(LduCnt + PfNumInDtlbLD, StaCnt, PfNumInDtlbPF) // (load + hyu + stream pf, store, sms+l2bop)
  val DTlbSize = TlbSubSizeVec.sum
  val TlbStartVec = TlbSubSizeVec.scanLeft(0)(_ + _).dropRight(1)
  val TlbEndVec = TlbSubSizeVec.scanLeft(0)(_ + _).drop(1)
  val L2toL1DTLBPortIndex = TlbStartVec(dtlb_pf_idx)
  println(f"TLB Size:")
  println(f"  size = $DTlbSize = ${TlbSubSizeVec}")
  println(f"TLB Index Vec:")
  println(f"  TlbStartVec = ${TlbStartVec}")
  println(f"  TlbEndVec = ${TlbEndVec}")
  println(f"TLB for Prefetcher:")
  prefetcherSeq zip pf2tlbIndexMap foreach { case (pf, idx) =>
    println(f"  TLB #$idx%-2d => ${pf.name}")
  }
  println(f"  TLB #$L2toL1DTLBPortIndex%-2d => L2Prefetcher")

  // dtlb instantiation
  val dtlb_ld_tlb_ld = Module(new TLBNonBlock(TlbSubSizeVec(dtlb_ld_idx), 2, ldtlbParams))
  val dtlb_st_tlb_st = Module(new TLBNonBlock(TlbSubSizeVec(dtlb_st_idx), 1, sttlbParams))
  val dtlb_prefetch_tlb_prefetch = Module(new TLBNonBlock(TlbSubSizeVec(dtlb_pf_idx), 2, pftlbParams))
  val dtlb_ld = Seq(dtlb_ld_tlb_ld.io)
  val dtlb_st = Seq(dtlb_st_tlb_st.io)
  val dtlb_prefetch = Seq(dtlb_prefetch_tlb_prefetch.io)
  val dtlb = dtlb_ld ++ dtlb_st ++ dtlb_prefetch

  val perfEventsDTLBld = dtlb_ld_tlb_ld.getPerfEvents.map { case (str, idx) => ("dtlb_ld_" + str, idx) }
  val perfEventsDTLBst = dtlb_st_tlb_st.getPerfEvents.map { case (str, idx) => ("dtlb_st_" + str, idx) }

  val ptwio = Wire(new VectorTlbPtwIO(DTlbSize))
  val dtlb_reqs = dtlb.map(_.requestor).flatten
  val dtlb_pmps = dtlb.map(_.pmp).flatten
  dtlb.map(_.hartId := io.hartId)
  dtlb.map(_.sfence := sfence)
  dtlb.map(_.csr := tlbcsr)
  dtlb.map(_.flushPipe.map(a => a := false.B)) // non-block doesn't need
  dtlb.map(_.redirect := redirect)
  if (refillBothTlb) {
    require(ldtlbParams.outReplace == sttlbParams.outReplace)
    require(ldtlbParams.outReplace == hytlbParams.outReplace)
    require(ldtlbParams.outReplace == pftlbParams.outReplace)
    require(ldtlbParams.outReplace)

    val replace = Module(new TlbReplace(DTlbSize, ldtlbParams))
    replace.io.apply_sep(dtlb_ld.map(_.replace) ++ dtlb_st.map(_.replace) ++ dtlb_prefetch.map(_.replace), ptwio.resp.bits.data.s1.entry.tag)
  } else {
    // TODO: there will be bugs in TlbReplace when outReplace enable, since the order of Hyu is not right.
    if (ldtlbParams.outReplace) {
      val replace_ld = Module(new TlbReplace(LduCnt + PfNumInDtlbLD, ldtlbParams))
      replace_ld.io.apply_sep(dtlb_ld.map(_.replace), ptwio.resp.bits.data.s1.entry.tag)
    }
    if (hytlbParams.outReplace) {
      val replace_hy = Module(new TlbReplace(HyuCnt, hytlbParams))
      replace_hy.io.apply_sep(dtlb_ld.map(_.replace), ptwio.resp.bits.data.s1.entry.tag)
    }
    if (sttlbParams.outReplace) {
      val replace_st = Module(new TlbReplace(StaCnt, sttlbParams))
      replace_st.io.apply_sep(dtlb_st.map(_.replace), ptwio.resp.bits.data.s1.entry.tag)
    }
    if (pftlbParams.outReplace) {
      val replace_pf = Module(new TlbReplace(TlbSubSizeVec(dtlb_pf_idx), pftlbParams))
      replace_pf.io.apply_sep(dtlb_prefetch.map(_.replace), ptwio.resp.bits.data.s1.entry.tag)
    }
  }

  val ptw_resp_next = RegEnable(ptwio.resp.bits, ptwio.resp.valid)
  val ptw_resp_v = RegNext(ptwio.resp.valid && !(sfence.valid || tlbcsr.satp.changed || tlbcsr.vsatp.changed || tlbcsr.hgatp.changed || tlbcsr.priv.virt_changed), init = false.B)
  ptwio.resp.ready := true.B

  val tlbreplay = WireInit(VecInit(Seq.fill(LdExuCnt)(false.B)))
  val tlbreplay_reg = GatedValidRegNext(tlbreplay)
  val dtlb_ld0_tlbreplay_reg = GatedValidRegNext(dtlb_ld(0).tlbreplay)

  if (backendParams.debugEn){ dontTouch(tlbreplay) }

  for (i <- 0 until LdExuCnt) {
    tlbreplay(i) := dtlb_ld(0).ptw.req(i).valid && ptw_resp_next.vector(0) && ptw_resp_v &&
      ptw_resp_next.data.hit(dtlb_ld(0).ptw.req(i).bits.vpn, tlbcsr.satp.asid, tlbcsr.vsatp.asid, tlbcsr.hgatp.vmid,
        allType = true, ignoreAsid = true) // Maybe need not ignoreAsid here, however not a functional bug
  }

  dtlb.flatMap(a => a.ptw.req)
    .zipWithIndex
    .foreach{ case (tlb, i) =>
      tlb.ready := ptwio.req(i).ready
      ptwio.req(i).bits := tlb.bits
    val vector_hit = if (refillBothTlb) Cat(ptw_resp_next.vector).orR
      else if (i < TlbEndVec(dtlb_ld_idx)) Cat(ptw_resp_next.vector.slice(TlbStartVec(dtlb_ld_idx), TlbEndVec(dtlb_ld_idx))).orR
      else if (i < TlbEndVec(dtlb_st_idx)) Cat(ptw_resp_next.vector.slice(TlbStartVec(dtlb_st_idx), TlbEndVec(dtlb_st_idx))).orR
      else                                 Cat(ptw_resp_next.vector.slice(TlbStartVec(dtlb_pf_idx), TlbEndVec(dtlb_pf_idx))).orR
    ptwio.req(i).valid := tlb.valid &&
      !(ptw_resp_v && vector_hit &&
        ptw_resp_next.data.hit(tlb.bits.vpn, tlbcsr.satp.asid, tlbcsr.vsatp.asid, tlbcsr.hgatp.vmid,
          allType = true, ignoreAsid = true)) // // Maybe need not ignoreAsid here, however not a functional bug
  }
  dtlb.foreach(_.ptw.resp.bits := ptw_resp_next.data)
  if (refillBothTlb) {
    dtlb.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector).orR)
  } else {
    dtlb_ld.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.slice(TlbStartVec(dtlb_ld_idx), TlbEndVec(dtlb_ld_idx))).orR)
    dtlb_st.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.slice(TlbStartVec(dtlb_st_idx), TlbEndVec(dtlb_st_idx))).orR)
    dtlb_prefetch.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.slice(TlbStartVec(dtlb_pf_idx), TlbEndVec(dtlb_pf_idx))).orR)
  }

  val dtlbRepeater  = PTWNewFilter(ldtlbParams.fenceDelay, ptwio, ptw.io.tlb(1), sfence, tlbcsr, l2tlbParams.dfilterSize)
  val itlbRepeater3 = PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, io.fetch_to_mem.itlb, ptw.io.tlb(0), sfence, tlbcsr)

  lsq.io.debugTopDown.robHeadMissInDTlb := dtlbRepeater.io.rob_head_miss_in_tlb

  // pmp
  val pmp = Module(new PMP())
  pmp.io.distribute_csr <> csrCtrl.distribute_csr

  val pmp_checkers = Seq.fill(DTlbSize)(Module(new PMPChecker(4, leaveHitMux = true)))
  val pmp_check = pmp_checkers.map(_.io)
  for ((p,d) <- pmp_check zip dtlb_pmps) {
    if (HasBitmapCheck) {
      if (KeyIDBits > 0) {
        p.apply(tlbcsr.mbmc.KEYIDEN.asBool, tlbcsr.mbmc.CMODE.asBool, tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
      } else {
        p.apply(tlbcsr.mbmc.CMODE.asBool, tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
      }
    } else {
      p.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
    }
    require(p.req.bits.size.getWidth == d.bits.size.getWidth)
  }


  /******************************************************************
   * Prefetcher
   *  - L1 site
   *  - L2 site: provide tlb interface
   *  - L1 prefetch request: set confidence
   *  - L1 prefetcher: fuzzer interface
   ******************************************************************/

  /** prefetcher site in L1 */
  val l1_pf_req = Wire(Decoupled(new L1PrefetchReq()))
  val prefetcher = Module(new PrefetcherWrapper)
  prefetcher.io.pfCtrlFromTile.l2PfqBusy := io.l2PfqBusy
  prefetcher.io.pfCtrlFromCSR := io.ooo_to_mem.csrCtrl.pf_ctrl
  prefetcher.io.pfCtrlFromDCache <> dcache.io.pf_ctrl
  prefetcher.io.fromDCache.sms_agt_evict_req <> dcache.io.sms_agt_evict_req
  prefetcher.io.fromDCache.refillTrain := dcache.io.refillTrain
  prefetcher.io.fromOOO.s1_loadPc := issueLda.map(x => RegNext(x.bits.pc.get)) ++ io.ooo_to_mem.hybridPc
  prefetcher.io.fromOOO.s1_storePc := io.ooo_to_mem.storePc ++ io.ooo_to_mem.hybridPc
  prefetcher.io.trainSource.s1_loadFireHint := loadUnits.map(_.io.s1_prefetch_spec)
  prefetcher.io.trainSource.s2_loadFireHint := loadUnits.map(_.io.s2_prefetch_spec)
  prefetcher.io.trainSource.s3_load := loadUnits.map(_.io.prefetch_train)
  prefetcher.io.trainSource.s3_ptrChasing := loadUnits.map(_ => false.B) // TODO: remove ptr chasing logic in prefetcher
  prefetcher.io.trainSource.s1_storeFireHint := storeUnits.map(_.io.s1_prefetch_spec)
  prefetcher.io.trainSource.s2_storeFireHint := storeUnits.map(_.io.s2_prefetch_spec)
  prefetcher.io.trainSource.s3_store <> storeUnits.map(_.io.prefetch_train)
  (0 until prefetcherNum).foreach { i => //NOTE lyq: prefetcherNum minimum is 1 for simpler code generation, which is ugly
    prefetcher.io.tlb_req(i).req.ready := false.B
    prefetcher.io.tlb_req(i).resp.valid := false.B
    prefetcher.io.tlb_req(i).resp.bits := DontCare
    prefetcher.io.pmp_resp(i) := DontCare
  }
  pf2tlbIndexMap.zipWithIndex.foreach{ case (k, i) =>
    dtlb_reqs(k) <> prefetcher.io.tlb_req(i)
    prefetcher.io.pmp_resp(i) := pmp_check(k).resp
  }
  l1_pf_req <> prefetcher.io.l1_pf_to_l1
  outer.l2_pf_sender_opt.foreach(_.out.head._1.addr_valid := prefetcher.io.l1_pf_to_l2.addr_valid)
  outer.l2_pf_sender_opt.foreach(_.out.head._1.addr := prefetcher.io.l1_pf_to_l2.addr)
  outer.l2_pf_sender_opt.foreach(_.out.head._1.pf_source :=  prefetcher.io.l1_pf_to_l2.pf_source)
  outer.l2_pf_sender_opt.foreach(_.out.head._1.l2_pf_en := prefetcher.io.l1_pf_to_l2.l2_pf_en)
  outer.l3_pf_sender_opt.foreach(_.out.head._1.addr_valid := prefetcher.io.l1_pf_to_l3.addr_valid)
  outer.l3_pf_sender_opt.foreach(_.out.head._1.addr := prefetcher.io.l1_pf_to_l3.addr)
  outer.l3_pf_sender_opt.foreach(_.out.head._1.l2_pf_en := prefetcher.io.l1_pf_to_l3.l2_pf_en)
  XSPerfAccumulate("prefetch_fire_l1", l1_pf_req.fire)
  XSPerfAccumulate("prefetch_fire_l2", outer.l2_pf_sender_opt.map(_.out.head._1.addr_valid).getOrElse(false.B))
  XSPerfAccumulate("prefetch_fire_l3", outer.l3_pf_sender_opt.map(_.out.head._1.addr_valid).getOrElse(false.B))

  /** prefetcher site in L2 */
  dtlb_reqs(L2toL1DTLBPortIndex) <> io.l2_tlb_req
  dtlb_reqs(L2toL1DTLBPortIndex).resp.ready := true.B
  io.l2_pmp_resp := pmp_check(L2toL1DTLBPortIndex).resp

  /** prefetch to l1 req */
  // Stream's confidence is always 1
  // NOTE lx:
  // loadUnits(0) has higher bank conflict and miss queue arb priority than loadUnits(1) and loadUnits(2)
  // when loadUnits(1)/loadUnits(2) stage 0 is busy, hw prefetch will never use that pipeline
  // NOTE lyq:
  // Because all the unfairness between ldu0 and ldu1/2, such as bank conflicts and lower entry priority in MissQueue,
  // belong to the replay channel, whose priority is higher than prefetch channel in loadunit.
  // Therefore, there is no need to distinguish among ldu0, ldu1, and ldu2 if **prefetch-request outstanding <= 1**.
  val canAcceptHighConfPrefetch = loadUnits.map(_.io.canAcceptHighConfPrefetch)
  val canAcceptLowConfPrefetch = loadUnits.map(_.io.canAcceptLowConfPrefetch)
  val canAcceptPrefetch = (0 until LduCnt + HyuCnt).map{ case i =>
    Mux(l1_pf_req.bits.confidence === 1.U, canAcceptHighConfPrefetch(i), canAcceptLowConfPrefetch(i))
    /* // if it needs to distinguish ldu0 with others, use the code below
    if (LduCnt > 1 && i == 0) {
      Mux(l1_pf_req.bits.confidence === 1.U, canAcceptHighConfPrefetch(i), canAcceptLowConfPrefetch(i))
    } else {
      canAcceptLowConfPrefetch(i)
    } */
  }
  l1_pf_req.ready := canAcceptPrefetch.reduce(_ || _)

  val toPrefetchValidVec = (0 until LduCnt + HyuCnt).map{ case i =>
    if(i==0) l1_pf_req.valid
    else l1_pf_req.valid && !canAcceptPrefetch.take(i).reduce(_ || _)
  }
  loadUnits.zipWithIndex.foreach { case(u, i) => {
    u.io.prefetch_req.valid <> toPrefetchValidVec(i)
    u.io.prefetch_req.bits <> l1_pf_req.bits
  }}

  /** l1 pf fuzzer interface */
  val DebugEnableL1PFFuzzer = false
  if (DebugEnableL1PFFuzzer) {
    // l1 pf req fuzzer
    val fuzzer = Module(new L1PrefetchFuzzer())
    fuzzer.io.vaddr := DontCare
    fuzzer.io.paddr := DontCare

    // override load_unit prefetch_req
    loadUnits.foreach(load_unit => {
      load_unit.io.prefetch_req.valid <> fuzzer.io.req.valid
      load_unit.io.prefetch_req.bits <> fuzzer.io.req.bits
    })

    fuzzer.io.req.ready := l1_pf_req.ready
  }

  for (i <- 0 until LduCnt) {
    io.debug_ls.debugLsInfo(i) := loadUnits(i).io.debug_ls
  }
  for (i <- 0 until StaCnt) {
    io.debug_ls.debugLsInfo.drop(LduCnt)(i) := storeUnits(i).io.debug_ls
  }

  io.mem_to_ooo.lsTopdownInfo := loadUnits.map(_.io.lsTopdownInfo)

  // trigger
  val tdata = RegInit(VecInit(Seq.fill(TriggerNum)(0.U.asTypeOf(new MatchTriggerIO))))
  val tEnable = RegInit(VecInit(Seq.fill(TriggerNum)(false.B)))
  tEnable := csrCtrl.mem_trigger.tEnableVec
  when(csrCtrl.mem_trigger.tUpdate.valid) {
    tdata(csrCtrl.mem_trigger.tUpdate.bits.addr) := csrCtrl.mem_trigger.tUpdate.bits.tdata
  }
  val triggerCanRaiseBpExp = csrCtrl.mem_trigger.triggerCanRaiseBpExp
  val debugMode = csrCtrl.mem_trigger.debugMode

  val backendTriggerTimingVec = VecInit(tdata.map(_.timing))
  val backendTriggerChainVec = VecInit(tdata.map(_.chain))

  XSDebug(tEnable.asUInt.orR, "Debug Mode: At least one store trigger is enabled\n")
  for (j <- 0 until TriggerNum)
    PrintTriggerInfo(tEnable(j), tdata(j))

  // The segment instruction is executed atomically.
  // After the segment instruction directive starts executing, no other instructions should be executed.
  val vSegmentFlag = RegInit(false.B)

  when(GatedValidRegNext(vSegmentUnit.io.in.fire)) {
    vSegmentFlag := true.B
  }.elsewhen(GatedValidRegNext(vSegmentUnit.io.uopwriteback.valid)) {
    vSegmentFlag := false.B
  }

  val misalign_allow_spec = RegInit(true.B)
  val ldu_rollback_with_misalign_nack = loadUnits.map(ldu =>
    ldu.io.lsq.ldin.bits.isFrmMisAlignBuf && ldu.io.lsq.ldin.bits.rep_info.rar_nack && ldu.io.rollback.valid
  ).reduce(_ || _)
  when (ldu_rollback_with_misalign_nack) {
    misalign_allow_spec := false.B
  } .elsewhen(lsq.io.rarValidCount < (LoadQueueRARSize - 4).U) {
    misalign_allow_spec := true.B
  }

  // LoadUnit
  val correctMissTrain = Constantin.createRecord(s"CorrectMissTrain$hartId", initValue = false)

  for (i <- 0 until LduCnt) {
    loadUnits(i).io.redirect <> redirect
    loadUnits(i).io.misalign_allow_spec := misalign_allow_spec

    // get input form dispatch
    loadUnits(i).io.ldin <> issueLda(i)
    loadUnits(i).io.feedback_slow <> io.mem_to_ooo.ldaIqFeedback(i).feedbackSlow
    io.mem_to_ooo.ldaIqFeedback(i).feedbackFast := DontCare
    loadUnits(i).io.correctMissTrain := correctMissTrain
    io.mem_to_ooo.ldCancel(i) := loadUnits(i).io.ldCancel
    io.mem_to_ooo.wakeup(i) := loadUnits(i).io.wakeup

    // vector
    if (i < VlduCnt) {
      loadUnits(i).io.vecldout.ready := false.B
    } else {
      loadUnits(i).io.vecldin.valid := false.B
      loadUnits(i).io.vecldin.bits := DontCare
      loadUnits(i).io.vecldout.ready := false.B
    }

    // fast replay
    loadUnits(i).io.fast_rep_in <> loadUnits(i).io.fast_rep_out

    // SoftPrefetch to frontend (prefetch.i)
    loadUnits(i).io.ifetchPrefetch <> io.ifetchPrefetch(i)

    // dcache access
    loadUnits(i).io.dcache <> dcache.io.lsu.load(i)
    if(i == 0){
      vSegmentUnit.io.rdcache := DontCare
      dcache.io.lsu.load(i).req.valid := loadUnits(i).io.dcache.req.valid || vSegmentUnit.io.rdcache.req.valid
      dcache.io.lsu.load(i).req.bits  := Mux1H(Seq(
        vSegmentUnit.io.rdcache.req.valid -> vSegmentUnit.io.rdcache.req.bits,
        loadUnits(i).io.dcache.req.valid -> loadUnits(i).io.dcache.req.bits
      ))
      vSegmentUnit.io.rdcache.req.ready := dcache.io.lsu.load(i).req.ready
    }

    // Dcache requests must also be preempted by the segment.
    when(vSegmentFlag){
      loadUnits(i).io.dcache.req.ready             := false.B // Dcache is preempted.

      dcache.io.lsu.load(0).pf_source              := vSegmentUnit.io.rdcache.pf_source
      dcache.io.lsu.load(0).s1_paddr_dup_lsu       := vSegmentUnit.io.rdcache.s1_paddr_dup_lsu
      dcache.io.lsu.load(0).s1_paddr_dup_dcache    := vSegmentUnit.io.rdcache.s1_paddr_dup_dcache
      dcache.io.lsu.load(0).s1_kill                := vSegmentUnit.io.rdcache.s1_kill
      dcache.io.lsu.load(0).s2_kill                := vSegmentUnit.io.rdcache.s2_kill
      dcache.io.lsu.load(0).s0_pc                  := vSegmentUnit.io.rdcache.s0_pc
      dcache.io.lsu.load(0).s1_pc                  := vSegmentUnit.io.rdcache.s1_pc
      dcache.io.lsu.load(0).s2_pc                  := vSegmentUnit.io.rdcache.s2_pc
      dcache.io.lsu.load(0).is128Req               := vSegmentUnit.io.rdcache.is128Req
    }.otherwise {
      loadUnits(i).io.dcache.req.ready             := dcache.io.lsu.load(i).req.ready

      dcache.io.lsu.load(0).pf_source              := loadUnits(0).io.dcache.pf_source
      dcache.io.lsu.load(0).s1_paddr_dup_lsu       := loadUnits(0).io.dcache.s1_paddr_dup_lsu
      dcache.io.lsu.load(0).s1_paddr_dup_dcache    := loadUnits(0).io.dcache.s1_paddr_dup_dcache
      dcache.io.lsu.load(0).s1_kill                := loadUnits(0).io.dcache.s1_kill
      dcache.io.lsu.load(0).s2_kill                := loadUnits(0).io.dcache.s2_kill
      dcache.io.lsu.load(0).s0_pc                  := loadUnits(0).io.dcache.s0_pc
      dcache.io.lsu.load(0).s1_pc                  := loadUnits(0).io.dcache.s1_pc
      dcache.io.lsu.load(0).s2_pc                  := loadUnits(0).io.dcache.s2_pc
      dcache.io.lsu.load(0).is128Req               := loadUnits(0).io.dcache.is128Req
    }

    // forward
    loadUnits(i).io.lsq.forward <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer <> sbuffer.io.forward(i)
    loadUnits(i).io.ubuffer <> uncache.io.forward(i)
    loadUnits(i).io.tl_d_channel := dcache.io.lsu.forward_D(i)
    loadUnits(i).io.forward_mshr <> dcache.io.lsu.forward_mshr(i)
    // ld-ld violation check
    loadUnits(i).io.lsq.ldld_nuke_query <> lsq.io.ldu.ldld_nuke_query(i)
    loadUnits(i).io.lsq.stld_nuke_query <> lsq.io.ldu.stld_nuke_query(i)
    // loadqueue old ptr
    loadUnits(i).io.lsq.lqDeqPtr := lsq.io.lqDeqPtr
    loadUnits(i).io.csrCtrl       <> csrCtrl
    // dcache refill req
  // loadUnits(i).io.refill           <> delayedDcacheRefill
    // dtlb
    loadUnits(i).io.tlb <> dtlb_reqs.take(LduCnt)(i)
    if(i == 0 ){ // port 0 assign to vsegmentUnit
      val vsegmentDtlbReqValid = vSegmentUnit.io.dtlb.req.valid // segment tlb resquest need to delay 1 cycle
      dtlb_reqs.take(LduCnt)(i).req.valid := loadUnits(i).io.tlb.req.valid || RegNext(vsegmentDtlbReqValid)
      vSegmentUnit.io.dtlb.req.ready      := dtlb_reqs.take(LduCnt)(i).req.ready
      dtlb_reqs.take(LduCnt)(i).req.bits  := ParallelPriorityMux(Seq(
        RegNext(vsegmentDtlbReqValid)     -> RegEnable(vSegmentUnit.io.dtlb.req.bits, vsegmentDtlbReqValid),
        loadUnits(i).io.tlb.req.valid     -> loadUnits(i).io.tlb.req.bits
      ))
    }
    // pmp
    loadUnits(i).io.pmp <> pmp_check(i).resp
    // st-ld violation query
    val stld_nuke_query = storeUnits.map(_.io.stld_nuke_query)
    for (s <- 0 until StorePipelineWidth) {
      loadUnits(i).io.stld_nuke_query(s) := stld_nuke_query(s)
    }

    // load replay
    loadUnits(i).io.replay <> lsq.io.replay(i)

    val l2_hint = RegNext(io.l2_hint)

    // L2 Hint for DCache
    dcache.io.l2_hint <> l2_hint

    loadUnits(i).io.tlb_hint.id := dtlbRepeater.io.hint.get.req(i).id
    loadUnits(i).io.tlb_hint.full := dtlbRepeater.io.hint.get.req(i).full ||
      tlbreplay_reg(i) || dtlb_ld0_tlbreplay_reg(i)

    // passdown to lsq (load s2)
    lsq.io.ldu.ldin(i) <> loadUnits(i).io.lsq.ldin
    if (i == UncacheWBPort) {
      lsq.io.ldout(i) <> loadUnits(i).io.lsq.uncache
    } else {
      lsq.io.ldout(i).ready := true.B
      loadUnits(i).io.lsq.uncache.valid := false.B
      loadUnits(i).io.lsq.uncache.bits := DontCare
    }
    lsq.io.ld_raw_data(i) <> loadUnits(i).io.lsq.ld_raw_data
    lsq.io.ncOut(i) <> loadUnits(i).io.lsq.nc_ldin
    lsq.io.l2_hint.valid := l2_hint.valid
    lsq.io.l2_hint.bits.sourceId := l2_hint.bits.sourceId
    lsq.io.l2_hint.bits.isKeyword := l2_hint.bits.isKeyword

    lsq.io.tlb_hint <> dtlbRepeater.io.hint.get

    // connect misalignBuffer
    loadMisalignBuffer.io.enq(i) <> loadUnits(i).io.misalign_enq

    if (i == MisalignWBPort) {
      loadUnits(i).io.misalign_ldin  <> loadMisalignBuffer.io.splitLoadReq
      loadUnits(i).io.misalign_ldout <> loadMisalignBuffer.io.splitLoadResp
    } else {
      loadUnits(i).io.misalign_ldin.valid := false.B
      loadUnits(i).io.misalign_ldin.bits := DontCare
    }

    // update mem dependency predictor
    // io.memPredUpdate(i) := DontCare

    // --------------------------------
    // Load Triggers
    // --------------------------------
    loadUnits(i).io.fromCsrTrigger.tdataVec := tdata
    loadUnits(i).io.fromCsrTrigger.tEnableVec := tEnable
    loadUnits(i).io.fromCsrTrigger.triggerCanRaiseBpExp := triggerCanRaiseBpExp
    loadUnits(i).io.fromCsrTrigger.debugMode := debugMode
  }

  // misalignBuffer
  loadMisalignBuffer.io.redirect                <> redirect
  loadMisalignBuffer.io.rob.lcommit             := io.ooo_to_mem.lsqio.lcommit
  loadMisalignBuffer.io.rob.scommit             := io.ooo_to_mem.lsqio.scommit
  loadMisalignBuffer.io.rob.commit              := io.ooo_to_mem.lsqio.commit
  loadMisalignBuffer.io.rob.pendingPtr          := io.ooo_to_mem.lsqio.pendingPtr
  loadMisalignBuffer.io.rob.pendingPtrNext      := io.ooo_to_mem.lsqio.pendingPtrNext

  lsq.io.loadMisalignFull                       := loadMisalignBuffer.io.loadMisalignFull
  lsq.io.misalignAllowSpec                      := misalign_allow_spec

  storeMisalignBuffer.io.redirect               <> redirect
  storeMisalignBuffer.io.rob.lcommit            := io.ooo_to_mem.lsqio.lcommit
  storeMisalignBuffer.io.rob.scommit            := io.ooo_to_mem.lsqio.scommit
  storeMisalignBuffer.io.rob.commit             := io.ooo_to_mem.lsqio.commit
  storeMisalignBuffer.io.rob.pendingPtr         := io.ooo_to_mem.lsqio.pendingPtr
  storeMisalignBuffer.io.rob.pendingPtrNext     := io.ooo_to_mem.lsqio.pendingPtrNext

  lsq.io.maControl                              <> storeMisalignBuffer.io.sqControl

  lsq.io.cmoOpReq <> dcache.io.cmoOpReq
  lsq.io.cmoOpResp <> dcache.io.cmoOpResp

  // StoreUnit
  for (i <- 0 until StdCnt) {
    stdExeUnits(i).io.flush <> redirect
    stdExeUnits(i).io.in <> issueStd(i)
    if (i < VstuCnt) {
      stdExeUnits(i).io.vstdIn := vsSplit(i).io.vstd.get
    } else {
      stdExeUnits(i).io.vstdIn := DontCare
      stdExeUnits(i).io.vstdIn.valid := false.B
    }
    lsq.io.std.storeDataIn(i) := stdExeUnits(i).io.sqData
  }

  for (i <- 0 until StaCnt) {
    val stu = storeUnits(i)

    stu.io.redirect      <> redirect
    stu.io.csrCtrl       <> csrCtrl
    stu.io.dcache        <> dcache.io.lsu.sta(i)
    stu.io.feedback_slow <> io.mem_to_ooo.staIqFeedback(i).feedbackSlow
    stu.io.stin         <> issueSta(i)
    stu.io.lsq          <> lsq.io.sta.storeAddrIn(i)
    stu.io.lsq_replenish <> lsq.io.sta.storeAddrInRe(i)
    // dtlb
    stu.io.tlb          <> dtlb_st.head.requestor(i)
    stu.io.pmp          <> pmp_check(TlbStartVec(dtlb_st_idx) + i).resp
    stu.io.sqCommitPtr     <> lsq.io.sqCommitPtr
    stu.io.sqCommitUopIdx  <> lsq.io.sqCommitUopIdx
    stu.io.sqCommitRobIdx  <> lsq.io.sqCommitRobIdx

    // -------------------------
    // Store Triggers
    // -------------------------
    stu.io.fromCsrTrigger.tdataVec := tdata
    stu.io.fromCsrTrigger.tEnableVec := tEnable
    stu.io.fromCsrTrigger.triggerCanRaiseBpExp := triggerCanRaiseBpExp
    stu.io.fromCsrTrigger.debugMode := debugMode

    // prefetch
    stu.io.prefetch_req <> sbuffer.io.store_prefetch(i)

    // store unit does not need fast feedback
    io.mem_to_ooo.staIqFeedback(i).feedbackFast := DontCare

    // Lsq to sta unit
    lsq.io.sta.storeMaskIn(i) <> stu.io.st_mask_out

    // connect misalignBuffer
    storeMisalignBuffer.io.enq(i) <> stu.io.misalign_enq

    if (i == 0) {
      stu.io.misalign_stin  <> storeMisalignBuffer.io.splitStoreReq
      stu.io.misalign_stout <> storeMisalignBuffer.io.splitStoreResp
    } else {
      stu.io.misalign_stin.valid := false.B
      stu.io.misalign_stin.bits := DontCare
    }

    // 1. sync issue info to store set LFST
    // 2. when store issue, broadcast issued sqPtr to wake up the following insts
    io.mem_to_ooo.updateLFST(i) := stu.io.updateLFST

    stu.io.stout.ready := true.B

    // vector
    if (i < VstuCnt) {
      stu.io.vecstin <> vsSplit(i).io.out
      // vsFlowQueue.io.pipeFeedback(i) <> stu.io.vec_feedback_slow // need connect
    } else {
      stu.io.vecstin.valid := false.B
      stu.io.vecstin.bits := DontCare
      stu.io.vecstout.ready := false.B
    }
    stu.io.vec_isFirstIssue := true.B // TODO
  }

  val sqStout, sqStoutLatch = Wire(DecoupledIO(new ExuOutput(staParams.head)))
  oneHotArbiter(Seq(lsq.io.mmioStout, lsq.io.cboZeroStout), sqStout, Some("sqStout"))
  NewPipelineConnect(sqStout, sqStoutLatch, sqStoutLatch.fire, false.B, Some("sqStout"))
  writebackSta.zipWithIndex.foreach { case (wb, i) =>
    if (i == 0) {
      arbiter(
        Seq(storeUnits(i).io.stout, sqStoutLatch, storeMisalignBuffer.io.writeBack),
        wb, Some(s"writebackSta_$i")
      )
    } else {
      wb <> storeUnits(i).io.stout
    }
  }

  lsq.io.vecmmioStout.ready := false.B

  // Uncache
  uncache.io.enableOutstanding := io.ooo_to_mem.csrCtrl.uncache_write_outstanding_enable
  uncache.io.hartId := io.hartId
  uncache.io.wfi.wfiReq := io.wfi.wfiReq
  lsq.io.uncacheOutstanding := io.ooo_to_mem.csrCtrl.uncache_write_outstanding_enable

  // Lsq
  io.mem_to_ooo.lsqio.mmio       := lsq.io.rob.mmio
  io.mem_to_ooo.lsqio.uop        := lsq.io.rob.uop
  lsq.io.rob.lcommit             := io.ooo_to_mem.lsqio.lcommit
  lsq.io.rob.scommit             := io.ooo_to_mem.lsqio.scommit
  lsq.io.rob.commit              := io.ooo_to_mem.lsqio.commit
  lsq.io.rob.pendingPtr          := io.ooo_to_mem.lsqio.pendingPtr
  lsq.io.rob.pendingPtrNext      := io.ooo_to_mem.lsqio.pendingPtrNext

  //  lsq.io.rob            <> io.lsqio.rob
  lsq.io.enq            <> io.ooo_to_mem.enqLsq
  lsq.io.brqRedirect    <> redirect

  //  violation rollback
  def selectOldestRedirect(xs: Seq[Valid[Redirect]]): Vec[Bool] = {
    val compareVec = (0 until xs.length).map(i => (0 until i).map(j => isAfter(xs(j).bits.robIdx, xs(i).bits.robIdx)))
    val resultOnehot = VecInit((0 until xs.length).map(i => Cat((0 until xs.length).map(j =>
      (if (j < i) !xs(j).valid || compareVec(i)(j)
      else if (j == i) xs(i).valid
      else !xs(j).valid || !compareVec(j)(i))
    )).andR))
    resultOnehot
  }
  val allRedirect = loadUnits.map(_.io.rollback) ++ lsq.io.nack_rollback ++ lsq.io.nuke_rollback
  val oldestOneHot = selectOldestRedirect(allRedirect)
  val oldestRedirect = WireDefault(Mux1H(oldestOneHot, allRedirect))
  // memory replay would not cause IAF/IPF/IGPF
  oldestRedirect.bits.backendIAF := false.B
  oldestRedirect.bits.backendIPF := false.B
  oldestRedirect.bits.backendIGPF := false.B
  io.mem_to_ooo.memoryViolation := oldestRedirect
  io.mem_to_ooo.lsqio.lqCanAccept  := lsq.io.lqCanAccept
  io.mem_to_ooo.lsqio.sqCanAccept  := lsq.io.sqCanAccept

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
          when (!lsq.io.uncache.req.bits.nc || !io.ooo_to_mem.csrCtrl.uncache_write_outstanding_enable) {
            uncacheState := s_scalar_uncache
          }
        }.otherwise {
          // val isStore = vsFlowQueue.io.uncache.req.bits.cmd === MemoryOpConstants.M_XWR
          when (!io.ooo_to_mem.csrCtrl.uncache_write_outstanding_enable) {
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
  when (io.ooo_to_mem.csrCtrl.uncache_write_outstanding_enable) {
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

  //lsq.io.refill         := delayedDcacheRefill
  lsq.io.release        := dcache.io.lsu.release
  lsq.io.lqCancelCnt <> io.mem_to_ooo.lqCancelCnt
  lsq.io.sqCancelCnt <> io.mem_to_ooo.sqCancelCnt
  lsq.io.lqDeq <> io.mem_to_ooo.lqDeq
  lsq.io.sqDeq <> io.mem_to_ooo.sqDeq
  // Todo: assign these
  io.mem_to_ooo.sqDeqPtr := lsq.io.sqDeqPtr
  io.mem_to_ooo.lqDeqPtr := lsq.io.lqDeqPtr
  lsq.io.tl_d_channel <> dcache.io.lsu.tl_d_channel

  // LSQ to store buffer
  lsq.io.sbuffer        <> sbuffer.io.in
  sbuffer.io.in(0).valid := lsq.io.sbuffer(0).valid || vSegmentUnit.io.sbuffer.valid
  sbuffer.io.in(0).bits  := Mux1H(Seq(
    vSegmentUnit.io.sbuffer.valid -> vSegmentUnit.io.sbuffer.bits,
    lsq.io.sbuffer(0).valid       -> lsq.io.sbuffer(0).bits
  ))
  vSegmentUnit.io.sbuffer.ready := sbuffer.io.in(0).ready
  lsq.io.sqEmpty        <> sbuffer.io.sqempty
  dcache.io.force_write := lsq.io.force_write

  // Initialize when unenabled difftest.
  sbuffer.io.diffStore := DontCare
  lsq.io.diffStore := DontCare
  vSegmentUnit.io.vecDifftestInfo := DontCare
  io.mem_to_ooo.storeDebugInfo := DontCare
  // store event difftest information
  if (env.EnableDifftest) {
    // diffStoreEvent for vSegment, pmaStore and ncStore
    (0 until EnsbufferWidth).foreach{i =>
      if(i == 0) {
        when(vSegmentUnit.io.sbuffer.valid) {
          sbuffer.io.diffStore.diffInfo(0) := vSegmentUnit.io.vecDifftestInfo.bits
          sbuffer.io.diffStore.pmaStore(0).valid := vSegmentUnit.io.sbuffer.fire
          sbuffer.io.diffStore.pmaStore(0).bits := vSegmentUnit.io.sbuffer.bits
        }.otherwise{
          sbuffer.io.diffStore.diffInfo(0) := lsq.io.diffStore.diffInfo(0)
          sbuffer.io.diffStore.pmaStore(0) := lsq.io.diffStore.pmaStore(0)
        }
      }else{
        sbuffer.io.diffStore.diffInfo(i) := lsq.io.diffStore.diffInfo(i)
        sbuffer.io.diffStore.pmaStore(i) := lsq.io.diffStore.pmaStore(i)
      }
      sbuffer.io.diffStore.ncStore := lsq.io.diffStore.ncStore
      io.mem_to_ooo.storeDebugInfo(i).robidx := sbuffer.io.diffStore.diffInfo(i).uop.robIdx
      sbuffer.io.diffStore.diffInfo(i).uop.pc := io.mem_to_ooo.storeDebugInfo(i).pc
    }
  }

  // lsq.io.vecStoreRetire <> vsFlowQueue.io.sqRelease
  // lsq.io.vecWriteback.valid := vlWrapper.io.uopWriteback.fire &&
  //   vlWrapper.io.uopWriteback.bits.uop.vpu.lastUop
  // lsq.io.vecWriteback.bits := vlWrapper.io.uopWriteback.bits

  // vector
  val vLoadCanAccept  = (0 until VlduCnt).map(i =>
    vlSplit(i).io.in.ready && VlduType.isVecLd(issueVldu(i).bits.fuOpType)
  )
  val vStoreCanAccept = (0 until VstuCnt).map(i =>
    vsSplit(i).io.in.ready && VstuType.isVecSt(issueVldu(i).bits.fuOpType)
  )
  val isSegment     = issueVldu.head.valid && isVsegls(issueVldu.head.bits.fuType)
  val isFixVlUop    = issueVldu.map{ x =>
    x.valid && x.bits.vpu.get.isVleff && x.bits.vpu.get.lastUop
  }

  // init port
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
  (0 until VstuCnt).foreach{i =>
    vsMergeBuffer(i).io.fromPipeline := DontCare
    vsMergeBuffer(i).io.fromSplit := DontCare

//    vsMergeBuffer(i).io.fromMisalignBuffer.get.flush := storeMisalignBuffer.io.toVecStoreMergeBuffer(i).flush
//    vsMergeBuffer(i).io.fromMisalignBuffer.get.mbIndex := storeMisalignBuffer.io.toVecStoreMergeBuffer(i).mbIndex
  }

  (0 until VstuCnt).foreach{i =>
    vsSplit(i).io.redirect <> redirect
    vsSplit(i).io.in <> issueVldu(i)
    vsSplit(i).io.in.valid := issueVldu(i).valid &&
                              vStoreCanAccept(i) && !isSegment
    vsSplit(i).io.toMergeBuffer <> vsMergeBuffer(i).io.fromSplit.head
    NewPipelineConnect(
      vsSplit(i).io.out, storeUnits(i).io.vecstin, storeUnits(i).io.vecstin.fire,
      Mux(vsSplit(i).io.out.fire, vsSplit(i).io.out.bits.uop.robIdx.needFlush(io.redirect), storeUnits(i).io.vecstin.bits.uop.robIdx.needFlush(io.redirect)),
      Option("VsSplitConnectStu")
    )
    vsSplit(i).io.vstd.get := DontCare // Todo: Discuss how to pass vector store data

    vsSplit(i).io.vstdMisalign.get.storeMisalignBufferEmpty  := storeMisalignBuffer.io.toVecSplit.empty
    vsSplit(i).io.vstdMisalign.get.storeMisalignBufferRobIdx := storeMisalignBuffer.io.toVecSplit.robIdx
    vsSplit(i).io.vstdMisalign.get.storeMisalignBufferUopIdx := storeMisalignBuffer.io.toVecSplit.uopIdx
    vsSplit(i).io.vstdMisalign.get.storePipeEmpty := !storeUnits.map(_.io.s0_s1_s2_valid).reduce(_||_)

  }
  (0 until VlduCnt).foreach{i =>
    vlSplit(i).io.redirect <> redirect
    vlSplit(i).io.in <> issueVldu(i)
    vlSplit(i).io.in.valid := issueVldu(i).valid &&
                              vLoadCanAccept(i) && !isSegment && !isFixVlUop(i)
    vlSplit(i).io.toMergeBuffer <> vlMergeBuffer.io.fromSplit(i)
    vlSplit(i).io.threshold.get.valid := vlMergeBuffer.io.toSplit.get.threshold
    vlSplit(i).io.threshold.get.bits  := lsq.io.lqDeqPtr
    NewPipelineConnect(
      vlSplit(i).io.out, loadUnits(i).io.vecldin, loadUnits(i).io.vecldin.fire,
      Mux(vlSplit(i).io.out.fire, vlSplit(i).io.out.bits.uop.robIdx.needFlush(io.redirect), loadUnits(i).io.vecldin.bits.uop.robIdx.needFlush(io.redirect)),
      Option("VlSplitConnectLdu")
    )

    //Subsequent instrction will be blocked
    vfofBuffer.io.in(i).valid := issueVldu(i).valid
    vfofBuffer.io.in(i).bits  := issueVldu(i).bits
  }
  (0 until LduCnt).foreach{i=>
    loadUnits(i).io.vecldout.ready         := vlMergeBuffer.io.fromPipeline(i).ready
    loadMisalignBuffer.io.vecWriteBack.ready := true.B

    if (i == MisalignWBPort) {
      when(loadUnits(i).io.vecldout.valid) {
        vlMergeBuffer.io.fromPipeline(i).valid := loadUnits(i).io.vecldout.valid
        vlMergeBuffer.io.fromPipeline(i).bits  := loadUnits(i).io.vecldout.bits
      } .otherwise {
        vlMergeBuffer.io.fromPipeline(i).valid   := loadMisalignBuffer.io.vecWriteBack.valid
        vlMergeBuffer.io.fromPipeline(i).bits    := loadMisalignBuffer.io.vecWriteBack.bits
      }
    } else {
      vlMergeBuffer.io.fromPipeline(i).valid := loadUnits(i).io.vecldout.valid
      vlMergeBuffer.io.fromPipeline(i).bits  := loadUnits(i).io.vecldout.bits
    }
  }

  (0 until StaCnt).foreach{i=>
    if(i < VstuCnt){
      arbiter(
        Seq(storeUnits(i).io.vecstout, storeMisalignBuffer.io.vecWriteBack(i)),
        vsMergeBuffer(i).io.fromPipeline.head,
        Some(s"vecstout_$i")
      )
    }
  }

  (0 until VlduCnt).foreach{i=>
    issueVldu(i).ready := vLoadCanAccept(i) || vStoreCanAccept(i)
  }

  vlMergeBuffer.io.redirect <> redirect
  vsMergeBuffer.map(_.io.redirect <> redirect)
  (0 until VlduCnt).foreach{i=>
    vlMergeBuffer.io.toLsq(i) <> lsq.io.ldvecFeedback(i)
  }
  (0 until VstuCnt).foreach{i=>
    vsMergeBuffer(i).io.toLsq.head <> lsq.io.stvecFeedback(i)
  }

  (0 until VlduCnt).foreach{i=>
    // send to RS
    vlMergeBuffer.io.feedback(i) <> io.mem_to_ooo.vlduIqFeedback(i).feedbackSlow
    io.mem_to_ooo.vlduIqFeedback(i).feedbackFast := DontCare
  }
  (0 until VstuCnt).foreach{i =>
    // send to RS
    if (i == 0){
      io.mem_to_ooo.vstuIqFeedback(i).feedbackSlow.valid := vsMergeBuffer(i).io.feedback.head.valid || vSegmentUnit.io.feedback.valid
      io.mem_to_ooo.vstuIqFeedback(i).feedbackSlow.bits := Mux1H(Seq(
        vSegmentUnit.io.feedback.valid -> vSegmentUnit.io.feedback.bits,
        vsMergeBuffer(i).io.feedback.head.valid ->  vsMergeBuffer(i).io.feedback.head.bits
      ))
      io.mem_to_ooo.vstuIqFeedback(i).feedbackFast := DontCare
    } else {
      vsMergeBuffer(i).io.feedback.head <> io.mem_to_ooo.vstuIqFeedback(i).feedbackSlow
      io.mem_to_ooo.vstuIqFeedback(i).feedbackFast := DontCare
    }
  }

  writebackVldu.zipWithIndex.foreach{ case (wb, i) =>
    if (i == 0){
      arbiter(
        Seq(vSegmentUnit.io.uopwriteback, vlMergeBuffer.io.uopWriteback(i), vsMergeBuffer(i).io.uopWriteback.head),
        wb, Some(s"writebackVldu_$i")
      )
    } else if (i == 1) {
      arbiter(
        Seq(vfofBuffer.io.uopWriteback, vlMergeBuffer.io.uopWriteback(i), vsMergeBuffer(i).io.uopWriteback.head),
        wb, Some(s"writebackVldu_$i")
      )
    } else {
      arbiter(
        Seq(vlMergeBuffer.io.uopWriteback(i), vsMergeBuffer(i).io.uopWriteback.head),
        wb, Some(s"writebackVldu_$i")
      )
    }

    vfofBuffer.io.mergeUopWriteback(i).valid := vlMergeBuffer.io.toLsq(i).valid
    vfofBuffer.io.mergeUopWriteback(i).bits  := vlMergeBuffer.io.toLsq(i).bits
  }


  vfofBuffer.io.redirect <> redirect

  // Sbuffer
  sbuffer.io.csrCtrl    <> csrCtrl
  sbuffer.io.dcache     <> dcache.io.lsu.store
  sbuffer.io.memSetPattenDetected := dcache.io.memSetPattenDetected
  sbuffer.io.force_write <> lsq.io.force_write
  // flush sbuffer
  val cmoFlush = lsq.io.flushSbuffer.valid
  val fenceFlush = io.ooo_to_mem.flushSb
  val atomicsFlush = atomicsUnit.io.flush_sbuffer.valid || vSegmentUnit.io.flush_sbuffer.valid
  val stIsEmpty = sbuffer.io.flush.empty && uncache.io.flush.empty
  io.mem_to_ooo.sbIsEmpty := RegNext(stIsEmpty)

  // if both of them tries to flush sbuffer at the same time
  // something must have gone wrong
  assert(!(fenceFlush && atomicsFlush && cmoFlush))
  sbuffer.io.flush.valid := RegNext(fenceFlush || atomicsFlush || cmoFlush)
  uncache.io.flush.valid := sbuffer.io.flush.valid

  // AtomicsUnit: AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val s_normal +: s_atomics = Enum(StaCnt + 1)
  val state = RegInit(s_normal)

  val st_atomics = Seq.tabulate(StaCnt)(i =>
    issueSta(i).valid && FuType.storeIsAMO((issueSta(i).bits.fuType))
  )

  for (i <- 0 until StaCnt) when(st_atomics(i)) {
    issueSta(i).ready := atomicsUnit.io.in.ready
    storeUnits(i).io.stin.valid := false.B

    state := s_atomics(i)
  }
  when (atomicsUnit.io.out.valid) {
    state := s_normal
  }

  atomicsUnit.io.in.valid := st_atomics.reduce(_ || _)
  atomicsUnit.io.in.bits := Mux1H(st_atomics, issueSta.map(_.bits))
  atomicsUnit.io.storeDataIn.zipWithIndex.foreach { case (stdin, i) =>
    stdin := stdExeUnits(i).io.atomicData
  }
  atomicsUnit.io.redirect <> redirect

  // TODO: complete amo's pmp support
  val amoTlb = dtlb_ld(0).requestor(0)
  atomicsUnit.io.dtlb.resp.valid := false.B
  atomicsUnit.io.dtlb.resp.bits  := DontCare
  atomicsUnit.io.dtlb.req.ready  := amoTlb.req.ready
  atomicsUnit.io.pmpResp := pmp_check(0).resp

  atomicsUnit.io.dcache <> dcache.io.lsu.atomics
  atomicsUnit.io.flush_sbuffer.empty := stIsEmpty

  atomicsUnit.io.csrCtrl := csrCtrl

  // for atomicsUnit, it uses loadUnit(0)'s TLB port

  when (state =/= s_normal) {
    // use store wb port instead of load
    loadUnits(0).io.ldout.ready := false.B
    // use load_0's TLB
    atomicsUnit.io.dtlb <> amoTlb

    // hw prefetch should be disabled while executing atomic insts
    loadUnits.map(i => i.io.prefetch_req.valid := false.B)

    // make sure there's no in-flight uops in load unit
    assert(!loadUnits(0).io.ldout.valid)
  }

  lsq.io.flushSbuffer.empty := sbuffer.io.sbempty

  for (i <- 0 until StaCnt) {
    when (state === s_atomics(i)) {
      io.mem_to_ooo.staIqFeedback(i).feedbackSlow := atomicsUnit.io.feedbackSlow
      assert(!storeUnits(i).io.feedback_slow.valid)
    }
  }

  lsq.io.exceptionAddr.isStore := io.ooo_to_mem.isStoreException
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

  val exceptionVaddr = Mux(
    atomicsException,
    atomicsExceptionAddress,
    Mux(misalignBufExceptionOverwrite,
      misalignBufExceptionVaddr,
      Mux(vSegmentException,
        vSegmentExceptionAddress,
        lsq.io.exceptionAddr.vaddr
      )
    )
  )
  // whether vaddr need ext or is hyper inst:
  // VaNeedExt: atomicsException -> false; misalignBufExceptionOverwrite -> true; vSegmentException -> false
  // IsHyper: atomicsException -> false; vSegmentException -> false
  val exceptionVaNeedExt = !atomicsException &&
    (misalignBufExceptionOverwrite ||
      (!vSegmentException && lsq.io.exceptionAddr.vaNeedExt))
  val exceptionIsHyper = !atomicsException &&
    (misalignBufExceptionOverwrite && misalignBufExceptionIsHyper ||
      (!vSegmentException && lsq.io.exceptionAddr.isHyper && !misalignBufExceptionOverwrite))

  def GenExceptionVa(
    mode: UInt, isVirt: Bool, vaNeedExt: Bool,
    satp: TlbSatpBundle, vsatp: TlbSatpBundle, hgatp: TlbHgatpBundle,
    vaddr: UInt
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

    val ExceptionVa = Wire(UInt(XLEN.W))
    when (vaNeedExt) {
      ExceptionVa := Mux1H(Seq(
        (useBareAddr)   -> bareAddr,
        (useSv39Addr)   -> sv39Addr,
        (useSv48Addr)   -> sv48Addr,
        (useSv39x4Addr) -> sv39x4Addr,
        (useSv48x4Addr) -> sv48x4Addr,
      ))
    } .otherwise {
      ExceptionVa := vaddr
    }

    ExceptionVa
  }

  io.mem_to_ooo.lsqio.vaddr := RegNext(
    GenExceptionVa(tlbcsr.priv.dmode, tlbcsr.priv.virt || exceptionIsHyper, exceptionVaNeedExt,
    tlbcsr.satp, tlbcsr.vsatp, tlbcsr.hgatp, exceptionVaddr)
  )

  // vsegment instruction is executed atomic, which mean atomicsException and vSegmentException should not raise at the same time.
  XSError(atomicsException && vSegmentException, "atomicsException and vSegmentException raise at the same time!")
  io.mem_to_ooo.lsqio.vstart := RegNext(Mux(vSegmentException,
                                            vSegmentExceptionVstart,
                                            lsq.io.exceptionAddr.vstart)
  )
  io.mem_to_ooo.lsqio.vl     := RegNext(Mux(vSegmentException,
                                            vSegmentExceptionVl,
                                            lsq.io.exceptionAddr.vl)
  )

  XSError(atomicsException && atomicsUnit.io.in.valid, "new instruction before exception triggers\n")
  io.mem_to_ooo.lsqio.gpaddr := RegNext(Mux(
    atomicsException,
    atomicsExceptionGPAddress,
    Mux(misalignBufExceptionOverwrite,
      misalignBufExceptionGpaddr,
      Mux(vSegmentException,
        vSegmentExceptionGPAddress,
        lsq.io.exceptionAddr.gpaddr
      )
    )
  ))
  io.mem_to_ooo.lsqio.isForVSnonLeafPTE := RegNext(Mux(
    atomicsException,
    atomicsExceptionIsForVSnonLeafPTE,
    Mux(misalignBufExceptionOverwrite,
      misalignBufExceptionIsForVSnonLeafPTE,
      Mux(vSegmentException,
        vSegmentExceptionIsForVSnonLeafPTE,
        lsq.io.exceptionAddr.isForVSnonLeafPTE
      )
    )
  ))
  io.mem_to_ooo.topToBackendBypass match { case x =>
    x.hartId            := io.hartId
    x.l2FlushDone       := RegNext(io.l2_flush_done)
    x.externalInterrupt.msip  := RegNext(outer.clint_int_sink.in.head._1(0))
    x.externalInterrupt.mtip  := RegNext(outer.clint_int_sink.in.head._1(1))
    x.externalInterrupt.meip  := RegNext(outer.plic_int_sink.in.head._1(0))
    x.externalInterrupt.seip  := RegNext(outer.plic_int_sink.in.last._1(0))
    x.externalInterrupt.debug := RegNext(outer.debug_int_sink.in.head._1(0))
    x.externalInterrupt.nmi.nmi_31 := RegNext(outer.nmi_int_sink.in.head._1(0) | outer.beu_local_int_sink.in.head._1(0))
    x.externalInterrupt.nmi.nmi_43 := RegNext(outer.nmi_int_sink.in.head._1(1))
    x.msiInfo           := DelayNWithValid(io.fromTopToBackend.msiInfo, 1)
    x.clintTime         := DelayNWithValid(io.fromTopToBackend.clintTime, 1)
  }

  io.memInfo.sqFull := RegNext(lsq.io.sqFull)
  io.memInfo.lqFull := RegNext(lsq.io.lqFull)
  io.memInfo.dcacheMSHRFull := RegNext(dcache.io.mshrFull)

  io.inner_hartId := io.hartId
  io.inner_reset_vector := RegNext(io.outer_reset_vector)
  io.outer_cpu_halt := RegNext(io.ooo_to_mem.backendToTopBypass.cpuHalted)
  io.outer_l2_flush_en := io.ooo_to_mem.csrCtrl.flush_l2_enable
  io.outer_power_down_en := io.ooo_to_mem.csrCtrl.power_down_enable
  io.outer_cpu_critical_error := RegNext(io.ooo_to_mem.backendToTopBypass.cpuCriticalError)
  io.outer_msi_ack := io.ooo_to_mem.backendToTopBypass.msiAck
  io.outer_beu_errors_icache := RegNext(io.inner_beu_errors_icache)
  io.inner_hc_perfEvents <> RegNext(io.outer_hc_perfEvents)
  io.outer_l2PfCtrl := DelayN(io.ooo_to_mem.csrCtrl.pf_ctrl.toL2PrefetchCtrl(), 2)

  // vector segmentUnit
  // TODO: DONT use `head` find segment
  vSegmentUnit.io.in.bits <> issueVldu.head.bits
  vSegmentUnit.io.csrCtrl <> csrCtrl
  vSegmentUnit.io.in.valid := isSegment && issueVldu.head.valid // is segment instruction
  vSegmentUnit.io.dtlb.resp.bits <> dtlb_reqs.take(LduCnt).head.resp.bits
  vSegmentUnit.io.dtlb.resp.valid <> dtlb_reqs.take(LduCnt).head.resp.valid
  vSegmentUnit.io.pmpResp <> pmp_check.head.resp
  vSegmentUnit.io.flush_sbuffer.empty := stIsEmpty
  vSegmentUnit.io.redirect <> redirect
  vSegmentUnit.io.rdcache.resp.bits := dcache.io.lsu.load(0).resp.bits
  vSegmentUnit.io.rdcache.resp.valid := dcache.io.lsu.load(0).resp.valid
  vSegmentUnit.io.rdcache.s2_bank_conflict := dcache.io.lsu.load(0).s2_bank_conflict
  // -------------------------
  // Vector Segment Triggers
  // -------------------------
  vSegmentUnit.io.fromCsrTrigger.tdataVec := tdata
  vSegmentUnit.io.fromCsrTrigger.tEnableVec := tEnable
  vSegmentUnit.io.fromCsrTrigger.triggerCanRaiseBpExp := triggerCanRaiseBpExp
  vSegmentUnit.io.fromCsrTrigger.debugMode := debugMode

  // reset tree of MemBlock
  if (p(DebugOptionsKey).ResetGen) {
    val leftResetTree = ResetGenNode(
      Seq(
        ModuleNode(ptw),
        ModuleNode(ptw_to_l2_buffer),
        ModuleNode(lsq),
        ModuleNode(dtlb_st_tlb_st),
        ModuleNode(dtlb_prefetch_tlb_prefetch),
        ModuleNode(pmp)
      )
      ++ pmp_checkers.map(ModuleNode(_))
      ++ Seq(ModuleNode(prefetcher))
    )
    val rightResetTree = ResetGenNode(
      Seq(
        ModuleNode(sbuffer),
        ModuleNode(dtlb_ld_tlb_ld),
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
  // trace interface
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
  traceToL2Top.toEncoder.mstatus := RegNext(traceFromBackend.toEncoder.mstatus)
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

  // top-down info
  dcache.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  dtlbRepeater.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  lsq.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  io.debugTopDown.toCore.robHeadMissInDCache := dcache.io.debugTopDown.robHeadMissInDCache
  io.debugTopDown.toCore.robHeadTlbReplay := lsq.io.debugTopDown.robHeadTlbReplay
  io.debugTopDown.toCore.robHeadTlbMiss := lsq.io.debugTopDown.robHeadTlbMiss
  io.debugTopDown.toCore.robHeadLoadVio := lsq.io.debugTopDown.robHeadLoadVio
  io.debugTopDown.toCore.robHeadLoadMSHR := lsq.io.debugTopDown.robHeadLoadMSHR
  dcache.io.debugTopDown.robHeadOtherReplay := lsq.io.debugTopDown.robHeadOtherReplay
  dcache.io.debugRolling := io.debugRolling

  lsq.io.noUopsIssued := io.topDownInfo.toBackend.noUopsIssued
  io.topDownInfo.toBackend.lqEmpty := lsq.io.lqEmpty
  io.topDownInfo.toBackend.sqEmpty := lsq.io.sqEmpty
  io.topDownInfo.toBackend.l1Miss := dcache.io.l1Miss
  io.topDownInfo.toBackend.l2TopMiss.l2Miss := RegNext(io.topDownInfo.fromL2Top.l2Miss)
  io.topDownInfo.toBackend.l2TopMiss.l3Miss := RegNext(io.topDownInfo.fromL2Top.l3Miss)

  val ldDeqCount = PopCount(issueLda.map(_.valid))
  val stDeqCount = PopCount(issueSta.take(StaCnt).map(_.valid))
  val iqDeqCount = ldDeqCount +& stDeqCount
  XSPerfAccumulate("load_iq_deq_count", ldDeqCount)
  XSPerfHistogram("load_iq_deq_count", ldDeqCount, true.B, 0, LdExuCnt + 1)
  XSPerfAccumulate("store_iq_deq_count", stDeqCount)
  XSPerfHistogram("store_iq_deq_count", stDeqCount, true.B, 0, StAddrCnt + 1)
  XSPerfAccumulate("ls_iq_deq_count", iqDeqCount)

  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := csrCtrl.distribute_csr
  val csrevents = pfevent.io.hpmevent.slice(16,24)

  val perfFromUnits = (loadUnits ++ Seq(sbuffer, lsq, dcache)).flatMap(_.getPerfEvents)
  val perfFromTLB = perfEventsDTLBld ++ perfEventsDTLBst
  val perfFromPTW = perfEventsPTW.map(x => ("PTW_" + x._1, x._2))
  val perfBlock     = Seq(("ldDeqCount", ldDeqCount),
                          ("stDeqCount", stDeqCount))
  // let index = 0 be no event
  val allPerfEvents = Seq(("noEvent", 0.U)) ++ perfFromUnits ++ perfFromTLB ++ perfFromPTW ++ perfBlock

  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("MemBlock perfEvents Set", name, inc, i)
    }
  }

  val allPerfInc = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  val perfEvents = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
  generatePerfEvent()

  private val mbistPl = MbistPipeline.PlaceMbistPipeline(Int.MaxValue, "MbistPipeMemBlk", hasMbist)
  private val mbistIntf = if(hasMbist) {
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
  } else {
    None
  }
  private val sigFromSrams = if (hasDFT) Some(SramHelper.genBroadCastBundleTop()) else None
  private val cg = ClockGate.genTeSrc
  dontTouch(cg)

  if (hasMbist) {
    cg.cgen := io.dft.get.cgen
  } else {
    cg.cgen := false.B
  }

  // sram debug
  sigFromSrams.foreach({ case sig => sig := DontCare })
  sigFromSrams.zip(io.dft).foreach {
    case (sig, dft) =>
      if (hasMbist) {
        sig.ram_hold := dft.ram_hold
        sig.ram_bypass := dft.ram_bypass
        sig.ram_bp_clken := dft.ram_bp_clken
        sig.ram_aux_clk := dft.ram_aux_clk
        sig.ram_aux_ckbp := dft.ram_aux_ckbp
        sig.ram_mcp_hold := dft.ram_mcp_hold
        sig.cgen := dft.cgen
      }
      if (hasSramCtl) {
        sig.ram_ctl := dft.ram_ctl
      }
  }
  io.dft_frnt.zip(sigFromSrams).foreach({ case (a, b) => a := b })
  io.dft_reset_frnt.zip(io.dft_reset).foreach({ case (a, b) => a := b })
  io.dft_bcknd.zip(sigFromSrams).foreach({ case (a, b) => a := b })
  io.dft_reset_bcknd.zip(io.dft_reset).foreach({ case (a, b) => a := b })
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
