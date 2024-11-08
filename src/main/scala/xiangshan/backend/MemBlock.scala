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

package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.tile.HasFPUParameters
import freechips.rocketchip.tilelink._
import coupledL2.{PrefetchRecv, CMOReq, CMOResp}
import device.MsiInfoBundle
import utils._
import utility._
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.ctrlblock.{DebugLSIO, LsTopdownInfo}
import xiangshan.backend.exu.MemExeUnit
import xiangshan.backend.fu._
import xiangshan.backend.fu.FuType._
import xiangshan.backend.rob.{RobDebugRollingIO, RobPtr}
import xiangshan.backend.fu.util.{HasCSRConst, SdtrigExt}
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem._
import xiangshan.mem.mdp._
import xiangshan.frontend.HasInstrMMIOConst
import xiangshan.mem.prefetch.{BasePrefecher, L1Prefetcher, SMSParams, SMSPrefetcher}
import xiangshan.backend.datapath.NewPipelineConnect
import system.SoCParamsKey
import xiangshan.backend.fu.NewCSR.TriggerUtil
import xiangshan.ExceptionNO._

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

  val loadFastMatch = Vec(LdExuCnt, Input(UInt(LdExuCnt.W)))
  val loadFastFuOpType = Vec(LdExuCnt, Input(FuOpType()))
  val loadFastImm = Vec(LdExuCnt, Input(UInt(12.W)))
  val sfence = Input(new SfenceBundle)
  val tlbCsr = Input(new TlbCsrBundle)
  val lsqio = new Bundle {
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val scommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val pendingUncacheld = Input(Bool())
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

  val loadPc = Vec(LduCnt, Input(UInt(VAddrBits.W))) // for hw prefetch
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
class InstrUncacheBuffer()(implicit p: Parameters) extends LazyModule with HasInstrMMIOConst {
  val node = new TLBufferNode(BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default)
  lazy val module = new InstrUncacheBufferImpl

  class InstrUncacheBufferImpl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.a <> BufferParams.default(BufferParams.default(in.a))
      in.d <> BufferParams.default(BufferParams.default(out.d))

      // only a.valid, a.ready, a.address can change
      // hoping that the rest would be optimized to keep MemBlock port unchanged after adding buffer
      out.a.bits.data := 0.U
      out.a.bits.mask := Fill(mmioBusBytes, 1.U(1.W))
      out.a.bits.opcode := 4.U // Get
      out.a.bits.size := log2Ceil(mmioBusBytes).U
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

// Frontend bus goes through MemBlock
class FrontendBridge()(implicit p: Parameters) extends LazyModule {
  val icache_node = LazyModule(new ICacheBuffer()).suggestName("icache").node// to keep IO port name
  val instr_uncache_node = LazyModule(new InstrUncacheBuffer()).suggestName("instr_uncache").node
  lazy val module = new LazyModuleImp(this) {
  }
}

class MemBlockInlined()(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  override def shouldBeInlined: Boolean = true

  val dcache = LazyModule(new DCacheWrapper())
  val uncache = LazyModule(new Uncache())
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
  val cmo_sender  = if (HasCMO) Some(BundleBridgeSource(() => DecoupledIO(new CMOReq))) else None
  val cmo_reciver = if (HasCMO) Some(BundleBridgeSink(Some(() => DecoupledIO(new CMOResp)))) else None
  val frontendBridge = LazyModule(new FrontendBridge)
  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val debug_int_sink = IntSinkNode(IntSinkPortSimple(1, 1))
  val plic_int_sink = IntSinkNode(IntSinkPortSimple(2, 1))
  val nmi_int_sink = IntSinkNode(IntSinkPortSimple(1, (new NonmaskableInterruptIO).elements.size))

  if (!coreParams.softPTW) {
    ptw_to_l2_buffer.node := ptw.node
  }

  lazy val module = new MemBlockInlinedImp(this)
}

class MemBlockInlinedImp(outer: MemBlockInlined) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasFPUParameters
  with HasPerfEvents
  with HasL1PrefetchSourceParameter
  with HasCircularQueuePtrHelper
  with HasMemBlockParameters
  with HasTlbConst
  with HasCSRConst
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

    val debugTopDown = new Bundle {
      val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
      val toCore = new MemCoreTopDownIO
    }
    val debugRolling = Flipped(new RobDebugRollingIO)

    // All the signals from/to frontend/backend to/from bus will go through MemBlock
    val fromTopToBackend = Input(new Bundle {
      val msiInfo   = ValidIO(new MsiInfoBundle)
      val clintTime = ValidIO(UInt(64.W))
    })
    val inner_hartId = Output(UInt(hartIdLen.W))
    val inner_reset_vector = Output(UInt(PAddrBits.W))
    val outer_reset_vector = Input(UInt(PAddrBits.W))
    val outer_cpu_halt = Output(Bool())
    val outer_cpu_critical_error = Output(Bool())
    val inner_beu_errors_icache = Input(new L1BusErrorUnitInfo)
    val outer_beu_errors_icache = Output(new L1BusErrorUnitInfo)
    val inner_l2_pf_enable = Input(Bool())
    val outer_l2_pf_enable = Output(Bool())
    val inner_hc_perfEvents = Output(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))
    val outer_hc_perfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))

    // reset signals of frontend & backend are generated in memblock
    val reset_backend = Output(Reset())
    // Reset singal from frontend.
    val resetInFrontendBypass = new Bundle{
      val fromFrontend = Input(Bool())
      val toL2Top      = Output(Bool())
    }
  })

  dontTouch(io.inner_hartId)
  dontTouch(io.inner_reset_vector)
  dontTouch(io.outer_reset_vector)
  dontTouch(io.outer_cpu_halt)
  dontTouch(io.outer_cpu_critical_error)
  dontTouch(io.inner_beu_errors_icache)
  dontTouch(io.outer_beu_errors_icache)
  dontTouch(io.inner_l2_pf_enable)
  dontTouch(io.outer_l2_pf_enable)
  dontTouch(io.inner_hc_perfEvents)
  dontTouch(io.outer_hc_perfEvents)

  val redirect = RegNextWithEnable(io.redirect)

  private val dcache = outer.dcache.module
  val uncache = outer.uncache.module

  //val delayedDcacheRefill = RegNext(dcache.io.lsu.lsq)

  val csrCtrl = DelayN(io.ooo_to_mem.csrCtrl, 2)
  dcache.io.csr.distribute_csr <> csrCtrl.distribute_csr
  dcache.io.l2_pf_store_only := RegNext(io.ooo_to_mem.csrCtrl.l2_pf_store_only, false.B)
  io.error <> DelayNWithValid(dcache.io.error, 2)
  when(!csrCtrl.cache_error_enable){
    io.error.bits.report_to_beu := false.B
    io.error.valid := false.B
  }

  val loadUnits = Seq.fill(LduCnt)(Module(new LoadUnit))
  val storeUnits = Seq.fill(StaCnt)(Module(new StoreUnit))
  val stdExeUnits = Seq.fill(StdCnt)(Module(new MemExeUnit(backendParams.memSchdParams.get.issueBlockParams.find(_.StdCnt != 0).get.exuBlockParams.head)))
  val hybridUnits = Seq.fill(HyuCnt)(Module(new HybridUnit)) // Todo: replace it with HybridUnit
  val stData = stdExeUnits.map(_.io.out)
  val exeUnits = loadUnits ++ storeUnits
  // val vlWrapper = Module(new VectorLoadWrapper)
  // val vsUopQueue = Module(new VsUopQueue)
  // val vsFlowQueue = Module(new VsFlowQueue)

  // The number of vector load/store units is decoupled with the number of load/store units
  val vlSplit = Seq.fill(VlduCnt)(Module(new VLSplitImp))
  val vsSplit = Seq.fill(VstuCnt)(Module(new VSSplitImp))
  val vlMergeBuffer = Module(new VLMergeBufferImp)
  val vsMergeBuffer = Seq.fill(VstuCnt)(Module(new VSMergeBufferImp))
  val vSegmentUnit  = Module(new VSegmentUnit)
  val vfofBuffer    = Module(new VfofBuffer)

  // misalign Buffer
  val loadMisalignBuffer = Module(new LoadMisalignBuffer)
  val storeMisalignBuffer = Module(new StoreMisalignBuffer)

  val l1_pf_req = Wire(Decoupled(new L1PrefetchReq()))
  dcache.io.sms_agt_evict_req.ready := false.B
  val prefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
    case _: SMSParams =>
      val sms = Module(new SMSPrefetcher())
      sms.io_agt_en := GatedRegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable_agt, 2, Some(false.B))
      sms.io_pht_en := GatedRegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable_pht, 2, Some(false.B))
      sms.io_act_threshold := GatedRegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_active_threshold, 2, Some(12.U))
      sms.io_act_stride := GatedRegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_active_stride, 2, Some(30.U))
      sms.io_stride_en := false.B
      sms.io_dcache_evict <> dcache.io.sms_agt_evict_req
      sms
  }
  prefetcherOpt.foreach{ pf => pf.io.l1_req.ready := false.B }
  val hartId = p(XSCoreParamsKey).HartId
  val l1PrefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
    case _ =>
      val l1Prefetcher = Module(new L1Prefetcher())
      l1Prefetcher.io.enable := Constantin.createRecord(s"enableL1StreamPrefetcher$hartId", initValue = true)
      l1Prefetcher.pf_ctrl <> dcache.io.pf_ctrl
      l1Prefetcher.l2PfqBusy := io.l2PfqBusy

      // stride will train on miss or prefetch hit
      for (i <- 0 until LduCnt) {
        val source = loadUnits(i).io.prefetch_train_l1
        l1Prefetcher.stride_train(i).valid := source.valid && source.bits.isFirstIssue && (
          source.bits.miss || isFromStride(source.bits.meta_prefetch)
        )
        l1Prefetcher.stride_train(i).bits := source.bits
        l1Prefetcher.stride_train(i).bits.uop.pc := Mux(
          loadUnits(i).io.s2_ptr_chasing,
          RegEnable(io.ooo_to_mem.loadPc(i), loadUnits(i).io.s2_prefetch_spec),
          RegEnable(RegEnable(io.ooo_to_mem.loadPc(i), loadUnits(i).io.s1_prefetch_spec), loadUnits(i).io.s2_prefetch_spec)
        )
      }
      for (i <- 0 until HyuCnt) {
        val source = hybridUnits(i).io.prefetch_train_l1
        l1Prefetcher.stride_train.drop(LduCnt)(i).valid := source.valid && source.bits.isFirstIssue && (
          source.bits.miss || isFromStride(source.bits.meta_prefetch)
        )
        l1Prefetcher.stride_train.drop(LduCnt)(i).bits := source.bits
        l1Prefetcher.stride_train.drop(LduCnt)(i).bits.uop.pc := Mux(
          hybridUnits(i).io.ldu_io.s2_ptr_chasing,
          RegNext(io.ooo_to_mem.hybridPc(i)),
          RegNext(RegNext(io.ooo_to_mem.hybridPc(i)))
        )
      }
      l1Prefetcher
  }
  // load prefetch to l1 Dcache
  l1PrefetcherOpt match {
    case Some(pf) => l1_pf_req <> Pipeline(in = pf.io.l1_req, depth = 1, pipe = false, name = Some("pf_queue_to_ldu_reg"))
    case None =>
      l1_pf_req.valid := false.B
      l1_pf_req.bits := DontCare
  }
  val pf_train_on_hit = RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_train_on_hit, 2, Some(true.B))

  loadUnits.zipWithIndex.map(x => x._1.suggestName("LoadUnit_"+x._2))
  storeUnits.zipWithIndex.map(x => x._1.suggestName("StoreUnit_"+x._2))
  hybridUnits.zipWithIndex.map(x => x._1.suggestName("HybridUnit_"+x._2))
  val atomicsUnit = Module(new AtomicsUnit)


  val ldaExeWbReqs = Wire(Vec(LduCnt, Decoupled(new MemExuOutput)))
  // atomicsUnit will overwrite the source from ldu if it is about to writeback
  val atomicWritebackOverride = Mux(
    atomicsUnit.io.out.valid,
    atomicsUnit.io.out.bits,
    loadUnits(AtomicWBPort).io.ldout.bits
  )
  ldaExeWbReqs(AtomicWBPort).valid := atomicsUnit.io.out.valid || loadUnits(AtomicWBPort).io.ldout.valid
  ldaExeWbReqs(AtomicWBPort).bits  := atomicWritebackOverride
  atomicsUnit.io.out.ready := ldaExeWbReqs(AtomicWBPort).ready
  loadUnits(AtomicWBPort).io.ldout.ready := ldaExeWbReqs(AtomicWBPort).ready

  // misalignBuffer will overwrite the source from ldu if it is about to writeback
  val misalignWritebackOverride = Mux(
     loadMisalignBuffer.io.writeBack.valid,
     loadMisalignBuffer.io.writeBack.bits,
     loadUnits(MisalignWBPort).io.ldout.bits
  )
  ldaExeWbReqs(MisalignWBPort).valid := loadMisalignBuffer.io.writeBack.valid || loadUnits(MisalignWBPort).io.ldout.valid
  ldaExeWbReqs(MisalignWBPort).bits  := misalignWritebackOverride
  loadMisalignBuffer.io.writeBack.ready := ldaExeWbReqs(MisalignWBPort).ready
  loadUnits(MisalignWBPort).io.ldout.ready := ldaExeWbReqs(MisalignWBPort).ready

  // loadUnit will overwrite the source from uncache if it is about to writeback
  ldaExeWbReqs(UncacheWBPort) <> loadUnits(UncacheWBPort).io.ldout
  io.mem_to_ooo.writebackLda <> ldaExeWbReqs
  io.mem_to_ooo.writebackSta <> storeUnits.map(_.io.stout)
  io.mem_to_ooo.writebackStd.zip(stdExeUnits).foreach {x =>
    x._1.bits  := x._2.io.out.bits
    x._1.valid := x._2.io.out.fire
  }
  io.mem_to_ooo.writebackHyuLda <> hybridUnits.map(_.io.ldout)
  io.mem_to_ooo.writebackHyuSta <> hybridUnits.map(_.io.stout)
  io.mem_to_ooo.otherFastWakeup := DontCare
  io.mem_to_ooo.otherFastWakeup.drop(HyuCnt).take(LduCnt).zip(loadUnits.map(_.io.fast_uop)).foreach{case(a,b)=> a := b}
  io.mem_to_ooo.otherFastWakeup.take(HyuCnt).zip(hybridUnits.map(_.io.ldu_io.fast_uop)).foreach{case(a,b)=> a:=b}
  val stOut = io.mem_to_ooo.writebackSta ++ io.mem_to_ooo.writebackHyuSta

  // prefetch to l1 req
  // Stream's confidence is always 1
  // (LduCnt + HyuCnt) l1_pf_reqs ?
  loadUnits.foreach(load_unit => {
    load_unit.io.prefetch_req.valid <> l1_pf_req.valid
    load_unit.io.prefetch_req.bits <> l1_pf_req.bits
  })

  hybridUnits.foreach(hybrid_unit => {
    hybrid_unit.io.ldu_io.prefetch_req.valid <> l1_pf_req.valid
    hybrid_unit.io.ldu_io.prefetch_req.bits <> l1_pf_req.bits
  })

  // NOTE: loadUnits(0) has higher bank conflict and miss queue arb priority than loadUnits(1) and loadUnits(2)
  // when loadUnits(1)/loadUnits(2) stage 0 is busy, hw prefetch will never use that pipeline
  val LowConfPorts = if (LduCnt == 2) Seq(1) else if (LduCnt == 3) Seq(1, 2) else Seq(0)
  LowConfPorts.map{case i => loadUnits(i).io.prefetch_req.bits.confidence := 0.U}
  hybridUnits.foreach(hybrid_unit => { hybrid_unit.io.ldu_io.prefetch_req.bits.confidence := 0.U })

  val canAcceptHighConfPrefetch = loadUnits.map(_.io.canAcceptHighConfPrefetch) ++
                                  hybridUnits.map(_.io.canAcceptLowConfPrefetch)
  val canAcceptLowConfPrefetch = loadUnits.map(_.io.canAcceptLowConfPrefetch) ++
                                 hybridUnits.map(_.io.canAcceptLowConfPrefetch)
  l1_pf_req.ready := (0 until LduCnt + HyuCnt).map{
    case i => {
      if (LowConfPorts.contains(i)) {
        loadUnits(i).io.canAcceptLowConfPrefetch
      } else {
        Mux(l1_pf_req.bits.confidence === 1.U, canAcceptHighConfPrefetch(i), canAcceptLowConfPrefetch(i))
      }
    }
  }.reduce(_ || _)

  // l1 pf fuzzer interface
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

    // override hybrid_unit prefetch_req
    hybridUnits.foreach(hybrid_unit => {
      hybrid_unit.io.ldu_io.prefetch_req.valid <> fuzzer.io.req.valid
      hybrid_unit.io.ldu_io.prefetch_req.bits <> fuzzer.io.req.bits
    })

    fuzzer.io.req.ready := l1_pf_req.ready
  }

  // TODO: fast load wakeup
  val lsq     = Module(new LsqWrapper)
  val sbuffer = Module(new Sbuffer)
  // if you wants to stress test dcache store, use FakeSbuffer
  // val sbuffer = Module(new FakeSbuffer) // out of date now
  io.mem_to_ooo.stIssuePtr := lsq.io.issuePtrExt

  dcache.io.hartId := io.hartId
  lsq.io.hartId := io.hartId
  sbuffer.io.hartId := io.hartId
  atomicsUnit.io.hartId := io.hartId

  dcache.io.lqEmpty := lsq.io.lqEmpty

  // load/store prefetch to l2 cache
  prefetcherOpt.foreach(sms_pf => {
    l1PrefetcherOpt.foreach(l1_pf => {
      val sms_pf_to_l2 = DelayNWithValid(sms_pf.io.l2_req, 2)
      val l1_pf_to_l2 = DelayNWithValid(l1_pf.io.l2_req, 2)

      outer.l2_pf_sender_opt.get.out.head._1.addr_valid := sms_pf_to_l2.valid || l1_pf_to_l2.valid
      outer.l2_pf_sender_opt.get.out.head._1.addr := Mux(l1_pf_to_l2.valid, l1_pf_to_l2.bits.addr, sms_pf_to_l2.bits.addr)
      outer.l2_pf_sender_opt.get.out.head._1.pf_source := Mux(l1_pf_to_l2.valid, l1_pf_to_l2.bits.source, sms_pf_to_l2.bits.source)
      outer.l2_pf_sender_opt.get.out.head._1.l2_pf_en := RegNextN(io.ooo_to_mem.csrCtrl.l2_pf_enable, 2, Some(true.B))

      sms_pf.io.enable := RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable, 2, Some(false.B))

      val l2_trace = Wire(new LoadPfDbBundle)
      l2_trace.paddr := outer.l2_pf_sender_opt.get.out.head._1.addr
      val table = ChiselDB.createTable(s"L2PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
      table.log(l2_trace, l1_pf_to_l2.valid, "StreamPrefetchTrace", clock, reset)
      table.log(l2_trace, !l1_pf_to_l2.valid && sms_pf_to_l2.valid, "L2PrefetchTrace", clock, reset)

      val l1_pf_to_l3 = ValidIODelay(l1_pf.io.l3_req, 4)
      outer.l3_pf_sender_opt.foreach(_.out.head._1.addr_valid := l1_pf_to_l3.valid)
      outer.l3_pf_sender_opt.foreach(_.out.head._1.addr := l1_pf_to_l3.bits)
      outer.l3_pf_sender_opt.foreach(_.out.head._1.l2_pf_en := RegNextN(io.ooo_to_mem.csrCtrl.l2_pf_enable, 4, Some(true.B)))

      val l3_trace = Wire(new LoadPfDbBundle)
      l3_trace.paddr := outer.l3_pf_sender_opt.map(_.out.head._1.addr).getOrElse(0.U)
      val l3_table = ChiselDB.createTable(s"L3PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
      l3_table.log(l3_trace, l1_pf_to_l3.valid, "StreamPrefetchTrace", clock, reset)

      XSPerfAccumulate("prefetch_fire_l2", outer.l2_pf_sender_opt.get.out.head._1.addr_valid)
      XSPerfAccumulate("prefetch_fire_l3", outer.l3_pf_sender_opt.map(_.out.head._1.addr_valid).getOrElse(false.B))
      XSPerfAccumulate("l1pf_fire_l2", l1_pf_to_l2.valid)
      XSPerfAccumulate("sms_fire_l2", !l1_pf_to_l2.valid && sms_pf_to_l2.valid)
      XSPerfAccumulate("sms_block_by_l1pf", l1_pf_to_l2.valid && sms_pf_to_l2.valid)
    })
  })

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

  val perfEventsPTW = Wire(Vec(19, new PerfEvent))
  if (!coreParams.softPTW) {
    perfEventsPTW := ptw.getPerf
  } else {
    perfEventsPTW := DontCare
  }

  // dtlb
  val dtlb_ld_tlb_ld = Module(new TLBNonBlock(LduCnt + HyuCnt + 1, 2, ldtlbParams))
  val dtlb_st_tlb_st = Module(new TLBNonBlock(StaCnt, 1, sttlbParams))
  val dtlb_prefetch_tlb_prefetch = Module(new TLBNonBlock(2, 2, pftlbParams))
  val dtlb_ld = Seq(dtlb_ld_tlb_ld.io)
  val dtlb_st = Seq(dtlb_st_tlb_st.io)
  val dtlb_prefetch = Seq(dtlb_prefetch_tlb_prefetch.io)
  /* tlb vec && constant variable */
  val dtlb = dtlb_ld ++ dtlb_st ++ dtlb_prefetch
  val (dtlb_ld_idx, dtlb_st_idx, dtlb_pf_idx) = (0, 1, 2)
  val TlbSubSizeVec = Seq(LduCnt + HyuCnt + 1, StaCnt, 2) // (load + hyu + stream pf, store, sms+l2bop)
  val DTlbSize = TlbSubSizeVec.sum
  val TlbStartVec = TlbSubSizeVec.scanLeft(0)(_ + _).dropRight(1)
  val TlbEndVec = TlbSubSizeVec.scanLeft(0)(_ + _).drop(1)

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
      val replace_ld = Module(new TlbReplace(LduCnt + 1, ldtlbParams))
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
      val replace_pf = Module(new TlbReplace(2, pftlbParams))
      replace_pf.io.apply_sep(dtlb_prefetch.map(_.replace), ptwio.resp.bits.data.s1.entry.tag)
    }
  }

  val ptw_resp_next = RegEnable(ptwio.resp.bits, ptwio.resp.valid)
  val ptw_resp_v = RegNext(ptwio.resp.valid && !(sfence.valid && tlbcsr.satp.changed && tlbcsr.vsatp.changed && tlbcsr.hgatp.changed), init = false.B)
  ptwio.resp.ready := true.B

  val tlbreplay = WireInit(VecInit(Seq.fill(LdExuCnt)(false.B)))
  val tlbreplay_reg = GatedValidRegNext(tlbreplay)
  val dtlb_ld0_tlbreplay_reg = GatedValidRegNext(dtlb_ld(0).tlbreplay)

  if (backendParams.debugEn){ dontTouch(tlbreplay) }

  for (i <- 0 until LdExuCnt) {
    tlbreplay(i) := dtlb_ld(0).ptw.req(i).valid && ptw_resp_next.vector(0) && ptw_resp_v &&
      ptw_resp_next.data.hit(dtlb_ld(0).ptw.req(i).bits.vpn, tlbcsr.satp.asid, tlbcsr.vsatp.asid, tlbcsr.hgatp.vmid, allType = true, ignoreAsid = true)
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
    ptwio.req(i).valid := tlb.valid && !(ptw_resp_v && vector_hit && ptw_resp_next.data.hit(tlb.bits.vpn, tlbcsr.satp.asid, tlbcsr.vsatp.asid, tlbcsr.hgatp.vmid, allType = true, ignoreAsid = true))
  }
  dtlb.foreach(_.ptw.resp.bits := ptw_resp_next.data)
  if (refillBothTlb) {
    dtlb.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector).orR)
  } else {
    dtlb_ld.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.slice(TlbStartVec(dtlb_ld_idx), TlbEndVec(dtlb_ld_idx))).orR)
    dtlb_st.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.slice(TlbStartVec(dtlb_st_idx), TlbEndVec(dtlb_st_idx))).orR)
    dtlb_prefetch.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.slice(TlbStartVec(dtlb_pf_idx), TlbEndVec(dtlb_pf_idx))).orR)
  }
  dtlb_ld.foreach(_.ptw.resp.bits.getGpa := Cat(ptw_resp_next.getGpa.take(LduCnt + HyuCnt + 1)).orR)
  dtlb_st.foreach(_.ptw.resp.bits.getGpa := Cat(ptw_resp_next.getGpa.slice(LduCnt + HyuCnt + 1, LduCnt + HyuCnt + 1 + StaCnt)).orR)
  dtlb_prefetch.foreach(_.ptw.resp.bits.getGpa := Cat(ptw_resp_next.getGpa.drop(LduCnt + HyuCnt + 1 + StaCnt)).orR)

  val dtlbRepeater  = PTWNewFilter(ldtlbParams.fenceDelay, ptwio, ptw.io.tlb(1), sfence, tlbcsr, l2tlbParams.dfilterSize)
  val itlbRepeater3 = PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, io.fetch_to_mem.itlb, ptw.io.tlb(0), sfence, tlbcsr)

  lsq.io.debugTopDown.robHeadMissInDTlb := dtlbRepeater.io.rob_head_miss_in_tlb

  // pmp
  val pmp = Module(new PMP())
  pmp.io.distribute_csr <> csrCtrl.distribute_csr

  val pmp_checkers = Seq.fill(DTlbSize)(Module(new PMPChecker(4, leaveHitMux = true)))
  val pmp_check = pmp_checkers.map(_.io)
  for ((p,d) <- pmp_check zip dtlb_pmps) {
    p.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
    require(p.req.bits.size.getWidth == d.bits.size.getWidth)
  }

  for (i <- 0 until LduCnt) {
    io.debug_ls.debugLsInfo(i) := loadUnits(i).io.debug_ls
  }
  for (i <- 0 until HyuCnt) {
    io.debug_ls.debugLsInfo.drop(LduCnt)(i) := hybridUnits(i).io.ldu_io.debug_ls
  }
  for (i <- 0 until StaCnt) {
    io.debug_ls.debugLsInfo.drop(LduCnt + HyuCnt)(i) := storeUnits(i).io.debug_ls
  }
  for (i <- 0 until HyuCnt) {
    io.debug_ls.debugLsInfo.drop(LduCnt + HyuCnt + StaCnt)(i) := hybridUnits(i).io.stu_io.debug_ls
  }

  io.mem_to_ooo.lsTopdownInfo := loadUnits.map(_.io.lsTopdownInfo) ++ hybridUnits.map(_.io.ldu_io.lsTopdownInfo)

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

  // LoadUnit
  val correctMissTrain = Constantin.createRecord(s"CorrectMissTrain$hartId", initValue = false)

  for (i <- 0 until LduCnt) {
    loadUnits(i).io.redirect <> redirect

    // get input form dispatch
    loadUnits(i).io.ldin <> io.ooo_to_mem.issueLda(i)
    loadUnits(i).io.feedback_slow <> io.mem_to_ooo.ldaIqFeedback(i).feedbackSlow
    io.mem_to_ooo.ldaIqFeedback(i).feedbackFast := DontCare
    loadUnits(i).io.correctMissTrain := correctMissTrain
    io.mem_to_ooo.ldCancel.drop(HyuCnt)(i) := loadUnits(i).io.ldCancel
    io.mem_to_ooo.wakeup.drop(HyuCnt)(i) := loadUnits(i).io.wakeup

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
    }

    // forward
    loadUnits(i).io.lsq.forward <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer <> sbuffer.io.forward(i)
    loadUnits(i).io.tl_d_channel := dcache.io.lsu.forward_D(i)
    loadUnits(i).io.forward_mshr <> dcache.io.lsu.forward_mshr(i)
    // ld-ld violation check
    loadUnits(i).io.lsq.ldld_nuke_query <> lsq.io.ldu.ldld_nuke_query(i)
    loadUnits(i).io.lsq.stld_nuke_query <> lsq.io.ldu.stld_nuke_query(i)
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
    val stld_nuke_query = storeUnits.map(_.io.stld_nuke_query) ++ hybridUnits.map(_.io.stu_io.stld_nuke_query)
    for (s <- 0 until StorePipelineWidth) {
      loadUnits(i).io.stld_nuke_query(s) := stld_nuke_query(s)
    }
    loadUnits(i).io.lq_rep_full <> lsq.io.lq_rep_full
    // load prefetch train
    prefetcherOpt.foreach(pf => {
      // sms will train on all miss load sources
      val source = loadUnits(i).io.prefetch_train
      pf.io.ld_in(i).valid := Mux(pf_train_on_hit,
        source.valid,
        source.valid && source.bits.isFirstIssue && source.bits.miss
      )
      pf.io.ld_in(i).bits := source.bits
      pf.io.ld_in(i).bits.uop.pc := Mux(
        loadUnits(i).io.s2_ptr_chasing,
        RegEnable(io.ooo_to_mem.loadPc(i), loadUnits(i).io.s2_prefetch_spec),
        RegEnable(RegEnable(io.ooo_to_mem.loadPc(i), loadUnits(i).io.s1_prefetch_spec), loadUnits(i).io.s2_prefetch_spec)
      )
    })
    l1PrefetcherOpt.foreach(pf => {
      // stream will train on all load sources
      val source = loadUnits(i).io.prefetch_train_l1
      pf.io.ld_in(i).valid := source.valid && source.bits.isFirstIssue
      pf.io.ld_in(i).bits := source.bits
    })

    // load to load fast forward: load(i) prefers data(i)
    val l2l_fwd_out = loadUnits.map(_.io.l2l_fwd_out) ++ hybridUnits.map(_.io.ldu_io.l2l_fwd_out)
    val fastPriority = (i until LduCnt + HyuCnt) ++ (0 until i)
    val fastValidVec = fastPriority.map(j => l2l_fwd_out(j).valid)
    val fastDataVec = fastPriority.map(j => l2l_fwd_out(j).data)
    val fastErrorVec = fastPriority.map(j => l2l_fwd_out(j).dly_ld_err)
    val fastMatchVec = fastPriority.map(j => io.ooo_to_mem.loadFastMatch(i)(j))
    loadUnits(i).io.l2l_fwd_in.valid := VecInit(fastValidVec).asUInt.orR
    loadUnits(i).io.l2l_fwd_in.data := ParallelPriorityMux(fastValidVec, fastDataVec)
    loadUnits(i).io.l2l_fwd_in.dly_ld_err := ParallelPriorityMux(fastValidVec, fastErrorVec)
    val fastMatch = ParallelPriorityMux(fastValidVec, fastMatchVec)
    loadUnits(i).io.ld_fast_match := fastMatch
    loadUnits(i).io.ld_fast_imm := io.ooo_to_mem.loadFastImm(i)
    loadUnits(i).io.ld_fast_fuOpType := io.ooo_to_mem.loadFastFuOpType(i)
    loadUnits(i).io.replay <> lsq.io.replay(i)

    val l2_hint = RegNext(io.l2_hint)

    // L2 Hint for DCache
    dcache.io.l2_hint <> l2_hint

    loadUnits(i).io.l2_hint <> l2_hint
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
    lsq.io.l2_hint.valid := l2_hint.valid
    lsq.io.l2_hint.bits.sourceId := l2_hint.bits.sourceId
    lsq.io.l2_hint.bits.isKeyword := l2_hint.bits.isKeyword

    lsq.io.tlb_hint <> dtlbRepeater.io.hint.get

    // connect misalignBuffer
    loadMisalignBuffer.io.req(i) <> loadUnits(i).io.misalign_buf

    if (i == 0) {
      loadUnits(i).io.misalign_ldin  <> loadMisalignBuffer.io.splitLoadReq
      loadUnits(i).io.misalign_ldout <> loadMisalignBuffer.io.splitLoadResp
    } else {
      loadUnits(i).io.misalign_ldin.valid := false.B
      loadUnits(i).io.misalign_ldin.bits := DontCare
    }

    // alter writeback exception info
    io.mem_to_ooo.s3_delayed_load_error(i) := loadUnits(i).io.s3_dly_ld_err

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

  for (i <- 0 until HyuCnt) {
    hybridUnits(i).io.redirect <> redirect

    // get input from dispatch
    hybridUnits(i).io.lsin <> io.ooo_to_mem.issueHya(i)
    hybridUnits(i).io.feedback_slow <> io.mem_to_ooo.hyuIqFeedback(i).feedbackSlow
    hybridUnits(i).io.feedback_fast <> io.mem_to_ooo.hyuIqFeedback(i).feedbackFast
    hybridUnits(i).io.correctMissTrain := correctMissTrain
    io.mem_to_ooo.ldCancel.take(HyuCnt)(i) := hybridUnits(i).io.ldu_io.ldCancel
    io.mem_to_ooo.wakeup.take(HyuCnt)(i) := hybridUnits(i).io.ldu_io.wakeup

    // ------------------------------------
    //  Load Port
    // ------------------------------------
    // fast replay
    hybridUnits(i).io.ldu_io.fast_rep_in <> hybridUnits(i).io.ldu_io.fast_rep_out

    // get input from dispatch
    hybridUnits(i).io.ldu_io.dcache <> dcache.io.lsu.load(LduCnt + i)
    hybridUnits(i).io.stu_io.dcache <> dcache.io.lsu.sta(StaCnt + i)

    // dcache access
    hybridUnits(i).io.ldu_io.lsq.forward <> lsq.io.forward(LduCnt + i)
    // forward
    hybridUnits(i).io.ldu_io.sbuffer <> sbuffer.io.forward(LduCnt + i)
    // hybridUnits(i).io.ldu_io.vec_forward <> vsFlowQueue.io.forward(LduCnt + i)
    hybridUnits(i).io.ldu_io.vec_forward := DontCare
    hybridUnits(i).io.ldu_io.tl_d_channel := dcache.io.lsu.forward_D(LduCnt + i)
    hybridUnits(i).io.ldu_io.forward_mshr <> dcache.io.lsu.forward_mshr(LduCnt + i)
    // ld-ld violation check
    hybridUnits(i).io.ldu_io.lsq.ldld_nuke_query <> lsq.io.ldu.ldld_nuke_query(LduCnt + i)
    hybridUnits(i).io.ldu_io.lsq.stld_nuke_query <> lsq.io.ldu.stld_nuke_query(LduCnt + i)
    hybridUnits(i).io.csrCtrl <> csrCtrl
    // dcache refill req
    hybridUnits(i).io.ldu_io.tlb_hint.id := dtlbRepeater.io.hint.get.req(LduCnt + i).id
    hybridUnits(i).io.ldu_io.tlb_hint.full := dtlbRepeater.io.hint.get.req(LduCnt + i).full ||
      tlbreplay_reg(LduCnt + i) || dtlb_ld0_tlbreplay_reg(LduCnt + i)

    // dtlb
    hybridUnits(i).io.tlb <> dtlb_ld.head.requestor(LduCnt + i)
    // pmp
    hybridUnits(i).io.pmp <> pmp_check.drop(LduCnt)(i).resp
    // st-ld violation query
    val stld_nuke_query = VecInit(storeUnits.map(_.io.stld_nuke_query) ++ hybridUnits.map(_.io.stu_io.stld_nuke_query))
    hybridUnits(i).io.ldu_io.stld_nuke_query := stld_nuke_query
    hybridUnits(i).io.ldu_io.lq_rep_full <> lsq.io.lq_rep_full
    // load prefetch train
    prefetcherOpt.foreach(pf => {
      val source = hybridUnits(i).io.prefetch_train
      pf.io.ld_in(LduCnt + i).valid := Mux(pf_train_on_hit,
        source.valid,
        source.valid && source.bits.isFirstIssue && source.bits.miss
      )
      pf.io.ld_in(LduCnt + i).bits := source.bits
      pf.io.ld_in(LduCnt + i).bits.uop.pc := Mux(hybridUnits(i).io.ldu_io.s2_ptr_chasing, io.ooo_to_mem.hybridPc(i), RegNext(io.ooo_to_mem.hybridPc(i)))
    })
    l1PrefetcherOpt.foreach(pf => {
      // stream will train on all load sources
      val source = hybridUnits(i).io.prefetch_train_l1
      pf.io.ld_in(LduCnt + i).valid := source.valid && source.bits.isFirstIssue &&
                                       FuType.isLoad(source.bits.uop.fuType)
      pf.io.ld_in(LduCnt + i).bits := source.bits
      pf.io.st_in(StaCnt + i).valid := false.B
      pf.io.st_in(StaCnt + i).bits := DontCare
    })
    prefetcherOpt.foreach(pf => {
      val source = hybridUnits(i).io.prefetch_train
      pf.io.st_in(StaCnt + i).valid := Mux(pf_train_on_hit,
        source.valid,
        source.valid && source.bits.isFirstIssue && source.bits.miss
      ) && FuType.isStore(source.bits.uop.fuType)
      pf.io.st_in(StaCnt + i).bits := source.bits
      pf.io.st_in(StaCnt + i).bits.uop.pc := RegNext(io.ooo_to_mem.hybridPc(i))
    })

    // load to load fast forward: load(i) prefers data(i)
    val l2l_fwd_out = loadUnits.map(_.io.l2l_fwd_out) ++ hybridUnits.map(_.io.ldu_io.l2l_fwd_out)
    val fastPriority = (LduCnt + i until LduCnt + HyuCnt) ++ (0 until LduCnt + i)
    val fastValidVec = fastPriority.map(j => l2l_fwd_out(j).valid)
    val fastDataVec = fastPriority.map(j => l2l_fwd_out(j).data)
    val fastErrorVec = fastPriority.map(j => l2l_fwd_out(j).dly_ld_err)
    val fastMatchVec = fastPriority.map(j => io.ooo_to_mem.loadFastMatch(LduCnt + i)(j))
    hybridUnits(i).io.ldu_io.l2l_fwd_in.valid := VecInit(fastValidVec).asUInt.orR
    hybridUnits(i).io.ldu_io.l2l_fwd_in.data := ParallelPriorityMux(fastValidVec, fastDataVec)
    hybridUnits(i).io.ldu_io.l2l_fwd_in.dly_ld_err := ParallelPriorityMux(fastValidVec, fastErrorVec)
    val fastMatch = ParallelPriorityMux(fastValidVec, fastMatchVec)
    hybridUnits(i).io.ldu_io.ld_fast_match := fastMatch
    hybridUnits(i).io.ldu_io.ld_fast_imm := io.ooo_to_mem.loadFastImm(LduCnt + i)
    hybridUnits(i).io.ldu_io.ld_fast_fuOpType := io.ooo_to_mem.loadFastFuOpType(LduCnt + i)
    hybridUnits(i).io.ldu_io.replay <> lsq.io.replay(LduCnt + i)
    hybridUnits(i).io.ldu_io.l2_hint <> io.l2_hint

    // uncache
    lsq.io.ldout.drop(LduCnt)(i) <> hybridUnits(i).io.ldu_io.lsq.uncache
    lsq.io.ld_raw_data.drop(LduCnt)(i) <> hybridUnits(i).io.ldu_io.lsq.ld_raw_data


    // passdown to lsq (load s2)
    lsq.io.ldu.ldin(LduCnt + i) <> hybridUnits(i).io.ldu_io.lsq.ldin
    // Lsq to sta unit
    lsq.io.sta.storeMaskIn(StaCnt + i) <> hybridUnits(i).io.stu_io.st_mask_out

    // Lsq to std unit's rs
    lsq.io.std.storeDataIn(StaCnt + i) := stData(StaCnt + i)
    // prefetch
    hybridUnits(i).io.stu_io.prefetch_req <> sbuffer.io.store_prefetch(StaCnt + i)

    io.mem_to_ooo.s3_delayed_load_error(LduCnt + i) := hybridUnits(i).io.ldu_io.s3_dly_ld_err

    // ------------------------------------
    //  Store Port
    // ------------------------------------
    hybridUnits(i).io.stu_io.lsq <> lsq.io.sta.storeAddrIn.takeRight(HyuCnt)(i)
    hybridUnits(i).io.stu_io.lsq_replenish <> lsq.io.sta.storeAddrInRe.takeRight(HyuCnt)(i)

    lsq.io.sta.storeMaskIn.takeRight(HyuCnt)(i) <> hybridUnits(i).io.stu_io.st_mask_out
    io.mem_to_ooo.stIn.takeRight(HyuCnt)(i).valid := hybridUnits(i).io.stu_io.issue.valid
    io.mem_to_ooo.stIn.takeRight(HyuCnt)(i).bits := hybridUnits(i).io.stu_io.issue.bits

    // ------------------------------------
    //  Vector Store Port
    // ------------------------------------
    hybridUnits(i).io.vec_stu_io.isFirstIssue := true.B

    // -------------------------
    // Store Triggers
    // -------------------------
    hybridUnits(i).io.fromCsrTrigger.tdataVec := tdata
    hybridUnits(i).io.fromCsrTrigger.tEnableVec := tEnable
    hybridUnits(i).io.fromCsrTrigger.triggerCanRaiseBpExp := triggerCanRaiseBpExp
    hybridUnits(i).io.fromCsrTrigger.debugMode := debugMode
  }

  // misalignBuffer
  loadMisalignBuffer.io.redirect                <> redirect
  loadMisalignBuffer.io.rob.lcommit             := io.ooo_to_mem.lsqio.lcommit
  loadMisalignBuffer.io.rob.scommit             := io.ooo_to_mem.lsqio.scommit
  loadMisalignBuffer.io.rob.pendingUncacheld    := io.ooo_to_mem.lsqio.pendingUncacheld
  loadMisalignBuffer.io.rob.pendingld           := io.ooo_to_mem.lsqio.pendingld
  loadMisalignBuffer.io.rob.pendingst           := io.ooo_to_mem.lsqio.pendingst
  loadMisalignBuffer.io.rob.pendingVst          := io.ooo_to_mem.lsqio.pendingVst
  loadMisalignBuffer.io.rob.commit              := io.ooo_to_mem.lsqio.commit
  loadMisalignBuffer.io.rob.pendingPtr          := io.ooo_to_mem.lsqio.pendingPtr
  loadMisalignBuffer.io.rob.pendingPtrNext      := io.ooo_to_mem.lsqio.pendingPtrNext

  lsq.io.flushFrmMaBuf                          := loadMisalignBuffer.io.flushLdExpBuff

  storeMisalignBuffer.io.redirect               <> redirect
  storeMisalignBuffer.io.rob.lcommit            := io.ooo_to_mem.lsqio.lcommit
  storeMisalignBuffer.io.rob.scommit            := io.ooo_to_mem.lsqio.scommit
  storeMisalignBuffer.io.rob.pendingUncacheld   := io.ooo_to_mem.lsqio.pendingUncacheld
  storeMisalignBuffer.io.rob.pendingld          := io.ooo_to_mem.lsqio.pendingld
  storeMisalignBuffer.io.rob.pendingst          := io.ooo_to_mem.lsqio.pendingst
  storeMisalignBuffer.io.rob.pendingVst         := io.ooo_to_mem.lsqio.pendingVst
  storeMisalignBuffer.io.rob.commit             := io.ooo_to_mem.lsqio.commit
  storeMisalignBuffer.io.rob.pendingPtr         := io.ooo_to_mem.lsqio.pendingPtr
  storeMisalignBuffer.io.rob.pendingPtrNext     := io.ooo_to_mem.lsqio.pendingPtrNext

  lsq.io.maControl                              <> storeMisalignBuffer.io.sqControl

  // lsq to l2 CMO
  outer.cmo_sender match {
    case Some(x) =>
      x.out.head._1 <> lsq.io.cmoOpReq
    case None =>
      lsq.io.cmoOpReq.ready  := false.B
  }
  outer.cmo_reciver match {
    case Some(x) =>
      x.in.head._1  <> lsq.io.cmoOpResp
    case None =>
      lsq.io.cmoOpResp.valid := false.B
      lsq.io.cmoOpResp.bits  := 0.U.asTypeOf(new CMOResp)
  }

  // Prefetcher
  val StreamDTLBPortIndex = TlbStartVec(dtlb_ld_idx) + LduCnt + HyuCnt
  val PrefetcherDTLBPortIndex = TlbStartVec(dtlb_pf_idx)
  val L2toL1DLBPortIndex = TlbStartVec(dtlb_pf_idx) + 1
  prefetcherOpt match {
  case Some(pf) => dtlb_reqs(PrefetcherDTLBPortIndex) <> pf.io.tlb_req
  case None =>
    dtlb_reqs(PrefetcherDTLBPortIndex) := DontCare
    dtlb_reqs(PrefetcherDTLBPortIndex).req.valid := false.B
    dtlb_reqs(PrefetcherDTLBPortIndex).resp.ready := true.B
  }
  l1PrefetcherOpt match {
    case Some(pf) => dtlb_reqs(StreamDTLBPortIndex) <> pf.io.tlb_req
    case None =>
        dtlb_reqs(StreamDTLBPortIndex) := DontCare
        dtlb_reqs(StreamDTLBPortIndex).req.valid := false.B
        dtlb_reqs(StreamDTLBPortIndex).resp.ready := true.B
  }
  dtlb_reqs(L2toL1DLBPortIndex) <> io.l2_tlb_req
  dtlb_reqs(L2toL1DLBPortIndex).resp.ready := true.B
  io.l2_pmp_resp := pmp_check(L2toL1DLBPortIndex).resp

  // StoreUnit
  for (i <- 0 until StdCnt) {
    stdExeUnits(i).io.flush <> redirect
    stdExeUnits(i).io.in.valid := io.ooo_to_mem.issueStd(i).valid
    io.ooo_to_mem.issueStd(i).ready := stdExeUnits(i).io.in.ready
    stdExeUnits(i).io.in.bits := io.ooo_to_mem.issueStd(i).bits
  }

  for (i <- 0 until StaCnt) {
    val stu = storeUnits(i)

    stu.io.redirect      <> redirect
    stu.io.csrCtrl       <> csrCtrl
    stu.io.dcache        <> dcache.io.lsu.sta(i)
    stu.io.feedback_slow <> io.mem_to_ooo.staIqFeedback(i).feedbackSlow
    stu.io.stin         <> io.ooo_to_mem.issueSta(i)
    stu.io.lsq          <> lsq.io.sta.storeAddrIn(i)
    stu.io.lsq_replenish <> lsq.io.sta.storeAddrInRe(i)
    // dtlb
    stu.io.tlb          <> dtlb_st.head.requestor(i)
    stu.io.pmp          <> pmp_check(LduCnt + HyuCnt + 1 + i).resp

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
    storeMisalignBuffer.io.req(i) <> stu.io.misalign_buf

    if (i == 0) {
      stu.io.misalign_stin  <> storeMisalignBuffer.io.splitStoreReq
      stu.io.misalign_stout <> storeMisalignBuffer.io.splitStoreResp
    } else {
      stu.io.misalign_stin.valid := false.B
      stu.io.misalign_stin.bits := DontCare
    }

    // Lsq to std unit's rs
    if (i < VstuCnt){
      when (vsSplit(i).io.vstd.get.valid) {
        lsq.io.std.storeDataIn(i).valid := true.B
        lsq.io.std.storeDataIn(i).bits := vsSplit(i).io.vstd.get.bits
        stData(i).ready := false.B
      }.otherwise {
        lsq.io.std.storeDataIn(i).valid := stData(i).valid
        lsq.io.std.storeDataIn(i).bits.uop := stData(i).bits.uop
        lsq.io.std.storeDataIn(i).bits.data := stData(i).bits.data
        lsq.io.std.storeDataIn(i).bits.mask.map(_ := 0.U)
        lsq.io.std.storeDataIn(i).bits.vdIdx.map(_ := 0.U)
        lsq.io.std.storeDataIn(i).bits.vdIdxInField.map(_ := 0.U)
        stData(i).ready := true.B
      }
    } else {
        lsq.io.std.storeDataIn(i).valid := stData(i).valid
        lsq.io.std.storeDataIn(i).bits.uop := stData(i).bits.uop
        lsq.io.std.storeDataIn(i).bits.data := stData(i).bits.data
        lsq.io.std.storeDataIn(i).bits.mask.map(_ := 0.U)
        lsq.io.std.storeDataIn(i).bits.vdIdx.map(_ := 0.U)
        lsq.io.std.storeDataIn(i).bits.vdIdxInField.map(_ := 0.U)
        stData(i).ready := true.B
    }
    lsq.io.std.storeDataIn.map(_.bits.debug := 0.U.asTypeOf(new DebugBundle))
    lsq.io.std.storeDataIn.foreach(_.bits.isFromLoadUnit := DontCare)


    // store prefetch train
    l1PrefetcherOpt.foreach(pf => {
      // stream will train on all load sources
      pf.io.st_in(i).valid := false.B
      pf.io.st_in(i).bits := DontCare
    })

    prefetcherOpt.foreach(pf => {
      pf.io.st_in(i).valid := Mux(pf_train_on_hit,
        stu.io.prefetch_train.valid,
        stu.io.prefetch_train.valid && stu.io.prefetch_train.bits.isFirstIssue && (
          stu.io.prefetch_train.bits.miss
          )
      )
      pf.io.st_in(i).bits := stu.io.prefetch_train.bits
      pf.io.st_in(i).bits.uop.pc := RegEnable(RegEnable(io.ooo_to_mem.storePc(i), stu.io.s1_prefetch_spec), stu.io.s2_prefetch_spec)
    })

    // 1. sync issue info to store set LFST
    // 2. when store issue, broadcast issued sqPtr to wake up the following insts
    // io.stIn(i).valid := io.issue(exuParameters.LduCnt + i).valid
    // io.stIn(i).bits := io.issue(exuParameters.LduCnt + i).bits
    io.mem_to_ooo.stIn(i).valid := stu.io.issue.valid
    io.mem_to_ooo.stIn(i).bits := stu.io.issue.bits

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

  // mmio store writeback will use store writeback port 0
  val mmioStout = WireInit(0.U.asTypeOf(lsq.io.mmioStout))
  NewPipelineConnect(
    lsq.io.mmioStout, mmioStout, mmioStout.fire,
    false.B,
    Option("mmioStOutConnect")
  )
  mmioStout.ready := false.B
  when (mmioStout.valid && !storeUnits(0).io.stout.valid) {
    stOut(0).valid := true.B
    stOut(0).bits  := mmioStout.bits
    mmioStout.ready := true.B
  }
  // vec mmio writeback
  lsq.io.vecmmioStout.ready := false.B
  when (lsq.io.vecmmioStout.valid && !storeUnits(0).io.vecstout.valid) {
    stOut(0).valid := true.B
    stOut(0).bits  := lsq.io.vecmmioStout.bits
    lsq.io.vecmmioStout.ready := true.B
  }
  // miss align buffer will overwrite stOut(0)
  storeMisalignBuffer.io.writeBack.ready := true.B
  when (storeMisalignBuffer.io.writeBack.valid) {
    stOut(0).valid := true.B
    stOut(0).bits  := storeMisalignBuffer.io.writeBack.bits
  }

  // Uncache
  uncache.io.enableOutstanding := io.ooo_to_mem.csrCtrl.uncache_write_outstanding_enable
  uncache.io.hartId := io.hartId
  lsq.io.uncacheOutstanding := io.ooo_to_mem.csrCtrl.uncache_write_outstanding_enable

  // Lsq
  io.mem_to_ooo.lsqio.mmio       := lsq.io.rob.mmio
  io.mem_to_ooo.lsqio.uop        := lsq.io.rob.uop
  lsq.io.rob.lcommit             := io.ooo_to_mem.lsqio.lcommit
  lsq.io.rob.scommit             := io.ooo_to_mem.lsqio.scommit
  lsq.io.rob.pendingUncacheld    := io.ooo_to_mem.lsqio.pendingUncacheld
  lsq.io.rob.pendingld           := io.ooo_to_mem.lsqio.pendingld
  lsq.io.rob.pendingst           := io.ooo_to_mem.lsqio.pendingst
  lsq.io.rob.pendingVst          := io.ooo_to_mem.lsqio.pendingVst
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
  val allRedirect = loadUnits.map(_.io.rollback) ++ hybridUnits.map(_.io.ldu_io.rollback) ++ Seq(lsq.io.nack_rollback) ++ lsq.io.nuke_rollback
  val oldestOneHot = selectOldestRedirect(allRedirect)
  val oldestRedirect = WireDefault(Mux1H(oldestOneHot, allRedirect))
  // memory replay would not cause IAF/IPF/IGPF
  oldestRedirect.bits.cfiUpdate.backendIAF := false.B
  oldestRedirect.bits.cfiUpdate.backendIPF := false.B
  oldestRedirect.bits.cfiUpdate.backendIGPF := false.B
  io.mem_to_ooo.memoryViolation := oldestRedirect
  io.mem_to_ooo.lsqio.lqCanAccept  := lsq.io.lqCanAccept
  io.mem_to_ooo.lsqio.sqCanAccept  := lsq.io.sqCanAccept

  // lsq.io.uncache        <> uncache.io.lsq
  val s_idle :: s_scalar_uncache :: s_vector_uncache :: Nil = Enum(3)
  val uncacheState = RegInit(s_idle)
  val uncacheReq = Wire(Decoupled(new UncacheWordReq))
  val uncacheResp = Wire(Decoupled(new UncacheWordResp))

  uncacheReq.bits := DontCare
  uncacheReq.valid := false.B
  uncacheReq.ready := false.B
  uncacheResp.bits := DontCare
  uncacheResp.valid := false.B
  uncacheResp.ready := false.B
  lsq.io.uncache.req.ready := false.B
  lsq.io.uncache.resp.valid := false.B
  lsq.io.uncache.resp.bits := DontCare

  switch (uncacheState) {
    is (s_idle) {
      when (uncacheReq.fire) {
        when (lsq.io.uncache.req.valid) {
          val isStore = lsq.io.uncache.req.bits.cmd === MemoryOpConstants.M_XWR
          when (!isStore || !io.ooo_to_mem.csrCtrl.uncache_write_outstanding_enable) {
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
    uncacheResp <> lsq.io.uncache.resp
  }.otherwise {
    when (uncacheState === s_scalar_uncache) {
      uncacheResp <> lsq.io.uncache.resp
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
  sbuffer.io.vecDifftestInfo      := DontCare
  lsq.io.sbufferVecDifftestInfo   := DontCare
  vSegmentUnit.io.vecDifftestInfo := DontCare
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

  // lsq.io.vecStoreRetire <> vsFlowQueue.io.sqRelease
  // lsq.io.vecWriteback.valid := vlWrapper.io.uopWriteback.fire &&
  //   vlWrapper.io.uopWriteback.bits.uop.vpu.lastUop
  // lsq.io.vecWriteback.bits := vlWrapper.io.uopWriteback.bits

  // vector
  val vLoadCanAccept  = (0 until VlduCnt).map(i =>
    vlSplit(i).io.in.ready && VlduType.isVecLd(io.ooo_to_mem.issueVldu(i).bits.uop.fuOpType)
  )
  val vStoreCanAccept = (0 until VstuCnt).map(i =>
    vsSplit(i).io.in.ready && VstuType.isVecSt(io.ooo_to_mem.issueVldu(i).bits.uop.fuOpType)
  )
  val isSegment     = io.ooo_to_mem.issueVldu.head.valid && isVsegls(io.ooo_to_mem.issueVldu.head.bits.uop.fuType)
  val isFixVlUop    = io.ooo_to_mem.issueVldu.map{x =>
    x.bits.uop.vpu.isVleff && x.bits.uop.vpu.lastUop && x.valid
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
  }

  (0 until VstuCnt).foreach{i =>
    vsSplit(i).io.redirect <> redirect
    vsSplit(i).io.in <> io.ooo_to_mem.issueVldu(i)
    vsSplit(i).io.in.valid := io.ooo_to_mem.issueVldu(i).valid &&
                              vStoreCanAccept(i) && !isSegment
    vsSplit(i).io.toMergeBuffer <> vsMergeBuffer(i).io.fromSplit.head
    NewPipelineConnect(
      vsSplit(i).io.out, storeUnits(i).io.vecstin, storeUnits(i).io.vecstin.fire,
      Mux(vsSplit(i).io.out.fire, vsSplit(i).io.out.bits.uop.robIdx.needFlush(io.redirect), storeUnits(i).io.vecstin.bits.uop.robIdx.needFlush(io.redirect)),
      Option("VsSplitConnectStu")
    )
    vsSplit(i).io.vstd.get := DontCare // Todo: Discuss how to pass vector store data

  }
  (0 until VlduCnt).foreach{i =>
    vlSplit(i).io.redirect <> redirect
    vlSplit(i).io.in <> io.ooo_to_mem.issueVldu(i)
    vlSplit(i).io.in.valid := io.ooo_to_mem.issueVldu(i).valid &&
                              vLoadCanAccept(i) && !isSegment && !isFixVlUop(i)
    vlSplit(i).io.toMergeBuffer <> vlMergeBuffer.io.fromSplit(i)
    NewPipelineConnect(
      vlSplit(i).io.out, loadUnits(i).io.vecldin, loadUnits(i).io.vecldin.fire,
      Mux(vlSplit(i).io.out.fire, vlSplit(i).io.out.bits.uop.robIdx.needFlush(io.redirect), loadUnits(i).io.vecldin.bits.uop.robIdx.needFlush(io.redirect)),
      Option("VlSplitConnectLdu")
    )

    //Subsequent instrction will be blocked
    vfofBuffer.io.in(i).valid := io.ooo_to_mem.issueVldu(i).valid
    vfofBuffer.io.in(i).bits  := io.ooo_to_mem.issueVldu(i).bits
  }
  (0 until LduCnt).foreach{i=>
    vlMergeBuffer.io.fromPipeline(i) <> loadUnits(i).io.vecldout
  }

  (0 until StaCnt).foreach{i=>
    if(i < VstuCnt){
      vsMergeBuffer(i).io.fromPipeline.head <> storeUnits(i).io.vecstout
    }
  }

  (0 until VlduCnt).foreach{i=>
    io.ooo_to_mem.issueVldu(i).ready := vLoadCanAccept(i) || vStoreCanAccept(i)
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

  (0 until VlduCnt).foreach{i=>
    if (i == 0){ // for segmentUnit, segmentUnit use port0 writeback
      io.mem_to_ooo.writebackVldu(i).valid := vlMergeBuffer.io.uopWriteback(i).valid || vsMergeBuffer(i).io.uopWriteback.head.valid || vSegmentUnit.io.uopwriteback.valid
      io.mem_to_ooo.writebackVldu(i).bits := PriorityMux(Seq(
        vSegmentUnit.io.uopwriteback.valid          -> vSegmentUnit.io.uopwriteback.bits,
        vlMergeBuffer.io.uopWriteback(i).valid      -> vlMergeBuffer.io.uopWriteback(i).bits,
        vsMergeBuffer(i).io.uopWriteback.head.valid -> vsMergeBuffer(i).io.uopWriteback.head.bits,
      ))
      vlMergeBuffer.io.uopWriteback(i).ready := io.mem_to_ooo.writebackVldu(i).ready && !vSegmentUnit.io.uopwriteback.valid
      vsMergeBuffer(i).io.uopWriteback.head.ready := io.mem_to_ooo.writebackVldu(i).ready && !vlMergeBuffer.io.uopWriteback(i).valid && !vSegmentUnit.io.uopwriteback.valid
      vSegmentUnit.io.uopwriteback.ready := io.mem_to_ooo.writebackVldu(i).ready
    } else if (i == 1) {
      io.mem_to_ooo.writebackVldu(i).valid := vlMergeBuffer.io.uopWriteback(i).valid || vsMergeBuffer(i).io.uopWriteback.head.valid || vfofBuffer.io.uopWriteback.valid
      io.mem_to_ooo.writebackVldu(i).bits := PriorityMux(Seq(
        vfofBuffer.io.uopWriteback.valid            -> vfofBuffer.io.uopWriteback.bits,
        vlMergeBuffer.io.uopWriteback(i).valid      -> vlMergeBuffer.io.uopWriteback(i).bits,
        vsMergeBuffer(i).io.uopWriteback.head.valid -> vsMergeBuffer(i).io.uopWriteback.head.bits,
      ))
      vlMergeBuffer.io.uopWriteback(i).ready := io.mem_to_ooo.writebackVldu(i).ready && !vfofBuffer.io.uopWriteback.valid
      vsMergeBuffer(i).io.uopWriteback.head.ready := io.mem_to_ooo.writebackVldu(i).ready && !vlMergeBuffer.io.uopWriteback(i).valid && !vfofBuffer.io.uopWriteback.valid
      vfofBuffer.io.uopWriteback.ready := io.mem_to_ooo.writebackVldu(i).ready
    } else {
      io.mem_to_ooo.writebackVldu(i).valid := vlMergeBuffer.io.uopWriteback(i).valid || vsMergeBuffer(i).io.uopWriteback.head.valid
      io.mem_to_ooo.writebackVldu(i).bits := PriorityMux(Seq(
        vlMergeBuffer.io.uopWriteback(i).valid -> vlMergeBuffer.io.uopWriteback(i).bits,
        vsMergeBuffer(i).io.uopWriteback.head.valid -> vsMergeBuffer(i).io.uopWriteback.head.bits,
      ))
      vlMergeBuffer.io.uopWriteback(i).ready := io.mem_to_ooo.writebackVldu(i).ready
      vsMergeBuffer(i).io.uopWriteback.head.ready := io.mem_to_ooo.writebackVldu(i).ready && !vlMergeBuffer.io.uopWriteback(i).valid
    }

    vfofBuffer.io.mergeUopWriteback(i).valid := vlMergeBuffer.io.uopWriteback(i).valid
    vfofBuffer.io.mergeUopWriteback(i).bits  := vlMergeBuffer.io.uopWriteback(i).bits
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
  val s_normal +: s_atomics = Enum(StaCnt + HyuCnt + 1)
  val state = RegInit(s_normal)

  val st_atomics = Seq.tabulate(StaCnt)(i =>
    io.ooo_to_mem.issueSta(i).valid && FuType.storeIsAMO((io.ooo_to_mem.issueSta(i).bits.uop.fuType))
  ) ++ Seq.tabulate(HyuCnt)(i =>
    io.ooo_to_mem.issueHya(i).valid && FuType.storeIsAMO((io.ooo_to_mem.issueHya(i).bits.uop.fuType))
  )

  val st_data_atomics = Seq.tabulate(StdCnt)(i =>
    stData(i).valid && FuType.storeIsAMO(stData(i).bits.uop.fuType)
  )

  for (i <- 0 until StaCnt) when(st_atomics(i)) {
    io.ooo_to_mem.issueSta(i).ready := atomicsUnit.io.in.ready
    storeUnits(i).io.stin.valid := false.B

    state := s_atomics(i)
    assert(!st_atomics.zipWithIndex.filterNot(_._2 == i).unzip._1.reduce(_ || _))
  }
  for (i <- 0 until HyuCnt) when(st_atomics(StaCnt + i)) {
    io.ooo_to_mem.issueHya(i).ready := atomicsUnit.io.in.ready
    hybridUnits(i).io.lsin.valid := false.B

    state := s_atomics(StaCnt + i)
    assert(!st_atomics.zipWithIndex.filterNot(_._2 == StaCnt + i).unzip._1.reduce(_ || _))
  }
  when (atomicsUnit.io.out.valid) {
    assert((0 until StaCnt + HyuCnt).map(state === s_atomics(_)).reduce(_ || _))
    state := s_normal
  }

  atomicsUnit.io.in.valid := st_atomics.reduce(_ || _)
  atomicsUnit.io.in.bits  := Mux1H(Seq.tabulate(StaCnt)(i =>
    st_atomics(i) -> io.ooo_to_mem.issueSta(i).bits) ++
    Seq.tabulate(HyuCnt)(i => st_atomics(StaCnt+i) -> io.ooo_to_mem.issueHya(i).bits))
  atomicsUnit.io.storeDataIn.valid := st_data_atomics.reduce(_ || _)
  atomicsUnit.io.storeDataIn.bits  := Mux1H(Seq.tabulate(StdCnt)(i =>
    st_data_atomics(i) -> stData(i).bits))
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
  for (i <- 0 until HyuCnt) {
    when (state === s_atomics(StaCnt + i)) {
      io.mem_to_ooo.hyuIqFeedback(i).feedbackSlow := atomicsUnit.io.feedbackSlow
      assert(!hybridUnits(i).io.feedback_slow.valid)
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

  def GenExceptionVa(mode: UInt, isVirt: Bool, vaNeedExt: Bool,
                     satp: TlbSatpBundle, vsatp: TlbSatpBundle, hgatp: TlbHgatpBundle,
                     vaddr: UInt) = {
    require(VAddrBits >= 50)

    val Sv39 = satp.mode === 8.U
    val Sv48 = satp.mode === 9.U
    val Sv39x4 = vsatp.mode === 8.U || hgatp.mode === 8.U
    val Sv48x4 = vsatp.mode === 9.U || hgatp.mode === 9.U
    val vmEnable = !isVirt && (Sv39 || Sv48) && (mode < ModeM)
    val s2xlateEnable = isVirt && (Sv39x4 || Sv48x4) && (mode < ModeM)

    val s2xlate = MuxCase(noS2xlate, Seq(
      !isVirt                                    -> noS2xlate,
      (vsatp.mode =/= 0.U && hgatp.mode =/= 0.U) -> allStage,
      (vsatp.mode === 0.U)                       -> onlyStage2,
      (hgatp.mode === 0.U)                       -> onlyStage1
    ))
    val onlyS2 = s2xlate === onlyStage2

    val bareAddr   = ZeroExt(vaddr(PAddrBits - 1, 0), XLEN)
    val sv39Addr   = SignExt(vaddr.take(39), XLEN)
    val sv39x4Addr = ZeroExt(vaddr.take(39 + 2), XLEN)
    val sv48Addr   = SignExt(vaddr.take(48), XLEN)
    val sv48x4Addr = ZeroExt(vaddr.take(48 + 2), XLEN)

    val ExceptionVa = Wire(UInt(XLEN.W))
    when (vaNeedExt) {
      ExceptionVa := Mux1H(Seq(
        (!(vmEnable || s2xlateEnable)) -> bareAddr,
        (!onlyS2 && (Sv39 || Sv39x4))  -> sv39Addr,
        (!onlyS2 && (Sv48 || Sv48x4))  -> sv48Addr,
        ( onlyS2 && (Sv39 || Sv39x4))  -> sv39x4Addr,
        ( onlyS2 && (Sv48 || Sv48x4))  -> sv48x4Addr,
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
    x.externalInterrupt.msip  := outer.clint_int_sink.in.head._1(0)
    x.externalInterrupt.mtip  := outer.clint_int_sink.in.head._1(1)
    x.externalInterrupt.meip  := outer.plic_int_sink.in.head._1(0)
    x.externalInterrupt.seip  := outer.plic_int_sink.in.last._1(0)
    x.externalInterrupt.debug := outer.debug_int_sink.in.head._1(0)
    x.externalInterrupt.nmi.nmi_31 := outer.nmi_int_sink.in.head._1(0)
    x.externalInterrupt.nmi.nmi_43 := outer.nmi_int_sink.in.head._1(1)
    x.msiInfo           := DelayNWithValid(io.fromTopToBackend.msiInfo, 1)
    x.clintTime         := DelayNWithValid(io.fromTopToBackend.clintTime, 1)
  }

  io.memInfo.sqFull := RegNext(lsq.io.sqFull)
  io.memInfo.lqFull := RegNext(lsq.io.lqFull)
  io.memInfo.dcacheMSHRFull := RegNext(dcache.io.mshrFull)

  io.inner_hartId := io.hartId
  io.inner_reset_vector := RegNext(io.outer_reset_vector)
  io.outer_cpu_halt := io.ooo_to_mem.backendToTopBypass.cpuHalted
  io.outer_cpu_critical_error := io.ooo_to_mem.backendToTopBypass.cpuCriticalError
  io.outer_beu_errors_icache := RegNext(io.inner_beu_errors_icache)
  io.outer_l2_pf_enable := io.inner_l2_pf_enable
  io.inner_hc_perfEvents <> io.outer_hc_perfEvents

  // vector segmentUnit
  vSegmentUnit.io.in.bits <> io.ooo_to_mem.issueVldu.head.bits
  vSegmentUnit.io.in.valid := isSegment && io.ooo_to_mem.issueVldu.head.valid// is segment instruction
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
      ++ (if (prefetcherOpt.isDefined) Seq(ModuleNode(prefetcherOpt.get)) else Nil)
      ++ (if (l1PrefetcherOpt.isDefined) Seq(ModuleNode(l1PrefetcherOpt.get)) else Nil)
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
    ResetGen(leftResetTree, reset, sim = false)
    ResetGen(rightResetTree, reset, sim = false)
  } else {
    io.reset_backend := DontCare
  }
  io.resetInFrontendBypass.toL2Top := io.resetInFrontendBypass.fromFrontend

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

  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := csrCtrl.distribute_csr
  val csrevents = pfevent.io.hpmevent.slice(16,24)

  val memBlockPerfEvents = Seq(
    ("ldDeqCount", ldDeqCount),
    ("stDeqCount", stDeqCount),
  )

  val perfFromUnits = (loadUnits ++ Seq(sbuffer, lsq, dcache)).flatMap(_.getPerfEvents)
  val perfFromPTW    = perfEventsPTW.map(x => ("perfEventsPTW", x.value))
  val perfBlock     = Seq(("ldDeqCount", ldDeqCount),
                          ("stDeqCount", stDeqCount))
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
    ResetGen(ResetGenNode(Seq(ModuleNode(wrapper.inner.module))), reset, sim = false)
  }
}