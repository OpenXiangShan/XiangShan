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
import device.MsiInfoBundle
import utils._
import utility._
import system.SoCParamsKey
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.frontend.HasInstrMMIOConst
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.ctrlblock.{DebugLSIO, LsTopdownInfo}
import xiangshan.backend.exu.MemExeUnit
import xiangshan.backend.fu._
import xiangshan.backend.fu.FuType._
import xiangshan.backend.fu.util.{HasCSRConst, SdtrigExt}
import xiangshan.backend.{BackendToTopBundle, TopToBackendBundle}
import xiangshan.backend.rob.{RobDebugRollingIO, RobPtr, RobLsqIO}
import xiangshan.backend.datapath.NewPipelineConnect
import xiangshan.backend.fu.NewCSR.{CsrTriggerBundle, TriggerUtil}
import xiangshan.backend.trace.{Itype, TraceCoreInterface}
import xiangshan.backend.Bundles._
import xiangshan.mem._
import xiangshan.mem.mdp._
import xiangshan.mem.prefetch.{BasePrefecher, L1Prefetcher, SMSParams, SMSPrefetcher}
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.mmu._
import coupledL2.{PrefetchRecv}

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

  val (ldIdx, stIdx, pfIdx) = (0, 1, 2)
  val TlbSubSizeVec = Seq(LduCnt + HyuCnt + 1, StaCnt, 2) // (load + hyu + stream pf, store, sms+l2bop)
  val DTlbSize = TlbSubSizeVec.sum
  val TlbStartVec = TlbSubSizeVec.scanLeft(0)(_ + _).dropRight(1)
  val TlbEndVec = TlbSubSizeVec.scanLeft(0)(_ + _).drop(1)
  val StreamDTLBPortIndex = TlbStartVec(ldIdx) + LduCnt + HyuCnt
  val PrefetcherDTLBPortIndex = TlbStartVec(pfIdx)
  val L2toL1DLBPortIndex = TlbStartVec(pfIdx) + 1
}

abstract class MemBlockBundle(implicit val p: Parameters) extends Bundle with HasMemBlockParameters

class BackendToMemBlockIO(implicit p: Parameters) extends MemBlockBundle {
  val sfence = Input(new SfenceBundle)
  val tlbCsr = Input(new TlbCsrBundle)
  val robLsqIO = new Bundle() {
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
  val enqLsq = new LsqEnqIO
  val flushSb = Input(Bool())

  val storePc = Vec(StaCnt, Input(UInt(VAddrBits.W))) // for hw prefetcher
  val hybridPc = Vec(HyuCnt, Input(UInt(VAddrBits.W))) // for hw prefetcher

  val issueLda = MixedVec(Seq.fill(LduCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueSta = MixedVec(Seq.fill(StaCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueStd = MixedVec(Seq.fill(StdCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueHya = MixedVec(Seq.fill(HyuCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueVldu = MixedVec(Seq.fill(VlduCnt)(Flipped(DecoupledIO(new MemExuInput(isVector=true)))))

  def issueUops = issueLda ++ issueSta ++ issueStd ++ issueHya ++ issueVldu
}

class MemBlockToBackendIO(implicit p: Parameters) extends MemBlockBundle {
  val topToBackendBypass = new TopToBackendBundle

  val otherFastWakeup = Vec(LdExuCnt, ValidIO(new DynInst))
  val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
  val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
  val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
  // used by VLSU issue queue, the vector store would wait all store before it, and the vector load would wait all load
  val sqDeqPtr = Output(new SqPtr)
  val lqDeqPtr = Output(new LqPtr)
  val stIssue = Vec(StAddrCnt, ValidIO(new MemExuInput))
  val stIssuePtr = Output(new SqPtr())

  val memoryViolation = ValidIO(new Redirect)
  val sbIsEmpty = Output(Bool())

  val storeDebugInfo = Vec(EnsbufferWidth, new Bundle {
    val robidx = Output(new RobPtr)
    val pc     = Input(UInt(VAddrBits.W))
  })

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

  val ldaIqFeedback = Vec(LduCnt, new MemRSFeedbackIO)
  val staIqFeedback = Vec(StaCnt, new MemRSFeedbackIO)
  val hyuIqFeedback = Vec(HyuCnt, new MemRSFeedbackIO)
  val vstuIqFeedback= Vec(VstuCnt, new MemRSFeedbackIO(isVector = true))
  val vlduIqFeedback= Vec(VlduCnt, new MemRSFeedbackIO(isVector = true))
  val ldCancel = Vec(LdExuCnt, new LoadCancelIO)
  val wakeup = Vec(LdExuCnt, Valid(new DynInst))

  def writeBack: Seq[DecoupledIO[MemExuOutput]] = {
    writebackSta ++
      writebackHyuLda ++ writebackHyuSta ++
      writebackLda ++
      writebackVldu ++
      writebackStd
  }
}

class FrontendToMemBlockIO(implicit p: Parameters) extends MemBlockBundle {
  val itlb = Flipped(new TlbPtwIO())
}

class MemBlockToFrontendIO(implicit p: Parameters) extends MemBlockBundle {
  val ifetchPrefetch = Vec(LduCnt, ValidIO(new SoftIfetchPrefetchBundle))
}

class MemBlockBypassIO(implicit p: Parameters) extends MemBlockBundle {
  val fromBackendToTop = Flipped(new BackendToTopBundle)
  val fromTopToBackend = Input(new Bundle {
      val msiInfo   = ValidIO(new MsiInfoBundle)
      val clintTime = ValidIO(UInt(64.W))
  })
  val innerHartId = Output(UInt(hartIdLen.W))
  val innerResetVector = Output(UInt(PAddrBits.W))
  val outerResetVector = Input(UInt(PAddrBits.W))
  val outerCpuHalt = Output(Bool())
  val outerCpuCriticalError = Output(Bool())
  val innerBeuErrorsICache = Input(new L1BusErrorUnitInfo)
  val outerBeuErrorsICache = Output(new L1BusErrorUnitInfo)
  val innerL2PfEnable = Input(Bool())
  val outerL2PfEnable = Output(Bool())
  val innerHcPerfEvents = Output(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))
  val outerHcPerfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))

  // reset signals of frontend & backend are generated in memblock
  val resetBackend = Output(Reset())
  // Reset singal from frontend.
  val resetInFrontendBypass = new Bundle{
    val fromFrontend = Input(Bool())
    val toL2Top = Output(Bool())
  }
  val traceCoreInterfaceBypass = new Bundle{
    val fromBackend = Flipped(new TraceCoreInterface(hasOffset = true))
    val toL2Top = new TraceCoreInterface
  }
}

class MemCoreTopDownIO extends Bundle {
  val robHeadMissInDCache = Output(Bool())
  val robHeadTlbReplay = Output(Bool())
  val robHeadTlbMiss = Output(Bool())
  val robHeadLoadVio = Output(Bool())
  val robHeadLoadMSHR = Output(Bool())
}

class MemBlockInlined()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = true
  val l1dToL2Buffer = if (coreParams.dcacheParametersOpt.nonEmpty) LazyModule(new TLBuffer) else null
  val ptwToL2Buffer = if (!coreParams.softPTW) LazyModule(new TLBuffer) else null
  val uncachePort = TLTempNode()
  val uncacheXBar = TLXbar()

  val dcache = LazyModule(new DCacheWrapper())
  val uncache = LazyModule(new Uncache())
  val memExuBlock = LazyModule(new MemExuBlock)
  val prefetcher = LazyModule(new MemPrefetcher)
  val mmu = LazyModule(new MemMMU)
  val dcachePort = TLNameNode("dcache_client") // to keep dcache-L2 port name
  val frontendBridge = LazyModule(new FrontendBridge)

  // interrupt sinks
  val clintIntSink = IntSinkNode(IntSinkPortSimple(1, 2))
  val debugIntSink = IntSinkNode(IntSinkPortSimple(1, 1))
  val plicIntSink = IntSinkNode(IntSinkPortSimple(2, 1))
  val nmiIntSink = IntSinkNode(IntSinkPortSimple(1, (new NonmaskableInterruptIO).elements.size))

  // connection
  uncacheXBar := TLBuffer() := uncache.clientNode
  if (dcache.uncacheNode.isDefined) {
    dcache.uncacheNode.get := TLBuffer.chainNode(2) := uncacheXBar
  }
  uncachePort := TLBuffer.chainNode(2) := uncacheXBar
  if (!coreParams.softPTW) {
    ptwToL2Buffer.node := mmu.ptwToL2Buffer.node
  }

  lazy val module = new MemBlockInlinedImp(this)
}

class MemBlockInlinedIO(implicit p: Parameters) extends XSBundle {
  val fromCtrl = new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    val csr = Flipped(new CustomCSRCtrlIO)
    val redirect = Flipped(ValidIO(new Redirect))
  }

  // from
  val fromBackend = new BackendToMemBlockIO
  val fromFrontend = new FrontendToMemBlockIO

  // to
  val toBackend = new MemBlockToBackendIO
  val toFrontend = new MemBlockToFrontendIO

  // bypass
  val bypass = new MemBlockBypassIO

  //
  val l2Hint = Input(Valid(new L2ToL1Hint()))
  val l2PfqBusy = Input(Bool())
  val l2TlbReq = Flipped(new TlbRequestIO(nRespDups = 2))
  val l2PmpResp = new PMPRespBundle
  val error = ValidIO(new L1CacheErrorInfo)

  // debug
  val memInfo = new Bundle {
    val sqFull = Output(Bool())
    val lqFull = Output(Bool())
    val dcacheMSHRFull = Output(Bool())
  }
  val debugLS = new DebugLSIO
  val debugTopDown = new Bundle {
    val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
    val toCore = new MemCoreTopDownIO
  }
  val debugRolling = Flipped(new RobDebugRollingIO)
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
  with SdtrigExt {

  val io = IO(new MemBlockInlinedIO)


  dontTouch(io.bypass.innerHartId)
  dontTouch(io.bypass.innerResetVector)
  dontTouch(io.bypass.outerResetVector)
  dontTouch(io.bypass.outerCpuHalt)
  dontTouch(io.bypass.outerCpuCriticalError)
  dontTouch(io.bypass.innerBeuErrorsICache)
  dontTouch(io.bypass.outerBeuErrorsICache)
  dontTouch(io.bypass.innerL2PfEnable)
  dontTouch(io.bypass.outerL2PfEnable)

  private val dcache = outer.dcache.module
  private val uncache = outer.uncache.module
  private val prefetcher = outer.prefetcher.module
  private val mmu = outer.mmu.module
  private val memExuBlock = outer.memExuBlock.module
  val vecExuBlock = Module(new VecExuBlock)
  val lsq = Module(new LsqWrapper)
  val sbuffer = Module(new Sbuffer)
  val exceptionGen = Module(new MemExceptionGen(4))

  private val ptwToL2Buffer = outer.ptwToL2Buffer.module
  private val l1dToL2Buffer = outer.l1dToL2Buffer.module

  // init
  val redirect = RegNextWithEnable(io.fromCtrl.redirect)
  val csrCtrl = DelayN(io.fromCtrl.csr, 2)
  val l2Hint = RegNext(io.l2Hint)
  val sfence = RegNext(RegNext(io.fromBackend.sfence))
  val tlbcsr = RegNext(RegNext(io.fromBackend.tlbCsr))

  // trigger
  val memTrigger = Wire(new CsrTriggerBundle)
  val tdata = RegInit(VecInit(Seq.fill(TriggerNum)(0.U.asTypeOf(new MatchTriggerIO))))
  val tEnable = RegInit(VecInit(Seq.fill(TriggerNum)(false.B)))
  tEnable := csrCtrl.mem_trigger.tEnableVec
  when(csrCtrl.mem_trigger.tUpdate.valid) {
    tdata(csrCtrl.mem_trigger.tUpdate.bits.addr) := csrCtrl.mem_trigger.tUpdate.bits.tdata
  }
  val triggerCanRaiseBpExp = csrCtrl.mem_trigger.triggerCanRaiseBpExp
  val debugMode = csrCtrl.mem_trigger.debugMode
  memTrigger.tdataVec := tdata
  memTrigger.tEnableVec := tEnable
  memTrigger.debugMode := debugMode
  memTrigger.triggerCanRaiseBpExp := triggerCanRaiseBpExp

  XSDebug(tEnable.asUInt.orR, "Debug Mode: At least one store trigger is enabled\n")
  for (j <- 0 until TriggerNum) {
    PrintTriggerInfo(tEnable(j), tdata(j))
  }

  // error
  io.error <> DelayNWithValid(dcache.io.error, 2)
  when(!csrCtrl.cache_error_enable){
    io.error.bits.report_to_beu := false.B
    io.error.valid := false.B
  }

  // mmu
  mmu.io.fromCtrl.hartId <> io.fromCtrl.hartId
  mmu.io.fromCtrl.csr <> csrCtrl
  mmu.io.fromCtrl.redirect <> redirect
  mmu.io.fromFrontend <> io.fromFrontend
  mmu.io.fromBackend.sfence <> sfence
  mmu.io.fromBackend.tlbCsr <> tlbcsr
  mmu.io.fromBackend.l2TlbReq.req <> io.l2TlbReq.req
  mmu.io.fromBackend.l2TlbReq.req_kill <> io.l2TlbReq.req_kill
  mmu.io.toBackend.l2TlbResp <> io.l2TlbReq.resp
  mmu.io.toBackend.l2PmpResp <> io.l2PmpResp

  // prefetcher
  val loadPc = RegNext(VecInit(io.fromBackend.issueLda.map(_.bits.uop.pc)))
  prefetcher.io.fromCtrl.csr := csrCtrl
  prefetcher.io.fromBackend.loadPc := loadPc
  prefetcher.io.fromBackend.storePc := io.fromBackend.storePc
  prefetcher.io.fromBackend.hybridPc := io.fromBackend.hybridPc
  prefetcher.io.fromBackend.l2PfqBusy := io.l2PfqBusy
  prefetcher.io.fromDCache.evict <> dcache.io.sms_agt_evict_req
  prefetcher.io.fromDCache.pfCtrl <> dcache.io.pf_ctrl

   // dcache
  dcache.io.hartId := io.fromCtrl.hartId
  dcache.io.lqEmpty := lsq.io.lqEmpty
  dcache.io.l2_hint <> l2Hint
  dcache.io.lsu.tl_d_channel <> lsq.io.tl_d_channel
  dcache.io.force_write := lsq.io.force_write
  dcache.io.l2_pf_store_only   := RegNext(io.fromCtrl.csr.l2_pf_store_only, false.B)

  // Uncache
  uncache.io.enableOutstanding := csrCtrl.uncache_write_outstanding_enable
  uncache.io.hartId := io.fromCtrl.hartId

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
          when (!isStore || !uncache.io.enableOutstanding) {
            uncacheState := s_scalar_uncache
          }
        }.otherwise {
          // val isStore = vsFlowQueue.io.uncache.req.bits.cmd === MemoryOpConstants.M_XWR
          when (!uncache.io.enableOutstanding) {
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
  when (uncache.io.enableOutstanding) {
    uncacheResp <> lsq.io.uncache.resp
  }.otherwise {
    when (uncacheState === s_scalar_uncache) {
      uncacheResp <> lsq.io.uncache.resp
    }
  }

  // delay dcache refill for 1 cycle for better timing
  AddPipelineReg(uncacheReq, uncache.io.lsq.req, false.B)
  AddPipelineReg(uncache.io.lsq.resp, uncacheResp, false.B)

  // Lsq
  lsq.io.hartId := io.fromCtrl.hartId
  lsq.io.release := dcache.io.lsu.release
  lsq.io.brqRedirect <> redirect
  lsq.io.enq <> io.fromBackend.enqLsq
  lsq.io.lqCancelCnt <> io.toBackend.lqCancelCnt
  lsq.io.sqCancelCnt <> io.toBackend.sqCancelCnt
  lsq.io.lqDeq <> io.toBackend.lqDeq
  lsq.io.sqDeq <> io.toBackend.sqDeq
  lsq.io.lqDeqPtr <> io.toBackend.lqDeqPtr
  lsq.io.sqDeqPtr <> io.toBackend.sqDeqPtr
  lsq.io.ldvecFeedback <> vecExuBlock.io.toLsq.ldvecFeedback
  lsq.io.stvecFeedback <> vecExuBlock.io.toLsq.stvecFeedback
  lsq.io.flushSbuffer.empty := sbuffer.io.sbempty
  lsq.io.exceptionAddr.isStore := io.fromBackend.isStoreException
  lsq.io.issuePtrExt <> io.toBackend.stIssuePtr
  lsq.io.l2_hint.valid := l2Hint.valid
  lsq.io.l2_hint.bits.sourceId := l2Hint.bits.sourceId
  lsq.io.l2_hint.bits.isKeyword := l2Hint.bits.isKeyword
  lsq.io.tlb_hint <> mmu.io.tlbHint
  lsq.io.maControl.toStoreQueue <> memExuBlock.io.toLsq.sqControl
  lsq.io.uncacheOutstanding := csrCtrl.uncache_write_outstanding_enable
  lsq.io.debugTopDown.robHeadMissInDTlb := mmu.io.robHeadMissInDTlb
  lsq.io.ldout.foreach { x => x.ready := false.B }
  lsq.io.vecmmioStout.ready := false.B
  lsq.io.loadMisalignFull <> memExuBlock.io.toLsq.loadMisalignFull
  io.toBackend.lsqio.lqCanAccept := lsq.io.lqCanAccept
  io.toBackend.lsqio.sqCanAccept := lsq.io.sqCanAccept
  io.toBackend.lsqio.mmio := lsq.io.rob.mmio
  io.toBackend.lsqio.uop := lsq.io.rob.uop
  connectSamePort(lsq.io.rob, io.fromBackend.robLsqIO)

  // lsq to l2 CMO
  lsq.io.cmoOpReq <> dcache.io.cmoOpReq
  lsq.io.cmoOpResp <> dcache.io.cmoOpResp

  // SBuffer
  sbuffer.io.hartId := io.fromCtrl.hartId
  sbuffer.io.csrCtrl := csrCtrl
  sbuffer.io.memSetPattenDetected := dcache.io.memSetPattenDetected
  sbuffer.io.force_write := lsq.io.force_write
  sbuffer.io.in <> lsq.io.sbuffer
  sbuffer.io.in(0).valid := lsq.io.sbuffer(0).valid || vecExuBlock.io.sbuffer.valid
  sbuffer.io.in(0).bits := Mux1H(Seq(
    vecExuBlock.io.sbuffer.valid -> vecExuBlock.io.sbuffer.bits,
    lsq.io.sbuffer(0).valid -> lsq.io.sbuffer(0).bits
  ))
  vecExuBlock.io.sbuffer.ready := sbuffer.io.in(0).ready
  sbuffer.io.sqempty := lsq.io.sqEmpty
  sbuffer.io.dcache <> dcache.io.lsu.store
  // flush sbuffer
  val cmoFlush = lsq.io.flushSbuffer.valid
  val fenceFlush = io.fromBackend.flushSb
  val atomicsFlush = memExuBlock.io.flushSbuffer.valid || vecExuBlock.io.flushSbuffer.valid
  val stIsEmpty = sbuffer.io.flush.empty && uncache.io.flush.empty
  io.toBackend.sbIsEmpty := RegNext(stIsEmpty)

  // if both of them tries to flush sbuffer at the same time
  // something must have gone wrong
  assert(!(fenceFlush && atomicsFlush && cmoFlush))
  sbuffer.io.flush.valid := RegNext(fenceFlush || atomicsFlush || cmoFlush)
  uncache.io.flush.valid := sbuffer.io.flush.valid

  // Vector Execution Block
  vecExuBlock.io.fromCtrl.hartId := io.fromCtrl.hartId
  vecExuBlock.io.fromCtrl.redirect <> redirect
  vecExuBlock.io.fromCtrl.csr := csrCtrl
  vecExuBlock.io.fromCtrl.trigger := memTrigger
  vecExuBlock.io.fromBackend.issueVldu <> io.fromBackend.issueVldu
  vecExuBlock.io.toBackend.vstuIqFeedback <> io.toBackend.vstuIqFeedback
  vecExuBlock.io.toBackend.vlduIqFeedback <> io.toBackend.vlduIqFeedback
  vecExuBlock.io.toBackend.writebackVldu <> io.toBackend.writebackVldu
  vecExuBlock.io.fromMemExuBlock <> memExuBlock.io.toVecExuBlock
  vecExuBlock.io.fromPmp := mmu.io.toMemExuBlock.pmpResp.head
  vecExuBlock.io.flushSbuffer.empty := stIsEmpty
  Connection.connect(
    sink        = vecExuBlock.io.fromTlb,
    source      = mmu.io.toMemExuBlock.tlbResp.head,
    connectFn   = None,
    connectName = "vector execute block tlb request"
  )

  val vSegmentFlag = vecExuBlock.io.vSegmentFlag
  val vSegmentException = RegInit(false.B)
  when (DelayN(redirect.valid, 10) && vSegmentException) {
    vSegmentException := false.B
  }.elsewhen (vecExuBlock.io.exceptionInfo.valid) {
    vSegmentException := true.B
  }

  // Mem Execution Block
  memExuBlock.io.fromCtrl.hartId := io.fromCtrl.hartId
  memExuBlock.io.fromCtrl.redirect <> redirect
  memExuBlock.io.fromCtrl.csr := csrCtrl
  memExuBlock.io.fromCtrl.trigger := memTrigger
  memExuBlock.io.fromBackend.issueLda <> io.fromBackend.issueLda
  memExuBlock.io.fromBackend.issueSta <> io.fromBackend.issueSta
  memExuBlock.io.fromBackend.issueStd <> io.fromBackend.issueStd
  memExuBlock.io.fromBackend.issueHya <> io.fromBackend.issueHya
  memExuBlock.io.fromBackend.robLsqIO <> io.fromBackend.robLsqIO
  memExuBlock.io.fromVecExuBlock <> vecExuBlock.io.toMemExuBlock
  memExuBlock.io.fromLsq.rarNuke <> VecInit(lsq.io.ldu.ldld_nuke_query.map(_.resp))
  memExuBlock.io.fromLsq.rawNuke <> VecInit(lsq.io.ldu.stld_nuke_query.map(_.resp))
  memExuBlock.io.fromLsq.replay <> lsq.io.replay
  memExuBlock.io.fromLsq.mmioLdWriteback <> lsq.io.ldout(UncacheWBPort)
  memExuBlock.io.fromLsq.mmioLdData <> lsq.io.ld_raw_data(UncacheWBPort)
  memExuBlock.io.fromLsq.mmioStWriteback <> lsq.io.mmioStout
  memExuBlock.io.fromLsq.ncOut <> lsq.io.ncOut
  memExuBlock.io.fromLsq.maControl <> lsq.io.maControl.toStoreMisalignBuffer
  memExuBlock.io.fromUncache.zip(uncache.io.forward.map(_.resp)).foreach {case  x => x._1 <> x._2 }
  memExuBlock.io.fromPmp <> mmu.io.toMemExuBlock.pmpResp
  memExuBlock.io.toBackend.stIssue  <> io.toBackend.stIssue
  memExuBlock.io.toBackend.writebackLda <> io.toBackend.writebackLda
  memExuBlock.io.toBackend.writebackSta <> io.toBackend.writebackSta
  memExuBlock.io.toBackend.writebackHyuLda <> io.toBackend.writebackHyuLda
  memExuBlock.io.toBackend.writebackHyuSta <> io.toBackend.writebackHyuSta
  memExuBlock.io.toBackend.ldaIqFeedback <> io.toBackend.ldaIqFeedback
  memExuBlock.io.toBackend.staIqFeedback <> io.toBackend.staIqFeedback
  memExuBlock.io.toBackend.hyuIqFeedback <> io.toBackend.hyuIqFeedback
  memExuBlock.io.toBackend.ldCancel <> io.toBackend.ldCancel
  memExuBlock.io.toBackend.wakeup <> io.toBackend.wakeup
  memExuBlock.io.toLsq.out <> lsq.io.ldu.ldin
  memExuBlock.io.toLsq.addrUpdate <> lsq.io.sta.storeAddrIn
  memExuBlock.io.toLsq.excpUpdate <> lsq.io.sta.storeAddrInRe
  memExuBlock.io.toLsq.maskOut <> lsq.io.sta.storeMaskIn
  memExuBlock.io.toUncache.zip(uncache.io.forward.map(_.req)).foreach {case x => x._2 <> x._1 }
  memExuBlock.io.toPrefetch.ifetch <> io.toFrontend.ifetchPrefetch
  memExuBlock.io.toPrefetch.train <> prefetcher.io.fromMemExuBlock.train
  memExuBlock.io.toPrefetch.trainL1 <> prefetcher.io.fromMemExuBlock.trainL1
  memExuBlock.io.amoDCacheIO <> dcache.io.lsu.atomics
  memExuBlock.io.flushSbuffer.empty := stIsEmpty
  memExuBlock.io.lsTopdownInfo <> io.toBackend.lsTopdownInfo
  memExuBlock.io.debugLsInfo <> io.debugLS.debugLsInfo

  // writeback overwrite
  Connection.connect(
    sinkSeq     = io.toBackend.writebackStd,
    sourceSeq   = memExuBlock.io.toBackend.writebackStd,
    connectFn   = Some((sink: DecoupledIO[MemExuOutput], source: DecoupledIO[MemExuOutput]) => {
      sink.valid := source.fire && !FuType.storeIsAMO(source.bits.uop.fuType)
      sink.bits := source.bits
      source.ready := sink.ready
    }),
    connectName = "MemExuBlock writeback to backend"
  )

  // tlb requests: [[MemExuBlock]] -> [[MMU]]
  mmu.io.fromMemExuBlock.tlbReq.zip(memExuBlock.io.toTlb).zipWithIndex.foreach {
    case ((sink, source), i) =>
      if (i == 0) {
        val vecExuTlbValid = vecExuBlock.io.toTlb.req.valid
        sink.req.valid := source.req.valid || vecExuTlbValid
        sink.req.bits := ParallelPriorityMux(Seq(
          RegNext(vecExuTlbValid) -> RegEnable(vecExuBlock.io.toTlb.req.bits, vecExuTlbValid),
          source.req.valid -> source.req.bits
        ))
        sink.req_kill := source.req_kill // FIXME: req_kill is right?
        source.req.ready := sink.req.ready && !vecExuTlbValid
        vecExuBlock.io.toTlb.req.ready := sink.req.ready
      } else {
        sink <> source
      }
  }

  // tlb responses: [[MMU]] -> [[MemExuBlock]]
  memExuBlock.io.fromTlb.zip(mmu.io.toMemExuBlock.tlbResp).foreach {
    case (sink, source) =>
      sink.resp <> source
  }
  memExuBlock.io.fromTlb.zip(mmu.io.toMemExuBlock.hint).foreach {
    case (sink, source) =>
      sink.hint <> source
  }
  // tlb requests: [[MMU]] -> [[VecExuBlock]]
  mmu.io.fromPrefetch.tlbReq.zip(prefetcher.io.toTlb).foreach {
    case (sink, source) =>
      sink.req <> source.req
      sink.req_kill <> source.req_kill
  }
  // tlb responses: [[MMU]] -> [[Prefetcher]]
  prefetcher.io.fromTlb.zip(mmu.io.toPrefetch.tlbResp).foreach {
    case (sink, source) =>
      sink <> source
  }

  // dcache requests: [[MemExuBlock]] -> [[DCache]]
  (dcache.io.lsu.load ++ dcache.io.lsu.sta).zip(memExuBlock.io.toDCache).zipWithIndex.foreach {
    case ((sink: DCacheLoadIO, source), i) =>
      sink.connectSameOutPort(source)
      sink.req <> source.req

      if (i == 0) {
        sink.req.valid := vecExuBlock.io.toDCache.req.valid || source.req.valid
        sink.req.bits  := Mux1H(Seq(
          vecExuBlock.io.toDCache.req.valid -> vecExuBlock.io.toDCache.req.bits,
          source.req.valid -> source.req.bits
        ))
        vecExuBlock.io.toDCache.req.ready := sink.req.ready

        when (vSegmentFlag) {
          sink.connectSameOutPort(vecExuBlock.io.toDCache)
        }
      }

      when (vSegmentFlag) {
        source.req.ready := false.B
      } .otherwise {
        source.req.ready := sink.req.ready
      }

    case ((sink: DCacheStoreIO, source), i) =>
      sink.req.valid := source.req.valid
      source.req.ready := sink.req.ready
      sink.req.bits.cmd := source.req.bits.cmd
      sink.req.bits.vaddr := source.req.bits.vaddr
      sink.req.bits.instrtype := source.req.bits.instrtype
      sink.s1_paddr := source.s1_paddr_dup_dcache
      sink.s1_kill := source.s1_kill
      sink.s2_kill := source.s2_kill
      sink.s2_pc := source.s2_pc

    case _ =>
  }

  // dcache responses: [[DCache]] -> [[MemExuBlock]]
  memExuBlock.io.fromDCache.zip((dcache.io.lsu.load ++ dcache.io.lsu.sta)).zipWithIndex.foreach {
    case ((sink, source: DCacheLoadIO), i) =>
      source.connectSameInPort(sink)
      sink.resp <> source.resp
      if (i == 0) {
        vecExuBlock.io.fromDCache := DontCare
        vecExuBlock.io.fromDCache.resp.valid := source.resp.valid
        vecExuBlock.io.fromDCache.resp.bits := source.resp.bits
        vecExuBlock.io.fromDCache.s2_bank_conflict := source.s2_bank_conflict
      }

    case ((sink, source: DCacheStoreIO), i) =>
      sink := DontCare
      sink.resp.valid := source.resp.valid
      sink.resp.bits.miss := source.resp.bits.miss
      sink.resp.bits.replay := source.resp.bits.replay
      sink.resp.bits.tag_error := source.resp.bits.tag_error
      source.resp.ready := sink.resp.ready

    case _ =>
  }

  // TLD forward req: [[TL D channel]] -> [[MemExuBlock]]
  Connection.connect(
    sinkSeq     = memExuBlock.io.fromTL,
    sourceSeq   = dcache.io.lsu.forward_D,
    connectName = "MemExuBlock forward from TL"
  )

  // Lsq nuke query: [[MemExuBlock]] -> [[Lsq]]
  lsq.io.ldu.stld_nuke_query.zip(memExuBlock.io.toLsq.rawNuke).foreach {
    case (sink, source) =>
      sink.req <> source.req
      sink.revoke := source.revoke
  }
  lsq.io.ldu.ldld_nuke_query.zip(memExuBlock.io.toLsq.rarNuke).foreach {
    case (sink, source) =>
      sink.req <> source.req
      sink.revoke := source.revoke
  }

  // Miss Queue forward req: [[MemExuBlock]] -> [[MissQueue]]
  dcache.io.lsu.forward_mshr.zip(memExuBlock.io.toMissQueue).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      sink.mshrid := source.bits.mshrId
      sink.paddr := source.bits.paddr
  }

  // Miss Queue forward resp: [[MissQueue]] -> [[MemExuBlock]]
  memExuBlock.io.fromMissQueue.zip(dcache.io.lsu.forward_mshr).foreach {
    case (sink, source) =>
      sink.forwardMshr := source.forward_mshr
      sink.forwardData := source.forwardData
      sink.forwardResultValid := source.forward_result_valid
  }

  // Lsq forward req: [[MemExuBlock]] -> [[Lsq]]
  lsq.io.forward.map(_.req).zip(memExuBlock.io.toLsq.forward).foreach {
    case (sink, source) =>
      sink <> source
  }

  // Lsq forward resp: [[Lsq]] -> [[MemExuBlock]]
  memExuBlock.io.fromLsq.forward.zip(lsq.io.forward.map(_.resp)).foreach {
    case (sink, source) =>
      sink <> source
  }

  // SBuffer forward req: [[MemExuBlock]] -> [[SBuffer]]
  sbuffer.io.forward.map(_.req).zip(memExuBlock.io.toSBuffer).foreach {
    case (sink, source) =>
      sink <> source
  }

  // SBuffer forward resp: [[SBuffer]] -> [[MemExuBlock]]
  memExuBlock.io.fromSBuffer.zip(sbuffer.io.forward.map(_.resp)).foreach {
    case (sink, source) =>
      sink <> source
  }

  // Lsq data in: [[MemExuBlock]] -> [[Lsq]]
  lsq.io.std.storeDataIn.zip(memExuBlock.io.toBackend.writebackStd).take(VstuCnt).
    zip(vecExuBlock.io.toLsq.vstd).zipWithIndex.foreach {
    case (((sink, std), vstd), i) =>
      sink.valid := std.valid
      sink.bits := 0.U.asTypeOf(sink.bits.cloneType)
      sink.bits.uop := std.bits.uop
      sink.bits.data := std.bits.data
      std.ready := true.B

      if (i < VstuCnt) {
        when (vstd.valid) {
          sink.valid := true.B
          sink.bits := vstd.bits
          std.ready := false.B
        }
      }
  }

  // store prefetch
  // LdExuCnt fix prefetch issue.
  val prefetchTrainReq = Seq.fill(LdExuCnt)(prefetcher.io.toMemExuBlock.trainReq) ++ sbuffer.io.store_prefetch
  memExuBlock.io.fromPrefetch.zip(prefetchTrainReq).foreach {
    case (sink, source) =>
      source.bits match {
        case load: L1PrefetchReq =>
          sink.valid := source.valid
          sink.bits.fromL1PrefetchReqBundle(load)
          source.ready := sink.ready
        case store: StorePrefetchReq =>
          sink.valid := source.valid
          sink.bits.fromStorePrefetchReqBundle(store)
          source.ready := sink.ready
        case _ =>
      }
  }

  /**
    * NOTE: loadUnits(0) has higher bank conflict and miss queue arb priority than loadUnits(1) and loadUnits(2)
    * when loadUnits(1)/loadUnits(2) stage 0 is busy, hw prefetch will never use that pipeline.
    */
  val LowConfPorts = if (LduCnt == 2) Seq(1) else if (LduCnt == 3) Seq(1, 2) else Seq(0)
  LowConfPorts.map {case i => memExuBlock.io.fromPrefetch(i).bits.confidence := 0.U }
  memExuBlock.io.fromPrefetch.drop(LduCnt).foreach {
    case hyu => hyu.bits.confidence := 0.U
  }

  val canAcceptHighConfPrefetch = memExuBlock.io.toPrefetch.train.map(_.canAcceptHighConfPrefetch)
  val canAcceptLowConfPrefetch = memExuBlock.io.toPrefetch.train.map(_.canAcceptLowConfPrefetch)
  prefetcher.io.toMemExuBlock.trainReq.ready := (0 until LduCnt + HyuCnt).map {
    case i => {
      if (LowConfPorts.contains(i)) {
        canAcceptLowConfPrefetch(i)
      } else {
        Mux(
          prefetcher.io.toMemExuBlock.trainReq.bits.confidence === 1.U,
          canAcceptHighConfPrefetch(i),
          canAcceptLowConfPrefetch(i)
        )
      }
    }
  }.reduce(_ || _)


  // rollback: [[MemBlock]] -> [[Backend]]
  def selectOldestRedirect(xs: Seq[Valid[Redirect]]): Vec[Bool] = {
    val compareVec = (0 until xs.length).map(i => (0 until i).map(j => isAfter(xs(j).bits.robIdx, xs(i).bits.robIdx)))
    val resultOnehot = VecInit((0 until xs.length).map(i => Cat((0 until xs.length).map(j =>
      (if (j < i) !xs(j).valid || compareVec(i)(j)
      else if (j == i) xs(i).valid
      else !xs(j).valid || !compareVec(j)(i))
    )).andR))
    resultOnehot
  }
  val allRedirect = memExuBlock.io.toBackend.rollback ++ lsq.io.nack_rollback ++ lsq.io.nuke_rollback
  val oldestOneHot = selectOldestRedirect(allRedirect)
  val oldestRedirect = WireDefault(Mux1H(oldestOneHot, allRedirect))
  // memory replay would not cause IAF/IPF/IGPF
  oldestRedirect.bits.cfiUpdate.backendIAF := false.B
  oldestRedirect.bits.cfiUpdate.backendIPF := false.B
  oldestRedirect.bits.cfiUpdate.backendIGPF := false.B
  io.toBackend.memoryViolation := oldestRedirect

  // exception generate
  val atomicsExceptionInfo = RegEnable(memExuBlock.io.atomicsExceptionInfo, memExuBlock.io.atomicsExceptionInfo.valid)
  val misalignExceptionInfo = memExuBlock.io.misalignExceptionInfo
  val vSegmentExceptionInfo = RegEnable(vecExuBlock.io.exceptionInfo, vecExuBlock.io.exceptionInfo.valid)

  val atomicsException = RegInit(false.B)
  when (DelayN(redirect.valid, 10) && atomicsException) {
    atomicsException := false.B
  }.elsewhen (memExuBlock.io.atomicsExceptionInfo.valid) {
    atomicsException := true.B
  }

// wrapper
  // whether vaddr need ext or is hyper inst:
  // VaNeedExt: atomicsException -> false; misalignBufExceptionOverwrite -> true; vSegmentException -> false
  // IsHyper: atomicsException -> false; vSegmentException -> false
  val atomicsExceptionReq = Wire(ValidIO(new ExceptionAddrIO))
  atomicsExceptionReq.valid := atomicsException
  atomicsExceptionReq.bits  := DontCare
  atomicsExceptionReq.bits.isHyper := false.B
  atomicsExceptionReq.bits.vaNeedExt := false.B
  atomicsExceptionReq.bits.vaddr := atomicsExceptionInfo.bits.vaddr
  atomicsExceptionReq.bits.gpaddr := atomicsExceptionInfo.bits.gpaddr
  atomicsExceptionReq.bits.isForVSnonLeafPTE := atomicsExceptionInfo.bits.isForVSnonLeafPTE

  val misalignExceptionReq = Wire(ValidIO(new ExceptionAddrIO))
  misalignExceptionReq.valid := misalignExceptionInfo.valid
  misalignExceptionReq.bits := DontCare
  misalignExceptionReq.bits.isHyper := misalignExceptionInfo.bits.isHyper
  misalignExceptionReq.bits.vaNeedExt := true.B
  misalignExceptionReq.bits.vaddr := misalignExceptionInfo.bits.vaddr
  misalignExceptionReq.bits.gpaddr := misalignExceptionInfo.bits.gpaddr
  misalignExceptionReq.bits.isForVSnonLeafPTE := misalignExceptionInfo.bits.isForVSnonLeafPTE

  val vSegmentExceptionReq = Wire(ValidIO(new ExceptionAddrIO))
  vSegmentExceptionReq.valid := vSegmentException
  vSegmentExceptionReq.bits := DontCare
  vSegmentExceptionReq.bits.isHyper := false.B
  vSegmentExceptionReq.bits.vaNeedExt := false.B
  vSegmentExceptionReq.bits.vaddr := vSegmentExceptionInfo.bits.vaddr
  vSegmentExceptionReq.bits.vstart := vSegmentExceptionInfo.bits.vstart
  vSegmentExceptionReq.bits.vl := vSegmentExceptionInfo.bits.vl
  vSegmentExceptionReq.bits.gpaddr := vSegmentExceptionInfo.bits.gpaddr
  vSegmentExceptionReq.bits.isForVSnonLeafPTE := vSegmentExceptionInfo.bits.isForVSnonLeafPTE

  val lsqExceptionReq = Wire(ValidIO(new ExceptionAddrIO))
  lsqExceptionReq.valid := true.B
  lsqExceptionReq.bits := lsq.io.exceptionAddr

  val exceptionReqs = VecInit(Seq(
    atomicsExceptionReq,
    misalignExceptionReq,
    vSegmentExceptionReq,
    lsqExceptionReq
  ))
  exceptionGen.io.redirect <> redirect
  exceptionGen.io.tlbCsr  <> tlbcsr
  exceptionGen.io.in <> exceptionReqs
  connectSamePort(io.toBackend.lsqio, exceptionGen.io.out.bits)

  // to backend
  io.toBackend.otherFastWakeup := DontCare // not support
  io.toBackend.topToBackendBypass match { case x =>
    x.hartId := io.fromCtrl.hartId
    x.externalInterrupt.msip  := outer.clintIntSink.in.head._1(0)
    x.externalInterrupt.mtip  := outer.clintIntSink.in.head._1(1)
    x.externalInterrupt.meip  := outer.plicIntSink.in.head._1(0)
    x.externalInterrupt.seip  := outer.plicIntSink.in.last._1(0)
    x.externalInterrupt.debug := outer.debugIntSink.in.head._1(0)
    x.externalInterrupt.nmi.nmi_31 := outer.nmiIntSink.in.head._1(0)
    x.externalInterrupt.nmi.nmi_43 := outer.nmiIntSink.in.head._1(1)
    x.msiInfo   := DelayNWithValid(io.bypass.fromTopToBackend.msiInfo, 1)
    x.clintTime := DelayNWithValid(io.bypass.fromTopToBackend.clintTime, 1)
  }

  io.memInfo.sqFull := RegNext(lsq.io.sqFull)
  io.memInfo.lqFull := RegNext(lsq.io.lqFull)
  io.memInfo.dcacheMSHRFull := RegNext(dcache.io.mshrFull)

  // bypass
  io.bypass.innerHartId <> io.fromCtrl.hartId
  io.bypass.innerResetVector <> RegNext(io.bypass.outerResetVector)
  io.bypass.outerCpuHalt <> io.bypass.fromBackendToTop.cpuHalted
  io.bypass.outerCpuCriticalError <> io.bypass.fromBackendToTop.cpuCriticalError
  io.bypass.outerBeuErrorsICache <> RegNext(io.bypass.innerBeuErrorsICache)
  io.bypass.outerL2PfEnable <> io.bypass.innerL2PfEnable
  io.bypass.outerHcPerfEvents <> io.bypass.innerHcPerfEvents

  // Initialize when unenabled difftest.
  sbuffer.io.vecDifftestInfo := DontCare
  lsq.io.sbufferVecDifftestInfo := DontCare
  vecExuBlock.io.vecDifftestInfo := DontCare
  if (env.EnableDifftest) {
    sbuffer.io.vecDifftestInfo.zipWithIndex.foreach {
      case (sbufferPort, index) =>
        if (index == 0) {
          val vSegmentDifftestValid = vecExuBlock.io.vecDifftestInfo.valid
          sbufferPort.valid := Mux(
            vSegmentDifftestValid,
            vecExuBlock.io.vecDifftestInfo.valid,
            lsq.io.sbufferVecDifftestInfo(0).valid
          )
          sbufferPort.bits := Mux(
            vSegmentDifftestValid,
            vecExuBlock.io.vecDifftestInfo.bits,
            lsq.io.sbufferVecDifftestInfo(0).bits
          )

          vecExuBlock.io.vecDifftestInfo.ready  := sbufferPort.ready
          lsq.io.sbufferVecDifftestInfo(0).ready := sbufferPort.ready
        } else {
           sbufferPort <> lsq.io.sbufferVecDifftestInfo(index)
        }
    }
  }

  // trace interface
  val traceToL2Top = io.bypass.traceCoreInterfaceBypass.toL2Top
  val traceFromBackend = io.bypass.traceCoreInterfaceBypass.fromBackend
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

  io.toBackend.storeDebugInfo := DontCare
  // store event difftest information
  if (env.EnableDifftest) {
    (0 until EnsbufferWidth).foreach{i =>
        io.toBackend.storeDebugInfo(i).robidx := sbuffer.io.vecDifftestInfo(i).bits.robIdx
        sbuffer.io.vecDifftestInfo(i).bits.pc := io.toBackend.storeDebugInfo(i).pc
    }
  }

  // reset tree of MemBlock
  if (p(DebugOptionsKey).ResetGen) {
    val leftResetTree = ResetGenNode(
      Seq(
        ModuleNode(lsq),
        ModuleNode(prefetcher),
        ModuleNode(mmu),
      )
    )
    val rightResetTree = ResetGenNode(
      Seq(
        ModuleNode(sbuffer),
        ModuleNode(dcache),
        ModuleNode(l1dToL2Buffer),
        CellNode(io.bypass.resetBackend)
      )
    )
    ResetGen(leftResetTree, reset, sim = false)
    ResetGen(rightResetTree, reset, sim = false)
  } else {
    io.bypass.resetBackend := DontCare
  }
  io.bypass.resetInFrontendBypass.toL2Top := io.bypass.resetInFrontendBypass.fromFrontend

  // top-down info
  dcache.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  mmu.io.robHeadVaddr := io.debugTopDown.robHeadVaddr
  lsq.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  io.debugTopDown.toCore.robHeadMissInDCache := dcache.io.debugTopDown.robHeadMissInDCache
  io.debugTopDown.toCore.robHeadTlbReplay := lsq.io.debugTopDown.robHeadTlbReplay
  io.debugTopDown.toCore.robHeadTlbMiss := lsq.io.debugTopDown.robHeadTlbMiss
  io.debugTopDown.toCore.robHeadLoadVio := lsq.io.debugTopDown.robHeadLoadVio
  io.debugTopDown.toCore.robHeadLoadMSHR := lsq.io.debugTopDown.robHeadLoadMSHR
  dcache.io.debugTopDown.robHeadOtherReplay := lsq.io.debugTopDown.robHeadOtherReplay
  dcache.io.debugRolling := io.debugRolling

  val hyLdDeqCount = PopCount(io.fromBackend.issueHya.map(x => x.valid && FuType.isLoad(x.bits.uop.fuType)))
  val hyStDeqCount = PopCount(io.fromBackend.issueHya.map(x => x.valid && FuType.isStore(x.bits.uop.fuType)))
  val ldDeqCount = PopCount(io.fromBackend.issueLda.map(_.valid)) +& hyLdDeqCount
  val stDeqCount = PopCount(io.fromBackend.issueSta.take(StaCnt).map(_.valid)) +& hyStDeqCount
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

  val perfFromUnits = (memExuBlock +: Seq(sbuffer, lsq, dcache)).flatMap(_.getPerfEvents)
  val perfBlock = Seq(("ldDeqCount", ldDeqCount), ("stDeqCount", stDeqCount))
  val perfFromPTW = mmu.getPerfEvents
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

class MemBlock()(implicit p: Parameters) extends LazyModule with HasXSParameter {
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
