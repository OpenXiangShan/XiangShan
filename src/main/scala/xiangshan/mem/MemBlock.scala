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
import device.MsiInfoBundle
import system.SoCParamsKey
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.frontend.HasInstrMMIOConst
import xiangshan.backend.datapath.NewPipelineConnect
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu._
import xiangshan.backend.fu.FuType._
import xiangshan.backend.fu.util.{HasCSRConst, SdtrigExt}
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput, connectSamePort}
import xiangshan.backend.ctrlblock.{DebugLSIO, LsTopdownInfo}
import xiangshan.backend.exu.MemExeUnit
import xiangshan.backend.rob.{RobDebugRollingIO, RobPtr, RobLsqIO}
import xiangshan.backend.{TopToBackendBundle, BackendToTopBundle}
import xiangshan.mem._
import xiangshan.mem.mdp._
import xiangshan.mem.prefetch.{BasePrefecher, L1Prefetcher, SMSParams, SMSPrefetcher}
import xiangshan.cache._
import xiangshan.cache.mmu._
import coupledL2.{PrefetchRecv, CMOReq, CMOResp}
import java.nio.file.Path

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

class Std(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits := 0.U.asTypeOf(io.out.bits)
  io.out.bits.res.data := io.in.bits.data.src(0)
  io.out.bits.ctrl.robIdx := io.in.bits.ctrl.robIdx
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
}

class MemBlockExceptionGen(numSources: Int)(implicit p: Parameters) extends XSModule
  with HasXSParameter
  with HasMemBlockParameters
  with HasTlbConst
  with HasCSRConst
{
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val tlbCsr   = Input(new TlbCsrBundle)
    val in       = Vec(numSources, Flipped(ValidIO(new ExceptionAddrIO)))
    val out      = ValidIO(new ExceptionAddrIO)
  })

  val highestException = PriorityMux(io.in.map(in => in.valid -> in))
  val exceptionVaddr = highestException.bits.vaddr
  val exceptionVaNeedExt = highestException.bits.vaNeedExt
  val exceptionIsHyper = highestException.bits.isHyper

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

    val exceptionVa = Wire(UInt(XLEN.W))
    when (vaNeedExt) {
      exceptionVa := Mux1H(Seq(
        (!(vmEnable || s2xlateEnable)) -> bareAddr,
        (!onlyS2 && (Sv39 || Sv39x4))  -> sv39Addr,
        (!onlyS2 && (Sv48 || Sv48x4))  -> sv48Addr,
        ( onlyS2 && (Sv39 || Sv39x4))  -> sv39x4Addr,
        ( onlyS2 && (Sv48 || Sv48x4))  -> sv48x4Addr,
      ))
    } .otherwise {
      exceptionVa := vaddr
    }
    exceptionVa
  }

  io.out.valid := RegNext(highestException.valid)
  io.out.bits := DontCare
  io.out.bits.vaddr := RegNext(
    GenExceptionVa(io.tlbCsr.priv.dmode, io.tlbCsr.priv.virt || exceptionIsHyper, exceptionVaNeedExt,
    io.tlbCsr.satp, io.tlbCsr.vsatp, io.tlbCsr.hgatp, exceptionVaddr)
  )
  io.out.bits.vstart := RegNext(highestException.bits.vstart)
  io.out.bits.vl     := RegNext(highestException.bits.vl)
  io.out.bits.gpaddr := RegNext(highestException.bits.gpaddr)
  io.out.bits.isForVSnonLeafPTE := RegNext(highestException.bits.gpaddr)
}

abstract class MemBlockBundle(implicit val p: Parameters) extends Bundle with HasMemBlockParameters

class BackendToMemBlockIO(implicit p: Parameters) extends MemBlockBundle {
  val enqLsq  = Flipped(new LsqEnqIO)
  val flushSb = Output(Bool())
  val rob     = new RobLsqIO
  val sfence  = Output(new SfenceBundle)
  val tlbCsr  = Output(new TlbCsrBundle)
  val isStoreException = Output(Bool())
  val isVlsException = Output(Bool())

  val loadPc    = Vec(LduCnt, Output(UInt(VAddrBits.W))) // for hw prefetch
  val storePc   = Vec(StaCnt, Output(UInt(VAddrBits.W))) // for hw prefetch
  val hybridPc  = Vec(HyuCnt, Output(UInt(VAddrBits.W))) // for hw prefetch

  val issueLda  = MixedVec(Seq.fill(LduCnt)(DecoupledIO(new MemExuInput)))
  val issueSta  = MixedVec(Seq.fill(StaCnt)(DecoupledIO(new MemExuInput)))
  val issueStd  = MixedVec(Seq.fill(StdCnt)(DecoupledIO(new MemExuInput)))
  val issueHya  = MixedVec(Seq.fill(HyuCnt)(DecoupledIO(new MemExuInput)))
  val issueVldu = MixedVec(Seq.fill(VlduCnt)(DecoupledIO(new MemExuInput(isVector=true))))

  def issueUops = issueLda ++ issueSta ++ issueStd ++ issueHya ++ issueVldu
}

class MemBlockToBackendIO(implicit p: Parameters) extends MemBlockBundle {
  val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
  val sqDeq       = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
  val lqDeq       = Output(UInt(log2Up(CommitWidth + 1).W))
  // used by VLSU issue queue, the vector store would wait all store before it, and the vector load would wait all load
  val sqDeqPtr    = Output(new SqPtr)
  val lqDeqPtr    = Output(new LqPtr)
  val stIssue     = Vec(StAddrCnt, ValidIO(new MemExuInput))
  val stIssuePtr  = Output(new SqPtr())
  val memoryViolation = ValidIO(new Redirect)
  val sbIsEmpty   = Output(Bool())


  val lsqio = new Bundle {
    val vaddr       = Output(UInt(XLEN.W))
    val vstart      = Output(UInt((log2Up(VLEN) + 1).W))
    val vl          = Output(UInt((log2Up(VLEN) + 1).W))
    val gpaddr      = Output(UInt(XLEN.W))
    val isForVSnonLeafPTE = Output(Bool())
    val mmio        = Output(Vec(LoadPipelineWidth, Bool()))
    val uop         = Output(Vec(LoadPipelineWidth, new DynInst))
    val lqCanAccept = Output(Bool())
    val sqCanAccept = Output(Bool())
  }
  val writebackLda    = Vec(LduCnt, DecoupledIO(new MemExuOutput))
  val writebackSta    = Vec(StaCnt, DecoupledIO(new MemExuOutput))
  val writebackStd    = Vec(StdCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuLda = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuSta = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackVldu   = Vec(VlduCnt, DecoupledIO(new MemExuOutput(isVector = true)))
  val ldaIqFeedback   = Vec(LduCnt, new MemRSFeedbackIO)
  val staIqFeedback   = Vec(StaCnt, new MemRSFeedbackIO)
  val hyuIqFeedback   = Vec(HyuCnt, new MemRSFeedbackIO)
  val vstuIqFeedback  = Vec(VstuCnt, new MemRSFeedbackIO(isVector = true))
  val vlduIqFeedback  = Vec(VlduCnt, new MemRSFeedbackIO(isVector = true))
  val ldCancel        = Vec(LdExuCnt, new LoadCancelIO)
  val wakeup          = Vec(LdExuCnt, Valid(new DynInst))

  val lsTopdownInfo = Vec(LdExuCnt, Output(new LsTopdownInfo))
  val s3DelayedLoadError = Vec(LdExuCnt, Output(Bool()))

  def writeBack: Seq[DecoupledIO[MemExuOutput]] = {
    writebackSta ++
      writebackHyuLda ++ writebackHyuSta ++
      writebackLda ++
      writebackVldu ++
      writebackStd
  }
}

class FrontendToMemBlockIO(implicit p: Parameters) extends MemBlockBundle {
  val itlb = new TlbPtwIO()
}

class MemBlockToFrontendIO(implicit p: Parameters) extends MemBlockBundle {
  val ifetchPrefetch = Vec(LduCnt, ValidIO(new SoftIfetchPrefetchBundle))
}

class MemBlockPathThroughIO(implicit p: Parameters) extends MemBlockBundle {
  val fromBackendToTop = Flipped(new BackendToTopBundle)
  val fromTopToBackend = new TopToBackendBundle
  val fromTop = Flipped(new Bundle() {
    val msiInfo   = ValidIO(new MsiInfoBundle)
    val clintTime = ValidIO(UInt(64.W))
  })

  val innerHartId       = Output(UInt(hartIdLen.W))
  val innerResetVector  = Output(UInt(PAddrBits.W))
  val outerResetVector  = Input(UInt(PAddrBits.W))
  val outerCpuHalt      = Output(Bool())
  val innerBeuErrorsIcache = Input(new L1BusErrorUnitInfo)
  val outerBeuErrorsIcache = Output(new L1BusErrorUnitInfo)
  val innerL2PfEnable   = Input(Bool())
  val outerL2PfEnable   = Output(Bool())
  val innerHcPerfEvents = Output(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))
  val outerHcPerfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))

  // reset signals of frontend & backend are generated in memblock
  val resetBackend = Output(Reset())
  // Reset singal from frontend.
  val resetInFrontendBypass = new Bundle{
    val fromFrontend = Input(Bool())
    val toL2Top      = Output(Bool())
  }
}

class MemCoreTopDownIO extends Bundle {
  val robHeadMissInDCache = Output(Bool())
  val robHeadTlbReplay    = Output(Bool())
  val robHeadTlbMiss      = Output(Bool())
  val robHeadLoadVio      = Output(Bool())
  val robHeadLoadMSHR     = Output(Bool())
}

class MemBlockInlined()(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  override def shouldBeInlined: Boolean = true

  val l2PfSenderOpt = coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new PrefetchRecv)
  )
  val l3PfSenderOpt = if (p(SoCParamsKey).L3CacheParamsOpt.nonEmpty) coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new huancun.PrefetchRecv)
  ) else None

  val dcache  = LazyModule(new DCacheWrapper())
  val uncache = LazyModule(new Uncache())
  val memExuBlock = LazyModule(new MemExuBlock)
  val ptw     = LazyModule(new L2TLBWrapper())

  val l1dToL2Buffer = if (coreParams.dcacheParametersOpt.nonEmpty) LazyModule(new TLBuffer) else null
  val ptwToL2Buffer = if (!coreParams.softPTW) LazyModule(new TLBuffer) else null
  if (!coreParams.softPTW) {
    ptwToL2Buffer.node := ptw.node
  }

  val dcachePort = TLNameNode("dcache_client") // to keep dcache-L2 port name
  val cmoSender  = if (HasCMO) Some(BundleBridgeSource(() => DecoupledIO(new CMOReq))) else None
  val cmoReciver = if (HasCMO) Some(BundleBridgeSink(Some(() => DecoupledIO(new CMOResp)))) else None
  val frontendBridge = LazyModule(new FrontendBridge)

  // interrupt sinks
  val clintIntSink  = IntSinkNode(IntSinkPortSimple(1, 2))
  val debugIntSink  = IntSinkNode(IntSinkPortSimple(1, 1))
  val plicIntSink   = IntSinkNode(IntSinkPortSimple(2, 1))
  val nmiIntSink    = IntSinkNode(IntSinkPortSimple(1, (new NonmaskableInterruptIO).elements.size))

  lazy val module = new MemBlockInlinedImp(this)
}

class MemBlockInlinedIO(implicit p: Parameters) extends MemBlockBundle {
  // from
  val fromCtrl = new Bundle() {
    val redirect  = Flipped(ValidIO(new Redirect))
    val hartId    = Input(UInt(hartIdLen.W))
    val csrCtrl   = Flipped(new CustomCSRCtrlIO)
  }
  val fromFrontend  = Flipped(new FrontendToMemBlockIO)
  val fromBackend   = Flipped(new BackendToMemBlockIO)

  // to
  val toFrontend  = new MemBlockToFrontendIO
  val toBackend   = new MemBlockToBackendIO

  // path through
  val bypass = new MemBlockPathThroughIO
  val debugTopDown = new Bundle {
    val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
    val toCore = new MemCoreTopDownIO
  }
  val debugRolling = Flipped(new RobDebugRollingIO)
  val error = ValidIO(new L1CacheErrorInfo)
  val memInfo = new Bundle {
    val sqFull = Output(Bool())
    val lqFull = Output(Bool())
    val dcacheMSHRFull = Output(Bool())
  }
  val debugLsInfo = new DebugLSIO
  val l2Hint = Input(Valid(new L2ToL1Hint()))
  val l2PfqBusy = Input(Bool())
  val l2TlbReq = Flipped(new TlbRequestIO(nRespDups = 2))
  val l2PmpResp = new PMPRespBundle
}

class MemBlockInlinedImp(wrapper: MemBlockInlined) extends LazyModuleImp(wrapper)
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

  private val (ldDTLBIdx, stDTLBIdx, pfDTLBIdx) = (0, 1, 2)
  private val TlbSubSizeVec = Seq(LduCnt + HyuCnt + 1, StaCnt, 2) // (load + hyu + stream pf, store, sms+l2bop)
  private val DTlbSize = TlbSubSizeVec.sum
  private val TlbStartVec = TlbSubSizeVec.scanLeft(0)(_ + _).dropRight(1)
  private val TlbEndVec = TlbSubSizeVec.scanLeft(0)(_ + _).drop(1)

  val io = IO(new MemBlockInlinedIO)

  private val fromCtrl = io.fromCtrl
  private val (fromBackend, toBackend) = (io.fromBackend, io.toBackend)
  private val (fromFrontend, toFrontend) = (io.fromFrontend, io.toFrontend)
  private val bypass = io.bypass

  dontTouch(bypass.innerHartId)
  dontTouch(bypass.innerResetVector)
  dontTouch(bypass.outerResetVector)
  dontTouch(bypass.outerCpuHalt)
  dontTouch(bypass.innerBeuErrorsIcache)
  dontTouch(bypass.outerBeuErrorsIcache)
  dontTouch(bypass.innerL2PfEnable)
  dontTouch(bypass.outerL2PfEnable)

  private val dcache      = wrapper.dcache.module
  private val ptw         = wrapper.ptw.module
  private val ptwToL2Buffer = wrapper.ptwToL2Buffer.module
  private val l1dToL2Buffer = wrapper.l1dToL2Buffer.module

  val uncache     = wrapper.uncache.module
  val memExuBlock = wrapper.memExuBlock.module
  val vecExuBlock = Module(new VecExuBlock)
  val ldDTLB      = Seq(Module(new TLBNonBlock(LduCnt + HyuCnt + 1, 2, ldtlbParams)))
  val stDTLB      = Seq(Module(new TLBNonBlock(StaCnt, 1, sttlbParams)))
  val pfDTLB      = Seq(Module(new TLBNonBlock(2, 2, pftlbParams)))
  val pmp         = Module(new PMP())
  val pmpCheckers = Seq.fill(DTlbSize)(Module(new PMPChecker(4, leaveHitMux = true)))
  val lsq         = Module(new LsqWrapper)
  val sbuffer     = Module(new Sbuffer)
  val prefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
    case _: SMSParams =>
      val sms = Module(new SMSPrefetcher())
      sms.io_agt_en := GatedRegNextN(fromCtrl.csrCtrl.l1D_pf_enable_agt, 2, Some(false.B))
      sms.io_pht_en := GatedRegNextN(fromCtrl.csrCtrl.l1D_pf_enable_pht, 2, Some(false.B))
      sms.io_act_threshold := GatedRegNextN(fromCtrl.csrCtrl.l1D_pf_active_threshold, 2, Some(12.U))
      sms.io_act_stride := GatedRegNextN(fromCtrl.csrCtrl.l1D_pf_active_stride, 2, Some(30.U))
      sms.io_stride_en := false.B
      sms.io_dcache_evict <> dcache.io.sms_agt_evict_req
      sms
  }
  prefetcherOpt.foreach{ pf => pf.io.l1_req.ready := false.B }
  val l1PrefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
    case _ =>
      val hartId = p(XSCoreParamsKey).HartId
      val l1Prefetcher = Module(new L1Prefetcher())
      l1Prefetcher.io.enable := Constantin.createRecord(s"enableL1StreamPrefetcher$hartId", initValue = true)
      l1Prefetcher.pf_ctrl <> dcache.io.pf_ctrl
      l1Prefetcher.l2PfqBusy := io.l2PfqBusy

      // stride will train on miss or prefetch hit
      l1Prefetcher.stride_train.zip(memExuBlock.io.toPrefetch.trainL1).zip(fromBackend.loadPc).foreach {
        case ((pf, source), loadPc) =>
          pf.valid := source.req.valid && source.req.bits.isFirstIssue && (
          source.req.bits.miss || isFromStride(source.req.bits.metaPrefetch)
        )
        pf.bits := source.req.bits
        pf.bits.uop.pc := RegEnable(RegEnable(loadPc, source.s1PrefetchSpec), source.s2PrefetchSpec)
      }
      l1Prefetcher
  }
  val exceptionGen  = Module(new MemBlockExceptionGen(4))


  // init
  val redirect = RegNextWithEnable(fromCtrl.redirect)
  val csrCtrl = DelayN(fromCtrl.csrCtrl, 2)
  val l2Hint = RegNext(io.l2Hint)
  val sfence = RegNext(RegNext(fromBackend.sfence))
  val tlbCsr = RegNext(RegNext(fromBackend.tlbCsr))

  dcache.io.csr.distribute_csr <> csrCtrl.distribute_csr
  dcache.io.l2_pf_store_only := RegNext(fromCtrl.csrCtrl.l2_pf_store_only, false.B)
  io.error <> DelayNWithValid(dcache.io.error, 2)
  when(!csrCtrl.cache_error_enable){
    io.error.bits.report_to_beu := false.B
    io.error.valid := false.B
  }

  val dTLB = ldDTLB ++ stDTLB ++ pfDTLB

  val ptwIO = Wire(new VectorTlbPtwIO(DTlbSize))
  val dtlbRepeater  = PTWNewFilter(ldtlbParams.fenceDelay, ptwIO, ptw.io.tlb(1), sfence, tlbCsr, l2tlbParams.dfilterSize)
  val itlbRepeater3 = PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, fromFrontend.itlb, ptw.io.tlb(0), sfence, tlbCsr)

  val dTLBReqs = dTLB.map(_.io.requestor).flatten
  val dTLBPmps = dTLB.map(_.io.pmp).flatten
  dTLB.map {
    case tlb =>
      tlb.io.hartId := io.fromCtrl.hartId
      tlb.io.sfence := sfence
      tlb.io.csr    := tlbCsr
      tlb.io.flushPipe.map(x => x := false.B)
      tlb.io.redirect := redirect
  }
  if (refillBothTlb) {
    require(ldtlbParams.outReplace == sttlbParams.outReplace)
    require(ldtlbParams.outReplace == hytlbParams.outReplace)
    require(ldtlbParams.outReplace == pftlbParams.outReplace)
    require(ldtlbParams.outReplace)

    val replace = Module(new TlbReplace(DTlbSize, ldtlbParams))
    replace.io.apply_sep(ldDTLB.map(_.io.replace) ++ stDTLB.map(_.io.replace) ++ pfDTLB.map(_.io.replace),  ptwIO.resp.bits.data.s1.entry.tag)
  } else {
    // TODO: there will be bugs in TlbReplace when outReplace enable, since the order of Hyu is not right.
    if (ldtlbParams.outReplace) {
      val replaceLd = Module(new TlbReplace(LduCnt + 1, ldtlbParams))
      replaceLd.io.apply_sep(ldDTLB.map(_.io.replace), ptwIO.resp.bits.data.s1.entry.tag)
    }
    if (hytlbParams.outReplace) {
      val replacHy = Module(new TlbReplace(HyuCnt, hytlbParams))
      replacHy.io.apply_sep(ldDTLB.map(_.io.replace), ptwIO.resp.bits.data.s1.entry.tag)
    }
    if (sttlbParams.outReplace) {
      val replaceSt = Module(new TlbReplace(StaCnt, sttlbParams))
      replaceSt.io.apply_sep(stDTLB.map(_.io.replace), ptwIO.resp.bits.data.s1.entry.tag)
    }
    if (pftlbParams.outReplace) {
      val replacePf = Module(new TlbReplace(2, pftlbParams))
      replacePf.io.apply_sep(pfDTLB.map(_.io.replace), ptwIO.resp.bits.data.s1.entry.tag)
    }
  }

  // ptw request
  val ptwRespNext = RegEnable(ptwIO.resp.bits, ptwIO.resp.valid)
  val ptwRespV = RegNext(ptwIO.resp.valid  && !(sfence.valid && tlbCsr.satp.changed && tlbCsr.hgatp.changed), init = false.B)
  dTLB.flatMap(a => a.io.ptw.req)
    .zipWithIndex
    .foreach{ case (tlb, i) =>
      tlb.ready := ptwIO.req(i).ready
      ptwIO.req(i).bits := tlb.bits
    val vector_hit = if (refillBothTlb) Cat(ptwRespNext.vector).orR
      else if (i < TlbEndVec(ldDTLBIdx)) Cat(ptwRespNext.vector.slice(TlbStartVec(ldDTLBIdx), TlbEndVec(ldDTLBIdx))).orR
      else if (i < TlbEndVec(stDTLBIdx)) Cat(ptwRespNext.vector.slice(TlbStartVec(stDTLBIdx), TlbEndVec(stDTLBIdx))).orR
      else                                 Cat(ptwRespNext.vector.slice(TlbStartVec(pfDTLBIdx), TlbEndVec(pfDTLBIdx))).orR
    ptwIO.req(i).valid := tlb.valid && !(ptwRespV && vector_hit && ptwRespNext.data.hit(tlb.bits.vpn, tlbCsr.satp.asid, tlbCsr.vsatp.asid, tlbCsr.hgatp.vmid, allType = true, ignoreAsid = true))
  }
  dTLB.foreach(_.io.ptw.resp.bits := ptwRespNext.data)
  if (refillBothTlb) {
    dTLB.foreach(_.io.ptw.resp.valid := ptwRespV && Cat(ptwRespNext.vector).orR)
  } else {
    ldDTLB.foreach(_.ptw.resp.valid := ptwRespV && Cat(ptwRespNext.vector.slice(TlbStartVec(ldDTLBIdx), TlbEndVec(ldDTLBIdx))).orR)
    stDTLB.foreach(_.ptw.resp.valid := ptwRespV && Cat(ptwRespNext.vector.slice(TlbStartVec(stDTLBIdx), TlbEndVec(stDTLBIdx))).orR)
    pfDTLB.foreach(_.ptw.resp.valid := ptwRespV && Cat(ptwRespNext.vector.slice(TlbStartVec(pfDTLBIdx), TlbEndVec(pfDTLBIdx))).orR)
  }
  ldDTLB.foreach(_.ptw.resp.bits.getGpa := Cat(ptwRespNext.getGpa.take(LduCnt + HyuCnt + 1)).orR)
  stDTLB.foreach(_.ptw.resp.bits.getGpa := Cat(ptwRespNext.getGpa.slice(LduCnt + HyuCnt + 1, LduCnt + HyuCnt + 1 + StaCnt)).orR)
  pfDTLB.foreach(_.ptw.resp.bits.getGpa := Cat(ptwRespNext.getGpa.drop(LduCnt + HyuCnt + 1 + StaCnt)).orR)

  pmp.io.distribute_csr <> csrCtrl.distribute_csr
  val pmpCheck = pmpCheckers.map(_.io)
  for ((p,d) <- pmpCheck zip dTLB.map(_.io.pmp).flatten) {
    p.apply(tlbCsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
    require(p.req.bits.size.getWidth == d.bits.size.getWidth)
  }

  // ptw
  ptw.io.hartId := fromCtrl.hartId
  ptw.io.sfence <> sfence
  ptw.io.csr.tlb <> tlbCsr
  ptw.io.csr.distribute_csr <> csrCtrl.distribute_csr
  val perfEventsPTW = Wire(Vec(19, new PerfEvent))
  if (!coreParams.softPTW) {
    perfEventsPTW := ptw.getPerf
  } else {
    perfEventsPTW := DontCare
  }
  ptwIO.resp.ready := true.B

  // trigger
  val memBlockTrigger = Wire(new CsrTriggerBundle)
  val tdata = RegInit(VecInit(Seq.fill(TriggerNum)(0.U.asTypeOf(new MatchTriggerIO))))
  val tEnable = RegInit(VecInit(Seq.fill(TriggerNum)(false.B)))
  tEnable := csrCtrl.mem_trigger.tEnableVec
  when(csrCtrl.mem_trigger.tUpdate.valid) {
    tdata(csrCtrl.mem_trigger.tUpdate.bits.addr) := csrCtrl.mem_trigger.tUpdate.bits.tdata
  }
  val triggerCanRaiseBpExp = csrCtrl.mem_trigger.triggerCanRaiseBpExp
  val debugMode = csrCtrl.mem_trigger.debugMode
  memBlockTrigger.tdataVec := tdata
  memBlockTrigger.tEnableVec := tEnable
  memBlockTrigger.debugMode := debugMode
  memBlockTrigger.triggerCanRaiseBpExp := triggerCanRaiseBpExp

  XSDebug(tEnable.asUInt.orR, "Debug Mode: At least one store trigger is enabled\n")
  for (j <- 0 until TriggerNum)
    PrintTriggerInfo(tEnable(j), tdata(j))

  // dcache
  dcache.io.hartId := fromCtrl.hartId
  dcache.io.lqEmpty := lsq.io.lqEmpty
  dcache.io.l2_hint <> l2Hint
  dcache.io.lsu.tl_d_channel <> lsq.io.tl_d_channel
  dcache.io.force_write := lsq.io.force_write
  dcache.io.sms_agt_evict_req.ready := false.B
  dcache.io.lqEmpty := lsq.io.lqEmpty

  // Uncache
  uncache.io.enableOutstanding := fromCtrl.csrCtrl.uncache_write_outstanding_enable
  uncache.io.hartId := fromCtrl.hartId

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
  lsq.io.hartId := fromCtrl.hartId
  lsq.io.release := dcache.io.lsu.release
  lsq.io.brqRedirect  <> redirect
  lsq.io.enq          <> fromBackend.enqLsq
  lsq.io.tl_d_channel <> dcache.io.lsu.tl_d_channel
  lsq.io.lqCancelCnt <> toBackend.lqCancelCnt
  lsq.io.sqCancelCnt <> toBackend.sqCancelCnt
  lsq.io.lqDeq <> toBackend.lqDeq
  lsq.io.sqDeq <> toBackend.sqDeq
  lsq.io.lqDeqPtr <> toBackend.lqDeqPtr
  lsq.io.sqDeqPtr <> toBackend.sqDeqPtr
  lsq.io.ldvecFeedback <> vecExuBlock.io.toLsq.ldvecFeedback
  lsq.io.stvecFeedback <> vecExuBlock.io.toLsq.stvecFeedback
  lsq.io.flushSbuffer.empty := sbuffer.io.sbempty
  lsq.io.exceptionAddr.isStore := fromBackend.isStoreException
  lsq.io.issuePtrExt <> toBackend.stIssuePtr
  lsq.io.l2_hint.valid := l2Hint.valid
  lsq.io.l2_hint.bits.sourceId := l2Hint.bits.sourceId
  lsq.io.l2_hint.bits.isKeyword := l2Hint.bits.isKeyword
  lsq.io.tlb_hint <> dtlbRepeater.io.hint.get
  lsq.io.maControl <> memExuBlock.io.toLsq.maControl
  lsq.io.flushFrmMaBuf := memExuBlock.io.toLsq.flushFromMaBuf
  lsq.io.uncacheOutstanding := csrCtrl.uncache_write_outstanding_enable
  lsq.io.debugTopDown.robHeadMissInDTlb := dtlbRepeater.io.rob_head_miss_in_tlb
  lsq.io.std.storeDataIn.zip(memExuBlock.io.toBackend.writebackStd).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits := DontCare
      connectSamePort(sink.bits,  source.bits)
  }
  lsq.io.std.storeDataIn.zip(memExuBlock.io.toBackend.writebackStd).take(VstuCnt).zip(vecExuBlock.io.toLsq.vstd).zipWithIndex.foreach {
    case (((sink, std), vstd), i) =>
      when (vstd.valid) {
        sink.valid := true.B
        sink.bits := vstd.bits
        std.ready := false.B
      }
  }
  lsq.io.std.storeDataIn.map(_.bits.debug := 0.U.asTypeOf(new DebugBundle))
  lsq.io.std.storeDataIn.foreach(_.bits.isFromLoadUnit := DontCare)
  lsq.io.rob <> fromBackend.rob
  toBackend.lsqio.mmio := lsq.io.rob.mmio
  toBackend.lsqio.uop  := lsq.io.rob.uop

  // lsq to l2 CMO
  wrapper.cmoSender match {
    case Some(x) =>
      x.out.head._1 <> lsq.io.cmoOpReq
    case None =>
      lsq.io.cmoOpReq.ready  := false.B
  }
  wrapper.cmoReciver match {
    case Some(x) =>
      x.in.head._1  <> lsq.io.cmoOpResp
    case None =>
      lsq.io.cmoOpResp.valid := false.B
      lsq.io.cmoOpResp.bits  := 0.U.asTypeOf(new CMOResp)
  }

  // SBuffer
  sbuffer.io.hartId := fromCtrl.hartId
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
  // flush sbuffer
  val cmoFlush = lsq.io.flushSbuffer.valid
  val fenceFlush = fromBackend.flushSb
  val atomicsFlush = memExuBlock.io.flushSbuffer.valid || vecExuBlock.io.flushSbuffer.valid
  val stIsEmpty = sbuffer.io.flush.empty && uncache.io.flush.empty
  toBackend.sbIsEmpty := RegNext(stIsEmpty)

  // if both of them tries to flush sbuffer at the same time
  // something must have gone wrong
  assert(!(fenceFlush && atomicsFlush && cmoFlush))
  sbuffer.io.flush.valid := RegNext(fenceFlush || atomicsFlush || cmoFlush)
  uncache.io.flush.valid := sbuffer.io.flush.valid

  // Vector Execution Block
  vecExuBlock.io.fromCtrl.hartId := fromCtrl.hartId
  vecExuBlock.io.fromCtrl.redirect <> redirect
  vecExuBlock.io.fromCtrl.csrCtrl  := csrCtrl
  vecExuBlock.io.fromCtrl.trigger  := memBlockTrigger
  vecExuBlock.io.fromBackend.issueVldu <> fromBackend.issueVldu
  vecExuBlock.io.fromTlb.valid := ldDTLB.head.io.requestor.head.resp.valid
  vecExuBlock.io.fromTlb.bits  := ldDTLB.head.io.requestor.head.resp.bits
  vecExuBlock.io.fromPmp       := pmpCheckers.head.io.resp
  vecExuBlock.io.toBackend.vstuIqFeedback <> toBackend.vstuIqFeedback
  vecExuBlock.io.toBackend.vlduIqFeedback <> toBackend.vlduIqFeedback
  vecExuBlock.io.toBackend.writebackVldu <> toBackend.writebackVldu

  val vSegmentFlag = vecExuBlock.io.vSegmentFlag
  val vSegmentException = RegInit(false.B)
  when (DelayN(redirect.valid, 10) && vSegmentException) {
    vSegmentException := false.B
  }.elsewhen (vecExuBlock.io.exceptionInfo.valid) {
    vSegmentException := true.B
  }

  // Mem Execution Block
  memExuBlock.io.fromCtrl.hartId := fromCtrl.hartId
  memExuBlock.io.fromCtrl.redirect <> redirect
  memExuBlock.io.fromCtrl.csrCtrl := csrCtrl
  memExuBlock.io.fromCtrl.trigger := memBlockTrigger
  memExuBlock.io.fromBackend.issueLda <> fromBackend.issueLda
  memExuBlock.io.fromBackend.issueSta <> fromBackend.issueSta
  memExuBlock.io.fromBackend.issueStd <> fromBackend.issueStd
  memExuBlock.io.fromBackend.issueHya <> fromBackend.issueHya
  memExuBlock.io.fromBackend.rob <> fromBackend.rob
  memExuBlock.io.fromVecExuBlock <> vecExuBlock.io.toMemExuBlock
  memExuBlock.io.fromLsq.rarNuke  <> VecInit(lsq.io.ldu.ldld_nuke_query.map(_.resp))
  memExuBlock.io.fromLsq.rawNuke  <> VecInit(lsq.io.ldu.stld_nuke_query.map(_.resp))
  memExuBlock.io.fromLsq.replay <> lsq.io.replay
  memExuBlock.io.toBackend.stIssue  <> toBackend.stIssue
  memExuBlock.io.toBackend.writebackLda <> toBackend.writebackLda
  memExuBlock.io.toBackend.writebackSta <> toBackend.writebackSta
  memExuBlock.io.toBackend.writebackStd <> toBackend.writebackStd
  memExuBlock.io.toBackend.writebackHyuLda <> toBackend.writebackHyuLda
  memExuBlock.io.toBackend.writebackHyuSta <> toBackend.writebackHyuSta
  memExuBlock.io.toBackend.ldaIqFeedback <> toBackend.ldaIqFeedback
  memExuBlock.io.toBackend.staIqFeedback <> toBackend.staIqFeedback
  memExuBlock.io.toBackend.hyuIqFeedback <> toBackend.hyuIqFeedback
  memExuBlock.io.toBackend.ldCancel <> toBackend.ldCancel
  memExuBlock.io.toBackend.wakeup <> toBackend.wakeup
  memExuBlock.io.toPrefetch.ifetch <> toFrontend.ifetchPrefetch
  memExuBlock.io.toVecExuBlock <> vecExuBlock.io.fromMemExuBlock
  memExuBlock.io.toLsq.out <> lsq.io.ldu.ldin
  memExuBlock.io.toLsq.addrUpdate <> lsq.io.sta.storeAddrIn
  memExuBlock.io.toLsq.excpUpdate <> lsq.io.sta.storeAddrInRe
  memExuBlock.io.toLsq.maskOut    <> lsq.io.sta.storeMaskIn
  memExuBlock.io.amoDCacheIO <> dcache.io.lsu.atomics
  memExuBlock.io.debugLsInfo <> io.debugLsInfo.debugLsInfo
  memExuBlock.io.lsTopdownInfo <> toBackend.lsTopdownInfo
  toBackend.s3DelayedLoadError.map(_ := false.B) // TODO: need assign

  // tlb response: [[Tlb]] -> [[MemExuBlock]]
  val ldDTLBResps = ldDTLB.map(_.io.requestor).flatten.take(LduCnt + HyuCnt).map(_.resp)
  val stDTLBResps = stDTLB.map(_.io.requestor.map(_.resp)).flatten.take(StaCnt)
  memExuBlock.io.fromTlb.map(_.resp).zip(ldDTLBResps ++ stDTLBResps).zipWithIndex.foreach {
    case ((sink, source), i) =>
      if (i == 0) {
          vecExuBlock.io.fromTlb.valid := source.valid
          vecExuBlock.io.fromTlb.bits  := source.bits
      }

      if (source.bits.paddr.length > 1) {
        sink <> source
      } else {
        sink.valid := source.valid
        sink.bits.miss := source.bits.miss
        sink.bits.fastMiss := source.bits.fastMiss
        sink.bits.ptwBack := source.bits.ptwBack
        sink.bits.memidx := source.bits.memidx
        sink.bits.isForVSnonLeafPTE := source.bits.isForVSnonLeafPTE
        sink.bits.debug := source.bits.debug
        sink.bits.paddr.map(_ := source.bits.paddr(0))
        sink.bits.gpaddr.map(_ := source.bits.gpaddr(0))
        sink.bits.pbmt.map(_ := source.bits.pbmt(0))
        sink.bits.excp.map(_ := source.bits.excp(0))
        source.ready := sink.ready
      }
  }

  // Lsq uncache
  memExuBlock.io.fromLsq.uncache.zip(lsq.io.ldout).zipWithIndex.foreach {
    case ((sink, source), i) =>
      if (i == 0) {
        sink.valid := source.valid
        sink.bits.fromMemExuOutputBundle(source.bits)
        source.ready := sink.ready
      } else {
        sink.valid := false.B
        sink.bits := DontCare
        source.ready := true.B
      }
  }

  // writeback overwrite
  toBackend.writebackStd.zip(memExuBlock.io.toBackend.writebackStd).foreach {x =>
    x._1.bits  := x._2.bits
    x._1.valid := x._2.fire
  }

  // prefetch req: [[Prefetch]] -> [[MemExuBlock]]
  val l1PfReq = Wire(Decoupled(new L1PrefetchReq()))
  l1PrefetcherOpt match {
    case Some(pf) => l1PfReq <> Pipeline(in = pf.io.l1_req, depth = 1, pipe = false, name = Some("pf_queue_to_ldu_reg"))
    case None =>
      l1PfReq.valid := false.B
      l1PfReq.bits := DontCare
  }

  // prefetch issue: [[Prefetch]] -> [[MemExuBlock]]
  memExuBlock.io.fromPrefetch.foreach {
    case (req) =>
      req.valid := l1PfReq.valid
      req.bits.fromL1PrefetchReqBundle(l1PfReq.bits)
  }

  // NOTE: loadUnits(0) has higher bank conflict and miss queue arb priority than loadUnits(1) and loadUnits(2)
  // when loadUnits(1)/loadUnits(2) stage 0 is busy, hw prefetch will never use that pipeline
  val LowConfPorts = if (LduCnt == 2) Seq(1) else if (LduCnt == 3) Seq(1, 2) else Seq(0)
  LowConfPorts.map{case i => memExuBlock.io.fromPrefetch(i).bits.confidence := 0.U }

  val canAcceptHighConfPrefetch = memExuBlock.io.toPrefetch.train.map(_.canAcceptHighConfPrefetch)
  val canAcceptLowConfPrefetch = memExuBlock.io.toPrefetch.train.map(_.canAcceptLowConfPrefetch)
  l1PfReq.ready := canAcceptHighConfPrefetch.zip(canAcceptLowConfPrefetch).zipWithIndex.map {
    case ((lowConf, highConf), i) =>
      if (LowConfPorts.contains(i)) {
        lowConf
      } else {
        Mux(l1PfReq.bits.confidence === 1.U, highConf, lowConf)
      }
  }.reduce(_|_)

  // tlb requests: [[MemExuBlock]] -> [[TLB]]
  (ldDTLB ++ stDTLB).map(_.io.requestor).flatten.zip(memExuBlock.io.toTlb).zipWithIndex.foreach {
    case ((sink, source), i) =>
      sink.req <> source.req
      sink.req_kill := source.req_kill

      if (i == 0) {
        sink.req.valid := source.req.valid || vecExuBlock.io.toTlb.req.valid
        sink.req.bits   := ParallelPriorityMux(Seq(
          RegNext(vecExuBlock.io.toTlb.req.valid) -> RegEnable(vecExuBlock.io.toTlb.req.bits, vecExuBlock.io.toTlb.req.valid),
          source.req.valid -> source.req.bits
        ))
        vecExuBlock.io.toTlb.req.ready := sink.req.ready
      }
  }

  // tlb hint: [[TLB]] -> [[MemExuBlock]]
  val tlbReplayWire = WireInit(VecInit(Seq.fill(LdExuCnt)(false.B)))
  val tlbReplayReg = GatedValidRegNext(tlbReplayWire)
  val dTLBLd0TlbReplayReg = GatedValidRegNext(ldDTLB.head.io.tlbreplay)
  dontTouch(tlbReplayWire)
  tlbReplayWire.zip(ldDTLB.head.io.ptw.req).foreach {
    case (sink, source) =>
      sink := source.valid && ptwRespNext.vector(0) && ptwRespV &&
              ptwRespNext.data.hit(source.bits.vpn, tlbCsr.satp.asid, tlbCsr.vsatp.asid, tlbCsr.hgatp.vmid, allType = true, ignoreAsid = true)
  }

  memExuBlock.io.fromTlb.map(_.hint).zip((dtlbRepeater.io.hint.get.req.zip(tlbReplayReg.zip(dTLBLd0TlbReplayReg)))).foreach {
    case (sink, (source, (tlbReplay, ld0TlbReplayReg))) =>
      sink.id := source.id
      sink.full := source.full || tlbReplay || ld0TlbReplayReg
  }

  // tlb responses: [[TLB]] -> [[MemExuBlock]]
  memExuBlock.io.fromTlb.zip((ldDTLB ++ stDTLB).map(_.io.requestor).flatten).zipWithIndex.foreach {
    case ((sink, source), i) =>
      if (i == 0) {
        vecExuBlock.io.fromTlb.valid := source.resp.valid
        vecExuBlock.io.fromTlb.bits  := source.resp.bits
      }
  }

  // pmp response: [[PMP]] -> [[MemExuBlock]]
  memExuBlock.io.fromPmp.zip(pmpCheckers).zipWithIndex.foreach {
    case ((sink, source), i) =>
      sink <> source.io.resp
  }

  // dcache requests: [[MemExuBlock]] -> [[DCache]]
  (dcache.io.lsu.load ++ dcache.io.lsu.sta).zip(memExuBlock.io.toDCache).zipWithIndex.foreach {
    case ((sink: DCacheLoadIO, source), i) =>
      sink.req <> source.req

      if (i == 0) {
        sink.req.valid := vecExuBlock.io.toDCache.req.valid || source.req.valid
        sink.req.bits  := Mux1H(Seq(
          vecExuBlock.io.toDCache.req.valid -> vecExuBlock.io.toDCache.req.bits,
          source.req.valid -> source.req.bits
        ))
        source.req.ready := sink.req.ready

        when (vSegmentFlag) {
          source.req.ready := false.B
          sink.pf_source           := vecExuBlock.io.toDCache.pf_source
          sink.s1_paddr_dup_lsu    := vecExuBlock.io.toDCache.s1_paddr_dup_lsu
          sink.s1_paddr_dup_dcache := vecExuBlock.io.toDCache.s1_paddr_dup_dcache
          sink.s1_kill             := vecExuBlock.io.toDCache.s1_kill
          sink.s2_kill             := vecExuBlock.io.toDCache.s2_kill
          sink.s0_pc               := vecExuBlock.io.toDCache.s0_pc
          sink.s1_pc               := vecExuBlock.io.toDCache.s1_pc
          sink.s2_pc               := vecExuBlock.io.toDCache.s2_pc
        } .otherwise {
          source.req.ready := sink.req.ready
          sink.pf_source           := source.pf_source
          sink.s1_paddr_dup_lsu    := source.s1_paddr_dup_lsu
          sink.s1_paddr_dup_dcache := source.s1_paddr_dup_dcache
          sink.s1_kill             := source.s1_kill
          sink.s2_kill             := source.s2_kill
          sink.s0_pc               := source.s0_pc
          sink.s1_pc               := source.s1_pc
          sink.s2_pc               := source.s2_pc
        }
      }

    case ((sink: DCacheStoreIO, source), i) =>
      sink.req.valid := source.req.valid
      sink.req.bits.cmd := source.req.bits.cmd
      sink.req.bits.vaddr := source.req.bits.vaddr
      sink.req.bits.instrtype := source.req.bits.instrtype
      source.req.ready := sink.req.ready
      sink.s1_paddr := source.s1_paddr_dup_dcache
      sink.s1_kill := source.s1_kill
      sink.s2_kill := source.s2_kill
      sink.s2_pc   := source.s2_pc

    case _ =>
  }

  // dcache responses: [[DCache]] -> [[MemExuBlock]]
  memExuBlock.io.fromDCache.zip((dcache.io.lsu.load ++ dcache.io.lsu.sta)).zipWithIndex.foreach {
    case ((sink, source: DCacheLoadIO), i) =>
      sink.resp <> source.resp
      sink.s1_disable_fast_wakeup := source.s1_disable_fast_wakeup
      sink.s2_hit := source.s2_hit
      sink.s2_first_hit := source.s2_first_hit
      sink.s2_bank_conflict := source.s2_bank_conflict
      sink.s2_wpu_pred_fail := source.s2_wpu_pred_fail
      sink.s2_mq_nack := source.s2_mq_nack
      sink.debug_s1_hit_way := source.debug_s1_hit_way
      sink.debug_s2_pred_way_num := source.debug_s2_pred_way_num
      sink.debug_s2_dm_way_num := source.debug_s2_dm_way_num
      sink.debug_s2_real_way_num := source.debug_s2_real_way_num

      if (i == 0) {
        vecExuBlock.io.fromDCache := DontCare
        vecExuBlock.io.fromDCache.resp.valid := source.resp.valid
        vecExuBlock.io.fromDCache.resp.bits  := source.resp.bits
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
  memExuBlock.io.fromTLDchannel.zip(dcache.io.lsu.forward_D).foreach {
    case (sink, source) =>
      sink <> source
  }

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
      sink.valid := true.B
      sink.bits.forwardMshr := source.forward_mshr
      sink.bits.forwardData := source.forwardData
      sink.bits.forwardResultValid := source.forward_result_valid
  }

  // Lsq forward req: [[MemExuBlock]] -> [[Lsq]]
  lsq.io.forward.zip(memExuBlock.io.toLsq.forward).foreach {
    case (sink, source) =>
      sink.req.valid := source.valid
      connectSamePort(sink.req.bits, source.bits)
  }

  // Lsq forward resp: [[Lsq]] -> [[MemExuBlock]]
  memExuBlock.io.fromLsq.forward.zip(lsq.io.forward).foreach {
    case (sink, source) =>
      sink.valid := true.B
      connectSamePort(sink.bits, source)
  }

  // SBuffer forward req: [[MemExuBlock]] -> [[SBuffer]]
  sbuffer.io.forward.zip(memExuBlock.io.toSBuffer).foreach {
    case (sink, source) =>
      sink.req.valid := source.valid
      connectSamePort(sink.req.bits, source.bits)
  }

  // SBuffer forward resp: [[SBuffer]] -> [[MemExuBlock]]
  memExuBlock.io.fromSBuffer.zip(sbuffer.io.forward).foreach {
    case (sink, source) =>
      sink.valid := true.B // TODO: need response valid signal?
      connectSamePort(sink.bits, source.resp)
  }

  // prfetch train: [[MemExuBlock]] -> prefetch
  val pfTrainOnHit = RegNextN(fromCtrl.csrCtrl.l1D_pf_train_on_hit, 2, Some(true.B))
  val prefetchOptReqs = (prefetcherOpt.map(_.io.ld_in) ++ prefetcherOpt.map(_.io.st_in)).flatten
  val prefetchSourcePc = fromBackend.loadPc ++ fromBackend.storePc
  prefetchOptReqs.zip(memExuBlock.io.toPrefetch.train).zip(prefetchSourcePc).zipWithIndex.foreach {
    case (((pf, source), sourcePc), i) =>
      pf.valid := Mux(pfTrainOnHit,
        source.req.valid,
        source.req.valid & source.req.bits.isFirstIssue && source.req.bits.miss
      )
      pf.bits := source.req.bits
      pf.bits.uop.pc := RegEnable(RegEnable(sourcePc, source.s1PrefetchSpec), source.s2PrefetchSpec)
  }
  l1PrefetcherOpt.get.io.ld_in.zip(memExuBlock.io.toPrefetch.trainL1).zipWithIndex.foreach {
    case ((pf, source), i) =>
      pf.valid := source.req.valid && source.req.bits.isFirstIssue
      pf.bits  := source.req.bits
  }
  l1PrefetcherOpt.get.io.st_in.zipWithIndex.foreach {
    case (pf, i) =>
      pf.valid := false.B
      pf.bits  := DontCare
  }

  // load/store prefetch to l2 cache
  val hartId = p(XSCoreParamsKey).HartId
  prefetcherOpt.foreach(smsPf => {
    l1PrefetcherOpt.foreach(l1Pf => {
      val smsPfToL2 = DelayNWithValid(smsPf.io.l2_req, 2)
      val l1PfToL2 = DelayNWithValid(l1Pf.io.l2_req, 2)

      wrapper.l2PfSenderOpt.get.out.head._1.addr_valid := smsPfToL2.valid || l1PfToL2.valid
      wrapper.l2PfSenderOpt.get.out.head._1.addr := Mux(l1PfToL2.valid, l1PfToL2.bits.addr, smsPfToL2.bits.addr)
      wrapper.l2PfSenderOpt.get.out.head._1.pf_source := Mux(l1PfToL2.valid, l1PfToL2.bits.source, smsPfToL2.bits.source)
      wrapper.l2PfSenderOpt.get.out.head._1.l2_pf_en := RegNextN(fromCtrl.csrCtrl.l2_pf_enable, 2, Some(true.B))

      smsPf.io.enable := RegNextN(fromCtrl.csrCtrl.l1D_pf_enable, 2, Some(false.B))

      val l2Trace = Wire(new LoadPfDbBundle)
      l2Trace.paddr := wrapper.l2PfSenderOpt.get.out.head._1.addr
      val table = ChiselDB.createTable(s"L2PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
      table.log(l2Trace, l1PfToL2.valid, "StreamPrefetchTrace", clock, reset)
      table.log(l2Trace, !l1PfToL2.valid && smsPfToL2.valid, "L2PrefetchTrace", clock, reset)

      val l1PfToL3 = ValidIODelay(l1Pf.io.l3_req, 4)
      wrapper.l3PfSenderOpt.foreach(_.out.head._1.addr_valid := l1PfToL3.valid)
      wrapper.l3PfSenderOpt.foreach(_.out.head._1.addr := l1PfToL3.bits)
      wrapper.l3PfSenderOpt.foreach(_.out.head._1.l2_pf_en := RegNextN(fromCtrl.csrCtrl.l2_pf_enable, 4, Some(true.B)))

      val l3Trace = Wire(new LoadPfDbBundle)
      l3Trace.paddr := wrapper.l3PfSenderOpt.map(_.out.head._1.addr).getOrElse(0.U)
      val l3Table = ChiselDB.createTable(s"L3PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
      l3Table.log(l3Trace, l1PfToL3.valid, "StreamPrefetchTrace", clock, reset)

      XSPerfAccumulate("prefetchFireL2", wrapper.l2PfSenderOpt.get.out.head._1.addr_valid)
      XSPerfAccumulate("prefetchFireL3", wrapper.l3PfSenderOpt.map(_.out.head._1.addr_valid).getOrElse(false.B))
      XSPerfAccumulate("l1PfFireL2", l1PfToL2.valid)
      XSPerfAccumulate("smsFireL2", !l1PfToL2.valid && smsPfToL2.valid)
      XSPerfAccumulate("smsBlockByL1Pf", l1PfToL2.valid && smsPfToL2.valid)
    })
  })

  // l1 pf fuzzer interface
  val DebugEnableL1PFFuzzer = false
  if (DebugEnableL1PFFuzzer) {
    // l1 pf req fuzzer
    val fuzzer = Module(new L1PrefetchFuzzer())
    fuzzer.io.vaddr := DontCare
    fuzzer.io.paddr := DontCare

    // override load_unit prefetch_req
    memExuBlock.io.fromPrefetch.foreach(req => {
      req.valid <> fuzzer.io.req.valid
      req.bits <> fuzzer.io.req.bits
    })

    fuzzer.io.req.ready := l1PfReq.ready
  }

  // Prefetcher
  val StreamDTLBPortIndex = TlbStartVec(ldDTLBIdx) + LduCnt + HyuCnt
  val PrefetcherDTLBPortIndex = TlbStartVec(pfDTLBIdx)
  val L2toL1DLBPortIndex = TlbStartVec(pfDTLBIdx) + 1
  prefetcherOpt match {
  case Some(pf) => dTLBReqs(PrefetcherDTLBPortIndex) <> pf.io.tlb_req
  case None =>
    dTLBReqs(PrefetcherDTLBPortIndex) := DontCare
    dTLBReqs(PrefetcherDTLBPortIndex).req.valid := false.B
    dTLBReqs(PrefetcherDTLBPortIndex).resp.ready := true.B
  }
  l1PrefetcherOpt match {
    case Some(pf) => dTLBReqs(StreamDTLBPortIndex) <> pf.io.tlb_req
    case None =>
        dTLBReqs(StreamDTLBPortIndex) := DontCare
        dTLBReqs(StreamDTLBPortIndex).req.valid := false.B
        dTLBReqs(StreamDTLBPortIndex).resp.ready := true.B
  }
  dTLBReqs(L2toL1DLBPortIndex) <> io.l2TlbReq
  dTLBReqs(L2toL1DLBPortIndex).resp.ready := true.B
  io.l2PmpResp := pmpCheckers(L2toL1DLBPortIndex).io.resp

  // mmio store writeback will use store writeback port 0
  val mmioStout = WireInit(0.U.asTypeOf(lsq.io.mmioStout))
  NewPipelineConnect(
    lsq.io.mmioStout, mmioStout, mmioStout.fire,
    false.B,
    Option("mmioStOutConnect")
  )
  mmioStout.ready := false.B
  when (mmioStout.valid && !memExuBlock.io.toBackend.writebackStd.head.valid) {
    toBackend.writebackSta(0).valid := true.B
    toBackend.writebackSta(0).bits  := mmioStout.bits
    mmioStout.ready := true.B
  }
  // vec mmio writeback
  lsq.io.vecmmioStout.ready := false.B
  when (lsq.io.vecmmioStout.valid && !vecExuBlock.io.fromMemExuBlock.vectorStoreWriteback.head.valid) {
    toBackend.writebackSta(0).valid := true.B
    toBackend.writebackSta(0).bits  := lsq.io.vecmmioStout.bits
    lsq.io.vecmmioStout.ready := true.B
  }

  //  rollback: [[MemBlock]] -> [[Backend]]
  def selectOldestRedirect(xs: Seq[Valid[Redirect]]): Vec[Bool] = {
    val compareVec = (0 until xs.length).map(i => (0 until i).map(j => isAfter(xs(j).bits.robIdx, xs(i).bits.robIdx)))
    val resultOnehot = VecInit((0 until xs.length).map(i => Cat((0 until xs.length).map(j =>
      (if (j < i) !xs(j).valid || compareVec(i)(j)
      else if (j == i) xs(i).valid
      else !xs(j).valid || !compareVec(j)(i))
    )).andR))
    resultOnehot
  }
  val allRedirect = memExuBlock.io.toBackend.rollback ++ Seq(lsq.io.nack_rollback) ++ lsq.io.nuke_rollback
  val oldestOneHot = selectOldestRedirect(allRedirect)
  val oldestRedirect = WireDefault(Mux1H(oldestOneHot, allRedirect))
  // memory replay would not cause IAF/IPF/IGPF
  oldestRedirect.bits.cfiUpdate.backendIAF := false.B
  oldestRedirect.bits.cfiUpdate.backendIPF := false.B
  oldestRedirect.bits.cfiUpdate.backendIGPF := false.B
  toBackend.memoryViolation := oldestRedirect
  toBackend.lsqio.lqCanAccept  := lsq.io.lqCanAccept
  toBackend.lsqio.sqCanAccept  := lsq.io.sqCanAccept

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
  lsqExceptionReq.bits  := lsq.io.exceptionAddr

  val exceptionReqs = Seq(atomicsExceptionReq, misalignExceptionReq, vSegmentExceptionReq, lsqExceptionReq)
  exceptionGen.io.redirect <> redirect
  exceptionGen.io.tlbCsr   <> tlbCsr
  exceptionGen.io.in.zip(exceptionReqs).foreach {
    case (sink, source) =>
      sink <> source
  }
  connectSamePort(toBackend.lsqio, exceptionGen.io.out)

  // to backend
  bypass.fromTopToBackend match { case x =>
    x.hartId                  := fromCtrl.hartId
    x.externalInterrupt.msip  := wrapper.clintIntSink.in.head._1(0)
    x.externalInterrupt.mtip  := wrapper.clintIntSink.in.head._1(1)
    x.externalInterrupt.meip  := wrapper.plicIntSink.in.head._1(0)
    x.externalInterrupt.seip  := wrapper.plicIntSink.in.last._1(0)
    x.externalInterrupt.debug := wrapper.debugIntSink.in.head._1(0)
    x.externalInterrupt.nmi.nmi_31 := wrapper.nmiIntSink.in.head._1(0)
    x.externalInterrupt.nmi.nmi_43 := wrapper.nmiIntSink.in.head._1(1)
    x.msiInfo           := DelayNWithValid(bypass.fromTop.msiInfo, 1)
    x.clintTime         := DelayNWithValid(bypass.fromTop.clintTime, 1)
  }

  io.memInfo.sqFull := RegNext(lsq.io.sqFull)
  io.memInfo.lqFull := RegNext(lsq.io.lqFull)
  io.memInfo.dcacheMSHRFull := RegNext(dcache.io.mshrFull)

  bypass.innerHartId := fromCtrl.hartId
  bypass.innerResetVector := RegNext(bypass.outerResetVector)
  bypass.outerCpuHalt := bypass.fromBackendToTop.cpuHalted
  bypass.outerBeuErrorsIcache := RegNext(bypass.innerBeuErrorsIcache)
  bypass.outerL2PfEnable := bypass.innerL2PfEnable
  // io.inner_hc_perfEvents <> io.outer_hc_perfEvents

  // Initialize when unenabled difftest.
  sbuffer.io.vecDifftestInfo      := DontCare
  lsq.io.sbufferVecDifftestInfo   := DontCare
  vecExuBlock.io.vecDifftestInfo := DontCare
  if (env.EnableDifftest) {
    sbuffer.io.vecDifftestInfo.zipWithIndex.map {
      case (sbufferPort, index) =>
        if (index == 0) {
          val vSegmentDifftestValid = vecExuBlock.io.vecDifftestInfo.valid
          sbufferPort.valid := Mux(vSegmentDifftestValid, vecExuBlock.io.vecDifftestInfo.valid, lsq.io.sbufferVecDifftestInfo(0).valid)
          sbufferPort.bits  := Mux(vSegmentDifftestValid, vecExuBlock.io.vecDifftestInfo.bits, lsq.io.sbufferVecDifftestInfo(0).bits)

          vecExuBlock.io.vecDifftestInfo.ready  := sbufferPort.ready
          lsq.io.sbufferVecDifftestInfo(0).ready := sbufferPort.ready
        } else {
           sbufferPort <> lsq.io.sbufferVecDifftestInfo(index)
        }
    }
  }

  // reset tree of MemBlock
  if (p(DebugOptionsKey).ResetGen) {
    val leftResetTree = ResetGenNode(
      Seq(
        ModuleNode(ptw),
        ModuleNode(ptwToL2Buffer),
        ModuleNode(lsq),
        ModuleNode(stDTLB.head),
        ModuleNode(pfDTLB.head),
        ModuleNode(pmp)
      )
      ++ pmpCheckers.map(ModuleNode(_))
      ++ (if (prefetcherOpt.isDefined) Seq(ModuleNode(prefetcherOpt.get)) else Nil)
      ++ (if (l1PrefetcherOpt.isDefined) Seq(ModuleNode(l1PrefetcherOpt.get)) else Nil)
    )
    val rightResetTree = ResetGenNode(
      Seq(
        ModuleNode(sbuffer),
        ModuleNode(ldDTLB.head),
        ModuleNode(dcache),
        ModuleNode(l1dToL2Buffer),
        CellNode(bypass.resetBackend)
      )
    )
    ResetGen(leftResetTree, reset, sim = false)
    ResetGen(rightResetTree, reset, sim = false)
  } else {
    bypass.resetBackend := DontCare
  }
  bypass.resetInFrontendBypass.toL2Top := bypass.resetInFrontendBypass.fromFrontend

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

  val hyLdDeqCount = PopCount(fromBackend.issueHya.map(x => x.valid && FuType.isLoad(x.bits.uop.fuType)))
  val hyStDeqCount = PopCount(fromBackend.issueHya.map(x => x.valid && FuType.isStore(x.bits.uop.fuType)))
  val ldDeqCount = PopCount(fromBackend.issueLda.map(_.valid)) +& hyLdDeqCount
  val stDeqCount = PopCount(fromBackend.issueSta.take(StaCnt).map(_.valid)) +& hyStDeqCount
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