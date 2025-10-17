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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] Alex Ramirez, Oliverio J. Santana, Josep L. Larriba-Pey, and Mateo Valero. "[Fetching instruction streams.]
* (https://doi.org/10.1109/MICRO.2002.1176264)" 35th Annual IEEE/ACM International Symposium on Microarchitecture
* (MICRO). 2002.
* [2] Yasuo Ishii, Jaekyu Lee, Krishnendra Nathella, and Dam Sunwoo. "[Rebasing instruction prefetching: An industry
* perspective.](https://doi.org/10.1109/LCA.2020.3035068)" IEEE Computer Architecture Letters 19.2: 147-150. 2020.
* [3] Yasuo Ishii, Jaekyu Lee, Krishnendra Nathella, and Dam Sunwoo. "[Re-establishing fetch-directed instruction
* prefetching: An industry perspective.](https://doi.org/10.1109/ISPASS51385.2021.00034)" 2021 IEEE International
* Symposium on Performance Analysis of Systems and Software (ISPASS). 2021.
***************************************************************************************/

package xiangshan.frontend
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import ftq.Ftq
import ftq.FtqPtr
import org.chipsalliance.cde.config.Parameters
import utility._
import utility.mbist.MbistInterface
import utility.mbist.MbistPipeline
import utility.sram.SramBroadcastBundle
import utility.sram.SramHelper
import xiangshan._
import xiangshan.backend.fu.NewCSR.PFEvent
import xiangshan.backend.fu.PMP
import xiangshan.backend.fu.PMPChecker
import xiangshan.backend.fu.PMPReqBundle
import xiangshan.cache.mmu._
import xiangshan.frontend.bpu.Bpu
import xiangshan.frontend.ibuffer.IBuffer
import xiangshan.frontend.icache._
import xiangshan.frontend.ifu._
import xiangshan.frontend.instruncache.InstrUncache
import xiangshan.frontend.simfrontend.SimFrontendInlinedImp

class Frontend()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false
  val inner       = LazyModule(new FrontendInlined)
  lazy val module = new FrontendImp(this)
}

class FrontendImp(wrapper: Frontend)(implicit p: Parameters) extends LazyModuleImp(wrapper) {
  val io      = IO(wrapper.inner.module.io.cloneType)
  val io_perf = IO(wrapper.inner.module.io_perf.cloneType)
  io <> wrapper.inner.module.io
  io_perf <> wrapper.inner.module.io_perf
  if (p(DebugOptionsKey).ResetGen) {
    ResetGen(ResetGenNode(Seq(ModuleNode(wrapper.inner.module))), reset, sim = false, io.dft_reset)
  }
}

abstract class FrontendInlinedImpBase(outer: FrontendInlined) extends LazyModuleImp(outer) with HasXSParameter
    with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId       = Input(UInt(hartIdLen.W))
    val reset_vector = Input(PrunedAddr(PAddrBits))
    val fencei       = Input(Bool())
    val ptw          = new TlbPtwIO()
    val backend      = new FrontendToCtrlIO
    val softPrefetch = Vec(backendParams.LduCnt, Flipped(Valid(new SoftIfetchPrefetchBundle)))
    val sfence       = Input(new SfenceBundle)
    val tlbCsr       = Input(new TlbCsrBundle)
    val csrCtrl      = Input(new CustomCSRCtrlIO)
    val error        = ValidIO(new L1CacheErrorInfo)
    val frontendInfo = new Bundle {
      val ibufFull = Output(Bool())
      val bpuInfo = new Bundle {
        val bpRight = Output(UInt(XLEN.W))
        val bpWrong = Output(UInt(XLEN.W))
      }
    }
    val resetInFrontend = Output(Bool())
    val debugTopDown = new Bundle {
      val robHeadVaddr = Flipped(Valid(PrunedAddr(VAddrBits)))
    }
    val dft       = Option.when(hasDFT)(Input(new SramBroadcastBundle))
    val dft_reset = Option.when(hasMbist)(Input(new DFTResetSignals()))
  })
}

class FrontendInlined()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = true

  val instrUncache = LazyModule(new InstrUncache())
  val icache       = LazyModule(new ICache())

  lazy val module: FrontendInlinedImpBase =
    if (env.EnableSimFrontend) new SimFrontendInlinedImp(this) else new FrontendInlinedImp(this)
}

class FrontendInlinedImp(outer: FrontendInlined) extends FrontendInlinedImpBase(outer) {

  // decouped-frontend modules
  val instrUncache = outer.instrUncache.module
  val icache       = outer.icache.module
  val bpu          = Module(new Bpu)
  val ifu          = Module(new Ifu)
  val ibuffer      = Module(new IBuffer)
  val ftq          = Module(new Ftq)

  val needFlush            = RegNext(io.backend.toFtq.redirect.valid)
  val FlushControlRedirect = RegNext(io.backend.toFtq.redirect.bits.debugIsCtrl)
  val FlushMemVioRedirect  = RegNext(io.backend.toFtq.redirect.bits.debugIsMemVio)
  val FlushControlBTBMiss  = Wire(Bool())
  val FlushTAGEMiss        = Wire(Bool())
  val FlushSCMiss          = Wire(Bool())
  val FlushITTAGEMiss      = Wire(Bool())
  val FlushRASMiss         = Wire(Bool())

  // TODO: what the fuck are these magic numbers?
  val tlbCsr  = DelayN(io.tlbCsr, 1)
  val csrCtrl = DelayN(io.csrCtrl, 2)
  val sfence  = DelayN(io.sfence, 2)

  // trigger
  ifu.io.frontendTrigger := csrCtrl.frontend_trigger

  // RVCDecoder fsIsOff
  ifu.io.csrFsIsOff := csrCtrl.fsIsOff

  // bpu ctrl
  bpu.io.ctrl        := csrCtrl.bp_ctrl
  bpu.io.resetVector := io.reset_vector

  // pmp
  val pmp       = Module(new PMP())
  val pmp_check = VecInit(Seq.fill(coreParams.ipmpPortNum)(Module(new PMPChecker(3, sameCycle = true)).io))
  pmp.io.distribute_csr := csrCtrl.distribute_csr
  val pmp_req_vec = Wire(Vec(coreParams.ipmpPortNum, Valid(new PMPReqBundle())))
  (0 until 2).foreach(i => pmp_req_vec(i) <> icache.io.pmp(i).req) // magic number 2: prefetchPipe & mainPipe
  pmp_req_vec.last <> ifu.io.pmp.req

  for (i <- pmp_check.indices) {
    if (HasBitmapCheck) {
      pmp_check(i).apply(tlbCsr.mbmc.CMODE.asBool, tlbCsr.priv.imode, pmp.io.pmp, pmp.io.pma, pmp_req_vec(i))
    } else {
      pmp_check(i).apply(tlbCsr.priv.imode, pmp.io.pmp, pmp.io.pma, pmp_req_vec(i))
    }
  }
  (0 until 2).foreach(i => icache.io.pmp(i).resp <> pmp_check(i).resp)
  ifu.io.pmp.resp <> pmp_check.last.resp

  // icache use a non-block tlb port, ifu use a block tlb port
  val itlb = Module(new TLB(coreParams.itlbPortNum, nRespDups = 1, Seq(false, true), itlbParams))
  itlb.io.requestor.head <> icache.io.itlb
  itlb.io.requestor.last <> ifu.io.itlb // mmio may need re-tlb, blocked
  itlb.io.hartId := io.hartId
  itlb.io.base_connect(sfence, tlbCsr)
  itlb.io.flushPipe.foreach(_ := icache.io.itlbFlushPipe)
  itlb.io.redirect := DontCare // itlb has flushpipe, don't need redirect signal

  val itlb_ptw = Wire(new VectorTlbPtwIO(coreParams.itlbPortNum))
  itlb_ptw.connect(itlb.io.ptw)
  val itlbRepeater1 = PTWFilter(itlbParams.fenceDelay, itlb_ptw, sfence, tlbCsr, l2tlbParams.ifilterSize)
  val itlbRepeater2 =
    PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, itlbRepeater1.io.ptw, io.ptw, sfence, tlbCsr)

  // ICache-Memblock
  icache.io.softPrefetchReq <> io.softPrefetch

  // wfi (backend-icache, backend-instrUncache)
  // DelayN for better timing, FIXME: maybe 1 cycle is not enough, to be evaluated
  private val wfiReq = DelayN(io.backend.wfi.wfiReq, 1)
  icache.io.wfi.wfiReq       := wfiReq
  instrUncache.io.wfi.wfiReq := wfiReq
  // return safe only when both icache & instrUncache are safe, also only when has wfiReq (like, safe := wfiReq.fire)
  io.backend.wfi.wfiSafe := DelayN(wfiReq && icache.io.wfi.wfiSafe && instrUncache.io.wfi.wfiSafe, 1)

  // IFU-Ftq
  ifu.io.fromFtq <> ftq.io.toIfu
  ftq.io.toIfu.req.ready := ifu.io.fromFtq.req.ready && icache.io.fromFtq.fetchReq.ready

  ftq.io.fromIfu <> ifu.io.toFtq
  bpu.io.fromFtq <> ftq.io.toBpu
  ftq.io.fromBpu <> bpu.io.toFtq

  // ICache-Ftq
  icache.io.fromFtq <> ftq.io.toICache
  // override fetchReq.ready to sync with Ifu
  ftq.io.toICache.fetchReq.ready := ifu.io.fromFtq.req.ready && icache.io.fromFtq.fetchReq.ready
  icache.io.flush                := DontCare

  // Ifu-ICache
  ifu.io.fromICache <> icache.io.toIfu
  ifu.io.toICache <> icache.io.fromIfu

  // ICache-Backend
  icache.io.csrPfEnable := RegNext(csrCtrl.pf_ctrl.l1I_pf_enable)
  icache.io.fencei      := RegNext(io.fencei)

  // IFU-Ibuffer
  ifu.io.toIBuffer <> ibuffer.io.in

  ftq.io.fromBackend <> io.backend.toFtq
  io.backend.fromFtq := ftq.io.toBackend
  io.backend.fromIfu := ifu.io.toBackend
  io.frontendInfo.bpuInfo <> ftq.io.bpuInfo

  val checkPcMem = Reg(Vec(FtqSize, new PrunedAddr(VAddrBits)))
  when(ftq.io.toBackend.wen) {
    checkPcMem(ftq.io.toBackend.ftqIdx) := ftq.io.toBackend.startVAddr
  }

  val checkTargetPtr = Wire(Vec(DecodeWidth, new FtqPtr))
  val checkTarget    = Wire(Vec(DecodeWidth, PrunedAddr(VAddrBits)))

  for (i <- 0 until DecodeWidth) {
    checkTargetPtr(i) := ibuffer.io.out(i).bits.ftqPtr
    checkTarget(i)    := checkPcMem((checkTargetPtr(i) + 1.U).value)
  }

  // FIXME: reconsider this check when newest_target_entry is deleted
  private def checkTakenNotConsecutive(): Unit = {
    val prevTakenValid  = RegInit(0.B)
    val prevTakenFtqPtr = Reg(new FtqPtr)
    for (i <- 0 until DecodeWidth - 1) {
      // for instrs that is not the last, if a taken br, the next instr should not have the same ftqPtr
      // for instrs that is the last, record and check next request
      when(ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && ibuffer.io.out(i).bits.fixedTaken) {
        when(ibuffer.io.out(i + 1).fire) {
          // not last br, check now
        }.otherwise {
          // last br, record its info
          prevTakenValid  := true.B
          prevTakenFtqPtr := checkTargetPtr(i)
        }
      }
      XSError(
        ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && ibuffer.io.out(i).bits.fixedTaken &&
          ibuffer.io.out(i + 1).fire &&
          (checkTargetPtr(i) + 1.U).value =/= checkTargetPtr(i + 1).value,
        "taken br should have consecutive ftqPtr\n"
      )
    }
    when(ibuffer.io.out(DecodeWidth - 1).fire && ibuffer.io.out(DecodeWidth - 1).bits.pd.isBr &&
      ibuffer.io.out(DecodeWidth - 1).bits.fixedTaken) {
      // last instr is a br, record its info
      prevTakenValid  := true.B
      prevTakenFtqPtr := checkTargetPtr(DecodeWidth - 1)
    }
    when(prevTakenValid && ibuffer.io.out(0).fire) {
      prevTakenValid := false.B
    }
    XSError(
      prevTakenValid && ibuffer.io.out(0).fire &&
        (prevTakenFtqPtr + 1.U).value =/= checkTargetPtr(0).value,
      "taken br should have consecutive ftqPtr\n"
    )
    when(needFlush) {
      prevTakenValid := false.B
    }
  }

  // FIXME: reconsider this check when newest_target_entry is deleted
  private def checkNotTakenPC(): Unit = {
    val prevNotTakenPC    = Reg(PrunedAddr(VAddrBits))
    val prevIsRVC         = Reg(Bool())
    val prevNotTakenValid = RegInit(0.B)

    for (i <- 0 until DecodeWidth - 1) {
      when(ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && !ibuffer.io.out(i).bits.fixedTaken) {
        when(ibuffer.io.out(i + 1).fire) {}.otherwise {
          prevNotTakenValid := true.B
          prevIsRVC         := ibuffer.io.out(i).bits.pd.isRVC
          prevNotTakenPC    := ibuffer.io.out(i).bits.pc
        }
      }
      XSError(
        ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && !ibuffer.io.out(i).bits.fixedTaken &&
          ibuffer.io.out(i + 1).fire &&
          ibuffer.io.out(i).bits.pc + Mux(ibuffer.io.out(i).bits.pd.isRVC, 2.U, 4.U) =/= ibuffer.io.out(
            i + 1
          ).bits.pc,
        "not-taken br should have consecutive pc\n"
      )
    }
    when(ibuffer.io.out(DecodeWidth - 1).fire && ibuffer.io.out(DecodeWidth - 1).bits.pd.isBr &&
      !ibuffer.io.out(DecodeWidth - 1).bits.fixedTaken) {
      prevNotTakenValid := true.B
      prevIsRVC         := ibuffer.io.out(DecodeWidth - 1).bits.pd.isRVC
      prevNotTakenPC    := ibuffer.io.out(DecodeWidth - 1).bits.pc
    }
    when(prevNotTakenValid && ibuffer.io.out(0).fire) {
      prevNotTakenValid := false.B
    }
    XSError(
      prevNotTakenValid && ibuffer.io.out(0).fire &&
        prevNotTakenPC + Mux(prevIsRVC, 2.U, 4.U) =/= PrunedAddrInit(ibuffer.io.out(0).bits.pc),
      "not-taken br should have same pc\n"
    )
    when(needFlush) {
      prevNotTakenValid := false.B
    }
  }

  // FIXME: reconsider this check when newest_target_entry is deleted
  private def checkTakenPC(): Unit = {
    val prevTakenFtqPtr = Reg(new FtqPtr)
    val prevTakenValid  = RegInit(0.B)
    val prevTakenTarget = Wire(PrunedAddr(VAddrBits))
    prevTakenTarget := checkPcMem((prevTakenFtqPtr + 1.U).value)

    for (i <- 0 until DecodeWidth - 1) {
      when(ibuffer.io.out(i).fire && !ibuffer.io.out(i).bits.pd.notCFI && ibuffer.io.out(i).bits.fixedTaken) {
        when(ibuffer.io.out(i + 1).fire) {}.otherwise {
          prevTakenValid  := true.B
          prevTakenFtqPtr := checkTargetPtr(i)
        }
      }
      XSError(
        ibuffer.io.out(i).fire && !ibuffer.io.out(i).bits.pd.notCFI && ibuffer.io.out(i).bits.fixedTaken &&
          ibuffer.io.out(i + 1).fire &&
          checkTarget(i) =/= PrunedAddrInit(ibuffer.io.out(i + 1).bits.pc),
        "taken instr should follow target pc\n"
      )
    }
    when(ibuffer.io.out(DecodeWidth - 1).fire && !ibuffer.io.out(DecodeWidth - 1).bits.pd.notCFI &&
      ibuffer.io.out(DecodeWidth - 1).bits.fixedTaken) {
      prevTakenValid  := true.B
      prevTakenFtqPtr := checkTargetPtr(DecodeWidth - 1)
    }
    when(prevTakenValid && ibuffer.io.out(0).fire) {
      prevTakenValid := false.B
    }
    XSError(
      prevTakenValid && ibuffer.io.out(0).fire &&
        prevTakenTarget =/= PrunedAddrInit(ibuffer.io.out(0).bits.pc),
      "taken instr should follow target pc\n"
    )
    when(needFlush) {
      prevTakenValid := false.B
    }
  }

  // FIXME: reconsider these checks when newest_target_entry is deleted
//  checkTakenNotConsecutive()
//  checkTakenPC()
//  checkNotTakenPC()

  ifu.io.robCommits <> io.backend.toFtq.rob_commits

  ibuffer.io.flush                := needFlush
  ibuffer.io.ControlRedirect      := FlushControlRedirect
  ibuffer.io.MemVioRedirect       := FlushMemVioRedirect
  ibuffer.io.ControlBTBMissBubble := FlushControlBTBMiss
  ibuffer.io.TAGEMissBubble       := FlushTAGEMiss
  ibuffer.io.SCMissBubble         := FlushSCMiss
  ibuffer.io.ITTAGEMissBubble     := FlushITTAGEMiss
  ibuffer.io.RASMissBubble        := FlushRASMiss
  ibuffer.io.decodeCanAccept      := io.backend.canAccept

  FlushControlBTBMiss := ftq.io.ControlBTBMissBubble
  FlushTAGEMiss       := ftq.io.TAGEMissBubble
  FlushSCMiss         := ftq.io.SCMissBubble
  FlushITTAGEMiss     := ftq.io.ITTAGEMissBubble
  FlushRASMiss        := ftq.io.RASMissBubble

  io.backend.cfVec <> ibuffer.io.out
  io.backend.stallReason <> ibuffer.io.stallReason

  instrUncache.io.fromIfu <> ifu.io.toUncache
  ifu.io.fromUncache <> instrUncache.io.toIfu
  instrUncache.io.flush := false.B

  io.error <> RegNext(RegNext(icache.io.error))

  icache.io.hartId := io.hartId

  itlbRepeater1.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr.map(_.toUInt)

  io.frontendInfo.ibufFull := RegNext(ibuffer.io.full)
  io.resetInFrontend       := reset.asBool

  // PFEvent
  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := io.csrCtrl.distribute_csr
  val csrevents = pfevent.io.hpmevent.take(8)

  val perfFromUnits = Seq(ifu, ibuffer, icache, ftq).flatMap(_.getPerfEvents)
  val perfFromIO    = Seq()
  val perfBlock     = Seq()
  // let index = 0 be no event
  val allPerfEvents = Seq(("noEvent", 0.U)) ++ perfFromUnits ++ perfFromIO ++ perfBlock

  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("Frontend perfEvents Set", name, inc, i)
    }
  }

  val allPerfInc          = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  override val perfEvents = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
  generatePerfEvent()

  private val mbistPl = MbistPipeline.PlaceMbistPipeline(Int.MaxValue, "MbistPipeFrontend", hasMbist)
  private val mbistIntf = if (hasMbist) {
    val params = mbistPl.get.nodeParams
    val intf = Some(Module(new MbistInterface(
      params = Seq(params),
      ids = Seq(mbistPl.get.childrenIds),
      name = s"MbistIntfFrontend",
      pipelineNum = 1
    )))
    intf.get.toPipeline.head <> mbistPl.get.mbist
    mbistPl.get.registerCSV(intf.get.info, "MbistFrontend")
    intf.get.mbist := DontCare
    dontTouch(intf.get.mbist)
    // TODO: add mbist controller connections here
    intf
  } else {
    None
  }
  private val sigFromSrams = if (hasDFT) Some(SramHelper.genBroadCastBundleTop()) else None
  private val cg           = ClockGate.genTeSrc
  dontTouch(cg)

  if (hasMbist) {
    cg.cgen := io.dft.get.cgen
  } else {
    cg.cgen := false.B
  }

  sigFromSrams.foreach { case sig => sig := DontCare }
  sigFromSrams.zip(io.dft).foreach {
    case (sig, dft) =>
      if (hasMbist) {
        sig.ram_hold     := dft.ram_hold
        sig.ram_bypass   := dft.ram_bypass
        sig.ram_bp_clken := dft.ram_bp_clken
        sig.ram_aux_clk  := dft.ram_aux_clk
        sig.ram_aux_ckbp := dft.ram_aux_ckbp
        sig.ram_mcp_hold := dft.ram_mcp_hold
        sig.cgen         := dft.cgen
      }
      if (hasSramCtl) {
        sig.ram_ctl := dft.ram_ctl
      }
  }

  // XSPerfCounters: Frontend Total
  XSPerfAccumulate(
    "numCycles",
    true.B
  )
  XSPerfAccumulate(
    "validCycles",
    ibuffer.io.out.map(_.valid && io.backend.canAccept).reduce(_ || _)
  )
  XSPerfAccumulate(
    "validInstrs",
    PopCount(ibuffer.io.out.map(_.valid && io.backend.canAccept))
  )
  XSPerfHistogram(
    "validInstrsDist",
    PopCount(ibuffer.io.out.map(_.valid)),
    io.backend.canAccept,
    0,
    DecodeWidth + 1
  )
  XSPerfAccumulate(
    "branchMispredicts",
    io.backend.toFtq.redirect.valid &&
      io.backend.toFtq.redirect.bits.isMisPred
  )
  // TODO: doubleLine
  XSPerfAccumulate(
    "fetchedCacheLines",
    Mux(
      icache.io.toIfu.fetchResp.fire,
      Mux(icache.io.toIfu.fetchResp.bits.doubleline, 2.U, 1.U),
      0.U
    )
  )

  // XSPerfCounters: Frontend Invalid
  XSPerfAccumulate(
    "stallCycles_fetch",
    !ftq.io.toIfu.req.fire
  )
  XSPerfAccumulate(
    "stallCycles_fetch_ftqNotvalid",
    !ftq.io.toIfu.req.valid
  )
  XSPerfAccumulate(
    "stallCycles_fetch_ifuNotReady",
    !ifu.io.fromFtq.req.ready
  )
  XSPerfAccumulate(
    "stallCycles_decodeFull",
    !io.backend.canAccept
  )
  XSPerfAccumulate(
    "stallCycles_ibufferFull",
    ibuffer.io.full
  )
  XSPerfAccumulate(
    "squashCycles",
    io.backend.toFtq.redirect.valid ||
      ifu.io.toFtq.wbRedirect.valid
  )
  XSPerfAccumulate(
    "squashCycles_bpWrong_preDecode",
    ifu.io.toFtq.wbRedirect.valid
  )
  XSPerfAccumulate(
    "squashCycles_bpWrong_redirect",
    io.backend.toFtq.redirect.valid && io.backend.toFtq.redirect.bits.isMisPred
  )
  XSPerfAccumulate(
    "squashCycles_memVio",
    io.backend.toFtq.redirect.valid && io.backend.toFtq.redirect.bits.debugIsMemVio
  )
  XSPerfAccumulate(
    "squashCycles_interrupt",
    io.backend.toFtq.redirect.valid && io.backend.toFtq.redirect.bits.interrupt
  )
  XSPerfAccumulate(
    "squashCycles_backendException",
    io.backend.toFtq.redirect.valid && io.backend.toFtq.redirect.bits.hasBackendFault
  )
}
