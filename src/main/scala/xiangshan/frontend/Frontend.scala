// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

// Acknowledgement
//
// This implementation is inspired by several key papers:
// [1] Alex Ramirez, Oliverio J. Santana, Josep L. Larriba-Pey, and Mateo Valero. "[Fetching instruction streams.]
// (https://doi.org/10.1109/MICRO.2002.1176264)" 35th Annual IEEE/ACM International Symposium on Microarchitecture
// (MICRO). 2002.
// [2] Yasuo Ishii, Jaekyu Lee, Krishnendra Nathella, and Dam Sunwoo. "[Rebasing instruction prefetching: An industry
// perspective.](https://doi.org/10.1109/LCA.2020.3035068)" IEEE Computer Architecture Letters 19.2: 147-150. 2020.
// [3] Yasuo Ishii, Jaekyu Lee, Krishnendra Nathella, and Dam Sunwoo. "[Re-establishing fetch-directed instruction
// prefetching: An industry perspective.](https://doi.org/10.1109/ISPASS51385.2021.00034)" 2021 IEEE International
// Symposium on Performance Analysis of Systems and Software (ISPASS). 2021.

package xiangshan.frontend
import chisel3._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import org.chipsalliance.cde.config.Parameters
import utility.ClockGate
import utility.DelayN
import utility.HasPerfEvents
import utility.HPerfMonitor
import utility.ModuleNode
import utility.PerfEvent
import utility.ResetGen
import utility.ResetGenNode
import utility.XSError
import utility.mbist.MbistInterface
import utility.mbist.MbistPipeline
import utility.sram.SramHelper
import xiangshan.DebugOptionsKey
import xiangshan.backend.fu.NewCSR.PFEvent
import xiangshan.backend.fu.PMP
import xiangshan.backend.fu.PMPChecker
import xiangshan.cache.mmu.PTWFilter
import xiangshan.cache.mmu.PTWRepeaterNB
import xiangshan.cache.mmu.TLB
import xiangshan.cache.mmu.VectorTlbPtwIO
import xiangshan.frontend.bpu.Bpu
import xiangshan.frontend.ftq.Ftq
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.ibuffer.IBuffer
import xiangshan.frontend.icache.ICache
import xiangshan.frontend.ifu.Ifu
import xiangshan.frontend.instruncache.InstrUncache
import xiangshan.frontend.simfrontend.SimFrontendInlinedImp

class Frontend()(implicit p: Parameters) extends LazyModule with HasFrontendParameters {
  override def shouldBeInlined: Boolean = false

  val inner:       FrontendInlined = LazyModule(new FrontendInlined)
  lazy val module: FrontendImp     = new FrontendImp(this)
}

class FrontendImp(wrapper: Frontend)(implicit p: Parameters) extends LazyModuleImp(wrapper) {
  val io:      FrontendIO     = IO(wrapper.inner.module.io.cloneType)
  val io_perf: Vec[PerfEvent] = IO(wrapper.inner.module.io_perf.cloneType)

  io <> wrapper.inner.module.io
  io_perf <> wrapper.inner.module.io_perf

  if (p(DebugOptionsKey).ResetGen) {
    ResetGen(ResetGenNode(Seq(ModuleNode(wrapper.inner.module))), reset, sim = false, io.dft_reset)
  }
}

abstract class FrontendInlinedImpBase(outer: FrontendInlined) extends LazyModuleImp(outer)
    with HasFrontendParameters
    with HasPerfEvents {

  val io: FrontendIO = IO(new FrontendIO)
}

class FrontendInlined()(implicit p: Parameters) extends LazyModule with HasFrontendParameters {
  override def shouldBeInlined: Boolean = true

  val instrUncache: InstrUncache = LazyModule(new InstrUncache)
  val icache:       ICache       = LazyModule(new ICache)

  lazy val module: FrontendInlinedImpBase =
    if (env.EnableSimFrontend) new SimFrontendInlinedImp(this) else new FrontendInlinedImp(this)
}

class FrontendInlinedImp(outer: FrontendInlined) extends FrontendInlinedImpBase(outer) {

  // decoupled-frontend modules
  private val instrUncache = outer.instrUncache.module
  private val icache       = outer.icache.module
  private val bpu          = Module(new Bpu)
  private val ifu          = Module(new Ifu)
  private val ibuffer      = Module(new IBuffer)
  private val ftq          = Module(new Ftq)

  private val needFlush = RegNext(io.backend.toFtq.redirect.valid)

  // FIXME: top-down related signals, should be redesigned
  private val FlushControlRedirect = RegNext(io.backend.toFtq.redirect.bits.debugIsCtrl)
  private val FlushMemVioRedirect  = RegNext(io.backend.toFtq.redirect.bits.debugIsMemVio)
  private val FlushControlBTBMiss  = Wire(Bool())
  private val FlushTAGEMiss        = Wire(Bool())
  private val FlushSCMiss          = Wire(Bool())
  private val FlushITTAGEMiss      = Wire(Bool())
  private val FlushRASMiss         = Wire(Bool())

  // TODO: what the fuck are these magic numbers?
  private val tlbCsr  = DelayN(io.tlbCsr, 1)
  private val csrCtrl = DelayN(io.csrCtrl, 2)
  private val sfence  = DelayN(io.sfence, 2)

  // trigger
  ifu.io.frontendTrigger := csrCtrl.frontend_trigger

  // RVCDecoder fsIsOff
  ifu.io.csrFsIsOff := csrCtrl.fsIsOff

  // bpu ctrl
  bpu.io.ctrl        := csrCtrl.bp_ctrl
  bpu.io.resetVector := io.resetVector

  // pmp
  private val pmp = Module(new PMP)
  pmp.io.distribute_csr := csrCtrl.distribute_csr

  private val pmpChecker = VecInit(Seq.fill(coreParams.ipmpPortNum)(Module(new PMPChecker(sameCycle = true)).io))

  private val pmpRequestor = VecInit(icache.io.pmp :+ ifu.io.pmp)
  require(pmpRequestor.length == coreParams.ipmpPortNum)

  (pmpChecker zip pmpRequestor).foreach { case (checker, requestor) =>
    if (HasBitmapCheck) {
      checker.apply(tlbCsr.mbmc.CMODE.asBool, tlbCsr.priv.imode, pmp.io.pmp, pmp.io.pma, requestor.req)
    } else {
      checker.apply(tlbCsr.priv.imode, pmp.io.pmp, pmp.io.pma, requestor.req)
    }
    requestor.resp := checker.resp
  }

  // icache use a non-block tlb port, ifu use a block tlb port
  private val itlb = Module(new TLB(coreParams.itlbPortNum, nRespDups = 1, Seq(false, true), itlbParams))
  itlb.io.hartId := io.hartId
  itlb.io.flushPipe.foreach(_ := icache.io.itlbFlushPipe)
  itlb.io.redirect := DontCare // itlb has flushpipe, don't need redirect signal
  itlb.io.base_connect(sfence, tlbCsr)

  private val itlbRequestor = VecInit(Seq(icache.io.itlb, ifu.io.itlb))
  require(itlbRequestor.length == coreParams.itlbPortNum)

  (itlb.io.requestor zip itlbRequestor).foreach { case (port, requestor) =>
    port <> requestor
  }

  private val ptw = Wire(new VectorTlbPtwIO(coreParams.itlbPortNum))
  ptw.connect(itlb.io.ptw)
  private val itlbRepeater1 =
    PTWFilter(itlbParams.fenceDelay, ptw, sfence, tlbCsr, l2tlbParams.ifilterSize)
  private val itlbRepeater2 =
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

  private val checkPcMem = Reg(Vec(FtqSize, new PrunedAddr(VAddrBits)))
  when(ftq.io.toBackend.pc_mem_wen) {
    checkPcMem(ftq.io.toBackend.pc_mem_waddr) := ftq.io.toBackend.pc_mem_wdata
  }

  private val checkTargetPtr = Wire(Vec(DecodeWidth, new FtqPtr))
  private val checkTarget    = Wire(Vec(DecodeWidth, PrunedAddr(VAddrBits)))

  for (i <- 0 until DecodeWidth) {
    checkTargetPtr(i) := ibuffer.io.out(i).bits.ftqPtr
    checkTarget(i) := Mux(
      ftq.io.toBackend.newest_entry_ptr.value === checkTargetPtr(i).value,
      PrunedAddrInit(ftq.io.toBackend.newest_entry_target),
      checkPcMem((checkTargetPtr(i) + 1.U).value)
    )
  }

  // FIXME: reconsider this check when newest_target_entry is deleted
  private def checkTakenNotConsecutive(): Unit = {
    val prevTakenValid  = RegInit(0.B)
    val prevTakenFtqPtr = Reg(new FtqPtr)
    for (i <- 0 until DecodeWidth - 1) {
      // for instrs that is not the last, if a taken br, the next instr should not have the same ftqPtr
      // for instrs that is the last, record and check next request
      when(ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && ibuffer.io.out(i).bits.pred_taken) {
        when(ibuffer.io.out(i + 1).fire) {
          // not last br, check now
        }.otherwise {
          // last br, record its info
          prevTakenValid  := true.B
          prevTakenFtqPtr := checkTargetPtr(i)
        }
      }
      XSError(
        ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && ibuffer.io.out(i).bits.pred_taken &&
          ibuffer.io.out(i + 1).fire &&
          (checkTargetPtr(i) + 1.U).value =/= checkTargetPtr(i + 1).value,
        "taken br should have consecutive ftqPtr\n"
      )
    }
    when(ibuffer.io.out(DecodeWidth - 1).fire && ibuffer.io.out(DecodeWidth - 1).bits.pd.isBr && ibuffer.io.out(
      DecodeWidth - 1
    ).bits.pred_taken) {
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
      when(ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && !ibuffer.io.out(i).bits.pred_taken) {
        when(ibuffer.io.out(i + 1).fire) {}.otherwise {
          prevNotTakenValid := true.B
          prevIsRVC         := ibuffer.io.out(i).bits.pd.isRVC
          prevNotTakenPC    := ibuffer.io.out(i).bits.pc
        }
      }
      XSError(
        ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && !ibuffer.io.out(i).bits.pred_taken &&
          ibuffer.io.out(i + 1).fire &&
          ibuffer.io.out(i).bits.pc + Mux(ibuffer.io.out(i).bits.pd.isRVC, 2.U, 4.U) =/= ibuffer.io.out(
            i + 1
          ).bits.pc,
        "not-taken br should have consecutive pc\n"
      )
    }
    when(ibuffer.io.out(DecodeWidth - 1).fire && ibuffer.io.out(DecodeWidth - 1).bits.pd.isBr && !ibuffer.io.out(
      DecodeWidth - 1
    ).bits.pred_taken) {
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
      when(ibuffer.io.out(i).fire && !ibuffer.io.out(i).bits.pd.notCFI && ibuffer.io.out(i).bits.pred_taken) {
        when(ibuffer.io.out(i + 1).fire) {}.otherwise {
          prevTakenValid  := true.B
          prevTakenFtqPtr := checkTargetPtr(i)
        }
      }
      XSError(
        ibuffer.io.out(i).fire && !ibuffer.io.out(i).bits.pd.notCFI && ibuffer.io.out(i).bits.pred_taken &&
          ibuffer.io.out(i + 1).fire &&
          checkTarget(i) =/= PrunedAddrInit(ibuffer.io.out(i + 1).bits.pc),
        "taken instr should follow target pc\n"
      )
    }
    when(ibuffer.io.out(DecodeWidth - 1).fire && !ibuffer.io.out(DecodeWidth - 1).bits.pd.notCFI && ibuffer.io.out(
      DecodeWidth - 1
    ).bits.pred_taken) {
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
  private val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := io.csrCtrl.distribute_csr
  private val csrevents = pfevent.io.hpmevent.take(8)

  private val perfFromUnits = Seq(ifu, ibuffer, icache, ftq).flatMap(_.getPerfEvents)
  private val perfFromIO    = Seq()
  private val perfBlock     = Seq()
  // let index = 0 be no event
  private val allPerfEvents = Seq(("noEvent", 0.U)) ++ perfFromUnits ++ perfFromIO ++ perfBlock

  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("Frontend perfEvents Set", name, inc, i)
    }
  }

  private val allPerfInc = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  override val perfEvents: Seq[(String, UInt)] = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
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

  sigFromSrams.foreach(_ := DontCare)
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
}
