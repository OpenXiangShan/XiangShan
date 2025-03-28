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
import org.chipsalliance.cde.config.Parameters
import utility._
import utility.mbist.MbistInterface
import utility.mbist.MbistPipeline
import utility.sram.SramBroadcastBundle
import utility.sram.SramCtlBundle
import utility.sram.SramHelper
import utility.sram.SramMbistBundle
import xiangshan._
import xiangshan.backend.fu.NewCSR.PFEvent
import xiangshan.backend.fu.PMP
import xiangshan.backend.fu.PMPChecker
import xiangshan.backend.fu.PMPReqBundle
import xiangshan.cache.mmu._
import xiangshan.frontend.icache._

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
    ResetGen(ResetGenNode(Seq(ModuleNode(wrapper.inner.module))), reset, sim = false, io.sramTest.mbistReset)
  }
}

class FrontendInlined()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = true

  val instrUncache = LazyModule(new InstrUncache())
  val icache       = LazyModule(new ICache())

  lazy val module = new FrontendInlinedImp(this)
}

class FrontendInlinedImp(outer: FrontendInlined) extends LazyModuleImp(outer)
    with HasXSParameter
    with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId       = Input(UInt(hartIdLen.W))
    val reset_vector = Input(UInt(PAddrBits.W))
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
      val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
    }
    val sramTest = new Bundle() {
      val mbist      = Option.when(hasMbist)(Input(new SramMbistBundle))
      val mbistReset = Option.when(hasMbist)(Input(new DFTResetSignals()))
      val sramCtl    = Option.when(hasSramCtl)(Input(UInt(64.W)))
    }
  })

  // decouped-frontend modules
  val instrUncache = outer.instrUncache.module
  val icache       = outer.icache.module
  val bpu          = Module(new Predictor)
  val ifu          = Module(new NewIFU)
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

  val tlbCsr  = DelayN(io.tlbCsr, 2)
  val csrCtrl = DelayN(io.csrCtrl, 2)
  val sfence  = RegNext(RegNext(io.sfence))

  // trigger
  ifu.io.frontendTrigger := csrCtrl.frontend_trigger

  // RVCDecoder fsIsOff
  ifu.io.csr_fsIsOff := csrCtrl.fsIsOff

  // bpu ctrl
  bpu.io.ctrl         := csrCtrl.bp_ctrl
  bpu.io.reset_vector := io.reset_vector

  // pmp
  val PortNumber = ICacheParameters().PortNumber
  val pmp        = Module(new PMP())
  val pmp_check  = VecInit(Seq.fill(coreParams.ipmpPortNum)(Module(new PMPChecker(3, sameCycle = true)).io))
  pmp.io.distribute_csr := csrCtrl.distribute_csr
  val pmp_req_vec = Wire(Vec(coreParams.ipmpPortNum, Valid(new PMPReqBundle())))
  (0 until 2 * PortNumber).foreach(i => pmp_req_vec(i) <> icache.io.pmp(i).req)
  pmp_req_vec.last <> ifu.io.pmp.req

  for (i <- pmp_check.indices) {
    if (HasBitmapCheck) {
      pmp_check(i).apply(tlbCsr.mbmc.CMODE.asBool, tlbCsr.priv.imode, pmp.io.pmp, pmp.io.pma, pmp_req_vec(i))
    } else {
      pmp_check(i).apply(tlbCsr.priv.imode, pmp.io.pmp, pmp.io.pma, pmp_req_vec(i))
    }
  }
  (0 until 2 * PortNumber).foreach(i => icache.io.pmp(i).resp <> pmp_check(i).resp)
  ifu.io.pmp.resp <> pmp_check.last.resp

  val itlb =
    Module(new TLB(coreParams.itlbPortNum, nRespDups = 1, Seq.fill(PortNumber)(false) ++ Seq(true), itlbParams))
  itlb.io.requestor.take(PortNumber) zip icache.io.itlb foreach { case (a, b) => a <> b }
  itlb.io.requestor.last <> ifu.io.iTLBInter // mmio may need re-tlb, blocked
  itlb.io.hartId := io.hartId
  itlb.io.base_connect(sfence, tlbCsr)
  itlb.io.flushPipe.foreach(_ := icache.io.itlbFlushPipe)
  itlb.io.redirect := DontCare // itlb has flushpipe, don't need redirect signal

  val itlb_ptw = Wire(new VectorTlbPtwIO(coreParams.itlbPortNum))
  itlb_ptw.connect(itlb.io.ptw)
  val itlbRepeater1 = PTWFilter(itlbParams.fenceDelay, itlb_ptw, sfence, tlbCsr, l2tlbParams.ifilterSize)
  val itlbRepeater2 =
    PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, itlbRepeater1.io.ptw, io.ptw, sfence, tlbCsr)

  icache.io.ftqPrefetch <> ftq.io.toPrefetch
  icache.io.softPrefetch <> io.softPrefetch

  // IFU-Ftq
  ifu.io.ftqInter.fromFtq <> ftq.io.toIfu
  ftq.io.toIfu.req.ready := ifu.io.ftqInter.fromFtq.req.ready && icache.io.fetch.req.ready

  ftq.io.fromIfu <> ifu.io.ftqInter.toFtq
  bpu.io.ftq_to_bpu <> ftq.io.toBpu
  ftq.io.fromBpu <> bpu.io.bpu_to_ftq

  ftq.io.mmioCommitRead <> ifu.io.mmioCommitRead

  // IFU-ICache
  icache.io.fetch.req <> ftq.io.toICache.req
  ftq.io.toICache.req.ready := ifu.io.ftqInter.fromFtq.req.ready && icache.io.fetch.req.ready

  ifu.io.icacheInter.resp <> icache.io.fetch.resp
  ifu.io.icacheInter.icacheReady       := icache.io.toIFU
  ifu.io.icacheInter.topdownIcacheMiss := icache.io.fetch.topdownIcacheMiss
  ifu.io.icacheInter.topdownItlbMiss   := icache.io.fetch.topdownItlbMiss
  icache.io.stop                       := ifu.io.icacheStop
  icache.io.flush                      := ftq.io.icacheFlush

  ifu.io.icachePerfInfo := icache.io.perfInfo

  icache.io.csr_pf_enable := RegNext(csrCtrl.pf_ctrl.l1I_pf_enable)

  icache.io.fencei := RegNext(io.fencei)

  // IFU-Ibuffer
  ifu.io.toIbuffer <> ibuffer.io.in

  ftq.io.fromBackend <> io.backend.toFtq
  io.backend.fromFtq := ftq.io.toBackend
  io.backend.fromIfu := ifu.io.toBackend
  io.frontendInfo.bpuInfo <> ftq.io.bpuInfo

  val checkPcMem = Reg(Vec(FtqSize, new Ftq_RF_Components))
  when(ftq.io.toBackend.pc_mem_wen) {
    checkPcMem(ftq.io.toBackend.pc_mem_waddr) := ftq.io.toBackend.pc_mem_wdata
  }

  val checkTargetPtr = Wire(Vec(DecodeWidth, new FtqPtr))
  val checkTarget    = Wire(Vec(DecodeWidth, UInt(VAddrBits.W)))

  for (i <- 0 until DecodeWidth) {
    checkTargetPtr(i) := ibuffer.io.out(i).bits.ftqPtr
    checkTarget(i) := Mux(
      ftq.io.toBackend.newest_entry_ptr.value === checkTargetPtr(i).value,
      ftq.io.toBackend.newest_entry_target,
      checkPcMem((checkTargetPtr(i) + 1.U).value).startAddr
    )
  }

  // commented out for this br could be the last instruction in the fetch block
  def checkNotTakenConsecutive = {
    val prevNotTakenValid  = RegInit(0.B)
    val prevNotTakenFtqPtr = Reg(new FtqPtr)
    for (i <- 0 until DecodeWidth - 1) {
      // for instrs that is not the last, if a not-taken br, the next instr should have the same ftqPtr
      // for instrs that is the last, record and check next request
      when(ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr) {
        when(ibuffer.io.out(i + 1).fire) {
          // not last br, check now
        }.otherwise {
          // last br, record its info
          prevNotTakenValid  := true.B
          prevNotTakenFtqPtr := checkTargetPtr(i)
        }
      }
      XSError(
        ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr &&
          ibuffer.io.out(i + 1).fire &&
          checkTargetPtr(i).value =/= checkTargetPtr(i + 1).value,
        "not-taken br should have same ftqPtr\n"
      )
    }
    when(ibuffer.io.out(DecodeWidth - 1).fire && ibuffer.io.out(DecodeWidth - 1).bits.pd.isBr) {
      // last instr is a br, record its info
      prevNotTakenValid  := true.B
      prevNotTakenFtqPtr := checkTargetPtr(DecodeWidth - 1)
    }
    when(prevNotTakenValid && ibuffer.io.out(0).fire) {
      prevNotTakenValid := false.B
    }
    XSError(
      prevNotTakenValid && ibuffer.io.out(0).fire &&
        prevNotTakenFtqPtr.value =/= checkTargetPtr(0).value,
      "not-taken br should have same ftqPtr\n"
    )

    when(needFlush) {
      prevNotTakenValid := false.B
    }
  }

  def checkTakenNotConsecutive = {
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

  def checkNotTakenPC = {
    val prevNotTakenPC    = Reg(UInt(VAddrBits.W))
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
        prevNotTakenPC + Mux(prevIsRVC, 2.U, 4.U) =/= ibuffer.io.out(0).bits.pc,
      "not-taken br should have same pc\n"
    )
    when(needFlush) {
      prevNotTakenValid := false.B
    }
  }

  def checkTakenPC = {
    val prevTakenFtqPtr = Reg(new FtqPtr)
    val prevTakenValid  = RegInit(0.B)
    val prevTakenTarget = Wire(UInt(VAddrBits.W))
    prevTakenTarget := checkPcMem((prevTakenFtqPtr + 1.U).value).startAddr

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
          checkTarget(i) =/= ibuffer.io.out(i + 1).bits.pc,
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
        prevTakenTarget =/= ibuffer.io.out(0).bits.pc,
      "taken instr should follow target pc\n"
    )
    when(needFlush) {
      prevTakenValid := false.B
    }
  }

  // checkNotTakenConsecutive
  checkTakenNotConsecutive
  checkTakenPC
  checkNotTakenPC

  ifu.io.rob_commits <> io.backend.toFtq.rob_commits

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

  instrUncache.io.req <> ifu.io.uncacheInter.toUncache
  ifu.io.uncacheInter.fromUncache <> instrUncache.io.resp
  instrUncache.io.flush := false.B
  io.error <> RegNext(RegNext(icache.io.error))

  icache.io.hartId := io.hartId

  itlbRepeater1.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr

  io.frontendInfo.ibufFull := RegNext(ibuffer.io.full)
  io.resetInFrontend       := reset.asBool

  // PFEvent
  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := io.csrCtrl.distribute_csr
  val csrevents = pfevent.io.hpmevent.take(8)

  val perfFromUnits = Seq(ifu, ibuffer, icache, ftq, bpu).flatMap(_.getPerfEvents)
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
  private val sigFromSrams = if (hasSramTest) Some(SramHelper.genBroadCastBundleTop()) else None
  private val cg           = ClockGate.genTeSrc
  dontTouch(cg)

  sigFromSrams.foreach { case sig => sig.mbist := DontCare }
  if (hasMbist) {
    sigFromSrams.get.mbist := io.sramTest.mbist.get
    cg.cgen                := io.sramTest.mbist.get.cgen
  } else {
    cg.cgen := false.B
  }

  sigFromSrams.foreach { case sig => sig.sramCtl := DontCare }
  if (hasSramCtl) {
    val sramCtlBundle = io.sramTest.sramCtl.get.asTypeOf(new SramCtlBundle)
    sigFromSrams.get.sramCtl.MCR := sramCtlBundle.MCR // CFG[5 : 4]
    sigFromSrams.get.sramCtl.MCW := sramCtlBundle.MCW // CFG[7 : 6]
    sigFromSrams.get.sramCtl.RCT := sramCtlBundle.RCT // CFG[35 : 34]
    sigFromSrams.get.sramCtl.WCT := sramCtlBundle.WCT // CFG[37 : 36]
    sigFromSrams.get.sramCtl.KP  := sramCtlBundle.KP  // CFG[40 : 38]
  }
}
