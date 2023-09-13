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

package xiangshan.frontend
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import utility._
import xiangshan._
import xiangshan.backend.fu.{PFEvent, PMP, PMPChecker,PMPReqBundle}
import xiangshan.cache.mmu._
import xiangshan.frontend.icache._


class Frontend()(implicit p: Parameters) extends LazyModule with HasXSParameter{

  val instrUncache  = LazyModule(new InstrUncache())
  val icache        = LazyModule(new ICache())

  lazy val module = new FrontendImp(this)
}


class FrontendImp (outer: Frontend) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    val reset_vector = Input(UInt(PAddrBits.W))
    val fencei = Input(Bool())
    val ptw = new TlbPtwIO()
    val backend = new FrontendToCtrlIO
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val csrUpdate = new DistributedCSRUpdateReq
    val error  = new L1CacheErrorInfo
    val frontendInfo = new Bundle {
      val ibufFull  = Output(Bool())
      val bpuInfo = new Bundle {
        val bpRight = Output(UInt(XLEN.W))
        val bpWrong = Output(UInt(XLEN.W))
      }
    }
  })

  //decouped-frontend modules
  val instrUncache = outer.instrUncache.module
  val icache       = outer.icache.module
  val bpu     = Module(new Predictor)
  val ifu     = Module(new NewIFU)
  val ibuffer =  Module(new Ibuffer)
  val ftq = Module(new Ftq)

  val needFlush = RegNext(io.backend.toFtq.redirect.valid)
  val FlushControlRedirect = RegNext(io.backend.toFtq.redirect.bits.debugIsCtrl)
  val FlushMemVioRedirect = RegNext(io.backend.toFtq.redirect.bits.debugIsMemVio)
  val FlushControlBTBMiss = Wire(Bool())
  val FlushTAGEMiss = Wire(Bool())
  val FlushSCMiss = Wire(Bool())
  val FlushITTAGEMiss = Wire(Bool())
  val FlushRASMiss = Wire(Bool())

  val tlbCsr = DelayN(io.tlbCsr, 2)
  val csrCtrl = DelayN(io.csrCtrl, 2)
  val sfence = RegNext(RegNext(io.sfence))

  // trigger
  ifu.io.frontendTrigger := csrCtrl.frontend_trigger
  val triggerEn = csrCtrl.trigger_enable
  ifu.io.csrTriggerEnable := VecInit(triggerEn(0), triggerEn(1), triggerEn(6), triggerEn(8))

  // bpu ctrl
  bpu.io.ctrl := csrCtrl.bp_ctrl
  bpu.io.reset_vector := io.reset_vector

// pmp
  val prefetchPipeNum = ICacheParameters().prefetchPipeNum
  val pmp = Module(new PMP())
  val pmp_check = VecInit(Seq.fill(coreParams.ipmpPortNum)(Module(new PMPChecker(3, sameCycle = true)).io))
  pmp.io.distribute_csr := csrCtrl.distribute_csr
  val pmp_req_vec     = Wire(Vec(coreParams.ipmpPortNum, Valid(new PMPReqBundle())))
  (0 until 2 + prefetchPipeNum).foreach(i => pmp_req_vec(i) <> icache.io.pmp(i).req)
  pmp_req_vec.last <> ifu.io.pmp.req

  for (i <- pmp_check.indices) {
    pmp_check(i).apply(tlbCsr.priv.imode, pmp.io.pmp, pmp.io.pma, pmp_req_vec(i))
  }
  (0 until 2 + prefetchPipeNum).foreach(i => icache.io.pmp(i).resp <> pmp_check(i).resp)
  ifu.io.pmp.resp <> pmp_check.last.resp

  val itlb = Module(new TLB(coreParams.itlbPortNum, nRespDups = 1,
    Seq(false, false) ++ Seq.fill(prefetchPipeNum)(false) ++ Seq(true), itlbParams))
  itlb.io.requestor.take(2 + prefetchPipeNum) zip icache.io.itlb foreach {case (a,b) => a <> b}
  itlb.io.requestor.last <> ifu.io.iTLBInter // mmio may need re-tlb, blocked
  itlb.io.base_connect(sfence, tlbCsr)
  itlb.io.flushPipe.map(_ := needFlush)

  val itlb_ptw = Wire(new VectorTlbPtwIO(coreParams.itlbPortNum))
  itlb_ptw.connect(itlb.io.ptw)
  val itlbRepeater1 = PTWFilter(itlbParams.fenceDelay, itlb_ptw, sfence, tlbCsr, l2tlbParams.ifilterSize)
  io.ptw <> itlbRepeater1.io.ptw

  icache.io.prefetch <> ftq.io.toPrefetch


  //IFU-Ftq
  ifu.io.ftqInter.fromFtq <> ftq.io.toIfu
  ftq.io.toIfu.req.ready :=  ifu.io.ftqInter.fromFtq.req.ready && icache.io.fetch.req.ready

  ftq.io.fromIfu          <> ifu.io.ftqInter.toFtq
  bpu.io.ftq_to_bpu       <> ftq.io.toBpu
  ftq.io.fromBpu          <> bpu.io.bpu_to_ftq

  ftq.io.mmioCommitRead   <> ifu.io.mmioCommitRead
  //IFU-ICache

  icache.io.fetch.req <> ftq.io.toICache.req
  ftq.io.toICache.req.ready :=  ifu.io.ftqInter.fromFtq.req.ready && icache.io.fetch.req.ready

  ifu.io.icacheInter.resp <>    icache.io.fetch.resp
  ifu.io.icacheInter.icacheReady :=  icache.io.toIFU
  ifu.io.icacheInter.topdownIcacheMiss := icache.io.fetch.topdownIcacheMiss
  ifu.io.icacheInter.topdownItlbMiss := icache.io.fetch.topdownItlbMiss
  icache.io.stop := ifu.io.icacheStop

  ifu.io.icachePerfInfo := icache.io.perfInfo

  icache.io.csr.distribute_csr <> DontCare
  io.csrUpdate := DontCare

  icache.io.csr_pf_enable     := RegNext(csrCtrl.l1I_pf_enable)
  icache.io.csr_parity_enable := RegNext(csrCtrl.icache_parity_enable)

  icache.io.fencei := io.fencei

  //IFU-Ibuffer
  ifu.io.toIbuffer    <> ibuffer.io.in

  ftq.io.fromBackend <> io.backend.toFtq
  io.backend.fromFtq <> ftq.io.toBackend
  io.frontendInfo.bpuInfo <> ftq.io.bpuInfo

  val checkPcMem = Reg(Vec(FtqSize, new Ftq_RF_Components))
  when (ftq.io.toBackend.pc_mem_wen) {
    checkPcMem(ftq.io.toBackend.pc_mem_waddr) := ftq.io.toBackend.pc_mem_wdata
  }

  val checkTargetIdx = Wire(Vec(DecodeWidth, UInt(log2Up(FtqSize).W)))
  val checkTarget = Wire(Vec(DecodeWidth, UInt(VAddrBits.W)))

  for (i <- 0 until DecodeWidth) {
    checkTargetIdx(i) := ibuffer.io.out(i).bits.ftqPtr.value
    checkTarget(i) := Mux(ftq.io.toBackend.newest_entry_ptr.value === checkTargetIdx(i),
                        ftq.io.toBackend.newest_entry_target,
                        checkPcMem(checkTargetIdx(i) + 1.U).startAddr)
  }

  // commented out for this br could be the last instruction in the fetch block
  def checkNotTakenConsecutive = {
    val prevNotTakenValid = RegInit(0.B)
    val prevNotTakenFtqIdx = Reg(UInt(log2Up(FtqSize).W))
    for (i <- 0 until DecodeWidth - 1) {
      // for instrs that is not the last, if a not-taken br, the next instr should have the same ftqPtr
      // for instrs that is the last, record and check next request
      when (ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr) {
        when (ibuffer.io.out(i+1).fire) {
          // not last br, check now
          XSError(checkTargetIdx(i) =/= checkTargetIdx(i+1), "not-taken br should have same ftqPtr\n")
        } .otherwise {
          // last br, record its info
          prevNotTakenValid := true.B
          prevNotTakenFtqIdx := checkTargetIdx(i)
        }
      }
    }
    when (ibuffer.io.out(DecodeWidth - 1).fire && ibuffer.io.out(DecodeWidth - 1).bits.pd.isBr) {
      // last instr is a br, record its info
      prevNotTakenValid := true.B
      prevNotTakenFtqIdx := checkTargetIdx(DecodeWidth - 1)
    }
    when (prevNotTakenValid && ibuffer.io.out(0).fire) {
      XSError(prevNotTakenFtqIdx =/= checkTargetIdx(0), "not-taken br should have same ftqPtr\n")
      prevNotTakenValid := false.B
    }
    when (needFlush) {
      prevNotTakenValid := false.B
    }
  }

  def checkTakenNotConsecutive = {
    val prevTakenValid = RegInit(0.B)
    val prevTakenFtqIdx = Reg(UInt(log2Up(FtqSize).W))
    for (i <- 0 until DecodeWidth - 1) {
      // for instrs that is not the last, if a taken br, the next instr should not have the same ftqPtr
      // for instrs that is the last, record and check next request
      when (ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && ibuffer.io.out(i).bits.pred_taken) {
        when (ibuffer.io.out(i+1).fire) {
          // not last br, check now
          XSError(checkTargetIdx(i) + 1.U =/= checkTargetIdx(i+1), "taken br should have consecutive ftqPtr\n")
        } .otherwise {
          // last br, record its info
          prevTakenValid := true.B
          prevTakenFtqIdx := checkTargetIdx(i)
        }
      }
    }
    when (ibuffer.io.out(DecodeWidth - 1).fire && ibuffer.io.out(DecodeWidth - 1).bits.pd.isBr && ibuffer.io.out(DecodeWidth - 1).bits.pred_taken) {
      // last instr is a br, record its info
      prevTakenValid := true.B
      prevTakenFtqIdx := checkTargetIdx(DecodeWidth - 1)
    }
    when (prevTakenValid && ibuffer.io.out(0).fire) {
      XSError(prevTakenFtqIdx + 1.U =/= checkTargetIdx(0), "taken br should have consecutive ftqPtr\n")
      prevTakenValid := false.B
    }
    when (needFlush) {
      prevTakenValid := false.B
    }
  }

  def checkNotTakenPC = {
    val prevNotTakenPC = Reg(UInt(VAddrBits.W))
    val prevIsRVC = Reg(Bool())
    val prevNotTakenValid = RegInit(0.B)

    for (i <- 0 until DecodeWidth - 1) {
      when (ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && !ibuffer.io.out(i).bits.pred_taken) {
        when (ibuffer.io.out(i+1).fire) {
          XSError(ibuffer.io.out(i).bits.pc + Mux(ibuffer.io.out(i).bits.pd.isRVC, 2.U, 4.U) =/= ibuffer.io.out(i+1).bits.pc, "not-taken br should have consecutive pc\n")
        } .otherwise {
          prevNotTakenValid := true.B
          prevIsRVC := ibuffer.io.out(i).bits.pd.isRVC
          prevNotTakenPC := ibuffer.io.out(i).bits.pc
        }
      }
    }
    when (ibuffer.io.out(DecodeWidth - 1).fire && ibuffer.io.out(DecodeWidth - 1).bits.pd.isBr && !ibuffer.io.out(DecodeWidth - 1).bits.pred_taken) {
      prevNotTakenValid := true.B
      prevIsRVC := ibuffer.io.out(DecodeWidth - 1).bits.pd.isRVC
      prevNotTakenPC := ibuffer.io.out(DecodeWidth - 1).bits.pc
    }
    when (prevNotTakenValid && ibuffer.io.out(0).fire) {
      XSError(prevNotTakenPC + Mux(prevIsRVC, 2.U, 4.U) =/= ibuffer.io.out(0).bits.pc, "not-taken br should have same pc\n")
      prevNotTakenValid := false.B
    }
    when (needFlush) {
      prevNotTakenValid := false.B
    }
  }

  def checkTakenPC = {
    val prevTakenFtqIdx = Reg(UInt(log2Up(FtqSize).W))
    val prevTakenValid = RegInit(0.B)
    val prevTakenTarget = Wire(UInt(VAddrBits.W))
    prevTakenTarget := checkPcMem(prevTakenFtqIdx + 1.U).startAddr

    for (i <- 0 until DecodeWidth - 1) {
      when (ibuffer.io.out(i).fire && !ibuffer.io.out(i).bits.pd.notCFI && ibuffer.io.out(i).bits.pred_taken) {
        when (ibuffer.io.out(i+1).fire) {
          XSError(checkTarget(i) =/= ibuffer.io.out(i+1).bits.pc, "taken instr should follow target pc\n")
        } .otherwise {
          prevTakenValid := true.B
          prevTakenFtqIdx := checkTargetIdx(i)
        }
      }
    }
    when (ibuffer.io.out(DecodeWidth - 1).fire && !ibuffer.io.out(DecodeWidth - 1).bits.pd.notCFI && ibuffer.io.out(DecodeWidth - 1).bits.pred_taken) {
      prevTakenValid := true.B
      prevTakenFtqIdx := checkTargetIdx(DecodeWidth - 1)
    }
    when (prevTakenValid && ibuffer.io.out(0).fire) {
      XSError(prevTakenTarget =/= ibuffer.io.out(0).bits.pc, "taken instr should follow target pc\n")
      prevTakenValid := false.B
    }
    when (needFlush) {
      prevTakenValid := false.B
    }
  }

  //checkNotTakenConsecutive
  checkTakenNotConsecutive
  checkTakenPC
  checkNotTakenPC

  ifu.io.rob_commits <> io.backend.toFtq.rob_commits

  ibuffer.io.flush := needFlush
  ibuffer.io.ControlRedirect := FlushControlRedirect
  ibuffer.io.MemVioRedirect := FlushMemVioRedirect
  ibuffer.io.ControlBTBMissBubble := FlushControlBTBMiss
  ibuffer.io.TAGEMissBubble := FlushTAGEMiss
  ibuffer.io.SCMissBubble := FlushSCMiss
  ibuffer.io.ITTAGEMissBubble := FlushITTAGEMiss
  ibuffer.io.RASMissBubble := FlushRASMiss

  FlushControlBTBMiss := ftq.io.ControlBTBMissBubble
  FlushTAGEMiss := ftq.io.TAGEMissBubble
  FlushSCMiss := ftq.io.SCMissBubble
  FlushITTAGEMiss := ftq.io.ITTAGEMissBubble
  FlushRASMiss := ftq.io.RASMissBubble

  io.backend.cfVec <> ibuffer.io.out
  io.backend.stallReason <> ibuffer.io.stallReason
  dontTouch(io.backend.stallReason)

  instrUncache.io.req   <> ifu.io.uncacheInter.toUncache
  ifu.io.uncacheInter.fromUncache <> instrUncache.io.resp
  instrUncache.io.flush := false.B
  io.error <> RegNext(RegNext(icache.io.error))

  icache.io.hartId := io.hartId

  val frontendBubble = PopCount((0 until DecodeWidth).map(i => io.backend.cfVec(i).ready && !ibuffer.io.out(i).valid))
  XSPerfAccumulate("FrontendBubble", frontendBubble)
  io.frontendInfo.ibufFull := RegNext(ibuffer.io.full)

  // PFEvent
  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := io.csrCtrl.distribute_csr
  val csrevents = pfevent.io.hpmevent.take(8)

  val allPerfEvents = Seq(ifu, ibuffer, icache, ftq, bpu).flatMap(_.getPerf)
  override val perfEvents = HPerfMonitor(csrevents, allPerfEvents).getPerfEvents
  generatePerfEvent()
}
