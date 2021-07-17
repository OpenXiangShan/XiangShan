/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
import utils.{AsyncDataModuleTemplate, CircularQueuePtr, DataModuleTemplate, HasCircularQueuePtrHelper, SRAMTemplate, SyncDataModuleTemplate, XSDebug, XSPerfAccumulate, XSError}
import xiangshan._
import scala.tools.nsc.doc.model.Val
import utils.{ParallelPriorityMux, ParallelPriorityEncoder}
import xiangshan.backend.{CtrlToFtqIO}

class FtqPtr(implicit p: Parameters) extends CircularQueuePtr[FtqPtr](
  p => p(XSCoreParamsKey).FtqSize
){
  override def cloneType = (new FtqPtr).asInstanceOf[this.type]
}

object FtqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): FtqPtr = {
    val ptr = Wire(new FtqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class FtqNRSRAM[T <: Data](gen: T, numRead: Int)(implicit p: Parameters) extends XSModule {

  val io = IO(new Bundle() {
    val raddr = Input(Vec(numRead, UInt(log2Up(FtqSize).W)))
    val ren = Input(Vec(numRead, Bool()))
    val rdata = Output(Vec(numRead, gen))
    val waddr = Input(UInt(log2Up(FtqSize).W))
    val wen = Input(Bool())
    val wdata = Input(gen)
  })

  for(i <- 0 until numRead){
    val sram = Module(new SRAMTemplate(gen, FtqSize))
    sram.io.r.req.valid := io.ren(i)
    sram.io.r.req.bits.setIdx := io.raddr(i)
    io.rdata(i) := sram.io.r.resp.data(0)
    sram.io.w.req.valid := io.wen
    sram.io.w.req.bits.setIdx := io.waddr
    sram.io.w.req.bits.data := VecInit(io.wdata)
  }

}

class Ftq_RF_Components(implicit p: Parameters) extends XSBundle {
  val startAddr = UInt(VAddrBits.W)
  val fallThruAddr = UInt(VAddrBits.W)
  val isNextMask = Vec(16, Bool())
  def getPc(offset: UInt) = {
    def getHigher(pc: UInt) = pc(VAddrBits-1, log2Ceil(16)+instOffsetBits)
    def getOffset(pc: UInt) = pc(log2Ceil(16)+instOffsetBits-1, instOffsetBits)
    Cat(getHigher(Mux(isNextMask(offset), fallThruAddr, startAddr)),
        getOffset(Mux(isNextMask(offset), fallThruAddr, startAddr))+offset,
        0.U(instOffsetBits.W))
  }
}

class Ftq_pd_Entry(implicit p: Parameters) extends XSBundle {
  val brMask = Vec(16, Bool())
  val jmpInfo = ValidUndirectioned(Vec(3, Bool()))
  val jmpOffset = UInt(4.W)
}

class Ftq_Redirect_SRAMEntry(implicit p: Parameters) extends XSBundle {
  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasEntry = new RASEntry
  val specCnt = Vec(1, UInt(10.W))
}

class Ftq_1R_SRAMEntry(implicit p: Parameters) extends XSBundle {
  val meta = UInt(120.W)
}

class FtqEntry(implicit p: Parameters) extends XSBundle {
  val startAddr = UInt(VAddrBits.W)
  val fallThruAddr = UInt(VAddrBits.W)
  val isNextMask = Vec(16, Bool())

  val meta = UInt()

  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasEntry = new RASEntry
  val hist = new GlobalHistory
  val specCnt = Vec(1, UInt(10.W))
  
  val valids = Vec(16, Bool())
  val brMask = Vec(16, Bool())
  // isJalr, isCall, isRet
  val jmpInfo = ValidUndirectioned(Vec(3, Bool()))
  val jmpOffset = UInt(4.W)
  
  val mispredVec = Vec(16, Bool())
  val cfiIndex = ValidUndirectioned(UInt(4.W))
  val target = UInt(VAddrBits.W)
}

class FtqRead[T <: Data](private val gen: T)(implicit p: Parameters) extends XSBundle {
  val ptr = Output(new FtqPtr)
  val offset = Output(UInt(log2Ceil(16).W))
  val data = Input(gen)
  def apply(ptr: FtqPtr, offset: UInt) = {
    this.ptr := ptr
    this.offset := offset
    this.data
  }
  override def cloneType = (new FtqRead(gen)).asInstanceOf[this.type]
}


class CfiInfoToCtrl(implicit p: Parameters) extends Bundle {
  val br_mask = Vec(16, Bool())
  val hist = new GlobalHistory
  override def cloneType = (new CfiInfoToCtrl).asInstanceOf[this.type]
}

class FtqToBpuIO(implicit p: Parameters) extends XSBundle {
  val redirect = Valid(new BranchPredictionRedirect)
  val update = Valid(new BranchPredictionUpdate)
}

class FtqToIfuIO(implicit p: Parameters) extends XSBundle {
  val req = Decoupled(new FetchRequestBundle)
  val redirect = Valid(new Redirect)
}

class FtqToCtrlIO(implicit p: Parameters) extends XSBundle {
  val pc_reads = Vec(1 + 6 + 1 + 1, Flipped(new FtqRead(UInt(VAddrBits.W))))
  val target_read = Flipped(new FtqRead(UInt(VAddrBits.W)))
  val cfi_reads = Vec(6, Flipped(new FtqRead(new CfiInfoToCtrl)))
  def getJumpPcRead = pc_reads.head
  def getRedirectPcRead = VecInit(pc_reads.tail.dropRight(2))
  def getMemPredPcRead = pc_reads.dropRight(1).last
  def getRoqFlushPcRead = pc_reads.last
}

class Ftq(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val fromBpu = Flipped(new BpuToFtqIO)
    val fromIfu = Flipped(new IfuToFtqIO)
    val fromBackend = Flipped(new CtrlToFtqIO)
    
    val toBpu = new FtqToBpuIO
    val toIfu = new FtqToIfuIO
    val toBackend = new FtqToCtrlIO

    val bpuInfo = new Bundle {
      val bpRight = Output(UInt(XLEN.W))
      val bpWrong = Output(UInt(XLEN.W))
    }
  })
  io.bpuInfo := DontCare

  val stage2Flush = io.fromBackend.stage2Redirect.valid || io.fromBackend.roqFlush.valid
  val backendFlush = stage2Flush || RegNext(stage2Flush)
  val ifuFlush = io.fromIfu.pdWb.valid && io.fromIfu.pdWb.bits.misOffset.valid

  val bpuPtr, ifuPtr, commPtr = RegInit(FtqPtr(false.B, 0.U))
  val validEntries = distanceBetween(bpuPtr, commPtr)

  // **********************************************************************
  // **************************** enq from bpu ****************************
  // **********************************************************************
  io.fromBpu.resp.ready := validEntries < FtqSize.U
  val enq_fire = io.fromBpu.resp.fire() && !backendFlush && !ifuFlush

  val ftq_pc_mem = Module(new SyncDataModuleTemplate(new Ftq_RF_Components, FtqSize, 10, 1))
  ftq_pc_mem.io.wen(0) := enq_fire
  ftq_pc_mem.io.waddr(0) := bpuPtr.value
  ftq_pc_mem.io.wdata(0).startAddr := io.fromBpu.resp.bits.pc
  ftq_pc_mem.io.wdata(0).fallThruAddr := io.fromBpu.resp.bits.ftb_entry.pftAddr
  ftq_pc_mem.io.wdata(0).isNextMask := VecInit((0 until 16).map(i => (io.fromBpu.resp.bits.pc(4, 1) +& i.U)(4).asBool()))

  val ftq_hist_mem = Module(new SyncDataModuleTemplate(new GlobalHistory, FtqSize, 7, 1))
  ftq_hist_mem.io.wen(0) := enq_fire
  ftq_hist_mem.io.waddr(0) := bpuPtr.value
  ftq_hist_mem.io.wdata(0) := io.fromBpu.resp.bits.ghist
  
  val ftq_redirect_sram = Module(new FtqNRSRAM(new Ftq_Redirect_SRAMEntry, 2))
  ftq_redirect_sram.io.wen := enq_fire
  ftq_redirect_sram.io.waddr := bpuPtr.value
  ftq_redirect_sram.io.wdata.rasSp := io.fromBpu.resp.bits.rasSp
  ftq_redirect_sram.io.wdata.rasEntry := io.fromBpu.resp.bits.rasTop
  ftq_redirect_sram.io.wdata.specCnt := io.fromBpu.resp.bits.specCnt

  val pred_target_sram = Module(new FtqNRSRAM(UInt(VAddrBits.W), 2))
  pred_target_sram.io.wen := enq_fire
  pred_target_sram.io.waddr := bpuPtr.value
  pred_target_sram.io.wdata := io.fromBpu.resp.bits.preds.target

  val ftq_meta_1r_sram = Module(new FtqNRSRAM(new Ftq_1R_SRAMEntry, 1))
  ftq_meta_1r_sram.io.wen := enq_fire
  ftq_meta_1r_sram.io.waddr := bpuPtr.value
  ftq_meta_1r_sram.io.wdata.meta := io.fromBpu.resp.bits.meta

  
  // multi-write
  val update_target = Reg(Vec(FtqSize, UInt(VAddrBits.W)))
  val cfiIndex_vec = Reg(Vec(FtqSize, ValidUndirectioned(UInt(log2Up(PredictWidth).W))))
  val mispredict_vec = Reg(Vec(FtqSize, Vec(PredictWidth, Bool())))
  
  val s_invalid :: s_valid :: s_commited :: Nil = Enum(3)
  val commitStateQueue = RegInit(VecInit(Seq.fill(FtqSize) {
    VecInit(Seq.fill(PredictWidth)(s_invalid))
  }))
  
  val f_to_send :: f_sent :: f_wb :: Nil = Enum(3)
  val entry_fetch_status = RegInit(VecInit(Seq.fill(FtqSize)(f_sent)))
  
  when (enq_fire) {
    val enqIdx = bpuPtr.value
    val preds = io.fromBpu.resp.bits.preds
    val ftb_entry = io.fromBpu.resp.bits.ftb_entry
    val real_taken_mask = preds.taken_mask.asUInt & Cat(ftb_entry.jmpValid, ftb_entry.brValids.asUInt)
    val jmp_offset = Cat(1.U(1.W), ftb_entry.pftAddr(4,1)) - Mux(ftb_entry.last_is_rvc, 2.U, 4.U) - io.fromBpu.resp.bits.pc(4,1)
    val jmp_target = io.fromBpu.resp.bits.ftb_entry.jmpTarget
    val offset_vec = VecInit(ftb_entry.brOffset :+ jmp_offset)
    val target_vec = VecInit(ftb_entry.brTargets :+ jmp_target)
    val enq_cfiIndex = WireInit(0.U.asTypeOf(new ValidUndirectioned(UInt(4.W))))
    entry_fetch_status(enqIdx) := f_to_send
    enq_cfiIndex.valid := real_taken_mask.orR
    enq_cfiIndex.bits := ParallelPriorityMux(real_taken_mask, offset_vec)
    cfiIndex_vec(enqIdx) := enq_cfiIndex
    mispredict_vec(enqIdx) := WireInit(VecInit(Seq.fill(16)(false.B)))
    update_target(enqIdx) := ParallelPriorityMux(real_taken_mask, target_vec)
  }
  
  bpuPtr := bpuPtr + enq_fire
  
  // *********************************************************************
  // **************************** wb from ifu ****************************
  // *********************************************************************
  val pdWb = io.fromIfu.pdWb
  val ifu_wb_valid = pdWb.valid
  // 0: commit, 1: cfiRead, 2-9: ftqRead, 10: ifuRead
  val ftq_pd_mem = Module(new SyncDataModuleTemplate(new Ftq_pd_Entry, FtqSize, 7, 1))
  ftq_pd_mem.io.wen(0) := ifu_wb_valid
  ftq_pd_mem.io.waddr(0) := pdWb.bits.ftqIdx.value
  val pds = pdWb.bits.pd
  ftq_pd_mem.io.wdata(0).brMask := VecInit(pds.map(pd => pd.isBr && pd.valid))
  ftq_pd_mem.io.wdata(0).jmpInfo.valid := VecInit(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid)).asUInt.orR
  ftq_pd_mem.io.wdata(0).jmpInfo.bits := ParallelPriorityMux(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid),
                                                          pds.map(pd => VecInit(pd.isJalr, pd.isCall, pd.isRet)))
  ftq_pd_mem.io.wdata(0).jmpOffset := ParallelPriorityEncoder(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid))
  
  when (ifu_wb_valid) {
    val ifuWbIdx = pdWb.bits.ftqIdx.value
    commitStateQueue(ifuWbIdx) := VecInit(pds.map(_.valid))
    entry_fetch_status(ifuWbIdx) := f_wb
  }

  // ****************************************************************
  // **************************** to ifu ****************************
  // ****************************************************************
  val ifu_req_buf = RegInit(0.U.asTypeOf(ValidUndirectioned(new FetchRequestBundle)))
  val to_buf_valid = entry_fetch_status(ifuPtr.value) === f_to_send && ifuPtr =/= bpuPtr
  // ftqIdx and ftqOffset enq
  val to_buf_fire = !backendFlush && !ifuFlush && to_buf_valid && (!ifu_req_buf.valid || io.toIfu.req.ready)
  when (to_buf_fire) {
    entry_fetch_status(ifuPtr.value) := f_sent
  }
  ifuPtr := ifuPtr + to_buf_fire
  
  when (backendFlush || ifuFlush) {
    ifu_req_buf.valid := false.B
  }.elsewhen (to_buf_fire) {
    ifu_req_buf.valid := true.B
  }.elsewhen (io.toIfu.req.fire()) {
    ifu_req_buf.valid := false.B
  }
  
  // read pc and target
  ftq_pc_mem.io.raddr(9) := ifuPtr.value
  pred_target_sram.io.raddr(0) := ifuPtr.value
  pred_target_sram.io.ren(0) := to_buf_fire
  
  when (to_buf_fire) {
    ifu_req_buf.bits.ftqIdx := ifuPtr
    ifu_req_buf.bits.ftqOffset := cfiIndex_vec(ifuPtr.value)
  }
  when (RegNext(to_buf_fire)) {
    ifu_req_buf.bits.startAddr := ftq_pc_mem.io.rdata(9).startAddr
    ifu_req_buf.bits.fallThruAddr := ftq_pc_mem.io.rdata(9).fallThruAddr
    ifu_req_buf.bits.target := pred_target_sram.io.rdata(0)
  }
  
  val last_cycle_to_buf_fire = RegNext(to_buf_fire)
  io.toIfu.req.valid := ifu_req_buf.valid
  io.toIfu.req.bits.ftqIdx := ifu_req_buf.bits.ftqIdx
  io.toIfu.req.bits.ftqOffset := ifu_req_buf.bits.ftqOffset
  io.toIfu.req.bits.startAddr := Mux(last_cycle_to_buf_fire,
                                     ftq_pc_mem.io.rdata(9).startAddr,
                                     ifu_req_buf.bits.startAddr)
  io.toIfu.req.bits.fallThruAddr := Mux(last_cycle_to_buf_fire,
                                        ftq_pc_mem.io.rdata(9).fallThruAddr,
                                        ifu_req_buf.bits.fallThruAddr)
  io.toIfu.req.bits.target := Mux(last_cycle_to_buf_fire,
                                  pred_target_sram.io.rdata(0),
                                  ifu_req_buf.bits.target)



  // pc reads
  for ((req, i) <- io.toBackend.pc_reads.zipWithIndex) {
    ftq_pc_mem.io.raddr(i) := req.ptr.value
    req.data := ftq_pc_mem.io.rdata(i).getPc(RegNext(req.offset))
  }
  // target read
  pred_target_sram.io.raddr(1) := io.toBackend.target_read.ptr.value
  pred_target_sram.io.ren(1) := true.B
  io.toBackend.target_read.data := pred_target_sram.io.rdata(1)
  // cfi read
  for ((req, i) <- io.toBackend.cfi_reads.zipWithIndex) {
    ftq_pd_mem.io.raddr(i) := req.ptr.value
    ftq_hist_mem.io.raddr(i) := req.ptr.value
    req.data.br_mask := ftq_pd_mem.io.rdata(i).brMask
    req.data.hist := ftq_hist_mem.io.rdata(i)
  }
  // redirect read cfiInfo, couples to redirectGen s2
  ftq_redirect_sram.io.ren(0) := io.fromBackend.stage2Redirect.valid
  ftq_redirect_sram.io.raddr(0) := io.fromBackend.stage2Redirect.bits.ftqIdx.value
  val stage3CfiInfo = ftq_redirect_sram.io.rdata(0)
  val fromBackendRedirect = WireInit(io.fromBackend.stage3Redirect)
  fromBackendRedirect.bits.cfiUpdate.rasSp := stage3CfiInfo.rasSp
  fromBackendRedirect.bits.cfiUpdate.rasEntry := stage3CfiInfo.rasEntry
  fromBackendRedirect.bits.cfiUpdate.specCnt := stage3CfiInfo.specCnt

  // ***************************************************************************
  // **************************** redirect from ifu ****************************
  // ***************************************************************************
  val fromIfuRedirect = WireInit(0.U.asTypeOf(Valid(new Redirect)))
  fromIfuRedirect.valid := pdWb.valid && pdWb.bits.misOffset.valid && !backendFlush
  fromIfuRedirect.bits.ftqIdx := pdWb.bits.ftqIdx
  fromIfuRedirect.bits.ftqOffset := pdWb.bits.misOffset.bits
  fromIfuRedirect.bits.level := RedirectLevel.flushAfter // TODO: is this right?

  val ifuRedirectCfiUpdate = fromIfuRedirect.bits.cfiUpdate
  ifuRedirectCfiUpdate.pc := pdWb.bits.pc(pdWb.bits.misOffset.bits)
  ifuRedirectCfiUpdate.pd := pdWb.bits.pd(pdWb.bits.misOffset.bits)
  ifuRedirectCfiUpdate.predTaken := cfiIndex_vec(pdWb.bits.ftqIdx.value).valid
  ifuRedirectCfiUpdate.target := Mux(pdWb.bits.cfiOffset.valid, pdWb.bits.target, pdWb.bits.pc(0)+32.U)
  ifuRedirectCfiUpdate.taken := pdWb.bits.cfiOffset.valid
  ifuRedirectCfiUpdate.isMisPred := pdWb.bits.misOffset.valid

  val ifuRedirectReg = RegNext(fromIfuRedirect, init=0.U.asTypeOf(Valid(new Redirect)))
  val ifuRedirectToBpu = WireInit(ifuRedirectReg)

  ftq_redirect_sram.io.ren(1) := fromIfuRedirect.valid
  ftq_redirect_sram.io.raddr(1) := fromIfuRedirect.bits.ftqIdx.value
  
  // TODO: assign redirect rdata to ifuRedirectToBpu
  
  ftq_hist_mem.io.raddr(6) := fromIfuRedirect.bits.ftqIdx.value
  ftq_pd_mem.io.raddr(6) := fromIfuRedirect.bits.ftqIdx.value

  // *********************************************************************                                  
  // **************************** wb from exu ****************************
  // *********************************************************************

  // when redirect cfi offset < current offset, update all cfi info
  val cfiWbEn_vec = VecInit(Seq.fill(FtqSize)(false.B))
  // when redirect cfi offset == current offset (and only), update cfi valid bit
  val cfiValidWbEn_vec = VecInit(Seq.fill(FtqSize)(false.B))

  val cfiIndexValidWb_vec = Wire(Vec(FtqSize, Bool()))
  val cfiIndexBitsWb_vec = Wire(Vec(FtqSize, UInt(log2Up(PredictWidth).W)))

  val nWbPorts = io.fromBackend.exuWriteback.size
  def getRedirect(exuOut: Valid[ExuOutput]): Valid[Redirect] = {
    val redirect = Wire(Valid(new Redirect))
    redirect.valid := exuOut.valid && exuOut.bits.redirect.cfiUpdate.isMisPred
    redirect.bits := exuOut.bits.redirect
    redirect
  }
  def extractRedirectInfo(wb: Valid[Redirect]) = {
    val ftqIdx = wb.bits.ftqIdx.value
    val ftqOffset = wb.bits.ftqOffset
    val taken = wb.bits.cfiUpdate.taken
    val mispred = wb.bits.cfiUpdate.isMisPred
    (wb.valid, ftqIdx, ftqOffset, taken, mispred)
  }
  val redirect_vec = VecInit(io.fromBackend.exuWriteback.map(getRedirect) :+ fromIfuRedirect)
  val redirect_infos = redirect_vec.map(extractRedirectInfo)
  val wbFtqOffset_vec = VecInit(redirect_infos.map(i=>i._3))
  // FtqSize * onehot
  val wbPortSel_vec = Wire(Vec(FtqSize, Vec(nWbPorts + 1, Bool())))
  // in order to handle situation in which multiple cfi taken writebacks target the same ftqEntry
  for (i <- 0 until FtqSize) {
    val needToUpdateThisEntry =
      //                                 valid   taken    ftqIdx         ftqOffset          ftqIdx
      VecInit(redirect_infos.map(r => r._1 && r._4 && r._2 === i.U && r._3 < cfiIndex_vec(r._2).bits))
      
    val updateCfiValidMask =
      //                                 valid   taken          ftqOffset             ftqIdx
      VecInit(redirect_infos.map(r => r._1 && r._2 === i.U && r._3 === cfiIndex_vec(r._2).bits))
      
    cfiWbEn_vec(i) := needToUpdateThisEntry.asUInt().orR()
    cfiValidWbEn_vec(i) := updateCfiValidMask.asUInt().orR()

    // TODO: distinguish exuWb and ifuWb
    for (n <- 0 until nWbPorts+1) {
      val hasFormerWriteBack = VecInit(
        for (another <- 0 until nWbPorts+1 if another != n) yield {
          needToUpdateThisEntry(another) && wbFtqOffset_vec(another) < wbFtqOffset_vec(n)
        }
      ).asUInt.orR
      wbPortSel_vec(i)(n) := needToUpdateThisEntry(n) && !hasFormerWriteBack || !needToUpdateThisEntry.asUInt().orR() && updateCfiValidMask(n)
    }
  
    XSError(PopCount(wbPortSel_vec(i)) > 1.U, p"multiple wb ports are selected to update cfiIndex_vec($i)\n")

    cfiIndexValidWb_vec(i) := cfiWbEn_vec(i) || cfiValidWbEn_vec(i) && Mux1H(wbPortSel_vec(i) zip redirect_infos.map(i=>i._4))
    cfiIndexBitsWb_vec(i) := Mux1H(wbPortSel_vec(i) zip wbFtqOffset_vec)
  }

  for (i <- 0 until FtqSize) {
    when (cfiWbEn_vec(i) || cfiValidWbEn_vec(i)) {
      cfiIndex_vec(i).valid := cfiIndexValidWb_vec(i)
    }
    when (cfiWbEn_vec(i)) {
      cfiIndex_vec(i).bits := cfiIndexBitsWb_vec(i)
    }
  }

  for (r <- redirect_infos) {
    //             idx   offset   mispred
    mispredict_vec(r._2)(r._3) := r._5
  }

  // fix mispredict entry
  val lastIsMispredict = RegNext(
    io.fromBackend.stage2Redirect.valid && io.fromBackend.stage2Redirect.bits.level === RedirectLevel.flushAfter, init = false.B
  )
  when(io.fromBackend.stage3Redirect.valid && lastIsMispredict) {
    update_target(io.fromBackend.stage3Redirect.bits.ftqIdx.value) := io.fromBackend.stage3Redirect.bits.cfiUpdate.target
  }.elsewhen (pdWb.valid && pdWb.bits.misOffset.valid && !backendFlush) {
    update_target(pdWb.bits.ftqIdx.value) := pdWb.bits.target
  }

  // ***********************************************************************************
  // **************************** flush ptr and state queue ****************************
  // ***********************************************************************************
  val roqFlush = io.fromBackend.roqFlush
  val stage2Redirect = io.fromBackend.stage2Redirect
  class RedirectInfo extends Bundle {
    val valid = Bool()
    val ftqIdx = new FtqPtr
    val ftqOffset = UInt(4.W)
    val notMispredict = Bool()
    def apply(valid: Bool, idx: FtqPtr, offset: UInt, notMispredict: Bool) = {
      this.valid := valid
      this.ftqIdx := idx
      this.ftqOffset := offset
      this.notMispredict := notMispredict
      this
    }
  }
  val redirectVec = Wire(Vec(3, new RedirectInfo))
  redirectVec(0).apply(
    roqFlush.valid,
    roqFlush.bits.ftqIdx,
    roqFlush.bits.ftqOffset,
    true.B)
  redirectVec(1).apply(
    stage2Redirect.valid,
    stage2Redirect.bits.ftqIdx,
    stage2Redirect.bits.ftqOffset,
    RedirectLevel.flushItself(stage2Redirect.bits.level))
  redirectVec(2).apply(
    fromIfuRedirect.valid,
    fromIfuRedirect.bits.ftqIdx,
    fromIfuRedirect.bits.ftqOffset,
    false.B)
  when(redirectVec.map(r => r.valid).reduce(_||_)){
    val idx = PriorityMux(redirectVec.map(r => (r.valid -> r.ftqIdx)))
    val next = idx + 1.U
    bpuPtr := next
    ifuPtr := next
    val offset = PriorityMux(redirectVec.map(r => (r.valid -> r.ftqOffset)))
    val notMisPredict = PriorityMux(redirectVec.map(r => (r.valid -> r.notMispredict)))
    commitStateQueue(idx.value).zipWithIndex.foreach({ case (s, i) =>
      when(i.U > offset || (notMisPredict && i.U === offset)){
        s := s_invalid
      }
    })
    when(next.value =/= commPtr.value){ // if next.value === commPtr.value, ftq is full
      commitStateQueue(next.value).foreach(_ := s_invalid)
    }
  }

  io.toIfu.redirect := io.fromBackend.stage3Redirect
  
  // commit
  for (c <- io.fromBackend.roq_commits) {
    when(c.valid) {
      commitStateQueue(c.bits.ftqIdx.value)(c.bits.ftqOffset) := s_commited
    }
  }

  val canDeq = entry_fetch_status(commPtr.value) === f_wb &&
    Cat(commitStateQueue(commPtr.value).map(s => {
      s === s_invalid || s === s_commited
    })).andR()
  when(canDeq && commPtr =/= bpuPtr) {
    commPtr := commPtr + 1.U
  }
  
  // ****************************************************************
  // **************************** to bpu ****************************
  // ****************************************************************
  io.toBpu.redirect <> Mux(fromBackendRedirect.valid, fromBackendRedirect, ifuRedirectToBpu)
  val commit_valids = VecInit(commitStateQueue(commPtr.value).map(s => s === s_commited))
  io.toBpu.update := DontCare
  ftq_meta_1r_sram.io.ren(0) := false.B
  ftq_meta_1r_sram.io.raddr(0) := 0.U


  // --------------------------- Debug --------------------------------
  XSDebug(enq_fire, io.fromBpu.resp.bits.toPrintable)
  XSDebug(io.toIfu.req.fire, p"fire to ifu " + io.toIfu.req.bits.toPrintable)
  XSDebug(canDeq, p"deq! [ptr] $commPtr\n")
  XSDebug(true.B, p"[bpuPtr] $bpuPtr, [ifuPtr] $ifuPtr, [commPtr] $commPtr\n")
  XSDebug(true.B, p"[in] v:${io.fromBpu.resp.valid} r:${io.fromBpu.resp.ready} " +
    p"[out] v:${io.toIfu.req.valid} r:${io.toIfu.req.ready}\n")
}

trait HasPerfDebug { this: Ftq =>
  val enq = io.fromBpu.resp
  val perf_redirect = io.fromBackend.stage2Redirect
  XSPerfAccumulate("entry", validEntries)
  XSPerfAccumulate("stall", enq.valid && !enq.ready)
  XSPerfAccumulate("mispredictRedirect", perf_redirect.valid && RedirectLevel.flushAfter === perf_redirect.bits.level)
  XSPerfAccumulate("replayRedirect", perf_redirect.valid && RedirectLevel.flushItself(perf_redirect.bits.level))

  // val predRights = (0 until PredictWidth).map{i => !commitEntry.mispred(i) && !commitEntry.pd(i).notCFI && commitEntry.valids(i)}
  // val predWrongs = (0 until PredictWidth).map{i => commitEntry.mispred(i) && !commitEntry.pd(i).notCFI && commitEntry.valids(i)}

  // // Branch Predictor Perf counters
  // if (!env.FPGAPlatform && env.EnablePerfDebug) {
  //   val fires = commitEntry.valids.zip(commitEntry.pd).map{case (valid, pd) => valid && !pd.notCFI}
  //   val isBTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isBr}
  //   val isJTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isJal}
  //   val isITypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isJalr}
  //   val isCTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isCall}
  //   val isRTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isRet}

  //   val mbpInstrs = fires
  //   val mbpRights = predRights
  //   val mbpWrongs = predWrongs
  //   val mbpBRights = Cat(predRights) & Cat(isBTypes)
  //   val mbpBWrongs = Cat(predWrongs) & Cat(isBTypes)
  //   val mbpJRights = Cat(predRights) & Cat(isJTypes)
  //   val mbpJWrongs = Cat(predWrongs) & Cat(isJTypes)
  //   val mbpIRights = Cat(predRights) & Cat(isITypes)
  //   val mbpIWrongs = Cat(predWrongs) & Cat(isITypes)
  //   val mbpCRights = Cat(predRights) & Cat(isCTypes)
  //   val mbpCWrongs = Cat(predWrongs) & Cat(isCTypes)
  //   val mbpRRights = Cat(predRights) & Cat(isRTypes)
  //   val mbpRWrongs = Cat(predWrongs) & Cat(isRTypes)

  //   def ubtbCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isBr,
  //         isWrong ^ Mux(ans.hit.asBool,
  //           Mux(ans.taken.asBool, taken && ans.target === commitEntry.target,
  //           !taken),
  //         !taken),
  //       false.B)
  //     }
  //   }

  //   def btbCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isBr,
  //         isWrong ^ Mux(ans.hit.asBool,
  //           Mux(ans.taken.asBool, taken && ans.target === commitEntry.target,
  //           !taken),
  //         !taken),
  //       false.B)
  //     }
  //   }

  //   def tageCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isBr,
  //         isWrong ^ (ans.taken.asBool === taken),
  //       false.B)
  //     }
  //   }

  //   def loopCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && (pd.isBr) && ans.hit.asBool, 
  //         isWrong ^ (!taken),
  //           false.B)
  //     }
  //   }

  //   def rasCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isRet.asBool /*&& taken*/ && ans.hit.asBool,
  //         isWrong ^ (ans.target === commitEntry.target),
  //           false.B)
  //     }
  //   }

  //   val ubtbRights = ubtbCheck(commitEntry, commitEntry.metas.map(_.ubtbAns), false.B)
  //   val ubtbWrongs = ubtbCheck(commitEntry, commitEntry.metas.map(_.ubtbAns), true.B)
  //   // btb and ubtb pred jal and jalr as well
  //   val btbRights = btbCheck(commitEntry, commitEntry.metas.map(_.btbAns), false.B)
  //   val btbWrongs = btbCheck(commitEntry, commitEntry.metas.map(_.btbAns), true.B)
  //   val tageRights = tageCheck(commitEntry, commitEntry.metas.map(_.tageAns), false.B)
  //   val tageWrongs = tageCheck(commitEntry, commitEntry.metas.map(_.tageAns), true.B)

  //   val loopRights = loopCheck(commitEntry, commitEntry.metas.map(_.loopAns), false.B)
  //   val loopWrongs = loopCheck(commitEntry, commitEntry.metas.map(_.loopAns), true.B)

  //   val rasRights = rasCheck(commitEntry, commitEntry.metas.map(_.rasAns), false.B)
  //   val rasWrongs = rasCheck(commitEntry, commitEntry.metas.map(_.rasAns), true.B)

  //   val perfCountsMap = Map(
  //     "BpInstr" -> PopCount(mbpInstrs),
  //     "BpBInstr" -> PopCount(commitEntry.valids.zip(commitEntry.pd).map{case (valid, pd) => valid && pd.isBr}),
  //     "BpRight"  -> PopCount(mbpRights),
  //     "BpWrong"  -> PopCount(mbpWrongs),
  //     "BpBRight" -> PopCount(mbpBRights),
  //     "BpBWrong" -> PopCount(mbpBWrongs),
  //     "BpJRight" -> PopCount(mbpJRights),
  //     "BpJWrong" -> PopCount(mbpJWrongs),
  //     "BpIRight" -> PopCount(mbpIRights),
  //     "BpIWrong" -> PopCount(mbpIWrongs),
  //     "BpCRight" -> PopCount(mbpCRights),
  //     "BpCWrong" -> PopCount(mbpCWrongs),
  //     "BpRRight" -> PopCount(mbpRRights),
  //     "BpRWrong" -> PopCount(mbpRWrongs),

  //     "ubtbRight" -> PopCount(ubtbRights),
  //     "ubtbWrong" -> PopCount(ubtbWrongs),
  //     "btbRight" -> PopCount(btbRights),
  //     "btbWrong" -> PopCount(btbWrongs),
  //     "tageRight" -> PopCount(tageRights),
  //     "tageWrong" -> PopCount(tageWrongs),

  //     "rasRight"  -> PopCount(rasRights),
  //     "rasWrong"  -> PopCount(rasWrongs),
  //     "loopRight" -> PopCount(loopRights),
  //     "loopWrong" -> PopCount(loopWrongs),
  //   )

  //   for((key, value) <- perfCountsMap) {
  //     XSPerfAccumulate(key, value)
  //   }
  // }

  // XSDebug(io.commit_ftqEntry.valid, p"ftq commit: ${io.commit_ftqEntry.bits}")
  XSDebug(enq.fire(), p"ftq enq: ${enq.bits}")

  // io.bpuInfo.bpRight := PopCount(predRights)
  // io.bpuInfo.bpWrong := PopCount(predWrongs)

}

