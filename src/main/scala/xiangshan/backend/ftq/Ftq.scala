package xiangshan.backend.ftq

import chisel3._
import chisel3.util._
import utils.{AsyncDataModuleTemplate, CircularQueuePtr, DataModuleTemplate, HasCircularQueuePtrHelper, SRAMTemplate, SyncDataModuleTemplate, XSDebug, XSPerf}
import xiangshan._
import xiangshan.frontend.{GlobalHistory, RASEntry}
import xiangshan.frontend.PreDecodeInfoForDebug

class FtqPtr extends CircularQueuePtr(FtqPtr.FtqSize) with HasCircularQueuePtrHelper

object FtqPtr extends HasXSParameter {
  def apply(f: Bool, v: UInt): FtqPtr = {
    val ptr = Wire(new FtqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

object GetPcByFtq extends HasXSParameter {
  def apply(ftqPC: UInt, ftqOffset: UInt, hasLastPrev: Bool, lastPacketPC: UInt) = {
    assert(ftqPC.getWidth == VAddrBits)
    assert(lastPacketPC.getWidth == VAddrBits)
    assert(ftqOffset.getWidth == log2Up(PredictWidth))
    val idxBits = ftqPC.head(VAddrBits - ftqOffset.getWidth - instOffsetBits)
    val lastIdxBits = lastPacketPC.head(VAddrBits - ftqOffset.getWidth - instOffsetBits)
    val selLastPacket = hasLastPrev && (ftqOffset === 0.U)
    val packetIdx = Mux(selLastPacket, lastIdxBits, idxBits)
    Cat(
      packetIdx, // packet pc
      Mux(selLastPacket, Fill(ftqOffset.getWidth, 1.U(1.W)), ftqOffset),
      0.U(instOffsetBits.W)
    )
  }
}


class FtqNRSRAM[T <: Data](gen: T, numRead: Int) extends XSModule {

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

class Ftq_4R_SRAMEntry extends XSBundle {
  val ftqPC = UInt(VAddrBits.W)
  val lastPacketPC = ValidUndirectioned(UInt(VAddrBits.W))
}

// redirect and commit need read these infos
class Ftq_2R_SRAMEntry extends XSBundle {
  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasEntry = new RASEntry
  val hist = new GlobalHistory
  val predHist = new GlobalHistory
  val specCnt = Vec(PredictWidth, UInt(10.W))
  val br_mask = Vec(PredictWidth, Bool())
}

class Ftq_1R_Commit_SRAMEntry extends XSBundle {
  val metas = Vec(PredictWidth, new BpuMeta)
  val rvc_mask = Vec(PredictWidth, Bool())

  val pd = Vec(PredictWidth, new PreDecodeInfoForDebug(!env.FPGAPlatform))
}

class FtqRead extends Bundle {
  val ptr = Output(new FtqPtr)
  val entry = Input(new FtqEntry)
}

class Ftq extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val enq = Flipped(DecoupledIO(new FtqEntry))
    val leftOne = Output(Bool())
    val enqPtr = Output(new FtqPtr)
    // roq commit, read out fectch packet and deq
    val roq_commits = Vec(CommitWidth, Flipped(ValidIO(new RoqCommitInfo)))
    val commit_ftqEntry = ValidIO(new FtqEntry)
    // redirect, reset enq ptr
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val flushIdx = Input(new FtqPtr)
    val flushOffset = Input(UInt(log2Up(PredictWidth).W))
    // update mispredict target
    val frontendRedirect = Flipped(ValidIO(new Redirect))
    // exu write back, update info
    val exuWriteback = Vec(exuParameters.JmpCnt + exuParameters.AluCnt, Flipped(ValidIO(new ExuOutput)))
    // pc read reqs (0: jump/auipc 1~6: mispredict/load replay 7: exceptions)
    val ftqRead = Vec(1 + 6 + 1, Flipped(new FtqRead))
    val cfiRead = Flipped(new FtqRead)
  })

  val headPtr, tailPtr = RegInit(FtqPtr(false.B, 0.U))

  val validEntries = distanceBetween(tailPtr, headPtr)

  // enq
  io.leftOne := validEntries === (FtqSize - 1).U
  io.enq.ready := validEntries < FtqSize.U
  io.enqPtr := tailPtr

  val stage2Flush = io.redirect.valid || io.flush
  val stage3Flush = RegNext(stage2Flush)

  val real_fire = io.enq.fire() && !stage2Flush && !stage3Flush

  val ftq_pc_mem = Module(new SyncDataModuleTemplate(new Ftq_4R_SRAMEntry, FtqSize, 9, 1))
  ftq_pc_mem.io.wen(0) := real_fire
  ftq_pc_mem.io.waddr(0) := tailPtr.value
  ftq_pc_mem.io.wdata(0).ftqPC := io.enq.bits.ftqPC
  ftq_pc_mem.io.wdata(0).lastPacketPC := io.enq.bits.lastPacketPC
  val ftq_2r_sram = Module(new FtqNRSRAM(new Ftq_2R_SRAMEntry, 2))
  ftq_2r_sram.io.wen := real_fire
  ftq_2r_sram.io.waddr := tailPtr.value
  ftq_2r_sram.io.wdata.rasSp := io.enq.bits.rasSp
  ftq_2r_sram.io.wdata.rasEntry := io.enq.bits.rasTop
  ftq_2r_sram.io.wdata.hist := io.enq.bits.hist
  ftq_2r_sram.io.wdata.predHist := io.enq.bits.predHist
  ftq_2r_sram.io.wdata.specCnt := io.enq.bits.specCnt
  ftq_2r_sram.io.wdata.br_mask := io.enq.bits.br_mask
  val pred_target_sram = Module(new FtqNRSRAM(UInt(VAddrBits.W), 1))
  pred_target_sram.io.wen := real_fire
  pred_target_sram.io.waddr := tailPtr.value
  pred_target_sram.io.wdata := io.enq.bits.target
  val ftq_1r_sram = Module(new FtqNRSRAM(new Ftq_1R_Commit_SRAMEntry, 1))
  ftq_1r_sram.io.wen := real_fire
  ftq_1r_sram.io.waddr := tailPtr.value
  ftq_1r_sram.io.wdata.metas := io.enq.bits.metas
  ftq_1r_sram.io.wdata.rvc_mask := io.enq.bits.rvc_mask
  ftq_1r_sram.io.wdata.pd := io.enq.bits.pd

  // multi-write
  val update_target = Reg(Vec(FtqSize, UInt(VAddrBits.W)))
  val cfiIndex_vec = Reg(Vec(FtqSize, ValidUndirectioned(UInt(log2Up(PredictWidth).W))))
  val cfiIsCall, cfiIsRet, cfiIsRVC = Reg(Vec(FtqSize, Bool()))
  val mispredict_vec = Reg(Vec(FtqSize, Vec(PredictWidth, Bool())))

  val s_invalid :: s_valid :: s_commited :: Nil = Enum(3)
  val commitStateQueue = RegInit(VecInit(Seq.fill(FtqSize) {
    VecInit(Seq.fill(PredictWidth)(s_invalid))
  }))

  when(real_fire) {
    val enqIdx = tailPtr.value
    commitStateQueue(enqIdx) := VecInit(io.enq.bits.valids.map(v => Mux(v, s_valid, s_invalid)))
    cfiIndex_vec(enqIdx) := io.enq.bits.cfiIndex
    cfiIsCall(enqIdx) := io.enq.bits.cfiIsCall
    cfiIsRet(enqIdx) := io.enq.bits.cfiIsRet
    cfiIsRVC(enqIdx) := io.enq.bits.cfiIsRVC
    mispredict_vec(enqIdx) := WireInit(VecInit(Seq.fill(PredictWidth)(false.B)))
    update_target(enqIdx) := io.enq.bits.target
  }

  tailPtr := tailPtr + real_fire

  // exu write back, update some info
  for ((wb, i) <- io.exuWriteback.zipWithIndex) {
    val wbIdx = wb.bits.redirect.ftqIdx.value
    val offset = wb.bits.redirect.ftqOffset
    val cfiUpdate = wb.bits.redirect.cfiUpdate
    when(wb.valid && wb.bits.redirectValid) {
      mispredict_vec(wbIdx)(offset) := cfiUpdate.isMisPred
      when(cfiUpdate.taken && offset < cfiIndex_vec(wbIdx).bits) {
        cfiIndex_vec(wbIdx).valid := true.B
        cfiIndex_vec(wbIdx).bits := offset
        cfiIsCall(wbIdx) := wb.bits.uop.cf.pd.isCall
        cfiIsRet(wbIdx) := wb.bits.uop.cf.pd.isRet
        cfiIsRVC(wbIdx) := wb.bits.uop.cf.pd.isRVC
      }
      when (offset === cfiIndex_vec(wbIdx).bits) {
        cfiIndex_vec(wbIdx).valid := cfiUpdate.taken
      }
    }
  }

  // fix mispredict entry
  val lastIsMispredict = RegNext(
    io.redirect.valid && io.redirect.bits.level === RedirectLevel.flushAfter, init = false.B
  )
  when(io.frontendRedirect.valid && lastIsMispredict) {
    update_target(io.frontendRedirect.bits.ftqIdx.value) := io.frontendRedirect.bits.cfiUpdate.target
  }

  // commit
  for (c <- io.roq_commits) {
    when(c.valid) {
      commitStateQueue(c.bits.ftqIdx.value)(c.bits.ftqOffset) := s_commited
    }
  }

  val headClear = Cat(commitStateQueue(headPtr.value).map(s => {
    s === s_invalid || s === s_commited
  })).andR()
  when(headClear && headPtr =/= tailPtr) {
    headPtr := headPtr + 1.U
  }

  ftq_pc_mem.io.raddr(0) := headPtr.value
  ftq_2r_sram.io.raddr(0) := headPtr.value
  ftq_2r_sram.io.ren(0) := true.B
  ftq_1r_sram.io.raddr(0) := headPtr.value
  ftq_1r_sram.io.ren(0) := true.B

  val commitEntry = Wire(new FtqEntry)
  val commit_valids = VecInit(commitStateQueue(headPtr.value).map(s => s === s_commited))
  // set state to invalid next cycle
  commitStateQueue(headPtr.value).zip(commit_valids).foreach({ case (s, v) => when(v) {
    s := s_invalid
  }
  })
  // from 4r sram
  commitEntry.ftqPC := RegNext(ftq_pc_mem.io.rdata(0).ftqPC)
  commitEntry.lastPacketPC := RegNext(ftq_pc_mem.io.rdata(0).lastPacketPC)
  // from 2r sram
  commitEntry.rasSp := RegNext(ftq_2r_sram.io.rdata(0).rasSp)
  commitEntry.rasTop := RegNext(ftq_2r_sram.io.rdata(0).rasEntry)
  commitEntry.hist := RegNext(ftq_2r_sram.io.rdata(0).hist)
  commitEntry.predHist := RegNext(ftq_2r_sram.io.rdata(0).predHist)
  commitEntry.specCnt := RegNext(ftq_2r_sram.io.rdata(0).specCnt)
  commitEntry.br_mask := RegNext(ftq_2r_sram.io.rdata(0).br_mask)
  // from 1r sram
  commitEntry.metas := RegNext(ftq_1r_sram.io.rdata(0).metas)
  commitEntry.rvc_mask := RegNext(ftq_1r_sram.io.rdata(0).rvc_mask)
  commitEntry.pd := RegNext(ftq_1r_sram.io.rdata(0).pd)
  // from regs
  commitEntry.valids := RegNext(RegNext(commit_valids))
  commitEntry.mispred := RegNext(RegNext(mispredict_vec(headPtr.value)))
  commitEntry.cfiIndex := RegNext(RegNext(cfiIndex_vec(headPtr.value)))
  commitEntry.cfiIsCall := RegNext(RegNext(cfiIsCall(headPtr.value)))
  commitEntry.cfiIsRet := RegNext(RegNext(cfiIsRet(headPtr.value)))
  commitEntry.cfiIsRVC := RegNext(RegNext(cfiIsRVC(headPtr.value)))
  commitEntry.target := RegNext(RegNext(update_target(headPtr.value)))

  io.commit_ftqEntry.valid := RegNext(RegNext(Cat(commit_valids).orR())) //TODO: do we need this?
  io.commit_ftqEntry.bits := commitEntry

  // read logic
  for ((req, i) <- io.ftqRead.zipWithIndex) {
    req.entry := DontCare
    ftq_pc_mem.io.raddr(1 + i) := req.ptr.value
    req.entry.ftqPC := ftq_pc_mem.io.rdata(1 + i).ftqPC
    req.entry.lastPacketPC := ftq_pc_mem.io.rdata(1 + i).lastPacketPC
    if(i == 0){ // jump, read npc
      pred_target_sram.io.raddr(0) := req.ptr.value
      pred_target_sram.io.ren(0) := true.B
      req.entry.target := pred_target_sram.io.rdata(0)
    }
  }
  ftq_2r_sram.io.raddr(1) := io.cfiRead.ptr.value
  ftq_2r_sram.io.ren(1) := true.B
  io.cfiRead.entry := DontCare
  io.cfiRead.entry.rasTop := ftq_2r_sram.io.rdata(1).rasEntry
  io.cfiRead.entry.rasSp := ftq_2r_sram.io.rdata(1).rasSp
  io.cfiRead.entry.hist := ftq_2r_sram.io.rdata(1).hist
  io.cfiRead.entry.predHist := ftq_2r_sram.io.rdata(1).predHist
  io.cfiRead.entry.specCnt := ftq_2r_sram.io.rdata(1).specCnt
  io.cfiRead.entry.br_mask := ftq_2r_sram.io.rdata(1).br_mask
  // redirect, reset ptr
  when(io.flush || io.redirect.valid){
    val idx = Mux(io.flush, io.flushIdx, io.redirect.bits.ftqIdx)
    val next = idx + 1.U
    tailPtr := next
    val offset = Mux(io.flush, io.flushOffset, io.redirect.bits.ftqOffset)
    val notMisPredict = io.flush || (io.redirect.valid && RedirectLevel.flushItself(io.redirect.bits.level))
    commitStateQueue(idx.value).zipWithIndex.foreach({ case (s, i) =>
      when(i.U > offset || (notMisPredict && i.U === offset)){
        s := s_invalid
      }
    })
    when(next.value =/= headPtr.value){ // if next.value === headPtr.value, ftq is full
      commitStateQueue(next.value).foreach(_ := s_invalid)
    }
  }

  XSPerf("entry", validEntries)
  XSPerf("stall", io.enq.valid && !io.enq.ready)
  XSPerf("mispredictRedirect", io.redirect.valid && RedirectLevel.flushAfter === io.redirect.bits.level)
  XSPerf("replayRedirect", io.redirect.valid && RedirectLevel.flushItself(io.redirect.bits.level))

  // Branch Predictor Perf counters
  if (!env.FPGAPlatform && env.EnablePerfDebug) {
    val fires = commitEntry.valids.zip(commitEntry.pd).map{case (valid, pd) => valid && !pd.notCFI}
    val predRights = (0 until PredictWidth).map{i => !commitEntry.mispred(i) && !commitEntry.pd(i).notCFI && commitEntry.valids(i)}
    val predWrongs = (0 until PredictWidth).map{i => commitEntry.mispred(i) && !commitEntry.pd(i).notCFI && commitEntry.valids(i)}
    val isBTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isBr}
    val isJTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isJal}
    val isITypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isJalr}
    val isCTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isCall}
    val isRTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isRet}

    val mbpInstrs = fires
    val mbpRights = predRights
    val mbpWrongs = predWrongs
    val mbpBRights = Cat(predRights) & Cat(isBTypes)
    val mbpBWrongs = Cat(predWrongs) & Cat(isBTypes)
    val mbpJRights = Cat(predRights) & Cat(isJTypes)
    val mbpJWrongs = Cat(predWrongs) & Cat(isJTypes)
    val mbpIRights = Cat(predRights) & Cat(isITypes)
    val mbpIWrongs = Cat(predWrongs) & Cat(isITypes)
    val mbpCRights = Cat(predRights) & Cat(isCTypes)
    val mbpCWrongs = Cat(predWrongs) & Cat(isCTypes)
    val mbpRRights = Cat(predRights) & Cat(isRTypes)
    val mbpRWrongs = Cat(predWrongs) & Cat(isRTypes)

    def ubtbCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
      commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
        case (((valid, pd), ans), taken) =>
        Mux(valid && pd.isBr,
          isWrong ^ Mux(ans.hit.asBool,
            Mux(ans.taken.asBool, taken && ans.target === commitEntry.target,
            !taken),
          !taken),
        false.B)
      }
    }

    def btbCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
      commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
        case (((valid, pd), ans), taken) =>
        Mux(valid && pd.isBr,
          isWrong ^ Mux(ans.hit.asBool,
            Mux(ans.taken.asBool, taken && ans.target === commitEntry.target,
            !taken),
          !taken),
        false.B)
      }
    }

    def tageCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
      commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
        case (((valid, pd), ans), taken) =>
        Mux(valid && pd.isBr,
          isWrong ^ (ans.taken.asBool === taken),
        false.B)
      }
    }

    def loopCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
      commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
        case (((valid, pd), ans), taken) =>
        Mux(valid && (pd.isBr) && ans.hit.asBool, 
          isWrong ^ (!taken),
            false.B)
      }
    }

    def rasCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
      commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
        case (((valid, pd), ans), taken) =>
        Mux(valid && pd.isRet.asBool /*&& taken*/ && ans.hit.asBool,
          isWrong ^ (ans.target === commitEntry.target),
            false.B)
      }
    }

    val ubtbRights = ubtbCheck(commitEntry, commitEntry.metas.map(_.ubtbAns), false.B)
    val ubtbWrongs = ubtbCheck(commitEntry, commitEntry.metas.map(_.ubtbAns), true.B)
    // btb and ubtb pred jal and jalr as well
    val btbRights = btbCheck(commitEntry, commitEntry.metas.map(_.btbAns), false.B)
    val btbWrongs = btbCheck(commitEntry, commitEntry.metas.map(_.btbAns), true.B)
    val tageRights = tageCheck(commitEntry, commitEntry.metas.map(_.tageAns), false.B)
    val tageWrongs = tageCheck(commitEntry, commitEntry.metas.map(_.tageAns), true.B)

    val loopRights = loopCheck(commitEntry, commitEntry.metas.map(_.loopAns), false.B)
    val loopWrongs = loopCheck(commitEntry, commitEntry.metas.map(_.loopAns), true.B)

    val rasRights = rasCheck(commitEntry, commitEntry.metas.map(_.rasAns), false.B)
    val rasWrongs = rasCheck(commitEntry, commitEntry.metas.map(_.rasAns), true.B)

    val perfCountsMap = Map(
      "BpInstr" -> PopCount(mbpInstrs),
      "BpBInstr" -> PopCount(commitEntry.valids.zip(commitEntry.pd).map{case (valid, pd) => valid && pd.isBr}),
      "BpRight"  -> PopCount(mbpRights),
      "BpWrong"  -> PopCount(mbpWrongs),
      "BpBRight" -> PopCount(mbpBRights),
      "BpBWrong" -> PopCount(mbpBWrongs),
      "BpJRight" -> PopCount(mbpJRights),
      "BpJWrong" -> PopCount(mbpJWrongs),
      "BpIRight" -> PopCount(mbpIRights),
      "BpIWrong" -> PopCount(mbpIWrongs),
      "BpCRight" -> PopCount(mbpCRights),
      "BpCWrong" -> PopCount(mbpCWrongs),
      "BpRRight" -> PopCount(mbpRRights),
      "BpRWrong" -> PopCount(mbpRWrongs),

      "ubtbRight" -> PopCount(ubtbRights),
      "ubtbWrong" -> PopCount(ubtbWrongs),
      "btbRight" -> PopCount(btbRights),
      "btbWrong" -> PopCount(btbWrongs),
      "tageRight" -> PopCount(tageRights),
      "tageWrong" -> PopCount(tageWrongs),

      "rasRight"  -> PopCount(rasRights),
      "rasWrong"  -> PopCount(rasWrongs),
      "loopRight" -> PopCount(loopRights),
      "loopWrong" -> PopCount(loopWrongs),
    )

    for((key, value) <- perfCountsMap) {
      XSPerf(key, value)
    }
  }

  XSDebug(io.commit_ftqEntry.valid, p"ftq commit: ${io.commit_ftqEntry.bits}")
  XSDebug(io.enq.fire(), p"ftq enq: ${io.enq.bits}")
}
