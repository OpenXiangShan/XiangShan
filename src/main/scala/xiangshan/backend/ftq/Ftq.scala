package xiangshan.backend.ftq

import chisel3._
import chisel3.util._
import utils.{CircularQueuePtr, DataModuleTemplate, HasCircularQueuePtrHelper}
import xiangshan._

class FtqPtr extends CircularQueuePtr (FtqPtr.FtqSize) with HasCircularQueuePtrHelper

object FtqPtr extends HasXSParameter {
  def apply(f: Bool, v: UInt): FtqPtr = {
    val ptr = Wire(new FtqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

object GetPcByFtq extends HasXSParameter {
  def apply(ftqPC: UInt, ftqOffset: UInt) = {
    assert(ftqPC.getWidth == VAddrBits)
    assert(ftqOffset.getWidth == log2Up(PredictWidth))
    Cat(
      ftqPC.head(VAddrBits - ftqOffset.getWidth - instOffsetBits), // packet pc
      ftqOffset,                                                   // offset
      0.U(instOffsetBits.W)                                        // 0s
    )
  }
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
    // update mispredict target
    val frontendRedirect = Flipped(ValidIO(new Redirect))
    // exu write back, update info
    val exuWriteback = Vec(exuParameters.JmpCnt + exuParameters.AluCnt, Flipped(ValidIO(new ExuOutput)))
    // pc read reqs (0: jump/auipc 1: mispredict/load replay 2: exceptions)
    val ftqRead = Vec(3, Flipped(new FtqRead))
  })

  val headPtr, tailPtr = RegInit(FtqPtr(false.B, 0.U))

  val validEntries = distanceBetween(tailPtr, headPtr)

  // enq
  io.leftOne := validEntries === (FtqSize - 1).U
  io.enq.ready := validEntries < FtqSize.U
  io.enqPtr := tailPtr

  val dataModule = Module(new DataModuleTemplate(new FtqEntry, FtqSize, 4, 1, true))
  dataModule.io.wen := io.enq.fire()
  dataModule.io.waddr := tailPtr
  dataModule.io.wdata := io.enq.bits

  /* TODO: wrap these sigs in DataModuleTemplate
      these fields need update when exu write back,
      so split them out
  */
  val target_vec = Reg(Vec(FtqSize, UInt(VAddrBits.W)))
  val cfiIndex_vec = Reg(Vec(FtqSize, ValidUndirectioned(UInt(log2Up(PredictWidth).W))))
  val cfiIsCall, cfiIsRet, cfiIsRVC = Reg(Vec(FtqSize, Bool()))
  val mispredict_vec = Reg(Vec(FtqSize, Vec(PredictWidth, Bool())))

  val commitStateQueue = Reg(Vec(FtqSize, Vec(PredictWidth, Bool())))

  when(io.enq.fire()){
    val enqIdx = tailPtr.value
    commitStateQueue(enqIdx) := io.enq.bits.valids
    cfiIndex_vec(enqIdx) := io.enq.bits.cfiIndex
    cfiIsCall(enqIdx) := io.enq.bits.cfiIsCall
    cfiIsRet(enqIdx) := io.enq.bits.cfiIsRet
    cfiIsRVC(enqIdx) := io.enq.bits.cfiIsRVC
    mispredict_vec(enqIdx) := WireInit(VecInit(Seq.fill(PredictWidth)(false.B)))
  }

  tailPtr := tailPtr + io.enq.fire()

  // exu write back, update some info
  for((wb, i) <- io.exuWriteback.zipWithIndex){
    val wbIdx = wb.bits.redirect.ftqIdx.value
    val offset = wb.bits.redirect.ftqOffset
    val cfiUpdate = wb.bits.redirect.cfiUpdate
    when(wb.bits.redirectValid){
      mispredict_vec(wbIdx)(offset) := cfiUpdate.isMisPred
      when(!cfiIndex_vec(wbIdx).valid || cfiIndex_vec(wbIdx).bits > offset){
        cfiIndex_vec(wbIdx).valid := true.B
        cfiIndex_vec(wbIdx).bits := offset
        cfiIsCall(wbIdx) := wb.bits.uop.cf.pd.isCall
        cfiIsRet(wbIdx) := wb.bits.uop.cf.pd.isRet
        cfiIsRVC(wbIdx) := wb.bits.uop.cf.pd.isRVC
      }
    }
  }

  // fix mispredict entry
  when(io.frontendRedirect.valid){
    target_vec(io.frontendRedirect.bits.ftqIdx.value) := io.frontendRedirect.bits.cfiUpdate.target
  }

  // commit
  val commitVec = Wire(Vec(PredictWidth, Bool()))
  for((c, v) <- io.roq_commits.zip(commitVec)){
    when(c.valid){
      commitStateQueue(c.bits.ftqIdx.value)(c.bits.ftqOffset) := false.B
      v := c.bits.ftqIdx.value === headPtr.value
    }
  }

  when(commitStateQueue(headPtr.value).asUInt() === 0.U && headPtr =/= tailPtr){
    headPtr := headPtr + 1.U
  }

  dataModule.io.raddr(0) := headPtr.value
  val commitEntry = WireInit(dataModule.io.rdata(0))
  commitEntry.valids := RegNext(commitVec)
  commitEntry.mispred := RegNext(mispredict_vec(headPtr.value))
  commitEntry.cfiIndex := RegNext(cfiIndex_vec(headPtr.value))
  commitEntry.cfiIsCall := RegNext(cfiIsCall(headPtr.value))
  commitEntry.cfiIsRet := RegNext(cfiIsRet(headPtr.value))
  commitEntry.cfiIsRVC := RegNext(cfiIsRVC(headPtr.value))
  commitEntry.jalr_target := RegNext(target_vec(headPtr.value))

  io.commit_ftqEntry.valid := RegNext(commitVec.asUInt().orR())
  io.commit_ftqEntry.bits := commitEntry

  // read logic
  for((req, i) <- io.ftqRead.zipWithIndex){
    dataModule.io.raddr(1 + i) := req.ptr.value
    val dataRead = WireInit(dataModule.io.rdata(1 + i))
    dataRead.mispred := RegNext(mispredict_vec(req.ptr.value))
    req.entry := dataRead
  }

  // redirect, reset ptr
  when(io.redirect.valid){
    when(io.redirect.bits.isUnconditional()){ // flush pipe / exception
      // clear ftq
      tailPtr := headPtr
      commitStateQueue(headPtr.value).foreach(_ := false.B)
      assert(headPtr === io.redirect.bits.ftqIdx)
    }.otherwise{ // branch misprediction or load replay
      val isReplay = RedirectLevel.flushItself(io.redirect.bits.level)
      tailPtr := io.redirect.bits.ftqIdx + 1.U
      val offset = io.redirect.bits.ftqOffset
      commitStateQueue(io.redirect.bits.ftqIdx.value).zipWithIndex.foreach({case(v, i) =>
        when(i.U > offset || (isReplay && i.U === offset)){ // replay will not commit
          v := false.B
        }
      })
    }
  }
}
