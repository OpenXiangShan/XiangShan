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
    assert(ftqPC.getWidth == (VAddrBits - log2Up(PredictWidth) - instOffsetBits))
    assert(ftqOffset.getWidth == log2Up(PredictWidth))
    Cat(ftqPC, ftqOffset, 0.U(instOffsetBits.W))
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
    val redirect = Input(ValidIO(new Redirect))
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
  val jalr_target_vec = Vec(FtqSize, Reg(UInt(VAddrBits.W)))
  val mispredict_vec = Vec(FtqSize, Reg(Vec(PredictWidth, Bool())))
  val taken_vec = Vec(FtqSize, Reg(Vec(PredictWidth, Bool())))

  val commitStateQueue = Vec(FtqSize, Vec(PredictWidth, Reg(Bool())))

  when(io.enq.fire()){
    val initVec = WireInit(VecInit(Seq.fill(PredictWidth)(false.B)))
    commitStateQueue(tailPtr.value) := io.enq.bits.valids
    jalr_target_vec(tailPtr.value) := io.enq.bits.jalr_target
    mispredict_vec(tailPtr.value) := initVec
    taken_vec(tailPtr.value) := initVec
  }

  tailPtr := tailPtr + io.enq.fire()

  // exu write back, update some info
  for((wb, i) <- io.exuWriteback.zipWithIndex){
    val wbIdx = wb.bits.redirect.ftqIdx.value
    val offset = wb.bits.redirect.ftqOffset
    val cfiUpdate = wb.bits.redirect.cfiUpdate
    when(wb.bits.redirectValid){
      if(i == 0){ // jump unit
        jalr_target_vec(wbIdx) := cfiUpdate.target
        mispredict_vec(wbIdx)(offset) := cfiUpdate.isMisPred
        taken_vec(wbIdx)(offset) := true.B // jump always taken
      } else {
        mispredict_vec(wbIdx)(offset) := cfiUpdate.isMisPred
        taken_vec(wbIdx)(offset) := cfiUpdate.taken
      }
    }
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
  commitEntry.taken := RegNext(taken_vec(headPtr.value))
  commitEntry.jalr_target := RegNext(jalr_target_vec(headPtr.value))

  io.commit_ftqEntry.valid := RegNext(commitVec.asUInt().orR())
  io.commit_ftqEntry.bits := commitEntry

  // read logic
  for((req, i) <- io.ftqRead.zipWithIndex){
    dataModule.io.raddr(1 + i) := req.ptr.value
    val dataRead = WireInit(dataModule.io.rdata(1 + i))
    dataRead.valids // TODO: how to set this ?
    dataRead.mispred := RegNext(mispredict_vec(req.ptr.value))
    dataRead.taken := RegNext(taken_vec(req.ptr.value))
    dataRead.jalr_target := RegNext(jalr_target_vec(req.ptr.value))
    req.entry := dataRead
  }

  // redirect, reset ptr
  when(io.redirect.valid){
    when(io.redirect.bits.isUnconditional()){ // flush pipe / exception
      tailPtr := headPtr
      assert(headPtr === io.redirect.bits.ftqIdx)
    }.otherwise{ // branch misprediction or load replay
      tailPtr := io.redirect.bits.ftqIdx
    }
  }
}
