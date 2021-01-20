package xiangshan.backend.ftq

import chisel3._
import chisel3.util._
import utils.{CircularQueuePtr, HasCircularQueuePtrHelper}
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

class FtqRead extends Bundle {
  val ptr = Input(new FtqPtr)
  val entry = Output(new FtqEntry)
}

class Ftq extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val enq = Flipped(DecoupledIO(new FtqEntry))
    val leftOne = Output(Bool())
    // roq commit, read out fectch packet and deq
    val commit_idxes = Vec(CommitWidth, Flipped(ValidIO(new FtqPtr)))
    val commit_cfiUpdate = Vec(CommitWidth, ValidIO(new CfiUpdateInfo))
    // redirect, reset enq ptr
    val redirect = Input(ValidIO(new Redirect))
    // exu write back, update info
    val exuWriteback = Vec(exuParameters.JmpCnt + exuParameters.AluCnt, Flipped(ValidIO(new ExuOutput)))
    // pc read reqs (1 for load replay / exceptions, 1 for jump/auipc)
    val ftqRead = Vec(2, Output(new FtqEntry))
  })

  val headPtr, tailPtr = RegInit(FtqPtr(false.B, 0.U))

  val validEntries = distanceBetween(tailPtr, headPtr)

  // enq
  io.leftOne := validEntries === (FtqSize - 1).U
  io.enq.ready := validEntries < FtqSize.U





}
