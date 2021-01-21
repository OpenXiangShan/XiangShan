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

class Ftq extends XSModule with HasCircularQueuePtrHelper with NeedImpl {
  val io = IO(new Bundle() {
    val enq = Flipped(DecoupledIO(new FtqEntry))
    val leftOne = Output(Bool())
    val enqPtr = Output(new FtqPtr)
    // roq commit, read out fectch packet and deq
    val roq_commits = Vec(CommitWidth, ValidIO(new RoqCommitInfo))
    val commit_ftqEntry = ValidIO(new FtqEntry)
    // redirect, reset enq ptr
    val redirect = Input(ValidIO(new Redirect))
    // exu write back, update info
    val exuWriteback = Vec(exuParameters.JmpCnt + exuParameters.AluCnt, Flipped(ValidIO(new ExuOutput)))
    // pc read reqs (0: jump/auipc 1: redirect 2: load replay / exceptions)
    val ftqRead = Vec(3, Flipped(new FtqRead))
  })

  val headPtr, tailPtr = RegInit(FtqPtr(false.B, 0.U))

  val validEntries = distanceBetween(tailPtr, headPtr)

  // enq
  io.leftOne := validEntries === (FtqSize - 1).U
  io.enq.ready := validEntries < FtqSize.U








}
