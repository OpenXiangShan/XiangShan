package xiangshan

import chisel3._
import org.chipsalliance.cde.config.Parameters

object TeaEvent {
  val DR_L1  = 0
  val DR_TLB = 1
  val DR_SQ  = 2
  val FL_MB  = 3
  val FL_EX  = 4
  val FL_MO  = 5
  val ST_L1  = 6
  val ST_TLB = 7
  val ST_LLC = 8

  val width = 9

  def bit(idx: Int): UInt = 1.U(width.W) << idx
}

object TeaPsvOps {
  def empty: UInt = 0.U(TeaEvent.width.W)

  def setBit(psv: UInt, event: Int): UInt = {
    psv | TeaEvent.bit(event)
  }
}

class TeaOIR(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val pc = UInt(VAddrBits.W)
  val psv = UInt(TeaEvent.width.W)
}

class TeaEntry(implicit p: Parameters) extends XSBundle {
  val cycle = UInt(64.W)
  val state = UInt(4.W)
  val validMask = UInt(CommitWidth.W)
  val pcVec = Vec(CommitWidth, UInt(VAddrBits.W))
  val psvVec = Vec(CommitWidth, UInt(TeaEvent.width.W))
  val oirValid = Bool()
  val pendingDrain = Bool()
}
