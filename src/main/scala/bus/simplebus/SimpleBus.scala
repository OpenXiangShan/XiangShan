package bus.simplebus

import chisel3._
import chisel3.util._

import utils._
import bus.axi4._

object SimpleBusCmd {
                                //   hit    |    miss
  def cmdRead       = "b0000".U //  read    |   refill
  def cmdWrite      = "b0001".U //  write   |   refill
  def cmdProbe      = "b0010".U //  read    | do nothing
  def cmdUpdate     = "b0011".U //  write   | do nothing
  def cmdInvalidate = "b0010".U // invalide | do nothing
}

// Ucache Lightweight
class SimpleBusULReqBundle(dataBits: Int, userBits: Int = 0) extends Bundle {
  val addr = Output(UInt(32.W))
  val cmd = Output(UInt(1.W))
  val wmask = Output(UInt((dataBits / 8).W))
  val wdata = Output(UInt(dataBits.W))
  val user = Output(UInt(userBits.W))

  override def cloneType = new SimpleBusULReqBundle(dataBits, userBits).asInstanceOf[this.type]
  override def toPrintable: Printable = {
    p"addr = 0x${Hexadecimal(addr)}, cmd = ${cmd}, " +
    p"wmask = 0x${Hexadecimal(wmask)}, wdata = 0x${Hexadecimal(wdata)}\n"
  }

  def isRead() = cmd === SimpleBusCmd.cmdRead
  def isWrite() = cmd === SimpleBusCmd.cmdWrite
}

class SimpleBusULRespBundle(dataBits: Int, userBits: Int = 0) extends Bundle {
  val rdata = Output(UInt(dataBits.W))
  val user = Output(UInt(userBits.W))

  override def cloneType = new SimpleBusULRespBundle(dataBits, userBits).asInstanceOf[this.type]
  override def toPrintable: Printable = p"rdata = ${Hexadecimal(rdata)}\n"
}

// Uncache Heavyweight
class SimpleBusUHReqBundle(dataBits: Int, userBits: Int = 0)
  extends SimpleBusULReqBundle(dataBits, userBits) {
  override val cmd = Output(UInt(4.W))
  val size = Output(UInt(3.W))
  val burst = Output(Bool())
  val wlast = Output(Bool())

  override def cloneType = new SimpleBusUHReqBundle(dataBits, userBits).asInstanceOf[this.type]
  override def toPrintable: Printable =
    super.toPrintable + p"size = 0x${Hexadecimal(size)}, burst = ${burst}, wlast = ${wlast}\n"

  def isUpdate() = cmd === SimpleBusCmd.cmdUpdate
}

class SimpleBusUHRespBundle(dataBits: Int, userBits: Int = 0)
  extends SimpleBusULRespBundle(dataBits, userBits) {
  val rlast = Output(Bool())

  override def cloneType = new SimpleBusUHRespBundle(dataBits, userBits).asInstanceOf[this.type]
  override def toPrintable: Printable = super.toPrintable + p"rlast = ${rlast}\n"
}

class SimpleBusUL(dataBits: Int = 32, userBits: Int = 0) extends Bundle {
  val req = Decoupled(new SimpleBusULReqBundle(dataBits, userBits))
  val resp = Flipped(Decoupled(new SimpleBusULRespBundle(dataBits, userBits)))

  override def cloneType = new SimpleBusUL(dataBits, userBits).asInstanceOf[this.type]
  def isWrite() = req.valid && req.bits.isWrite()
  def isRead()  = req.valid && req.bits.isRead()
  def toAXI4() = SimpleBus2AXI4Converter(this, new AXI4Lite)

  def dump(name: String) = {
    when (req.fire()) { printf(p"${GTimer()},[${name}] ${req.bits}") }
    when (resp.fire()) { printf(p"${GTimer()},[${name}] ${resp.bits}") }
  }
}

class SimpleBusUH(dataBits: Int = 32, userBits: Int = 0)
  extends SimpleBusUL(dataBits, userBits) {
  override val req = Decoupled(new SimpleBusUHReqBundle(dataBits, userBits))
  override val resp = Flipped(Decoupled(new SimpleBusUHRespBundle(dataBits, userBits)))

  override def cloneType = new SimpleBusUH(dataBits, userBits).asInstanceOf[this.type]
  override def toAXI4() = SimpleBus2AXI4Converter(this, new AXI4)
}

// Cache
class SimpleBusC(dataBits: Int = 32, userBits: Int = 0) extends Bundle {
  val mem = new SimpleBusUH(dataBits, userBits)
  val coh = Flipped(new SimpleBusUH(dataBits, userBits))

  override def cloneType = new SimpleBusC(dataBits, userBits).asInstanceOf[this.type]
}
