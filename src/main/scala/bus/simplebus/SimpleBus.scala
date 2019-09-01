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

class SimpleBusReqBundle(val dataBits: Int, val userBits: Int = 0) extends Bundle {
  val addr = Output(UInt(32.W))
  val size = Output(UInt(3.W))
  val burst = Output(Bool())
  val cmd = Output(UInt(4.W))
  val wmask = Output(UInt((dataBits / 8).W))
  val wdata = Output(UInt(dataBits.W))
  val wlast = Output(Bool())
  val user = Output(UInt(userBits.W))

  override def toPrintable: Printable = {
    p"addr = 0x${Hexadecimal(addr)}, size = 0x${Hexadecimal(size)}, burst = ${burst} " +
    p"cmd = ${cmd}, wmask = 0x${Hexadecimal(wmask)}, wdata = 0x${Hexadecimal(wdata)}, wlast = ${wlast}"
  }

  def isRead() = cmd === SimpleBusCmd.cmdRead
  def isWrite() = cmd === SimpleBusCmd.cmdWrite
  def isUpdate() = cmd === SimpleBusCmd.cmdUpdate
}

class SimpleBusRespBundle(val dataBits: Int, val userBits: Int = 0) extends Bundle {
  val rdata = Output(UInt(dataBits.W))
  val rlast = Output(Bool())
  val user = Output(UInt(userBits.W))

  override def toPrintable: Printable = {
    p"rdata = ${Hexadecimal(rdata)}, rlast = ${rlast}"
  }
}

class SimpleBus(val dataBits: Int = 32, val userBits: Int = 0) extends Bundle {
  val req = Decoupled(new SimpleBusReqBundle(dataBits, userBits))
  val resp = Flipped(Decoupled(new SimpleBusRespBundle(dataBits, userBits)))

  def isWrite() = req.valid && req.bits.isWrite()
  def isRead()  = req.valid && req.bits.isRead()

  def toAXI4(isLite: Boolean = false) = {
    val mem2axi = Module(new SimpleBus2AXI4Converter(
      new AXI4, dataBits, userBits))
    mem2axi.io.in <> this
    mem2axi.io.out
  }

  def dump(name: String) = {
    when (req.fire()) { printf(p"${GTimer()},[${name}] ${req.bits}\n") }
    when (resp.fire()) { printf(p"${GTimer()},[${name}] ${resp.bits}\n") }
  }
}
