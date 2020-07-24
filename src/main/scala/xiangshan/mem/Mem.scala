// See LICENSE.Berkeley for license details.

package xiangshan.mem

import chisel3._
import chisel3.util._
import xiangshan.XSBundle

trait MemoryOpConstants {
  val META_SZ   = 64
  val NUM_XA_OPS = 9
  val M_SZ      = 5
  def M_X       = BitPat("b?????")
  def M_XRD     = "b00000".U // int load
  def M_XWR     = "b00001".U // int store
  def M_PFR     = "b00010".U // prefetch with intent to read
  def M_PFW     = "b00011".U // prefetch with intent to write
  def M_XA_SWAP = "b00100".U
  def M_FLUSH_ALL = "b00101".U  // flush all lines
  def M_XLR     = "b00110".U
  def M_XSC     = "b00111".U
  def M_XA_ADD  = "b01000".U
  def M_XA_XOR  = "b01001".U
  def M_XA_OR   = "b01010".U
  def M_XA_AND  = "b01011".U
  def M_XA_MIN  = "b01100".U
  def M_XA_MAX  = "b01101".U
  def M_XA_MINU = "b01110".U
  def M_XA_MAXU = "b01111".U
  def M_FLUSH   = "b10000".U // write back dirty data and cede R/W permissions
  def M_PWR     = "b10001".U // partial (masked.U store
  def M_PRODUCE = "b10010".U // write back dirty data and cede W permissions
  def M_CLEAN   = "b10011".U // write back dirty data and retain R/W permissions
  def M_SFENCE  = "b10100".U // flush TLB
  def M_WOK     = "b10111".U // check write permissions but don't perform a write

  def isAMOLogical(cmd: UInt) = cmd === M_XA_SWAP || cmd === M_XA_XOR || cmd === M_XA_OR || cmd === M_XA_AND
  def isAMOArithmetic(cmd: UInt) = cmd === M_XA_ADD || cmd === M_XA_MIN || cmd === M_XA_MAX || cmd === M_XA_MINU || cmd === M_XA_MAXU
  def isAMO(cmd: UInt) = isAMOLogical(cmd) || isAMOArithmetic(cmd)
  def isPrefetch(cmd: UInt) = cmd === M_PFR || cmd === M_PFW
  def isRead(cmd: UInt) = cmd === M_XRD || cmd === M_XLR || cmd === M_XSC || isAMO(cmd)
  def isWrite(cmd: UInt) = cmd === M_XWR || cmd === M_PWR || cmd === M_XSC || isAMO(cmd)
  def isWriteIntent(cmd: UInt) = isWrite(cmd) || cmd === M_PFW || cmd === M_XLR
}

object MemoryOpConstants extends MemoryOpConstants {
}

class MemBundle extends XSBundle
  with MemoryOpConstants

class DCacheReq extends MemBundle
{
  val cmd  = UInt(M_SZ.W)
  val addr  = UInt(PAddrBits.W)
  val data  = UInt(DataBits.W)
  val mask  = UInt((DataBits/8).W)
  val meta  = UInt(META_SZ.W)
}

class DCacheResp extends MemBundle
{
  val data = UInt(DataBits.W)
  val meta  = UInt(META_SZ.W)
  val nack  = Bool()
}

class LSUDMemIO extends MemBundle
{
  val req = new DecoupledIO(Vec(memWidth, Valid(new DCacheReq)))
  val resp = Flipped(Vec(memWidth, new ValidIO(new DCacheResp)))
}
