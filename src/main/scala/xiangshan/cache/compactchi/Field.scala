package oceanus.compactchi

import chisel3._
import chisel3.util._

class CCHISize(val value: Int, val name: String) {
  def asUInt: UInt = value.U
  def U = asUInt
}

object CCHISize {

  val B1      = new CCHISize(0b000, "1 Byte")
  val B2      = new CCHISize(0b001, "2 Bytes")
  val B4      = new CCHISize(0b010, "4 Bytes")
  val B8      = new CCHISize(0b011, "8 Bytes")
  val B16     = new CCHISize(0b100, "16 Bytes")
  val B32     = new CCHISize(0b101, "32 Bytes")
  val B64     = new CCHISize(0b110, "64 Bytes")

  def apply() = UInt(3.W)
}

class CCHIResp(val value: Int, val name: String) {
  def isPD: Boolean = (value & 0b100) != 0
  def asUInt: UInt = value.U
  def U = asUInt
}

object CCHIResp {

  val I       = new CCHIResp(0b000, "I")
  val SC      = new CCHIResp(0b001, "SC")
  val UC      = new CCHIResp(0b010, "UC")
  val I_PD    = new CCHIResp(0b100, "I_PD")
  val SC_PD   = new CCHIResp(0b101, "SC_PD")
  val UC_PD   = new CCHIResp(0b110, "UC_PD")

  def isPD(value: UInt): Bool = (value & 0b100.U) =/= 0.U

  def apply() = UInt(3.W)
}
