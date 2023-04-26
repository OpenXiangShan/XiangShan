package xiangshan.backend.fu.vector

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{BitPat, log2Up}
import xiangshan.XSCoreParamsKey
import xiangshan.backend.decode.isa.bitfield.InstVType

object Bundles {

  /**
    * vtype bundle, should not used as csr reg
    */
  class VType extends Bundle {
    val illegal = Bool()
    val vma     = Bool()
    val vta     = Bool()
    val vsew    = VSew()
    val vlmul   = VLmul()
  }

  object VType {
    def apply() : VType = {
      new VType
    }

    def fromInstVType(instVType: InstVType) : VType = {
      val res = Wire(VType())
      res.vma   := instVType.vma
      res.vta   := instVType.vta
      res.vsew  := instVType.vsew(VSew.width - 1, 0)
      res.vlmul := instVType.vlmul
      res.illegal := false.B // Todo: add illegal check function
      res
    }
  }

  class VConfig(implicit p: Parameters) extends Bundle {
    val vl    = Vl()
    val vtype = new VType
  }

  object VConfig {
    def apply()(implicit p: Parameters) : VConfig = {
      new VConfig()
    }
  }

  def mu: UInt = 0.U(1.W)
  def ma: UInt = 1.U(1.W)
  def tu: UInt = 0.U(1.W)
  def ta: UInt = 1.U(1.W)

  object VSew {
    def apply(): UInt = UInt(width.W)

    def width = 2 // modify it when support more vector data width

    def e8  : UInt = "b000".U(width.W)
    def e16 : UInt = "b001".U(width.W)
    def e32 : UInt = "b010".U(width.W)
    def e64 : UInt = "b011".U(width.W)

    def reserved: BitPat = BitPat("b1??")

    def isReserved(sew: UInt) : Bool = {
      require(sew.getWidth >= 2 && sew.getWidth <= 3)
      if (sew.getWidth == 3) {
        sew === reserved
      } else {
        false.B
      }
    }
  }

  object VLmul {
    def apply(): UInt = UInt(width.W)

    def width = 3

    def m1  : UInt = "b000".U(width.W)
    def m2  : UInt = "b001".U(width.W)
    def m4  : UInt = "b010".U(width.W)
    def m8  : UInt = "b011".U(width.W)
    def mf2 : UInt = "b111".U(width.W)
    def mf4 : UInt = "b110".U(width.W)
    def mf8 : UInt = "b101".U(width.W)

    def reserved: BitPat = BitPat("b100")

    def isReserved(vlmul: UInt) : Bool = {
      require(vlmul.getWidth == 3)
      vlmul === reserved
    }
  }

  object Vl {
    def apply()(implicit p: Parameters): UInt = UInt(width.W)

    def width(implicit p: Parameters) = p(XSCoreParamsKey).vlWidth
  }
}
