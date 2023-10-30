package xiangshan.backend.fu.vector

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.XSCoreParamsKey
import xiangshan.backend.decode.isa.bitfield.InstVType
import xiangshan.backend.fu.VtypeStruct
import _root_.utils.NamedUInt

object Bundles {

  /**
    * vtype bundle, should not used as csr reg
    */
  class VType(implicit p: Parameters) extends Bundle {
    val illegal = Bool()
    val vma     = Bool()
    val vta     = Bool()
    val vsew    = VSew()
    val vlmul   = VLmul()
  }

  object VType {
    def apply()(implicit p: Parameters) : VType = {
      new VType
    }

    def fromInstVType(instVType: InstVType)(implicit p: Parameters) : VType = {
      val res = Wire(VType())
      res.vma   := instVType.vma
      res.vta   := instVType.vta
      res.vsew  := instVType.vsew(VSew.width - 1, 0)
      res.vlmul := instVType.vlmul
      res.illegal := false.B // Todo: add illegal check function
      res
    }

    def fromVtypeStruct(vtypeStruct: VtypeStruct)(implicit p: Parameters): VType = {
      val res = Wire(VType())
      res.illegal := vtypeStruct.vill
      res.vma := vtypeStruct.vma
      res.vta := vtypeStruct.vta
      res.vsew := vtypeStruct.vsew(VSew.width - 1, 0)
      res.vlmul := vtypeStruct.vlmul
      res
    }

    def toVtypeStruct(vtype: VType)(implicit p: Parameters) : VtypeStruct = {
      val res = WireInit(0.U.asTypeOf(new VtypeStruct))
      res.vill := vtype.illegal
      res.vma := vtype.vma
      res.vta := vtype.vta
      res.vsew := Cat(0.U(1.W), vtype.vsew)
      res.vlmul := vtype.vlmul
      res
    }
  }

  class VConfig(implicit p: Parameters) extends Bundle {
    val vtype = new VType
    val vl    = Vl()
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

  // modify the width when support more vector data width
  object VSew extends NamedUInt(2) {
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

  object VLmul extends NamedUInt(3) {
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

  object Vxsat extends NamedUInt(1)

  object Vxrm extends NamedUInt(2)

  object Nf extends NamedUInt(3)

  object VEew extends NamedUInt(3)

  class Fpu extends Bundle{
    val isFpToVecInst = Bool()
    val isFP32Instr   = Bool()
    val isFP64Instr   = Bool()
    val isReduction   = Bool()
    val isFoldTo1_2   = Bool()
    val isFoldTo1_4   = Bool()
    val isFoldTo1_8   = Bool()
  }
  object Fpu {
    def apply() = new Fpu
  }

  object Category extends NamedUInt(3) {
    def OPIVV : UInt = "b000".U(width.W)
    def OPFVV : UInt = "b001".U(width.W)
    def OPMVV : UInt = "b010".U(width.W)
    def OPIVI : UInt = "b011".U(width.W)
    def OPIVX : UInt = "b100".U(width.W)
    def OPFVF : UInt = "b101".U(width.W)
    def OPMVX : UInt = "b110".U(width.W)
    def OPCFG : UInt = "b111".U(width.W)
    def needScalaSrc(category: UInt) : Bool = {
      Seq(OPFVF).map(_ === category).reduce(_ || _)
    }
  }
}
