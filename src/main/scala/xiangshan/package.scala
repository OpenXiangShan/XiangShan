import chisel3._
import chisel3.util._

package object xiangshan {
  object SrcType {
    def reg = "b00".U
    def pc  = "b01".U
    def imm = "b01".U
    def fp  = "b10".U

    def isReg(srcType: UInt) = srcType===reg
    def isPc(srcType: UInt) = srcType===pc
    def isImm(srcType: UInt) = srcType===imm
    def isFp(srcType: UInt) = srcType===fp
    def isPcImm(srcType: UInt) = isPc(srcType) || isImm(srcType)
    def isRegFp(srcType: UInt) = isReg(srcType) || isFp(srcType)

    def apply() = UInt(2.W)
  }

  object SrcState {
    def busy    = "b00".U
    def rdy     = "b01".U
    def specRdy = "b10".U // speculative ready, for future use
    def apply() = UInt(2.W)
  }

  object FuType extends HasXSParameter {
    def num           = exuConfig.NRFuType
    def bru          = "b0000".U
    def alu          = "b0001".U
    def mul          = "b0010".U
    def mdu          = "b0011".U
    def fmac         = "b0100".U
    def fmisc        = "b0101".U
    def fmiscDivSqrt = "b0110".U
    def ldu          = "b1001".U
    def stu          = "b1000".U

    def apply() = UInt(log2Up(num).W)

    def isIntExu(fuType: UInt) =  fuType(3, 2) === "b00".U
    def isFpExu(fuType: UInt) = fuType(2)
    def isMemExu(fuType: UInt) = fuType(3)
  }

  object FuOpType extends HasXSParameter {
    def apply() = UInt(exuConfig.FuOpWidth.W)
  }
}
