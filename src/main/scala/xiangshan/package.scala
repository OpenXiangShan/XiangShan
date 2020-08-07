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
    def num           = exuParameters.NRFuType

    def jmp          = "b0000".U
    def i2f          = "b0001".U
    def csr          = "b0010".U
    def alu          = "b0011".U
    def mul          = "b0100".U
    def div          = "b0101".U

    def fmac         = "b1000".U
    def fmisc        = "b1001".U
    def fDivSqrt     = "b1010".U

    def ldu          = "b1100".U
    def stu          = "b1101".U

    def apply() = UInt(log2Up(num).W)

    def isIntExu(fuType: UInt) =  !fuType(3)
    def isFpExu(fuType: UInt) = fuType(3, 2) === "b10".U
    def isMemExu(fuType: UInt) = fuType(3, 2) === "b11".U
    def isLoadExu(fuType: UInt) = fuType === ldu
    def isStoreExu(fuType: UInt) = fuType === stu

    val functionNameMap = Map(
      jmp.litValue() -> "jmp",
      i2f.litValue() -> "int to float",
      csr.litValue() -> "csr",
      alu.litValue() -> "alu",
      mul.litValue() -> "mul",
      div.litValue() -> "div",
      fmac.litValue() -> "fmac",
      fmisc.litValue() -> "fmisc",
      fDivSqrt.litValue() -> "fdiv/fsqrt",
      ldu.litValue() -> "load",
      stu.litValue() -> "store"
    )

  }

  object FuOpType extends HasXSParameter {
    def apply() = UInt(exuParameters.FuOpWidth.W)
  }

  object BTBtype {
    def B = "b00".U  // branch
    def J = "b01".U  // jump
    def I = "b10".U  // indirect
    def R = "b11".U  // return

    def apply() = UInt(2.W)
  }

  object CommitType {
    def INT = "b00".U    // int
    def FP = "b01".U     // fp
    def LOAD = "b10".U   // load
    def STORE = "b11".U  // store

    def apply() = UInt(2.W)
    def isLoadStore(commitType: UInt) = commitType(1)
  }
}
