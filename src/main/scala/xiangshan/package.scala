import chisel3._
import chisel3.util._

package object xiangshan {
  object SrcType {
    def reg = "b00".U
    def pc  = "b01".U
    def imm = "b01".U
    def fp  = "b10".U

    def DC = imm // Don't Care

    def isReg(srcType: UInt) = srcType===reg
    def isPc(srcType: UInt) = srcType===pc
    def isImm(srcType: UInt) = srcType===imm
    def isFp(srcType: UInt) = srcType===fp
    def isPcImm(srcType: UInt) = isPc(srcType) || isImm(srcType)
    def isRegFp(srcType: UInt) = isReg(srcType) || isFp(srcType)

    def apply() = UInt(2.W)
  }

  object SrcState {
    def busy    = "b0".U
    def rdy     = "b1".U
    // def specRdy = "b10".U // speculative ready, for future use
    def apply() = UInt(1.W)
  }

  object FuType extends HasXSParameter {
    def num           = exuParameters.NRFuType

    def jmp          = "b0000".U
    def i2f          = "b0001".U
    def csr          = "b0010".U
    def alu          = "b0110".U
    def mul          = "b0100".U
    def div          = "b0101".U
    def fence        = "b0011".U

    def fmac         = "b1000".U
    def fmisc        = "b1011".U
    def fDivSqrt     = "b1010".U

    def ldu          = "b1100".U
    def stu          = "b1101".U
    def mou          = "b1111".U // for amo, lr, sc, fence

    def apply() = UInt(log2Up(num).W)

    def isIntExu(fuType: UInt) = !fuType(3)
    def isJumpExu(fuType: UInt) = fuType === jmp
    def isFpExu(fuType: UInt) = fuType(3, 2) === "b10".U
    def isMemExu(fuType: UInt) = fuType(3, 2) === "b11".U
    def isLoadStore(fuType: UInt) = isMemExu(fuType) && !fuType(1)
    def isStoreExu(fuType: UInt) = isMemExu(fuType) && fuType(0)
    def isAMO(fuType: UInt) = fuType(1)

    def jmpCanAccept(fuType: UInt) = !fuType(2)
    def mduCanAccept(fuType: UInt) = fuType(2) && !fuType(1)
    def aluCanAccept(fuType: UInt) = fuType(2) && fuType(1)

    def fmacCanAccept(fuType: UInt) = !fuType(1)
    def fmiscCanAccept(fuType: UInt) = fuType(1)

    def loadCanAccept(fuType: UInt) = !fuType(0)
    def storeCanAccept(fuType: UInt) = fuType(0)

    def storeIsAMO(fuType: UInt) = fuType(1)

    val functionNameMap = Map(
      jmp.litValue() -> "jmp",
      i2f.litValue() -> "int to float",
      csr.litValue() -> "csr",
      alu.litValue() -> "alu",
      mul.litValue() -> "mul",
      div.litValue() -> "div",
      fence.litValue() -> "fence",
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
    def NORMAL = "b00".U  // int/fp
    def BRANCH = "b01".U  // branch
    def LOAD   = "b10".U  // load
    def STORE  = "b11".U  // store

    def apply() = UInt(2.W)
    def isLoadStore(commitType: UInt) = commitType(1)
    def lsInstIsStore(commitType: UInt) = commitType(0)
    def isStore(commitType: UInt) = isLoadStore(commitType) && lsInstIsStore(commitType)
    def isBranch(commitType: UInt) = commitType(0) && !commitType(1)
  }

  object RedirectLevel {
    def flushAfter = "b00".U
    def flush      = "b01".U
    def flushAll   = "b10".U
    def exception  = "b11".U

    def apply() = UInt(2.W)
    def isUnconditional(level: UInt) = level(1)
    def flushItself(level: UInt) = level(0)
    def isException(level: UInt) = level(1) && level(0)
  }

  object ExceptionVec {
    def apply() = Vec(16, Bool())
  }

  object PMAMode {
    def R = "b1".U << 0 //readable
    def W = "b1".U << 1 //writeable
    def X = "b1".U << 2 //executable
    def I = "b1".U << 3 //cacheable: icache
    def D = "b1".U << 4 //cacheable: dcache
    def S = "b1".U << 5 //enable speculative access
    def A = "b1".U << 6 //enable atomic operation, A imply R & W
    def C = "b1".U << 7 //if it is cacheable is configable
    def Reserved = "b0".U

    def apply() = UInt(7.W)

    def read(mode: UInt) = mode(0)
    def write(mode: UInt) = mode(1)
    def execute(mode: UInt) = mode(2)
    def icache(mode: UInt) = mode(3)
    def dcache(mode: UInt) = mode(4)
    def speculate(mode: UInt) = mode(5)
    def atomic(mode: UInt) = mode(6)
    def configable_cache(mode: UInt) = mode(7)

    def strToMode(s: String) = {
      var result = 0.U << 8
      if (s.toUpperCase.indexOf("R") >= 0) result = result + R
      if (s.toUpperCase.indexOf("W") >= 0) result = result + W
      if (s.toUpperCase.indexOf("X") >= 0) result = result + X
      if (s.toUpperCase.indexOf("I") >= 0) result = result + I
      if (s.toUpperCase.indexOf("D") >= 0) result = result + D
      if (s.toUpperCase.indexOf("S") >= 0) result = result + S
      if (s.toUpperCase.indexOf("A") >= 0) result = result + A
      if (s.toUpperCase.indexOf("C") >= 0) result = result + C
      result
    }
  }
}
