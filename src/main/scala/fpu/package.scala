import chisel3._
import chisel3.util._

package object fpu {

  object FPUOpType {
    def funcWidth = 6
    def FpuOp(fu: String, op: String): UInt = ("b" + fu + op).U(funcWidth.W)

    // FMA
    def fadd:UInt   = FpuOp("000", "000")
    def fsub:UInt   = FpuOp("000", "001")
    def fmadd:UInt  = FpuOp("000", "100")
    def fmsub:UInt  = FpuOp("000", "101")
    def fnmsub:UInt = FpuOp("000", "110")
    def fnmadd:UInt = FpuOp("000", "111")
    def fmul:UInt   = FpuOp("000", "010")

    // FCMP
    def fmin:UInt   = FpuOp("001", "000")
    def fmax:UInt   = FpuOp("001", "001")
    def fle:UInt    = FpuOp("001", "010")
    def flt:UInt    = FpuOp("001", "011")
    def feq:UInt    = FpuOp("001", "100")

    // FMV
    def fmv_f2i:UInt= FpuOp("010", "000")
    def fmv_i2f:UInt= FpuOp("010", "001")
    def fclass:UInt = FpuOp("010", "010")
    def fsgnj:UInt  = FpuOp("010", "110")
    def fsgnjn:UInt = FpuOp("010", "101")
    def fsgnjx:UInt = FpuOp("010", "100")

    // FloatToInt
    def f2w:UInt    = FpuOp("011", "000")
    def f2wu:UInt   = FpuOp("011", "001")
    def f2l:UInt    = FpuOp("011", "010")
    def f2lu:UInt   = FpuOp("011", "011")

    // IntToFloat
    def w2f:UInt    = FpuOp("100", "000")
    def wu2f:UInt   = FpuOp("100", "001")
    def l2f:UInt    = FpuOp("100", "010")
    def lu2f:UInt   = FpuOp("100", "011")

    // FloatToFloat
    def s2d:UInt    = FpuOp("101", "000")
    def d2s:UInt    = FpuOp("110", "000")

    // Div/Sqrt
    def fdiv:UInt   = FpuOp("111", "000")
    def fsqrt:UInt  = FpuOp("111", "001")
  }

  object FPUIOFunc {
    def in_raw = 0.U(1.W)
    def in_unbox = 1.U(1.W)

    def out_raw = 0.U(2.W)
    def out_box = 1.U(2.W)
    def out_sext = 2.U(2.W)
    def out_zext = 3.U(2.W)

    def apply(inputFunc: UInt, outputFunc:UInt) = Cat(inputFunc, outputFunc)
  }

  class Fflags extends Bundle {
    val invalid = Bool()    // 4
    val infinite = Bool()   // 3
    val overflow = Bool()   // 2
    val underflow = Bool()  // 1
    val inexact = Bool()    // 0
  }

  object RoudingMode {
    val RNE = "b000".U(3.W)
    val RTZ = "b001".U(3.W)
    val RDN = "b010".U(3.W)
    val RUP = "b011".U(3.W)
    val RMM = "b100".U(3.W)
  }

  class FloatPoint(val expWidth: Int, val mantWidth:Int) extends Bundle{
    val sign = Bool()
    val exp = UInt(expWidth.W)
    val mant = UInt(mantWidth.W)
    def defaultNaN: UInt = Cat(0.U(1.W), Fill(expWidth+1,1.U(1.W)), Fill(mantWidth-1,0.U(1.W)))
    def posInf: UInt = Cat(0.U(1.W), Fill(expWidth, 1.U(1.W)), 0.U(mantWidth.W))
    def negInf: UInt = Cat(1.U(1.W), posInf.tail(1))
    def maxNorm: UInt = Cat(0.U(1.W), Fill(expWidth-1, 1.U(1.W)), 0.U(1.W), Fill(mantWidth, 1.U(1.W)))
    def expBias: UInt = Fill(expWidth-1, 1.U(1.W))
    def expBiasInt: Int = (1 << (expWidth-1)) - 1
    def mantExt: UInt = Cat(exp=/=0.U, mant)
    def apply(x: UInt): FloatPoint = x.asTypeOf(new FloatPoint(expWidth, mantWidth))
  }

  object Float32 extends FloatPoint(8, 23)
  object Float64 extends FloatPoint(11, 52)

  def expOverflow(sexp: SInt, expWidth: Int): Bool = sexp >= Cat(0.U(1.W), Fill(expWidth, 1.U(1.W))).asSInt()
  def expOverflow(uexp: UInt, expWidth: Int): Bool = expOverflow(Cat(0.U(1.W), uexp).asSInt(), expWidth)

  def boxF32ToF64(x: UInt): UInt = Cat(Fill(32, 1.U(1.W)), x(31, 0))
  def unboxF64ToF32(x: UInt): UInt = Mux(x(63, 32)===Fill(32, 1.U(1.W)), x(31, 0), Float32.defaultNaN)

  def extF32ToF64(x: UInt): UInt = {
    val f32 = Float32(x)
    Cat(
      f32.sign,
      Mux(f32.exp === 0.U,
        0.U(Float64.expWidth.W),
        Mux((~f32.exp).asUInt() === 0.U,
          Cat("b111".U(3.W), f32.exp),
          Cat("b0111".U(4.W) + f32.exp.head(1), f32.exp.tail(1))
        )
      ),
      Cat(f32.mant, 0.U((Float64.mantWidth - Float32.mantWidth).W))
    )
  }
}

