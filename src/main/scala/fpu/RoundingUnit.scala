package fpu

import chisel3._
import chisel3.util._
import fpu.RoudingMode._
import fpu.util.ORTree

class RoundingUnit(mantWidth: Int) extends Module{
  val io = IO(new Bundle() {
    val in = Input(new Bundle() {
      val rm = UInt(3.W)
      val mant = UInt(mantWidth.W)
      val sign, guard, round, sticky = Bool()
    })
    val out = Output(new Bundle() {
      val mantRounded = UInt(mantWidth.W)
      val inexact = Bool()
      val mantCout = Bool()
      val roundUp = Bool()
    })
  })

  val inexact = io.in.guard | io.in.round | io.in.sticky
  val lsb = io.in.mant(0)
  val roundUp = MuxLookup(io.in.rm, false.B, Seq(
    RNE -> (io.in.guard && (io.in.round | io.in.sticky | lsb)),
    RTZ -> false.B,
    RUP -> (inexact & (!io.in.sign)),
    RDN -> (inexact & io.in.sign),
    RMM -> io.in.guard
  ))
  val mantRoundUp = io.in.mant +& 1.U
  val cout = mantRoundUp(mantWidth)
  val mantRounded = Mux(roundUp,
    mantRoundUp(mantWidth-1, 0),
    io.in.mant
  )
  io.out.inexact := inexact
  io.out.mantRounded := mantRounded
  io.out.mantCout := cout & roundUp
  io.out.roundUp := roundUp
}

class RoundWithExceptionsIO(sexpWidth:Int, mantWidth:Int) extends Bundle {
  val isDouble = Input(Bool())
  val denormShiftAmt = Input(SInt(sexpWidth.W))
  val sign = Input(Bool())
  val expNorm = Input(SInt(sexpWidth.W))
  val mantWithGRS = Input(UInt((mantWidth+3).W))
  val rm = Input(UInt(3.W))
  val specialCaseHappen = Input(Bool())
  val expRounded = Output(SInt(sexpWidth.W))
  val mantRounded = Output(UInt(mantWidth.W))
  val inexact = Output(Bool())
  val overflow = Output(Bool())
  val underflow = Output(Bool())
  val ovSetInf = Output(Bool())
  val isZeroResult = Output(Bool())

  override def cloneType: RoundWithExceptionsIO.this.type =
    new RoundWithExceptionsIO(sexpWidth, mantWidth).asInstanceOf[this.type]
}

class RoundF64AndF32WithExceptions
(
  expInHasBias: Boolean = false,
  D_MANT_WIDTH: Int = Float64.mantWidth + 1,
  D_SEXP_WIDTH: Int = Float64.expWidth + 2,
  D_EXP_WIDTH: Int = Float64.expWidth,
  S_MANT_WIDTH: Int = Float32.mantWidth + 1,
  S_SEXP_WIDTH: Int = Float32.expWidth + 2,
  S_EXP_WIDTH: Int = Float32.expWidth
) extends Module with HasUIntToSIntHelper {

  val io = IO(new RoundWithExceptionsIO(D_SEXP_WIDTH, D_MANT_WIDTH))

  val isDouble = io.isDouble

  val rounding = Module(new RoundingUnit(D_MANT_WIDTH))

  val mantUnrounded = io.mantWithGRS.head(D_MANT_WIDTH)
  rounding.io.in.sign := io.sign
  rounding.io.in.mant := mantUnrounded
  rounding.io.in.rm := io.rm
  rounding.io.in.guard := io.mantWithGRS(2)
  rounding.io.in.round := io.mantWithGRS(1)
  rounding.io.in.sticky := io.mantWithGRS(0)

  val mantRounded = rounding.io.out.mantRounded
  val mantCout = Mux(isDouble,
    Mux(!mantUnrounded(D_MANT_WIDTH-1),
      mantRounded(D_MANT_WIDTH-1),
      rounding.io.out.mantCout
    ),
    Mux(!mantUnrounded(S_MANT_WIDTH-1),
      mantRounded(S_MANT_WIDTH-1),
      mantRounded(S_MANT_WIDTH)
    )
  )
  val isZeroResult = !ORTree(Cat(mantCout, mantRounded))

  val expRounded = Mux(io.denormShiftAmt > 0.S || isZeroResult,
    0.S,
    if(expInHasBias) io.expNorm
    else io.expNorm + Mux(isDouble, Float64.expBias, Float32.expBias).toSInt
  ) + mantCout.toSInt

  val common_inexact = rounding.io.out.inexact

  val roundingInc = MuxLookup(io.rm, "b10".U(2.W), Seq(
    RoudingMode.RDN -> Mux(io.sign, "b11".U, "b00".U),
    RoudingMode.RUP -> Mux(io.sign, "b00".U, "b11".U),
    RoudingMode.RTZ -> "b00".U
  ))
  val isDenormalMant = (io.mantWithGRS + roundingInc) < Mux(isDouble,
    Cat(1.U(1.W), 0.U((D_MANT_WIDTH+2).W)),
    Cat(1.U(1.W), 0.U((S_MANT_WIDTH+2).W))
  )

  val common_underflow = (
    io.denormShiftAmt > 1.S ||
      io.denormShiftAmt===1.S && isDenormalMant ||
      isZeroResult
    ) && common_inexact


  val common_overflow = Mux(isDouble,
    expOverflow(expRounded, D_EXP_WIDTH),
    expOverflow(expRounded, S_EXP_WIDTH)
  )
  val ovSetInf = io.rm === RoudingMode.RNE ||
    io.rm === RoudingMode.RMM ||
    (io.rm === RoudingMode.RDN && io.sign) ||
    (io.rm === RoudingMode.RUP && !io.sign)

  io.expRounded := expRounded
  io.mantRounded := mantRounded
  io.inexact := !io.specialCaseHappen && (common_inexact || common_overflow || common_underflow)
  io.overflow := !io.specialCaseHappen && common_overflow
  io.underflow := !io.specialCaseHappen && common_underflow
  io.ovSetInf := ovSetInf
  io.isZeroResult := isZeroResult
}