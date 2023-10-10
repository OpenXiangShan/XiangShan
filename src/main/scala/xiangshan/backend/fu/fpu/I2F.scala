
package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.{FuConfig, FuncUnit, HasPipelineReg}
import chipsalliance.rocketchip.config.Parameters

import yunsuan.vector.VectorConvert.utils._
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.vector.VectorConvert.{FloatFormat, f32, f64}
import yunsuan.vector.VectorConvert.util._

object I2fType {
  def width = 4
  //code: move/cvt ## i64/i32(input) ## f64/f32(output) ## hassign
  def fcvt_s_wu       = "b0000".U(4.W)
  def fcvt_s_w        = "b0001".U(4.W)
  def fcvt_s_lu       = "b0100".U(4.W)
  def fcvt_s_l        = "b0101".U(4.W)

  def fcvt_d_wu       = "b0010".U(4.W)
  def fcvt_d_w        = "b0011".U(4.W)
  def fcvt_d_lu       = "b0110".U(4.W)
  def fcvt_d_l        = "b0111".U(4.W)

  def fmv_w_x         = "b1000".U(4.W)
  def fmv_d_x         = "b1110".U(4.W)
}

class I2FCVT(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) with HasPipelineReg{
  override def latency: Int = cfg.latency.latencyVal.get
  val i2f = Module(new Int2FP)
  i2f.io.src := io.in.bits.data.src(0)
  i2f.io.opType := io.in.bits.ctrl.fuOpType
  i2f.io.rm :=  Mux(io.frm.get === "b111".U, io.in.bits.ctrl.rmInst.get, io.frm.get)

  io.out.bits.res.data := i2f.io.result
  io.out.bits.res.fflags.get := i2f.io.fflags
}

class I2fCvtIO extends Bundle{
  val src = Input(UInt(64.W))
  val opType = Input(UInt(4.W))
  val rm = Input(UInt(3.W))

  val result = Output(UInt(64.W))
  val fflags = Output(UInt(5.W))
}

class Int2FP extends Module{

  /** critical path
   * int->fp: abs(adder) -> exp adder  -> sl | ->  rounding(adder) -> Mux/Mux1H  |-> result & fflags
   */

  //parameter
  val fpParamMap: Seq[FloatFormat] = Seq(f32, f64)
  val widthExpAdder = 13 // 13bits is enough

  //input
  val io = IO(new I2fCvtIO)
  val (src, opType, rm) = (io.src, io.opType, io.rm)

  //cycle0
  val hasSignInt = opType(0).asBool

  val inputWidth = opType(2)
  val outputWidth = opType(1)
  val isMvInst = opType(3)
  val s2_isMvInst = RegNext(RegNext(isMvInst))
  val s2_is32Bits = RegNext(RegNext(!outputWidth))
  val s2_src = RegNext(RegNext(src))

  val s0_float1HOut = opType(1) ## !opType(1)
  val s1_float1HOut = RegNext(s0_float1HOut, 0.U)

  val srcMap: Seq[UInt] = Seq(src.tail(32), src)
  val intMap: Seq[Bits] = srcMap.map((int: UInt) => intExtend(int, hasSignInt && int.head(1).asBool))

  val input = Mux(inputWidth,
    intMap(1),
    intMap(0)
  )

  val s0_signSrc = input.head(1).asBool

  val absIntSrc = Wire(UInt(64.W)) //cycle0
  absIntSrc := Mux(s0_signSrc, (~input.tail(1)).asUInt + 1.U, input.tail(1))
  val s0_isZeroIntSrc = !absIntSrc.orR

  val s1_signSrc = RegNext(s0_signSrc, false.B)
  val s1_rm = RegNext(rm, false.B)
  
  val s1_isZeroIntSrc = RegNext(s0_isZeroIntSrc, false.B)


  val s0_exp = Wire(UInt(widthExpAdder.W))
  val expReg = RegNext(s0_exp, 0.U(widthExpAdder.W))
  val s1_exp = Wire(UInt(widthExpAdder.W))
  s1_exp := expReg


  //for cycle2 -> output
  val nv, dz, of, uf, nx = Wire(Bool()) //cycle1
  val s1_fflags = Wire(UInt(5.W))
  s1_fflags := Cat(nv, dz, of, uf, nx)
  val s2_fflags = RegNext(s1_fflags, 0.U(5.W))
  val s1_result = Wire(UInt(64.W))
  val s2_result = RegNext(s1_result, 0.U(64.W))

  val leadZeros = CLZ(absIntSrc)
  s0_exp := Mux1H(s0_float1HOut, fpParamMap.map(fp => (fp.bias + 63).U)) - leadZeros

  val shiftLeft = Wire(UInt(64.W))
  shiftLeft := absIntSrc.asUInt << 1 << leadZeros //cycle0

  val s0_rounderMapIn = Wire(UInt(64.W))
  s0_rounderMapIn := shiftLeft
  val rounderMapInReg = RegNext(s0_rounderMapIn, 0.U(64.W))

  val s1_rounderMapIn = Wire(UInt(64.W))
  s1_rounderMapIn := rounderMapInReg

  val rounderMap =
    fpParamMap.map(fp => Seq(
      s1_rounderMapIn.head(fp.fracWidth),
      s1_rounderMapIn.tail(fp.fracWidth).head(1),
      s1_rounderMapIn.tail(fp.fracWidth + 1).orR,
      s1_rounderMapIn.head(fp.fracWidth).andR
    )).transpose

  val (rounderInputMap, rounerInMap, rounderStikyMap, isOnesRounderInputMap) = {
    (rounderMap(0), rounderMap(1), rounderMap(2), rounderMap(3))
  }

  val rounderInput = Mux1H(s1_float1HOut, rounderInputMap)


  val rounder = Module(new RoundingUnit(64))
  rounder.io.in := rounderInput
  rounder.io.roundIn := Mux1H(s1_float1HOut, rounerInMap)
  rounder.io.stickyIn := Mux1H(s1_float1HOut, rounderStikyMap)
  rounder.io.signIn := s1_signSrc
  rounder.io.rm := s1_rm

  // from rounder
  val nxRounded = rounder.io.inexact
  val upRounded = rounder.io.r_up

  val expIncrease = s1_exp + 1.U
  val rounderInputIncrease = rounderInput + 1.U

  val cout = upRounded &&
    Mux1H(s1_float1HOut, isOnesRounderInputMap).asBool

  val expRounded = Wire(UInt(f64.expWidth.W))
  expRounded := Mux(cout, expIncrease, s1_exp)
  val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)

  val rmin =
    s1_rm === RTZ || (s1_signSrc && s1_rm === RUP) || (!s1_signSrc && s1_rm === RDN) //cycle1


  // Mux(cout, exp > FP.maxExp -1, exp > FP.maxExp)
  val ofRounded = !s1_exp.head(1).asBool && Mux1H(s1_float1HOut,
    fpParamMap.map(fp => Mux(cout,
      s1_exp(fp.expWidth - 1, 1).andR || s1_exp(s1_exp.getWidth - 2, fp.expWidth).orR,
      s1_exp(fp.expWidth - 1, 0).andR || s1_exp(s1_exp.getWidth - 2, fp.expWidth).orR)
    )
  )

  nv := false.B
  dz := false.B
  of := ofRounded
  uf := false.B
  nx := ofRounded || nxRounded

  val result1H = Cat(
    ofRounded && rmin,
    ofRounded && !rmin,
    s1_isZeroIntSrc,
    !ofRounded && !s1_isZeroIntSrc
  )

  def int2FpResultMapGen(fp: FloatFormat): Seq[UInt] = {
    VecInit((0 to 3).map {
      case 0 => s1_signSrc ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W) //GNF
      case 1 => s1_signSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W) // INF
      case 2 => s1_signSrc ## 0.U((fp.width - 1).W) // 0
      case 3 => s1_signSrc ## expRounded(fp.expWidth-1, 0) ## fracRounded(fp.fracWidth-1, 0) // normal
    })
  }

  val int2FpResultMap: Seq[UInt] = fpParamMap.map(fp => Mux1H(result1H.asBools.reverse, int2FpResultMapGen(fp)))
  s1_result := Mux1H(s1_float1HOut, int2FpResultMap)

  val dest = Mux(s2_isMvInst, s2_src, s2_result)
  io.result := Mux(s2_is32Bits, ~0.U(32.W) ## dest.tail(32), dest)
  io.fflags :=  Mux(s2_isMvInst, 0.U, s2_fflags)
}

