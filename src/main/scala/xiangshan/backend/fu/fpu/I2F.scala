
package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.{FuConfig, FuncUnit, HasPipelineReg}
import chipsalliance.rocketchip.config.Parameters

import yunsuan.vector.VectorConvert.utils._
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.vector.VectorConvert.{FloatFormat, f32, f64}
import yunsuan.vector.VectorConvert.util._

// todo: refactor scalar i2f fu to rm fpctrlsign
// TODO: 看这里的dyinst的字段是怎么给到功能单元的

object I2fType {
  def width = 4
  // todo:  move/cvt ## i64/i32(input) ## f64/f32(output) ## hassign
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
  val fpParamMap = Seq(f32, f64)
  val widthExpAdder = 13 // 13bits is enough

  //input
  val io = IO(new I2fCvtIO)
  val (src, opType, rmNext) = (io.src, io.opType, io.rm)

  //cycle0
  val hasSignInt = opType(0).asBool
  val hasSignOut = RegNext(RegNext(hasSignInt))

  val inputWidth = opType(2)
  val outputWidth = opType(1)
  val isMvInst = opType(3)
  val isMvInstOut = RegNext(RegNext(isMvInst))
  val is32BitsOut = RegNext(RegNext(!outputWidth))
  val srcOut = RegNext(RegNext(src))

  val float1HOutNext = opType(1) ## !opType(1)
  val float1HOut = RegNext(float1HOutNext, 0.U)

  val srcMap = Seq(src.tail(32), src)
  val intMap = srcMap.map(int => intExtend(int, hasSignInt && int.head(1).asBool))

  val input = Mux(inputWidth,
    intMap(1),
    intMap(0)
  )

  val signSrcNext = input.head(1).asBool

  val absIntSrc = Wire(UInt(64.W)) //cycle0
  absIntSrc := Mux(signSrcNext, (~input.tail(1)).asUInt + 1.U, input.tail(1))
  val isZeroIntSrcNext = !absIntSrc.orR

  val signSrc = RegNext(signSrcNext, false.B)
  val rm = RegNext(rmNext, false.B)
  
  val isZeroIntSrc = RegNext(isZeroIntSrcNext, false.B)


  val expNext = Wire(UInt(widthExpAdder.W))
  val expReg = RegNext(expNext, 0.U(widthExpAdder.W))
  val exp = Wire(UInt(widthExpAdder.W))
  exp := expReg


  //for cycle2 -> output
  val nv, dz, of, uf, nx = Wire(Bool()) //cycle1
  val fflagsNext = Wire(UInt(5.W))
  fflagsNext := Cat(nv, dz, of, uf, nx)
  val fflags = RegNext(fflagsNext, 0.U(5.W))
  val resultNext = Wire(UInt(64.W))
  val result = RegNext(resultNext, 0.U(64.W))

  val leadZeros = CLZ(absIntSrc)
  expNext := Mux1H(float1HOutNext, fpParamMap.map(fp => (fp.bias + 63).U)) - leadZeros

  val shiftLeft = Wire(UInt(64.W))
  shiftLeft := absIntSrc.asUInt << 1 << leadZeros //cycle0

  val rounderMapInNext = Wire(UInt(64.W))
  val rounderMapInReg = RegNext(rounderMapInNext, 0.U(64.W))
  rounderMapInNext := shiftLeft

  val rounderMapIn = Wire(UInt(64.W))
  rounderMapIn := rounderMapInReg

  val rounderMap =
    fpParamMap.map(fp => Seq(
      rounderMapIn.head(fp.fracWidth),
      rounderMapIn.tail(fp.fracWidth).head(1),
      rounderMapIn.tail(fp.fracWidth + 1).orR,
      rounderMapIn.head(fp.fracWidth).andR
    )
    ).transpose

  val (rounderInputMap, rounerInMap, rounderStikyMap, isOnesRounderInputMap) = {
    (rounderMap(0), rounderMap(1), rounderMap(2), rounderMap(3))
  }

  val rounderInput = Mux1H(float1HOut, rounderInputMap)


  val rounder = Module(new RoundingUnit(64))
  rounder.io.in := rounderInput
  rounder.io.roundIn := Mux1H(float1HOut, rounerInMap)
  rounder.io.stickyIn := Mux1H(float1HOut, rounderStikyMap)
  rounder.io.signIn := signSrc
  rounder.io.rm := rm

  // from rounder
  val nxRounded = rounder.io.inexact
  val upRounded = rounder.io.r_up

  /** after rounding
   *  for all exclude estimate7 & fp->fp widen
   *  cycle: 1
   */
  val expIncrease = exp + 1.U
  val rounderInputIncrease = rounderInput + 1.U

  val cout = upRounded &&
    Mux1H(float1HOut, isOnesRounderInputMap).asBool

  val expRounded = Wire(UInt(f64.expWidth.W))
  expRounded := Mux(cout, expIncrease, exp)
  val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)

  val rmin =
    rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN) //cycle1


  // Mux(cout, exp > FP.maxExp -1, exp > FP.maxExp)
  val ofRounded = !exp.head(1).asBool && Mux1H(float1HOut,
    fpParamMap.map(fp => Mux(cout,
      exp(fp.expWidth - 1, 1).andR || exp(exp.getWidth - 2, fp.expWidth).orR,
      exp(fp.expWidth - 1, 0).andR || exp(exp.getWidth - 2, fp.expWidth).orR)
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
    isZeroIntSrc,
    !ofRounded && !isZeroIntSrc
  )

  def int2FpResultMapGen(fp: FloatFormat): Seq[UInt] = {
    VecInit((0 to 3).map {
      case 0 => signSrc ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W) //GNF
      case 1 => signSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W) // INF
      case 2 => signSrc ## 0.U((fp.width - 1).W) // 0
      case 3 => signSrc ## expRounded(fp.expWidth-1, 0) ## fracRounded(fp.fracWidth-1, 0) // normal
    })
  }

  val int2FpResultMap: Seq[UInt] = fpParamMap.map(fp => Mux1H(result1H.asBools.reverse, int2FpResultMapGen(fp)))
  resultNext := Mux1H(float1HOut, int2FpResultMap)

  val cvtResult = Mux(is32BitsOut && hasSignOut,
    Fill(32, result(31)) ## result(31, 0),
    result
  )
  
  val mvResult = Mux(is32BitsOut, ~0.U(32.W) ## srcOut.tail(32), srcOut)

  io.result :=  Mux(isMvInstOut, mvResult, cvtResult)
  io.fflags :=  Mux(isMvInstOut, 0.U, fflags)
}

