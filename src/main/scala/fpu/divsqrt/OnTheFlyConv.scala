package fpu.divsqrt

import chisel3._
import chisel3.util._
import utils._
import fpu._
import fpu.util.FPUDebug

class OnTheFlyConv(len: Int) extends Module {
  val io = IO(new Bundle() {
    val resetSqrt = Input(Bool())
    val resetDiv = Input(Bool())
    val enable = Input(Bool())
    val qi = Input(SInt(3.W))
    val QM = Output(UInt(len.W))
    val Q = Output(UInt(len.W))
    val F = Output(UInt(len.W))
  })
  val Q, QM = Reg(UInt(len.W))

  /**  FGen:
    *  use additional regs to avoid
    *  big width shifter since FGen is in cirtical path
    */
  val mask = Reg(SInt(len.W))
  val b_111, b_1100 = Reg(UInt(len.W))
  when(io.resetSqrt){
    mask := Cat("b1".U(1.W), 0.U((len-1).W)).asSInt()
    b_111 := "b111".U(3.W) << (len-5)
    b_1100 := "b1100".U(4.W) << (len-5)
  }.elsewhen(io.enable){
    mask := mask >> 2
    b_111 := b_111 >> 2
    b_1100 := b_1100 >> 2
  }
  val b_00, b_01, b_10, b_11 = Reg(UInt((len-3).W))
  b_00 := 0.U
  when(io.resetDiv || io.resetSqrt){
    b_01 := Cat("b01".U(2.W), 0.U((len-5).W))
    b_10 := Cat("b10".U(2.W), 0.U((len-5).W))
    b_11 := Cat("b11".U(2.W), 0.U((len-5).W))
  }.elsewhen(io.enable){
    b_01 := b_01 >> 2
    b_10 := b_10 >> 2
    b_11 := b_11 >> 2
  }

  val negQ = ~Q
  val sqrtToCsaMap = Seq(
    1 -> (negQ, b_111),
    2 -> (negQ, b_1100),
    -1 -> (QM, b_111),
    -2 -> (QM, b_1100)
  ).map(
    m => m._1.S(3.W).asUInt() ->
      ( ((m._2._1 << Mux(io.qi(0), 1.U, 2.U)).asUInt() & (mask >> io.qi(0)).asUInt()) | m._2._2 )
  )
  val sqrtToCsa = MuxLookup(io.qi.asUInt(), 0.U, sqrtToCsaMap)

  val Q_load_00 = Q | b_00
  val Q_load_01 = Q | b_01
  val Q_load_10 = Q | b_10
  val QM_load_01 = QM | b_01
  val QM_load_10 = QM | b_10
  val QM_load_11 = QM | b_11

  when(io.resetSqrt){
    Q := Cat(1.U(3.W), 0.U((len-3).W))
    QM := 0.U
  }.elsewhen(io.resetDiv){
    Q := 0.U
    QM := 0.U
  }.elsewhen(io.enable){
    val QConvMap = Seq(
      0 -> Q_load_00,
      1 -> Q_load_01,
      2 -> Q_load_10,
      -1 -> QM_load_11,
      -2 -> QM_load_10
    ).map(m => m._1.S(3.W).asUInt() -> m._2)
    val QMConvMap = Seq(
      0 -> QM_load_11,
      1 -> Q_load_00,
      2 -> Q_load_01,
      -1 -> QM_load_10,
      -2 -> QM_load_01
    ).map(m => m._1.S(3.W).asUInt() -> m._2)
    Q := MuxLookup(io.qi.asUInt(), DontCare, QConvMap)
    QM := MuxLookup(io.qi.asUInt(), DontCare, QMConvMap)
  }

  io.F := sqrtToCsa
  io.QM := QM
  io.Q := Q

  FPUDebug(){
    when(io.enable){
      printf(p"[on the fly conv] q:${io.qi} A:${Binary(Q)} B:${Binary(QM)} \n")
    }
  }
}
