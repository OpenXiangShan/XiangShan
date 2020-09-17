package xiangshan.backend.fu.fpu.divsqrt


import chisel3._
import chisel3.util._
import utils._
import xiangshan.backend.fu.fpu._

class SrtTable extends Module {
  val io = IO(new Bundle() {
    val d = Input(UInt(3.W))
    val y = Input(UInt(8.W))
    val q = Output(SInt(3.W))
  })
  val qSelTable = Array(
    Array(12, 4, -4, -13),
    Array(14, 4, -5, -14),
    Array(16, 4, -6, -16),
    Array(16, 4, -6, -17),
    Array(18, 6, -6, -18),
    Array(20, 6, -8, -20),
    Array(20, 8, -8, -22),
    Array(24, 8, -8, -23)
  ).map(_.map(_ * 2))

  var ge = Map[Int, Bool]()
  for(row <- qSelTable){
    for(k <- row){
      if(!ge.contains(k)) ge = ge + (k -> (io.y.asSInt() >= k.S(8.W)))
    }
  }
  io.q := MuxLookup(io.d, 0.S,
    qSelTable.map(x =>
      MuxCase((-2).S(3.W), Seq(
        ge(x(0)) -> 2.S(3.W),
        ge(x(1)) -> 1.S(3.W),
        ge(x(2)) -> 0.S(3.W),
        ge(x(3)) -> (-1).S(3.W)
      ))
    ).zipWithIndex.map({case(v, i) => i.U -> v})
  )
}
