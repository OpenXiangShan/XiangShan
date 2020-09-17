package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._

class FMV(XLEN: Int) extends FPUSubModule with HasPipelineReg {

  def latency = 1

  val isDouble = io.in.bits.isDouble
  val op = io.in.bits.op
  val src = Seq(io.in.bits.a, io.in.bits.b).map(x =>
    Mux(isDouble || op(2,1)==="b00".U, x, extF32ToF64(x))
  )
  val aSign = Mux(op(2,1)==="b00".U && !isDouble, src(0)(31), src(0)(63))
  val bSign = Mux(op(2,1)==="b00".U && !isDouble, src(1)(31), src(1)(63))
  val sgnjSign = Mux(op(1),
    bSign,
    Mux(op(0), !bSign, aSign ^ bSign)
  )
  val resSign = Mux(op(2), sgnjSign, aSign)

  val cls = Module(new Classify(Float64.expWidth, Float64.mantWidth)).io
  cls.in := src(0)

  val classifyResult = Cat(
    cls.isQNaN,            // 9
    cls.isSNaN,            // 8
    cls.isPosInf,          // 7
    cls.isPosNormal,       // 6
    cls.isPosSubnormal,    // 5
    cls.isPosZero,         // 4
    cls.isNegZero,         // 3
    cls.isNegSubnormal,    // 2
    cls.isNegNormal,       // 1
    cls.isNegInf           // 0
  )

  val result = Mux(op === "b010".U,
    classifyResult,
    Mux(isDouble,
      Cat(resSign, io.in.bits.a(62, 0)),
      Cat(resSign, io.in.bits.a(30 ,0))
    )
  )
  val resultReg = S1Reg(result)

  io.out.bits.result := resultReg
  io.out.bits.fflags := 0.U.asTypeOf(new Fflags)
}
