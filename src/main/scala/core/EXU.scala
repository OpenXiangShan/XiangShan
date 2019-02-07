package core

import chisel3._
import chisel3.util._

import Decode._

object LookupTree {
  def apply[T <: Data](key: UInt, mapping: Iterable[(UInt, T)]): T =
    Mux1H(mapping.map(p => (p._1 === key, p._2)))
}

class ALU {
  private val useMuxTree = true
  def access(src1: UInt, src2: UInt, func: UInt): UInt = {
    val shamt = src2(4, 0)
    val funcList = List(
      AluAdd  -> (src1  +  src2),
      AluSll  -> ((src1  << shamt)(31, 0)),
      AluSlt  -> ((src1.asSInt < src2.asSInt).asUInt),
      AluSltu -> ((src1 < src2).asUInt),
      AluXor  -> (src1  ^  src2),
      AluSlr  -> (src1  >> shamt),
      AluOr   -> (src1  |  src2),
      AluAnd  -> (src1  &  src2),
      AluSub  -> (src1  -  src2),
      AluSar  -> ((src1.asSInt >> shamt).asUInt)
    )

    if (useMuxTree) LookupTree(func, funcList)
    else MuxLookup(func, 0.U, funcList)
  }
}

class EXU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new PcCtrlDataIO)
    val out = new PcCtrlDataIO
  })

  val aluOut = (new ALU).access(src1 = io.in.data.src1, src2 = io.in.data.src2, func = io.in.ctrl.fuOpType)

  io.out.data := DontCare
  io.out.data.dest := Mux(io.in.ctrl.fuType === FuAlu, aluOut, 0.U)

  io.out.ctrl := DontCare
  (io.out.ctrl, io.in.ctrl) match { case (o, i) =>
    o.rfWen := i.rfWen
    o.rfDest := i.rfDest
  }
  io.out.pc := io.in.pc
}
