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

class BRU {
  private val useMuxTree = true
  def access(src1: UInt, src2: UInt, func: UInt): (UInt, Bool) = {
    val funcList = List(
      BruJal  -> (src1  +  src2),
      BruJalr -> (src1  +  src2)
    )

    val target = (if (useMuxTree) LookupTree(func, funcList)
                  else MuxLookup(func, 0.U, funcList))
    val isTaken = func(3)
    (target, isTaken)
  }
}

class LSU {
  private val useMuxTree = true
  def access(src1: UInt, src2: UInt, func: UInt): (UInt, Bool) = {
    val funcList = List(
      LsuSw   -> (src1  +  src2)
    )

    val addr = (if (useMuxTree) LookupTree(func, funcList)
                else MuxLookup(func, 0.U, funcList))
    val wen = func(3)
    (addr, wen)
  }
}

class EXU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new PcCtrlDataIO)
    val out = new PcCtrlDataIO
    val br = new BranchIO
    val dmem = new MemIO
  })

  val (src1, src2, fuType, fuOpType) = (io.in.data.src1, io.in.data.src2, io.in.ctrl.fuType, io.in.ctrl.fuOpType)
  val aluOut = (new ALU).access(src1 = src1, src2 = src2, func = fuOpType)

  val (bruOut, bruIsTaken) = (new BRU).access(src1 = src1, src2 = src2, func = fuOpType)
  io.br.isTaken := (fuType === FuBru) && bruIsTaken
  io.br.target := bruOut

  val (dmemAddr, dmemWen) = (new LSU).access(src1 = src1, src2 = src2, func = fuOpType)
  io.dmem.out.bits.addr := dmemAddr
  io.dmem.out.valid := fuType === FuLsu
  io.dmem.out.bits.wen := (fuType === FuLsu) && dmemWen
  io.dmem.out.bits.wdata := io.in.data.dest

  io.out.data := DontCare
  io.out.data.dest := Mux(fuType === FuAlu, aluOut,
                        Mux(fuType === FuBru, io.in.pc + 4.U,
                          Mux(fuType === FuLsu, io.dmem.in.rdata, 0.U)))

  io.out.ctrl := DontCare
  (io.out.ctrl, io.in.ctrl) match { case (o, i) =>
    o.rfWen := i.rfWen
    o.rfDest := i.rfDest
  }
  io.out.pc := io.in.pc
}
