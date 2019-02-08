package core

import chisel3._
import chisel3.util._

import Decode._

object LookupTree {
  private val useMuxTree = true

  def apply[T <: Data](key: UInt, mapping: Iterable[(UInt, T)]): T =
    Mux1H(mapping.map(p => (p._1 === key, p._2)))

  def apply[T <: Data](key: UInt, default: T, mapping: Iterable[(UInt, T)]): T =
    if (useMuxTree) apply(key, mapping) else MuxLookup(key, default, mapping.toSeq)
}

class ALU {
  def access(src1: UInt, src2: UInt, func: UInt): UInt = {
    val shamt = src2(4, 0)
    val funcList = List(
      AluAdd  -> (src1  +  src2),
      AluSll  -> ((src1  << shamt)(31, 0)),
      AluSlt  -> ((src1.asSInt < src2.asSInt).asUInt),
      AluSltu -> ((src1 < src2).asUInt),
      AluXor  -> (src1  ^  src2),
      AluSrl  -> (src1  >> shamt),
      AluOr   -> (src1  |  src2),
      AluAnd  -> (src1  &  src2),
      AluSub  -> (src1  -  src2),
      AluLui  -> src2,
      AluSra  -> ((src1.asSInt >> shamt).asUInt)
    )

    LookupTree(func, 0.U, funcList)
  }
}

class BRU {
  def access(pc: UInt, offset: UInt, src1: UInt, src2: UInt, func: UInt): (UInt, Bool) = {
    val funcList = List(
      BruBeq  -> (src1 === src2),
      BruBne  -> (src1 =/= src2),
      BruBlt  -> (src1.asSInt  <  src2.asSInt),
      BruBge  -> (src1.asSInt >=  src2.asSInt),
      BruJal  -> true.B,
      BruJalr -> true.B
    )

    val target = Mux(func === BruJalr, src1 + src2, pc + offset)
    val isTaken = LookupTree(func, false.B, funcList)
    (target, isTaken)
  }
}

class LSU {
  def access(src1: UInt, src2: UInt, func: UInt): (UInt, Bool) = {
    val funcList = List(
      LsuSw   -> (src1  +  src2)
    )

    val addr = LookupTree(func, 0.U, funcList)
    val wen = func(3)
    (addr, wen)
  }
}

class MDU {
  def access(src1: UInt, src2: UInt, func: UInt): UInt = {
    val mulRes = (src1.asSInt * src2.asSInt).asUInt
    val funcList = List(
      MduMul  -> mulRes(31, 0),
      MduMulh -> mulRes(63, 32),
      MduDiv  -> (src1.asSInt  /  src2.asSInt).asUInt,
      MduRem  -> (src1.asSInt  %  src2.asSInt).asUInt
    )

    LookupTree(func, 0.U, funcList)
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

  val (bruOut, bruIsTaken) = (new BRU).access(pc = io.in.pc, offset = src2,
    src1 = src1, src2 = io.in.data.dest, func = fuOpType)
  io.br.isTaken := (fuType === FuBru) && bruIsTaken
  io.br.target := bruOut

  val (dmemAddr, dmemWen) = (new LSU).access(src1 = src1, src2 = src2, func = fuOpType)
  io.dmem.out.bits.addr := dmemAddr
  io.dmem.out.valid := fuType === FuLsu
  io.dmem.out.bits.wen := (fuType === FuLsu) && dmemWen
  io.dmem.out.bits.wdata := io.in.data.dest

  val mduOut = (new MDU).access(src1 = src1, src2 = src2, func = fuOpType)

  io.out.data := DontCare
  io.out.data.dest := LookupTree(fuType, 0.U, List(
    FuAlu -> aluOut,
    FuBru -> (io.in.pc + 4.U),
    FuLsu -> io.dmem.in.rdata,
    FuMdu -> mduOut
  ))

  io.out.ctrl := DontCare
  (io.out.ctrl, io.in.ctrl) match { case (o, i) =>
    o.rfWen := i.rfWen
    o.rfDest := i.rfDest
  }
  io.out.pc := io.in.pc

  printf("EXU: src1 = 0x%x, src2 = 0x%x\n", src1, src2)
}
