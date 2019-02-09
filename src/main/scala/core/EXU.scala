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
    LookupTree(func, 0.U, List(
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
    ))
  }
}

class BRU {
  def access(isBru: Bool, pc: UInt, offset: UInt, src1: UInt, src2: UInt, func: UInt): BranchIO = {
    val branch = Wire(new BranchIO)
    branch.target := Mux(func === BruJalr, src1 + offset, pc + offset)
    branch.isTaken := isBru && LookupTree(func, false.B, List(
      BruBeq  -> (src1 === src2),
      BruBne  -> (src1 =/= src2),
      BruBlt  -> (src1.asSInt  <  src2.asSInt),
      BruBge  -> (src1.asSInt >=  src2.asSInt),
      BruBltu -> (src1  <  src2),
      BruBgeu -> (src1  >= src2),
      BruJal  -> true.B,
      BruJalr -> true.B
    ))
    branch
  }
}

class LSU {
  def access(isLsu: Bool, base: UInt, offset: UInt, func: UInt, wdata: UInt): MemIO = {
    val dmem = Wire(new MemIO)
    dmem.out.bits.addr := base + offset
    dmem.out.valid := isLsu
    dmem.out.bits.wen := isLsu && func(3)
    dmem.out.bits.size := func(1, 0)
    dmem.out.bits.wdata := wdata
    dmem
  }
  def rdataExt(rdata: UInt, func: UInt): UInt = {
    LookupTree(func, rdata, List(
      LsuLb   -> Cat(Fill(24, rdata(7)), rdata(7, 0)),
      LsuLh   -> Cat(Fill(16, rdata(15)), rdata(15, 0)),
      LsuLw   -> rdata,
      LsuLbu  -> Cat(0.U(24.W), rdata(7, 0)),
      LsuLhu  -> Cat(0.U(16.W), rdata(15, 0))
    ))
  }
}

class MDU {
  def access(src1: UInt, src2: UInt, func: UInt): UInt = {
    val mulRes = (src1.asSInt * src2.asSInt).asUInt
    LookupTree(func, 0.U, List(
      MduMul  -> mulRes(31, 0),
      MduMulh -> mulRes(63, 32),
      MduDiv  -> (src1.asSInt  /  src2.asSInt).asUInt,
      MduDivu -> (src1  /  src2),
      MduRem  -> (src1.asSInt  %  src2.asSInt).asUInt,
      MduRemu -> (src1  %  src2)
    ))
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

  val bruOut = (new BRU).access(isBru = fuType === FuBru, pc = io.in.pc, offset = src2,
    src1 = src1, src2 = io.in.data.dest, func = fuOpType)

  val lsu = new LSU
  io.dmem <> lsu.access(isLsu = fuType === FuLsu, base = src1, offset = src2,
    func = fuOpType, wdata = io.in.data.dest)

  val mduOut = (new MDU).access(src1 = src1, src2 = src2, func = fuOpType)

  val csr = new CSR
  val csrOut = csr.access(isCsr = fuType === FuCsr, addr = src2(11, 0), src = src1, cmd = fuOpType)
  val exceptionJmp = csr.jmp(isCsr = fuType === FuCsr, addr = src2(11, 0), pc = io.in.pc, cmd = fuOpType)

  io.out.data := DontCare
  io.out.data.dest := LookupTree(fuType, 0.U, List(
    FuAlu -> aluOut,
    FuBru -> (io.in.pc + 4.U),
    FuLsu -> lsu.rdataExt(io.dmem.in.rdata, fuOpType),
    FuCsr -> csrOut,
    FuMdu -> mduOut
  ))

  when (exceptionJmp.isTaken) { io.br <> exceptionJmp }
  .otherwise { io.br <> bruOut }

  io.out.ctrl := DontCare
  (io.out.ctrl, io.in.ctrl) match { case (o, i) =>
    o.rfWen := i.rfWen
    o.rfDest := i.rfDest
  }
  io.out.pc := io.in.pc

  //printf("EXU: src1 = 0x%x, src2 = 0x%x\n", src1, src2)
}
