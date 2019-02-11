package noop

import chisel3._
import chisel3.util._

object LookupTree {
  private val useMuxTree = true

  def apply[T <: Data](key: UInt, mapping: Iterable[(UInt, T)]): T =
    Mux1H(mapping.map(p => (p._1 === key, p._2)))

  def apply[T <: Data](key: UInt, default: T, mapping: Iterable[(UInt, T)]): T =
    if (useMuxTree) apply(key, mapping) else MuxLookup(key, default, mapping.toSeq)
}

class EXU extends Module with HasFuType {
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
    FuLsu -> lsu.rdataExt(io.dmem.r.bits.data, io.dmem.a.bits.addr, fuOpType),
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
