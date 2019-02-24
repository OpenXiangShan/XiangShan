package noop

import chisel3._
import chisel3.util._

import memory.MemIO

object LookupTree {
  private val useMuxTree = true

  def apply[T <: Data](key: UInt, mapping: Iterable[(UInt, T)]): T =
    Mux1H(mapping.map(p => (p._1 === key, p._2)))

  def apply[T <: Data](key: UInt, default: T, mapping: Iterable[(UInt, T)]): T =
    if (useMuxTree) apply(key, mapping) else MuxLookup(key, default, mapping.toSeq)
}

class EXU extends Module with HasFuType {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new PcCtrlDataIO))
    val out = Valid((new PcCtrlDataIO))
    val br = new BranchIO
    val dmem = new MemIO
    val csrCtrl = new Bundle {
      val instrCommit = Input(Bool())
    }
  })

  val (src1, src2, fuType, fuOpType) = (io.in.bits.data.src1, io.in.bits.data.src2,
    io.in.bits.ctrl.fuType, io.in.bits.ctrl.fuOpType)

  val alu = Module(new ALU)
  val aluOut = alu.access(valid = (fuType === FuAlu), src1 = src1, src2 = src2, func = fuOpType)
  alu.io.out.ready := true.B

  val bru = Module(new BRU)
  val bruOut = bru.access(valid = (fuType === FuBru), src1 = src1, src2 = io.in.bits.data.dest, func = fuOpType)
  bru.io.pc := io.in.bits.pc
  bru.io.offset := src2
  bru.io.out.ready := true.B

  val lsu = Module(new LSU)
  val lsuOut = lsu.access(valid = (fuType === FuLsu), src1 = src1, src2 = src2, func = fuOpType)
  lsu.io.wdata := io.in.bits.data.dest
  io.dmem <> lsu.io.dmem
  lsu.io.out.ready := true.B

  val mdu = Module(new MDU)
  val mduOut = mdu.access(valid = (fuType === FuMdu), src1 = src1, src2 = src2, func = fuOpType)
  mdu.io.out.ready := true.B

  val csr = Module(new CSR)
  val csrOut = csr.access(valid = (fuType === FuCsr), src1 = src1, src2 = src2, func = fuOpType)
  csr.io.pc := io.in.bits.pc
  csr.io.isException := (io.in.bits.ctrl.isInvOpcode)
  csr.io.exceptionNO := Mux(io.in.bits.ctrl.isInvOpcode, 2.U, 0.U)
  csr.io.out.ready := true.B

  io.out.bits.data := DontCare
  io.out.bits.data.dest := LookupTree(fuType, 0.U, List(
    FuAlu -> aluOut,
    FuBru -> bruOut,
    FuLsu -> lsuOut,
    FuCsr -> csrOut,
    FuMdu -> mduOut
  ))

  when (csr.io.csrjmp.isTaken) { io.br <> csr.io.csrjmp }
  .otherwise { io.br <> bru.io.branch }

  io.out.bits.ctrl := DontCare
  (io.out.bits.ctrl, io.in.bits.ctrl) match { case (o, i) =>
    o.rfWen := i.rfWen
    o.rfDest := i.rfDest
  }
  io.out.bits.pc := io.in.bits.pc
  io.out.valid := io.in.valid && MuxLookup(fuType, true.B, List(
    FuLsu -> lsu.io.out.valid,
    FuMdu -> mdu.io.out.valid
  ))

  csr.io.instrCommit := io.csrCtrl.instrCommit
}
