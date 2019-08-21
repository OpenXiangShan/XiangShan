package noop

import chisel3._
import chisel3.util._

import utils._
import bus.simplebus.SimpleBus

class EXU extends Module with HasFuType {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new PcCtrlDataIO))
    val out = Decoupled(new CommitIO)
    val flush = Input(Bool())
    val csrjmp = Flipped(new BranchIO)
    val dmem = new SimpleBus
    val mmio = new SimpleBus
    val forward = new ForwardIO
    val wbData = Input(UInt(32.W))
    val bpu1Update = Output(new BRUIO)
    val csr = new Bundle {
      val isCsr = Output(Bool())
      val in = Flipped(Decoupled(UInt(32.W)))
      val instrType = Vec(FuTypeNum, Output(Bool()))
      val isMul = Output(Bool())
    }
  })

  val src1 = io.in.bits.data.src1
  val src2 = io.in.bits.data.src2

  val (fuType, fuOpType) = (io.in.bits.ctrl.fuType, io.in.bits.ctrl.fuOpType)

  val fuValids = Wire(Vec(FuTypeNum, Bool()))
  (0 until FuTypeNum).map (i => fuValids(i) := (fuType === i.U) && io.in.valid && !io.flush)

  val alu = Module(new ALU)
  val aluOut = alu.access(valid = fuValids(FuAlu), src1 = src1, src2 = src2, func = fuOpType)
  alu.io.out.ready := true.B

  val bru = Module(new BRU)
  val bruOut = bru.access(valid = fuValids(FuBru), src1 = src1, src2 = src2, func = fuOpType)
  bru.io.pc := io.in.bits.pc
  bru.io.offset := io.in.bits.data.imm
  bru.io.predictTaken := io.in.bits.isBranchTaken
  io.out.bits.br <> Mux(io.csrjmp.isTaken, io.csrjmp, bru.io.branch)
  bru.io.out.ready := true.B
  io.bpu1Update := bru.io

  val lsu = Module(new LSU)
  val lsuOut = lsu.access(valid = fuValids(FuLsu), src1 = src1, src2 = io.in.bits.data.imm, func = fuOpType)
  lsu.io.wdata := src2
  io.out.bits.isMMIO := lsu.io.isMMIO
  io.dmem <> lsu.io.dmem
  io.mmio <> lsu.io.mmio
  lsu.io.out.ready := true.B

  val mdu = Module(new MDU)
  val mduOut = mdu.access(valid = fuValids(FuMdu), src1 = src1, src2 = src2, func = fuOpType)
  mdu.io.out.ready := true.B

  // CSR is instantiated under NOOP
  io.csr.isCsr := fuValids(FuCsr)
  io.csr.in.ready := true.B

  io.out.bits.ctrl := DontCare
  (io.out.bits.ctrl, io.in.bits.ctrl) match { case (o, i) =>
    o.rfWen := i.rfWen
    o.rfDest := i.rfDest
    o.fuType := i.fuType
  }
  io.out.bits.pc := io.in.bits.pc
  // FIXME: should handle io.out.ready == false
  io.out.valid := io.in.valid && MuxLookup(fuType, true.B, List(
    FuLsu -> lsu.io.out.valid,
    FuMdu -> mdu.io.out.valid
  ))

  io.out.bits.commits := DontCare
  io.out.bits.commits(FuAlu).rfWdata := aluOut
  io.out.bits.commits(FuBru).rfWdata := bruOut
  io.out.bits.commits(FuLsu).rfWdata := lsuOut
  io.out.bits.commits(FuCsr).rfWdata := io.csr.in.bits
  io.out.bits.commits(FuMdu).rfWdata := mduOut

  io.in.ready := !io.in.valid || io.out.fire()

  io.forward.valid := io.in.valid
  io.forward.rfWen := io.in.bits.ctrl.rfWen
  io.forward.rfDest := io.in.bits.ctrl.rfDest
  io.forward.fuType := io.in.bits.ctrl.fuType
  io.forward.rfData := Mux(alu.io.out.fire(), aluOut, lsuOut)

  // perfcnt
  io.csr.instrType(FuAlu) := alu.io.out.fire()
  io.csr.instrType(FuBru) := bru.io.out.fire()
  io.csr.instrType(FuLsu) := lsu.io.out.fire()
  io.csr.instrType(FuMdu) := mdu.io.out.fire()
  io.csr.instrType(FuCsr) := io.csr.isCsr && io.csr.in.ready
  io.csr.isMul := mdu.io.isMul
}
