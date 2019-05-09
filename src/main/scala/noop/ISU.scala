package noop

import chisel3._
import chisel3.util._

import utils.DiffTestIO

class RegFile {
  val rf = Mem(32, UInt(32.W))
  def read(addr: UInt) : UInt = Mux(addr === 0.U, 0.U, rf(addr))
  def write(addr: UInt, data: UInt) = { rf(addr) := data }
}

class ISU extends Module with HasSrcType {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new PcCtrlDataIO))
    val out = Valid(new PcCtrlDataIO)
    val wb = Flipped(new WriteBackIO)
    val trap = Output(UInt(2.W))
    val difftestRegs = Output(Vec(32, UInt(32.W)))
  })

  val rf = new RegFile
  val rs1Data = rf.read(io.in.bits.ctrl.rfSrc1)
  val rs2Data = rf.read(io.in.bits.ctrl.rfSrc2)
  io.out.bits.data.src1 := Mux(io.in.bits.ctrl.src1Type === Src1Pc, io.in.bits.pc, rs1Data)
  io.out.bits.data.src2 := Mux(io.in.bits.ctrl.src2Type === Src2Reg, rs2Data, io.in.bits.data.src2)
  io.out.bits.data.dest := rs2Data // for S-type and B-type

  when (io.wb.rfWen) { rf.write(io.wb.rfDest, io.wb.rfWdata) }

  io.out.bits.ctrl := DontCare
  (io.out.bits.ctrl, io.in.bits.ctrl) match { case (o, i) =>
    o.fuType := i.fuType
    o.fuOpType := i.fuOpType
    o.rfWen := i.rfWen
    o.rfDest := i.rfDest
    o.isInvOpcode := i.isInvOpcode
  }
  io.out.bits.pc := io.in.bits.pc
  io.out.valid := io.in.valid

  io.trap := //Mux(io.in.bits.ctrl.isInvOpcode, NOOPTrap.StateInvOpcode,
              Mux(io.in.bits.ctrl.isNoopTrap,
                Mux(rs1Data === 0.U, NOOPTrap.StateGoodTrap, NOOPTrap.StateBadTrap),
                NOOPTrap.StateRunning) //)

  io.difftestRegs.zipWithIndex.map{ case (r, i) => r := rf.read(i.U) }
}
