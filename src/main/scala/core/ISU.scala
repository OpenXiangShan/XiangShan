package core

import chisel3._
import chisel3.util._

class RegFile {
  val rf = Mem(32, UInt(32.W))
  def read(addr: UInt) : UInt = Mux(addr === 0.U, 0.U, rf(addr))
  def write(addr: UInt, data: UInt) = { rf(addr) := data }
}

class ISU extends Module with HasSrcType {
  val io = IO(new Bundle {
    val in = Flipped(new PcCtrlDataIO)
    val out = new PcCtrlDataIO
    val wb = Flipped(new WriteBackIO)
    val trap = Output(UInt(2.W))
  })

  val rf = new RegFile
  val rs1Data = rf.read(io.in.ctrl.rfSrc1)
  val rs2Data = rf.read(io.in.ctrl.rfSrc2)
  io.out.data.src1 := Mux(io.in.ctrl.src1Type === Src1Pc, io.in.pc, rs1Data)
  io.out.data.src2 := Mux(io.in.ctrl.src2Type === Src2Reg, rs2Data, io.in.data.src2)
  io.out.data.dest := rs2Data // for S-type and B-type

  when (io.wb.rfWen) { rf.write(io.wb.rfDest, io.wb.rfWdata) }

  io.out.ctrl := DontCare
  (io.out.ctrl, io.in.ctrl) match { case (o, i) =>
    o.fuType := i.fuType
    o.fuOpType := i.fuOpType
    o.rfWen := i.rfWen
    o.rfDest := i.rfDest
  }
  io.out.pc := io.in.pc

  io.trap := Mux(io.in.ctrl.isTrap(1), 2.U, Mux(io.in.ctrl.isTrap(0), Mux(rs1Data === 0.U, 0.U, 1.U), 3.U))
}
