package core

import chisel3._
import chisel3.util._

import Decode._

class IDU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new PcInstrIO)
    val out = new PcCtrlDataIO
  })

  val instr = io.in.instr
  val instrType :: fuType :: fuOpType :: Nil = ListLookup(instr, DecodeDefault, DecodeTable)

  io.out.ctrl.fuType := fuType
  io.out.ctrl.fuOpType := fuOpType
  io.out.ctrl.src1Type := LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._1)))
  io.out.ctrl.src2Type := LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._2)))
  io.out.ctrl.rfSrc1 := instr(19, 15)
  io.out.ctrl.rfSrc2 := instr(24, 20)
  io.out.ctrl.rfWen := isrfWen(instrType)
  io.out.ctrl.rfDest := Mux(isrfWen(instrType), instr(11, 7), instr(24, 20))

  io.out.data.src1 := DontCare
  io.out.data.src2 := LookupTree(instrType, List(
    InstrI -> Cat(Fill(20, instr(31)), instr(31, 20)),
    InstrS -> Cat(Fill(20, instr(31)), instr(31, 25), instr(11, 7)),
    InstrB -> Cat(Fill(20, instr(31)), instr(7), instr(30, 25), instr(11, 6), 0.U(1.W)),
    InstrU -> Cat(instr(31, 12), 0.U(12.W)),
    InstrJ -> Cat(Fill(12, instr(31)), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W))
  ))
  io.out.data.dest := DontCare

  io.out.pc := io.in.pc

  io.out.ctrl.isTrap := Cat(instrType === InstrN, instr === TRAP)
}
