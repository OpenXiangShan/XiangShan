package noop

import chisel3._
import chisel3.util._

import utils._

class IDU(implicit val p: NOOPConfig) extends Module with HasInstrType {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new PcInstrIO))
    val out = Decoupled(new PcCtrlDataIO)
  })

  val instr = io.in.bits.instr
  val instrType :: fuType :: fuOpType :: Nil =
    ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)

  io.out.bits := DontCare

  io.out.bits.ctrl.fuType := fuType
  io.out.bits.ctrl.fuOpType := fuOpType

  val SrcTypeTable = List(
    InstrI -> (SrcType.reg, SrcType.imm),
    InstrR -> (SrcType.reg, SrcType.reg),
    InstrS -> (SrcType.reg, SrcType.reg),
    InstrB -> (SrcType.reg, SrcType.reg),
    InstrU -> (SrcType.pc , SrcType.imm),
    InstrJ -> (SrcType.pc , SrcType.imm),
    InstrN -> (SrcType.pc , SrcType.imm)
  )
  val src1Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._1)))
  val src2Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._2)))

  val (rs, rt, rd) = (instr(19, 15), instr(24, 20), instr(11, 7))
  // make non-register addressing to zero, since isu.sb.isBusy(0) === false.B
  io.out.bits.ctrl.rfSrc1 := Mux(src1Type === SrcType.pc, 0.U, rs)
  io.out.bits.ctrl.rfSrc2 := Mux(src2Type === SrcType.reg, rt, 0.U)
  io.out.bits.ctrl.rfWen := isrfWen(instrType)
  io.out.bits.ctrl.rfDest := Mux(isrfWen(instrType), rd, 0.U)

  io.out.bits.data := DontCare
  io.out.bits.data.imm  := LookupTree(instrType, List(
    InstrI -> Cat(Fill(20, instr(31)), instr(31, 20)),
    InstrS -> Cat(Fill(20, instr(31)), instr(31, 25), instr(11, 7)),
    InstrB -> Cat(Fill(20, instr(31)), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)),
    InstrU -> Cat(instr(31, 12), 0.U(12.W)),
    InstrJ -> Cat(Fill(12, instr(31)), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W))
  ))

  when (fuType === FuType.alu) {
    when (rd === 1.U && fuOpType === BRUOpType.jal) { io.out.bits.ctrl.fuOpType := BRUOpType.call }
    when (rs === 1.U && fuOpType === BRUOpType.jalr) { io.out.bits.ctrl.fuOpType := BRUOpType.ret }
  }
  // fix LUI
  io.out.bits.ctrl.src1Type := Mux(instr(6,0) === "b0110111".U, SrcType.reg, src1Type)
  io.out.bits.ctrl.src2Type := src2Type

  io.out.bits.pc := io.in.bits.pc
  io.out.bits.npc := io.in.bits.npc

  io.out.bits.ctrl.isInvOpcode := (instrType === InstrN) && io.in.valid
  io.out.bits.ctrl.isNoopTrap := (instr === NOOPTrap.TRAP) && io.in.valid
  io.out.valid := io.in.valid

  io.in.ready := !io.in.valid || io.out.fire()
}
