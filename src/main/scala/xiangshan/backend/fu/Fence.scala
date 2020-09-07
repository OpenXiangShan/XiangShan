package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils

import xiangshan.backend.FenceOpType

class FenceExeUnit extends Exu(Exu.fenceExeUnitCfg) {
  val (valid, src1, src2, uop, func, lsrc1, lsrc2) = 
    (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.uop, io.in.bits.uop.ctrl.fuOpType, io.in.bits.uop.ctrl.lsrc1, io.in.bits.uop.ctrl.lsrc2)

  val s_sb :: s_tlb :: s_icache :: Nil = Enum(3)
  val state = RegInit(s_sb)

  val sfence  = WireInit(0.U.asTypeOf(new SfenceBundle))
  val sbuffer = WireInit(false.B)
  val fencei  = WireInit(false.B)
  val sbEmpty = WireInit(false.B)
  BoringUtils.addSource(sbuffer, "FenceUnitSbufferFlush")
  BoringUtils.addSource(sfence, "SfenceBundle")
  BoringUtils.addSource(fencei,  "FenceI")
  BoringUtils.addSink(sbEmpty, "SBufferEmpty")
  // NOTE: icache & tlb & sbuffer must receive flush signal at any time
  sbuffer      := valid && state === s_sb && !sbEmpty
  fencei       := (state === s_icache && sbEmpty) || (state === s_sb && valid && sbEmpty && func === FenceOpType.fencei)
  sfence.valid := (state === s_tlb && sbEmpty) || (state === s_sb && valid && sbEmpty && func === FenceOpType.sfence)
  sfence.bits.rs1  := Mux(state === s_sb, lsrc1 === 0.U, RegEnable(lsrc1 === 0.U, io.in.fire()))
  sfence.bits.rs2  := Mux(state === s_sb, lsrc2 === 0.U, RegEnable(lsrc2 === 0.U, io.in.fire()))
  sfence.bits.addr := Mux(state === s_sb, src1,          RegEnable(src1, io.in.fire()))

  when (state === s_sb && valid && func === FenceOpType.fencei && !sbEmpty) { state := s_icache }
  when (state === s_sb && valid && func === FenceOpType.sfence && !sbEmpty) { state := s_tlb }
  when (state =/= s_sb && sbEmpty) { state := s_sb } 

  assert(!(io.out.valid && io.out.bits.uop.ctrl.rfWen))
  io.in.ready := state === s_sb
  io.out.valid := (state =/= s_sb && sbEmpty) || (state === s_sb && sbEmpty && valid)
  io.out.bits.data := DontCare
  io.out.bits.uop := Mux(state === s_sb, uop, RegEnable(uop, io.in.fire()))
  io.out.bits.redirect <> DontCare
  io.out.bits.redirectValid := false.B
  io.out.bits.debug <> DontCare

  assert(!(valid || state =/= s_sb) || io.out.ready) // NOTE: fence instr must be the first(only one) instr, so io.out.ready must be true

  XSDebug(valid || state=/=s_sb || io.out.valid, p"In(${io.in.valid} ${io.in.ready}) Out(${io.out.valid} ${io.out.ready}) state:${state} sbuffer(flush:${sbuffer} empty:${sbEmpty}) fencei:${fencei} sfence:${sfence} Inpc:0x${Hexadecimal(io.in.bits.uop.cf.pc)} InroqIdx:${io.in.bits.uop.roqIdx} Outpc:0x${Hexadecimal(io.out.bits.uop.cf.pc)} OutroqIdx:${io.out.bits.uop.roqIdx}\n")
}