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

  val s_req :: s_resp :: Nil = Enum(2)
  val state = RegInit(s_req)

  val sfence  = WireInit(0.U.asTypeOf(new SfenceBundle))
  val sbuffer = WireInit(false.B)
  val fencei  = WireInit(false.B)
  val sbEmpty = WireInit(false.B)
  BoringUtils.addSource(sbuffer, "FenceUnitSbufferFlush")
  BoringUtils.addSource(sfence, "SfenceBundle")
  BoringUtils.addSource(fencei,  "FenceI")
  BoringUtils.addSink(sbEmpty, "SBufferEmpty")
  // NOTE: icache & tlb & sbuffer must receive flush signal at any time
  sbuffer      := valid && state === s_req && !sbEmpty
  fencei       := valid && state === s_req && func === FenceOpType.fencei
  sfence.valid := valid && state === s_req && func === FenceOpType.sfence
  sfence.bits.rs1  := lsrc1 === 0.U
  sfence.bits.rs2  := lsrc2 === 0.U
  sfence.bits.addr := src1

  switch (state) {
    is (s_req) { // send all the flush at s_req
      when (valid && (!sbEmpty || !io.out.ready)) { state := s_resp }
    }
    is (s_resp) { // wait for sbEmpty if send flush to sbuffer
      when (sbEmpty && io.out.ready) { state := s_req }
    }
  }

  assert(!(io.out.valid && io.out.bits.uop.ctrl.rfWen))
  io.in.ready := state === s_req
  io.out.valid := (state === s_resp && sbEmpty) || (state === s_req && sbEmpty && valid)
  io.out.bits.data := DontCare
  io.out.bits.uop := Mux(state === s_req, uop, RegEnable(uop, io.in.fire()))
  io.out.bits.redirect <> DontCare
  io.out.bits.redirectValid := false.B
  io.out.bits.debug <> DontCare

  XSDebug(valid || state=/=s_req || io.out.valid, p"In(${io.in.valid} ${io.in.ready}) Out(${io.out.valid} ${io.out.ready}) state:${state} sbuffer(flush:${sbuffer} empty:${sbEmpty}) fencei:${fencei} sfence:${sfence} Inpc:0x${Hexadecimal(io.in.bits.uop.cf.pc)} InroqIdx:${io.in.bits.uop.roqIdx} Outpc:0x${Hexadecimal(io.out.bits.uop.cf.pc)} OutroqIdx:${io.out.bits.uop.roqIdx}\n")
}