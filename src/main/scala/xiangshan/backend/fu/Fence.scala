package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.FenceOpType

class FenceToSbuffer extends XSBundle {
  val flushSb = Output(Bool())
  val sbIsEmpty = Input(Bool())
}

// class Fence extends FunctionUnit(FuConfig(
  // /*FuType.fence, 1, 0, writeIntRf = false, writeFpRf = false, hasRedirect = false,*/ latency = UncertainLatency()
// )){
class Fence extends FunctionUnit{ // TODO: check it

  val sfence = IO(Output(new SfenceBundle))
  val fencei = IO(Output(Bool()))
  val toSbuffer = IO(new FenceToSbuffer)

  val (valid, src1) = (
    io.in.valid,
    io.in.bits.src(0)
  )

  val s_idle :: s_wait :: s_tlb :: s_icache :: s_fence :: Nil = Enum(5)
  val state = RegInit(s_idle)
  /* fsm
   * s_idle    : init state, send sbflush
   * s_wait  : send sbflush, wait for sbEmpty
   * s_tlb   : flush tlb, just hold one cycle
   * s_icache: flush icache, just hold one cycle
   * s_fence : do nothing, for timing optimiaztion
   */

  val sbuffer = toSbuffer.flushSb
  val sbEmpty = toSbuffer.sbIsEmpty
  val uop = RegEnable(io.in.bits.uop, io.in.fire())
  val func = uop.ctrl.fuOpType
  val lsrc1 = uop.ctrl.lsrc1
  val lsrc2 = uop.ctrl.lsrc2

  // NOTE: icache & tlb & sbuffer must receive flush signal at any time
  sbuffer      := state === s_wait
  fencei       := state === s_icache
  sfence.valid := state === s_tlb
  sfence.bits.rs1  := lsrc1 === 0.U
  sfence.bits.rs2  := lsrc2 === 0.U
  sfence.bits.addr := RegEnable(src1, io.in.fire())

  when (state === s_idle && valid) { state := s_wait }
  when (state === s_wait && func === FenceOpType.fencei && sbEmpty) { state := s_icache }
  when (state === s_wait && func === FenceOpType.sfence && sbEmpty) { state := s_tlb }
  when (state === s_wait && func === FenceOpType.fence  && sbEmpty) { state := s_fence }
  when (state =/= s_idle && state =/= s_wait) { state := s_idle }

  io.in.ready := state === s_idle
  io.out.valid := state =/= s_idle && state =/= s_wait
  io.out.bits.data := DontCare
  io.out.bits.uop := uop

  XSDebug(valid, p"In(${io.in.valid} ${io.in.ready}) state:${state} Inpc:0x${Hexadecimal(io.in.bits.uop.cf.pc)} InroqIdx:${io.in.bits.uop.roqIdx}\n")
  XSDebug(state =/= s_idle, p"state:${state} sbuffer(flush:${sbuffer} empty:${sbEmpty}) fencei:${fencei} sfence:${sfence}\n")
  XSDebug(io.out.valid, p" Out(${io.out.valid} ${io.out.ready}) state:${state} Outpc:0x${Hexadecimal(io.out.bits.uop.cf.pc)} OutroqIdx:${io.out.bits.uop.roqIdx}\n")

  assert(!(io.out.valid && io.out.bits.uop.ctrl.rfWen))
  assert(!io.out.valid || io.out.ready, "when fence is out valid, out ready should always be true")
}