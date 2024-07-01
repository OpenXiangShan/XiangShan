/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSDebug
import xiangshan.ExceptionNO.{illegalInstr, virtualInstr}
import xiangshan._

class FenceIO(implicit p: Parameters) extends XSBundle {
  val sfence = Output(new SfenceBundle)
  val fencei = Output(Bool())
  val sbuffer = new FenceToSbuffer
}

class FenceToSbuffer extends Bundle {
  val flushSb = Output(Bool())
  val sbIsEmpty = Input(Bool())
}

class Fence(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {

  val sfence = io.fenceio.get.sfence
  val fencei = io.fenceio.get.fencei
  val toSbuffer = io.fenceio.get.sbuffer
  val (valid, src1) = (
    io.in.valid,
    io.in.bits.data.src(0)
  )

  val s_idle :: s_wait :: s_tlb :: s_icache :: s_fence :: s_nofence :: Nil = Enum(6)

  val state = RegInit(s_idle)
  /* fsm
   * s_idle    : init state, send sbflush
   * s_wait  : send sbflush, wait for sbEmpty
   * s_tlb   : flush tlb, just hold one cycle
   * s_icache: flush icache, just hold one cycle
   * s_fence : do nothing, for timing optimiaztion
   * s_nofence: do nothing , for Svinval extension
   */

  val sbuffer = toSbuffer.flushSb
  val sbEmpty = toSbuffer.sbIsEmpty
  val uop = RegEnable(io.in.bits, io.in.fire)
  val func = uop.ctrl.fuOpType

  // NOTE: icache & tlb & sbuffer must receive flush signal at any time
  sbuffer      := state === s_wait
  fencei       := state === s_icache
  sfence.valid := state === s_tlb && (func === FenceOpType.sfence || func === FenceOpType.hfence_v || func === FenceOpType.hfence_g)
  sfence.bits.rs1  := uop.data.imm(4, 0) === 0.U
  sfence.bits.rs2  := uop.data.imm(9, 5) === 0.U
  sfence.bits.flushPipe := uop.ctrl.flushPipe.get
  sfence.bits.hv := func === FenceOpType.hfence_v
  sfence.bits.hg := func === FenceOpType.hfence_g
  sfence.bits.addr := RegEnable(io.in.bits.data.src(0), io.in.fire)
  sfence.bits.id   := RegEnable(io.in.bits.data.src(1), io.in.fire)

  when (state === s_idle && io.in.valid) { state := s_wait }
  when (state === s_wait && func === FenceOpType.fencei && sbEmpty) { state := s_icache }
  when (state === s_wait && ((func === FenceOpType.sfence || func === FenceOpType.hfence_g || func === FenceOpType.hfence_v) && sbEmpty)) { state := s_tlb }
  when (state === s_wait && func === FenceOpType.fence  && sbEmpty) { state := s_fence }
  when (state === s_wait && func === FenceOpType.nofence  && sbEmpty) { state := s_nofence }
  when (state =/= s_idle && state =/= s_wait) { state := s_idle }

  io.in.ready := state === s_idle
  io.out.valid := state =/= s_idle && state =/= s_wait
  io.out.bits.res.data := 0.U
  io.out.bits.ctrl.robIdx := uop.ctrl.robIdx
  io.out.bits.res.pc.get := uop.data.pc.get
  io.out.bits.ctrl.pdest := uop.ctrl.pdest
  io.out.bits.ctrl.flushPipe.get := uop.ctrl.flushPipe.get
  io.out.bits.ctrl.exceptionVec.get := 0.U.asTypeOf(io.out.bits.ctrl.exceptionVec.get)
  io.out.bits.perfDebugInfo := io.in.bits.perfDebugInfo

  XSDebug(io.in.valid, p"In(${io.in.valid} ${io.in.ready}) state:${state} Inpc:0x${Hexadecimal(io.in.bits.data.pc.get)} InrobIdx:${io.in.bits.ctrl.robIdx}\n")
  XSDebug(state =/= s_idle, p"state:${state} sbuffer(flush:${sbuffer} empty:${sbEmpty}) fencei:${fencei} sfence:${sfence}\n")
  XSDebug(io.out.valid, p" Out(${io.out.valid} ${io.out.ready}) state:${state} Outpc:0x${Hexadecimal(io.out.bits.res.pc.get)} OutrobIdx:${io.out.bits.ctrl.robIdx}\n")

  assert(!io.out.valid || io.out.ready, "when fence is out valid, out ready should always be true")
}
