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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import utils._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan._
import xiangshan.v2backend.FuConfig
import xiangshan.v2backend.fu.FuncUnit

class FenceToSbuffer extends Bundle {
  val flushSb = Output(Bool())
  val sbIsEmpty = Input(Bool())
}

class Fence(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {

  val sfence = io.fenceio.get.sfence
  val fencei = io.fenceio.get.fencei
  val toSbuffer = io.fenceio.get.sbuffer
  val disableSfence = io.fenceio.get.disableSfence

  val (valid, src1) = (
    io.in.valid,
    io.in.bits.src(0)
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
  val func = uop.fuOpType

  // NOTE: icache & tlb & sbuffer must receive flush signal at any time
  sbuffer      := state === s_wait && !(func === FenceOpType.sfence && disableSfence)
  fencei       := state === s_icache
  sfence.valid := state === s_tlb && !disableSfence
  sfence.bits.rs1  := uop.imm(4, 0) === 0.U
  sfence.bits.rs2  := uop.imm(9, 5) === 0.U
  sfence.bits.flushPipe := uop.flushPipe.get
//  XSError(sfence.valid && uop.lsrc(0) =/= uop.imm(4, 0), "lsrc0 is passed by imm\n")
//  XSError(sfence.valid && uop.lsrc(1) =/= uop.imm(9, 5), "lsrc1 is passed by imm\n")
  sfence.bits.addr := RegEnable(io.in.bits.src(0), io.in.fire())
  sfence.bits.asid := RegEnable(io.in.bits.src(1), io.in.fire())

  when (state === s_idle && io.in.valid) { state := s_wait }
  when (state === s_wait && func === FenceOpType.fencei && sbEmpty) { state := s_icache }
  when (state === s_wait && func === FenceOpType.sfence && (sbEmpty || disableSfence)) { state := s_tlb }
  when (state === s_wait && func === FenceOpType.fence  && sbEmpty) { state := s_fence }
  when (state === s_wait && func === FenceOpType.nofence  && sbEmpty) { state := s_nofence }
  when (state =/= s_idle && state =/= s_wait) { state := s_idle }

  io.in.ready := state === s_idle
  io.out.valid := state =/= s_idle && state =/= s_wait
  io.out.bits.data := 0.U
  io.out.bits.robIdx := uop.robIdx
  io.out.bits.pc.get := uop.pc.get
  io.out.bits.pdest := uop.pdest
  io.out.bits.flushPipe.get := uop.flushPipe.get
  io.out.bits.exceptionVec.get := 0.U.asTypeOf(io.out.bits.exceptionVec.get)
  io.out.bits.exceptionVec.get(illegalInstr) := func === FenceOpType.sfence && disableSfence

  XSDebug(io.in.valid, p"In(${io.in.valid} ${io.in.ready}) state:${state} Inpc:0x${Hexadecimal(io.in.bits.pc.get)} InrobIdx:${io.in.bits.robIdx}\n")
  XSDebug(state =/= s_idle, p"state:${state} sbuffer(flush:${sbuffer} empty:${sbEmpty}) fencei:${fencei} sfence:${sfence}\n")
  XSDebug(io.out.valid, p" Out(${io.out.valid} ${io.out.ready}) state:${state} Outpc:0x${Hexadecimal(io.out.bits.pc.get)} OutrobIdx:${io.out.bits.robIdx}\n")

  // assert(!(io.out.valid && io.out.bits.uop.ctrl.rfWen))
  assert(!io.out.valid || io.out.ready, "when fence is out valid, out ready should always be true")
}
