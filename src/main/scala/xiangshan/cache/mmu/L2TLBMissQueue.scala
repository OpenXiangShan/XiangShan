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

package xiangshan.cache.mmu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._

/* Miss Queue dont care about duplicate req, which is done by PtwFilter
 * PtwMissQueue is just a Queue inside Chisel with flush
 */
class PtwMissQueue(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val vpn = UInt(vpnLen.W)
      val source = UInt(bPtwWidth.W)
    }))
    val sfence = Input(new SfenceBundle)
    val out = Decoupled(new Bundle {
      val vpn = UInt(vpnLen.W)
      val source = UInt(bPtwWidth.W)
    })
    val empty = Output(Bool())
  })

  val vpn = Reg(Vec(MSHRSize, UInt(vpnLen.W))) // request vpn
  val source = Reg(Vec(MSHRSize, UInt(bPtwWidth.W))) // is itlb
  val enqPtr = RegInit(0.U(log2Up(MSHRSize).W))
  val deqPtr = RegInit(0.U(log2Up(MSHRSize).W))

  val mayFull = RegInit(false.B)
  val full = mayFull && enqPtr === deqPtr
  val empty = !mayFull && enqPtr === deqPtr

  val do_enq = io.in.fire()
  val do_deq = io.out.fire()

  when (do_enq) {
    enqPtr := enqPtr + 1.U
    vpn(enqPtr) := io.in.bits.vpn
    source(enqPtr) := io.in.bits.source
  }

  when (do_deq) {
    deqPtr := deqPtr + 1.U
  }

  when (do_enq =/= do_deq) {
    mayFull := do_enq
  }

  when (io.sfence.valid) {
    enqPtr := 0.U
    deqPtr := 0.U
    mayFull := false.B
  }

  io.in.ready := !full
  io.out.valid := !empty
  io.out.bits.vpn := vpn(deqPtr)
  io.out.bits.source := source(deqPtr)
  io.empty := empty

  XSPerfAccumulate("mq_in_count", io.in.fire())
  XSPerfAccumulate("mq_in_block", io.in.valid && !io.in.ready)
  val count = RegInit(0.U(log2Up(MSHRSize+1).W))
  when (do_enq =/= do_deq) {
    count := Mux(do_enq, count + 1.U, count - 1.U)
  }
  when (io.sfence.valid) {
    count := 0.U
  }
  for (i <- 0 until MSHRSize) {
    XSPerfAccumulate(s"numExist${i}", count === i.U)
  }
}