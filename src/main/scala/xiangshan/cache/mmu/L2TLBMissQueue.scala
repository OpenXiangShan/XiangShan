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
import chisel3.internal.naming.chiselName
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._

/* Miss Queue dont care about duplicate req, which is done by PtwFilter
 * PtwMissQueue is just a Queue inside Chisel with flush
 */

class L2TlbMQEntry(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val vpn = UInt(vpnLen.W)
  val source = UInt(bPtwWidth.W)
  val ppn = UInt(ppnLen.W)
}

class L2TlbMQIO(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val in = Flipped(Decoupled(new Bundle {
    val vpn = Output(UInt(vpnLen.W))
    val source = Output(UInt(bPtwWidth.W))
    val l3 = Valid(UInt(PAddrBits.W))
  }))
  val sfence = Input(new SfenceBundle)
  val cache = Decoupled(new Bundle {
    val vpn = UInt(vpnLen.W)
    val source = UInt(bPtwWidth.W)
  })
  val mem = new Bundle {
    val req = DecoupledIO(new L2TlbMemReqBundle())
    val resp = Flipped(Valid(new Bundle {
      val id = Output(UInt(log2Up(MSHRSize).W))
    }))
    val out = DecoupledIO(new Bundle {
      val source = Output(UInt(bPtwWidth.W))
      val id = Output(UInt(bMemID.W))
      val vpn = Output(UInt(vpnLen.W))
    })
    val refill_vpn = Output(UInt(vpnLen.W))
    val req_mask = Input(Vec(MSHRSize, Bool()))
  }
}

@chiselName
class L2TlbMissQueue(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new L2TlbMQIO())

  val state_idle :: state_cache :: state_mem_req :: state_mem_waiting :: state_mem_out :: Nil = Enum(5)

  val state = RegInit(VecInit(Seq.fill(MSHRSize)(state_idle)))
  val is_emptys = state.map(_ === state_idle)
  val is_caches = state.map(_ === state_cache)
  val is_mems = state.map(_ === state_mem_req)
  val is_waiting = state.map(_ === state_mem_waiting)
  val is_having = state.map(_ === state_mem_out)

  val entries = Reg(Vec(MSHRSize, new L2TlbMQEntry()))

  val full = !ParallelOR(is_emptys).asBool()
  val non_empty = ParallelOR(is_caches).asBool()

  val enq_ptr = ParallelPriorityEncoder(is_emptys)
  val cache_ptr = ParallelPriorityEncoder(is_caches)
  val mem_ptr = ParallelPriorityEncoder(is_having)

  val mem_arb = Module(new RRArbiter(new L2TlbMQEntry(), MSHRSize))
  for (i <- 0 until MSHRSize) {
    mem_arb.io.in(i).bits := entries(i)
    mem_arb.io.in(i).valid := is_mems(i) && !io.mem.req_mask(i)
  }

  when (io.in.fire()) {
    state(enq_ptr) := Mux(io.in.bits.l3.valid, state_mem_req, state_cache)
    entries(enq_ptr).vpn := io.in.bits.vpn
    entries(enq_ptr).ppn := io.in.bits.l3.bits
    entries(enq_ptr).source := io.in.bits.source
  }
  when (mem_arb.io.out.fire()) {
    state(mem_arb.io.chosen) := state_mem_waiting
  }
  when (io.mem.resp.fire()) {
    state(io.mem.resp.bits.id(log2Up(MSHRSize)-1, 0)) := state_mem_out
  }
  when (io.mem.out.fire()) {
    assert(state(mem_ptr) === state_mem_out)
    state(mem_ptr) := state_idle
  }
  when (io.cache.fire()) {
    state(cache_ptr) := state_idle
  }

  when (io.sfence.valid) {
    state.map(_ := state_idle)
  }

  io.in.ready := !full
  io.cache.valid := ParallelOR(is_caches).asBool()
  io.cache.bits.vpn := entries(cache_ptr).vpn
  io.cache.bits.source := entries(cache_ptr).source
  io.mem.out.valid := ParallelOR(is_having).asBool()
  io.mem.out.bits.source := entries(mem_ptr).source
  io.mem.out.bits.vpn := entries(mem_ptr).vpn
  io.mem.out.bits.id := mem_ptr
  io.mem.req.valid := mem_arb.io.out.valid
  io.mem.req.bits.addr := MakeAddr(mem_arb.io.out.bits.ppn, getVpnn(mem_arb.io.out.bits.vpn, 0))
  io.mem.req.bits.id := mem_arb.io.chosen
  mem_arb.io.out.ready := io.mem.req.ready
  io.mem.refill_vpn := entries(RegNext(io.mem.resp.bits.id(log2Up(MSHRSize)-1, 0))).vpn

  XSPerfAccumulate("mq_in_count", io.in.fire())
  XSPerfAccumulate("mq_in_block", io.in.valid && !io.in.ready)
  for (i <- 0 until (MSHRSize + 1)) {
    XSPerfAccumulate(s"util${i}", PopCount(is_emptys.map(!_)) === i.U)
    XSPerfAccumulate(s"cache_util${i}", PopCount(is_caches) === i.U)
    XSPerfAccumulate(s"mem_util${i}", PopCount(is_mems) === i.U)
    XSPerfAccumulate(s"waiting_util${i}", PopCount(is_waiting) === i.U)
  }
  XSPerfAccumulate("mem_count", io.mem.req.fire())
  XSPerfAccumulate("mem_cycle", PopCount(is_waiting) =/= 0.U)
}