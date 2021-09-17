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
  val wait_id = UInt(log2Up(MSHRSize).W)
}

class L2TlbMQInBundle(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val vpn = Output(UInt(vpnLen.W))
  val source = Output(UInt(bPtwWidth.W))
  val l3 = Valid(Output(UInt(PAddrBits.W)))
}

class L2TlbMQCacheBundle(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val vpn = Output(UInt(vpnLen.W))
  val source = Output(UInt(bPtwWidth.W))
}

class L2TlbMQIO(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val in = Flipped(Decoupled(new L2TlbMQInBundle()))
  val sfence = Input(new SfenceBundle)
  val cache = Decoupled(new L2TlbMQCacheBundle())
  val fsm_done = Input(Bool())
  val out = DecoupledIO(new Bundle {
    val source = Output(UInt(bPtwWidth.W))
    val id = Output(UInt(bMemID.W))
    val vpn = Output(UInt(vpnLen.W))
  })
  val mem = new Bundle {
    val req = DecoupledIO(new L2TlbMemReqBundle())
    val resp = Flipped(Valid(new Bundle {
      val id = Output(UInt(log2Up(MSHRSize).W))
    }))

    val refill_vpn = Output(UInt(vpnLen.W))
    val req_mask = Input(Vec(MSHRSize, Bool()))
  }
}

@chiselName
class L2TlbMissQueue(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new L2TlbMQIO())

  val entries = Reg(Vec(MSHRSize, new L2TlbMQEntry()))
  val state_idle :: state_cache_high :: state_cache_low :: state_cache_next :: state_mem_req :: state_mem_waiting :: state_mem_out :: Nil = Enum(7)
  val state = RegInit(VecInit(Seq.fill(MSHRSize)(state_idle)))
  val is_emptys = state.map(_ === state_idle)
  val is_caches_high = state.map(_ === state_cache_high)
  val is_caches_low = state.map(_ === state_cache_low)
  val is_mems = state.map(_ === state_mem_req)
  val is_waiting = state.map(_ === state_mem_waiting)
  val is_having = state.map(_ === state_mem_out)
  val is_cache_next = state.map(_ === state_cache_next)

  val full = !ParallelOR(is_emptys).asBool()
  val enq_ptr = ParallelPriorityEncoder(is_emptys)
  val cache_high_ptr = ParallelPriorityEncoder(is_caches_high)
  val cache_low_ptr = ParallelPriorityEncoder(is_caches_low)

  val cache_arb = Module(new Arbiter(new L2TlbMQCacheBundle(), 2))
  cache_arb.io.in(0).valid := Cat(is_caches_high).orR && io.fsm_done // fsm busy, required l1/l2 pte is not ready
  cache_arb.io.in(0).bits.vpn := entries(cache_high_ptr).vpn
  cache_arb.io.in(0).bits.source := entries(cache_high_ptr).source
  cache_arb.io.in(1).valid := Cat(is_caches_low).orR
  cache_arb.io.in(1).bits.vpn := entries(cache_low_ptr).vpn
  cache_arb.io.in(1).bits.source := entries(cache_low_ptr).source
  cache_arb.io.out.ready := io.cache.ready
  val cache_ptr = Mux(cache_arb.io.chosen === 0.U, cache_high_ptr, cache_low_ptr)

  val mem_ptr = ParallelPriorityEncoder(is_having)
  val mem_arb = Module(new RRArbiter(new L2TlbMQEntry(), MSHRSize))
  for (i <- 0 until MSHRSize) {
    mem_arb.io.in(i).bits := entries(i)
    mem_arb.io.in(i).valid := is_mems(i) && !io.mem.req_mask(i)
  }

  // duplicate req
  // to_wait: wait for the last to access mem, set to mem_resp
  // to_cache: the last is back just right now, set to mem_cache
  val dropLowVpn = entries.map(a => dropL3SectorBits(a.vpn))
  val dropLowVpnIn = dropL3SectorBits(io.in.bits.vpn)
  val dup_vec = state.indices.map(i =>
    io.in.bits.l3.valid && (dropLowVpnIn === dropLowVpn(i))
  )
  val dup_vec_mem = dup_vec.zip(is_mems).map{case (d, m) => d && m} // already some req are state_mem_req
  val dup_vec_wait = dup_vec.zip(is_waiting).map{case (d, w) => d && w} // already some req are state_mem_wait
  val dup_vec_wait_id = dup_vec_mem.zip(dup_vec_wait).map{case (a, b) => a || b} // get the wait_id from above reqs
  val dup_vec_having = dup_vec.zipWithIndex.map{case (d, i) => d && (is_having(i) || is_caches_low(i) || is_cache_next(i))}
  val wait_id = ParallelMux(dup_vec_wait_id zip entries.map(_.wait_id))
  val dup_wait_resp = io.mem.resp.valid && VecInit(dup_vec_wait)(io.mem.resp.bits.id)
  val to_wait = Cat(dup_vec_mem).orR || (Cat(dup_vec_wait).orR && !dup_wait_resp)
  val to_cache = Cat(dup_vec_having).orR || dup_wait_resp

  for (i <- 0 until MSHRSize) {
    when (state(i) === state_cache_next) {
      state(i) := state_cache_low
    }
  }
  val enq_state = Mux(to_cache, state_cache_next, // relay one cycle to wait for refill
    Mux(to_wait, state_mem_waiting,
    Mux(io.in.bits.l3.valid, state_mem_req, state_cache_high)))
  when (io.in.fire()) {
    state(enq_ptr) := enq_state
    entries(enq_ptr).vpn := io.in.bits.vpn
    entries(enq_ptr).ppn := io.in.bits.l3.bits
    entries(enq_ptr).source := io.in.bits.source
    entries(enq_ptr).wait_id := Mux(to_wait, wait_id, enq_ptr)
  }
  when (mem_arb.io.out.fire()) {
    state(mem_arb.io.chosen) := state_mem_waiting
    entries(mem_arb.io.chosen).wait_id := mem_arb.io.chosen
  }
  when (io.mem.resp.fire()) {
    state.indices.map{i =>
      when (state(i) === state_mem_waiting &&
        io.mem.resp.bits.id === entries(i).wait_id &&
        i.U =/= entries(i).wait_id) {
        state(i) := state_cache_low
      }
    }
    state(io.mem.resp.bits.id(log2Up(MSHRSize)-1, 0)) := state_mem_out
  }
  when (io.out.fire()) {
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
  io.cache.valid := cache_arb.io.out.valid
  io.cache.bits.vpn := cache_arb.io.out.bits.vpn
  io.cache.bits.source := cache_arb.io.out.bits.source
  io.out.valid := ParallelOR(is_having).asBool()
  io.out.bits.source := entries(mem_ptr).source
  io.out.bits.vpn := entries(mem_ptr).vpn
  io.out.bits.id := mem_ptr
  io.mem.req.valid := mem_arb.io.out.valid
  io.mem.req.bits.addr := MakeAddr(mem_arb.io.out.bits.ppn, getVpnn(mem_arb.io.out.bits.vpn, 0))
  io.mem.req.bits.id := mem_arb.io.chosen
  mem_arb.io.out.ready := io.mem.req.ready
  io.mem.refill_vpn := entries(RegNext(io.mem.resp.bits.id(log2Up(MSHRSize)-1, 0))).vpn

  XSPerfAccumulate("mq_in_count", io.in.fire())
  XSPerfAccumulate("mq_in_block", io.in.valid && !io.in.ready)
  for (i <- 0 until 7) {
    XSPerfAccumulate(s"enq_state${i}", io.in.fire() && enq_state === i.U)
  }
  for (i <- 0 until (MSHRSize + 1)) {
    XSPerfAccumulate(s"util${i}", PopCount(is_emptys.map(!_)) === i.U)
    XSPerfAccumulate(s"cache_high_util${i}", PopCount(is_caches_high) === i.U)
    XSPerfAccumulate(s"cache_low_util${i}", PopCount(is_caches_low) === i.U)
    XSPerfAccumulate(s"mem_util${i}", PopCount(is_mems) === i.U)
    XSPerfAccumulate(s"waiting_util${i}", PopCount(is_waiting) === i.U)
  }
  XSPerfAccumulate("mem_count", io.mem.req.fire())
  XSPerfAccumulate("mem_cycle", PopCount(is_waiting) =/= 0.U)

  for (i <- 0 until MSHRSize) {
    TimeOutAssert(state(i) =/= state_idle, timeOutThreshold, s"missqueue time out no out ${i}")
  }
  assert(!io.in.valid || io.in.ready, "when io.in.valid, should always ready")
}