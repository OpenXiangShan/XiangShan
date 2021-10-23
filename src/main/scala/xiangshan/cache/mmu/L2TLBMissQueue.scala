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
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}

/* Miss Queue dont care about duplicate req, which is done by PtwFilter
 * PtwMissQueue is just a Queue inside Chisel with flush
 */

class L2TlbMQEntry(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val req_info = new L2TlbInnerBundle()
  val ppn = UInt(ppnLen.W)
  val wait_id = UInt(log2Up(MSHRSize).W)
  val af = Bool()
}

class L2TlbMQInBundle(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val req_info = Output(new L2TlbInnerBundle())
  val l3 = Valid(Output(UInt(PAddrBits.W)))
}

class L2TlbMQCacheBundle(implicit p: Parameters) extends L2TlbInnerBundle with HasPtwConst

class L2TlbMQIO(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val in = Flipped(Decoupled(new L2TlbMQInBundle()))
  val cache = Decoupled(new L2TlbMQCacheBundle())
  val fsm_done = Input(Bool())
  val out = DecoupledIO(new Bundle {
    val req_info = Output(new L2TlbInnerBundle())
    val id = Output(UInt(bMemID.W))
    val af = Output(Bool())
  })
  val mem = new Bundle {
    val req = DecoupledIO(new L2TlbMemReqBundle())
    val resp = Flipped(Valid(new Bundle {
      val id = Output(UInt(log2Up(MSHRSize).W))
    }))
    val enq_ptr = Output(UInt(log2Ceil(MSHRSize).W))
    val buffer_it = Output(Vec(MSHRSize, Bool()))
    val refill = Output(new L2TlbInnerBundle())
    val req_mask = Input(Vec(MSHRSize, Bool()))
  }
  val pmp = new Bundle {
    val req = Valid(new PMPReqBundle())
    val resp = Flipped(new PMPRespBundle())
  }
}

@chiselName
class L2TlbMissQueue(implicit p: Parameters) extends XSModule with HasPtwConst {
  require(MSHRSize >= (2 + l2tlbParams.filterSize))

  val io = IO(new L2TlbMQIO())

  val entries = Reg(Vec(MSHRSize, new L2TlbMQEntry()))
  val state_idle :: state_cache_high :: state_cache_low :: state_addr_check :: state_mem_req :: state_mem_waiting :: state_mem_out :: Nil = Enum(7)
  val state = RegInit(VecInit(Seq.fill(MSHRSize)(state_idle)))
  val is_emptys = state.map(_ === state_idle)
  val is_caches_high = state.map(_ === state_cache_high)
  val is_caches_low = state.map(_ === state_cache_low)
  val is_mems = state.map(_ === state_mem_req)
  val is_waiting = state.map(_ === state_mem_waiting)
  val is_having = state.map(_ === state_mem_out)

  val full = !ParallelOR(is_emptys).asBool()
  val enq_ptr = ParallelPriorityEncoder(is_emptys)
  val cache_high_ptr = ParallelPriorityEncoder(is_caches_high)
  val cache_low_ptr = ParallelPriorityEncoder(is_caches_low)

  val cache_arb = Module(new RRArbiter(new L2TlbMQCacheBundle(), 2))
  cache_arb.io.in(0).valid := Cat(is_caches_high).orR && io.fsm_done // fsm busy, required l1/l2 pte is not ready
  cache_arb.io.in(0).bits := entries(cache_high_ptr).req_info
  cache_arb.io.in(1).valid := Cat(is_caches_low).orR
  cache_arb.io.in(1).bits := entries(cache_low_ptr).req_info
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
  def dup(vpn1: UInt, vpn2: UInt): Bool = {
    dropL3SectorBits(vpn1) === dropL3SectorBits(vpn2)
  }
  val dup_vec = state.indices.map(i =>
    dup(io.in.bits.req_info.vpn, entries(i).req_info.vpn)
  )
  val dup_req_fire = mem_arb.io.out.fire() && dup(io.in.bits.req_info.vpn, mem_arb.io.out.bits.req_info.vpn) // dup with the req fire entry
  val dup_vec_wait = dup_vec.zip(is_waiting).map{case (d, w) => d && w} // dup with "mem_waiting" entres, sending mem req already
  val dup_vec_having = dup_vec.zipWithIndex.map{case (d, i) => d && is_having(i)} // dup with the "mem_out" entry recv the data just now
  val wait_id = Mux(dup_req_fire, mem_arb.io.chosen, ParallelMux(dup_vec_wait zip entries.map(_.wait_id)))
  val dup_wait_resp = io.mem.resp.fire() && VecInit(dup_vec_wait)(io.mem.resp.bits.id) // dup with the entry that data coming next cycle
  val to_wait = Cat(dup_vec_wait).orR || dup_req_fire
  val to_mem_out = dup_wait_resp
  val to_cache_low = Cat(dup_vec_having).orR
  assert(RegNext(!(dup_req_fire && Cat(dup_vec_wait).orR), init = true.B), "mem req but some entries already waiting, should not happed")

  val mem_resp_hit = RegInit(VecInit(Seq.fill(MSHRSize)(false.B)))
  val enq_state = Mux(to_mem_out, state_mem_out, // same to the blew, but the mem resp now
    Mux(to_cache_low, state_cache_low, // same to the below, but the mem resp last cycle
    Mux(to_wait, state_mem_waiting, // wait for the prev mem resp
    Mux(io.in.bits.l3.valid, state_addr_check, state_cache_high))))
  when (io.in.fire()) {
    // if prefetch req does not need mem access, just give it up.
    // so there will be at most 1 + FilterSize entries that needs re-access page cache
    // so 2 + FilterSize is enough to avoid dead-lock
    state(enq_ptr) := Mux(from_pre(io.in.bits.req_info.source) && enq_state =/= state_addr_check, state_idle, enq_state)
    entries(enq_ptr).req_info := io.in.bits.req_info
    entries(enq_ptr).ppn := io.in.bits.l3.bits
    entries(enq_ptr).wait_id := Mux(to_wait, wait_id, enq_ptr)
    entries(enq_ptr).af := false.B
    mem_resp_hit(enq_ptr) := to_mem_out
  }
  when (mem_arb.io.out.fire()) {
    for (i <- state.indices) {
      when (state(i) =/= state_idle && dup(entries(i).req_info.vpn, mem_arb.io.out.bits.req_info.vpn)) {
        // NOTE: "dup enq set state to mem_wait" -> "sending req set other dup entries to mem_wait"
        state(i) := state_mem_waiting
        entries(i).wait_id := mem_arb.io.chosen
      }
    }
  }
  when (io.mem.resp.fire()) {
    state.indices.map{i =>
      when (state(i) === state_mem_waiting && io.mem.resp.bits.id === entries(i).wait_id) {
        state(i) := state_mem_out
        mem_resp_hit(i) := true.B
      }
    }
  }
  when (io.out.fire()) {
    assert(state(mem_ptr) === state_mem_out)
    state(mem_ptr) := state_idle
  }
  when (io.cache.fire()) {
    state(cache_ptr) := state_idle
  }

  mem_resp_hit.map(a => when (a) { a := false.B } )

  val enq_ptr_reg = RegNext(enq_ptr)

  io.pmp.req.valid := RegNext(enq_state === state_addr_check)
  io.pmp.req.bits.addr := MakeAddr(entries(enq_ptr_reg).ppn, getVpnn(entries(enq_ptr_reg).req_info.vpn, 0))
  io.pmp.req.bits.cmd := TlbCmd.read
  io.pmp.req.bits.size := 3.U // TODO: fix it
  val pmp_resp_valid = io.pmp.req.valid // same cycle
  when (pmp_resp_valid && (state(enq_ptr_reg) === state_addr_check) &&
    !(mem_arb.io.out.fire && dup(entries(enq_ptr_reg).req_info.vpn, mem_arb.io.out.bits.req_info.vpn))) {
    // NOTE: when pmp resp but state is not addr check, then the entry is dup with other entry, the state was changed before
    //       when dup with the req-ing entry, set to mem_waiting (above codes), and the ld must be false, so dontcare
    entries(enq_ptr_reg).af := io.pmp.resp.ld
    state(enq_ptr_reg) := Mux(io.pmp.resp.ld, state_mem_out, state_mem_req)
  }

  val flush = io.sfence.valid || io.csr.satp.changed
  when (flush) {
    state.map(_ := state_idle)
  }

  io.in.ready := !full
  io.cache.valid := cache_arb.io.out.valid
  io.cache.bits := cache_arb.io.out.bits

  io.out.valid := ParallelOR(is_having).asBool()
  io.out.bits.req_info := entries(mem_ptr).req_info
  io.out.bits.id := mem_ptr
  io.out.bits.af := entries(mem_ptr).af

  io.mem.req.valid := mem_arb.io.out.valid && !flush
  io.mem.req.bits.addr := MakeAddr(mem_arb.io.out.bits.ppn, getVpnn(mem_arb.io.out.bits.req_info.vpn, 0))
  io.mem.req.bits.id := mem_arb.io.chosen
  mem_arb.io.out.ready := io.mem.req.ready
  io.mem.refill := entries(RegNext(io.mem.resp.bits.id(log2Up(MSHRSize)-1, 0))).req_info
  io.mem.buffer_it := mem_resp_hit
  io.mem.enq_ptr := enq_ptr

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
  XSPerfAccumulate("blocked_in", io.in.valid && !io.in.ready)

  for (i <- 0 until MSHRSize) {
    TimeOutAssert(state(i) =/= state_idle, timeOutThreshold, s"missqueue time out no out ${i}")
  }

  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(4))
  })
  val perfEvents = Seq(
    ("tlbmissq_incount           ", io.in.fire()               ),
    ("tlbmissq_inblock           ", io.in.valid && !io.in.ready),
    ("tlbmissq_memcount          ", io.mem.req.fire()          ),
    ("tlbmissq_memcycle          ", PopCount(is_waiting)       ),
  )

  for (((perf_out,(perf_name,perf)),i) <- perfinfo.perfEvents.perf_events.zip(perfEvents).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }
}
