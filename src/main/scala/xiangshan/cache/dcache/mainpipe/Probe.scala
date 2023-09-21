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

package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{TLBundleB, TLEdgeOut, TLMessages, TLPermissions}
import utils.{HasPerfEvents, HasTLDump, XSDebug, XSPerfAccumulate}

class ProbeReq(implicit p: Parameters) extends DCacheBundle
{
  val source = UInt()
  val opcode = UInt()
  val addr   = UInt(PAddrBits.W)
  val vaddr  = UInt(VAddrBits.W) // l2 uses vaddr index to probe l1
  val param  = UInt(TLPermissions.bdWidth.W)
  val needData = Bool()

  // probe queue entry ID
  val id = UInt(log2Up(cfg.nProbeEntries).W)

  def dump() = {
    XSDebug("ProbeReq source: %d opcode: %d addr: %x param: %d\n",
      source, opcode, addr, param)
  }
}

class ProbeResp(implicit p: Parameters) extends DCacheBundle {
  // probe queue entry ID
  val id = UInt(log2Up(cfg.nProbeEntries).W)
}

class ProbeEntry(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new ProbeReq))
    val pipe_req  = DecoupledIO(new MainPipeReq)
    val pipe_resp = Input(Valid(new ProbeResp))
    val lrsc_locked_block = Input(Valid(UInt()))
    val id = Input(UInt(log2Up(cfg.nProbeEntries).W))

    // the block we are probing
    val block_addr  = Output(Valid(UInt()))
  })

  val s_invalid :: s_pipe_req :: s_wait_resp :: Nil = Enum(3)

  val state = RegInit(s_invalid)

  val req = Reg(new ProbeReq)

  // assign default values to signals
  io.req.ready      := false.B
  io.pipe_req.valid := false.B
  io.pipe_req.bits  := DontCare

  io.block_addr.valid := state =/= s_invalid
  io.block_addr.bits  := req.addr

  when (state =/= s_invalid) {
    XSDebug("state: %d\n", state)
  }

  when (state =/= s_invalid) {
    XSDebug("ProbeEntry: state: %d block_addr: %x\n", state, io.block_addr.bits)
  }

  when (state === s_invalid) {
    io.req.ready := true.B
    when (io.req.fire) {
      req := io.req.bits
      state := s_pipe_req
    }
  }

  val lrsc_blocked = Mux(
    io.req.fire,
    io.lrsc_locked_block.valid && get_block(io.lrsc_locked_block.bits) === get_block(io.req.bits.addr),
    io.lrsc_locked_block.valid && get_block(io.lrsc_locked_block.bits) === get_block(req.addr)
  )

  when (state === s_pipe_req) {
    // Note that probe req will be blocked in the next cycle if a lr updates lrsc_locked_block addr
    // in this way, we can RegNext(lrsc_blocked) for better timing
    io.pipe_req.valid := !RegNext(lrsc_blocked)

    val pipe_req = io.pipe_req.bits
    pipe_req := DontCare
    pipe_req.miss := false.B
    pipe_req.probe := true.B
    pipe_req.probe_param := req.param
    pipe_req.addr   := req.addr
    pipe_req.vaddr  := req.vaddr
    pipe_req.probe_need_data := req.needData
    pipe_req.error := false.B
    pipe_req.id := io.id

    when (io.pipe_req.fire) {
      state := s_wait_resp
    }
  }

  when (state === s_wait_resp) {
    when (io.pipe_resp.valid && io.id === io.pipe_resp.bits.id) {
      state := s_invalid
    }
  }

  // perfoemance counters
  XSPerfAccumulate("probe_req", state === s_invalid && io.req.fire)
  XSPerfAccumulate("probe_penalty", state =/= s_invalid)
  XSPerfAccumulate("probe_penalty_blocked_by_lrsc", state === s_pipe_req && io.lrsc_locked_block.valid && get_block(io.lrsc_locked_block.bits) === get_block(req.addr))
  XSPerfAccumulate("probe_penalty_blocked_by_pipeline", state === s_pipe_req && io.pipe_req.valid && !io.pipe_req.ready)
}

class ProbeQueue(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasTLDump with HasPerfEvents
{
  val io = IO(new Bundle {
    val mem_probe = Flipped(Decoupled(new TLBundleB(edge.bundle)))
    val pipe_req  = DecoupledIO(new MainPipeReq)
    val lrsc_locked_block = Input(Valid(UInt()))
    val update_resv_set = Input(Bool())
  })

  val pipe_req_arb = Module(new Arbiter(new MainPipeReq, cfg.nProbeEntries))

  // allocate a free entry for incoming request
  val primary_ready  = Wire(Vec(cfg.nProbeEntries, Bool()))
  val allocate = primary_ready.asUInt.orR
  val alloc_idx = PriorityEncoder(primary_ready)

  // translate to inner req
  val req = Wire(new ProbeReq)
  val alias_addr_frag = io.mem_probe.bits.data(2, 1) // add extra 2 bits from vaddr to get vindex
  req.source := io.mem_probe.bits.source
  req.opcode := io.mem_probe.bits.opcode
  req.addr := io.mem_probe.bits.address
  if(DCacheAboveIndexOffset > DCacheTagOffset) {
    // have alias problem, extra alias bits needed for index
    req.vaddr := Cat(
      io.mem_probe.bits.address(PAddrBits - 1, DCacheAboveIndexOffset), // dontcare
      alias_addr_frag(DCacheAboveIndexOffset - DCacheTagOffset - 1, 0), // index
      io.mem_probe.bits.address(DCacheTagOffset - 1, 0)                 // index & others
    )
  } else { // no alias problem
    req.vaddr := io.mem_probe.bits.address
  }
  req.param := io.mem_probe.bits.param
  req.needData := io.mem_probe.bits.data(0)
  req.id := DontCare

  io.mem_probe.ready := allocate

  val entries = (0 until cfg.nProbeEntries) map { i =>
    val entry = Module(new ProbeEntry)
    entry.io.id := i.U

    // entry req
    entry.io.req.valid := (i.U === alloc_idx) && allocate && io.mem_probe.valid
    primary_ready(i)   := entry.io.req.ready
    entry.io.req.bits  := req

    // pipe_req
    pipe_req_arb.io.in(i) <> entry.io.pipe_req

    // pipe_resp
    entry.io.pipe_resp.valid := io.pipe_req.fire
    entry.io.pipe_resp.bits.id := io.pipe_req.bits.id

    entry.io.lrsc_locked_block := io.lrsc_locked_block

    entry
  }

  // delay probe req for 1 cycle
  val selected_req_valid = RegInit(false.B) 
  val selected_req_bits = RegEnable(pipe_req_arb.io.out.bits, pipe_req_arb.io.out.fire)
  val selected_lrsc_blocked = Mux(
    pipe_req_arb.io.out.fire,
    io.lrsc_locked_block.valid && get_block(io.lrsc_locked_block.bits) === get_block(pipe_req_arb.io.out.bits.addr),
    io.lrsc_locked_block.valid && get_block(io.lrsc_locked_block.bits) === get_block(selected_req_bits.addr) && selected_req_valid
  )
  val resvsetProbeBlock = RegNext(io.update_resv_set || selected_lrsc_blocked)
  // When we update update_resv_set, block all probe req in the next cycle
  // It should give Probe reservation set addr compare an independent cycle,
  // which will lead to better timing
  pipe_req_arb.io.out.ready := !selected_req_valid || io.pipe_req.fire
  io.pipe_req.valid := selected_req_valid && !resvsetProbeBlock
  io.pipe_req.bits := selected_req_bits
  when(io.pipe_req.fire){
    selected_req_valid := false.B
  }
  when(pipe_req_arb.io.out.fire){
    selected_req_valid := true.B
  }

  // print all input/output requests for debug purpose
  when (io.mem_probe.valid) {
    // before a probe finishes, L2 should not further issue probes on this block
    val probe_conflict = VecInit(entries.map(e => e.io.block_addr.valid && get_block(e.io.block_addr.bits) === get_block(io.mem_probe.bits.address))).asUInt.orR
    assert (!probe_conflict)
    // for now, we can only deal with ProbeBlock
    assert (io.mem_probe.bits.opcode === TLMessages.Probe)
  }

  // debug output
  when (io.mem_probe.fire) {
    XSDebug("mem_probe: ")
    io.mem_probe.bits.dump
  }

//  when (io.pipe_req.fire) {
//    io.pipe_req.bits.dump()
//  }

  when (io.lrsc_locked_block.valid) {
    XSDebug("lrsc_locked_block: %x\n", io.lrsc_locked_block.bits)
  }
  XSPerfAccumulate("ProbeL1DCache", io.mem_probe.fire)

  val perfValidCount = RegNext(PopCount(entries.map(e => e.io.block_addr.valid)))
  val perfEvents = Seq(
    ("dcache_probq_req      ", io.pipe_req.fire),
    ("dcache_probq_1_4_valid", (perfValidCount < (cfg.nProbeEntries.U/4.U))),
    ("dcache_probq_2_4_valid", (perfValidCount > (cfg.nProbeEntries.U/4.U)) & (perfValidCount <= (cfg.nProbeEntries.U/2.U))),
    ("dcache_probq_3_4_valid", (perfValidCount > (cfg.nProbeEntries.U/2.U)) & (perfValidCount <= (cfg.nProbeEntries.U*3.U/4.U))),
    ("dcache_probq_4_4_valid", (perfValidCount > (cfg.nProbeEntries.U*3.U/4.U))),
  )
  generatePerfEvent()
}
