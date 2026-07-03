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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{TLBundleB, TLEdgeOut, TLMessages, TLPermissions}
import utils.HasTLDump
import utility.{XSDebug, XSPerfAccumulate, HasPerfEvents}

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

  def dump(cond: Bool) = {
    XSDebug(cond, "ProbeReq source: %d opcode: %d addr: %x param: %d\n",
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
    val pipeReq  = DecoupledIO(new MainPipeReq)
    val pipeResp = Input(Valid(new ProbeResp))
    val lrscLockedBlock = Input(Valid(UInt()))
    val id = Input(UInt(log2Up(cfg.nProbeEntries).W))

    // the block we are probing
    val blockAddr  = Output(Valid(UInt()))
  })

  val sInvalid :: s_pipe_req :: s_wait_resp :: Nil = Enum(3)

  val state = RegInit(sInvalid)

  val req = Reg(new ProbeReq)

  // assign default values to signals
  io.req.ready      := false.B
  io.pipeReq.valid := false.B
  io.pipeReq.bits  := DontCare

  io.blockAddr.valid := state =/= sInvalid
  io.blockAddr.bits  := req.addr

  XSDebug(state =/= sInvalid, "state: %d\n", state)

  XSDebug(state =/= sInvalid, "ProbeEntry: state: %d block_addr: %x\n", state, io.blockAddr.bits)

  when (state === sInvalid) {
    io.req.ready := true.B
    when (io.req.fire) {
      req := io.req.bits
      state := s_pipe_req
    }
  }

  val lrscBlocked = Mux(
    io.req.fire,
    io.lrscLockedBlock.valid && get_block(io.lrscLockedBlock.bits) === get_block(io.req.bits.addr),
    io.lrscLockedBlock.valid && get_block(io.lrscLockedBlock.bits) === get_block(req.addr)
  )

  when (state === s_pipe_req) {
    // Note that probe req will be blocked in the next cycle if a lr updates lrsc_locked_block addr
    // in this way, we can RegNext(lrsc_blocked) for better timing
    io.pipeReq.valid := !RegNext(lrscBlocked)

    val pipeReq = io.pipeReq.bits
    pipeReq := DontCare
    pipeReq.miss := false.B
    pipeReq.probe := true.B
    pipeReq.probeParam := req.param
    pipeReq.addr   := req.addr
    pipeReq.vaddr  := req.vaddr
    pipeReq.probeNeedData := req.needData
    pipeReq.error := false.B
    pipeReq.id := io.id
    pipeReq.missFailCauseEvictBtot := false.B

    when (io.pipeReq.fire) {
      state := s_wait_resp
    }
  }

  when (state === s_wait_resp) {
    when (io.pipeResp.valid && io.id === io.pipeResp.bits.id) {
      state := sInvalid
    }
  }

  // perfoemance counters
  XSPerfAccumulate("probe_req", state === sInvalid && io.req.fire)
  XSPerfAccumulate("probe_penalty", state =/= sInvalid)
  XSPerfAccumulate("probe_penalty_blocked_by_lrsc", state === s_pipe_req && io.lrscLockedBlock.valid && get_block(io.lrscLockedBlock.bits) === get_block(req.addr))
  XSPerfAccumulate("probe_penalty_blocked_by_pipeline", state === s_pipe_req && io.pipeReq.valid && !io.pipeReq.ready)
}

class ProbeQueue(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasTLDump with HasPerfEvents
{
  val io = IO(new Bundle {
    val memProbe = Flipped(Decoupled(new TLBundleB(edge.bundle)))
    val pipeReq  = DecoupledIO(new MainPipeReq)
    val lrscLockedBlock = Input(Valid(UInt()))
    val updateResvSet = Input(Bool())
  })

  val pipeReqArb = Module(new Arbiter(new MainPipeReq, cfg.nProbeEntries))

  // allocate a free entry for incoming request
  val primaryReady  = Wire(Vec(cfg.nProbeEntries, Bool()))
  val allocate = primaryReady.asUInt.orR
  val allocIdx = PriorityEncoder(primaryReady)

  // translate to inner req
  val req = Wire(new ProbeReq)
  val aliasAddrFrag = io.memProbe.bits.data(2, 1) // add extra 2 bits from vaddr to get vindex
  req.source := io.memProbe.bits.source
  req.opcode := io.memProbe.bits.opcode
  req.addr := io.memProbe.bits.address
  if(DCacheAboveIndexOffset > DCacheTagOffset) {
    // have alias problem, extra alias bits needed for index
    req.vaddr := Cat(
      0.U((PAddrBits - DCacheAboveIndexOffset).W), // dontcare
      aliasAddrFrag(DCacheAboveIndexOffset - DCacheTagOffset - 1, 0), // index
      io.memProbe.bits.address(DCacheTagOffset - 1, 0)                 // index & others
    )
  } else { // no alias problem
    req.vaddr := io.memProbe.bits.address
  }
  req.param := io.memProbe.bits.param
  req.needData := io.memProbe.bits.data(0)
  req.id := DontCare

  io.memProbe.ready := allocate

  val entries = (0 until cfg.nProbeEntries) map { i =>
    val entry = Module(new ProbeEntry)
    entry.io.id := i.U

    // entry req
    entry.io.req.valid := (i.U === allocIdx) && allocate && io.memProbe.valid
    primaryReady(i)   := entry.io.req.ready
    entry.io.req.bits  := req

    // pipe_req
    pipeReqArb.io.in(i) <> entry.io.pipeReq

    // pipe_resp
    entry.io.pipeResp.valid := io.pipeReq.fire
    entry.io.pipeResp.bits.id := io.pipeReq.bits.id

    entry.io.lrscLockedBlock := io.lrscLockedBlock

    entry
  }

  // delay probe req for 1 cycle
  val selectedReqValid = RegInit(false.B)
  val selectedReqBits = RegEnable(pipeReqArb.io.out.bits, pipeReqArb.io.out.fire)
  val selectedLrscBlocked = Mux(
    pipeReqArb.io.out.fire,
    io.lrscLockedBlock.valid && get_block(io.lrscLockedBlock.bits) === get_block(pipeReqArb.io.out.bits.addr),
    io.lrscLockedBlock.valid && get_block(io.lrscLockedBlock.bits) === get_block(selectedReqBits.addr) && selectedReqValid
  )
  val resvsetProbeBlock = RegNext(io.updateResvSet || selectedLrscBlocked)
  // When we update update_resv_set, block all probe req in the next cycle
  // It should give Probe reservation set addr compare an independent cycle,
  // which will lead to better timing
  pipeReqArb.io.out.ready := !selectedReqValid || io.pipeReq.fire
  io.pipeReq.valid := selectedReqValid && !resvsetProbeBlock
  io.pipeReq.bits := selectedReqBits
  when(io.pipeReq.fire){
    selectedReqValid := false.B
  }
  when(pipeReqArb.io.out.fire){
    selectedReqValid := true.B
  }

  // print all input/output requests for debug purpose
  when (io.memProbe.valid) {
    // before a probe finishes, L2 should not further issue probes on this block
    val probeConflict = VecInit(entries.map(e => e.io.blockAddr.valid && get_block(e.io.blockAddr.bits) === get_block(io.memProbe.bits.address))).asUInt.orR
    assert (!probeConflict)
    // for now, we can only deal with ProbeBlock
    assert (io.memProbe.bits.opcode === TLMessages.Probe)
  }

  // debug output
  XSDebug(io.memProbe.fire, "mem_probe: ")
  io.memProbe.bits.dump(io.memProbe.fire)

// io.pipe_req.bits.dump(io.pipe_req.fire)

  XSDebug(io.lrscLockedBlock.valid, "lrsc_locked_block: %x\n", io.lrscLockedBlock.bits)
  XSPerfAccumulate("ProbeL1DCache", io.memProbe.fire)

  val perfValidCount = RegNext(PopCount(entries.map(e => e.io.blockAddr.valid)))
  val perfEvents = Seq(
    ("dcache_probq_req      ", io.pipeReq.fire),
    ("dcache_probq_1_4_valid", (perfValidCount < (cfg.nProbeEntries.U/4.U))),
    ("dcache_probq_2_4_valid", (perfValidCount > (cfg.nProbeEntries.U/4.U)) & (perfValidCount <= (cfg.nProbeEntries.U/2.U))),
    ("dcache_probq_3_4_valid", (perfValidCount > (cfg.nProbeEntries.U/2.U)) & (perfValidCount <= (cfg.nProbeEntries.U*3.U/4.U))),
    ("dcache_probq_4_4_valid", (perfValidCount > (cfg.nProbeEntries.U*3.U/4.U))),
  )
  generatePerfEvent()
}
