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

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.decode.ImmUnion
import xiangshan.cache._
import xiangshan.cache.mmu.{TlbRequestIO, TlbReq, TlbResp, TlbCmd}

class LoadToLsqIO(implicit p: Parameters) extends XSBundle {
  val loadIn = ValidIO(new LsPipelineBundle)
  val ldout = Flipped(DecoupledIO(new ExuOutput))
  val loadDataForwarded = Output(Bool())
  val needReplayFromRS = Output(Bool())
  val forward = new PipeLoadForwardQueryIO
}

// Load Pipeline Stage 0
// Generate addr, use addr to query DCache and DTLB
class LoadUnit_S0(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new ExuInput))
    val out = Decoupled(new LsPipelineBundle)
    val dtlbReq = DecoupledIO(new TlbReq)
    val dcacheReq = DecoupledIO(new DCacheWordReq)
    val rsIdx = Input(UInt(log2Up(IssQueSize).W))
    val isFirstIssue = Input(Bool())
  })

  val s0_uop = io.in.bits.uop
  // val s0_vaddr = io.in.bits.src(0) + SignExt(s0_uop.ctrl.imm(11,0), VAddrBits)
  // val s0_mask = genWmask(s0_vaddr, s0_uop.ctrl.fuOpType(1,0))
  val imm12 = WireInit(s0_uop.ctrl.imm(11,0))
  val s0_vaddr_lo = io.in.bits.src(0)(11,0) + Cat(0.U(1.W), imm12)
  val s0_vaddr_hi = Mux(s0_vaddr_lo(12),
    Mux(imm12(11), io.in.bits.src(0)(VAddrBits-1, 12), io.in.bits.src(0)(VAddrBits-1, 12)+1.U),
    Mux(imm12(11), io.in.bits.src(0)(VAddrBits-1, 12)+SignExt(1.U, VAddrBits-12), io.in.bits.src(0)(VAddrBits-1, 12)),
  )
  val s0_vaddr = Cat(s0_vaddr_hi, s0_vaddr_lo(11,0))
  val s0_mask = genWmask(s0_vaddr_lo, s0_uop.ctrl.fuOpType(1,0))

  // query DTLB
  io.dtlbReq.valid := io.in.valid
  io.dtlbReq.bits.vaddr := s0_vaddr
  io.dtlbReq.bits.cmd := TlbCmd.read
  io.dtlbReq.bits.roqIdx := s0_uop.roqIdx
  io.dtlbReq.bits.debug.pc := s0_uop.cf.pc
  io.dtlbReq.bits.debug.isFirstIssue := io.isFirstIssue

  // query DCache
  io.dcacheReq.valid := io.in.valid
  io.dcacheReq.bits.cmd  := MemoryOpConstants.M_XRD
  io.dcacheReq.bits.addr := s0_vaddr
  io.dcacheReq.bits.mask := s0_mask
  io.dcacheReq.bits.data := DontCare

  // TODO: update cache meta
  io.dcacheReq.bits.id   := DontCare

  val addrAligned = LookupTree(s0_uop.ctrl.fuOpType(1, 0), List(
    "b00".U   -> true.B,                   //b
    "b01".U   -> (s0_vaddr(0)    === 0.U), //h
    "b10".U   -> (s0_vaddr(1, 0) === 0.U), //w
    "b11".U   -> (s0_vaddr(2, 0) === 0.U)  //d
  ))

  io.out.valid := io.in.valid && io.dcacheReq.ready

  io.out.bits := DontCare
  io.out.bits.vaddr := s0_vaddr
  io.out.bits.mask := s0_mask
  io.out.bits.uop := s0_uop
  io.out.bits.uop.cf.exceptionVec(loadAddrMisaligned) := !addrAligned
  io.out.bits.rsIdx := io.rsIdx

  io.in.ready := !io.in.valid || (io.out.ready && io.dcacheReq.ready)

  XSDebug(io.dcacheReq.fire(),
    p"[DCACHE LOAD REQ] pc ${Hexadecimal(s0_uop.cf.pc)}, vaddr ${Hexadecimal(s0_vaddr)}\n"
  )
  XSPerfAccumulate("in", io.in.valid)
  XSPerfAccumulate("stall_out", io.out.valid && !io.out.ready && io.dcacheReq.ready)
  XSPerfAccumulate("stall_dcache", io.out.valid && io.out.ready && !io.dcacheReq.ready)
  XSPerfAccumulate("addr_spec_success", io.out.fire() && s0_vaddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_failed", io.out.fire() && s0_vaddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_success_once", io.out.fire() && s0_vaddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12) && io.isFirstIssue)
  XSPerfAccumulate("addr_spec_failed_once", io.out.fire() && s0_vaddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12) && io.isFirstIssue)
}


// Load Pipeline Stage 1
// TLB resp (send paddr to dcache)
class LoadUnit_S1(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val dtlbResp = Flipped(DecoupledIO(new TlbResp))
    val dcachePAddr = Output(UInt(PAddrBits.W))
    val dcacheKill = Output(Bool())
    val fullForwardFast = Output(Bool())
    val sbuffer = new LoadForwardQueryIO
    val lsq = new PipeLoadForwardQueryIO
  })

  val s1_uop = io.in.bits.uop
  val s1_paddr = io.dtlbResp.bits.paddr
  val s1_exception = selectLoad(io.out.bits.uop.cf.exceptionVec, false).asUInt.orR
  val s1_tlb_miss = io.dtlbResp.bits.miss
  val s1_mmio = !s1_tlb_miss && io.dtlbResp.bits.mmio
  val s1_mask = io.in.bits.mask

  io.out.bits := io.in.bits // forwardXX field will be updated in s1

  io.dtlbResp.ready := true.B

  // TOOD: PMA check
  io.dcachePAddr := s1_paddr
  io.dcacheKill := s1_tlb_miss || s1_exception || s1_mmio

  // load forward query datapath
  io.sbuffer.valid := io.in.valid && !(s1_exception || s1_tlb_miss)
  io.sbuffer.vaddr := io.in.bits.vaddr
  io.sbuffer.paddr := s1_paddr
  io.sbuffer.uop := s1_uop
  io.sbuffer.sqIdx := s1_uop.sqIdx
  io.sbuffer.mask := s1_mask
  io.sbuffer.pc := s1_uop.cf.pc // FIXME: remove it

  io.lsq.valid := io.in.valid && !(s1_exception || s1_tlb_miss)
  io.lsq.vaddr := io.in.bits.vaddr
  io.lsq.paddr := s1_paddr
  io.lsq.uop := s1_uop
  io.lsq.sqIdx := s1_uop.sqIdx
  io.lsq.sqIdxMask := DontCare // will be overwritten by sqIdxMask pre-generated in s0
  io.lsq.mask := s1_mask
  io.lsq.pc := s1_uop.cf.pc // FIXME: remove it

  // Generate forwardMaskFast to wake up insts earlier
  val forwardMaskFast = io.lsq.forwardMaskFast.asUInt | io.sbuffer.forwardMaskFast.asUInt
  io.fullForwardFast := (~forwardMaskFast & s1_mask) === 0.U


  io.out.valid := io.in.valid// && !s1_tlb_miss
  io.out.bits.paddr := s1_paddr
  io.out.bits.mmio := s1_mmio && !s1_exception
  io.out.bits.tlbMiss := s1_tlb_miss
  io.out.bits.uop.cf.exceptionVec(loadPageFault) := io.dtlbResp.bits.excp.pf.ld
  io.out.bits.uop.cf.exceptionVec(loadAccessFault) := io.dtlbResp.bits.excp.af.ld
  io.out.bits.ptwBack := io.dtlbResp.bits.ptwBack
  io.out.bits.rsIdx := io.in.bits.rsIdx

  io.in.ready := !io.in.valid || io.out.ready

  XSPerfAccumulate("in", io.in.valid)
  XSPerfAccumulate("tlb_miss", io.in.valid && s1_tlb_miss)
  XSPerfAccumulate("stall_out", io.out.valid && !io.out.ready)
}


// Load Pipeline Stage 2
// DCache resp
class LoadUnit_S2(implicit p: Parameters) extends XSModule with HasLoadHelper {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val rsFeedback = ValidIO(new RSFeedback)
    val dcacheResp = Flipped(DecoupledIO(new DCacheWordResp))
    val lsq = new LoadForwardQueryIO
    val sbuffer = new LoadForwardQueryIO
    val dataForwarded = Output(Bool())
    val needReplayFromRS = Output(Bool())
  })

  val s2_uop = io.in.bits.uop
  val s2_mask = io.in.bits.mask
  val s2_paddr = io.in.bits.paddr
  val s2_tlb_miss = io.in.bits.tlbMiss
  val s2_data_invalid = io.lsq.dataInvalid
  val s2_exception = selectLoad(io.in.bits.uop.cf.exceptionVec, false).asUInt.orR
  val s2_mmio = io.in.bits.mmio && !s2_exception
  val s2_cache_miss = io.dcacheResp.bits.miss
  val s2_cache_replay = io.dcacheResp.bits.replay

  // val cnt = RegInit(127.U)
  // cnt := cnt + io.in.valid.asUInt
  // val s2_forward_fail = io.lsq.matchInvalid || io.sbuffer.matchInvalid || cnt === 0.U

  val s2_forward_fail = io.lsq.matchInvalid || io.sbuffer.matchInvalid

  // assert(!s2_forward_fail)

  io.dcacheResp.ready := true.B
  val dcacheShouldResp = !(s2_tlb_miss || s2_exception || s2_mmio)
  assert(!(io.in.valid && dcacheShouldResp && !io.dcacheResp.valid), "DCache response got lost")

  // merge forward result
  // lsq has higher priority than sbuffer
  val forwardMask = Wire(Vec(8, Bool()))
  val forwardData = Wire(Vec(8, UInt(8.W)))

  val fullForward = (~forwardMask.asUInt & s2_mask) === 0.U && !io.lsq.dataInvalid
  io.lsq := DontCare
  io.sbuffer := DontCare

  // generate XLEN/8 Muxs
  for (i <- 0 until XLEN / 8) {
    forwardMask(i) := io.lsq.forwardMask(i) || io.sbuffer.forwardMask(i)
    forwardData(i) := Mux(io.lsq.forwardMask(i), io.lsq.forwardData(i), io.sbuffer.forwardData(i))
  }

  XSDebug(io.out.fire(), "[FWD LOAD RESP] pc %x fwd %x(%b) + %x(%b)\n",
    s2_uop.cf.pc,
    io.lsq.forwardData.asUInt, io.lsq.forwardMask.asUInt,
    io.in.bits.forwardData.asUInt, io.in.bits.forwardMask.asUInt
  )

  // data merge
  val rdataVec = VecInit((0 until XLEN / 8).map(j =>
    Mux(forwardMask(j), forwardData(j), io.dcacheResp.bits.data(8*(j+1)-1, 8*j))))
  val rdata = rdataVec.asUInt
  val rdataSel = LookupTree(s2_paddr(2, 0), List(
    "b000".U -> rdata(63, 0),
    "b001".U -> rdata(63, 8),
    "b010".U -> rdata(63, 16),
    "b011".U -> rdata(63, 24),
    "b100".U -> rdata(63, 32),
    "b101".U -> rdata(63, 40),
    "b110".U -> rdata(63, 48),
    "b111".U -> rdata(63, 56)
  ))
  val rdataPartialLoad = rdataHelper(s2_uop, rdataSel)

  io.out.valid := io.in.valid && !s2_tlb_miss && !s2_data_invalid
  // Inst will be canceled in store queue / lsq,
  // so we do not need to care about flush in load / store unit's out.valid
  io.out.bits := io.in.bits
  io.out.bits.data := rdataPartialLoad
  // when exception occurs, set it to not miss and let it write back to roq (via int port)
  if (EnableFastForward) {
    io.out.bits.miss := s2_cache_miss && !s2_exception && !s2_forward_fail && !fullForward
  } else {
    io.out.bits.miss := s2_cache_miss && !s2_exception && !s2_forward_fail
  }
  io.out.bits.uop.ctrl.fpWen := io.in.bits.uop.ctrl.fpWen && !s2_exception
  io.out.bits.uop.cf.replayInst := s2_forward_fail && !s2_mmio // if forward fail, repaly this inst
  io.out.bits.mmio := s2_mmio
  
  // For timing reasons, sometimes we can not let
  // io.out.bits.miss := s2_cache_miss && !s2_exception && !fullForward
  // We use io.dataForwarded instead. It means forward logic have prepared all data needed,
  // and dcache query is no longer needed.
  // Such inst will be writebacked from load queue.
  io.dataForwarded := s2_cache_miss && fullForward && !s2_exception && !s2_forward_fail
  // io.out.bits.forwardX will be send to lq
  io.out.bits.forwardMask := forwardMask
  // data retbrived from dcache is also included in io.out.bits.forwardData
  io.out.bits.forwardData := rdataVec

  io.in.ready := io.out.ready || !io.in.valid

  // feedback tlb result to RS
  io.rsFeedback.valid := io.in.valid
  io.rsFeedback.bits.hit := !s2_tlb_miss && (!s2_cache_replay || s2_mmio || s2_exception || fullForward) && !s2_data_invalid
  io.rsFeedback.bits.rsIdx := io.in.bits.rsIdx
  io.rsFeedback.bits.flushState := io.in.bits.ptwBack
  io.rsFeedback.bits.sourceType := Mux(s2_tlb_miss, RSFeedbackType.tlbMiss,
    Mux(io.lsq.dataInvalid,
      RSFeedbackType.dataInvalid,
      RSFeedbackType.mshrFull
    )
  )

  // s2_cache_replay is quite slow to generate, send it separately to LQ
  io.needReplayFromRS := s2_cache_replay

  XSDebug(io.out.fire(), "[DCACHE LOAD RESP] pc %x rdata %x <- D$ %x + fwd %x(%b)\n",
    s2_uop.cf.pc, rdataPartialLoad, io.dcacheResp.bits.data,
    forwardData.asUInt, forwardMask.asUInt
  )

  XSPerfAccumulate("in", io.in.valid)
  XSPerfAccumulate("dcache_miss", io.in.valid && s2_cache_miss)
  XSPerfAccumulate("full_forward", io.in.valid && fullForward)
  XSPerfAccumulate("dcache_miss_full_forward", io.in.valid && s2_cache_miss && fullForward)
  XSPerfAccumulate("replay",  io.rsFeedback.valid && !io.rsFeedback.bits.hit)
  XSPerfAccumulate("replay_tlb_miss", io.rsFeedback.valid && !io.rsFeedback.bits.hit && s2_tlb_miss)
  XSPerfAccumulate("replay_cache", io.rsFeedback.valid && !io.rsFeedback.bits.hit && !s2_tlb_miss && s2_cache_replay)
  XSPerfAccumulate("stall_out", io.out.valid && !io.out.ready)
}

class LoadUnit(implicit p: Parameters) extends XSModule with HasLoadHelper {
  val io = IO(new Bundle() {
    val ldin = Flipped(Decoupled(new ExuInput))
    val ldout = Decoupled(new ExuOutput)
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val rsFeedback = ValidIO(new RSFeedback)
    val rsIdx = Input(UInt(log2Up(IssQueSize).W))
    val isFirstIssue = Input(Bool())
    val dcache = new DCacheLoadIO
    val dtlb = new TlbRequestIO()
    val sbuffer = new LoadForwardQueryIO
    val lsq = new LoadToLsqIO
    val fastUop = ValidIO(new MicroOp) // early wakeup signal generated in load_s1
  })

  val load_s0 = Module(new LoadUnit_S0)
  val load_s1 = Module(new LoadUnit_S1)
  val load_s2 = Module(new LoadUnit_S2)

  load_s0.io.in <> io.ldin
  load_s0.io.dtlbReq <> io.dtlb.req
  load_s0.io.dcacheReq <> io.dcache.req
  load_s0.io.rsIdx := io.rsIdx
  load_s0.io.isFirstIssue := io.isFirstIssue

  PipelineConnect(load_s0.io.out, load_s1.io.in, true.B, load_s0.io.out.bits.uop.roqIdx.needFlush(io.redirect, io.flush))

  load_s1.io.dtlbResp <> io.dtlb.resp
  io.dcache.s1_paddr <> load_s1.io.dcachePAddr
  io.dcache.s1_kill <> load_s1.io.dcacheKill
  load_s1.io.sbuffer <> io.sbuffer
  load_s1.io.lsq <> io.lsq.forward

  PipelineConnect(load_s1.io.out, load_s2.io.in, true.B, load_s1.io.out.bits.uop.roqIdx.needFlush(io.redirect, io.flush))

  load_s2.io.dcacheResp <> io.dcache.resp
  load_s2.io.lsq.forwardData <> io.lsq.forward.forwardData
  load_s2.io.lsq.forwardMask <> io.lsq.forward.forwardMask
  load_s2.io.lsq.forwardMaskFast <> io.lsq.forward.forwardMaskFast // should not be used in load_s2
  load_s2.io.lsq.dataInvalid <> io.lsq.forward.dataInvalid
  load_s2.io.lsq.matchInvalid <> io.lsq.forward.matchInvalid
  load_s2.io.sbuffer.forwardData <> io.sbuffer.forwardData
  load_s2.io.sbuffer.forwardMask <> io.sbuffer.forwardMask
  load_s2.io.sbuffer.forwardMaskFast <> io.sbuffer.forwardMaskFast // should not be used in load_s2
  load_s2.io.sbuffer.dataInvalid <> io.sbuffer.dataInvalid // always false
  load_s2.io.sbuffer.matchInvalid <> io.sbuffer.matchInvalid
  load_s2.io.dataForwarded <> io.lsq.loadDataForwarded
  io.rsFeedback.bits := RegNext(load_s2.io.rsFeedback.bits)
  io.rsFeedback.valid := RegNext(load_s2.io.rsFeedback.valid && !load_s2.io.out.bits.uop.roqIdx.needFlush(io.redirect, io.flush))
  io.lsq.needReplayFromRS := load_s2.io.needReplayFromRS

  // pre-calcuate sqIdx mask in s0, then send it to lsq in s1 for forwarding
  val sqIdxMaskReg = RegNext(UIntToMask(load_s0.io.in.bits.uop.sqIdx.value, StoreQueueSize))
  io.lsq.forward.sqIdxMask := sqIdxMaskReg

  // // use s2_hit_way to select data received in s1
  // load_s2.io.dcacheResp.bits.data := Mux1H(RegNext(io.dcache.s1_hit_way), RegNext(io.dcache.s1_data))
  // assert(load_s2.io.dcacheResp.bits.data === io.dcache.resp.bits.data)

  io.fastUop.valid := io.dcache.s1_hit_way.orR && // dcache hit
    !io.dcache.s1_disable_fast_wakeup &&  // load fast wakeup should be disabled when dcache data read is not ready
    load_s1.io.in.valid && // valid laod request
    !load_s1.io.dcacheKill && // not mmio or tlb miss
    !io.lsq.forward.dataInvalidFast // forward failed
  io.fastUop.bits := load_s1.io.out.bits.uop

  XSDebug(load_s0.io.out.valid,
    p"S0: pc ${Hexadecimal(load_s0.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s0.io.out.bits.uop.lqIdx.asUInt)}, " +
    p"vaddr ${Hexadecimal(load_s0.io.out.bits.vaddr)}, mask ${Hexadecimal(load_s0.io.out.bits.mask)}\n")
  XSDebug(load_s1.io.out.valid,
    p"S1: pc ${Hexadecimal(load_s1.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s1.io.out.bits.uop.lqIdx.asUInt)}, tlb_miss ${io.dtlb.resp.bits.miss}, " +
    p"paddr ${Hexadecimal(load_s1.io.out.bits.paddr)}, mmio ${load_s1.io.out.bits.mmio}\n")

  // writeback to LSQ
  // Current dcache use MSHR
  // Load queue will be updated at s2 for both hit/miss int/fp load
  io.lsq.loadIn.valid := load_s2.io.out.valid
  io.lsq.loadIn.bits := load_s2.io.out.bits

  // write to rob and writeback bus
  val s2_wb_valid = load_s2.io.out.valid && !load_s2.io.out.bits.miss && !load_s2.io.out.bits.mmio

  // Int load, if hit, will be writebacked at s2
  val hitLoadOut = Wire(Valid(new ExuOutput))
  hitLoadOut.valid := s2_wb_valid
  hitLoadOut.bits.uop := load_s2.io.out.bits.uop
  hitLoadOut.bits.data := load_s2.io.out.bits.data
  hitLoadOut.bits.redirectValid := false.B
  hitLoadOut.bits.redirect := DontCare
  hitLoadOut.bits.debug.isMMIO := load_s2.io.out.bits.mmio
  hitLoadOut.bits.debug.isPerfCnt := false.B
  hitLoadOut.bits.debug.paddr := load_s2.io.out.bits.paddr
  hitLoadOut.bits.fflags := DontCare

  load_s2.io.out.ready := true.B

  io.ldout.bits := Mux(hitLoadOut.valid, hitLoadOut.bits, io.lsq.ldout.bits)
  io.ldout.valid := hitLoadOut.valid || io.lsq.ldout.valid

  io.lsq.ldout.ready := !hitLoadOut.valid

  when(io.ldout.fire()){
    XSDebug("ldout %x\n", io.ldout.bits.uop.cf.pc)
  }
}
