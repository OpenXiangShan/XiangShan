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

package xiangshan.frontend.icache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.tilelink._
import utils._
import xiangshan.cache.mmu._
import xiangshan.frontend._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}
import huancun.PreferCacheKey
import xiangshan.XSCoreParamsKey
import utility._

abstract class IPrefetchBundle(implicit p: Parameters) extends ICacheBundle
abstract class IPrefetchModule(implicit p: Parameters) extends ICacheModule

class IPredfetchIO(implicit p: Parameters) extends IPrefetchBundle {
  // control
  val csr_pf_enable     = Input(Bool())
  val flush             = Input(Bool())

  val ftqReq            = Flipped(new FtqToPrefetchIO)
  val itlb              = Vec(PortNumber, new TlbRequestIO)
  val pmp               = Vec(PortNumber, new ICachePMPBundle)
  val metaRead          = new ICacheMetaReqBundle
  val MSHRReq           = DecoupledIO(new ICacheMissReq)
  val MSHRResp          = Flipped(ValidIO(new ICacheMissResp))
  val wayLookupWrite    = DecoupledIO(new WayLookupInfo)
}

class IPrefetchPipe(implicit p: Parameters) extends  IPrefetchModule
{
  val io = IO(new IPredfetchIO)

  val fromFtq = io.ftqReq
  val (toITLB,  fromITLB) = (io.itlb.map(_.req), io.itlb.map(_.resp))
  val (toPMP,  fromPMP)   = (io.pmp.map(_.req), io.pmp.map(_.resp))
  val (toMeta,  fromMeta) = (io.metaRead.toIMeta,  io.metaRead.fromIMeta)
  val (toMSHR, fromMSHR)  = (io.MSHRReq, io.MSHRResp)
  val toWayLookup = io.wayLookupWrite

  val enableBit = RegInit(false.B)
  enableBit := io.csr_pf_enable

  val s0_fire, s1_fire, s2_fire             = WireInit(false.B)
  val s0_discard, s2_discard                = WireInit(false.B)
  val s0_ready, s1_ready, s2_ready          = WireInit(false.B)
  val s0_flush, s1_flush, s2_flush          = WireInit(false.B)
  val from_bpu_s0_flush, from_bpu_s1_flush  = WireInit(false.B)

  /**
    ******************************************************************************
    * IPrefetch Stage 0
    * - 1. receive ftq req
    * - 2. send req to ITLB
    * - 3. send req to Meta SRAM
    ******************************************************************************
    */
  val s0_valid  = fromFtq.req.valid

  /**
    ******************************************************************************
    * receive ftq req
    ******************************************************************************
    */
  val s0_req_vaddr    = VecInit(Seq(fromFtq.req.bits.startAddr, fromFtq.req.bits.nextlineStart))
  val s0_req_ftqIdx   = fromFtq.req.bits.ftqIdx
  val s0_doubleline   = fromFtq.req.bits.crossCacheline
  val s0_req_vSetIdx  = s0_req_vaddr.map(get_idx(_))

  from_bpu_s0_flush := fromFtq.flushFromBpu.shouldFlushByStage2(s0_req_ftqIdx) ||
                       fromFtq.flushFromBpu.shouldFlushByStage3(s0_req_ftqIdx)
  s0_flush := io.flush || from_bpu_s0_flush || s1_flush

  val s0_can_go = s1_ready && toITLB(0).ready && toITLB(1).ready && toMeta.ready
  fromFtq.req.ready := s0_can_go

  s0_fire := s0_valid && s0_can_go && !s0_flush

  /**
    ******************************************************************************
    * IPrefetch Stage 1
    * - 1. Receive resp from ITLB
    * - 2. Receive resp from IMeta and check
    * - 3. Monitor the requests from missUnit to write to SRAM.
    * - 4. Wirte wayLookup
    ******************************************************************************
    */
  val s1_valid = generatePipeControl(lastFire = s0_fire, thisFire = s1_fire, thisFlush = s1_flush, lastFlush = false.B)

  val s1_req_vaddr    = RegEnable(s0_req_vaddr, 0.U.asTypeOf(s0_req_vaddr), s0_fire)
  val s1_doubleline   = RegEnable(s0_doubleline, 0.U.asTypeOf(s0_doubleline), s0_fire)
  val s1_req_ftqIdx   = RegEnable(s0_req_ftqIdx, 0.U.asTypeOf(s0_req_ftqIdx), s0_fire)
  val s1_req_vSetIdx  = VecInit(s1_req_vaddr.map(get_idx(_)))

  val m_idle :: m_itlbResend :: m_metaResend :: m_enqWay :: m_enterS2 :: Nil = Enum(5)
  val state = RegInit(m_idle)
  val next_state = WireDefault(state)
  val s0_fire_r = RegNext(s0_fire)
  dontTouch(state)
  dontTouch(next_state)
  state := next_state

  /**
    ******************************************************************************
    * resend itlb req if miss
    ******************************************************************************
    */
  val s1_wait_itlb  = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach { i =>
    when(s1_flush) {
      s1_wait_itlb(i) := false.B
    }.elsewhen(RegNext(s0_fire) && fromITLB(i).bits.miss) {
      s1_wait_itlb(i) := true.B
    }.elsewhen(s1_wait_itlb(i) && !fromITLB(i).bits.miss) {
      s1_wait_itlb(i) := false.B
    }
  }
  val s1_need_itlb    = VecInit(Seq((RegNext(s0_fire) || s1_wait_itlb(0)) && fromITLB(0).bits.miss,
                                    (RegNext(s0_fire) || s1_wait_itlb(1)) && fromITLB(1).bits.miss && s1_doubleline))
  val tlb_valid_pulse = VecInit(Seq((RegNext(s0_fire) || s1_wait_itlb(0)) && !fromITLB(0).bits.miss,
                                    (RegNext(s0_fire) || s1_wait_itlb(1)) && !fromITLB(1).bits.miss && s1_doubleline))
  val tlb_valid_latch = VecInit((0 until PortNumber).map(i => ValidHoldBypass(tlb_valid_pulse(i), s1_fire, flush=s1_flush)))
  val itlb_finish     = tlb_valid_latch(0) && (!s1_doubleline || tlb_valid_latch(1))

  for (i <- 0 until PortNumber) {
    toITLB(i).valid             := s1_need_itlb(i) || (s0_valid && (if(i == 0) true.B else s0_doubleline))
    toITLB(i).bits              := DontCare
    toITLB(i).bits.size         := 3.U
    toITLB(i).bits.vaddr        := Mux(s1_need_itlb(i), s1_req_vaddr(i), s0_req_vaddr(i))
    toITLB(i).bits.debug.pc     := Mux(s1_need_itlb(i), s1_req_vaddr(i), s0_req_vaddr(i))
    toITLB(i).bits.cmd          := TlbCmd.exec
    toITLB(i).bits.no_translate := false.B
  }
  fromITLB.foreach(_.ready := true.B)
  io.itlb.foreach(_.req_kill := false.B)

  /**
    ******************************************************************************
    * Receive resp from ITLB
    ******************************************************************************
    */
  val s1_req_paddr_wire   = VecInit(fromITLB.map(_.bits.paddr(0)))
  val s1_req_paddr_reg    = VecInit((0 until PortNumber).map(i =>
                                RegEnable(s1_req_paddr_wire(i), 0.U(PAddrBits.W), tlb_valid_pulse(i))))
  val s1_req_paddr        = VecInit((0 until PortNumber).map(i => 
                                Mux(tlb_valid_pulse(i), s1_req_paddr_wire(i), s1_req_paddr_reg(i))))
  val s1_req_gpaddr_tmp   = VecInit((0 until PortNumber).map(i =>
                                ResultHoldBypass(valid = tlb_valid_pulse(i), init = 0.U.asTypeOf(fromITLB(i).bits.gpaddr(0)), data = fromITLB(i).bits.gpaddr(0))))
  val itlbExcpPF          = VecInit((0 until PortNumber).map(i =>
                                ResultHoldBypass(valid = tlb_valid_pulse(i), init = 0.U.asTypeOf(fromITLB(i).bits.excp(0).pf.instr), data = fromITLB(i).bits.excp(0).pf.instr)))
  val itlbExcpGPF         = VecInit((0 until PortNumber).map(i =>
                                ResultHoldBypass(valid = tlb_valid_pulse(i), init = 0.U.asTypeOf(fromITLB(i).bits.excp(0).gpf.instr), data = fromITLB(i).bits.excp(0).gpf.instr)))
  val itlbExcpAF          = VecInit((0 until PortNumber).map(i =>
                                ResultHoldBypass(valid = tlb_valid_pulse(i), init = 0.U.asTypeOf(fromITLB(i).bits.excp(0).af.instr), data = fromITLB(i).bits.excp(0).af.instr)))
  val itlbExcp            = VecInit((0 until PortNumber).map(i => itlbExcpAF(i) || itlbExcpPF(i) || itlbExcpGPF(i)))

  /* Select gpaddr with the first gpf
   * Note: the backend wants the base guest physical address of a fetch block
   *       for port(i), its base gpaddr is actually (gpaddr - i * blocksize)
   *       see GPAMem: https://github.com/OpenXiangShan/XiangShan/blob/344cf5d55568dd40cd658a9ee66047a505eeb504/src/main/scala/xiangshan/backend/GPAMem.scala#L33-L34
   *       see also: https://github.com/OpenXiangShan/XiangShan/blob/344cf5d55568dd40cd658a9ee66047a505eeb504/src/main/scala/xiangshan/frontend/IFU.scala#L374-L375
   */
  val s1_req_gpaddr = PriorityMuxDefault(
    itlbExcpGPF zip (0 until PortNumber).map(i => s1_req_gpaddr_tmp(i) - (i << blockOffBits).U),
    0.U.asTypeOf(s1_req_gpaddr_tmp(0))
  )

  /**
    ******************************************************************************
    * resend metaArray read req when itlb miss finish
    ******************************************************************************
    */
  val s1_need_meta = ((state === m_itlbResend) && itlb_finish) || (state === m_metaResend)
  toMeta.valid              := s1_need_meta || s0_valid
  toMeta.bits               := DontCare
  toMeta.bits.isDoubleLine  := Mux(s1_need_meta, s1_doubleline, s0_doubleline)

  for (i <- 0 until PortNumber) {
    toMeta.bits.vSetIdx(i)  := Mux(s1_need_meta, s1_req_vSetIdx(i), s0_req_vSetIdx(i))
  }

  /**
    ******************************************************************************
    * Receive resp from IMeta and check
    ******************************************************************************
    */
  val s1_req_ptags    = VecInit(s1_req_paddr.map(get_phy_tag(_)))

  val s1_meta_ptags   = fromMeta.tags
  val s1_meta_valids  = fromMeta.entryValid
  val s1_meta_errors = VecInit((0 until PortNumber).map( p =>
    // If error is found in either way, the tag_eq_vec is unreliable, so we do not use waymask, but directly .orR
    fromMeta.errors(p).asUInt.orR
  ))

  def get_waymask(paddrs: Vec[UInt]): Vec[UInt] = {
    val ptags         = paddrs.map(get_phy_tag(_))
    val tag_eq_vec    = VecInit((0 until PortNumber).map( p => VecInit((0 until nWays).map( w => s1_meta_ptags(p)(w) === ptags(p)))))
    val tag_match_vec = VecInit((0 until PortNumber).map( k => VecInit(tag_eq_vec(k).zipWithIndex.map{ case(way_tag_eq, w) => way_tag_eq && s1_meta_valids(k)(w)})))
    val waymasks      = VecInit(tag_match_vec.map(_.asUInt))
    waymasks
  }

  val s1_SRAM_waymasks = VecInit((0 until PortNumber).map(i =>
                            Mux(tlb_valid_pulse(i), get_waymask(s1_req_paddr_wire)(i), get_waymask(s1_req_paddr_reg)(i))))

  /**
    ******************************************************************************
    * update waymask according to MSHR update data
    ******************************************************************************
    */
  def update_waymask(mask: UInt, vSetIdx: UInt, ptag: UInt): UInt = {
    require(mask.getWidth == nWays)
    val new_mask  = WireInit(mask)
    val valid = fromMSHR.valid && !fromMSHR.bits.corrupt
    val vset_same = fromMSHR.bits.vSetIdx === vSetIdx
    val ptag_same = getPhyTagFromBlk(fromMSHR.bits.blkPaddr) === ptag
    val way_same  = fromMSHR.bits.waymask === mask
    when(valid && vset_same) {
      when(ptag_same) { 
        new_mask := fromMSHR.bits.waymask
      }.elsewhen(way_same) {
        new_mask := 0.U
      }
    }
    new_mask
  }

  val s1_SRAM_valid = s0_fire_r || RegNext(s1_need_meta && toMeta.ready)
  val s1_MSHR_valid = fromMSHR.valid && !fromMSHR.bits.corrupt
  val s1_waymasks   = WireInit(VecInit(Seq.fill(PortNumber)(0.U(nWays.W))))
  val s1_waymasks_r = RegEnable(s1_waymasks, 0.U.asTypeOf(s1_waymasks), s1_SRAM_valid || s1_MSHR_valid)
  (0 until PortNumber).foreach{i =>
    val old_waymask = Mux(s1_SRAM_valid, s1_SRAM_waymasks(i), s1_waymasks_r(i))
    s1_waymasks(i) := update_waymask(old_waymask, s1_req_vSetIdx(i), s1_req_ptags(i))
  }

  /**
    ******************************************************************************
    * send enqueu req to WayLookup
    ******** **********************************************************************
    */
  // Disallow enqueuing wayLookup when SRAM write occurs.
  toWayLookup.valid             := ((state === m_enqWay) || ((state === m_idle) && itlb_finish)) && !s1_flush && !fromMSHR.valid
  toWayLookup.bits.vSetIdx      := s1_req_vSetIdx
  toWayLookup.bits.waymask      := s1_waymasks
  toWayLookup.bits.ptag         := s1_req_ptags
  toWayLookup.bits.gpaddr       := s1_req_gpaddr
  (0 until PortNumber).foreach { i =>
    val excpValid = (if (i == 0) true.B else s1_doubleline)  // exception in first line is always valid, in second line is valid iff is doubleline request
    toWayLookup.bits.excp_tlb_af(i)  := excpValid && itlbExcpAF(i)
    toWayLookup.bits.excp_tlb_pf(i)  := excpValid && itlbExcpPF(i)
    toWayLookup.bits.excp_tlb_gpf(i) := excpValid && itlbExcpGPF(i)
    toWayLookup.bits.meta_errors(i)  := excpValid && s1_meta_errors(i)
  }

  val s1_waymasks_vec = s1_waymasks.map(_.asTypeOf(Vec(nWays, Bool())))
  when(toWayLookup.fire) {
    assert(PopCount(s1_waymasks_vec(0)) <= 1.U && (PopCount(s1_waymasks_vec(1)) <= 1.U || !s1_doubleline),
      "Multiple hit in main pipe, port0:is=%d,ptag=0x%x,vidx=0x%x,vaddr=0x%x port1:is=%d,ptag=0x%x,vidx=0x%x,vaddr=0x%x ",
      PopCount(s1_waymasks_vec(0)) > 1.U, s1_req_ptags(0), get_idx(s1_req_vaddr(0)), s1_req_vaddr(0),
      PopCount(s1_waymasks_vec(1)) > 1.U && s1_doubleline, s1_req_ptags(1), get_idx(s1_req_vaddr(1)), s1_req_vaddr(1))
  }

  /**
    ******************************************************************************
    * PMP check
    ******************************************************************************
    */
  toPMP.zipWithIndex.map { case (p, i) =>
    p.valid     := s1_valid
    p.bits.addr := s1_req_paddr(i)
    p.bits.size := 3.U // TODO
    p.bits.cmd  := TlbCmd.exec
  }
  val pmpExcp = VecInit(Seq(fromPMP(0).instr || fromPMP(0).mmio,
                             fromPMP(0).instr || fromPMP(1).instr || fromPMP(0).mmio))

  /**
    ******************************************************************************
    * state machine
    ******** **********************************************************************
    */

  switch(state) {
    is(m_idle) {
      when(s1_valid && !itlb_finish) {
        next_state := m_itlbResend
      }.elsewhen(s1_valid && itlb_finish && !toWayLookup.fire) {
        next_state := m_enqWay
      }.elsewhen(s1_valid && itlb_finish && toWayLookup.fire && !s2_ready) {
        next_state := m_enterS2
      }
    }
    is(m_itlbResend) {
      when(itlb_finish && !toMeta.ready) {
        next_state := m_metaResend
      }.elsewhen(itlb_finish && toMeta.ready) {
        next_state := m_enqWay
      }
    }
    is(m_metaResend) {
      when(toMeta.ready) {
        next_state := m_enqWay
      }
    }
    is(m_enqWay) {
      when(toWayLookup.fire && !s2_ready) {
        next_state := m_enterS2
      }.elsewhen(toWayLookup.fire && s2_ready) {
        next_state := m_idle
      }
    }
    is(m_enterS2) {
      when(s2_ready) {
        next_state := m_idle
      }
    }
  }

  when(s1_flush) {
    next_state := m_idle
  }

  /** Stage 1 control */
  from_bpu_s1_flush := s1_valid && fromFtq.flushFromBpu.shouldFlushByStage3(s1_req_ftqIdx)
  s1_flush := io.flush || from_bpu_s1_flush

  s1_ready      := next_state === m_idle
  s1_fire       := (next_state === m_idle) && s1_valid && !s1_flush

  /**
    ******************************************************************************
    * IPrefetch Stage 2
    * - 1. Monitor the requests from missUnit to write to SRAM.
    * - 2. send req to missUnit
    ******************************************************************************
    */
  val s2_valid  = generatePipeControl(lastFire = s1_fire, thisFire = s2_fire, thisFlush = s2_flush, lastFlush = false.B)

  val s2_req_vaddr    = RegEnable(s1_req_vaddr, 0.U.asTypeOf(s1_req_vaddr), s1_fire)
  val s2_doubleline   = RegEnable(s1_doubleline, 0.U.asTypeOf(s1_doubleline), s1_fire)
  val s2_req_paddr    = RegEnable(s1_req_paddr, 0.U.asTypeOf(s1_req_paddr), s1_fire)

  val s2_pmpExcp      = RegEnable(pmpExcp, 0.U.asTypeOf(pmpExcp), s1_fire)
  val s2_itlbExcp     = RegEnable(itlbExcp, 0.U.asTypeOf(itlbExcp), s1_fire)
  val s2_waymasks     = RegEnable(s1_waymasks, 0.U.asTypeOf(s1_waymasks), s1_fire)

  val s2_req_vSetIdx  = s2_req_vaddr.map(get_idx(_))
  val s2_req_ptags    = s2_req_paddr.map(get_phy_tag(_))

  /**
    ******************************************************************************
    * Monitor the requests from missUnit to write to SRAM
    ******************************************************************************
    */
  val s2_MSHR_match = VecInit((0 until PortNumber).map(i => (s2_req_vSetIdx(i) === fromMSHR.bits.vSetIdx) &&
                                                            (s2_req_ptags(i) === getPhyTagFromBlk(fromMSHR.bits.blkPaddr)) &&
                                                            s2_valid && fromMSHR.valid && !fromMSHR.bits.corrupt))
  val s2_MSHR_hits = (0 until PortNumber).map(i => ValidHoldBypass(s2_MSHR_match(i), s2_fire || s2_flush))

  val s2_hits = s2_waymasks.map(_.orR)
  val s2_miss = VecInit(Seq(!s2_itlbExcp(0) && !s2_pmpExcp(0) && !s2_hits(0) && !s2_MSHR_hits(0),
                            !s2_itlbExcp(0) && !s2_pmpExcp(0) && !s2_itlbExcp(1) && !s2_pmpExcp(1) && !s2_hits(1) && !s2_MSHR_hits(1) && s2_doubleline))

  /**
    ******************************************************************************
    * send req to missUnit
    ******************************************************************************
    */
  val toMSHRArbiter = Module(new Arbiter(new ICacheMissReq, PortNumber))
  
  // To avoid sending duplicate requests.
  val has_send = RegInit(VecInit(Seq.fill(2)(false.B)))
  (0 until PortNumber).foreach{ i =>
    when(s1_fire) {
      has_send(i) := false.B
    }.elsewhen(toMSHRArbiter.io.in(i).fire) {
      has_send(i) := true.B
    }
  }

  (0 until PortNumber).map{ i =>
    toMSHRArbiter.io.in(i).valid          := s2_valid && s2_miss(i) && !has_send(i)
    toMSHRArbiter.io.in(i).bits.blkPaddr  := getBlkAddr(s2_req_paddr(i))
    toMSHRArbiter.io.in(i).bits.vSetIdx   := s2_req_vSetIdx(i)
  }

  toMSHR <> toMSHRArbiter.io.out

  s2_flush := io.flush

  val s2_finish  = (0 until PortNumber).map(i => has_send(i) || !s2_miss(i) || toMSHRArbiter.io.in(i).fire).reduce(_&&_)
  s2_ready      := s2_finish || !s2_valid
  s2_fire       := s2_valid && s2_finish && !s2_flush

  /** PerfAccumulate */
  // the number of prefetch request received from ftq
  XSPerfAccumulate("prefetch_req_receive", fromFtq.req.fire)
  // the number of prefetch request sent to missUnit
  XSPerfAccumulate("prefetch_req_send", toMSHR.fire)
  XSPerfAccumulate("to_missUnit_stall", toMSHR.valid && !toMSHR.ready)
  /**
    * Count the number of requests that are filtered for various reasons.
    * The number of prefetch discard in Performance Accumulator may be
    * a littel larger the number of really discarded. Because there can
    * be multiple reasons for a canceled request at the same time.
    */
  // discard prefetch request by flush
  // XSPerfAccumulate("fdip_prefetch_discard_by_tlb_except",  p1_discard && p1_tlb_except)
  // // discard prefetch request by hit icache SRAM
  // XSPerfAccumulate("fdip_prefetch_discard_by_hit_cache",   p2_discard && p1_meta_hit)
  // // discard prefetch request by hit wirte SRAM
  // XSPerfAccumulate("fdip_prefetch_discard_by_p1_monoitor", p1_discard && p1_monitor_hit)
  // // discard prefetch request by pmp except or mmio
  // XSPerfAccumulate("fdip_prefetch_discard_by_pmp",         p2_discard && p2_pmp_except)
  // // discard prefetch request by hit mainPipe info
  // // XSPerfAccumulate("fdip_prefetch_discard_by_mainPipe",    p2_discard && p2_mainPipe_hit)
}