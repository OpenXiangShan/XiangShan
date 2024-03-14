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
  val wayLookupWrite    = DecoupledIO(new WayLookupWrite)
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
  val s0_discard, s1_discard, s2_discard    = WireInit(false.B)
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

  /**
    ******************************************************************************
    * send req to ITLB and Meta SRAM
    ******************************************************************************
    */
  for (i <- 0 until PortNumber) {
    toITLB(i).valid             := s0_valid && (if(i == 0) true.B else s0_doubleline)
    toITLB(i).bits              := DontCare
    toITLB(i).bits.size         := 3.U
    toITLB(i).bits.vaddr        := s0_req_vaddr(i)
    toITLB(i).bits.debug.pc     := s0_req_vaddr(i)
    toITLB(i).bits.cmd          := TlbCmd.exec
    toITLB(i).bits.no_translate := false.B
  }
  fromITLB.foreach(_.ready := true.B)
  io.itlb.foreach(_.req_kill := false.B)

  toMeta.valid              := s0_valid
  toMeta.bits.vSetIdx       := s0_req_vSetIdx
  toMeta.bits.isDoubleLine  := s0_doubleline
  toMeta.bits.waymask       := DontCare

  from_bpu_s0_flush := fromFtq.flushFromBpu.shouldFlushByStage2(s0_req_ftqIdx) ||
                       fromFtq.flushFromBpu.shouldFlushByStage3(s0_req_ftqIdx)
  s0_flush := io.flush || from_bpu_s0_flush || s1_flush

  val itlb_hit = !fromITLB(0).bits.miss && (!s0_doubleline || !fromITLB(1).bits.miss)
  val s0_can_go = s1_ready && toITLB(0).ready && toITLB(1).ready && itlb_hit && toMeta.ready
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
  val s1_valid = generatePipeControl(lastFire = s0_fire, thisFire = s1_fire || s1_discard, thisFlush = s1_flush, lastFlush = false.B)

  val s1_req_vaddr  = RegEnable(s0_req_vaddr, s0_fire)
  val s1_doubleline = RegEnable(s0_doubleline, s0_fire)
  val s1_req_ftqIdx = RegEnable(s0_req_ftqIdx, s0_fire)

  val s1_req_vSetIdx = VecInit(s1_req_vaddr.map(get_idx(_)))

  /**
    ******************************************************************************
    * Receive resp from ITLB
    ******************************************************************************
    */
  val tlbRespPAddr  = VecInit((0 until PortNumber).map(i =>
                        ResultHoldBypass(valid = RegNext(s0_fire), data = fromITLB(i).bits.paddr(0))))
  val tlbExcpPF     = VecInit((0 until PortNumber).map(i =>
                        ResultHoldBypass(valid = RegNext(s0_fire), data = fromITLB(i).bits.excp(0).pf.instr)))
  val tlbExcpAF     = VecInit((0 until PortNumber).map(i =>
                        ResultHoldBypass(valid = RegNext(s0_fire), data = fromITLB(i).bits.excp(0).af.instr)))
  val tlbExcp       = VecInit((0 until PortNumber).map(i => tlbExcpAF(i) || tlbExcpPF(i)))

  /**
    ******************************************************************************
    * Receive resp from IMeta and check
    ******************************************************************************
    */
  val s1_req_paddr   = tlbRespPAddr
  val s1_req_ptags    = VecInit(s1_req_paddr.map(get_phy_tag(_)))

  val s1_meta_ptags   = fromMeta.tags
  val s1_meta_valids  = fromMeta.entryValid

  val s1_tag_eq_vec       = VecInit((0 until PortNumber).map( p => VecInit((0 until nWays).map( w => s1_meta_ptags(p)(w) === s1_req_ptags(p)))))
  val s1_tag_match_vec    = VecInit((0 until PortNumber).map( k => VecInit(s1_tag_eq_vec(k).zipWithIndex.map{ case(way_tag_eq, w) => way_tag_eq && s1_meta_valids(k)(w)})))
  val s1_SRAM_hits        = ResultHoldBypass(data = VecInit(s1_tag_match_vec.map(ParallelOR(_))), valid = RegNext(s0_fire))
  val s1_SRAM_waymasks    = s1_tag_match_vec.map(_.asUInt)

  /**
    ******************************************************************************
    * monitor missUint response port
    ******************************************************************************
    */
  val s1_MSHR_match = VecInit((0 until PortNumber).map(i => (s1_req_vSetIdx(i) === fromMSHR.bits.vSetIdx) &&
                                                            (s1_req_ptags(i) === getPhyTagFromBlk(fromMSHR.bits.blkPaddr)) &&
                                                            s1_valid && fromMSHR.valid && !fromMSHR.bits.corrupt))
  val s1_MSHR_ways  = (0 until PortNumber).map(i => ResultHoldBypass(data = fromMSHR.bits.waymask, valid = s1_MSHR_match(i)))
  val s1_MSHR_hits  = (0 until PortNumber).map(i => ValidHoldBypass(s1_MSHR_match(i), s1_fire || s1_flush || s1_discard))

  when(s1_valid) {
    assert(!(s1_SRAM_hits(0) && s1_MSHR_match(0)) && (!s1_doubleline || !(s1_SRAM_hits(1) && s1_MSHR_match(1))),
          "Multi hit in iprefetch s1: vaddr0=0x%x s1_SRAM_hits(0)=%d s1_MSHR_match(0)=%d s1_SRAM_hits(1)=%d s1_MSHR_match(1)=%d",
          s1_req_vaddr(0), s1_SRAM_hits(0), s1_MSHR_match(0), s1_SRAM_hits(1), s1_MSHR_match(1))
  }

  when(s1_fire) {
    assert(PopCount(s1_tag_match_vec(0)) <= 1.U && (PopCount(s1_tag_match_vec(1)) <= 1.U || !s1_doubleline),
      "Multiple hit in main pipe, port0:is=%d,ptag=0x%x,vidx=0x%x,vaddr=0x%x port1:is=%d,ptag=0x%x,vidx=0x%x,vaddr=0x%x ",
      PopCount(s1_tag_match_vec(0)) > 1.U,s1_req_ptags(0), get_idx(s1_req_vaddr(0)), s1_req_vaddr(0),
      PopCount(s1_tag_match_vec(1)) > 1.U && s1_doubleline, s1_req_ptags(1), get_idx(s1_req_vaddr(1)), s1_req_vaddr(1))
  }

  /**
    ******************************************************************************
    * write wayLookup (blocked when wayLookup not ready)
    ******** **********************************************************************
    */
  def update_waymask(mask: UInt, vSetIdx: UInt, ptag: UInt): UInt = {
    require(mask.getWidth == nWays)
    val new_mask  = WireInit(mask)
    val vset_same = (fromMSHR.bits.vSetIdx === vSetIdx) && !fromMSHR.bits.corrupt && fromMSHR.valid
    val ptag_same = getPhyTagFromBlk(fromMSHR.bits.blkPaddr) === ptag
    val way_same  = fromMSHR.bits.waymask === mask
    when(vset_same) {
      when(ptag_same) {
        new_mask := fromMSHR.bits.waymask
      }.elsewhen(way_same) {
        new_mask := 0.U
      }
    }
    new_mask
  }

  val s1_waymasks_reg = RegInit(VecInit(Seq.fill(PortNumber)(0.U(nWays.W))))
  val s1_waymasks = WireInit(s1_waymasks_reg)
  s1_waymasks_reg := s1_waymasks
  (0 until PortNumber).foreach{i =>
    when(RegNext(s0_fire)) {
      s1_waymasks(i) := update_waymask(s1_SRAM_waymasks(i), s1_req_vSetIdx(i), s1_req_ptags(i))
    }.otherwise {
      s1_waymasks(i) := update_waymask(s1_waymasks_reg(i), s1_req_vSetIdx(i), s1_req_ptags(i))
    }
  }

  val has_write = RegInit(false.B)
  when(s0_fire || s1_flush) {
    has_write := false.B
  }.elsewhen(toWayLookup.fire) {
    has_write := true.B
  }

  // disable write wayLookup when SRAM write valid
  toWayLookup.valid             := s1_valid && !has_write && !fromMSHR.valid && !s1_flush
  toWayLookup.bits.ptag         := s1_req_ptags
  toWayLookup.bits.vSetIdx      := s1_req_vSetIdx
  toWayLookup.bits.excp_tlb_af  := tlbExcpAF
  toWayLookup.bits.excp_tlb_pf  := tlbExcpPF
  toWayLookup.bits.waymask      := s1_waymasks

  val s1_need_miss = VecInit(Seq(!s1_SRAM_hits(0) && !s1_MSHR_hits(0) && !tlbExcp(0),
                            !s1_SRAM_hits(1) && !s1_MSHR_hits(1) && !tlbExcp(0) && !tlbExcp(1) && s1_doubleline))

  from_bpu_s1_flush := s1_valid && fromFtq.flushFromBpu.shouldFlushByStage3(s1_req_ftqIdx)
  s1_flush := io.flush || from_bpu_s1_flush

  /** Stage 1 control */
  val s1_has_miss = s1_need_miss.reduce(_||_) && enableBit
  val s1_finish   = has_write || toWayLookup.fire
  s1_ready      := (s2_ready && s1_finish) || !s1_valid
  s1_fire       := s2_ready && s1_valid && s1_finish && s1_has_miss && !s1_flush
  s1_discard    := s1_valid && s1_finish && !s1_has_miss

  /**
    ******************************************************************************
    * IPrefetch Stage 2
    * - 1. PMP check
    * - 2. Monitor the requests from missUnit to write to SRAM.
    * - 3. send req to missUnit
    ******************************************************************************
    */
  val s2_valid  = generatePipeControl(lastFire = s1_fire, thisFire = s2_fire, thisFlush = s2_flush, lastFlush = false.B)

  val s2_req_vaddr    = RegEnable(s1_req_vaddr, s1_fire)
  val s2_doubleline   = RegEnable(s1_doubleline, s1_fire)
  val s2_req_paddr    = RegEnable(s1_req_paddr, s1_fire)
  val s2_need_miss    = RegEnable(s1_need_miss, s1_fire)

  val s2_req_vSetIdx  = s2_req_vaddr.map(get_idx(_))
  val s2_req_ptags    = s2_req_paddr.map(get_phy_tag(_))

  /**
    ******************************************************************************
    * PMP check
    ******************************************************************************
    */
  toPMP.zipWithIndex.map { case (p, i) =>
    p.valid     := s2_valid
    p.bits.addr := s2_req_paddr(i)
    p.bits.size := 3.U // TODO
    p.bits.cmd  := TlbCmd.exec
  }

  val cancel_by_pmp_temp = VecInit(Seq(fromPMP(0).instr || fromPMP(0).mmio,
                                       fromPMP(0).instr || fromPMP(1).instr || fromPMP(0).mmio))
  val cancel_by_pmp = (0 until PortNumber).map(i => ValidHoldBypass(cancel_by_pmp_temp(i), s2_fire || s2_flush))

  /**
    ******************************************************************************
    * Monitor the requests from missUnit to write to SRAM
    ******************************************************************************
    */
  val s2_MSHR_match = VecInit((0 until PortNumber).map(i => (s2_req_vSetIdx(i) === fromMSHR.bits.vSetIdx) &&
                                                            (s2_req_ptags(i) === getPhyTagFromBlk(fromMSHR.bits.blkPaddr)) &&
                                                            s2_valid && fromMSHR.valid && !fromMSHR.bits.corrupt))
  val s2_MSHR_hits = (0 until PortNumber).map(i => ValidHoldBypass(s2_MSHR_match(i), s2_fire || s2_flush))

  /**
    ******************************************************************************
    * send req to missUnit
    ******************************************************************************
    */
  val s2_miss = (0 until PortNumber).map(i => s2_need_miss(i) && !s2_MSHR_hits(i) && !cancel_by_pmp(i))
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
    toMSHRArbiter.io.in(i).valid          := s2_valid && s2_miss(i) && !has_send(i) && !s2_flush
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
  // XSPerfAccumulate("fdip_prefetch_discard_by_fencei",      p0_discard && io.flush)
  // // discard prefetch request by tlb miss
  // XSPerfAccumulate("fdip_prefetch_discard_by_tlb_miss",    p1_discard && p1_tlb_miss)
  // // discard prefetch request by tlb except
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

  /**
    ******************************************************************************
    * Receive resp from IMeta and check
    ******************************************************************************
    */
  // val s1_req_paddr   = tlbRespPAddr
  // val s1_req_ptags    = VecInit(s1_req_paddr.map(get_phy_tag(_)))

  // val meta_valid = (RegNext(s0_fire) || RegNext(s1_need_meta)) && !s1_need_meta

  // val s1_meta_ptags   = fromMeta.tags
  // val s1_meta_valids  = fromMeta.entryValid

  // val s1_tag_eq_vec       = VecInit((0 until PortNumber).map( p => VecInit((0 until nWays).map( w => s1_meta_ptags(p)(w) === s1_req_ptags(p)))))
  // val s1_tag_match_vec    = VecInit((0 until PortNumber).map( k => VecInit(s1_tag_eq_vec(k).zipWithIndex.map{ case(way_tag_eq, w) => way_tag_eq && s1_meta_valids(k)(w)})))
  // val s1_SRAM_ways        = ResultHoldBypass(data = s1_tag_match_vec.map(_.asUInt), valid = meta_valid)
  // val s1_SRAM_hit         = s1_SRAM_ways.map(ParallelOR(_))

  // /**
  //   ******************************************************************************
  //   * monitor missUint response port
  //   ******************************************************************************
  //   */
  // val s1_MSHR_match = VecInit((0 until PortNumber).map(i => (s1_req_vSetIdx(i) === fromMSHR.bits.vSetIdx) &&
  //                                                           (s1_req_ptags(i) === getPhyTagFromBlk(fromMSHR.bits.blkPaddr)) &&
  //                                                           s1_valid && fromMSHR.valid && !fromMSHR.bits.corrupt && tlbRespAllValid))
  // val s1_MSHR_ways  = (0 until PortNumber).map(i => ResultHoldBypass(data = fromMSHR.bits.way, valid = s1_MSHR_match(i)))
  // val s1_MSHR_hits  = (0 until PortNumber).map(i => ValidHoldBypass(s1_MSHR_match(i), s1_fire || s1_flush))

  // /**
  //   ******************************************************************************
  //   * write wayLookup
  //   ******************************************************************************
  //   */
  // val has_write = RegInit(false.B)
  // when(s0_fire || s1_flush) {
  //   has_write := false.B
  // }.elsewhen(toWayLookup.fire) {
  //   has_write := true.B
  // }

  // toWayLookup.valid             := s1_valid && tlbRespAllValid && !has_write && !s1_flush
  // toWayLookup.bits.ptags        := s1_req_ptags
  // toWayLookup.bits.vSetIdxs     := s1_req_vSetIdx
  // toWayLookup.bits.tlb_excp_af  := tlbExcpAF
  // toWayLookup.bits.tlb_excp_pf  := tlbExcpPF
  // toWayLookup.bits.ways         := VecInit((0 until PortNumber).map(i => Mux(s1_MSHR_hit(i), s1_MSHR_ways(i), s1_SRAM_ways(i))))

  // val s1_miss = VecInit(Seq(!s1_SRAM_hit(0) && !s1_MSHR_hit(0) && !tlbExcp(0),
  //                           !s1_SRAM_hit(1) && !s1_MSHR_hit(1) && !tlbExcp(0) && !tlbExcp(1) && s1_doubleline))

  // from_bpu_s1_flush := fromFtq.flushFromBpu.shouldFlushByStage3(s1_req_ftqIdx)
  // s1_flush := io.flush || from_bpu_s1_flush

  // /** Stage 1 control */
  // val s1_finish   = has_write || toWayLookup.fire
  // val s1_need_miss = s1_miss.reduce(_||_)
  // s1_ready      := s2_ready && s1_finish || !s1_valid
  // s1_fire       := s2_ready && s1_valid && s1_finish && s1_need_miss && enableBit && !s1_flush
  // s1_discard    := s1_valid && s1_finish && !s1_need_miss