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

import chipsalliance.rocketchip.config.Parameters
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

//TODO: remove this
object DebugFlags {
  val fdip = false
}

class PIQData(implicit p: Parameters) extends IPrefetchBundle {
  val ptage = UInt(tagBits.W)
  val vSetIdx = UInt(idxBits.W)
  val cacheline = UInt(blockBits.W)
  val writeBack = Bool()
}

class PIQToMainPipe(implicit  p: Parameters) extends IPrefetchBundle{
  val info = DecoupledIO(new PIQData)
}

class PrefetchReq(implicit  p: Parameters) extends IPrefetchBundle{
  val paddr     = UInt(PAddrBits.W)
  val vSetIdx   = UInt(idxBits.W)
}

class PrefetchBufferIO(implicit p: Parameters) extends IPrefetchBundle {
  val hartId = Input(UInt(8.W))
  val fencei = Input(Bool())
  val IPFFilterRead   = new IPFBufferFilterRead
  val IPFBufferRead   = new IPFBufferRead
  val IPFBufferWrite  = Flipped(DecoupledIO(new IPFBufferWrite))
  val metaWrite       = DecoupledIO(new ICacheMetaWriteBundle)
  val dataWrite       = DecoupledIO(new ICacheDataWriteBundle)
  val IPFReplacer     = new IPFReplacer
  val ipfRecentWrite  = Output(Vec(2, new FilterInfo))
}

class PrefetchBuffer(implicit p: Parameters) extends IPrefetchModule
{
  val io = IO(new PrefetchBufferIO)
  
  val fromIPrefetch   = io.IPFFilterRead.req
  val toIPrefetch     = io.IPFFilterRead.resp
  val fromMainPipe    = io.IPFBufferRead.req
  val toMainPipe      = io.IPFBufferRead.resp
  val fromPIQ         = io.IPFBufferWrite
  val toICacheMeta    = io.metaWrite
  val toICacheData    = io.dataWrite
  val fromReplacer    = io.IPFReplacer
  val toIPrefetchInfo = io.ipfRecentWrite

  class IPFBufferEntryMeta(implicit p: Parameters) extends IPrefetchBundle
  {
    val valid         = Bool()
    val paddr         = UInt(PAddrBits.W)
    val vSetIdx       = UInt(idxBits.W)
    val confidence    = UInt(log2Ceil(maxIPFMoveConf + 1).W)
  }

  class IPFBufferEntryData(implicit p: Parameters) extends IPrefetchBundle
  {
    val cacheline = UInt(blockBits.W)
  }

  val meta_buffer = InitQueue(new IPFBufferEntryMeta, size = nPrefBufferEntries)
  val data_buffer = InitQueue(new IPFBufferEntryData, size = nPrefBufferEntries)

  /**
    ******************************************************************************
    * handle look-up request from IPrefetchPipe
    * - 1. Receive request from IPrefetch
    * - 2. Look up whether hit in meta_buffer
    * - 3. Send response to IPrefetch at the same cycle
    ******************************************************************************
    */
  val fr_block  = get_block(fromIPrefetch.paddr)
  val fr_hit_oh = meta_buffer.map(e => e.valid && (get_block(e.paddr) === fr_block))
  toIPrefetch.ipf_hit := ParallelOR(fr_hit_oh)
  // only hit one entry in meta_buffer at the same time
  assert(PopCount(fr_hit_oh) <= 1.U, "More than 1 hit in L1 prefetch buffer for filter")

  /**
    ******************************************************************************
    * handle read request from ICacheMainPipe
    * - 1. Receive request from ICacheMainPipe
    * - 2. Look up whether hit in meta_buffer and get hitted data
    * - 3. Send response to ICacheMainPipe at the same cycle
    ******************************************************************************
    */
  fromMainPipe.foreach(_.ready := DontCare)
  val r_blocks      = fromMainPipe.map (e => get_block(e.bits.paddr))
  val r_hit_oh      = r_blocks.map (ptag => meta_buffer.map(e => e.valid && (get_block(e.paddr) === ptag)))
  val curr_hit_ptr  = r_hit_oh.map (OHToUInt(_))
  val r_hit_data    = r_hit_oh.map (oh => Mux1H(oh, data_buffer.map(_.cacheline)))
  val r_hit         = (0 until PortNumber).map (i => ParallelOR(r_hit_oh(i)) && fromMainPipe(i).valid)
  (0 until PortNumber).foreach (i => {
    toMainPipe(i).ipf_hit   := r_hit(i)
    toMainPipe(i).cacheline := r_hit_data(i)
    // only hit one entry in meta_buffer
    assert(PopCount(r_hit_oh(i)) <= 1.U, "More than 1 hit in L1 prefetch buffer")
    XSPerfAccumulate("fdip_mainpipe_hit_in_ipf_" + i, r_hit(i))
  })
  

  /**
    ******************************************************************************
    * move the entry arriving max confidence to icache
    * - 1. Chose a moved entry from the entries that reach max confidence (LSB first)
    * - 2. Get victim way of array from replacer of ICacheMainPipe
    * - 3. Send write req to metaArray and dataArray and ensure fire together
    ******************************************************************************
    */
  val move_oh   = meta_buffer.map (e => e.confidence === maxIPFMoveConf.U && e.valid)
  val move_need = ParallelOR(move_oh)
  val curr_move_ptr  = PriorityEncoder(move_oh)

  fromReplacer.vsetIdx := meta_buffer(curr_move_ptr).vSetIdx

  toICacheMeta.valid := move_need
  toICacheMeta.bits.generate(
    tag     = get_phy_tag(meta_buffer(curr_move_ptr).paddr),
    idx     = meta_buffer(curr_move_ptr).vSetIdx,
    waymask = fromReplacer.waymask,
    bankIdx = meta_buffer(curr_move_ptr).vSetIdx(0))

  toICacheData.valid := move_need
  toICacheData.bits.generate(
    data    = data_buffer(curr_move_ptr).cacheline,
    idx     = meta_buffer(curr_move_ptr).vSetIdx,
    waymask = fromReplacer.waymask,
    bankIdx = meta_buffer(curr_move_ptr).vSetIdx(0),
    paddr   = meta_buffer(curr_move_ptr).paddr)

  // TODO: how to ensure
  assert((toICacheMeta.fire && toICacheData.fire) || (!toICacheMeta.fire && !toICacheData.fire),
    "meta and data array need fire at same time")

  XSPerfAccumulate("fdip_move_to_icache", toICacheMeta.fire && toICacheData.fire)

  when(toICacheMeta.fire) {
    XSDebug(p"PrefetchBuffer: write prefetch data to icache . vSetIdx:${meta_buffer(curr_move_ptr).vSetIdx} " +
            p"paddr:${meta_buffer(curr_move_ptr).paddr}")
  }

  /**
    ******************************************************************************
    * Prefetch buffer write logic
    * Read old data when read and write occur in the same cycle
    * There are 4 situations that can cause meta_buffer and data_buffer to be written
    * - 1. ICacheMainPipe read hit that need to change some status
    * - 2. Move prefetch cacheline to icache
    * - 3. Receive write request from ICacheMissUnit
    * - 4. Receive fencei req that need to flush all buffer
    * Priority: 4 > 3 > 2 > 1
    * The order of the code determines the priority, with the following priority being higher
    ******************************************************************************
    */

  /** 1. confidence++ for every ICacheMainPipe hit */
  (0 until PortNumber) .map (i =>
    when(r_hit(i)) {
      // handle overflow
      when(meta_buffer(curr_hit_ptr(i)).confidence =/= maxIPFMoveConf.U) {
        meta_buffer(curr_hit_ptr(i)).confidence := meta_buffer(curr_hit_ptr(i)).confidence + 1.U
      }
    }
  )

  /** 2. set entry invalid after move to icache */
  when(toICacheMeta.fire) {
    meta_buffer(curr_move_ptr).valid := false.B
  }

  /** 3. write prefetch data to entry */
  // try to get a free entry ptr  
  val free_oh       = meta_buffer.map (_.valid === false.B)
  val has_free      = ParallelOR(free_oh)
  val ptr_from_free = PriorityEncoder(free_oh)
  // get a ptr from random replacement policy
  val replacer = ReplacementPolicy.fromString(Some("random"), nPrefBufferEntries)
  // write to free entry if has, otherwise random entry
  val curr_write_ptr = Mux(has_free, ptr_from_free, replacer.way)
  fromPIQ.ready := DontCare
  when(fromPIQ.valid) {
    meta_buffer(curr_write_ptr).valid         := true.B
    meta_buffer(curr_write_ptr).confidence    := fromPIQ.bits.has_hit.asUInt
    meta_buffer(curr_write_ptr).paddr         := fromPIQ.bits.paddr
    meta_buffer(curr_write_ptr).vSetIdx       := fromPIQ.bits.vSetIdx
    data_buffer(curr_write_ptr).cacheline     := fromPIQ.bits.cacheline
  }

  if(DebugFlags.fdip) {
    when(fromPIQ.valid) {
      printf(p"PrefetchBuffer: receive prefetch data. vSetIdx:${fromPIQ.bits.vSetIdx} paddr:${fromPIQ.bits.paddr}")
    }
  }

  XSPerfAccumulate("fdip_vitcm_used",     !has_free && fromPIQ.valid && (meta_buffer(curr_write_ptr).confidence =/= 0.U))
  XSPerfAccumulate("fdip_vitcm_no_used",  !has_free && fromPIQ.valid && (meta_buffer(curr_write_ptr).confidence === 0.U))

  /** 4. flush all prefetch buffer when fencei */
  when(io.fencei) {
    meta_buffer.foreach { b =>
      b.valid         := false.B
      b.confidence    := 0.U
    }
  }

  /**
    ******************************************************************************
    * Register 2 cycle meta write info for IPrefetchPipe filter
    ******************************************************************************
    */
  val meta_write_buffer = InitQueue(new FilterInfo, size = 2)
  meta_write_buffer(0).valid := toICacheMeta.fire
  meta_write_buffer(0).paddr := meta_buffer(curr_move_ptr).paddr
  meta_write_buffer(1)       := meta_write_buffer(0)
  (0 until PortNumber).foreach (i => {
    toIPrefetchInfo(i) := meta_write_buffer(i)
  })

  XSPerfAccumulate("fdip_fencei_cycle", io.fencei)

  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffRefillEvent)
    difftest.clock   := clock
    difftest.coreid  := io.hartId
    difftest.index   := 6.U
    difftest.valid   := toICacheData.fire
    difftest.addr    := toICacheData.bits.paddr
    difftest.data    := toICacheData.bits.data.asTypeOf(difftest.data)
  }
}

class IPredfetchIO(implicit p: Parameters) extends IPrefetchBundle {
  val ftqReq              = Flipped(new FtqPrefechBundle)
  val iTLBInter           = new TlbRequestIO
  val pmp                 = new ICachePMPBundle
  val metaReadReq         = Decoupled(new ICacheMetaReadReqBundle)
  val metaReadResp        = Input(new ICacheMetaReadRespBundle)
  val prefetchReq         = DecoupledIO(new PrefetchReq)

  val IPFFilterRead       = Flipped(new IPFBufferFilterRead)
  val PIQFilterRead       = Flipped(new PIQFilterRead)

  val ipfRecentWrite      = Input(Vec(2, new FilterInfo))
  val ICacheMissUnitInfo  = Flipped(new ICacheMissUnitInfo)
  val ICacheMainPipeInfo  = Flipped(new ICacheMainPipeInfo)

  val fencei              = Input(Bool())
}

class IPrefetchPipe(implicit p: Parameters) extends  IPrefetchModule
{
  val io = IO(new IPredfetchIO)

  val enableBit = RegInit(false.B)
  enableBit := enableICachePrefetch.B

  val fromFtq = io.ftqReq
  val (toITLB,  fromITLB) = (io.iTLBInter.req, io.iTLBInter.resp)
  val (toIMeta, fromIMeta, fromIMetaValid) = (io.metaReadReq, io.metaReadResp.metaData, io.metaReadResp.entryValid)
  val (toIPFBuffer, fromIPFBuffer) = (io.IPFFilterRead.req, io.IPFFilterRead.resp)
  val (toPIQ, fromPIQ) = (io.PIQFilterRead.req, io.PIQFilterRead.resp)
  val (toPMP,  fromPMP)   = (io.pmp.req, io.pmp.resp)
  val toPIQEnqReq = io.prefetchReq

  val fromIPFInfo      = io.ipfRecentWrite
  val fromMainPipeInfo = io.ICacheMainPipeInfo
  val fromMissUnitInfo = io.ICacheMissUnitInfo

  val p0_fire, p1_fire, p2_fire           = WireInit(false.B)
  val p0_discard, p1_discard, p2_discard  = WireInit(false.B)
  val p0_ready, p1_ready, p2_ready        = WireInit(false.B)

  /**
    ******************************************************************************
    * IPrefetch Stage 0
    * - 1. send req to IMeta
    * - 2. send req to ITLB (no blocked)
    * - 3. check last req
    ******************************************************************************
    */
  val p0_valid  = fromFtq.req.valid

  val p0_vaddr      = addrAlign(fromFtq.req.bits.target, blockBytes, VAddrBits)
  val p0_vaddr_reg  = RegEnable(p0_vaddr, p0_fire)
  val p0_req_cancel = Wire(Bool())

  /** 1. send req to IMeta */
  toIMeta.valid     := p0_valid && !p0_req_cancel
  toIMeta.bits.idx  := get_idx(p0_vaddr)

  /** 2. send req to ITLB (no blocked) */
  toITLB.valid                    := p0_valid && !p0_req_cancel
  toITLB.bits.size                := 3.U // TODO: fix the size
  toITLB.bits.vaddr               := p0_vaddr
  toITLB.bits.debug.pc            := p0_vaddr
  toITLB.bits.kill                := DontCare
  toITLB.bits.cmd                 := TlbCmd.exec
  toITLB.bits.debug.robIdx        := DontCare
  toITLB.bits.debug.isFirstIssue  := DontCare
  toITLB.bits.memidx              := DontCare
  toITLB.bits.no_translate        := false.B
  fromITLB.ready                  := true.B
  // TODO: whether to handle tlb miss for prefetch
  io.iTLBInter.req_kill           := true.B

  /** FTQ request port */
  fromFtq.req.ready := p0_ready

  /** 3. Check last req: request from FTQ is same as last time or behind */
  val p0_hit_behind = Wire(Bool())

  /** stage 0 control */
  // Cancel request when prefetch not enable or the request from FTQ is same as last time
  p0_req_cancel := !enableBit || p0_hit_behind || io.fencei
  p0_ready      := p0_req_cancel || p1_ready && toITLB.ready && toIMeta.ready
  p0_fire       := p0_valid && p1_ready && toITLB.ready && toIMeta.ready && enableBit && !p0_req_cancel
  p0_discard    := p0_valid && p0_req_cancel

  if(DebugFlags.fdip) {
    when(p0_discard) {
      printf(p"IPrefetchPipe: discard req in s0.")
    }
  }

  /**
    ******************************************************************************
    * IPrefetch Stage 1
    * - 1. Receive resp from ITLB (no blocked)
    * - 2. Receive resp from IMeta
    * - 3. Check MSHR
    ******************************************************************************
    */
  val p1_valid  = generatePipeControl(lastFire = p0_fire, thisFire = p1_fire || p1_discard, thisFlush = false.B, lastFlush = false.B)

  val p1_vaddr      = RegEnable(p0_vaddr, p0_fire)
  val p1_req_cancel = Wire(Bool())

  /** 1. Receive resp from ITLB (no blocked) */
  val p1_tlb_resp_miss  = ResultHoldBypass(valid = RegNext(p0_fire), data = fromITLB.bits.miss)
  val p1_tlb_resp_paddr = ResultHoldBypass(valid = RegNext(p0_fire), data = fromITLB.bits.paddr(0))
  val p1_tlb_resp_pf    = ResultHoldBypass(valid = RegNext(p0_fire), data = fromITLB.bits.excp(0).pf.instr)
  val p1_tlb_resp_af    = ResultHoldBypass(valid = RegNext(p0_fire), data = fromITLB.bits.excp(0).af.instr)
  val p1_exception      = VecInit(Seq(p1_tlb_resp_pf, p1_tlb_resp_af))
  val p1_has_except     = p1_exception.reduce(_ || _)
  val p1_paddr          = p1_tlb_resp_paddr

  /** 2. Receive resp from IMeta. Register the data first because of strict timing. */
  val p1_meta_ptags_reg   = RegEnable(VecInit(fromIMeta.map(way => way.tag)), RegNext(p0_fire))
  val p1_meta_valids_reg  = RegEnable(fromIMetaValid, RegNext(p0_fire))

  /** Stage 1 control */
  p1_req_cancel := p1_tlb_resp_miss || p1_has_except || io.fencei
  p1_ready      := p1_valid && p2_ready || !p1_valid
  p1_fire       := p1_valid && !p1_req_cancel && p2_ready && enableBit
  p1_discard    := p1_valid && p1_req_cancel

  when(p1_discard) {
    XSDebug(p"IPrefetchPipe: discard req in s1. vaddr:${p1_vaddr}")
  }

  /**
    ******************************************************************************
    * IPrefetch Stage 2
    * - 1. IMeta Check: check whether req hit in icache
    * - 2. PMP Check: send req and receive resq in the same cycle
    * - 3. Prefetch buffer Check (if prefetch to L1)
    * - 4. Prefetch Queue Check 
    * - 5. Prefetch Buffer Recently write chcek
    * - 6. MainPipeInfo chcek
    * - 7. MissUnitInfo chcek
    * - 8. Recently meta write check
    ******************************************************************************
    */
  val p2_valid  = generatePipeControl(lastFire = p1_fire, thisFire = p2_fire || p2_discard, thisFlush = false.B, lastFlush = false.B)
  
  val p2_paddr      = RegEnable(p1_paddr, p1_fire)
  val p2_vaddr      = RegEnable(p1_vaddr, p1_fire)
  val p2_req_cancel = Wire(Bool())
  val p2_vidx       = get_idx(p2_vaddr)

  p0_hit_behind := (p0_vaddr === p1_vaddr) || (p0_vaddr === p2_vaddr)

  /** 1. IMeta Check */
  val p2_ptag           = get_phy_tag(p2_paddr)
  val p2_tag_eq_vec     = VecInit(p1_meta_ptags_reg.map(_  ===  p2_ptag ))
  val p2_tag_match_vec  = VecInit(p2_tag_eq_vec.zipWithIndex.map{ case(way_tag_eq, w) => way_tag_eq && p1_meta_valids_reg(w)})
  val p2_tag_match      = DataHoldBypass(ParallelOR(p2_tag_match_vec), RegNext(p1_fire))

  /** 2. PMP Check */
  // TODO: When fromPMP.mmio is low but fromPMP.instr is high, which is a except?
  val p2_exception  = DataHoldBypass((fromPMP.mmio && !fromPMP.instr) || fromPMP.instr, RegNext(p1_fire))
  // PMP request
  toPMP.valid     := p2_valid
  toPMP.bits.addr := p2_paddr
  toPMP.bits.size := 3.U
  toPMP.bits.cmd  := TlbCmd.exec

  /** 3. Prefetch buffer Check */
  toIPFBuffer.paddr := p2_paddr
  val p2_hit_buffer = fromIPFBuffer.ipf_hit

  /** 4. Prefetch Queue Check */
  toPIQ.paddr := p2_paddr
  val p2_hit_piq = fromPIQ.piq_hit

  /** 5. Prefetch Buffer Recently write chcek */
  val p2_check_ipf_info = VecInit(fromIPFInfo.map(info =>
        info.valid && getBlkPaddr(info.paddr) === getBlkPaddr(p2_paddr))).reduce(_||_)

  /** 6. MainPipeInfo chcek */
  val check_mp_s1 = VecInit(fromMainPipeInfo.s1Info.map(info =>
        info.valid && getBlkPaddr(info.paddr) === getBlkPaddr(p2_paddr))).reduce(_||_)
  val check_mp_s2 = VecInit(fromMainPipeInfo.s2Info.map(info =>
        info.valid && getBlkPaddr(info.paddr) === getBlkPaddr(p2_paddr))).reduce(_||_)
  val check_mp_missSlot = VecInit(fromMainPipeInfo.missSlot.map(info =>
        info.valid && info.ptag === get_phy_tag(p2_paddr) && info.vSetIdx === p2_vidx)).reduce(_||_)
  val p2_check_mp_info = check_mp_s1 || check_mp_s2 || check_mp_missSlot

  /** 7. MissUnitInfo chcek */
  val check_mu_mshr = VecInit(fromMissUnitInfo.mshr.map(info =>
        info.valid && getBlkPaddr(info.paddr) === getBlkPaddr(p2_paddr))).reduce(_||_)
  val check_mu_recent_write = VecInit(fromMissUnitInfo.recentWrite.map(info =>
        info.valid && getBlkPaddr(info.paddr) === getBlkPaddr(p2_paddr))).reduce(_||_)
  val p2_check_mu_info = check_mu_mshr || check_mu_recent_write

  /** 8. send req to piq */
  toPIQEnqReq.valid := p2_valid && !p2_req_cancel
  toPIQEnqReq.bits.paddr := p2_paddr
  toPIQEnqReq.bits.vSetIdx := p2_vidx

  /** Stage 2 control */
  p2_req_cancel := p2_tag_match || p2_exception || p2_hit_buffer || p2_hit_piq ||
                    p2_check_ipf_info || p2_check_mp_info || p2_check_mu_info || io.fencei
  p2_ready      := p2_valid && toPIQEnqReq.ready || !p2_valid
  p2_fire       := p2_valid
  p2_discard    := p2_valid && p2_req_cancel

  when(p2_discard) {
    XSDebug(p"IPrefetchPipe: discard req in s2. vaddr:${p2_vaddr}")
  }

  when(p2_fire) {
    XSDebug(p"IPrefetchPipe: send req to PIQ. vaddr:${p2_vaddr} paddr:${p2_paddr}")
  }

  /** PerfAccumulate */
  // the number of prefetch request received from ftq
  XSPerfAccumulate("prefetch_req_receive", fromFtq.req.fire())
  // the number of prefetch request sent to PIQ
  XSPerfAccumulate("prefetch_req_send", toPIQEnqReq.fire())
  /**
    * Count the number of requests that are filtered for various reasons.
    * The number of prefetch discard in Performance Accumulator may be
    * a littel larger the number of really discarded. Because there can
    * be multiple reasons for a canceled request at the same time.
    */
  // discard prefetch request by fencei instruction
  XSPerfAccumulate("fdip_prefetch_discard_by_fencei",      (p0_discard || p1_discard || p2_discard) && io.fencei)
  
  // discard prefetch request by the request is same as pipeline behind
  XSPerfAccumulate("fdip_prefetch_discard_by_behind",      p0_discard && p0_hit_behind)
  // discard prefetch request by tlb miss
  XSPerfAccumulate("fdip_prefetch_discard_by_tlb_miss",    p1_discard && p1_tlb_resp_miss)
  // discard prefetch request by tlb except
  XSPerfAccumulate("fdip_prefetch_discard_by_tlb_except",  p1_discard && p1_has_except)
  // discard prefetch request by hit mainPipe info
  XSPerfAccumulate("fdip_prefetch_discard_by_mainPipe",    p2_discard && p2_check_mp_info)
  // discard prefetch request by hit missUnit info
  XSPerfAccumulate("fdip_prefetch_discard_by_missUnit",    p2_discard && p2_check_mu_info)
  // discard prefetch request by hit icache
  XSPerfAccumulate("fdip_prefetch_discard_by_hit_cache",   p2_discard && p2_tag_match)
  // discard prefetch request by pmp except or mmio
  XSPerfAccumulate("fdip_prefetch_discard_by_pmp",         p2_discard && p2_exception)
  // discard prefetch request by hit in prefetch buffer
  XSPerfAccumulate("fdip_prefetch_discard_by_hit_pf",      p2_discard && p2_hit_buffer)
  // discard prefetch request by hit in prefetch queue
  XSPerfAccumulate("fdip_prefetch_discard_by_hit_piq",     p2_discard && p2_hit_piq)
}

class PrefetchQueueIO(edge: TLEdgeOut)(implicit p: Parameters) extends IPrefetchBundle {
  val hartId              = Input(UInt(8.W))
  val prefetchReq         = Flipped(DecoupledIO(new PrefetchReq))
  val PIQFilterRead       = new PIQFilterRead
  val PIQRead             = new PIQRead
  val IPFBufferWrite      = DecoupledIO(new IPFBufferWrite)
  /** TileLink Port */
  val mem_acquire         = DecoupledIO(new TLBundleA(edge.bundle))
  val mem_grant           = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
  val fencei              = Input(Bool())
}

class PrefetchQueue(edge: TLEdgeOut)(implicit p: Parameters) extends IPrefetchModule {
  val io = IO(new PrefetchQueueIO(edge))
  
  val enqueReq          = io.prefetchReq
  val fromIPrefetch     = io.PIQFilterRead.req
  val toIPrefetch       = io.PIQFilterRead.resp
  val fromMainPipe      = io.PIQRead.req
  val toMainPipe        = io.PIQRead.resp
  val toIPF             = io.IPFBufferWrite

  /** Define entryt content and initial FIFO queue */
  class PrefetchEntry(implicit p: Parameters) extends IPrefetchBundle {
    val valid   = Bool()
    val paddr   = UInt(PAddrBits.W)
    val vSetIdx = UInt(idxBits.W)
  }
  val PIQ = RegInit(VecInit(Seq.fill(nPrefetchEntries)(0.U.asTypeOf((new PrefetchEntry).cloneType))))

  class HandleEntry(implicit p: Parameters) extends IPrefetchBundle {
    val valid       = Bool()
    val paddr       = UInt(PAddrBits.W)
    val vSetIdx     = UInt(idxBits.W)
    val issue       = Bool()
    val finish      = Bool()
    val flash       = Bool()
    val readBeatCnt = UInt(log2Up(refillCycles).W)
    val respData    = Vec(refillCycles, UInt(beatBits.W))

    def cacheline = respData.asUInt
  }
  val handleEntry = RegInit(0.U.asTypeOf((new HandleEntry).cloneType))

  /**
    ******************************************************************************
    * handle look-up request from IPrefetchPipe
    * - 1. Receive request from IPrefetch
    * - 2. Look up whether hit in PIQ and handleEntry
    * - 3. Send response to IPrefetch at the same cycle
    ******************************************************************************
    */
  val fr_block  = get_block(fromIPrefetch.paddr)
  val fr_hit_oh = PIQ.map(e => e.valid && (get_block(e.paddr) === fr_block)) :+
                    (handleEntry.valid && (get_block(handleEntry.paddr) === fr_block))
  val fr_hit    = ParallelOR(fr_hit_oh)
  toIPrefetch.piq_hit := fr_hit
  // only hit one entry in PIQ
  assert(PopCount(fr_hit_oh) <= 1.U, "More than 1 hit in L1 PIQ for filter")


  /**
    ******************************************************************************
    * handle read request from ICacheMainPipe
    * - 1. Receive request from ICacheMainPipe
    * - 2. Look up PIQ and handleEntry
    * - 3. Send response to ICacheMainPipe at the same cycle
    ******************************************************************************
    */
  fromMainPipe.foreach(_.ready := DontCare)
  val r_blocks      = fromMainPipe.map (e => get_block(e.bits.paddr))
  /** When hit in PIQ, invalid hitted entry */
  val r_hit_oh      = r_blocks.map (block => PIQ.map(e => e.valid && (get_block(e.paddr) === block)))
  val curr_hit_ptr  = r_hit_oh.map (OHToUInt(_))
  val r_hit_PIQ     = (0 until PortNumber).map (i => ParallelOR(r_hit_oh(i)) && fromMainPipe(i).valid)
  // only hit one entry in PIQ
  (0 until PortNumber).map (i =>
    assert(PopCount(r_hit_oh(i)) <= 1.U, "More than 1 hit in L1 PIQ")
  )

  /** When hit in handleEntry has't been issued, cancel it and return not hit at the same cycle.
   * When hit in handleEntry has been issued return hit at the same cycle, then return data when transiton finish.
   * ICacheMainPipe should be blocked during handle handleEntry.
  */
  val r_hit_handle = r_blocks.map(block => handleEntry.valid && (block === get_block(handleEntry.paddr)))
  val cancelHandleEntry = (((0 until PortNumber).map(i=> fromMainPipe(i).valid && r_hit_handle(i))).reduce(_||_) || io.fencei) && !handleEntry.issue
  val piq_hit = (0 until PortNumber).map(i => r_hit_handle(i) && handleEntry.issue && !handleEntry.flash && !io.fencei)
  toMainPipe := DontCare
  if (prefetchToL1) {
    (0 until PortNumber).foreach (i => {
      toMainPipe(i).piq_hit     := piq_hit(i)
      toMainPipe(i).cacheline   := handleEntry.cacheline
      toMainPipe(i).data_valid  := handleEntry.valid && handleEntry.finish
    })
  }
  val piq_hit_req = RegEnable(piq_hit.reduce(_||_), fromMainPipe.map(_.valid).reduce(_||_))

  /**
    ******************************************************************************
    * Manage prefetch requests by queue
    * - 1. Shift: entry shift when there are free entries behind
    * - 2. Enqueue: when PIQ is full, will flush earlier entry
    * - 3. Free: invalid the entry hitted mainpipe look up
    * - 4. Fencei PIQ: invalid all PIQ entry when fencei
    * - 5. Dequeue: move prefetch request from last entry to handle entry
    * - 6. Cancel handleEntry hitted mainpipe if hasn't been issued
    * - 7. Valid flash of handleEntry when fencei if has been issued
    ******************************************************************************
    */
  /** 1. Shift: entry shift when there are free entries behind */
  val move = Wire(Vec(nPrefetchEntries, Bool()))
  (0 until nPrefetchEntries).map (i =>
    // shift when there are invalid (free) entry behind
    move(i) := ParallelOR((i+1 until nPrefetchEntries).map(!PIQ(_).valid) :+ !handleEntry.valid :+ enqueReq.valid)
  )
  (1 until nPrefetchEntries).map (i =>
    when(move(i-1)) {
      PIQ(i) := PIQ(i-1)
    }
  )

  /** 2. Enqueue: receive prefetch request to PIQ(0) no-blocked*/
  enqueReq.ready := DontCare
  when(enqueReq.valid) {
    PIQ(0).valid    := true.B
    PIQ(0).vSetIdx  := enqueReq.bits.vSetIdx
    PIQ(0).paddr    := enqueReq.bits.paddr
  }.otherwise {
    PIQ(0).valid    := false.B
  }

  /** 3. Free: invalid the entry hitted mainpipe */
  (0 until PortNumber) .map (i =>
    when(r_hit_PIQ(i)) {
      when(move(curr_hit_ptr(i)) && (curr_hit_ptr(i) =/= (nPrefetchEntries-1).U)) {
        // the hitted entry will be shifted, so invalid next entry
        PIQ(curr_hit_ptr(i) + 1.U).valid := false.B
      }.otherwise {
        PIQ(curr_hit_ptr(i)).valid := false.B
      }
    }
  )

  /** 4. Fencei: invalid all PIQ entry when fencei */
  when(io.fencei) {
    PIQ.map (
      _.valid := false.B
    )
  }

  /** 5. Dequeue: move PIQ.last to handle entry when handle entry is free */
  // cancel entry when PIQ.last is hitted by mainpipe
  val cancel = (0 until PortNumber).map (i => r_hit_PIQ(i) && (curr_hit_ptr(i) === (nPrefetchEntries-1).U)).reduce(_||_)
  when(!handleEntry.valid && !cancel) {
    handleEntry.valid       := PIQ(nPrefetchEntries - 1).valid
    handleEntry.paddr       := PIQ(nPrefetchEntries - 1).paddr
    handleEntry.vSetIdx     := PIQ(nPrefetchEntries - 1).vSetIdx
    handleEntry.issue       := false.B
    handleEntry.finish      := false.B
    handleEntry.flash       := false.B
    handleEntry.readBeatCnt := 0.U
  }

  /** 6. Cancel handleEntry hitted mainpipe or receive fencei if hasn't been issued */
  when(cancelHandleEntry) {
    handleEntry.valid := false.B
  }

  /** 7. Valid flash of handleEntry when fencei. Drop data from TileLink port */
  when(io.fencei) {
    handleEntry.flash := true.B
  }


  /**
    ******************************************************************************
    * Write Prefetch Data to Prefetch Buffer
    * - valid signal only hold one cycle
    ******************************************************************************
    */
  toIPF.valid          := handleEntry.valid && handleEntry.finish
  toIPF.bits.paddr     := handleEntry.paddr
  toIPF.bits.vSetIdx   := handleEntry.vSetIdx
  toIPF.bits.cacheline := handleEntry.cacheline
  toIPF.bits.has_hit   := piq_hit_req


  /**
    ******************************************************************************
    * Tilelink Transition
    * - 1. Send acquire to L2Cache, after which the request cannot be canceled (issue valid)
    * - 2. Wait data response from Tilelink
    * - 3. Get data, valid finish (only one cycle) and invalid issue
    ******************************************************************************
    */
  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)

  val s_idle :: s_memReadReq :: s_memReadResp :: s_write_back :: Nil = Enum(4)
  val state = RegInit(s_idle)

  // acquire port
  io.mem_acquire.bits := DontCare
  if (prefetchToL1) {
    io.mem_acquire.bits := edge.Get(
      fromSource      = PortNumber.U,
      toAddress       = Cat(handleEntry.paddr(PAddrBits - 1, log2Ceil(blockBytes)), 0.U(log2Ceil(blockBytes).W)),
      lgSize          = (log2Up(cacheParams.blockBytes)).U)._2
  } else {
    io.mem_acquire.bits := edge.Hint(
      fromSource      = PortNumber.U,
      toAddress       = addrAlign(handleEntry.paddr, blockBytes, PAddrBits),
      lgSize          = (log2Up(cacheParams.blockBytes)).U,
      param           = TLHints.PREFETCH_READ)._2
  }
  
  io.mem_acquire.bits.user.lift(PreferCacheKey).foreach(_ := true.B)
  io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.L1InstPrefetch.id.U)

  // grant port
  io.mem_grant.ready    := true.B
  io.mem_acquire.valid  := (state === s_memReadReq)

  if (prefetchToL1) {
    switch(state) {
      is(s_idle) {
        when(handleEntry.valid && !cancelHandleEntry) {
          handleEntry.issue := true.B
          state             := s_memReadReq
        }
      }
      is(s_memReadReq) {
        state := Mux(io.mem_acquire.fire(), s_memReadResp, s_memReadReq)
      }
      is(s_memReadResp) {
        when (edge.hasData(io.mem_grant.bits) && io.mem_grant.fire()) {
          handleEntry.readBeatCnt := handleEntry.readBeatCnt + 1.U
          handleEntry.respData(handleEntry.readBeatCnt) := io.mem_grant.bits.data
          when (handleEntry.readBeatCnt === (refillCycles - 1).U) {
            assert(refill_done, "refill not done!")
            state := s_write_back
            when (!io.fencei && !handleEntry.flash) {
              handleEntry.issue   := false.B
              handleEntry.finish  := true.B
            }
          }
        }
      }
      is(s_write_back) {
        state             := s_idle
        handleEntry.valid := false.B
      }
    }
  } else {
    switch(state) {
      is(s_idle) {
        when(handleEntry.valid && !cancelHandleEntry) {
          state := s_memReadReq
        }
      }
      is(s_memReadReq) {
        when(io.mem_acquire.fire() || cancelHandleEntry) {
          state := s_idle
          handleEntry.valid := false.B
        } otherwise {
          state := s_memReadReq
        }
      }
    }
  }

  if (env.EnableDifftest) {
    val diffipfrefill = DifftestModule(new DiffRefillEvent)
    diffipfrefill.clock    := clock
    diffipfrefill.coreid   := io.hartId
    diffipfrefill.index    := 3.U
    diffipfrefill.valid    := handleEntry.valid && handleEntry.finish
    diffipfrefill.addr     := handleEntry.paddr
    diffipfrefill.data     := handleEntry.cacheline.asTypeOf(diffipfrefill.data)
  }
}

class FDIPPrefetchIO(edge: TLEdgeOut)(implicit p: Parameters) extends IPrefetchBundle {
  /** commen */
  val fencei = Input(Bool())
  val hartId = Input(UInt(8.W))

  /** Prefetch Mainpipe IO */
  val ftqReq              = Flipped(new FtqPrefechBundle)
  val iTLBInter           = new TlbRequestIO
  val pmp                 = new ICachePMPBundle
  val metaReadReq         = Decoupled(new ICacheMetaReadReqBundle)
  val metaReadResp        = Input(new ICacheMetaReadRespBundle)

  val ICacheMissUnitInfo  = Flipped(new ICacheMissUnitInfo)
  val ICacheMainPipeInfo  = Flipped(new ICacheMainPipeInfo)

  /** Prefetch Buffer IO */
  val IPFBufferRead   = new IPFBufferRead
  val metaWrite       = DecoupledIO(new ICacheMetaWriteBundle)
  val dataWrite       = DecoupledIO(new ICacheDataWriteBundle)
  val IPFReplacer     = new IPFReplacer

  /** Prefetch Queue IO */
  val PIQRead         = new PIQRead
  val mem_acquire     = new DecoupledIO(new TLBundleA(edge.bundle))
  val mem_grant       = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
}

class FDIPPrefetch(edge: TLEdgeOut)(implicit p: Parameters) extends IPrefetchModule
{
  val io = IO(new FDIPPrefetchIO(edge))

  val prefetchPipe    = Module(new IPrefetchPipe)
  val prefetchQueue   = Module(new PrefetchQueue(edge))

  io.metaReadReq    <> prefetchPipe.io.metaReadReq
  io.metaWrite      <> DontCare
  io.dataWrite      <> DontCare
  io.IPFReplacer    <> DontCare
  io.IPFBufferRead  <> DontCare
  io.mem_acquire    <> prefetchQueue.io.mem_acquire
  io.mem_grant      <> prefetchQueue.io.mem_grant

  // outside
  prefetchPipe.io.fencei              <> io.fencei
  prefetchPipe.io.ftqReq              <> io.ftqReq
  prefetchPipe.io.iTLBInter           <> io.iTLBInter
  prefetchPipe.io.pmp                 <> io.pmp
  prefetchPipe.io.metaReadResp        <> io.metaReadResp
  prefetchPipe.io.ICacheMissUnitInfo  <> io.ICacheMissUnitInfo
  prefetchPipe.io.ICacheMainPipeInfo  <> io.ICacheMainPipeInfo
  // inside
  prefetchPipe.io.ipfRecentWrite      <> DontCare
  prefetchPipe.io.IPFFilterRead       <> DontCare

  // outside
  prefetchQueue.io.hartId         <> io.hartId
  prefetchQueue.io.fencei         <> io.fencei
  prefetchQueue.io.PIQRead        <> io.PIQRead
  // inside
  prefetchQueue.io.prefetchReq    <> prefetchPipe.io.prefetchReq
  prefetchQueue.io.PIQFilterRead  <> prefetchPipe.io.PIQFilterRead
  prefetchQueue.io.IPFBufferWrite <> DontCare

  if (prefetchToL1) {
    val prefetchBuffer  = Module(new PrefetchBuffer)
    // outside
    prefetchBuffer.io.hartId          <> io.hartId
    prefetchBuffer.io.fencei          <> io.fencei
    prefetchBuffer.io.IPFBufferRead   <> io.IPFBufferRead
    prefetchBuffer.io.metaWrite       <> io.metaWrite
    prefetchBuffer.io.dataWrite       <> io.dataWrite
    prefetchBuffer.io.IPFReplacer     <> io.IPFReplacer
    // inside
    prefetchBuffer.io.IPFFilterRead  <> prefetchPipe.io.IPFFilterRead
    prefetchBuffer.io.ipfRecentWrite <> prefetchPipe.io.ipfRecentWrite
    prefetchBuffer.io.IPFBufferWrite <> prefetchQueue.io.IPFBufferWrite
  }
}