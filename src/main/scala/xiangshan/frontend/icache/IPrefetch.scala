/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan.SoftIfetchPrefetchBundle
import xiangshan.cache.mmu._
import xiangshan.frontend._

abstract class IPrefetchBundle(implicit p: Parameters) extends ICacheBundle
abstract class IPrefetchModule(implicit p: Parameters) extends ICacheModule

class IPrefetchReq(implicit p: Parameters) extends IPrefetchBundle {
  val startAddr:        UInt   = UInt(VAddrBits.W)
  val nextlineStart:    UInt   = UInt(VAddrBits.W)
  val ftqIdx:           FtqPtr = new FtqPtr
  val isSoftPrefetch:   Bool   = Bool()
  val backendException: UInt   = UInt(ExceptionType.width.W)
  def crossCacheline:   Bool   = startAddr(blockOffBits - 1) === 1.U

  def fromFtqICacheInfo(info: FtqICacheInfo): IPrefetchReq = {
    this.startAddr      := info.startAddr
    this.nextlineStart  := info.nextlineStart
    this.ftqIdx         := info.ftqIdx
    this.isSoftPrefetch := false.B
    this
  }

  def fromSoftPrefetch(req: SoftIfetchPrefetchBundle): IPrefetchReq = {
    this.startAddr      := req.vaddr
    this.nextlineStart  := req.vaddr + (1 << blockOffBits).U
    this.ftqIdx         := DontCare
    this.isSoftPrefetch := true.B
    this
  }
}

class IPrefetchIO(implicit p: Parameters) extends IPrefetchBundle {
  // control
  val csr_pf_enable: Bool = Input(Bool())
  val ecc_enable:    Bool = Input(Bool())
  val flush:         Bool = Input(Bool())

  val req:            DecoupledIO[IPrefetchReq]  = Flipped(Decoupled(new IPrefetchReq))
  val flushFromBpu:   BpuFlushInfo               = Flipped(new BpuFlushInfo)
  val itlb:           Vec[TlbRequestIO]          = Vec(PortNumber, new TlbRequestIO)
  val itlbFlushPipe:  Bool                       = Bool()
  val pmp:            Vec[ICachePMPBundle]       = Vec(PortNumber, new ICachePMPBundle)
  val metaRead:       ICacheMetaReqBundle        = new ICacheMetaReqBundle
  val MSHRReq:        DecoupledIO[ICacheMissReq] = DecoupledIO(new ICacheMissReq)
  val MSHRResp:       Valid[ICacheMissResp]      = Flipped(ValidIO(new ICacheMissResp))
  val wayLookupWrite: DecoupledIO[WayLookupInfo] = DecoupledIO(new WayLookupInfo)
}

class IPrefetchPipe(implicit p: Parameters) extends IPrefetchModule with HasICacheECCHelper {
  val io: IPrefetchIO = IO(new IPrefetchIO)

  private val (toITLB, fromITLB) = (io.itlb.map(_.req), io.itlb.map(_.resp))
  private val (toPMP, fromPMP)   = (io.pmp.map(_.req), io.pmp.map(_.resp))
  private val (toMeta, fromMeta) = (io.metaRead.toIMeta, io.metaRead.fromIMeta)
  private val (toMSHR, fromMSHR) = (io.MSHRReq, io.MSHRResp)
  private val toWayLookup        = io.wayLookupWrite

  private val s0_fire, s1_fire, s2_fire            = WireInit(false.B)
  private val s1_ready, s2_ready                   = WireInit(false.B)
  private val s0_flush, s1_flush, s2_flush         = WireInit(false.B)
  private val from_bpu_s0_flush, from_bpu_s1_flush = WireInit(false.B)

  /**
    ******************************************************************************
    * IPrefetch Stage 0
    * - 1. receive ftq req
    * - 2. send req to ITLB
    * - 3. send req to Meta SRAM
    ******************************************************************************
    */
  private val s0_valid = io.req.valid

  /**
    ******************************************************************************
    * receive ftq req
    ******************************************************************************
    */
  private val s0_req_vaddr        = VecInit(Seq(io.req.bits.startAddr, io.req.bits.nextlineStart))
  private val s0_req_ftqIdx       = io.req.bits.ftqIdx
  private val s0_isSoftPrefetch   = io.req.bits.isSoftPrefetch
  private val s0_doubleline       = io.req.bits.crossCacheline
  private val s0_req_vSetIdx      = s0_req_vaddr.map(get_idx)
  private val s0_backendException = VecInit(Seq.fill(PortNumber)(io.req.bits.backendException))

  from_bpu_s0_flush := !s0_isSoftPrefetch && (io.flushFromBpu.shouldFlushByStage2(s0_req_ftqIdx) ||
    io.flushFromBpu.shouldFlushByStage3(s0_req_ftqIdx))
  s0_flush := io.flush || from_bpu_s0_flush || s1_flush

  private val s0_can_go = s1_ready && toITLB(0).ready && toITLB(1).ready && toMeta.ready
  io.req.ready := s0_can_go

  s0_fire := s0_valid && s0_can_go && !s0_flush

  /**
    ******************************************************************************
    * IPrefetch Stage 1
    * - 1. Receive resp from ITLB
    * - 2. Receive resp from IMeta and check
    * - 3. Monitor the requests from missUnit to write to SRAM.
    * - 4. Write wayLookup
    ******************************************************************************
    */
  private val s1_valid =
    generatePipeControl(lastFire = s0_fire, thisFire = s1_fire, thisFlush = s1_flush, lastFlush = false.B)

  private val s1_req_vaddr        = RegEnable(s0_req_vaddr, 0.U.asTypeOf(s0_req_vaddr), s0_fire)
  private val s1_isSoftPrefetch   = RegEnable(s0_isSoftPrefetch, 0.U.asTypeOf(s0_isSoftPrefetch), s0_fire)
  private val s1_doubleline       = RegEnable(s0_doubleline, 0.U.asTypeOf(s0_doubleline), s0_fire)
  private val s1_req_ftqIdx       = RegEnable(s0_req_ftqIdx, 0.U.asTypeOf(s0_req_ftqIdx), s0_fire)
  private val s1_req_vSetIdx      = VecInit(s1_req_vaddr.map(get_idx))
  private val s1_backendException = RegEnable(s0_backendException, 0.U.asTypeOf(s0_backendException), s0_fire)

  private val m_idle :: m_itlbResend :: m_metaResend :: m_enqWay :: m_enterS2 :: Nil = Enum(5)

  private val state      = RegInit(m_idle)
  private val next_state = WireDefault(state)
  private val s0_fire_r  = RegNext(s0_fire)
  dontTouch(state)
  dontTouch(next_state)
  state := next_state

  /**
    ******************************************************************************
    * resend itlb req if miss
    ******************************************************************************
    */
  private val s1_wait_itlb = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach { i =>
    when(s1_flush) {
      s1_wait_itlb(i) := false.B
    }.elsewhen(RegNext(s0_fire) && fromITLB(i).bits.miss) {
      s1_wait_itlb(i) := true.B
    }.elsewhen(s1_wait_itlb(i) && !fromITLB(i).bits.miss) {
      s1_wait_itlb(i) := false.B
    }
  }
  private val s1_need_itlb = VecInit(Seq(
    (RegNext(s0_fire) || s1_wait_itlb(0)) && fromITLB(0).bits.miss,
    (RegNext(s0_fire) || s1_wait_itlb(1)) && fromITLB(1).bits.miss && s1_doubleline
  ))
  private val tlb_valid_pulse = VecInit(Seq(
    (RegNext(s0_fire) || s1_wait_itlb(0)) && !fromITLB(0).bits.miss,
    (RegNext(s0_fire) || s1_wait_itlb(1)) && !fromITLB(1).bits.miss && s1_doubleline
  ))
  private val tlb_valid_latch =
    VecInit((0 until PortNumber).map(i => ValidHoldBypass(tlb_valid_pulse(i), s1_fire, flush = s1_flush)))
  private val itlb_finish = tlb_valid_latch(0) && (!s1_doubleline || tlb_valid_latch(1))

  (0 until PortNumber).foreach { i =>
    toITLB(i).valid             := s1_need_itlb(i) || (s0_valid && (if (i == 0) true.B else s0_doubleline))
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
  private val s1_req_paddr_wire = VecInit(fromITLB.map(_.bits.paddr(0)))
  private val s1_req_paddr_reg = VecInit((0 until PortNumber).map { i =>
    RegEnable(s1_req_paddr_wire(i), 0.U(PAddrBits.W), tlb_valid_pulse(i))
  })
  private val s1_req_paddr = VecInit((0 until PortNumber).map { i =>
    Mux(tlb_valid_pulse(i), s1_req_paddr_wire(i), s1_req_paddr_reg(i))
  })
  private val s1_req_gpaddr_tmp = VecInit((0 until PortNumber).map { i =>
    ResultHoldBypass(
      valid = tlb_valid_pulse(i),
      // NOTE: we dont use GPAddrBits or XLEN here, refer to ICacheMainPipe.scala L43-48 and PR#3795
      init = 0.U(PAddrBitsMax.W),
      data = fromITLB(i).bits.gpaddr(0)
    )
  })
  private val s1_req_isForVSnonLeafPTE_tmp = VecInit((0 until PortNumber).map { i =>
    ResultHoldBypass(
      valid = tlb_valid_pulse(i),
      init = 0.U.asTypeOf(fromITLB(i).bits.isForVSnonLeafPTE),
      data = fromITLB(i).bits.isForVSnonLeafPTE
    )
  })
  private val s1_itlb_exception_tmp = VecInit((0 until PortNumber).map { i =>
    ResultHoldBypass(
      valid = tlb_valid_pulse(i),
      init = 0.U(ExceptionType.width.W),
      data = ExceptionType.fromTlbResp(fromITLB(i).bits)
    )
  })
  private val s1_itlb_pbmt = VecInit((0 until PortNumber).map { i =>
    ResultHoldBypass(
      valid = tlb_valid_pulse(i),
      init = 0.U.asTypeOf(fromITLB(i).bits.pbmt(0)),
      data = fromITLB(i).bits.pbmt(0)
    )
  })

  // merge backend exception and itlb exception
  // for area concern, we don't have 64 bits vaddr in frontend, but spec asks page fault when high bits are not all 0/1
  // this check is finished in backend, and passed to frontend with redirect, we see it as a part of itlb exception
  private val s1_itlb_exception = ExceptionType.merge(
    s1_backendException,
    s1_itlb_exception_tmp
  )
  // debug
  dontTouch(s1_itlb_exception_tmp)
  dontTouch(s1_itlb_exception)

  private val s1_itlb_exception_gpf = VecInit(s1_itlb_exception.map(_ === ExceptionType.gpf))

  /* Select gpaddr with the first gpf
   * Note: the backend wants the base guest physical address of a fetch block
   *       for port(i), its base gpaddr is actually (gpaddr - i * blocksize)
   *       see GPAMem: https://github.com/OpenXiangShan/XiangShan/blob/344cf5d55568dd40cd658a9ee66047a505eeb504/src/main/scala/xiangshan/backend/GPAMem.scala#L33-L34
   *       see also: https://github.com/OpenXiangShan/XiangShan/blob/344cf5d55568dd40cd658a9ee66047a505eeb504/src/main/scala/xiangshan/frontend/IFU.scala#L374-L375
   */
  private val s1_req_gpaddr = PriorityMuxDefault(
    s1_itlb_exception_gpf zip (0 until PortNumber).map(i => s1_req_gpaddr_tmp(i) - (i << blockOffBits).U),
    0.U.asTypeOf(s1_req_gpaddr_tmp(0))
  )

  private val s1_req_isForVSnonLeafPTE = PriorityMuxDefault(
    s1_itlb_exception_gpf zip s1_req_isForVSnonLeafPTE_tmp,
    0.U.asTypeOf(s1_req_isForVSnonLeafPTE_tmp(0))
  )

  /**
    ******************************************************************************
    * resend metaArray read req when itlb miss finish
    ******************************************************************************
    */
  private val s1_need_meta = ((state === m_itlbResend) && itlb_finish) || (state === m_metaResend)
  toMeta.valid             := s1_need_meta || s0_valid
  toMeta.bits              := DontCare
  toMeta.bits.isDoubleLine := Mux(s1_need_meta, s1_doubleline, s0_doubleline)

  (0 until PortNumber).foreach { i =>
    toMeta.bits.vSetIdx(i) := Mux(s1_need_meta, s1_req_vSetIdx(i), s0_req_vSetIdx(i))
  }

  /**
    ******************************************************************************
    * Receive resp from IMeta and check
    ******************************************************************************
    */
  private val s1_req_ptags = VecInit(s1_req_paddr.map(get_phy_tag))

  private val s1_meta_ptags  = fromMeta.tags
  private val s1_meta_valids = fromMeta.entryValid

  private def getWaymask(paddrs: Vec[UInt]): Vec[UInt] = {
    val ptags = paddrs.map(get_phy_tag)
    val tag_eq_vec =
      VecInit((0 until PortNumber).map(p => VecInit((0 until nWays).map(w => s1_meta_ptags(p)(w) === ptags(p)))))
    val tag_match_vec = VecInit((0 until PortNumber).map { k =>
      VecInit(tag_eq_vec(k).zipWithIndex.map { case (way_tag_eq, w) => way_tag_eq && s1_meta_valids(k)(w) })
    })
    val waymasks = VecInit(tag_match_vec.map(_.asUInt))
    waymasks
  }

  private val s1_SRAM_waymasks = VecInit((0 until PortNumber).map { port =>
    Mux(tlb_valid_pulse(port), getWaymask(s1_req_paddr_wire)(port), getWaymask(s1_req_paddr_reg)(port))
  })

  // select ecc code
  /* NOTE:
   * When ECC check fails, s1_waymasks may be corrupted, so this selected meta_codes may be wrong.
   * However, we can guarantee that the request sent to the l2 cache and the response to the IFU are both correct,
   * considering the probability of bit flipping abnormally is very small, consider there's up to 1 bit being wrong:
   * 1. miss -> fake hit: The wrong bit in s1_waymasks was set to true.B, thus selects the wrong meta_codes,
   *                      but we can detect this by checking whether `encodeMetaECC(req_ptags) === meta_codes`.
   * 2. hit -> fake multi-hit: In normal situation, multi-hit never happens, so multi-hit indicates ECC failure,
   *                           we can detect this by checking whether `PopCount(waymasks) <= 1.U`,
   *                           and meta_codes is not important in this situation.
   * 3. hit -> fake miss: We can't detect this, but we can (pre)fetch the correct data from L2 cache, so it's not a problem.
   * 4. hit -> hit / miss -> miss: ECC failure happens in an irrelevant way, so we don't care about it this time.
   */
  private val s1_SRAM_meta_codes = VecInit((0 until PortNumber).map { port =>
    Mux1H(s1_SRAM_waymasks(port), fromMeta.codes(port))
  })

  /**
    ******************************************************************************
    * update waymasks and meta_codes according to MSHR update data
    ******************************************************************************
    */
  private def updateMetaInfo(mask: UInt, vSetIdx: UInt, ptag: UInt, code: UInt): (UInt, UInt) = {
    require(mask.getWidth == nWays)
    val new_mask  = WireInit(mask)
    val new_code  = WireInit(code)
    val valid     = fromMSHR.valid && !fromMSHR.bits.corrupt
    val vset_same = fromMSHR.bits.vSetIdx === vSetIdx
    val ptag_same = getPhyTagFromBlk(fromMSHR.bits.blkPaddr) === ptag
    val way_same  = fromMSHR.bits.waymask === mask
    when(valid && vset_same) {
      when(ptag_same) {
        new_mask := fromMSHR.bits.waymask
        // also update meta_codes
        // we have getPhyTagFromBlk(fromMSHR.bits.blkPaddr) === ptag, so we can use ptag directly for better timing
        new_code := encodeMetaECC(ptag)
      }.elsewhen(way_same) {
        new_mask := 0.U
        // we don't care about new_code, since it's not used for a missed request
      }
    }
    (new_mask, new_code)
  }

  private val s1_SRAM_valid   = s0_fire_r || RegNext(s1_need_meta && toMeta.ready)
  private val s1_MSHR_valid   = fromMSHR.valid && !fromMSHR.bits.corrupt
  private val s1_waymasks     = WireInit(VecInit(Seq.fill(PortNumber)(0.U(nWays.W))))
  private val s1_waymasks_r   = RegEnable(s1_waymasks, 0.U.asTypeOf(s1_waymasks), s1_SRAM_valid || s1_MSHR_valid)
  private val s1_meta_codes   = WireInit(VecInit(Seq.fill(PortNumber)(0.U(ICacheMetaCodeBits.W))))
  private val s1_meta_codes_r = RegEnable(s1_meta_codes, 0.U.asTypeOf(s1_meta_codes), s1_SRAM_valid || s1_MSHR_valid)

  // update waymasks and meta_codes
  (0 until PortNumber).foreach { i =>
    val old_waymask    = Mux(s1_SRAM_valid, s1_SRAM_waymasks(i), s1_waymasks_r(i))
    val old_meta_codes = Mux(s1_SRAM_valid, s1_SRAM_meta_codes(i), s1_meta_codes_r(i))
    val new_info       = updateMetaInfo(old_waymask, s1_req_vSetIdx(i), s1_req_ptags(i), old_meta_codes)
    s1_waymasks(i)   := new_info._1
    s1_meta_codes(i) := new_info._2
  }

  /**
    ******************************************************************************
    * send enqueue req to WayLookup
    ******** **********************************************************************
    */
  // Disallow enqueuing wayLookup when SRAM write occurs.
  toWayLookup.valid := ((state === m_enqWay) || ((state === m_idle) && itlb_finish)) &&
    !s1_flush && !fromMSHR.valid && !s1_isSoftPrefetch // do not enqueue soft prefetch
  toWayLookup.bits.vSetIdx           := s1_req_vSetIdx
  toWayLookup.bits.waymask           := s1_waymasks
  toWayLookup.bits.ptag              := s1_req_ptags
  toWayLookup.bits.gpaddr            := s1_req_gpaddr
  toWayLookup.bits.isForVSnonLeafPTE := s1_req_isForVSnonLeafPTE
  toWayLookup.bits.meta_codes        := s1_meta_codes
  (0 until PortNumber).foreach { i =>
    // exception in first line is always valid, in second line is valid iff is doubleline request
    val excpValid = if (i == 0) true.B else s1_doubleline
    // Send s1_itlb_exception to WayLookup (instead of s1_exception_out) for better timing.
    // Will check pmp again in mainPipe
    toWayLookup.bits.itlb_exception(i) := Mux(
      excpValid,
      s1_itlb_exception(i), // includes backend exception
      ExceptionType.none
    )
    toWayLookup.bits.itlb_pbmt(i) := Mux(excpValid, s1_itlb_pbmt(i), Pbmt.pma)
  }

  private val s1_waymasks_vec = s1_waymasks.map(_.asTypeOf(Vec(nWays, Bool())))
  when(toWayLookup.fire) {
    assert(
      PopCount(s1_waymasks_vec(0)) <= 1.U && (PopCount(s1_waymasks_vec(1)) <= 1.U || !s1_doubleline),
      "Multi-hit:\nport0: count=%d ptag=0x%x vSet=0x%x vaddr=0x%x\nport1: count=%d ptag=0x%x vSet=0x%x vaddr=0x%x",
      PopCount(s1_waymasks_vec(0)) > 1.U,
      s1_req_ptags(0),
      get_idx(s1_req_vaddr(0)),
      s1_req_vaddr(0),
      PopCount(s1_waymasks_vec(1)) > 1.U && s1_doubleline,
      s1_req_ptags(1),
      get_idx(s1_req_vaddr(1)),
      s1_req_vaddr(1)
    )
  }

  /**
    ******************************************************************************
    * PMP check
    ******************************************************************************
    */
  toPMP.zipWithIndex.foreach { case (p, i) =>
    // if itlb has exception, paddr can be invalid, therefore pmp check can be skipped
    p.valid     := s1_valid // !ExceptionType.hasException(s1_itlb_exception(i))
    p.bits.addr := s1_req_paddr(i)
    p.bits.size := 3.U
    p.bits.cmd  := TlbCmd.exec
  }
  private val s1_pmp_exception = VecInit(fromPMP.map(ExceptionType.fromPMPResp))
  private val s1_pmp_mmio      = VecInit(fromPMP.map(_.mmio))

  // merge s1 itlb/pmp exceptions, itlb has the highest priority, pmp next
  // for timing consideration, meta_corrupt is not merged, and it will NOT cancel prefetch
  private val s1_exception_out = ExceptionType.merge(
    s1_itlb_exception, // includes backend exception
    s1_pmp_exception
  )

  // merge pmp mmio and itlb pbmt
  private val s1_mmio = VecInit((s1_pmp_mmio zip s1_itlb_pbmt).map { case (mmio, pbmt) =>
    mmio || Pbmt.isUncache(pbmt)
  })

  /**
    ******************************************************************************
    * state machine
    ******** **********************************************************************
    */

  switch(state) {
    is(m_idle) {
      when(s1_valid) {
        when(!itlb_finish) {
          next_state := m_itlbResend
        }.elsewhen(!toWayLookup.fire) { // itlb_finish
          next_state := m_enqWay
        }.elsewhen(!s2_ready) { // itlb_finish && toWayLookup.fire
          next_state := m_enterS2
        } // .otherwise { next_state := m_idle }
      }   // .otherwise { next_state := m_idle }  // !s1_valid
    }
    is(m_itlbResend) {
      when(itlb_finish) {
        when(!toMeta.ready) {
          next_state := m_metaResend
        }.otherwise { // toMeta.ready
          next_state := m_enqWay
        }
      } // .otherwise { next_state := m_itlbResend }  // !itlb_finish
    }
    is(m_metaResend) {
      when(toMeta.ready) {
        next_state := m_enqWay
      } // .otherwise { next_state := m_metaResend }  // !toMeta.ready
    }
    is(m_enqWay) {
      when(toWayLookup.fire || s1_isSoftPrefetch) {
        when(!s2_ready) {
          next_state := m_enterS2
        }.otherwise { // s2_ready
          next_state := m_idle
        }
      } // .otherwise { next_state := m_enqWay }
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
  from_bpu_s1_flush := s1_valid && !s1_isSoftPrefetch && io.flushFromBpu.shouldFlushByStage3(s1_req_ftqIdx)
  s1_flush          := io.flush || from_bpu_s1_flush
  // when s1 is flushed, itlb pipeline should also be flushed
  io.itlbFlushPipe := s1_flush

  s1_ready := next_state === m_idle
  s1_fire  := (next_state === m_idle) && s1_valid && !s1_flush // used to clear s1_valid & itlb_valid_latch
  private val s1_real_fire = s1_fire && io.csr_pf_enable // real "s1 fire" that s1 enters s2

  /**
    ******************************************************************************
    * IPrefetch Stage 2
    * - 1. Monitor the requests from missUnit to write to SRAM.
    * - 2. send req to missUnit
    ******************************************************************************
    */
  private val s2_valid =
    generatePipeControl(lastFire = s1_real_fire, thisFire = s2_fire, thisFlush = s2_flush, lastFlush = false.B)

  private val s2_req_vaddr      = RegEnable(s1_req_vaddr, 0.U.asTypeOf(s1_req_vaddr), s1_real_fire)
  private val s2_isSoftPrefetch = RegEnable(s1_isSoftPrefetch, 0.U.asTypeOf(s1_isSoftPrefetch), s1_real_fire)
  private val s2_doubleline     = RegEnable(s1_doubleline, 0.U.asTypeOf(s1_doubleline), s1_real_fire)
  private val s2_req_paddr      = RegEnable(s1_req_paddr, 0.U.asTypeOf(s1_req_paddr), s1_real_fire)
  private val s2_exception =
    RegEnable(s1_exception_out, 0.U.asTypeOf(s1_exception_out), s1_real_fire) // includes itlb/pmp exception
  // disabled for timing consideration
// private val s2_exception_in =
//   RegEnable(s1_exception_out, 0.U.asTypeOf(s1_exception_out), s1_real_fire)
  private val s2_mmio     = RegEnable(s1_mmio, 0.U.asTypeOf(s1_mmio), s1_real_fire)
  private val s2_waymasks = RegEnable(s1_waymasks, 0.U.asTypeOf(s1_waymasks), s1_real_fire)
  // disabled for timing consideration
// private val s2_meta_codes   = RegEnable(s1_meta_codes, 0.U.asTypeOf(s1_meta_codes), s1_real_fire)

  private val s2_req_vSetIdx = s2_req_vaddr.map(get_idx)
  private val s2_req_ptags   = s2_req_paddr.map(get_phy_tag)

  // disabled for timing consideration
//  // do metaArray ECC check
//  val s2_meta_corrupt = VecInit((s2_req_ptags zip s2_meta_codes zip s2_waymasks).map{ case ((meta, code), waymask) =>
//    val hit_num = PopCount(waymask)
//    // NOTE: if not hit, encodeMetaECC(meta) =/= code can also be true, but we don't care about it
//    (encodeMetaECC(meta) =/= code && hit_num === 1.U) ||  // hit one way, but parity code does not match, ECC failure
//      hit_num > 1.U                                       // hit multi-way, must be an ECC failure
//  })
//
//  // generate exception
//  val s2_meta_exception = VecInit(s2_meta_corrupt.map(ExceptionType.fromECC(io.ecc_enable, _)))
//
//  // merge meta exception and itlb/pmp exception
//  val s2_exception = ExceptionType.merge(s2_exception_in, s2_meta_exception)

  /**
    ******************************************************************************
    * Monitor the requests from missUnit to write to SRAM
    ******************************************************************************
    */

  /* NOTE: If fromMSHR.bits.corrupt, we should set s2_MSHR_hits to false.B, and send prefetch requests again.
   * This is the opposite of how mainPipe handles fromMSHR.bits.corrupt,
   *   in which we should set s2_MSHR_hits to true.B, and send error to ifu.
   */
  private val s2_MSHR_match = VecInit((0 until PortNumber).map { i =>
    (s2_req_vSetIdx(i) === fromMSHR.bits.vSetIdx) &&
    (s2_req_ptags(i) === getPhyTagFromBlk(fromMSHR.bits.blkPaddr)) &&
    s2_valid && fromMSHR.valid && !fromMSHR.bits.corrupt
  })
  private val s2_MSHR_hits = (0 until PortNumber).map(i => ValidHoldBypass(s2_MSHR_match(i), s2_fire || s2_flush))

  private val s2_SRAM_hits = s2_waymasks.map(_.orR)
  private val s2_hits      = VecInit((0 until PortNumber).map(i => s2_MSHR_hits(i) || s2_SRAM_hits(i)))

  /* s2_exception includes itlb pf/gpf/af, pmp af and meta corruption (af), neither of which should be prefetched
   * mmio should not be prefetched
   * also, if previous has exception, latter port should also not be prefetched
   */
  private val s2_miss = VecInit((0 until PortNumber).map { i =>
    !s2_hits(i) && (if (i == 0) true.B else s2_doubleline) &&
    !ExceptionType.hasException(s2_exception.take(i + 1)) &&
    s2_mmio.take(i + 1).map(!_).reduce(_ && _)
  })

  /**
    ******************************************************************************
    * send req to missUnit
    ******************************************************************************
    */
  private val toMSHRArbiter = Module(new Arbiter(new ICacheMissReq, PortNumber))

  // To avoid sending duplicate requests.
  private val has_send = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach { i =>
    when(s1_real_fire) {
      has_send(i) := false.B
    }.elsewhen(toMSHRArbiter.io.in(i).fire) {
      has_send(i) := true.B
    }
  }

  (0 until PortNumber).foreach { i =>
    toMSHRArbiter.io.in(i).valid         := s2_valid && s2_miss(i) && !has_send(i)
    toMSHRArbiter.io.in(i).bits.blkPaddr := getBlkAddr(s2_req_paddr(i))
    toMSHRArbiter.io.in(i).bits.vSetIdx  := s2_req_vSetIdx(i)
  }

  toMSHR <> toMSHRArbiter.io.out

  s2_flush := io.flush

  // toMSHRArbiter.io.in(i).fire is not used here for timing consideration
// private val s2_finish =
//   (0 until PortNumber).map(i => has_send(i) || !s2_miss(i) || toMSHRArbiter.io.in(i).fire).reduce(_ && _)
  private val s2_finish = (0 until PortNumber).map(i => has_send(i) || !s2_miss(i)).reduce(_ && _)
  s2_ready := s2_finish || !s2_valid
  s2_fire  := s2_valid && s2_finish && !s2_flush

  /** PerfAccumulate */
  // the number of bpu flush
  XSPerfAccumulate("bpu_s0_flush", from_bpu_s0_flush)
  XSPerfAccumulate("bpu_s1_flush", from_bpu_s1_flush)
  // the number of prefetch request received from ftq or backend (software prefetch)
//  XSPerfAccumulate("prefetch_req_receive", io.req.fire)
  XSPerfAccumulate("prefetch_req_receive_hw", io.req.fire && !io.req.bits.isSoftPrefetch)
  XSPerfAccumulate("prefetch_req_receive_sw", io.req.fire && io.req.bits.isSoftPrefetch)
  // the number of prefetch request sent to missUnit
//  XSPerfAccumulate("prefetch_req_send", toMSHR.fire)
  XSPerfAccumulate("prefetch_req_send_hw", toMSHR.fire && !s2_isSoftPrefetch)
  XSPerfAccumulate("prefetch_req_send_sw", toMSHR.fire && s2_isSoftPrefetch)
  XSPerfAccumulate("to_missUnit_stall", toMSHR.valid && !toMSHR.ready)

  /**
    * Count the number of requests that are filtered for various reasons.
    * The number of prefetch discard in Performance Accumulator may be
    * a little larger the number of really discarded. Because there can
    * be multiple reasons for a canceled request at the same time.
    */
  // discard prefetch request by flush
  // XSPerfAccumulate("fdip_prefetch_discard_by_tlb_except",  p1_discard && p1_tlb_except)
  // // discard prefetch request by hit icache SRAM
  // XSPerfAccumulate("fdip_prefetch_discard_by_hit_cache",   p2_discard && p1_meta_hit)
  // // discard prefetch request by hit write SRAM
  // XSPerfAccumulate("fdip_prefetch_discard_by_p1_monitor",  p1_discard && p1_monitor_hit)
  // // discard prefetch request by pmp except or mmio
  // XSPerfAccumulate("fdip_prefetch_discard_by_pmp",         p2_discard && p2_pmp_except)
  // // discard prefetch request by hit mainPipe info
  // // XSPerfAccumulate("fdip_prefetch_discard_by_mainPipe",    p2_discard && p2_mainPipe_hit)
}
