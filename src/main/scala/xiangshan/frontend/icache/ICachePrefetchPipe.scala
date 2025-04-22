// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.DataHoldBypass
import utility.PriorityMuxDefault
import utility.ValidHold
import utility.XSPerfAccumulate
import utils.NamedUInt
import xiangshan.cache.mmu.Pbmt
import xiangshan.cache.mmu.TlbCmd
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.cache.mmu.ValidHoldBypass // FIXME: should move this to utility?
import xiangshan.frontend.BpuFlushInfo
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit

class ICachePrefetchPipe(implicit p: Parameters) extends ICacheModule
    with ICacheAddrHelper
    with ICacheMetaHelper
    with ICacheMissUpdateHelper {

  class ICachePrefetchPipeIO(implicit p: Parameters) extends ICacheBundle {
    // control
    val csrPfEnable: Bool = Input(Bool())
    val eccEnable:   Bool = Input(Bool())
    val flush:       Bool = Input(Bool())

    val req:            DecoupledIO[PrefetchReqBundle] = Flipped(Decoupled(new PrefetchReqBundle))
    val flushFromBpu:   BpuFlushInfo                   = Flipped(new BpuFlushInfo)
    val itlb:           Vec[TlbRequestIO]              = Vec(PortNumber, new TlbRequestIO)
    val itlbFlushPipe:  Bool                           = Output(Bool())
    val pmp:            Vec[PmpCheckBundle]            = Vec(PortNumber, new PmpCheckBundle)
    val metaRead:       MetaReadBundle                 = new MetaReadBundle
    val missReq:        DecoupledIO[MissReqBundle]     = DecoupledIO(new MissReqBundle)
    val missResp:       Valid[MissRespBundle]          = Flipped(ValidIO(new MissRespBundle))
    val wayLookupWrite: DecoupledIO[WayLookupBundle]   = DecoupledIO(new WayLookupBundle)
  }

  val io: ICachePrefetchPipeIO = IO(new ICachePrefetchPipeIO)

  private val (toItlb, fromItlb) = (io.itlb.map(_.req), io.itlb.map(_.resp))
  private val (toPmp, fromPmp)   = (io.pmp.map(_.req), io.pmp.map(_.resp))
  private val (toMeta, fromMeta) = (io.metaRead.req, io.metaRead.resp)
  private val (toMiss, fromMiss) = (io.missReq, io.missResp)
  private val toWayLookup        = io.wayLookupWrite

  private val s0_fire, s1_fire, s2_fire      = WireInit(false.B)
  private val s1_ready, s2_ready             = WireInit(false.B)
  private val s0_flush, s1_flush, s2_flush   = WireInit(false.B)
  private val fromBpuS0Flush, fromBpuS1Flush = WireInit(false.B)

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
  private val s0_vAddr            = VecInit(Seq(io.req.bits.startAddr, io.req.bits.nextlineStart))
  private val s0_ftqIdx           = io.req.bits.ftqIdx
  private val s0_isSoftPrefetch   = io.req.bits.isSoftPrefetch
  private val s0_doubleline       = io.req.bits.crossCacheline
  private val s0_vSetIdx          = s0_vAddr.map(get_idx)
  private val s0_backendException = VecInit(Seq.fill(PortNumber)(io.req.bits.backendException))

  fromBpuS0Flush := !s0_isSoftPrefetch && (io.flushFromBpu.shouldFlushByStage2(s0_ftqIdx) ||
    io.flushFromBpu.shouldFlushByStage3(s0_ftqIdx))
  s0_flush := io.flush || fromBpuS0Flush || s1_flush

  private val s0_canGo = s1_ready && toItlb(0).ready && toItlb(1).ready && toMeta.ready
  io.req.ready := s0_canGo

  s0_fire := s0_valid && s0_canGo && !s0_flush

  /**
    ******************************************************************************
    * IPrefetch Stage 1
    * - 1. Receive resp from ITLB
    * - 2. Receive resp from IMeta and check
    * - 3. Monitor the requests from missUnit to write to SRAM.
    * - 4. Write wayLookup
    ******************************************************************************
    */
  private val s1_valid = ValidHold(s0_fire, s1_fire, s1_flush)

  private val s1_vAddr            = RegEnable(s0_vAddr, 0.U.asTypeOf(s0_vAddr), s0_fire)
  private val s1_isSoftPrefetch   = RegEnable(s0_isSoftPrefetch, 0.U.asTypeOf(s0_isSoftPrefetch), s0_fire)
  private val s1_doubleline       = RegEnable(s0_doubleline, 0.U.asTypeOf(s0_doubleline), s0_fire)
  private val s1_ftqIdx           = RegEnable(s0_ftqIdx, 0.U.asTypeOf(s0_ftqIdx), s0_fire)
  private val s1_vSetIdx          = VecInit(s1_vAddr.map(get_idx))
  private val s1_backendException = RegEnable(s0_backendException, 0.U.asTypeOf(s0_backendException), s0_fire)

  private def nS1FsmState: Int = 5
  private object S1FsmState extends NamedUInt(log2Up(nS1FsmState)) {
    def Idle:       UInt = 0.U(width.W)
    def ItlbResend: UInt = 1.U(width.W)
    def MetaResend: UInt = 2.U(width.W)
    def EnqWay:     UInt = 3.U(width.W)
    def EnterS2:    UInt = 4.U(width.W)
  }

  private val s1_state     = RegInit(S1FsmState.Idle)
  private val s1_nextState = WireDefault(s1_state)
  private val s0_fireNext  = RegNext(s0_fire)
  dontTouch(s1_state)
  dontTouch(s1_nextState)
  s1_state := s1_nextState

  /**
    ******************************************************************************
    * resend itlb req if miss
    ******************************************************************************
    */
  private val s1_waitItlb = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach { i =>
    when(s1_flush) {
      s1_waitItlb(i) := false.B
    }.elsewhen(RegNext(s0_fire) && fromItlb(i).bits.miss) {
      s1_waitItlb(i) := true.B
    }.elsewhen(s1_waitItlb(i) && !fromItlb(i).bits.miss) {
      s1_waitItlb(i) := false.B
    }
  }
  private val s1_needItlb = VecInit(Seq(
    (RegNext(s0_fire) || s1_waitItlb(0)) && fromItlb(0).bits.miss,
    (RegNext(s0_fire) || s1_waitItlb(1)) && fromItlb(1).bits.miss && s1_doubleline
  ))
  private val tlbValidPulse = VecInit(Seq(
    (RegNext(s0_fire) || s1_waitItlb(0)) && !fromItlb(0).bits.miss,
    (RegNext(s0_fire) || s1_waitItlb(1)) && !fromItlb(1).bits.miss && s1_doubleline
  ))
  private val tlbValidLatch =
    VecInit((0 until PortNumber).map(i => ValidHoldBypass(tlbValidPulse(i), s1_fire, flush = s1_flush)))
  private val tlbFinish = tlbValidLatch(0) && (!s1_doubleline || tlbValidLatch(1))

  (0 until PortNumber).foreach { i =>
    toItlb(i).valid             := s1_needItlb(i) || (s0_valid && (if (i == 0) true.B else s0_doubleline))
    toItlb(i).bits              := DontCare
    toItlb(i).bits.size         := 3.U
    toItlb(i).bits.vaddr        := Mux(s1_needItlb(i), s1_vAddr(i).toUInt, s0_vAddr(i).toUInt)
    toItlb(i).bits.debug.pc     := Mux(s1_needItlb(i), s1_vAddr(i).toUInt, s0_vAddr(i).toUInt)
    toItlb(i).bits.cmd          := TlbCmd.exec
    toItlb(i).bits.no_translate := false.B
  }
  fromItlb.foreach(_.ready := true.B)
  io.itlb.foreach(_.req_kill := false.B)

  /**
    ******************************************************************************
    * Receive resp from ITLB
    ******************************************************************************
    */
  // NOTE: we don't use DataHoldBypass for s1_pAddr, we need s1_pAddrWire later
  private val s1_pAddrWire = VecInit(fromItlb.map(tlbRequest => PrunedAddrInit(tlbRequest.bits.paddr(0))))
  private val s1_pAddrReg = VecInit((0 until PortNumber).map { i =>
    RegEnable(s1_pAddrWire(i), PrunedAddrInit(0.U(PAddrBits.W)), tlbValidPulse(i))
  })
  private val s1_pAddr = VecInit((0 until PortNumber).map { i =>
    Mux(tlbValidPulse(i), s1_pAddrWire(i), s1_pAddrReg(i))
  })
  private val s1_itlbExceptionRaw = VecInit((0 until PortNumber).map { i =>
    DataHoldBypass(ExceptionType.fromTlbResp(fromItlb(i).bits), 0.U(ExceptionType.width.W), tlbValidPulse(i))
  })
  private val s1_itlbPbmt = VecInit((0 until PortNumber).map { i =>
    DataHoldBypass(fromItlb(i).bits.pbmt(0), 0.U.asTypeOf(fromItlb(i).bits.pbmt(0)), tlbValidPulse(i))
  })

  // Guest page fault related: save tlb raw response, select later
  // NOTE: we don't use GPAddrBits or XLEN here, refer to ICacheMainPipe.scala L43-48 and PR#3795
  private val s1_gpAddrRaw = VecInit((0 until PortNumber).map { i =>
    DataHoldBypass(fromItlb(i).bits.gpaddr(0), 0.U(PAddrBitsMax.W), tlbValidPulse(i))
  })
  private val s1_isForVSnonLeafPTERaw = VecInit((0 until PortNumber).map { i =>
    DataHoldBypass(
      fromItlb(i).bits.isForVSnonLeafPTE,
      0.U.asTypeOf(fromItlb(i).bits.isForVSnonLeafPTE),
      tlbValidPulse(i)
    )
  })

  // merge backend exception and itlb exception
  // for area concern, we don't have 64 bits vaddr in frontend, but spec asks page fault when high bits are not all 0/1
  // this check is finished in backend, and passed to frontend with redirect, we see it as a part of itlb exception
  private val s1_itlbException = ExceptionType.merge(
    s1_backendException,
    s1_itlbExceptionRaw
  )
  // debug
  dontTouch(s1_itlbExceptionRaw)
  dontTouch(s1_itlbException)

  /* Select gpAddr with the first gpf
   * Note: the backend wants the base guest physical address of a fetch block
   *       for port(i), its base gpAddr is actually (gpAddr - i * blocksize)
   *       see GPAMem: https://github.com/OpenXiangShan/XiangShan/blob/344cf5d55568dd40cd658a9ee66047a505eeb504/src/main/scala/xiangshan/backend/GPAMem.scala#L33-L34
   *       see also: https://github.com/OpenXiangShan/XiangShan/blob/344cf5d55568dd40cd658a9ee66047a505eeb504/src/main/scala/xiangshan/frontend/IFU.scala#L374-L375
   */
  private val s1_itlbExceptionIsGpf = VecInit(s1_itlbException.map(_ === ExceptionType.gpf))
  private val s1_gpAddr = PriorityMuxDefault(
    s1_itlbExceptionIsGpf zip (0 until PortNumber).map(i => s1_gpAddrRaw(i) - (i << blockOffBits).U),
    0.U.asTypeOf(s1_gpAddrRaw(0))
  )
  private val s1_isForVSnonLeafPTE = PriorityMuxDefault(
    s1_itlbExceptionIsGpf zip s1_isForVSnonLeafPTERaw,
    0.U.asTypeOf(s1_isForVSnonLeafPTERaw(0))
  )

  /**
    ******************************************************************************
    * resend metaArray read req when itlb miss finish
    ******************************************************************************
    */
  private val s1_needMeta = ((s1_state === S1FsmState.ItlbResend) && tlbFinish) || (s1_state === S1FsmState.MetaResend)
  toMeta.valid             := s1_needMeta || s0_valid
  toMeta.bits              := DontCare
  toMeta.bits.isDoubleLine := Mux(s1_needMeta, s1_doubleline, s0_doubleline)

  (0 until PortNumber).foreach(i => toMeta.bits.vSetIdx(i) := Mux(s1_needMeta, s1_vSetIdx(i), s0_vSetIdx(i)))

  /**
    ******************************************************************************
    * Receive resp from IMeta and check
    ******************************************************************************
    */
  private val s1_pTags = VecInit(s1_pAddr.map(get_phy_tag))

  private val s1_metaPTags  = fromMeta.tags
  private val s1_metaValids = fromMeta.entryValid

  private val s1_sramWaymasks = VecInit((0 until PortNumber).map { port =>
    Mux(
      tlbValidPulse(port),
      getWaymask(s1_pAddrWire(port), s1_metaPTags(port), s1_metaValids(port)),
      getWaymask(s1_pAddrReg(port), s1_metaPTags(port), s1_metaValids(port))
    )
  })

  // select ecc code
  /* NOTE:
   * When ECC check fails, s1_waymasks may be corrupted, so this selected meta_codes may be wrong.
   * However, we can guarantee that the request sent to the l2 cache and the response to the IFU are both correct,
   * considering the probability of bit flipping abnormally is very small, consider there's up to 1 bit being wrong:
   * 1. miss -> fake hit: The wrong bit in s1_waymasks was set to true.B, thus selects the wrong meta_codes,
   *                      but we can detect this by checking whether `encodeMetaECC(req_pTags) === meta_codes`.
   * 2. hit -> fake multi-hit: In normal situation, multi-hit never happens, so multi-hit indicates ECC failure,
   *                           we can detect this by checking whether `PopCount(waymasks) <= 1.U`,
   *                           and meta_codes is not important in this situation.
   * 3. hit -> fake miss: We can't detect this, but we can (pre)fetch the correct data from L2 cache, so it's not a problem.
   * 4. hit -> hit / miss -> miss: ECC failure happens in an irrelevant way, so we don't care about it this time.
   */
  private val s1_sramMetaCodes = VecInit((0 until PortNumber).map { port =>
    Mux1H(s1_sramWaymasks(port), fromMeta.codes(port))
  })

  private val s1_sramMaybeRvcMap = VecInit((0 until PortNumber).map { port =>
    Mux1H(s1_sramWaymasks(port), fromMeta.maybeRvcMap(port))
  })

  /**
    ******************************************************************************
    * update waymasks and meta_codes according to MSHR update data
    ******************************************************************************
    */

  private val s1_sramValid      = s0_fireNext || RegNext(s1_needMeta && toMeta.ready)
  private val s1_mshrValid      = fromMiss.valid && !fromMiss.bits.corrupt
  private val s1_waymasks       = WireInit(VecInit(Seq.fill(PortNumber)(0.U(nWays.W))))
  private val s1_waymasksReg    = RegEnable(s1_waymasks, 0.U.asTypeOf(s1_waymasks), s1_sramValid || s1_mshrValid)
  private val s1_metaCodes      = WireInit(VecInit(Seq.fill(PortNumber)(0.U(ICacheMetaCodeBits.W))))
  private val s1_metaCodesReg   = RegEnable(s1_metaCodes, 0.U.asTypeOf(s1_metaCodes), s1_sramValid || s1_mshrValid)
  private val s1_maybeRvcMap    = WireInit(VecInit(Seq.fill(PortNumber)(0.U(MaxInstNumPerBlock.W))))
  private val s1_maybeRvcMapReg = RegEnable(s1_maybeRvcMap, 0.U.asTypeOf(s1_maybeRvcMap), s1_sramValid || s1_mshrValid)

  // update waymasks and meta_codes
  (0 until PortNumber).foreach { i =>
    val (_, newMask, newCode, newMaybeRvcMap) = updateMetaInfo(
      fromMiss,
      Mux(s1_sramValid, s1_sramWaymasks(i), s1_waymasksReg(i)),
      s1_vSetIdx(i),
      s1_pTags(i),
      Mux(s1_sramValid, s1_sramMetaCodes(i), s1_metaCodesReg(i)),
      Mux(s1_sramValid, s1_sramMaybeRvcMap(i), s1_maybeRvcMapReg(i))
    )
    s1_waymasks(i)    := newMask
    s1_metaCodes(i)   := newCode
    s1_maybeRvcMap(i) := newMaybeRvcMap
  }

  /**
    ******************************************************************************
    * send enqueue req to ICacheWayLookup
    ******** **********************************************************************
    */
  // Disallow enqueuing wayLookup when SRAM write occurs.
  toWayLookup.valid := ((s1_state === S1FsmState.EnqWay) || ((s1_state === S1FsmState.Idle) && tlbFinish)) &&
    !s1_flush && !fromMiss.valid && !s1_isSoftPrefetch // do not enqueue soft prefetch
  toWayLookup.bits.vSetIdx           := s1_vSetIdx
  toWayLookup.bits.waymask           := s1_waymasks
  toWayLookup.bits.pTag              := s1_pTags
  toWayLookup.bits.gpAddr            := s1_gpAddr(PAddrBitsMax - 1, 0)
  toWayLookup.bits.isForVSnonLeafPTE := s1_isForVSnonLeafPTE
  toWayLookup.bits.metaCodes         := s1_metaCodes
  toWayLookup.bits.maybeRvcMap       := s1_maybeRvcMap
  (0 until PortNumber).foreach { i =>
    // exception in first line is always valid, in second line is valid iff is doubleline request
    val excpValid = if (i == 0) true.B else s1_doubleline
    // Send s1_itlbException to ICacheWayLookup (instead of s1_exceptionOut) for better timing.
    // Will check pmp again in mainPipe
    toWayLookup.bits.itlbException(i) := Mux(
      excpValid,
      s1_itlbException(i), // includes backend exception
      ExceptionType.none
    )
    toWayLookup.bits.itlbPbmt(i) := Mux(excpValid, s1_itlbPbmt(i), Pbmt.pma)
  }

  when(toWayLookup.fire) {
    val waymasksVec = s1_waymasks.map(_.asTypeOf(Vec(nWays, Bool())))
    assert(
      PopCount(waymasksVec(0)) <= 1.U && (PopCount(waymasksVec(1)) <= 1.U || !s1_doubleline),
      "Multi-hit:\nport0: count=%d pTag=0x%x vSet=0x%x vAddr=0x%x\nport1: count=%d pTag=0x%x vSet=0x%x vAddr=0x%x",
      PopCount(waymasksVec(0)) > 1.U,
      s1_pTags(0),
      get_idx(s1_vAddr(0)),
      s1_vAddr(0).toUInt,
      PopCount(waymasksVec(1)) > 1.U && s1_doubleline,
      s1_pTags(1),
      get_idx(s1_vAddr(1)),
      s1_vAddr(1).toUInt
    )
  }

  /**
    ******************************************************************************
    * PMP check
    ******************************************************************************
    */
  toPmp.zipWithIndex.foreach { case (p, i) =>
    // if itlb has exception, pAddr can be invalid, therefore pmp check can be skipped
    p.valid     := s1_valid // !ExceptionType.hasException(s1_itlbException(i))
    p.bits.addr := s1_pAddr(i).toUInt
    p.bits.size := 3.U
    p.bits.cmd  := TlbCmd.exec
  }
  private val s1_pmpException = VecInit(fromPmp.map(ExceptionType.fromPMPResp))
  private val s1_pmpMmio      = VecInit(fromPmp.map(_.mmio))

  // merge s1 itlb/pmp exceptions, itlb has the highest priority, pmp next
  // for timing consideration, meta_corrupt is not merged, and it will NOT cancel prefetch
  private val s1_exceptionOut = ExceptionType.merge(
    s1_itlbException, // includes backend exception
    s1_pmpException
  )

  // merge pmp mmio and itlb pbmt
  private val s1_isMmio = VecInit((s1_pmpMmio zip s1_itlbPbmt).map { case (mmio, pbmt) =>
    mmio || Pbmt.isUncache(pbmt)
  })

  /**
    ******************************************************************************
    * s1_state machine
    ******** **********************************************************************
    */

  switch(s1_state) {
    is(S1FsmState.Idle) {
      when(s1_valid) {
        when(!tlbFinish) {
          s1_nextState := S1FsmState.ItlbResend
        }.elsewhen(!toWayLookup.fire) { // tlbFinish
          s1_nextState := S1FsmState.EnqWay
        }.elsewhen(!s2_ready) { // tlbFinish && toWayLookup.fire
          s1_nextState := S1FsmState.EnterS2
        } // .otherwise { s1_nextState := S1FsmState.Idle }
      }   // .otherwise { s1_nextState := S1FsmState.Idle }  // !s1_valid
    }
    is(S1FsmState.ItlbResend) {
      when(tlbFinish) {
        when(!toMeta.ready) {
          s1_nextState := S1FsmState.MetaResend
        }.otherwise { // toMeta.ready
          s1_nextState := S1FsmState.EnqWay
        }
      } // .otherwise { s1_nextState := S1FsmState.itlbResend }  // !tlbFinish
    }
    is(S1FsmState.MetaResend) {
      when(toMeta.ready) {
        s1_nextState := S1FsmState.EnqWay
      } // .otherwise { s1_nextState := S1FsmState.metaResend }  // !toMeta.ready
    }
    is(S1FsmState.EnqWay) {
      when(toWayLookup.fire || s1_isSoftPrefetch) {
        when(!s2_ready) {
          s1_nextState := S1FsmState.EnterS2
        }.otherwise { // s2_ready
          s1_nextState := S1FsmState.Idle
        }
      } // .otherwise { s1_nextState := S1FsmState.enqWay }
    }
    is(S1FsmState.EnterS2) {
      when(s2_ready) {
        s1_nextState := S1FsmState.Idle
      }
    }
  }

  when(s1_flush) {
    s1_nextState := S1FsmState.Idle
  }

  /** Stage 1 control */
  fromBpuS1Flush := s1_valid && !s1_isSoftPrefetch && io.flushFromBpu.shouldFlushByStage3(s1_ftqIdx)
  s1_flush       := io.flush || fromBpuS1Flush
  // when s1 is flushed, itlb pipeline should also be flushed
  io.itlbFlushPipe := s1_flush

  s1_ready := s1_nextState === S1FsmState.Idle
  s1_fire  := (s1_nextState === S1FsmState.Idle) && s1_valid && !s1_flush // used to clear s1_valid & itlb_valid_latch
  private val s1_realFire = s1_fire && io.csrPfEnable // real "s1 fire" that s1 enters s2

  /**
    ******************************************************************************
    * IPrefetch Stage 2
    * - 1. Monitor the requests from missUnit to write to SRAM.
    * - 2. send req to missUnit
    ******************************************************************************
    */
  private val s2_valid = ValidHold(s1_realFire, s2_fire, s2_flush)

  private val s2_vAddr          = RegEnable(s1_vAddr, 0.U.asTypeOf(s1_vAddr), s1_realFire)
  private val s2_isSoftPrefetch = RegEnable(s1_isSoftPrefetch, 0.U.asTypeOf(s1_isSoftPrefetch), s1_realFire)
  private val s2_doubleline     = RegEnable(s1_doubleline, 0.U.asTypeOf(s1_doubleline), s1_realFire)
  private val s2_pAddr          = RegEnable(s1_pAddr, 0.U.asTypeOf(s1_pAddr), s1_realFire)
  private val s2_exception =
    RegEnable(s1_exceptionOut, 0.U.asTypeOf(s1_exceptionOut), s1_realFire) // includes itlb/pmp exception
  // disabled for timing consideration
// private val s2_exceptionIn =
//   RegEnable(s1_exceptionOut, 0.U.asTypeOf(s1_exceptionOut), s1_realFire)
  private val s2_isMmio   = RegEnable(s1_isMmio, 0.U.asTypeOf(s1_isMmio), s1_realFire)
  private val s2_waymasks = RegEnable(s1_waymasks, 0.U.asTypeOf(s1_waymasks), s1_realFire)
  // disabled for timing consideration
// private val s2_metaCodes   = RegEnable(s1_metaCodes, 0.U.asTypeOf(s1_metaCodes), s1_realFire)

  private val s2_vSetIdx = s2_vAddr.map(get_idx)
  private val s2_pTags   = s2_pAddr.map(get_phy_tag)

  // disabled for timing consideration
//  // do metaArray ECC check
//  val s2_metaCorrupt = VecInit((s2_pTags zip s2_metaCodes zip s2_waymasks).map{ case ((meta, code), waymask) =>
//    val hit_num = PopCount(waymask)
//    // NOTE: if not hit, encodeMetaECC(meta) =/= code can also be true, but we don't care about it
//    (encodeMetaECC(meta) =/= code && hit_num === 1.U) ||  // hit one way, but parity code does not match, ECC failure
//      hit_num > 1.U                                       // hit multi-way, must be an ECC failure
//  })
//
//  // generate exception
//  val s2_metaException = VecInit(s2_metaCorrupt.map(ExceptionType.fromECC(io.ecc_enable, _)))
//
//  // merge meta exception and itlb/pmp exception
//  val s2_exception = ExceptionType.merge(s2_exceptionIn, s2_metaException)

  /**
    ******************************************************************************
    * Monitor the requests from missUnit to write to SRAM
    ******************************************************************************
    */

  /* NOTE: If fromMiss.bits.corrupt, we should set s2_mshrHits to false.B, and send prefetch requests again.
   * This is the opposite of how mainPipe handles fromMiss.bits.corrupt,
   *   in which we should set s2_mshrHits to true.B, and send error to ifu.
   */
  private val s2_mshrHits = (0 until PortNumber).map(i =>
    ValidHoldBypass(
      checkMshrHit(fromMiss, s2_vSetIdx(i), s2_pTags(i), s2_valid),
      s2_fire || s2_flush
    )
  )

  private val s2_sramHits = s2_waymasks.map(_.orR)
  private val s2_hits     = VecInit((0 until PortNumber).map(i => s2_mshrHits(i) || s2_sramHits(i)))

  /* s2_exception includes itlb pf/gpf/af, pmp af and meta corruption (af), neither of which should be prefetched
   * mmio should not be prefetched
   * also, if previous has exception, latter port should also not be prefetched
   */
  private val s2_miss = VecInit((0 until PortNumber).map { i =>
    !s2_hits(i) && (if (i == 0) true.B else s2_doubleline) &&
    !ExceptionType.hasException(s2_exception.take(i + 1)) &&
    s2_isMmio.take(i + 1).map(!_).reduce(_ && _)
  })

  /**
    ******************************************************************************
    * send req to missUnit
    ******************************************************************************
    */
  private val toMissArbiter = Module(new Arbiter(new MissReqBundle, PortNumber))

  // To avoid sending duplicate requests.
  private val s2_hasSend = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach { i =>
    when(s1_realFire) {
      s2_hasSend(i) := false.B
    }.elsewhen(toMissArbiter.io.in(i).fire) {
      s2_hasSend(i) := true.B
    }
  }

  (0 until PortNumber).foreach { i =>
    toMissArbiter.io.in(i).valid         := s2_valid && s2_miss(i) && !s2_hasSend(i)
    toMissArbiter.io.in(i).bits.blkPAddr := getBlkAddr(s2_pAddr(i))
    toMissArbiter.io.in(i).bits.vSetIdx  := s2_vSetIdx(i)
  }

  toMiss <> toMissArbiter.io.out

  s2_flush := io.flush

  // toMissArbiter.io.in(i).fire is not used here for timing consideration
// private val s2_finish =
//   (0 until PortNumber).map(i => s2_hasSend(i) || !s2_miss(i) || toMissArbiter.io.in(i).fire).reduce(_ && _)
  private val s2_finish = (0 until PortNumber).map(i => s2_hasSend(i) || !s2_miss(i)).reduce(_ && _)
  s2_ready := s2_finish || !s2_valid
  s2_fire  := s2_valid && s2_finish && !s2_flush

  /** PerfAccumulate */
  // the number of bpu flush
  XSPerfAccumulate("bpu_s0_flush", fromBpuS0Flush)
  XSPerfAccumulate("bpu_s1_flush", fromBpuS1Flush)
  // the number of prefetch request received from ftq or backend (software prefetch)
//  XSPerfAccumulate("prefetch_req_receive", io.req.fire)
  XSPerfAccumulate("prefetch_req_receive_hw", io.req.fire && !io.req.bits.isSoftPrefetch)
  XSPerfAccumulate("prefetch_req_receive_sw", io.req.fire && io.req.bits.isSoftPrefetch)
  // the number of prefetch request sent to missUnit
//  XSPerfAccumulate("prefetch_req_send", toMiss.fire)
  XSPerfAccumulate("prefetch_req_send_hw", toMiss.fire && !s2_isSoftPrefetch)
  XSPerfAccumulate("prefetch_req_send_sw", toMiss.fire && s2_isSoftPrefetch)
  XSPerfAccumulate("to_missUnit_stall", toMiss.valid && !toMiss.ready)

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
