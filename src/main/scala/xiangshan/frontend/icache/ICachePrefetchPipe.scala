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
import utils.EnumUInt
import xiangshan.cache.mmu.Pbmt
import xiangshan.cache.mmu.TlbCmd
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.cache.mmu.ValidHoldBypass // FIXME: should move this to utility?
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.ftq.BpuFlushInfo

class ICachePrefetchPipe(implicit p: Parameters) extends ICacheModule
    with ICacheAddrHelper
    with ICacheMetaHelper
    with ICacheMissUpdateHelper {

  class ICachePrefetchPipeIO(implicit p: Parameters) extends ICacheBundle {
    // control
    val csrPfEnable: Bool = Input(Bool())
    val eccEnable:   Bool = Input(Bool())
    val flush:       Bool = Input(Bool())

    val req:            DecoupledIO[PrefetchReqBundle]    = Flipped(Decoupled(new PrefetchReqBundle))
    val flushFromBpu:   BpuFlushInfo                      = Flipped(new BpuFlushInfo)
    val itlb:           TlbRequestIO                      = new TlbRequestIO
    val itlbFlushPipe:  Bool                              = Output(Bool())
    val pmp:            PmpCheckBundle                    = new PmpCheckBundle
    val metaRead:       MetaReadBundle                    = new MetaReadBundle
    val missReq:        DecoupledIO[MissReqBundle]        = DecoupledIO(new MissReqBundle)
    val missResp:       Valid[MissRespBundle]             = Flipped(ValidIO(new MissRespBundle))
    val wayLookupWrite: DecoupledIO[WayLookupWriteBundle] = DecoupledIO(new WayLookupWriteBundle)

    val perf: PrefetchPipePerfInfo = Output(new PrefetchPipePerfInfo)
  }

  val io: ICachePrefetchPipeIO = IO(new ICachePrefetchPipeIO)

  private val (toItlb, fromItlb) = (io.itlb.req, io.itlb.resp)
  private val (toPmp, fromPmp)   = (io.pmp.req, io.pmp.resp)
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
  private val s0_vSetIdx          = VecInit(s0_vAddr.map(get_idx))
  private val s0_backendException = io.req.bits.backendException

  fromBpuS0Flush := !s0_isSoftPrefetch && io.flushFromBpu.shouldFlushByStage3(s0_ftqIdx, s0_valid)
  s0_flush       := io.flush || fromBpuS0Flush || s1_flush

  private val s0_canGo = s1_ready && toItlb.ready && toMeta.ready
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
  private object S1FsmState extends EnumUInt(nS1FsmState) {
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
  private val s1_waitItlb = RegInit(false.B)
  when(s1_flush) {
    s1_waitItlb := false.B
  }.elsewhen(RegNext(s0_fire) && fromItlb.bits.miss) {
    s1_waitItlb := true.B
  }.elsewhen(s1_waitItlb && !fromItlb.bits.miss) {
    s1_waitItlb := false.B
  }
  private val s1_needItlb   = (RegNext(s0_fire) || s1_waitItlb) && fromItlb.bits.miss
  private val tlbValidPulse = (RegNext(s0_fire) || s1_waitItlb) && !fromItlb.bits.miss
  private val tlbValidLatch = ValidHoldBypass(tlbValidPulse, s1_fire, flush = s1_flush)
  private val tlbFinish     = tlbValidLatch

  // NOTE: in kunminghu-v3, Bpu/Ftq ensure that a single fetch request will not cross page,
  //       so we need only one Itlb port to get pAddr / itlbException,
  //       and we can simply use vAddr of first cacheline to send Itlb request.
  toItlb.valid             := s1_needItlb || s0_valid
  toItlb.bits              := DontCare
  toItlb.bits.size         := 3.U
  toItlb.bits.vaddr        := Mux(s1_needItlb, s1_vAddr.head.toUInt, s0_vAddr.head.toUInt)
  toItlb.bits.debug.pc     := Mux(s1_needItlb, s1_vAddr.head.toUInt, s0_vAddr.head.toUInt)
  toItlb.bits.cmd          := TlbCmd.exec
  toItlb.bits.no_translate := false.B
  fromItlb.ready           := true.B
  io.itlb.req_kill         := false.B

  /**
    ******************************************************************************
    * Receive resp from Itlb
    ******************************************************************************
    */
  private val s1_pTag =
    DataHoldBypass(get_phy_tag(fromItlb.bits.paddr.head), 0.U(tagBits.W), tlbValidPulse)

  private val s1_itlbExceptionRaw =
    DataHoldBypass(ExceptionType.fromTlbResp(fromItlb.bits), ExceptionType.None, tlbValidPulse)
  private val s1_itlbPbmt = DataHoldBypass(fromItlb.bits.pbmt.head, Pbmt.pma, tlbValidPulse)

  // Guest page fault related: save tlb raw response, select later
  // NOTE: we don't use GPAddrBits or XLEN here, refer to ICacheMainPipe.scala L43-48 and PR#3795
  private val s1_gpAddr = DataHoldBypass(fromItlb.bits.gpaddr.head, 0.U(PAddrBitsMax.W), tlbValidPulse)
  private val s1_isForVSnonLeafPTE =
    DataHoldBypass(
      fromItlb.bits.isForVSnonLeafPTE,
      0.U.asTypeOf(fromItlb.bits.isForVSnonLeafPTE),
      tlbValidPulse
    )

  // merge backend exception and itlb exception, note this `||` is overloaded
  // for area concern, we don't have 64 bits vaddr in frontend, but spec asks page fault when high bits are not all 0/1
  // this check is finished in backend, and passed to frontend with redirect, we see it as a part of itlb exception
  private val s1_itlbException = s1_backendException || s1_itlbExceptionRaw
  // debug
  dontTouch(s1_itlbExceptionRaw)
  dontTouch(s1_itlbException)

  /**
    ******************************************************************************
    * resend metaArray read req when itlb miss finish
    ******************************************************************************
    */
  private val s1_needMeta = ((s1_state === S1FsmState.ItlbResend) && tlbFinish) || (s1_state === S1FsmState.MetaResend)
  toMeta.valid             := s1_needMeta || s0_valid
  toMeta.bits.isDoubleLine := Mux(s1_needMeta, s1_doubleline, s0_doubleline)
  toMeta.bits.vSetIdx      := Mux(s1_needMeta, s1_vSetIdx, s0_vSetIdx)

  /**
    ******************************************************************************
    * Receive resp from IMeta and check
    ******************************************************************************
    */
  private val s1_metaPTags  = fromMeta.tags
  private val s1_metaValids = fromMeta.entryValid

  private val s1_sramWaymasks = VecInit((0 until PortNumber).map { port =>
    getWaymask(s1_pTag, s1_metaPTags(port), s1_metaValids(port))
  })

  private val s1_sramMaybeRvcMap = VecInit((0 until PortNumber).map { port =>
    Mux1H(s1_sramWaymasks(port), fromMeta.maybeRvcMap(port))
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

  /**
    ******************************************************************************
    * update waymasks and meta_codes according to MSHR update data
    ******************************************************************************
    */

  private val s1_sramValid      = s0_fireNext || RegNext(s1_needMeta && toMeta.ready)
  private val s1_mshrValid      = fromMiss.valid && !fromMiss.bits.corrupt
  private val s1_waymasks       = WireInit(VecInit(Seq.fill(PortNumber)(0.U(nWays.W))))
  private val s1_waymasksReg    = RegEnable(s1_waymasks, 0.U.asTypeOf(s1_waymasks), s1_sramValid || s1_mshrValid)
  private val s1_maybeRvcMap    = WireInit(VecInit(Seq.fill(PortNumber)(0.U(MaxInstNumPerBlock.W))))
  private val s1_maybeRvcMapReg = RegEnable(s1_maybeRvcMap, 0.U.asTypeOf(s1_maybeRvcMap), s1_sramValid || s1_mshrValid)
  private val s1_metaCodes      = WireInit(VecInit(Seq.fill(PortNumber)(0.U(MetaEccBits.W))))
  private val s1_metaCodesReg   = RegEnable(s1_metaCodes, 0.U.asTypeOf(s1_metaCodes), s1_sramValid || s1_mshrValid)

  // update waymasks and meta_codes
  (0 until PortNumber).foreach { i =>
    val (_, newMask, newMaybeRvcMap, newCode) = updateMetaInfo(
      fromMiss,
      Mux(s1_sramValid, s1_sramWaymasks(i), s1_waymasksReg(i)),
      s1_vSetIdx(i),
      s1_pTag,
      Mux(s1_sramValid, s1_sramMaybeRvcMap(i), s1_maybeRvcMapReg(i)),
      Mux(s1_sramValid, s1_sramMetaCodes(i), s1_metaCodesReg(i))
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
  toWayLookup.valid := (
    (s1_state === S1FsmState.EnqWay) ||
      ((s1_state === S1FsmState.Idle) && tlbFinish)
  ) && !s1_flush && !fromMiss.valid && !s1_isSoftPrefetch // do not enqueue soft prefetch
  toWayLookup.bits.ftqIdx            := s1_ftqIdx
  toWayLookup.bits.vSetIdx           := s1_vSetIdx
  toWayLookup.bits.waymask           := s1_waymasks
  toWayLookup.bits.pTag              := s1_pTag
  toWayLookup.bits.maybeRvcMap       := s1_maybeRvcMap
  toWayLookup.bits.gpAddr            := s1_gpAddr(PAddrBitsMax - 1, 0)
  toWayLookup.bits.isForVSnonLeafPTE := s1_isForVSnonLeafPTE
  toWayLookup.bits.metaCodes         := s1_metaCodes
  toWayLookup.bits.itlbException     := s1_itlbException
  toWayLookup.bits.itlbPbmt          := s1_itlbPbmt

  when(toWayLookup.fire) {
    val waymasksVec = s1_waymasks.map(_.asTypeOf(Vec(nWays, Bool())))
    assert(
      PopCount(waymasksVec(0)) <= 1.U && (PopCount(waymasksVec(1)) <= 1.U || !s1_doubleline),
      "Multi-hit:\nport0: count=%d pTag=0x%x vSet=0x%x vAddr=0x%x\nport1: count=%d pTag=0x%x vSet=0x%x vAddr=0x%x",
      PopCount(waymasksVec(0)) > 1.U,
      s1_pTag,
      get_idx(s1_vAddr(0)),
      s1_vAddr(0).toUInt,
      PopCount(waymasksVec(1)) > 1.U && s1_doubleline,
      s1_pTag,
      get_idx(s1_vAddr(1)),
      s1_vAddr(1).toUInt
    )
  }

  /**
    ******************************************************************************
    * PMP check
    ******************************************************************************
    */
  // if itlb has exception, pAddr can be invalid, therefore pmp check can be skipped
  toPmp.valid     := s1_valid // !ExceptionType.hasException(s1_itlbException(i)) // bad for timing
  toPmp.bits.addr := getPAddrFromPTag(s1_vAddr.head, s1_pTag).toUInt
  toPmp.bits.size := 3.U
  toPmp.bits.cmd  := TlbCmd.exec
  private val s1_pmpException = ExceptionType.fromPmpResp(fromPmp)
  private val s1_pmpMmio      = fromPmp.mmio

  // merge s1 itlb/pmp exceptions, itlb has the highest priority, pmp next, note this `||` is overloaded
  // here, itlb exception includes backend exception
  private val s1_exceptionOut = s1_itlbException || s1_pmpException

  // merge pmp mmio and itlb pbmt
  private val s1_isMmio = s1_pmpMmio || Pbmt.isUncache(s1_itlbPbmt)

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
  fromBpuS1Flush := !s1_isSoftPrefetch && io.flushFromBpu.shouldFlushByStage3(s1_ftqIdx, s1_valid)
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
  private val s2_pTag           = RegEnable(s1_pTag, 0.U.asTypeOf(s1_pTag), s1_realFire)
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
      checkMshrHit(fromMiss, s2_vSetIdx(i), s2_pTag, s2_valid),
      s2_fire || s2_flush
    )
  )

  private val s2_sramHits = s2_waymasks.map(_.orR)
  private val s2_hits     = VecInit((0 until PortNumber).map(i => s2_mshrHits(i) || s2_sramHits(i)))

  // do prefetch if not hit and no exception/mmio
  private val s2_miss = VecInit((0 until PortNumber).map { i =>
    !s2_hits(i) && (if (i == 0) true.B else s2_doubleline) &&
    s2_exception.isNone && !s2_isMmio
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
    toMissArbiter.io.in(i).bits.blkPAddr := getBlkAddrFromPTag(s2_vAddr(i), s2_pTag)
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

  /* *****************************************************************************
   * perf
   * ***************************************************************************** */
  // tell ICache top when handling itlb miss
  io.perf.pendingItlbMiss := s1_valid && !tlbFinish

  // the number of bpu flush
  XSPerfAccumulate("bpuS0Flush", fromBpuS0Flush)
  XSPerfAccumulate("bpuS1Flush", fromBpuS1Flush)
  // the number of prefetch request received from ftq or backend (software prefetch)
  XSPerfAccumulate("hwReq", io.req.fire && !io.req.bits.isSoftPrefetch)
  XSPerfAccumulate("swReq", io.req.fire && io.req.bits.isSoftPrefetch)
  // the number of prefetch request sent to missUnit
  XSPerfAccumulate("hwMiss", toMiss.fire && !s2_isSoftPrefetch)
  XSPerfAccumulate("swMiss", toMiss.fire && s2_isSoftPrefetch)
  XSPerfAccumulate("missUnitStall", toMiss.valid && !toMiss.ready)

  // itlb miss bubble
  XSPerfAccumulate("itlbMissBubble", s1_valid && !tlbFinish)
}
