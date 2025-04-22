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
import difftest.DiffRefillEvent
import difftest.DifftestModule
import org.chipsalliance.cde.config.Parameters
import utility.DataHoldBypass
import utility.ValidHold
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.L1CacheErrorInfo
import xiangshan.cache.mmu.Pbmt
import xiangshan.cache.mmu.TlbCmd
import xiangshan.cache.mmu.ValidHoldBypass
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FtqToFetchBundle

class ICacheMainPipe(implicit p: Parameters) extends ICacheModule
    with ICacheEccHelper
    with ICacheAddrHelper
    with ICacheDataHelper
    with ICacheMissUpdateHelper {

  class ICacheMainPipeIO(implicit p: Parameters) extends ICacheBundle {
    val hartId: UInt = Input(UInt(hartIdLen.W))

    /* *** internal interface *** */
    val dataRead:      DataReadBundle               = new DataReadBundle
    val metaFlush:     MetaFlushBundle              = new MetaFlushBundle
    val replacerTouch: ReplacerTouchBundle          = new ReplacerTouchBundle
    val wayLookupRead: DecoupledIO[WayLookupBundle] = Flipped(DecoupledIO(new WayLookupBundle))
    val missReq:       DecoupledIO[MissReqBundle]   = DecoupledIO(new MissReqBundle)
    val missResp:      Valid[MissRespBundle]        = Flipped(ValidIO(new MissRespBundle))
    val eccEnable:     Bool                         = Input(Bool())

    /* *** outside interface *** */
    // Ftq
    val req:   DecoupledIO[FtqToFetchBundle] = Flipped(DecoupledIO(new FtqToFetchBundle))
    val flush: Bool                          = Input(Bool())
    // Pmp
    val pmp: Vec[PmpCheckBundle] = Vec(PortNumber, new PmpCheckBundle)
    // Ifu
    val s1Resp:    Valid[ICacheS1RespBundle] = ValidIO(new ICacheS1RespBundle)
    val s2Resp:    Valid[ICacheS2RespBundle] = ValidIO(new ICacheS2RespBundle)
    val respStall: Bool                      = Input(Bool())
    // backend/Beu
    val errors: Vec[Valid[L1CacheErrorInfo]] = Output(Vec(PortNumber, ValidIO(new L1CacheErrorInfo)))

    /* *** Perf *** */
    val perf:    ICachePerfInfo    = Output(new ICachePerfInfo)
    val topdown: ICacheTopdownInfo = Output(new ICacheTopdownInfo)
  }

  val io: ICacheMainPipeIO = IO(new ICacheMainPipeIO)

  /* *** Input/Output port *** */
  private val (fromFtq, toIfuS1, toIfuS2) = (io.req, io.s1Resp, io.s2Resp)
  private val (toData, fromData)          = (io.dataRead.req, io.dataRead.resp)
  private val toMetaFlush                 = io.metaFlush.req
  private val (toMiss, fromMiss)          = (io.missReq, io.missResp)
  private val (toPmp, fromPmp)            = (io.pmp.map(_.req), io.pmp.map(_.resp))
  private val fromWayLookup               = io.wayLookupRead
  private val eccEnable =
    if (ICacheForceMetaECCError || ICacheForceDataECCError) true.B else io.eccEnable

  // Statistics on the frequency distribution of FTQ fire interval
  private val cntFtqFireInterval      = RegInit(0.U(32.W))
  private val cntFtqFireIntervalStart = 1
  private val cntFtqFireIntervalEnd   = 300
  cntFtqFireInterval := Mux(fromFtq.fire, 1.U, cntFtqFireInterval + 1.U)
  XSPerfHistogram(
    "ftq2icache_fire",
    cntFtqFireInterval,
    fromFtq.fire,
    cntFtqFireIntervalStart,
    cntFtqFireIntervalEnd,
    right_strict = true
  )

  /** pipeline control signal */
  private val s1_ready, s2_ready           = Wire(Bool())
  private val s0_fire, s1_fire, s2_fire    = Wire(Bool())
  private val s0_flush, s1_flush, s2_flush = Wire(Bool())

  /**
    ******************************************************************************
    * ICache Stage 0
    * - send req to data SRAM
    * - get waymask and tlb info from wayLookup
    ******************************************************************************
    */

  /** s0 control */
  // 0,1,2,3 -> dataArray(data); 4 -> mainPipe
  // Ftq RegNext Register
  private val fromFtqReq      = fromFtq.bits.req
  private val s0_valid        = fromFtq.valid
  private val s0_readValidAll = (0 until partWayNum + 1).map(i => fromFtq.bits.readValid(i))
  private val s0_vAddrAll =
    (0 until partWayNum + 1).map(i => VecInit(Seq(fromFtqReq(i).startAddr, fromFtqReq(i).nextlineStart)))
  private val s0_vSetIdxAll = (0 until partWayNum + 1).map(i => VecInit(s0_vAddrAll(i).map(get_idx)))
  private val s0_offsetAll  = (0 until partWayNum + 1).map(i => s0_vAddrAll(i)(0)(log2Ceil(blockBytes) - 1, 0))
  private val s0_doublelineAll =
    (0 until partWayNum + 1).map(i => fromFtq.bits.readValid(i) && fromFtqReq(i).crossCacheline)

  private val s0_vAddr      = s0_vAddrAll.last
  private val s0_vSetIdx    = s0_vSetIdxAll.last
  private val s0_doubleline = s0_doublelineAll.last

  private val s0_isBackendException = fromFtq.bits.isBackendException

  /**
    ******************************************************************************
    * get waymask and tlb info from wayLookup
    ******************************************************************************
    */
  fromWayLookup.ready := s0_fire
  private val s0_waymasks          = VecInit(fromWayLookup.bits.waymask.map(_.asTypeOf(Vec(nWays, Bool()))))
  private val s0_pTags             = fromWayLookup.bits.pTag
  private val s0_gpAddr            = fromWayLookup.bits.gpAddr
  private val s0_isForVSnonLeafPTE = fromWayLookup.bits.isForVSnonLeafPTE
  private val s0_itlbException     = fromWayLookup.bits.itlbException
  private val s0_itlbPbmt          = fromWayLookup.bits.itlbPbmt
  private val s0_metaCodes         = fromWayLookup.bits.metaCodes
  private val s0_maybeRvcMap       = fromWayLookup.bits.maybeRvcMap
  private val s0_hits              = VecInit(fromWayLookup.bits.waymask.map(_.orR))

  when(s0_fire) {
    assert(
      (0 until PortNumber).map(i => s0_vSetIdx(i) === fromWayLookup.bits.vSetIdx(i)).reduce(_ && _),
      "vSetIdx from ftq and wayLookup mismatch! vAddr=0x%x ftq: vSet0=0x%x vSet1=0x%x wayLookup: vSet0=0x%x vSet1=0x%x",
      s0_vAddr(0).toUInt,
      s0_vSetIdx(0),
      s0_vSetIdx(1),
      fromWayLookup.bits.vSetIdx(0),
      fromWayLookup.bits.vSetIdx(1)
    )
  }

  /**
    ******************************************************************************
    * data SRAM request
    ******************************************************************************
    */
  (0 until partWayNum).foreach { i =>
    toData(i).valid             := s0_readValidAll(i)
    toData(i).bits.isDoubleLine := s0_doublelineAll(i)
    toData(i).bits.vSetIdx      := s0_vSetIdxAll(i)
    toData(i).bits.blkOffset    := s0_offsetAll(i)
    toData(i).bits.waymask      := s0_waymasks
  }

  private val s0_canGo = toData.last.ready && fromWayLookup.valid && s1_ready
  s0_flush := io.flush
  s0_fire  := s0_valid && s0_canGo && !s0_flush

  fromFtq.ready := s0_canGo

  /**
    ******************************************************************************
    * ICache Stage 1
    * - PMP check
    * - get Data SRAM read responses (latched for pipeline stop)
    * - monitor missUint response port
    ******************************************************************************
    */
  private val s1_valid = ValidHold(s0_fire, s1_fire, s1_flush)

  private val s1_vAddr  = RegEnable(s0_vAddr, 0.U.asTypeOf(s0_vAddr), s0_fire)
  private val s1_pTags  = RegEnable(s0_pTags, 0.U.asTypeOf(s0_pTags), s0_fire)
  private val s1_gpAddr = RegEnable(s0_gpAddr, 0.U.asTypeOf(s0_gpAddr), s0_fire)
  private val s1_isForVSnonLeafPTE =
    RegEnable(s0_isForVSnonLeafPTE, 0.U.asTypeOf(s0_isForVSnonLeafPTE), s0_fire)
  private val s1_doubleline         = RegEnable(s0_doubleline, 0.U.asTypeOf(s0_doubleline), s0_fire)
  private val s1_sramHits           = RegEnable(s0_hits, 0.U.asTypeOf(s0_hits), s0_fire)
  private val s1_itlbException      = RegEnable(s0_itlbException, 0.U.asTypeOf(s0_itlbException), s0_fire)
  private val s1_isBackendException = RegEnable(s0_isBackendException, false.B, s0_fire)
  private val s1_itlbPbmt           = RegEnable(s0_itlbPbmt, 0.U.asTypeOf(s0_itlbPbmt), s0_fire)
  private val s1_waymasks           = RegEnable(s0_waymasks, 0.U.asTypeOf(s0_waymasks), s0_fire)
  private val s1_metaCodes          = RegEnable(s0_metaCodes, 0.U.asTypeOf(s0_metaCodes), s0_fire)
  private val s1_sramMaybeRvcMap    = RegEnable(s0_maybeRvcMap, 0.U.asTypeOf(s0_maybeRvcMap), s0_fire)

  private val s1_vSetIdx = VecInit(s1_vAddr.map(get_idx))
  private val s1_pAddr   = getPAddrFromPTag(s1_vAddr, s1_pTags)
  private val s1_offset  = s1_vAddr(0)(log2Ceil(blockBytes) - 1, 0)

  // do metaArray ECC check
  private val s1_metaCorrupt = checkMetaEcc(
    VecInit((s1_pTags zip s1_sramMaybeRvcMap).map { case (pt, rvc) => ICacheMetadata(pt, rvc) }),
    s1_metaCodes,
    s1_waymasks,
    eccEnable
  )

  /**
    ******************************************************************************
    * update replacement status register
    ******************************************************************************
    */
  (0 until PortNumber).foreach { i =>
    io.replacerTouch.req(i).bits.vSetIdx := s1_vSetIdx(i)
    io.replacerTouch.req(i).bits.way     := OHToUInt(s1_waymasks(i))
  }
  io.replacerTouch.req(0).valid := RegNext(s0_fire) && s1_sramHits(0)
  io.replacerTouch.req(1).valid := RegNext(s0_fire) && s1_sramHits(1) && s1_doubleline

  /**
    ******************************************************************************
    * PMP check
    ******************************************************************************
    */
  toPmp.zipWithIndex.foreach { case (p, i) =>
    // if itlb has exception, pAddr can be invalid, therefore pmp check can be skipped do not do this now for timing
    p.valid     := s1_valid // && !ExceptionType.hasException(s1_itlbException(i))
    p.bits.addr := s1_pAddr(i).toUInt
    p.bits.size := 3.U
    p.bits.cmd  := TlbCmd.exec
  }
  private val s1_pmpException = VecInit(fromPmp.map(ExceptionType.fromPMPResp))
  private val s1_pmpMmio      = VecInit(fromPmp.map(_.mmio))

  // merge s1 itlb/pmp exceptions, itlb has the highest priority, pmp next
  private val s1_exceptionOut = ExceptionType.merge(
    s1_itlbException,
    s1_pmpException
  )

  /**
    ******************************************************************************
    * select data from MSHR, SRAM
    ******************************************************************************
    */
  private val s1_mshrHits = checkMshrHitVec(
    fromMiss,
    s1_vSetIdx,
    s1_pTags,
    VecInit(s1_valid, s1_valid && s1_doubleline)
  )
  private val s1_mshrDatas = fromMiss.bits.data.asTypeOf(Vec(ICacheDataBanks, UInt((blockBits / ICacheDataBanks).W)))

  private val s1_hits = (0 until PortNumber).map { i =>
    ValidHoldBypass(s1_mshrHits(i) || (RegNext(s0_fire) && s1_sramHits(i)), s1_fire || s1_flush)
  }

  private val s1_bankMshrHit = getBankValid(s1_mshrHits, s1_offset)

  private val s1_datas = VecInit((0 until ICacheDataBanks).map { i =>
    DataHoldBypass(Mux(s1_bankMshrHit(i), s1_mshrDatas(i), fromData.datas(i)), s1_bankMshrHit(i) || RegNext(s0_fire))
  })
  private val s1_dataIsFromMshr = VecInit((0 until ICacheDataBanks).map { i =>
    DataHoldBypass(s1_bankMshrHit(i), s1_bankMshrHit(i) || RegNext(s0_fire))
  })
  private val s1_codes = DataHoldBypass(fromData.codes, RegNext(s0_fire))

  private val s1_maybeRvcMap = VecInit((0 until PortNumber).map { i =>
    DataHoldBypass(
      Mux(s1_mshrHits(i), fromMiss.bits.maybeRvcMap, s1_sramMaybeRvcMap(i)),
      s1_mshrHits(i) || RegNext(s0_fire)
    )
  })

  s1_flush := io.flush
  s1_ready := s2_ready || !s1_valid
  s1_fire  := s1_valid && s2_ready && !s1_flush

  /**
    ******************************************************************************
    * ICache Stage 2
    * - send request to MSHR if ICache miss
    * - monitor missUint response port
    * - response to IFU
    ******************************************************************************
    */
  private val s2_valid = ValidHold(s1_fire, s2_fire, s2_flush)

  private val s2_vAddr  = RegEnable(s1_vAddr, 0.U.asTypeOf(s1_vAddr), s1_fire)
  private val s2_pTags  = RegEnable(s1_pTags, 0.U.asTypeOf(s1_pTags), s1_fire)
  private val s2_gpAddr = RegEnable(s1_gpAddr, 0.U.asTypeOf(s1_gpAddr), s1_fire)
  private val s2_isForVSnonLeafPTE =
    RegEnable(s1_isForVSnonLeafPTE, 0.U.asTypeOf(s1_isForVSnonLeafPTE), s1_fire)
  private val s2_doubleline         = RegEnable(s1_doubleline, 0.U.asTypeOf(s1_doubleline), s1_fire)
  private val s2_exception          = RegEnable(s1_exceptionOut, 0.U.asTypeOf(s1_exceptionOut), s1_fire)
  private val s2_isBackendException = RegEnable(s1_isBackendException, false.B, s1_fire)
  private val s2_pmpMmio            = RegEnable(s1_pmpMmio, 0.U.asTypeOf(s1_pmpMmio), s1_fire)
  private val s2_itlbPbmt           = RegEnable(s1_itlbPbmt, 0.U.asTypeOf(s1_itlbPbmt), s1_fire)
  private val s2_waymasks           = RegEnable(s1_waymasks, 0.U.asTypeOf(s1_waymasks), s1_fire)

  private val s2_vSetIdx = VecInit(s2_vAddr.map(get_idx))
  private val s2_offset  = s2_vAddr(0)(log2Ceil(blockBytes) - 1, 0)
  private val s2_pAddr   = getPAddrFromPTag(s2_vAddr, s2_pTags)

  private val s2_sramHits       = RegEnable(s1_sramHits, 0.U.asTypeOf(s1_sramHits), s1_fire)
  private val s2_codes          = RegEnable(s1_codes, 0.U.asTypeOf(s1_codes), s1_fire)
  private val s2_hits           = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  private val s2_datas          = RegInit(VecInit(Seq.fill(ICacheDataBanks)(0.U((blockBits / ICacheDataBanks).W))))
  private val s2_dataIsFromMshr = RegInit(VecInit(Seq.fill(ICacheDataBanks)(false.B)))
  private val s2_maybeRvcMap    = RegInit(VecInit(Seq.fill(PortNumber)(0.U(MaxInstNumPerBlock.W))))
  private val s2_l2Corrupt      = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))

  /**
    ******************************************************************************
    * ECC check
    ******************************************************************************
    */
  // check data error
  private val s2_dataCorrupt = checkDataEcc(
    s2_datas,
    s2_codes,
    eccEnable,
    getBankSel(s2_offset, s2_valid),
    VecInit(s2_dataIsFromMshr.map(!_)),
    s2_sramHits
  )
  // force clear data_corrupt when parity check is disabled
  when(!eccEnable) {
    s2_dataCorrupt := VecInit(Seq.fill(PortNumber)(false.B))
  }
  // meta error is checked in s1 stage
  private val s2_metaCorrupt = RegEnable(s1_metaCorrupt, 0.U.asTypeOf(s1_metaCorrupt), s1_fire)
  // send errors to top
  // TODO: support RERI spec standard interface
  (0 until PortNumber).foreach { i =>
    io.errors(i).valid              := (s2_metaCorrupt(i) || s2_dataCorrupt(i)) && RegNext(s1_fire)
    io.errors(i).bits.report_to_beu := (s2_metaCorrupt(i) || s2_dataCorrupt(i)) && RegNext(s1_fire)
    io.errors(i).bits.paddr         := s2_pAddr(i).toUInt
    io.errors(i).bits.source        := DontCare
    io.errors(i).bits.source.tag    := s2_metaCorrupt(i)
    io.errors(i).bits.source.data   := s2_dataCorrupt(i)
    io.errors(i).bits.source.l2     := false.B
    io.errors(i).bits.opType        := DontCare
    io.errors(i).bits.opType.fetch  := true.B
  }
  // flush metaArray to prepare for re-fetch
  (0 until PortNumber).foreach { i =>
    toMetaFlush(i).valid        := (s2_metaCorrupt(i) || s2_dataCorrupt(i)) && RegNext(s1_fire)
    toMetaFlush(i).bits.vSetIdx := s2_vSetIdx(i)
    // if is meta corrupt, clear all way (since waymask may be unreliable)
    // if is data corrupt, only clear the way that has error
    toMetaFlush(i).bits.waymask := Mux(s2_metaCorrupt(i), Fill(nWays, true.B), s2_waymasks(i).asUInt)
  }
  // PERF: count the number of data parity errors
  XSPerfAccumulate("data_corrupt_0", s2_dataCorrupt(0) && RegNext(s1_fire))
  XSPerfAccumulate("data_corrupt_1", s2_dataCorrupt(1) && RegNext(s1_fire))
  XSPerfAccumulate("meta_corrupt_0", s2_metaCorrupt(0) && RegNext(s1_fire))
  XSPerfAccumulate("meta_corrupt_1", s2_metaCorrupt(1) && RegNext(s1_fire))
  // TEST: stop simulation if parity error is detected, and dump wave
//  val (assert_valid, assert_val) = DelayNWithValid(s2_metaCorrupt.reduce(_ || _), s2_valid, 1000)
//  assert(!(assert_valid && assert_val))
//  val (assert_valid, assert_val) = DelayNWithValid(s2_dataCorrupt.reduce(_ || _), s2_valid, 1000)
//  assert(!(assert_valid && assert_val))

  /**
    ******************************************************************************
    * monitor missUint response port
    ******************************************************************************
    */
  private val s2_mshrHits = checkMshrHitVec(
    fromMiss,
    s2_vSetIdx,
    s2_pTags,
    VecInit(s2_valid, s2_valid && s2_doubleline),
    allowCorrupt = true // we also need to update s2_hits when fromMiss.bits.corrupt
  )
  private val s2_mshrDatas = fromMiss.bits.data.asTypeOf(Vec(ICacheDataBanks, UInt((blockBits / ICacheDataBanks).W)))

  private val s2_bankMshrHit = getBankValid(s2_mshrHits, s2_offset)

  (0 until ICacheDataBanks).foreach { i =>
    when(s1_fire) {
      s2_datas          := s1_datas
      s2_dataIsFromMshr := s1_dataIsFromMshr
    }.elsewhen(s2_bankMshrHit(i)) {
      s2_datas(i) := s2_mshrDatas(i)
      // also update s2_dataIsFromMshr when re-fetched, to clear s2_dataCorrupt flag and let s2_fire
      s2_dataIsFromMshr(i) := true.B
    }
  }

  (0 until PortNumber).foreach { i =>
    when(s1_fire) {
      s2_hits         := s1_hits
      s2_maybeRvcMap  := s1_maybeRvcMap
      s2_l2Corrupt(i) := false.B
    }.elsewhen(s2_mshrHits(i)) {
      // update s2_hits even if it's corrupt, to let s2_fire
      s2_hits(i) := true.B
      // also clear s2_metaCorrupt flag when re-fetched, to let s2_fire
      s2_metaCorrupt(i) := false.B
      // and update info
      s2_maybeRvcMap(i) := fromMiss.bits.maybeRvcMap
      s2_l2Corrupt(i)   := fromMiss.bits.corrupt
    }
  }

  /**
    ******************************************************************************
    * send request to MSHR if ICache miss / ECC corrupt
    ******************************************************************************
    */

  // merge pmp mmio and itlb pbmt
  private val s2_isMmio = VecInit((s2_pmpMmio zip s2_itlbPbmt).map { case (mmio, pbmt) =>
    mmio || Pbmt.isUncache(pbmt)
  })

  // try re-fetch data from L2 cache if ECC error is detected, unless it's from MSHR
  private val s2_corruptRefetch = (s2_metaCorrupt zip s2_dataCorrupt).map {
    case (meta, data) => meta || data
  }

  /* s2_exception includes itlb pf/gpf/af, pmp af and meta corruption (af), neither of which should be fetched
   * mmio should not be fetched, it will be fetched by IFU mmio fsm
   * also, if previous has exception, latter port should also not be fetched
   */
  private val s2_shouldFetch = VecInit((0 until PortNumber).map { i =>
    (!s2_hits(i) || s2_corruptRefetch(i)) &&
    (if (i == 0) true.B else s2_doubleline) &&
    !ExceptionType.hasException(s2_exception.take(i + 1)) &&
    s2_isMmio.take(i + 1).map(!_).reduce(_ && _)
  })

  private val toMissArbiter = Module(new Arbiter(new MissReqBundle, PortNumber))

  // To avoid sending duplicate requests.
  private val s2_hasSend = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach { i =>
    when(s1_fire) {
      s2_hasSend(i) := false.B
    }.elsewhen(toMissArbiter.io.in(i).fire) {
      s2_hasSend(i) := true.B
    }
  }

  (0 until PortNumber).foreach { i =>
    toMissArbiter.io.in(i).valid         := s2_valid && s2_shouldFetch(i) && !s2_hasSend(i) && !s2_flush
    toMissArbiter.io.in(i).bits.blkPAddr := getBlkAddr(s2_pAddr(i))
    toMissArbiter.io.in(i).bits.vSetIdx  := s2_vSetIdx(i)
  }
  toMiss <> toMissArbiter.io.out

  XSPerfAccumulate("to_missUnit_stall", toMiss.valid && !toMiss.ready)

  private val s2_fetchFinish = !s2_shouldFetch.reduce(_ || _)

  // also raise af if l2 corrupt is detected
  private val s2_l2Exception = VecInit(s2_l2Corrupt.map(ExceptionType.fromTilelink))
  // NOTE: do NOT raise af if meta/data corrupt is detected, they are automatically recovered by re-fetching from L2

  // merge s2 exceptions, itlb has the highest priority, then l2
  private val s2_exceptionOut = ExceptionType.merge(
    s2_exception, // includes itlb/pmp exception
    s2_l2Exception
  )

  /**
    ******************************************************************************
    * response to IFU
    ******************************************************************************
    */
  toIfuS1.valid := s1_fire
  (0 until PortNumber).foreach { i =>
    val needThisLine = if (i == 0) true.B else s1_doubleline
    toIfuS1.bits.maybeRvcMap(i).valid := s1_fire && s1_hits(i) && needThisLine
    toIfuS1.bits.maybeRvcMap(i).bits  := s1_maybeRvcMap(i)
  }

  toIfuS2.valid                   := s2_fire
  toIfuS2.bits.doubleline         := s2_doubleline
  toIfuS2.bits.data               := s2_datas.asTypeOf(UInt(blockBits.W))
  toIfuS2.bits.maybeRvcMap        := s2_maybeRvcMap
  toIfuS2.bits.isBackendException := s2_isBackendException
  toIfuS2.bits.vAddr              := s2_vAddr
  toIfuS2.bits.pAddr              := s2_pAddr
  (0 until PortNumber).foreach { i =>
    val needThisLine = if (i == 0) true.B else s2_doubleline
    toIfuS2.bits.exception(i) := Mux(needThisLine, s2_exceptionOut(i), ExceptionType.none)
    toIfuS2.bits.pmpMmio(i)   := Mux(needThisLine, s2_pmpMmio(i), false.B)
    toIfuS2.bits.itlbPbmt(i)  := Mux(needThisLine, s2_itlbPbmt(i), Pbmt.pma)
  }
  // valid only for the first gpf
  toIfuS2.bits.gpAddr            := s2_gpAddr
  toIfuS2.bits.isForVSnonLeafPTE := s2_isForVSnonLeafPTE

  s2_flush := io.flush
  s2_ready := (s2_fetchFinish && !io.respStall) || !s2_valid
  s2_fire  := s2_valid && s2_fetchFinish && !io.respStall && !s2_flush

  /**
    ******************************************************************************
    * report Tilelink corrupt error
    ******************************************************************************
    */
  (0 until PortNumber).foreach { i =>
    when(RegNext(s2_fire && s2_l2Corrupt(i))) {
      io.errors(i).valid              := true.B
      io.errors(i).bits.report_to_beu := false.B // l2 should have report that to bus error unit, no need to do it again
      io.errors(i).bits.paddr         := RegNext(s2_pAddr(i).toUInt)
      io.errors(i).bits.source.tag    := false.B
      io.errors(i).bits.source.data   := false.B
      io.errors(i).bits.source.l2     := true.B
    }
  }

  /**
    ******************************************************************************
    * performance info. TODO: need to simplify the logic
    ***********************************************************s*******************
    */
  io.perf.only0Hit     := s2_hits(0) && !s2_doubleline
  io.perf.only0Miss    := !s2_hits(0) && !s2_doubleline
  io.perf.hit0Hit1     := s2_hits(0) && s2_hits(1) && s2_doubleline
  io.perf.hit0Miss1    := s2_hits(0) && !s2_hits(1) && s2_doubleline
  io.perf.miss0Hit1    := !s2_hits(0) && s2_hits(1) && s2_doubleline
  io.perf.miss0Miss1   := !s2_hits(0) && !s2_hits(1) && s2_doubleline
  io.perf.hit0Except1  := s2_hits(0) && ExceptionType.hasException(s2_exception(1)) && s2_doubleline
  io.perf.miss0Except1 := !s2_hits(0) && ExceptionType.hasException(s2_exception(1)) && s2_doubleline
  io.perf.bankHit(0)   := s2_hits(0)
  io.perf.bankHit(1)   := s2_hits(1) && s2_doubleline
  io.perf.except0      := ExceptionType.hasException(s2_exception(0))
  io.perf.hit          := s2_hits(0) && (!s2_doubleline || s2_hits(1))

  /** <PERF> fetch bubble generated by icache miss */
  XSPerfAccumulate("icache_bubble_s2_miss", s2_valid && !s2_fetchFinish)
  XSPerfAccumulate("icache_bubble_s0_wayLookup", s0_valid && !fromWayLookup.ready)

  io.topdown.icacheMiss := !s2_fetchFinish
  io.topdown.itlbMiss   := s0_valid && !fromWayLookup.ready

  // class ICacheTouchDB(implicit p: Parameters) extends ICacheBundle{
  //   val blkPAddr  = UInt((PAddrBits - blockOffBits).W)
  //   val vSetIdx   = UInt(idxBits.W)
  //   val waymask   = UInt(wayBits.W)
  // }

  // private val isWriteICacheTouchTable =
  //   WireInit(Constantin.createRecord("isWriteICacheTouchTable" + p(XSCoreParamsKey).HartId.toString))
  // private val ICacheTouchTable =
  //   ChiselDB.createTable("ICacheTouchTable" + p(XSCoreParamsKey).HartId.toString, new ICacheTouchDB)

  // val ICacheTouchDumpData = Wire(Vec(PortNumber, new ICacheTouchDB))
  // (0 until PortNumber).foreach{ i =>
  //   ICacheTouchDumpData(i).blkPAddr  := getBlkAddr(s2_pAddr(i))
  //   ICacheTouchDumpData(i).vSetIdx   := s2_vSetIdx(i)
  //   ICacheTouchDumpData(i).waymask   := OHToUInt(s2_tag_match_vec(i))
  //   ICacheTouchTable.log(
  //     data  = ICacheTouchDumpData(i),
  //     en    = io.replacerTouch.req(i).valid,
  //     site  = "req_" + i.toString,
  //     clock = clock,
  //     reset = reset
  //   )
  // }

  /**
    ******************************************************************************
    * difftest refill check
    ******************************************************************************
    */
  if (env.EnableDifftest) {
    val discards = (0 until PortNumber).map { i =>
      ExceptionType.hasException(toIfuS2.bits.exception(i)) ||
      toIfuS2.bits.pmpMmio(i) ||
      Pbmt.isUncache(toIfuS2.bits.itlbPbmt(i))
    }
    val blkPaddrAll = s2_pAddr.map(addr => (addr(PAddrBits - 1, blockOffBits) << blockOffBits).asUInt)
    (0 until ICacheDataBanks).foreach { i =>
      val diffMainPipeOut = DifftestModule(new DiffRefillEvent, dontCare = true)
      diffMainPipeOut.coreid := io.hartId
      diffMainPipeOut.index  := (3 + i).U

      val bankSel = getBankSel(s2_offset, s2_valid).map(_.asUInt).reduce(_ | _)
      val lineSel = getLineSel(s2_offset)

      diffMainPipeOut.valid := s2_fire && bankSel(i).asBool && Mux(lineSel(i), !discards(1), !discards(0))
      diffMainPipeOut.addr := Mux(
        lineSel(i),
        blkPaddrAll(1) + (i.U << log2Ceil(blockBytes / ICacheDataBanks)).asUInt,
        blkPaddrAll(0) + (i.U << log2Ceil(blockBytes / ICacheDataBanks)).asUInt
      )

      diffMainPipeOut.data  := s2_datas(i).asTypeOf(diffMainPipeOut.data)
      diffMainPipeOut.idtfr := DontCare
    }
  }
}
