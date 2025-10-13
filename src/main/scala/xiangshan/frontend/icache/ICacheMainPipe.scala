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
import xiangshan.frontend.FtqFetchRequest
import xiangshan.frontend.ftq.BpuFlushInfo

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
    val req:          DecoupledIO[FtqFetchRequest] = Flipped(DecoupledIO(new FtqFetchRequest))
    val flush:        Bool                         = Input(Bool())
    val flushFromBpu: BpuFlushInfo                 = Input(new BpuFlushInfo)
    // Pmp
    val pmp: PmpCheckBundle = new PmpCheckBundle
    // Ifu
    val resp:      Valid[ICacheRespBundle] = ValidIO(new ICacheRespBundle)
    val respStall: Bool                    = Input(Bool())
    // backend/Beu
    val errors: Vec[Valid[L1CacheErrorInfo]] = Output(Vec(PortNumber, ValidIO(new L1CacheErrorInfo)))

    val perf: MainPipePerfInfo = Output(new MainPipePerfInfo)
  }

  val io: ICacheMainPipeIO = IO(new ICacheMainPipeIO)

  /* *** Input/Output port *** */
  private val (fromFtq, toIfu)   = (io.req, io.resp)
  private val (toData, fromData) = (io.dataRead.req, io.dataRead.resp)
  private val toMetaFlush        = io.metaFlush.req
  private val (toMiss, fromMiss) = (io.missReq, io.missResp)
  private val (toPmp, fromPmp)   = (io.pmp.req, io.pmp.resp)
  private val fromWayLookup      = io.wayLookupRead
  private val eccEnable =
    if (ForceMetaEccFail || ForceDataEccFail) true.B else io.eccEnable

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

  /* *** pipeline control signal *** */
  private val s1_ready           = Wire(Bool())
  private val s0_fire, s1_fire   = Wire(Bool())
  private val s0_flush, s1_flush = Wire(Bool())

  /* ICache Stage 0
   * - send req to data SRAM
   * - get waymask and tlb info from wayLookup
   */

  /** s0 control */
  private val fromFtqReq = fromFtq.bits
  private val s0_valid   = fromFtq.valid

  private val s0_ftqIdx    = fromFtqReq.ftqIdx
  private val s0_vAddr     = VecInit(Seq(fromFtqReq.startVAddr, fromFtqReq.nextCachelineVAddr))
  private val s0_vSetIdx   = VecInit(s0_vAddr.map(get_idx))
  private val s0_blkOffset = fromFtqReq.startVAddr(blockOffBits - 1, 0)

  private val s0_blkEndOffsetTmp = s0_blkOffset +& Cat(fromFtqReq.takenCfiOffset, 0.U(instOffsetBits.W))
  private val s0_blkEndOffset    = s0_blkEndOffsetTmp(blockOffBits - 1, 0)
  private val s0_doubleline      = s0_valid && s0_blkEndOffsetTmp(blockOffBits)

  private val s0_isBackendException = fromFtqReq.isBackendException

  /**
    ******************************************************************************
    * get waymask and tlb info from wayLookup
    ******************************************************************************
    */
  fromWayLookup.ready := s0_fire
  private val s0_waymasks          = VecInit(fromWayLookup.bits.waymask.map(_.asTypeOf(Vec(nWays, Bool()))))
  private val s0_pTag              = fromWayLookup.bits.pTag
  private val s0_gpAddr            = fromWayLookup.bits.gpAddr
  private val s0_isForVSnonLeafPTE = fromWayLookup.bits.isForVSnonLeafPTE
  private val s0_itlbException     = fromWayLookup.bits.itlbException
  private val s0_itlbPbmt          = fromWayLookup.bits.itlbPbmt
  private val s0_maybeRvcMap       = fromWayLookup.bits.maybeRvcMap
  private val s0_metaCodes         = fromWayLookup.bits.metaCodes
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
  toData.valid             := s0_valid
  toData.bits.isDoubleLine := s0_doubleline
  toData.bits.vSetIdx      := s0_vSetIdx
  toData.bits.blkOffset    := s0_blkOffset
  toData.bits.blkEndOffset := s0_blkEndOffset
  toData.bits.waymask      := s0_waymasks

  private val s0_canGo = toData.ready && fromWayLookup.valid && s1_ready
  s0_flush := io.flush || io.flushFromBpu.shouldFlushByStage3(s0_ftqIdx, s0_valid)
  s0_fire  := s0_valid && s0_canGo && !s0_flush

  fromFtq.ready := s0_canGo

  /* ICache Stage 1
   * - Pmp check (to be removed)
   * - get Data Sram read responses (latched for pipeline stop)
   * - monitor missUnit response port
   * - Ecc check
   * - send request to Mshr if ICache miss
   * - response to Ifu
   */
  private val s1_valid = ValidHold(s0_fire, s1_fire, s1_flush)

  private val s1_ftqIdx = RegEnable(s0_ftqIdx, 0.U.asTypeOf(s0_ftqIdx), s0_fire)
  private val s1_vAddr  = RegEnable(s0_vAddr, 0.U.asTypeOf(s0_vAddr), s0_fire)
  private val s1_pTag   = RegEnable(s0_pTag, 0.U(tagBits.W), s0_fire)
  private val s1_gpAddr = RegEnable(s0_gpAddr, 0.U.asTypeOf(s0_gpAddr), s0_fire)
  private val s1_isForVSnonLeafPTE =
    RegEnable(s0_isForVSnonLeafPTE, 0.U.asTypeOf(s0_isForVSnonLeafPTE), s0_fire)
  private val s1_doubleline         = RegEnable(s0_doubleline, 0.U.asTypeOf(s0_doubleline), s0_fire)
  private val s1_itlbException      = RegEnable(s0_itlbException, 0.U.asTypeOf(s0_itlbException), s0_fire)
  private val s1_isBackendException = RegEnable(s0_isBackendException, false.B, s0_fire)
  private val s1_itlbPbmt           = RegEnable(s0_itlbPbmt, 0.U.asTypeOf(s0_itlbPbmt), s0_fire)
  private val s1_waymasks           = RegEnable(s0_waymasks, 0.U.asTypeOf(s0_waymasks), s0_fire)
  private val s1_sramMaybeRvcMapRaw = RegEnable(s0_maybeRvcMap, 0.U.asTypeOf(s0_maybeRvcMap), s0_fire)
  private val s1_metaCodes          = RegEnable(s0_metaCodes, 0.U.asTypeOf(s0_metaCodes), s0_fire)

  private val s1_vSetIdx = VecInit(s1_vAddr.map(get_idx))
  private val s1_offset  = s1_vAddr(0)(log2Ceil(blockBytes) - 1, 0)

  private val s1_blkEndOffset = RegEnable(s0_blkEndOffset, 0.U.asTypeOf(s0_blkEndOffset), s0_fire)

  /* *******************************************************************
   * Receive data from sram and mshr
   * ******************************************************************* */
  // sram: valid when RegNext(s0_fire)
  private val s1_sramHits  = RegEnable(s0_hits, 0.U.asTypeOf(s0_hits), s0_fire)
  private val s1_sramDatas = fromData.datas
  private val s1_sramCodes = fromData.codes

  // mshr: valid when fromMiss.valid
  private val s1_mshrHits = checkMshrHitVec(
    fromMiss,
    s1_vSetIdx,
    s1_pTag,
    VecInit(s1_valid, s1_valid && s1_doubleline),
    allowCorrupt = true // we also need to update registers when fromMiss.bits.corrupt
  )
  private val s1_mshrDatas = fromMiss.bits.data.asTypeOf(Vec(DataBanks, UInt(ICacheDataBits.W)))

  // select data
  private val s1_bankMshrHit = getBankValid(s1_mshrHits, s1_offset)

  private val s1_dataIsFromMshr = VecInit((0 until DataBanks).map { i =>
    DataHoldBypass(
      s1_bankMshrHit(i),
      s1_bankMshrHit(i) || RegNext(s0_fire)
    )
  })

  // select maybeRvc
  private val s1_sramMaybeRvcMap =
    s1_sramMaybeRvcMapRaw.asTypeOf(Vec(PortNumber, Vec(DataBanks, UInt(MaxInstNumPerBank.W))))
  private val s1_mshrMaybeRvcMap =
    fromMiss.bits.maybeRvcMap.asTypeOf(Vec(DataBanks, UInt(MaxInstNumPerBank.W)))

  private val s1_hits = VecInit((0 until PortNumber).map { i =>
    DataHoldBypass(
      s1_mshrHits(i) || s1_sramHits(i),
      s1_mshrHits(i) || RegNext(s0_fire)
    )
  })

  private val s1_datas = VecInit((0 until DataBanks).map { i =>
    DataHoldBypass(
      Mux(s1_bankMshrHit(i), s1_mshrDatas(i), s1_sramDatas(i)),
      s1_bankMshrHit(i) || RegNext(s0_fire)
    )
  })

  private val s1_maybeRvcMap = VecInit((0 until DataBanks).map { i =>
    DataHoldBypass(
      Mux(
        s1_bankMshrHit(i),
        s1_mshrMaybeRvcMap(i),
        Mux(getLineSel(s1_offset)(i), s1_sramMaybeRvcMap(1)(i), s1_sramMaybeRvcMap(0)(i))
      ),
      s1_bankMshrHit(i) || RegNext(s0_fire)
    )
  })

  private val s1_l2Corrupt = VecInit((0 until PortNumber).map { i =>
    DataHoldBypass(
      s1_mshrHits(i) && fromMiss.bits.corrupt,
      s1_mshrHits(i) || RegNext(s0_fire)
    )
  })

  /* *** Update replacer *** */
  (0 until PortNumber).foreach { i =>
    io.replacerTouch.req(i).bits.vSetIdx := s1_vSetIdx(i)
    io.replacerTouch.req(i).bits.way     := OHToUInt(s1_waymasks(i))
  }
  io.replacerTouch.req(0).valid := RegNext(s0_fire) && s1_sramHits(0)
  io.replacerTouch.req(1).valid := RegNext(s0_fire) && s1_sramHits(1) && s1_doubleline

  /* *** PMP check (to be removed) *** */
  // if itlb has exception, pAddr can be invalid, therefore pmp check can be skipped do not do this now for timing
  toPmp.valid     := s1_valid // && !ExceptionType.hasException(s1_itlbException(i))
  toPmp.bits.addr := getPAddrFromPTag(s1_vAddr.head, s1_pTag).toUInt
  toPmp.bits.size := 3.U
  toPmp.bits.cmd  := TlbCmd.exec
  private val s1_pmpException = ExceptionType.fromPmpResp(fromPmp)
  private val s1_pmpMmio      = fromPmp.mmio

  // merge s1 itlb/pmp exceptions, itlb has the highest priority, pmp next, note this `||` is overloaded
  private val s1_exception = s1_itlbException || s1_pmpException

  /* *** Ecc check *** */
  private val s1_metaCorrupt =
    checkMetaEcc(
      VecInit(s1_sramMaybeRvcMapRaw.map(rvc => ICacheMetadata(s1_pTag, rvc))),
      s1_metaCodes,
      s1_waymasks,
      eccEnable,
      s1_doubleline
    )

  // valid only when RegNext(s0_fire)
  // check data error
  private val s1_dataCorrupt = checkDataEcc(
    s1_sramDatas,
    s1_sramCodes,
    eccEnable,
    getBankSel(s1_offset, s1_valid, s1_doubleline),
    VecInit(s1_dataIsFromMshr.map(!_)),
    s1_sramHits
  )

  /* NOTE: if !s1_doubleline:
   * - s1_meta_corrupt(1) should be false.B (as waymask(1) is invalid, and meta ecc is not checked)
   * - s1_data_corrupt(1) should also be false.B, as getLineSel() should not select line(1)
   * so we don't need to check s2_doubleline in the following io.errors and toMetaFlush ports
   * we add a sanity check to make sure the above assumption holds
   */
  assert(
    !(!s1_doubleline && (s1_metaCorrupt(1) || s1_dataCorrupt(1))),
    "meta or data corrupt detected on line 1 but s2_doubleline is false.B"
  )

  // send errors to top
  // TODO: support RERI spec standard interface
  (0 until PortNumber).foreach { i =>
    io.errors(i).valid              := (s1_metaCorrupt(i) || s1_dataCorrupt(i)) && RegNext(s0_fire)
    io.errors(i).bits.report_to_beu := (s1_metaCorrupt(i) || s1_dataCorrupt(i)) && RegNext(s0_fire)
    io.errors(i).bits.paddr         := getPAddrFromPTag(s1_vAddr(i), s1_pTag).toUInt
    io.errors(i).bits.source        := DontCare
    io.errors(i).bits.source.tag    := s1_metaCorrupt(i)
    io.errors(i).bits.source.data   := s1_dataCorrupt(i)
    io.errors(i).bits.source.l2     := false.B
    io.errors(i).bits.opType        := DontCare
    io.errors(i).bits.opType.fetch  := true.B
  }
  // flush metaArray to prepare for re-fetch
  (0 until PortNumber).foreach { i =>
    toMetaFlush(i).valid        := (s1_metaCorrupt(i) || s1_dataCorrupt(i)) && RegNext(s0_fire)
    toMetaFlush(i).bits.vSetIdx := s1_vSetIdx(i)
    // if is meta corrupt, clear all way (since waymask may be unreliable)
    // if is data corrupt, only clear the way that has error
    toMetaFlush(i).bits.waymask := Mux(s1_metaCorrupt(i), Fill(nWays, true.B), s1_waymasks(i).asUInt)
  }
  // PERF: count the number of data parity errors
  XSPerfAccumulate("data_corrupt_0", s1_dataCorrupt(0) && RegNext(s0_fire))
  XSPerfAccumulate("data_corrupt_1", s1_dataCorrupt(1) && RegNext(s0_fire))
  XSPerfAccumulate("meta_corrupt_0", s1_metaCorrupt(0) && RegNext(s0_fire))
  XSPerfAccumulate("meta_corrupt_1", s1_metaCorrupt(1) && RegNext(s0_fire))
  // TEST: stop simulation if parity error is detected, and dump wave
//  val (assert_valid, assert_val) = DelayNWithValid(s2_metaCorrupt.reduce(_ || _), s2_valid, 1000)
//  assert(!(assert_valid && assert_val))
//  val (assert_valid, assert_val) = DelayNWithValid(s2_dataCorrupt.reduce(_ || _), s2_valid, 1000)
//  assert(!(assert_valid && assert_val))

  private val s1_corruptRefetch = VecInit((0 until PortNumber).map { i =>
    ValidHoldBypass(
      (s1_metaCorrupt(i) || s1_dataCorrupt(i)) && RegNext(s0_fire),
      s1_mshrHits(i), // clear re-fetch flag when re-fetched from mshr
      s1_flush
    )
  })

  /* *** Fetch when miss or corrupt *** */
  // do not fetch if is mmio
  private val s1_isMmio = s1_pmpMmio || Pbmt.isUncache(s1_itlbPbmt)

  private val s1_shouldFetch = VecInit((0 until PortNumber).map { i =>
    (!s1_hits(i) || s1_corruptRefetch(i)) &&
    (if (i == 0) true.B else s1_doubleline) &&
    s1_exception.isNone && !s1_isMmio
  })

  private val toMissArbiter = Module(new Arbiter(new MissReqBundle, PortNumber))

  // To avoid sending duplicate requests.
  private val s1_hasSend = VecInit((0 until PortNumber).map { i =>
    ValidHold(
      toMissArbiter.io.in(i).fire,
      s1_fire,
      s1_flush
    )
  })

  (0 until PortNumber).foreach { i =>
    toMissArbiter.io.in(i).valid         := s1_valid && s1_shouldFetch(i) && !s1_hasSend(i) && !s1_flush
    toMissArbiter.io.in(i).bits.blkPAddr := getBlkAddrFromPTag(s1_vAddr(i), s1_pTag)
    toMissArbiter.io.in(i).bits.vSetIdx  := s1_vSetIdx(i)
  }
  toMiss <> toMissArbiter.io.out

  XSPerfAccumulate("to_missUnit_stall", toMiss.valid && !toMiss.ready)

  private val s1_fetchFinish = !s1_shouldFetch.reduce(_ || _)

  // also raise af if l2 corrupt is detected
  private val s1_l2Exception = ExceptionType(hasAf = s1_l2Corrupt.reduce(_ || _))
  // NOTE: do NOT raise af if meta/data corrupt is detected, they are automatically recovered by re-fetching from L2

  // merge s2 exceptions, itlb/pmp has the highest priority, then l2
  private val s1_exceptionOut = s1_exception || s1_l2Exception

  /* *** send response to IFU *** */
  toIfu.valid                   := s1_fire
  toIfu.bits.doubleline         := s1_doubleline
  toIfu.bits.data               := s1_datas.asUInt
  toIfu.bits.maybeRvcMap        := s1_maybeRvcMap.asUInt
  toIfu.bits.isBackendException := s1_isBackendException
  toIfu.bits.vAddr              := s1_vAddr
  toIfu.bits.pAddr              := getPAddrFromPTag(s1_vAddr.head, s1_pTag)
  toIfu.bits.exception          := s1_exceptionOut
  toIfu.bits.pmpMmio            := s1_pmpMmio
  toIfu.bits.itlbPbmt           := s1_itlbPbmt
  // valid only for the first gpf
  toIfu.bits.gpAddr            := s1_gpAddr
  toIfu.bits.isForVSnonLeafPTE := s1_isForVSnonLeafPTE

  s1_flush := io.flush || io.flushFromBpu.shouldFlushByStage3(s1_ftqIdx, s1_valid)
  s1_ready := (s1_fetchFinish && !io.respStall) || !s1_valid
  s1_fire  := s1_valid && s1_fetchFinish && !io.respStall && !s1_flush

  /* *** perf *** */
  // when fired, tell ifu raw hit state of each cache line
  // NOTE: we cannot use s2_hits, it will be reset when refilled from L2
  private val s1_rawHits = RegEnable(s0_hits, 0.U.asTypeOf(s0_hits), s0_fire)
  io.perf.rawHits := s1_rawHits
  // tell ICache top when handling miss
  io.perf.pendingMiss := s1_valid && !s1_fetchFinish

  XSPerfAccumulate("missUnitStall", toMiss.valid && !toMiss.ready)
  XSPerfAccumulate("missBubble", s1_valid && !s1_fetchFinish)
  XSPerfAccumulate("wayLookupBubble", s0_valid && !fromWayLookup.valid)

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
  //   ICacheTouchDumpData(i).blkPAddr  := getBlkAddr(s1_pAddr(i))
  //   ICacheTouchDumpData(i).vSetIdx   := s1_vSetIdx(i)
  //   ICacheTouchDumpData(i).waymask   := OHToUInt(s1_tag_match_vec(i))
  //   ICacheTouchTable.log(
  //     data  = ICacheTouchDumpData(i),
  //     en    = io.replacerTouch.req(i).valid,
  //     site  = "req_" + i.toString,
  //     clock = clock,
  //     reset = reset
  //   )
  // }

  /* *** difftest refill check *** */
  if (env.EnableDifftest) {
    val discard = toIfu.bits.exception.hasException || toIfu.bits.pmpMmio || Pbmt.isUncache(toIfu.bits.itlbPbmt)
    val blkPaddrAll =
      VecInit(s1_vAddr.map(va => (getPAddrFromPTag(va, s1_pTag)(PAddrBits - 1, blockOffBits) << blockOffBits).asUInt))
    (0 until DataBanks).foreach { i =>
      val diffMainPipeOut = DifftestModule(new DiffRefillEvent, dontCare = true)
      diffMainPipeOut.coreid := io.hartId
      diffMainPipeOut.index  := (3 + i).U

      val bankSel = getBankSel(s1_offset, s1_blkEndOffset, s1_doubleline).map(_.asUInt).reduce(_ | _)
      val lineSel = getLineSel(s1_offset)

      diffMainPipeOut.valid := s1_fire && bankSel(i).asBool && !discard
      diffMainPipeOut.addr := Mux(
        lineSel(i),
        blkPaddrAll(1) + (i.U << log2Ceil(blockBytes / DataBanks)).asUInt,
        blkPaddrAll(0) + (i.U << log2Ceil(blockBytes / DataBanks)).asUInt
      )

      diffMainPipeOut.data  := s1_datas(i).asTypeOf(diffMainPipeOut.data)
      diffMainPipeOut.idtfr := DontCare
    }
  }
}
