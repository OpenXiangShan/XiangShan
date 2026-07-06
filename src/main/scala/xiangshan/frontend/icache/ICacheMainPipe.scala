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
import utility.ChiselDB
import utility.DataHoldBypass
import utility.ValidHold
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.L1CacheErrorInfo
import xiangshan.cache.mmu.Pbmt
import xiangshan.cache.mmu.TlbCmd
import xiangshan.cache.mmu.ValidHoldBypass
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchRequestBundle
import xiangshan.frontend.ftq.BpuFlushInfo

class ICacheMainPipe(implicit p: Parameters) extends ICacheModule
    with ICacheEccHelper
    with ICacheAddrHelper
    with ICacheDataHelper
    with ICacheMissUpdateHelper {

  class ICacheMainPipeIO(implicit p: Parameters) extends ICacheBundle {
    val hartId: UInt = Input(UInt(hartIdLen.W))

    /* *** internal interface *** */
    val dataRead:      DataReadBundle                         = new DataReadBundle
    val metaFlush:     MetaFlushBundle                        = new MetaFlushBundle
    val replacerTouch: ReplacerTouchBundle                    = new ReplacerTouchBundle
    val fromWayLookup: DecoupledIO[WayLookupToMainPipeBundle] = Flipped(DecoupledIO(new WayLookupToMainPipeBundle))
    val missReq:       DecoupledIO[MissReqBundle]             = DecoupledIO(new MissReqBundle)
    val missResp:      Valid[MissRespBundle]                  = Flipped(ValidIO(new MissRespBundle))
    val eccEnable:     Bool                                   = Input(Bool())

    /* *** outside interface *** */
    val flush:        Bool         = Input(Bool())
    val flushFromBpu: BpuFlushInfo = Input(new BpuFlushInfo)
    // Pmp
    val pmp: PmpCheckBundle = new PmpCheckBundle
    // Ifu
    val toIfu: MainPipeToIfuIO = new MainPipeToIfuIO
    // backend/Beu
    val errors: Vec[Valid[L1CacheErrorInfo]] = Output(Vec(PortNumber, ValidIO(new L1CacheErrorInfo)))

    val perf: MainPipePerfInfo = Output(new MainPipePerfInfo)
  }

  val io: ICacheMainPipeIO = IO(new ICacheMainPipeIO)

  /* *** Input/Output port *** */
  private val (toData, fromData) = (io.dataRead.req, io.dataRead.resp)
  private val toMetaFlush        = io.metaFlush.req
  private val (toMiss, fromMiss) = (io.missReq, io.missResp)
  private val (toPmp, fromPmp)   = (io.pmp.req, io.pmp.resp)
  private val eccEnable =
    if (ForceMetaEccFail || ForceDataEccFail) true.B else io.eccEnable

  // Statistics on the frequency distribution of FTQ fire interval
  private val cntFtqFireInterval      = RegInit(0.U(32.W))
  private val cntFtqFireIntervalStart = 1
  private val cntFtqFireIntervalEnd   = 300
  cntFtqFireInterval := Mux(io.fromWayLookup.fire, 1.U, cntFtqFireInterval + 1.U) // FIXME
  XSPerfHistogram(
    "ftq2icache_fire",
    cntFtqFireInterval,
    io.fromWayLookup.fire,
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
  private val s0_valid = io.fromWayLookup.valid
  private val s0_req   = io.fromWayLookup.bits.req

  private val s0_ftqIdx = s0_req(0).ftqIdx

  private val s0_wayLookupEntry = VecInit(io.fromWayLookup.bits.wayLookupInfo.map(_.entry))
  private val s0_exceptionInfo  = VecInit(io.fromWayLookup.bits.wayLookupInfo.map(_.exceptionEntry))
  private val s0_wayMask        = VecInit(s0_wayLookupEntry.map(_.waymask))

  s0_flush := io.flush || io.flushFromBpu.shouldFlushByStage3(s0_ftqIdx, s0_valid)

  io.fromWayLookup.ready := toData.ready && s1_ready && !s0_flush

  s0_fire := io.fromWayLookup.fire

  /**
    ******************************************************************************
    * data SRAM request
    ******************************************************************************
    */

  toData.valid := s0_valid && s1_ready && !s0_flush
  toData.bits.zipWithIndex.foreach { case (readReq, i) =>
    readReq.valid        := s0_req(i).valid
    readReq.bits.bankSel := s0_req(i).bankSel
    readReq.bits.waymask := s0_wayMask(i)
    readReq.bits.vSetIdx := s0_req(i).vSetIdx
  }

  /* ICache Stage 1
   * - Pmp check (to be removed)
   * - get Data Sram read responses (latched for pipeline stop)
   * - monitor missUnit response port
   * - Ecc check
   * - send request to Mshr if ICache miss
   * - response to Ifu
   */
  private val s1_valid          = ValidHold(s0_fire, s1_fire, s1_flush)
  private val s1_req            = RegEnable(s0_req, s0_fire)
  private val s1_wayLookupEntry = RegEnable(s0_wayLookupEntry, s0_fire)
  private val s1_exceptionInfo  = RegEnable(s0_exceptionInfo, s0_fire)

  private val s1_wayMask = VecInit(s1_wayLookupEntry.map(_.waymask))

  private val s1_pTag   = s1_wayLookupEntry(0).pTag
  private val s1_ftqIdx = s1_req(0).ftqIdx

  // the offset of the start pc within the cache line
  private val s1_offset = VecInit(s1_req.map(_.startVAddr(blockOffBits - 1, 0)))

  private val s1_lineSel = VecInit(s1_offset.map(getLineSel))

  /* *******************************************************************
   * Receive data from sram and mshr
   * ******************************************************************* */

  private val s1_sramHits = VecInit((0 until MaxFetchReqNum).map(i => VecInit(s1_wayMask(i).map(_.orR))))

  private val s1_sramDatas = fromData.map(_.datas)
  private val s1_sramCodes = fromData.map(_.codes)

  private val s1_sramRespValid = RegNext(s0_fire)
  private val s1_lineValid = VecInit((0 until MaxFetchReqNum).map { i =>
    VecInit(s1_req(i).valid, s1_req(i).valid && s1_req(i).isCrossLine)
  })
  private val s1_sramValid = VecInit((0 until MaxFetchReqNum).map { i =>
    VecInit(s1_lineValid(i).map(s1_sramRespValid && _))
  })
  private val s1_bankSramValid = VecInit((0 until MaxFetchReqNum).map { i =>
    getBankValid(s1_sramValid(i), s1_offset(i))
  })

  // mshr: valid when fromMiss.valid
  private val s1_mshrValid = VecInit((0 until MaxFetchReqNum).map { i =>
    checkMshrHitVec(
      fromMiss,
      s1_req(i).vSetIdx,
      s1_pTag,
      VecInit(s1_lineValid(i).map(s1_valid && _)),
      allowCorrupt = true // we also need to update registers when fromMiss.bits.corrupt
    )
  })

  dontTouch(s1_mshrValid)

  private val s1_bankMshrValid = VecInit((0 until MaxFetchReqNum).map { i =>
    getBankValid(s1_mshrValid(i), s1_offset(i))
  })
  private val s1_mshrDatas       = fromMiss.bits.data.asTypeOf(Vec(DataBanks, UInt(ICacheDataBits.W)))
  private val s1_mshrMaybeRvcMap = fromMiss.bits.maybeRvcMap.asTypeOf(Vec(DataBanks, UInt(MaxInstNumPerBank.W)))

  private val s1_hits = VecInit((0 until MaxFetchReqNum).map { reqIdx =>
    VecInit((0 until PortNumber).map { portIdx =>
      DataHoldBypass(
        s1_mshrValid(reqIdx)(portIdx) || s1_sramHits(reqIdx)(portIdx),
        s1_mshrValid(reqIdx)(portIdx) || s1_sramValid(reqIdx)(portIdx)
      )
    })
  })

  dontTouch(s1_hits)

  private val s1_data = VecInit((0 until MaxFetchReqNum).map { reqIdx =>
    VecInit((0 until DataBanks).map { bankIdx =>
      DataHoldBypass(
        Mux(
          s1_bankMshrValid(reqIdx)(bankIdx),
          s1_mshrDatas(bankIdx),
          s1_sramDatas(reqIdx)(bankIdx)
        ),
        s1_bankMshrValid(reqIdx)(bankIdx) || s1_bankSramValid(reqIdx)(bankIdx)
      )
    }).asUInt
  })

  private val s1_maybeRvcMap = VecInit((0 until MaxFetchReqNum).map { reqIdx =>
    val sramMaybeRvcMap =
      s1_wayLookupEntry(reqIdx).maybeRvcMap.asTypeOf(Vec(PortNumber, Vec(DataBanks, UInt(MaxInstNumPerBank.W))))

    VecInit((0 until DataBanks).map { bankIdx =>
      DataHoldBypass(
        Mux(
          s1_bankMshrValid(reqIdx)(bankIdx),
          s1_mshrMaybeRvcMap(bankIdx),
          Mux(
            s1_lineSel(reqIdx)(bankIdx),
            sramMaybeRvcMap(1)(bankIdx),
            sramMaybeRvcMap(0)(bankIdx)
          )
        ),
        s1_bankMshrValid(reqIdx)(bankIdx) || s1_bankSramValid(reqIdx)(bankIdx)
      )
    }).asUInt
  })

  private val s1_tlCorrupt = VecInit((0 until MaxFetchReqNum).map { reqIdx =>
    VecInit((0 until PortNumber).map { portIdx =>
      DataHoldBypass(
        s1_mshrValid(reqIdx)(portIdx) && fromMiss.bits.corrupt,
        s1_mshrValid(reqIdx)(portIdx) || s1_sramValid(reqIdx)(portIdx)
      )
    })
  })

  private val s1_tlDenied = VecInit((0 until MaxFetchReqNum).map { reqIdx =>
    VecInit((0 until PortNumber).map { portIdx =>
      DataHoldBypass(
        s1_mshrValid(reqIdx)(portIdx) && fromMiss.bits.denied,
        s1_mshrValid(reqIdx)(portIdx) || s1_sramValid(reqIdx)(portIdx)
      )
    })
  })

  /* *** Update replacer *** */
  (0 until PortNumber).foreach { i =>
    io.replacerTouch.req(i).bits.vSetIdx := s1_req(0).vSetIdx(i)                      // FIXME
    io.replacerTouch.req(i).bits.way     := OHToUInt(s1_wayLookupEntry(0).waymask(i)) // FIXME
  }
  io.replacerTouch.req(0).valid := RegNext(s0_fire) && s1_sramHits(0)(0)                          // FIXME
  io.replacerTouch.req(1).valid := RegNext(s0_fire) && s1_sramHits(0)(1) && s1_req(0).isCrossLine // FIXME

  /* *** PMP check (to be removed) *** */
  // if itlb has exception, pAddr can be invalid, therefore pmp check can be skipped do not do this now for timing
  toPmp.valid     := s1_valid // && !ExceptionType.hasException(s1_itlbException(i))
  toPmp.bits.addr := getPAddrFromPTag(s1_req(0).startVAddr, s1_pTag).toUInt
  toPmp.bits.size := 3.U
  toPmp.bits.cmd  := TlbCmd.exec
  private val s1_pmpException = ExceptionType.fromPmpResp(fromPmp)
  private val s1_pmpMmio      = fromPmp.mmio

  // merge s1 itlb/pmp exceptions, itlb has the highest priority, pmp next, note this `||` is overloaded
  private val s1_exception = s1_exceptionInfo(0).itlbException || s1_pmpException

  /* *** Ecc check *** */
  private val s1_metaCorrupt =
    checkMetaEcc(
      VecInit(s1_wayLookupEntry(0).maybeRvcMap.map(rvc => ICacheMetadata(s1_pTag, rvc))),
      s1_wayLookupEntry(0).metaCodes,
      s1_wayMask(0),
      eccEnable,
      s1_req(0).isCrossLine
    )

  // valid only when RegNext(s0_fire)
  // check data error
  private val s1_dataCorrupt = checkDataEcc(
    s1_sramDatas(0),
    s1_sramCodes(0),
    eccEnable,
    getBankSel(s1_offset(0), s1_valid, s1_req(0).isCrossLine),
    s1_bankSramValid(0),
    s1_sramHits(0)
  )

  /* NOTE: if !s1_doubleline:
   * - s1_meta_corrupt(1) should be false.B (as waymask(1) is invalid, and meta ecc is not checked)
   * - s1_data_corrupt(1) should also be false.B, as getLineSel() should not select line(1)
   * so we don't need to check s2_doubleline in the following io.errors and toMetaFlush ports
   * we add a sanity check to make sure the above assumption holds
   */
  when(s1_valid) {
    assert(
      !(!s1_req(0).isCrossLine && (s1_metaCorrupt(1) || s1_dataCorrupt(1))),
      "meta or data corrupt detected on line 1 but s2_doubleline is false.B"
    )
  }

  private val s1_vAddr = VecInit(s1_req.flatMap(req => Seq(req.startVAddr, req.nextLineVAddr)))
  dontTouch(s1_vAddr)

  // send errors to top
  // TODO: support RERI spec standard interface
  (0 until PortNumber).foreach { i =>
    io.errors(i).valid              := (s1_metaCorrupt(i) || s1_dataCorrupt(i)) && RegNext(s0_fire)
    io.errors(i).bits.report_to_beu := (s1_metaCorrupt(i) || s1_dataCorrupt(i)) && RegNext(s0_fire)
    io.errors(i).bits.paddr         := getPAddrFromPTag(s1_vAddr(i), s1_pTag).toUInt // FIXME
    io.errors(i).bits.source        := DontCare
    io.errors(i).bits.source.tag    := s1_metaCorrupt(i)
    io.errors(i).bits.source.data   := s1_dataCorrupt(i)
    io.errors(i).bits.source.l2     := false.B
    io.errors(i).bits.opType        := DontCare
    io.errors(i).bits.opType.fetch  := true.B
  }

  // If EnableCorruptRefetch, flush metaArray to prepare for re-fetch
  toMetaFlush.zipWithIndex.foreach { case (flush, i) =>
    if (EnableCorruptRefetch) {
      flush.valid        := (s1_metaCorrupt(i) || s1_dataCorrupt(i)) && RegNext(s0_fire)
      flush.bits.vSetIdx := s1_req(0).vSetIdx(i) // FIXME
      // if is meta corrupt, clear all way (since waymask may be unreliable)
      // if is data corrupt, only clear the way that has error
      flush.bits.waymask := Mux(s1_metaCorrupt(i), Fill(nWays, true.B), s1_wayMask(0)(i))
    } else {
      flush.valid := false.B
      flush.bits  := DontCare
    }
  }

  // PERF: count the number of data parity errors
  XSPerfAccumulate("data_corrupt_0", s1_dataCorrupt(0) && RegNext(s0_fire))
  XSPerfAccumulate("data_corrupt_1", s1_dataCorrupt(1) && RegNext(s0_fire))
  XSPerfAccumulate("meta_corrupt_0", s1_metaCorrupt(0) && RegNext(s0_fire))
  XSPerfAccumulate("meta_corrupt_1", s1_metaCorrupt(1) && RegNext(s0_fire))

  // If enable CorruptRefetch, set s1_shouldFetch flag according to ecc check result, otherwise always false
  private val s1_corruptRefetch = VecInit((0 until PortNumber).map { i =>
    if (EnableCorruptRefetch)
      ValidHoldBypass(
        (s1_metaCorrupt(i) || s1_dataCorrupt(i)) && RegNext(s0_fire),
        s1_mshrValid(0)(i), // clear re-fetch flag when re-fetched from mshr
        s1_flush
      )
    else
      false.B
  })

  /* *** Fetch when miss or corrupt *** */
  // do not fetch if is mmio
  private val s1_isMmio = s1_pmpMmio || Pbmt.isUncache(s1_wayLookupEntry(0).itlbPbmt)

  private val s1_shouldFetch = VecInit((0 until MaxFetchReqNum).flatMap { reqIdx =>
    (0 until PortNumber).map { portIdx =>
      val needCorruptRefetch = if (reqIdx == 0) s1_corruptRefetch(portIdx) else false.B
      (!s1_hits(reqIdx)(portIdx) || needCorruptRefetch) &&
      s1_lineValid(reqIdx)(portIdx) &&
      s1_exception.isNone && !s1_isMmio
    }
  })

  dontTouch(s1_shouldFetch)

  private val toMissArbiter = Module(new Arbiter(new MissReqBundle, MaxFetchLineNum))

  // To avoid sending duplicate requests.
  private val s1_hasSend = VecInit((0 until MaxFetchLineNum).map { i =>
    ValidHold(
      toMissArbiter.io.in(i).fire,
      s1_fire,
      s1_flush
    )
  })

  private val s1_vSetIdx = VecInit(s1_req.flatMap(_.vSetIdx))

  (0 until MaxFetchLineNum).foreach { i =>
    toMissArbiter.io.in(i).valid         := s1_valid && s1_shouldFetch(i) && !s1_hasSend(i) && !s1_flush
    toMissArbiter.io.in(i).bits.blkPAddr := getBlkAddrFromPTag(s1_vAddr(i), s1_pTag)
    toMissArbiter.io.in(i).bits.vSetIdx  := s1_vSetIdx(i)
  }
  toMiss <> toMissArbiter.io.out

  XSPerfAccumulate("to_missUnit_stall", toMiss.valid && !toMiss.ready)

  private val s1_fetchFinish = !s1_shouldFetch.reduce(_ || _)
  dontTouch(s1_fetchFinish)

  private val s1_portValid = VecInit(s1_lineValid.flatten)

  // also raise af if l2 corrupt is detected
  private val s1_tlException =
    (s1_tlCorrupt.flatten zip s1_tlDenied.flatten).zipWithIndex.map { case ((corrupt, denied), i) =>
      val portValid   = s1_portValid(i)
      val realCorrupt = corrupt && portValid
      val realDenied  = denied && portValid
      val canAssert   = s1_valid && portValid
      ExceptionType.fromTileLink(realCorrupt, realDenied, canAssert)
    }.reduce(_ || _)

  // If EnableCorruptRefetch, no need to raise exception as it's been auto-recovered by re-fetching from L2
  // otherwise, raise Hardware Error Exception
  private val s1_eccException =
    if (EnableCorruptRefetch)
      ExceptionType.None
    else
      ExceptionType.fromEcc(s1_metaCorrupt.reduce(_ || _) || s1_dataCorrupt.reduce(_ || _), s1_valid)

  // merge all exceptions, itlb/pmp has the highest priority, then l2/ecc
  private val s1_exceptionOut = s1_exception || s1_tlException || s1_eccException

  io.toIfu.req.valid := s1_valid && s1_fetchFinish && !s1_flush
  io.toIfu.req.bits.zipWithIndex.foreach { case (req, i) =>
    req.valid            := s1_req(i).valid
    req.startVAddr       := s1_req(i).startVAddr
    req.ftqIdx           := s1_req(i).ftqIdx
    req.takenCfiOffset   := s1_req(i).takenCfiOffset
    req.range            := Fill(FetchBlockInstNum, 1.U(1.W)) >> (~s1_req(i).takenCfiOffset.bits).asUInt
    req.size             := s1_req(i).takenCfiOffset.bits +& 1.U
    req.data             := s1_data(i)
    req.maybeRvcMap      := s1_maybeRvcMap(i)
    req.perf_isCrossLine := s1_req(i).isCrossLine

    req.icacheMeta.exception          := s1_exceptionOut
    req.icacheMeta.pmpMmio            := s1_pmpMmio
    req.icacheMeta.isBackendException := s1_req(i).hasBackendException
    req.icacheMeta.isForVSnonLeafPTE  := s1_exceptionInfo(i).isForVSnonLeafPTE
    req.icacheMeta.itlbPbmt           := s1_wayLookupEntry(i).itlbPbmt
    req.icacheMeta.pAddr              := getPAddrFromPTag(s1_vAddr(2 * i), s1_pTag)
    req.icacheMeta.gpAddr             := s1_exceptionInfo(i).gpAddr
  }
  s1_flush := io.flush || io.flushFromBpu.shouldFlushByStage3(s1_ftqIdx, s1_valid)
  s1_ready := (s1_fetchFinish && io.toIfu.req.ready) || !s1_valid
  s1_fire  := s1_valid && s1_fetchFinish && io.toIfu.req.ready && !s1_flush

  /* *** perf *** */
  // when fired, tell ifu raw hit state of each cache line
  // NOTE: we cannot use s2_hits, it will be reset when refilled from L2
  private val s1_rawHits = s1_sramHits(0) // FIXME
  io.perf.rawHits := s1_rawHits
  // tell ICache top when handling miss
  io.perf.pendingMiss := s1_valid && !s1_fetchFinish

  XSPerfAccumulate("stallCycles_fetch_icacheMain", !s1_fire)
  XSPerfAccumulate("stallCycles_fetch_icacheMain_prefetch", s0_valid && !io.fromWayLookup.valid)
  XSPerfAccumulate("stallCycles_fetch_icacheMain_dataArray", s0_valid && !toData.ready)
  XSPerfAccumulate("stallCycles_fetch_icacheMain_missUnit", s1_valid && !s1_fetchFinish)

  private class AccessTrace extends Bundle {
    val vAddr:      UInt      = UInt(VAddrBits.W)
    val pAddr:      UInt      = UInt(PAddrBits.W)
    val wayMask:    Vec[UInt] = Vec(PortNumber, UInt(nWays.W))
    val crossLine:  Bool      = Bool()
    val waitRefill: UInt      = UInt(XLEN.W)
    val rawHits:    Vec[Bool] = Vec(PortNumber, Bool())

    val exception: ExceptionType = new ExceptionType
    val pmpMmio:   Bool          = Bool()
    val itlbPbmt:  UInt          = UInt(Pbmt.width.W)
  }

  private val perf_waitRefill = RegInit(0.U(XLEN.W))
  when(s1_valid && !s1_fetchFinish) {
    perf_waitRefill := perf_waitRefill + 1.U
  }.elsewhen(s1_fetchFinish || s1_flush) {
    perf_waitRefill := 0.U
  }

  private val accessTrace = Wire(new AccessTrace)
  accessTrace.vAddr      := s1_vAddr(0).toUInt
  accessTrace.pAddr      := getPAddrFromPTag(s1_vAddr(0), s1_pTag).toUInt
  accessTrace.wayMask    := s1_wayMask(0)
  accessTrace.crossLine  := s1_req(0).isCrossLine
  accessTrace.waitRefill := perf_waitRefill
  accessTrace.exception  := s1_exceptionOut
  accessTrace.pmpMmio    := s1_pmpMmio
  accessTrace.itlbPbmt   := s1_wayLookupEntry(0).itlbPbmt
  accessTrace.rawHits    := s1_rawHits

  private val accessTable = ChiselDB.createTable("ICacheAccessTable", new AccessTrace, EnableTrace)
  accessTable.log(
    data = accessTrace,
    en = s1_fire,
    clock = clock,
    reset = reset
  )

  /* *** difftest refill check *** */
//  if (env.EnableDifftest) {
//    val bankSel = getBankSel(s1_offset(0), s1_blkEndOffset, s1_fetchReq(0).isCrossLine)
//
//    // do difftest for each fetched cache line
//    s1_vAddr(0).zipWithIndex.foreach { case (va, i) =>
//      val difftest = DifftestModule(new DiffRefillEvent, dontCare = true)
//      difftest.coreid := io.hartId
//      difftest.index  := (3 + i).U // magic number 3/4: ICache MainPipe refill test
//
//      difftest.valid := false.B
////      difftest.valid := s1_fire && !(
////        toIfu.bits.exception.hasException ||
////          toIfu.bits.pmpMmio ||
////          Pbmt.isUncache(toIfu.bits.itlbPbmt)
////      )
//      difftest.addr := Cat(getBlkAddrFromPTag(va, s1_pTag(0)), 0.U(blockOffBits.W))
//      difftest.data := s1_datas.asTypeOf(difftest.data)
//      // NOTE: each mask bit controls (512bit / difftest.mask.getWidth) (currently 64bit) comparison
//      // this only works for DataBanks <= difftest.mask.getWidth (and isPow2)
//      difftest.mask := VecInit((0 until difftest.mask.getWidth).map { j =>
//        // the i-th mask locates in (i / (difftest.mask.getWidth / DataBanks)) bank
//        bankSel(i)(j / (difftest.mask.getWidth / DataBanks))
//      }).asUInt
//    }
//  }
}
