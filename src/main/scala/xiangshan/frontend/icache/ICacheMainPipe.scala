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
    with ICacheDataSelHelper {

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
    val resp:      Valid[ICacheRespBundle] = ValidIO(new ICacheRespBundle)
    val respStall: Bool                    = Input(Bool())
    // backend/Beu
    val errors: Vec[Valid[L1CacheErrorInfo]] = Output(Vec(PortNumber, ValidIO(new L1CacheErrorInfo)))

    /* *** Perf *** */
    val perf:    ICachePerfInfo    = Output(new ICachePerfInfo)
    val topdown: ICacheTopdownInfo = Output(new ICacheTopdownInfo)
  }

  val io: ICacheMainPipeIO = IO(new ICacheMainPipeIO)

  /* *** Input/Output port *** */
  private val (fromFtq, toIfu)   = (io.req, io.resp)
  private val (toData, fromData) = (io.dataRead.req, io.dataRead.resp)
  private val toMetaFlush        = io.metaFlush.req
  private val (toMSHR, fromMSHR) = (io.missReq, io.missResp)
  private val (toPMP, fromPMP)   = (io.pmp.map(_.req), io.pmp.map(_.resp))
  private val fromWayLookup      = io.wayLookupRead
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
  private val fromFtqReq       = fromFtq.bits.req
  private val s0_valid         = fromFtq.valid
  private val s0_req_valid_all = (0 until partWayNum + 1).map(i => fromFtq.bits.readValid(i))
  private val s0_req_vaddr_all =
    (0 until partWayNum + 1).map(i => VecInit(Seq(fromFtqReq(i).startAddr, fromFtqReq(i).nextlineStart)))
  private val s0_req_vSetIdx_all = (0 until partWayNum + 1).map(i => VecInit(s0_req_vaddr_all(i).map(get_idx)))
  private val s0_req_offset_all = (0 until partWayNum + 1).map(i => s0_req_vaddr_all(i)(0)(log2Ceil(blockBytes) - 1, 0))
  private val s0_doubleline_all =
    (0 until partWayNum + 1).map(i => fromFtq.bits.readValid(i) && fromFtqReq(i).crossCacheline)

  private val s0_req_vaddr   = s0_req_vaddr_all.last
  private val s0_req_vSetIdx = s0_req_vSetIdx_all.last
  private val s0_doubleline  = s0_doubleline_all.last

  private val s0_isBackendException = fromFtq.bits.isBackendException

  /**
    ******************************************************************************
    * get waymask and tlb info from wayLookup
    ******************************************************************************
    */
  fromWayLookup.ready := s0_fire
  private val s0_waymasks              = VecInit(fromWayLookup.bits.waymask.map(_.asTypeOf(Vec(nWays, Bool()))))
  private val s0_req_pTags             = fromWayLookup.bits.pTag
  private val s0_req_gpaddr            = fromWayLookup.bits.gpAddr
  private val s0_req_isForVSnonLeafPTE = fromWayLookup.bits.isForVSnonLeafPTE
  private val s0_itlb_exception        = fromWayLookup.bits.itlbException
  private val s0_itlb_pbmt             = fromWayLookup.bits.itlbPbmt
  private val s0_meta_codes            = fromWayLookup.bits.metaCodes
  private val s0_hits                  = VecInit(fromWayLookup.bits.waymask.map(_.orR))

  when(s0_fire) {
    assert(
      (0 until PortNumber).map(i => s0_req_vSetIdx(i) === fromWayLookup.bits.vSetIdx(i)).reduce(_ && _),
      "vSetIdx from ftq and wayLookup mismatch! vAddr=0x%x ftq: vSet0=0x%x vSet1=0x%x wayLookup: vSet0=0x%x vSet1=0x%x",
      s0_req_vaddr(0),
      s0_req_vSetIdx(0),
      s0_req_vSetIdx(1),
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
    toData(i).valid             := s0_req_valid_all(i)
    toData(i).bits.isDoubleLine := s0_doubleline_all(i)
    toData(i).bits.vSetIdx      := s0_req_vSetIdx_all(i)
    toData(i).bits.blkOffset    := s0_req_offset_all(i)
    toData(i).bits.waymask      := s0_waymasks
  }

  private val s0_can_go = toData.last.ready && fromWayLookup.valid && s1_ready
  s0_flush := io.flush
  s0_fire  := s0_valid && s0_can_go && !s0_flush

  fromFtq.ready := s0_can_go

  /**
    ******************************************************************************
    * ICache Stage 1
    * - PMP check
    * - get Data SRAM read responses (latched for pipeline stop)
    * - monitor missUint response port
    ******************************************************************************
    */
  private val s1_valid = ValidHold(s0_fire, s1_fire, s1_flush)

  private val s1_req_vaddr  = RegEnable(s0_req_vaddr, 0.U.asTypeOf(s0_req_vaddr), s0_fire)
  private val s1_req_pTags  = RegEnable(s0_req_pTags, 0.U.asTypeOf(s0_req_pTags), s0_fire)
  private val s1_req_gpaddr = RegEnable(s0_req_gpaddr, 0.U.asTypeOf(s0_req_gpaddr), s0_fire)
  private val s1_req_isForVSnonLeafPTE =
    RegEnable(s0_req_isForVSnonLeafPTE, 0.U.asTypeOf(s0_req_isForVSnonLeafPTE), s0_fire)
  private val s1_doubleline         = RegEnable(s0_doubleline, 0.U.asTypeOf(s0_doubleline), s0_fire)
  private val s1_SRAMhits           = RegEnable(s0_hits, 0.U.asTypeOf(s0_hits), s0_fire)
  private val s1_itlb_exception     = RegEnable(s0_itlb_exception, 0.U.asTypeOf(s0_itlb_exception), s0_fire)
  private val s1_isBackendException = RegEnable(s0_isBackendException, false.B, s0_fire)
  private val s1_itlb_pbmt          = RegEnable(s0_itlb_pbmt, 0.U.asTypeOf(s0_itlb_pbmt), s0_fire)
  private val s1_waymasks           = RegEnable(s0_waymasks, 0.U.asTypeOf(s0_waymasks), s0_fire)
  private val s1_meta_codes         = RegEnable(s0_meta_codes, 0.U.asTypeOf(s0_meta_codes), s0_fire)

  private val s1_req_vSetIdx = s1_req_vaddr.map(get_idx)
  private val s1_req_paddr   = getPAddrFromPTag(s1_req_vaddr, s1_req_pTags)
  private val s1_req_offset  = s1_req_vaddr(0)(log2Ceil(blockBytes) - 1, 0)

  // do metaArray ECC check
  private val s1_meta_corrupt = VecInit((s1_req_pTags zip s1_meta_codes zip s1_waymasks).map {
    case ((meta, code), waymask) =>
      val hit_num = PopCount(waymask)
      // NOTE: if not hit, encodeMetaECC(meta) =/= code can also be true, but we don't care about it
      (encodeMetaEcc(meta) =/= code && hit_num === 1.U) || // hit one way, but parity code does not match, ECC failure
      hit_num > 1.U                                        // hit multi-way, must be an ECC failure
  })
  // force clear meta_corrupt when parity check is disabled
  when(!eccEnable) {
    s1_meta_corrupt := VecInit(Seq.fill(PortNumber)(false.B))
  }

  /**
    ******************************************************************************
    * update replacement status register
    ******************************************************************************
    */
  (0 until PortNumber).foreach { i =>
    io.replacerTouch.req(i).bits.vSetIdx := s1_req_vSetIdx(i)
    io.replacerTouch.req(i).bits.way     := OHToUInt(s1_waymasks(i))
  }
  io.replacerTouch.req(0).valid := RegNext(s0_fire) && s1_SRAMhits(0)
  io.replacerTouch.req(1).valid := RegNext(s0_fire) && s1_SRAMhits(1) && s1_doubleline

  /**
    ******************************************************************************
    * PMP check
    ******************************************************************************
    */
  toPMP.zipWithIndex.foreach { case (p, i) =>
    // if itlb has exception, pAddr can be invalid, therefore pmp check can be skipped do not do this now for timing
    p.valid     := s1_valid // && !ExceptionType.hasException(s1_itlb_exception(i))
    p.bits.addr := s1_req_paddr(i)
    p.bits.size := 3.U
    p.bits.cmd  := TlbCmd.exec
  }
  private val s1_pmp_exception = VecInit(fromPMP.map(ExceptionType.fromPMPResp))
  private val s1_pmp_mmio      = VecInit(fromPMP.map(_.mmio))

  // merge s1 itlb/pmp exceptions, itlb has the highest priority, pmp next
  private val s1_exception_out = ExceptionType.merge(
    s1_itlb_exception,
    s1_pmp_exception
  )

  /**
    ******************************************************************************
    * select data from MSHR, SRAM
    ******************************************************************************
    */
  private val s1_MSHR_match = VecInit((0 until PortNumber).map { i =>
    (s1_req_vSetIdx(i) === fromMSHR.bits.vSetIdx) &&
    (s1_req_pTags(i) === getPTagFromBlk(fromMSHR.bits.blkPAddr)) &&
    fromMSHR.valid && !fromMSHR.bits.corrupt
  })
  private val s1_MSHR_hits  = Seq(s1_valid && s1_MSHR_match(0), s1_valid && (s1_MSHR_match(1) && s1_doubleline))
  private val s1_MSHR_datas = fromMSHR.bits.data.asTypeOf(Vec(ICacheDataBanks, UInt((blockBits / ICacheDataBanks).W)))

  private val s1_hits = (0 until PortNumber).map { i =>
    ValidHoldBypass(s1_MSHR_hits(i) || (RegNext(s0_fire) && s1_SRAMhits(i)), s1_fire || s1_flush)
  }

  private val s1_bankIdxLow = (s1_req_offset >> log2Ceil(blockBytes / ICacheDataBanks)).asUInt
  private val s1_bankMSHRHit = VecInit((0 until ICacheDataBanks).map { i =>
    (i.U >= s1_bankIdxLow) && s1_MSHR_hits(0) ||
    (i.U < s1_bankIdxLow) && s1_MSHR_hits(1)
  })
  private val s1_datas = VecInit((0 until ICacheDataBanks).map { i =>
    DataHoldBypass(Mux(s1_bankMSHRHit(i), s1_MSHR_datas(i), fromData.datas(i)), s1_bankMSHRHit(i) || RegNext(s0_fire))
  })
  private val s1_data_is_from_MSHR = VecInit((0 until ICacheDataBanks).map { i =>
    DataHoldBypass(s1_bankMSHRHit(i), s1_bankMSHRHit(i) || RegNext(s0_fire))
  })
  private val s1_codes = DataHoldBypass(fromData.codes, RegNext(s0_fire))

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

  private val s2_req_vaddr  = RegEnable(s1_req_vaddr, 0.U.asTypeOf(s1_req_vaddr), s1_fire)
  private val s2_req_pTags  = RegEnable(s1_req_pTags, 0.U.asTypeOf(s1_req_pTags), s1_fire)
  private val s2_req_gpaddr = RegEnable(s1_req_gpaddr, 0.U.asTypeOf(s1_req_gpaddr), s1_fire)
  private val s2_req_isForVSnonLeafPTE =
    RegEnable(s1_req_isForVSnonLeafPTE, 0.U.asTypeOf(s1_req_isForVSnonLeafPTE), s1_fire)
  private val s2_doubleline         = RegEnable(s1_doubleline, 0.U.asTypeOf(s1_doubleline), s1_fire)
  private val s2_exception          = RegEnable(s1_exception_out, 0.U.asTypeOf(s1_exception_out), s1_fire)
  private val s2_isBackendException = RegEnable(s1_isBackendException, false.B, s1_fire)
  private val s2_pmp_mmio           = RegEnable(s1_pmp_mmio, 0.U.asTypeOf(s1_pmp_mmio), s1_fire)
  private val s2_itlb_pbmt          = RegEnable(s1_itlb_pbmt, 0.U.asTypeOf(s1_itlb_pbmt), s1_fire)
  private val s2_waymasks           = RegEnable(s1_waymasks, 0.U.asTypeOf(s1_waymasks), s1_fire)

  private val s2_req_vSetIdx = s2_req_vaddr.map(get_idx)
  private val s2_req_offset  = s2_req_vaddr(0)(log2Ceil(blockBytes) - 1, 0)
  private val s2_req_paddr   = getPAddrFromPTag(s2_req_vaddr, s2_req_pTags)

  private val s2_SRAMhits          = RegEnable(s1_SRAMhits, 0.U.asTypeOf(s1_SRAMhits), s1_fire)
  private val s2_codes             = RegEnable(s1_codes, 0.U.asTypeOf(s1_codes), s1_fire)
  private val s2_hits              = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  private val s2_datas             = RegInit(VecInit(Seq.fill(ICacheDataBanks)(0.U((blockBits / ICacheDataBanks).W))))
  private val s2_data_is_from_MSHR = RegInit(VecInit(Seq.fill(ICacheDataBanks)(false.B)))

  /**
    ******************************************************************************
    * ECC check
    ******************************************************************************
    */
  // check data error
  private val s2_bankSel      = getBankSel(s2_req_offset, s2_valid)
  private val s2_bank_corrupt = (0 until ICacheDataBanks).map(i => encodeDataEcc(s2_datas(i)) =/= s2_codes(i))
  // if data is from MSHR, we don't need to check ECC
  private val s2_data_corrupt = VecInit((0 until PortNumber).map { port =>
    (0 until ICacheDataBanks).map { bank =>
      s2_bank_corrupt(bank) && s2_bankSel(port)(bank).asBool && !s2_data_is_from_MSHR(bank)
    }.reduce(_ || _) && s2_SRAMhits(port)
  })
  // force clear data_corrupt when parity check is disabled
  when(!eccEnable) {
    s2_data_corrupt := VecInit(Seq.fill(PortNumber)(false.B))
  }
  // meta error is checked in s1 stage
  private val s2_meta_corrupt = RegEnable(s1_meta_corrupt, 0.U.asTypeOf(s1_meta_corrupt), s1_fire)
  // send errors to top
  // TODO: support RERI spec standard interface
  (0 until PortNumber).foreach { i =>
    io.errors(i).valid              := (s2_meta_corrupt(i) || s2_data_corrupt(i)) && RegNext(s1_fire)
    io.errors(i).bits.report_to_beu := (s2_meta_corrupt(i) || s2_data_corrupt(i)) && RegNext(s1_fire)
    io.errors(i).bits.paddr         := s2_req_paddr(i)
    io.errors(i).bits.source        := DontCare
    io.errors(i).bits.source.tag    := s2_meta_corrupt(i)
    io.errors(i).bits.source.data   := s2_data_corrupt(i)
    io.errors(i).bits.source.l2     := false.B
    io.errors(i).bits.opType        := DontCare
    io.errors(i).bits.opType.fetch  := true.B
  }
  // flush metaArray to prepare for re-fetch
  (0 until PortNumber).foreach { i =>
    toMetaFlush(i).valid        := (s2_meta_corrupt(i) || s2_data_corrupt(i)) && RegNext(s1_fire)
    toMetaFlush(i).bits.vSetIdx := s2_req_vSetIdx(i)
    // if is meta corrupt, clear all way (since waymask may be unreliable)
    // if is data corrupt, only clear the way that has error
    toMetaFlush(i).bits.waymask := Mux(s2_meta_corrupt(i), Fill(nWays, true.B), s2_waymasks(i).asUInt)
  }
  // PERF: count the number of data parity errors
  XSPerfAccumulate("data_corrupt_0", s2_data_corrupt(0) && RegNext(s1_fire))
  XSPerfAccumulate("data_corrupt_1", s2_data_corrupt(1) && RegNext(s1_fire))
  XSPerfAccumulate("meta_corrupt_0", s2_meta_corrupt(0) && RegNext(s1_fire))
  XSPerfAccumulate("meta_corrupt_1", s2_meta_corrupt(1) && RegNext(s1_fire))
  // TEST: stop simulation if parity error is detected, and dump wave
//  val (assert_valid, assert_val) = DelayNWithValid(s2_meta_corrupt.reduce(_ || _), s2_valid, 1000)
//  assert(!(assert_valid && assert_val))
//  val (assert_valid, assert_val) = DelayNWithValid(s2_data_corrupt.reduce(_ || _), s2_valid, 1000)
//  assert(!(assert_valid && assert_val))

  /**
    ******************************************************************************
    * monitor missUint response port
    ******************************************************************************
    */
  private val s2_MSHR_match = VecInit((0 until PortNumber).map { i =>
    (s2_req_vSetIdx(i) === fromMSHR.bits.vSetIdx) &&
    (s2_req_pTags(i) === getPTagFromBlk(fromMSHR.bits.blkPAddr)) &&
    fromMSHR.valid // we don't care about whether it's corrupt here
  })
  private val s2_MSHR_hits  = Seq(s2_valid && s2_MSHR_match(0), s2_valid && s2_MSHR_match(1) && s2_doubleline)
  private val s2_MSHR_datas = fromMSHR.bits.data.asTypeOf(Vec(ICacheDataBanks, UInt((blockBits / ICacheDataBanks).W)))

  private val s2_bankIdxLow = (s2_req_offset >> log2Ceil(blockBytes / ICacheDataBanks)).asUInt
  private val s2_bankMSHRHit = VecInit((0 until ICacheDataBanks).map { i =>
    ((i.U >= s2_bankIdxLow) && s2_MSHR_hits(0)) || ((i.U < s2_bankIdxLow) && s2_MSHR_hits(1))
  })

  (0 until ICacheDataBanks).foreach { i =>
    when(s1_fire) {
      s2_datas             := s1_datas
      s2_data_is_from_MSHR := s1_data_is_from_MSHR
    }.elsewhen(s2_bankMSHRHit(i)) {
      s2_datas(i) := s2_MSHR_datas(i)
      // also update s2_data_is_from_MSHR when re-fetched, to clear s2_data_corrupt flag and let s2_fire
      s2_data_is_from_MSHR(i) := true.B
    }
  }

  (0 until PortNumber).foreach { i =>
    when(s1_fire) {
      s2_hits := s1_hits
    }.elsewhen(s2_MSHR_hits(i)) {
      // update s2_hits even if it's corrupt, to let s2_fire
      s2_hits(i) := true.B
      // also clear s2_meta_corrupt flag when re-fetched, to let s2_fire
      s2_meta_corrupt(i) := false.B
    }
  }

  private val s2_l2_corrupt = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach { i =>
    when(s1_fire) {
      s2_l2_corrupt(i) := false.B
    }.elsewhen(s2_MSHR_hits(i)) {
      s2_l2_corrupt(i) := fromMSHR.bits.corrupt
    }
  }

  /**
    ******************************************************************************
    * send request to MSHR if ICache miss / ECC corrupt
    ******************************************************************************
    */

  // merge pmp mmio and itlb pbmt
  private val s2_mmio = VecInit((s2_pmp_mmio zip s2_itlb_pbmt).map { case (mmio, pbmt) =>
    mmio || Pbmt.isUncache(pbmt)
  })

  // try re-fetch data from L2 cache if ECC error is detected, unless it's from MSHR
  private val s2_corrupt_refetch = (s2_meta_corrupt zip s2_data_corrupt).map {
    case (meta, data) => meta || data
  }

  /* s2_exception includes itlb pf/gpf/af, pmp af and meta corruption (af), neither of which should be fetched
   * mmio should not be fetched, it will be fetched by IFU mmio fsm
   * also, if previous has exception, latter port should also not be fetched
   */
  private val s2_should_fetch = VecInit((0 until PortNumber).map { i =>
    (!s2_hits(i) || s2_corrupt_refetch(i)) &&
    (if (i == 0) true.B else s2_doubleline) &&
    !ExceptionType.hasException(s2_exception.take(i + 1)) &&
    s2_mmio.take(i + 1).map(!_).reduce(_ && _)
  })

  private val toMSHRArbiter = Module(new Arbiter(new MissReqBundle, PortNumber))

  // To avoid sending duplicate requests.
  private val s2_has_send = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach { i =>
    when(s1_fire) {
      s2_has_send(i) := false.B
    }.elsewhen(toMSHRArbiter.io.in(i).fire) {
      s2_has_send(i) := true.B
    }
  }

  (0 until PortNumber).foreach { i =>
    toMSHRArbiter.io.in(i).valid         := s2_valid && s2_should_fetch(i) && !s2_has_send(i) && !s2_flush
    toMSHRArbiter.io.in(i).bits.blkPAddr := getBlkAddr(s2_req_paddr(i))
    toMSHRArbiter.io.in(i).bits.vSetIdx  := s2_req_vSetIdx(i)
  }
  toMSHR <> toMSHRArbiter.io.out

  XSPerfAccumulate("to_missUnit_stall", toMSHR.valid && !toMSHR.ready)

  private val s2_fetch_finish = !s2_should_fetch.reduce(_ || _)

  // also raise af if l2 corrupt is detected
  private val s2_l2_exception = VecInit(s2_l2_corrupt.map(ExceptionType.fromTilelink))
  // NOTE: do NOT raise af if meta/data corrupt is detected, they are automatically recovered by re-fetching from L2

  // merge s2 exceptions, itlb has the highest priority, then l2
  private val s2_exception_out = ExceptionType.merge(
    s2_exception, // includes itlb/pmp exception
    s2_l2_exception
  )

  /**
    ******************************************************************************
    * response to IFU
    ******************************************************************************
    */
  toIfu.valid                   := s2_fire
  toIfu.bits.doubleline         := s2_doubleline
  toIfu.bits.data               := s2_datas.asTypeOf(UInt(blockBits.W))
  toIfu.bits.isBackendException := s2_isBackendException
  (0 until PortNumber).foreach { i =>
    toIfu.bits.vAddr(i) := s2_req_vaddr(i)
    toIfu.bits.pAddr(i) := s2_req_paddr(i)
    val needThisLine = if (i == 0) true.B else s2_doubleline
    toIfu.bits.exception(i) := Mux(needThisLine, s2_exception_out(i), ExceptionType.none)
    toIfu.bits.pmpMmio(i)   := Mux(needThisLine, s2_pmp_mmio(i), false.B)
    toIfu.bits.itlbPbmt(i)  := Mux(needThisLine, s2_itlb_pbmt(i), Pbmt.pma)
  }
  // valid only for the first gpf
  toIfu.bits.gpAddr            := s2_req_gpaddr
  toIfu.bits.isForVSnonLeafPTE := s2_req_isForVSnonLeafPTE

  s2_flush := io.flush
  s2_ready := (s2_fetch_finish && !io.respStall) || !s2_valid
  s2_fire  := s2_valid && s2_fetch_finish && !io.respStall && !s2_flush

  /**
    ******************************************************************************
    * report Tilelink corrupt error
    ******************************************************************************
    */
  (0 until PortNumber).foreach { i =>
    when(RegNext(s2_fire && s2_l2_corrupt(i))) {
      io.errors(i).valid              := true.B
      io.errors(i).bits.report_to_beu := false.B // l2 should have report that to bus error unit, no need to do it again
      io.errors(i).bits.paddr         := RegNext(s2_req_paddr(i))
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
  XSPerfAccumulate("icache_bubble_s2_miss", s2_valid && !s2_fetch_finish)
  XSPerfAccumulate("icache_bubble_s0_wayLookup", s0_valid && !fromWayLookup.ready)

  io.topdown.icacheMiss := !s2_fetch_finish
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
  //   ICacheTouchDumpData(i).blkPAddr  := getBlkAddr(s2_req_paddr(i))
  //   ICacheTouchDumpData(i).vSetIdx   := s2_req_vSetIdx(i)
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
      ExceptionType.hasException(toIfu.bits.exception(i)) ||
      toIfu.bits.pmpMmio(i) ||
      Pbmt.isUncache(toIfu.bits.itlbPbmt(i))
    }
    val blkPaddrAll = s2_req_paddr.map(addr => (addr(PAddrBits - 1, blockOffBits) << blockOffBits).asUInt)
    (0 until ICacheDataBanks).foreach { i =>
      val diffMainPipeOut = DifftestModule(new DiffRefillEvent, dontCare = true)
      diffMainPipeOut.coreid := io.hartId
      diffMainPipeOut.index  := (3 + i).U

      val bankSel = getBankSel(s2_req_offset, s2_valid).reduce(_ | _)
      val lineSel = getLineSel(s2_req_offset)

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
