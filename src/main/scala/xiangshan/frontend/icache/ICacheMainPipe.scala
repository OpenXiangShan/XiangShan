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
import difftest._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.fu.PMPReqBundle
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache.mmu._
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FtqToICacheRequestBundle

class ICacheMainPipeResp(implicit p: Parameters) extends ICacheBundle {
  val doubleline:       Bool      = Bool()
  val vaddr:            Vec[UInt] = Vec(PortNumber, UInt(VAddrBits.W))
  val data:             UInt      = UInt(blockBits.W)
  val paddr:            Vec[UInt] = Vec(PortNumber, UInt(PAddrBits.W))
  val exception:        Vec[UInt] = Vec(PortNumber, UInt(ExceptionType.width.W))
  val pmp_mmio:         Vec[Bool] = Vec(PortNumber, Bool())
  val itlb_pbmt:        Vec[UInt] = Vec(PortNumber, UInt(Pbmt.width.W))
  val backendException: Bool      = Bool()
  /* NOTE: GPAddrBits(=50bit) is not enough for gpaddr here, refer to PR#3795
   * Sv48*4 only allows 50bit gpaddr, when software violates this requirement
   * it needs to fill the mtval2 register with the full XLEN(=64bit) gpaddr,
   * PAddrBitsMax(=56bit currently) is required for the frontend datapath due to the itlb ppn length limitation
   * (cases 56<x<=64 are handled by the backend datapath)
   */
  val gpaddr:            UInt = UInt(PAddrBitsMax.W)
  val isForVSnonLeafPTE: Bool = Bool()
}

class ICacheMainPipeBundle(implicit p: Parameters) extends ICacheBundle {
  val req:               DecoupledIO[FtqToICacheRequestBundle] = Flipped(DecoupledIO(new FtqToICacheRequestBundle))
  val resp:              Valid[ICacheMainPipeResp]             = ValidIO(new ICacheMainPipeResp)
  val topdownIcacheMiss: Bool                                  = Output(Bool())
  val topdownItlbMiss:   Bool                                  = Output(Bool())
}

class ICacheMetaReqBundle(implicit p: Parameters) extends ICacheBundle {
  val toIMeta:   DecoupledIO[ICacheReadBundle] = DecoupledIO(new ICacheReadBundle)
  val fromIMeta: ICacheMetaRespBundle          = Input(new ICacheMetaRespBundle)
}

class ICacheDataReqBundle(implicit p: Parameters) extends ICacheBundle {
  val toIData:   Vec[DecoupledIO[ICacheReadBundle]] = Vec(partWayNum, DecoupledIO(new ICacheReadBundle))
  val fromIData: ICacheDataRespBundle               = Input(new ICacheDataRespBundle)
}

class ICacheMSHRBundle(implicit p: Parameters) extends ICacheBundle {
  val req:  DecoupledIO[ICacheMissReq] = DecoupledIO(new ICacheMissReq)
  val resp: Valid[ICacheMissResp]      = Flipped(ValidIO(new ICacheMissResp))
}

class ICachePMPBundle(implicit p: Parameters) extends ICacheBundle {
  val req:  Valid[PMPReqBundle] = ValidIO(new PMPReqBundle())
  val resp: PMPRespBundle       = Input(new PMPRespBundle())
}

class ICachePerfInfo(implicit p: Parameters) extends ICacheBundle {
  val only_0_hit:      Bool      = Bool()
  val only_0_miss:     Bool      = Bool()
  val hit_0_hit_1:     Bool      = Bool()
  val hit_0_miss_1:    Bool      = Bool()
  val miss_0_hit_1:    Bool      = Bool()
  val miss_0_miss_1:   Bool      = Bool()
  val hit_0_except_1:  Bool      = Bool()
  val miss_0_except_1: Bool      = Bool()
  val except_0:        Bool      = Bool()
  val bank_hit:        Vec[Bool] = Vec(PortNumber, Bool())
  val hit:             Bool      = Bool()
}

class ICacheMainPipeInterface(implicit p: Parameters) extends ICacheBundle {
  val hartId: UInt = Input(UInt(hartIdLen.W))

  /*** internal interface ***/
  val dataArray:      ICacheDataReqBundle               = new ICacheDataReqBundle
  val metaArrayFlush: Vec[Valid[ICacheMetaFlushBundle]] = Vec(PortNumber, ValidIO(new ICacheMetaFlushBundle))
  val touch:          Vec[Valid[ReplacerTouch]]         = Vec(PortNumber, ValidIO(new ReplacerTouch))
  val wayLookupRead:  DecoupledIO[WayLookupInfo]        = Flipped(DecoupledIO(new WayLookupInfo))
  val mshr:           ICacheMSHRBundle                  = new ICacheMSHRBundle
  val ecc_enable:     Bool                              = Input(Bool())

  /*** outside interface ***/
  // FTQ
  val fetch: ICacheMainPipeBundle = new ICacheMainPipeBundle
  val flush: Bool                 = Input(Bool())
  // PMP
  val pmp: Vec[ICachePMPBundle] = Vec(PortNumber, new ICachePMPBundle)
  // IFU
  val respStall: Bool = Input(Bool())
  // backend/BEU
  val errors: Vec[Valid[L1CacheErrorInfo]] = Output(Vec(PortNumber, ValidIO(new L1CacheErrorInfo)))

  /*** PERF ***/
  val perfInfo: ICachePerfInfo = Output(new ICachePerfInfo)
}

//class ICacheDB(implicit p: Parameters) extends ICacheBundle {
//  val blk_vaddr: UInt = UInt((VAddrBits - blockOffBits).W)
//  val blk_paddr: UInt = UInt((PAddrBits - blockOffBits).W)
//  val hit:       Bool = Bool()
//}

class ICacheMainPipe(implicit p: Parameters) extends ICacheModule with HasICacheECCHelper {
  val io: ICacheMainPipeInterface = IO(new ICacheMainPipeInterface)

  /** Input/Output port */
  private val (fromFtq, toIFU)   = (io.fetch.req, io.fetch.resp)
  private val (toData, fromData) = (io.dataArray.toIData, io.dataArray.fromIData)
  private val toMetaFlush        = io.metaArrayFlush
  private val (toMSHR, fromMSHR) = (io.mshr.req, io.mshr.resp)
  private val (toPMP, fromPMP)   = (io.pmp.map(_.req), io.pmp.map(_.resp))
  private val fromWayLookup      = io.wayLookupRead
  private val ecc_enable =
    if (ICacheForceMetaECCError || ICacheForceDataECCError) true.B else io.ecc_enable

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
  val s1_ready, s2_ready           = Wire(Bool())
  val s0_fire, s1_fire, s2_fire    = Wire(Bool())
  val s0_flush, s1_flush, s2_flush = Wire(Bool())

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
  private val fromFtqReq       = fromFtq.bits.pcMemRead
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

  private val s0_backendException = fromFtq.bits.backendException

  /**
    ******************************************************************************
    * get waymask and tlb info from wayLookup
    ******************************************************************************
    */
  fromWayLookup.ready := s0_fire
  private val s0_waymasks              = VecInit(fromWayLookup.bits.waymask.map(_.asTypeOf(Vec(nWays, Bool()))))
  private val s0_req_ptags             = fromWayLookup.bits.ptag
  private val s0_req_gpaddr            = fromWayLookup.bits.gpaddr
  private val s0_req_isForVSnonLeafPTE = fromWayLookup.bits.isForVSnonLeafPTE
  private val s0_itlb_exception        = fromWayLookup.bits.itlb_exception
  private val s0_itlb_pbmt             = fromWayLookup.bits.itlb_pbmt
  private val s0_meta_codes            = fromWayLookup.bits.meta_codes
  private val s0_hits                  = VecInit(fromWayLookup.bits.waymask.map(_.orR))

  when(s0_fire) {
    assert(
      (0 until PortNumber).map(i => s0_req_vSetIdx(i) === fromWayLookup.bits.vSetIdx(i)).reduce(_ && _),
      "vSetIdx from ftq and wayLookup mismatch! vaddr=0x%x ftq: vSet0=0x%x vSet1=0x%x wayLookup: vSet0=0x%x vSet1=0x%x",
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
  private val s1_valid =
    generatePipeControl(lastFire = s0_fire, thisFire = s1_fire, thisFlush = s1_flush, lastFlush = false.B)

  private val s1_req_vaddr  = RegEnable(s0_req_vaddr, 0.U.asTypeOf(s0_req_vaddr), s0_fire)
  private val s1_req_ptags  = RegEnable(s0_req_ptags, 0.U.asTypeOf(s0_req_ptags), s0_fire)
  private val s1_req_gpaddr = RegEnable(s0_req_gpaddr, 0.U.asTypeOf(s0_req_gpaddr), s0_fire)
  private val s1_req_isForVSnonLeafPTE =
    RegEnable(s0_req_isForVSnonLeafPTE, 0.U.asTypeOf(s0_req_isForVSnonLeafPTE), s0_fire)
  private val s1_doubleline       = RegEnable(s0_doubleline, 0.U.asTypeOf(s0_doubleline), s0_fire)
  private val s1_SRAMhits         = RegEnable(s0_hits, 0.U.asTypeOf(s0_hits), s0_fire)
  private val s1_itlb_exception   = RegEnable(s0_itlb_exception, 0.U.asTypeOf(s0_itlb_exception), s0_fire)
  private val s1_backendException = RegEnable(s0_backendException, false.B, s0_fire)
  private val s1_itlb_pbmt        = RegEnable(s0_itlb_pbmt, 0.U.asTypeOf(s0_itlb_pbmt), s0_fire)
  private val s1_waymasks         = RegEnable(s0_waymasks, 0.U.asTypeOf(s0_waymasks), s0_fire)
  private val s1_meta_codes       = RegEnable(s0_meta_codes, 0.U.asTypeOf(s0_meta_codes), s0_fire)

  private val s1_req_vSetIdx = s1_req_vaddr.map(get_idx)
  private val s1_req_paddr   = getPaddrFromPtag(s1_req_vaddr, s1_req_ptags)
  private val s1_req_offset  = s1_req_vaddr(0)(log2Ceil(blockBytes) - 1, 0)

  // do metaArray ECC check
  private val s1_meta_corrupt = VecInit((s1_req_ptags zip s1_meta_codes zip s1_waymasks).map {
    case ((meta, code), waymask) =>
      val hit_num = PopCount(waymask)
      // NOTE: if not hit, encodeMetaECC(meta) =/= code can also be true, but we don't care about it
      (encodeMetaECC(meta) =/= code && hit_num === 1.U) || // hit one way, but parity code does not match, ECC failure
      hit_num > 1.U                                        // hit multi-way, must be an ECC failure
  })
  // force clear meta_corrupt when parity check is disabled
  when(!ecc_enable) {
    s1_meta_corrupt := VecInit(Seq.fill(PortNumber)(false.B))
  }

  /**
    ******************************************************************************
    * update replacement status register
    ******************************************************************************
    */
  (0 until PortNumber).foreach { i =>
    io.touch(i).bits.vSetIdx := s1_req_vSetIdx(i)
    io.touch(i).bits.way     := OHToUInt(s1_waymasks(i))
  }
  io.touch(0).valid := RegNext(s0_fire) && s1_SRAMhits(0)
  io.touch(1).valid := RegNext(s0_fire) && s1_SRAMhits(1) && s1_doubleline

  /**
    ******************************************************************************
    * PMP check
    ******************************************************************************
    */
  toPMP.zipWithIndex.foreach { case (p, i) =>
    // if itlb has exception, paddr can be invalid, therefore pmp check can be skipped do not do this now for timing
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
    (s1_req_ptags(i) === getPhyTagFromBlk(fromMSHR.bits.blkPaddr)) &&
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

  private val s2_valid =
    generatePipeControl(lastFire = s1_fire, thisFire = s2_fire, thisFlush = s2_flush, lastFlush = false.B)

  private val s2_req_vaddr  = RegEnable(s1_req_vaddr, 0.U.asTypeOf(s1_req_vaddr), s1_fire)
  private val s2_req_ptags  = RegEnable(s1_req_ptags, 0.U.asTypeOf(s1_req_ptags), s1_fire)
  private val s2_req_gpaddr = RegEnable(s1_req_gpaddr, 0.U.asTypeOf(s1_req_gpaddr), s1_fire)
  private val s2_req_isForVSnonLeafPTE =
    RegEnable(s1_req_isForVSnonLeafPTE, 0.U.asTypeOf(s1_req_isForVSnonLeafPTE), s1_fire)
  private val s2_doubleline       = RegEnable(s1_doubleline, 0.U.asTypeOf(s1_doubleline), s1_fire)
  private val s2_exception        = RegEnable(s1_exception_out, 0.U.asTypeOf(s1_exception_out), s1_fire)
  private val s2_backendException = RegEnable(s1_backendException, false.B, s1_fire)
  private val s2_pmp_mmio         = RegEnable(s1_pmp_mmio, 0.U.asTypeOf(s1_pmp_mmio), s1_fire)
  private val s2_itlb_pbmt        = RegEnable(s1_itlb_pbmt, 0.U.asTypeOf(s1_itlb_pbmt), s1_fire)
  private val s2_waymasks         = RegEnable(s1_waymasks, 0.U.asTypeOf(s1_waymasks), s1_fire)

  private val s2_req_vSetIdx = s2_req_vaddr.map(get_idx)
  private val s2_req_offset  = s2_req_vaddr(0)(log2Ceil(blockBytes) - 1, 0)
  private val s2_req_paddr   = getPaddrFromPtag(s2_req_vaddr, s2_req_ptags)

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
  private val s2_bank_corrupt = (0 until ICacheDataBanks).map(i => encodeDataECC(s2_datas(i)) =/= s2_codes(i))
  // if data is from MSHR, we don't need to check ECC
  private val s2_data_corrupt = VecInit((0 until PortNumber).map { port =>
    (0 until ICacheDataBanks).map { bank =>
      s2_bank_corrupt(bank) && s2_bankSel(port)(bank).asBool && !s2_data_is_from_MSHR(bank)
    }.reduce(_ || _) && s2_SRAMhits(port)
  })
  // force clear data_corrupt when parity check is disabled
  when(!ecc_enable) {
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
    toMetaFlush(i).valid       := (s2_meta_corrupt(i) || s2_data_corrupt(i)) && RegNext(s1_fire)
    toMetaFlush(i).bits.virIdx := s2_req_vSetIdx(i)
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
    (s2_req_ptags(i) === getPhyTagFromBlk(fromMSHR.bits.blkPaddr)) &&
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

  private val toMSHRArbiter = Module(new Arbiter(new ICacheMissReq, PortNumber))

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
    toMSHRArbiter.io.in(i).bits.blkPaddr := getBlkAddr(s2_req_paddr(i))
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
  toIFU.valid                 := s2_fire
  toIFU.bits.doubleline       := s2_doubleline
  toIFU.bits.data             := s2_datas.asTypeOf(UInt(blockBits.W))
  toIFU.bits.backendException := s2_backendException
  (0 until PortNumber).foreach { i =>
    toIFU.bits.vaddr(i) := s2_req_vaddr(i)
    toIFU.bits.paddr(i) := s2_req_paddr(i)
    val needThisLine = if (i == 0) true.B else s2_doubleline
    toIFU.bits.exception(i) := Mux(needThisLine, s2_exception_out(i), ExceptionType.none)
    toIFU.bits.pmp_mmio(i)  := Mux(needThisLine, s2_pmp_mmio(i), false.B)
    toIFU.bits.itlb_pbmt(i) := Mux(needThisLine, s2_itlb_pbmt(i), Pbmt.pma)
  }
  // valid only for the first gpf
  toIFU.bits.gpaddr            := s2_req_gpaddr
  toIFU.bits.isForVSnonLeafPTE := s2_req_isForVSnonLeafPTE

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
  io.perfInfo.only_0_hit      := s2_hits(0) && !s2_doubleline
  io.perfInfo.only_0_miss     := !s2_hits(0) && !s2_doubleline
  io.perfInfo.hit_0_hit_1     := s2_hits(0) && s2_hits(1) && s2_doubleline
  io.perfInfo.hit_0_miss_1    := s2_hits(0) && !s2_hits(1) && s2_doubleline
  io.perfInfo.miss_0_hit_1    := !s2_hits(0) && s2_hits(1) && s2_doubleline
  io.perfInfo.miss_0_miss_1   := !s2_hits(0) && !s2_hits(1) && s2_doubleline
  io.perfInfo.hit_0_except_1  := s2_hits(0) && ExceptionType.hasException(s2_exception(1)) && s2_doubleline
  io.perfInfo.miss_0_except_1 := !s2_hits(0) && ExceptionType.hasException(s2_exception(1)) && s2_doubleline
  io.perfInfo.bank_hit(0)     := s2_hits(0)
  io.perfInfo.bank_hit(1)     := s2_hits(1) && s2_doubleline
  io.perfInfo.except_0        := ExceptionType.hasException(s2_exception(0))
  io.perfInfo.hit             := s2_hits(0) && (!s2_doubleline || s2_hits(1))

  /** <PERF> fetch bubble generated by icache miss */
  XSPerfAccumulate("icache_bubble_s2_miss", s2_valid && !s2_fetch_finish)
  XSPerfAccumulate("icache_bubble_s0_wayLookup", s0_valid && !fromWayLookup.ready)

  io.fetch.topdownIcacheMiss := !s2_fetch_finish
  io.fetch.topdownItlbMiss   := s0_valid && !fromWayLookup.ready

  // class ICacheTouchDB(implicit p: Parameters) extends ICacheBundle{
  //   val blkPaddr  = UInt((PAddrBits - blockOffBits).W)
  //   val vSetIdx   = UInt(idxBits.W)
  //   val waymask   = UInt(wayBits.W)
  // }

  // private val isWriteICacheTouchTable =
  //   WireInit(Constantin.createRecord("isWriteICacheTouchTable" + p(XSCoreParamsKey).HartId.toString))
  // private val ICacheTouchTable =
  //   ChiselDB.createTable("ICacheTouchTable" + p(XSCoreParamsKey).HartId.toString, new ICacheTouchDB)

  // val ICacheTouchDumpData = Wire(Vec(PortNumber, new ICacheTouchDB))
  // (0 until PortNumber).foreach{ i =>
  //   ICacheTouchDumpData(i).blkPaddr  := getBlkAddr(s2_req_paddr(i))
  //   ICacheTouchDumpData(i).vSetIdx   := s2_req_vSetIdx(i)
  //   ICacheTouchDumpData(i).waymask   := OHToUInt(s2_tag_match_vec(i))
  //   ICacheTouchTable.log(
  //     data  = ICacheTouchDumpData(i),
  //     en    = io.touch(i).valid,
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
      ExceptionType.hasException(toIFU.bits.exception(i)) ||
      toIFU.bits.pmp_mmio(i) ||
      Pbmt.isUncache(toIFU.bits.itlb_pbmt(i))
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
