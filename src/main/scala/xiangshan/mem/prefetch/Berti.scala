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

package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SeqBoolBitwiseOps
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache.mmu._
import xiangshan.cache.{DCacheBundle, DCacheModule, HasDCacheParameters, HasL1CacheParameters}
import xiangshan.mem.L1PrefetchReq

case class BertiParams
(
  name: String = "berti",
  ht_set_cnt: Int = 1, // areaOpt. // 64, gem5. // 8 , paper.
  ht_way_cnt: Int = 16,
  ht_way_replace: String = "plru",
  ht_list_size: Int = 6 , // areaOpt. // 6 , gem5. // 16, paper.
  ht_list_replace: String = "fifo",
  dt_way_cnt: Int = 16,   // 64, gem5. // 16, paper.
  dt_delta_size: Int = 4, // 4 , gem5. // 16, paper.
  use_byte_addr: Boolean = false,
) extends PrefetcherParams{
  override def TRAIN_FILTER_SIZE = 6
  override def PREFETCH_FILTER_SIZE = 16
}

trait HasBertiHelper extends HasCircularQueuePtrHelper with HasDCacheParameters {
  def bertiParams = p(XSCoreParamsKey).prefetcher.find {
    case p: BertiParams => true
    case _ => false
  }.get.asInstanceOf[BertiParams]
  /**
    * Ht: history Table
    * Dt: Delta Table
    * tsp: Timestamp
    */

  def _name: String = bertiParams.name

  val VADDR_HASH_WIDTH = 5
  val BLK_ADDR_RAW_WIDTH = 10
  val HASH_TAG_WIDTH = VADDR_HASH_WIDTH + BLK_ADDR_RAW_WIDTH

  def PcOffsetWidth: Int = 2
  def DeltaWidth: Int = 13
  def HtPcTagWidth: Int = HASH_TAG_WIDTH
  def HtLineVAddrWidth: Int = 24
  def HtLineOffsetWidth: Int = DCacheLineOffset
  def DtPcTagWidth: Int = HASH_TAG_WIDTH
  def DtCntWidth: Int = 4

  def useByteAddr: Boolean = bertiParams.use_byte_addr
  def useFIFO: Boolean = bertiParams.ht_list_replace == "fifo"
  assert(useFIFO, s"unsupported ht replacement policy: ${bertiParams.ht_list_replace}")
  def HtSetSize: Int = bertiParams.ht_set_cnt
  def HtWaySize: Int = bertiParams.ht_way_cnt
  def DtWaySize: Int = bertiParams.dt_way_cnt
  def HtListSize: Int = bertiParams.ht_list_size
  def DtDeltaSize: Int = bertiParams.dt_delta_size
  def DtDeltaIndexWidth: Int = log2Up(DtDeltaSize)

  def HtSetWidth: Int = log2Up(HtSetSize)
  def HtWayWidth: Int = log2Up(HtWaySize)
  def DtWayWidth: Int = log2Up(DtWaySize)

  def DIR_REGION: Int = 256 // 256 lines -> a dcache way
  def DIR_REGION_BITS: Int = log2Up(DIR_REGION)
  def DELTA_THRESHOLD: Int = if (useByteAddr) blockBytes else 1 // 64 Bytes = 1 line

  def _getLineVAddr(vaddr: UInt): UInt = {
    vaddr(vaddr.getWidth - 1, HtLineOffsetWidth)
  }

  def _signedExtend(x: UInt, width: Int): UInt = {
    if (x.getWidth >= width) {
      x
    } else {
      Cat(Fill(width - x.getWidth, x.head(1)), x)
    }
  }

  def vaddr_hash(x: UInt): UInt = {
    val width = VADDR_HASH_WIDTH
    val low = x(width - 1, 0)
    val mid = x(2 * width - 1, width)
    val high = x(3 * width - 1, 2 * width)
    low ^ mid ^ high
  }
  def pc_hash_tag(x: UInt): UInt = {
    val low = x(BLK_ADDR_RAW_WIDTH - 1, 0)
    val high = x(BLK_ADDR_RAW_WIDTH - 1 + 3 * VADDR_HASH_WIDTH, BLK_ADDR_RAW_WIDTH)
    val high_hash = vaddr_hash(high)
    Cat(high_hash, low)
  }

  // for 16-bit instruction
  def getPCHash(pc: UInt): UInt = pc_hash_tag(pc) // pc >> 1

  def getTrainBaseAddr(vaddr: UInt): UInt = {
    if (useByteAddr) {
      vaddr
    } else {
      _getLineVAddr(vaddr)
    }
  }

  private def shiftedDelta(delta: SInt, shift: Int): SInt = {
    val padded = delta.pad(VAddrBits).asUInt
    if (shift == 0) {
      padded.asSInt
    } else {
      Cat(padded(VAddrBits - shift - 1, 0), 0.U(shift.W)).asSInt
    }
  }

  def getPrefetchVAddr(triggerVA: UInt, delta: SInt, ratio: Int = 0): UInt = {
    val shift = if (useByteAddr) ratio else HtLineOffsetWidth + ratio
    (triggerVA.asSInt + shiftedDelta(delta, shift)).asUInt
  }

  def getPrefetchVAddr(triggerVA: UInt, delta: SInt, ratio: UInt): UInt = {
    val baseShift = if (useByteAddr) 0 else HtLineOffsetWidth
    val shifted = MuxLookup(ratio, shiftedDelta(delta, baseShift))((0 until 8).map { i =>
      i.U -> shiftedDelta(delta, baseShift + i)
    })
    (triggerVA.asSInt + shifted).asUInt
  }
}

abstract class BertiBundle(implicit p: Parameters) extends XSBundle with HasBertiHelper
abstract class BertiModule(implicit p: Parameters) extends XSModule with HasBertiHelper

object DeltaStatus extends ChiselEnum {
  // prefetch priority is from low to high
  val NO_PREF, L2_PREF_REPL, L2_PREF, L1_PREF = Value
}

class HTSearchReq(implicit p: Parameters) extends BertiBundle {
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val latency = UInt(LATENCY_WIDTH.W)
}
class LearnDeltasLiteIO(implicit p: Parameters) extends BertiBundle {
  val valid = Bool()
  val delta = SInt(DeltaWidth.W)
  val pc = UInt(VAddrBits.W)
}

class LearnDeltasIO(implicit p: Parameters) extends BertiBundle {
  val validVec = Vec(HtListSize, Bool())
  val deltaVec = Vec(HtListSize, SInt(DeltaWidth.W))
  val pc = UInt(VAddrBits.W)
}

class HistoryTable()(implicit p: Parameters) extends BertiModule {
  /*** static variable ***/
  val a1_stat_access_pcHysteresis = WireInit(false.B)
  val a1_stat_access_replace = WireInit(false.B)
  val a1_stat_access_update = WireInit(false.B)
  val a1_stat_access_currVA = WireInit(0.U(HtLineVAddrWidth.W))
  val a1_stat_access_lastVA = WireInit(0.U(HtLineVAddrWidth.W))
  val s2_stat_find_delta = WireInit(false.B)
  val s2_stat_late = WireInit(false.B)
  val s2_stat_overflow = WireInit(false.B)
  val s2_stat_satisfy = WireInit(false.B)
  val s2_stat_dissatisfy = WireInit(false.B)
  val s2_stat_histLineVA = WireInit(0.U(HtLineVAddrWidth.W))
  val s2_stat_currLineVA = WireInit(0.U(HtLineVAddrWidth.W))
  /*** built-in function */
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until HtWaySize).map(f))
  def listMap[T <: Data](f: Int => T) = VecInit((0 until HtListSize).map(f))
  def getIndex(pc: UInt): UInt = if(HtSetSize == 1) 0.U else getPCHash(pc)(HtSetWidth-1, 0)
  def getTag(pc: UInt): UInt = if(HtSetSize == 1) getPCHash(pc) else getPCHash(pc)(HtPcTagWidth + HtSetWidth - 1, HtSetWidth)
  def getTrainBaseAddr2HT(vaddr: UInt): UInt = {
    getTrainBaseAddr(vaddr)(HtLineVAddrWidth - 1, 0)
  }
  def getDirRegionAddr(baseVAddr: UInt): UInt = {
    baseVAddr >> DIR_REGION_BITS
  }
  def checkDirSameRegion(delta: SInt): Bool = {
    delta < (DIR_REGION).S(DeltaWidth.W) && delta > (-DIR_REGION).S(DeltaWidth.W)
  }
  def checkTimeliness(currTsp: UInt, latency: UInt, recordTsp: UInt): Bool = {
    // control the width of the calculation
    val timelyTsp = Cat(0.U, currTsp(LATENCY_WIDTH-1, 0)) - latency(LATENCY_WIDTH-1, 0)
    // head: the overflow bit, tail(1): the tsp bit
    latency =/= 0.U && !timelyTsp.head(1) && timelyTsp.tail(1) > recordTsp(LATENCY_WIDTH-1, 0)
  }
  def checkDissatisfy(delta: SInt): Bool = {
    delta < (DELTA_THRESHOLD).S(DeltaWidth.W) && delta > (-DELTA_THRESHOLD).S(DeltaWidth.W)
  }

  /*** built-in class */
  class Entry()(implicit p: Parameters) extends BertiBundle {
    val baseVAddr = UInt(HtLineVAddrWidth.W)
    val tsp = UInt(LATENCY_WIDTH.W)

    def alloc(_baseVAddr: UInt, _tsp: UInt): Unit = {
      baseVAddr := _baseVAddr
      tsp := _tsp
    }

    def update(_baseVAddr: UInt, _tsp: UInt): Unit = {
      baseVAddr := _baseVAddr
      tsp := _tsp
    }
  }

  class HtListPointer(implicit p: Parameters) extends CircularQueuePtr[HtListPointer](HtListSize) {}

  /*** io */
  val io = IO(new Bundle{
    val access = Flipped(ValidIO(new Bundle{
      val pc = UInt(VAddrBits.W)
      val vaddr = UInt(VAddrBits.W)
    }))
    val search = new Bundle{
      val req = Flipped(ValidIO(new HTSearchReq()))
      val resp = Output(new LearnDeltasLiteIO())
    }
  })

  /*** data structure */
  // TODO lyq: refractor
  val entries = Reg(Vec(HtSetSize, Vec(HtWaySize, Vec(HtListSize, new Entry))))
  val valids = RegInit(0.U.asTypeOf(Vec(HtSetSize, Vec(HtWaySize, Vec(HtListSize, Bool())))))
  val hysteresis = RegInit(0.U.asTypeOf(Vec(HtSetSize, Vec(HtWaySize, Bool()))))
  val pcTags = RegInit(0.U.asTypeOf(Vec(HtSetSize, Vec(HtWaySize, UInt(HtPcTagWidth.W)))))
  val wayReplacer = ReplacementPolicy.fromString("setplru", HtWaySize, HtSetSize)
  // list: for FIFO replace policy
  val accessPtrs = RegInit(0.U.asTypeOf(Vec(HtSetSize, Vec(HtWaySize, new HtListPointer))))
  // list: for easier learning policy
  val learnPtrs = RegInit(0.U.asTypeOf(Vec(HtSetSize, Vec(HtWaySize, new HtListPointer))))

  val currTime = GTimer()
  val currTsp = Wire(UInt(LATENCY_WIDTH.W))
  currTsp := currTime(LATENCY_WIDTH-1, 0)

  /*** functional function */
  def init(): Unit = {
    valids := 0.U.asTypeOf(chiselTypeOf(valids))
    accessPtrs.foreach(_.foreach(_ := 0.U.asTypeOf(new HtListPointer)))
    learnPtrs.foreach(_.foreach(_ := 0.U.asTypeOf(new HtListPointer)))
  }

  /**
    * A new entry is inserted in the history table (Write port in Figure 5)
    * either on-demand misses (Miss arrow from the L1D in Figure 5) or on hits
    * for prefetched cache lines (Hitp in Figure 5). The virtual address (VA) and
    * the IP (IP, VA arrow in Figure 5) are stored in the new entry along with
    * the current timestamp (not shown in the figure)
    * 
    * // TODO lyq:
    *   How to support multi port of access for both historyTable and deltaTable.
    *   Maybe hard due to set division.
    * 
    */

  /*** access */
  val a1_valid = Wire(Bool())
  val a1_set = Wire(UInt(HtSetWidth.W))
  val a1_tag = Wire(UInt(HtPcTagWidth.W))
  val a1_way = Wire(UInt(HtWayWidth.W))

  val a0_valid = io.access.valid
  val a0_req = io.access.bits
  val a0_set = getIndex(a0_req.pc)
  val a0_tag = getTag(a0_req.pc)
  val a0_baseVAddr = getTrainBaseAddr2HT(a0_req.vaddr)
  val a0_wayMatchVec = wayMap(w => valids(a0_set)(w).asUInt.orR && pcTags(a0_set)(w) === a0_tag)
  val a0_pcMatch = a0_wayMatchVec.orR
  val a0_hitWay = OHToUInt(a0_wayMatchVec)
  val a0_wayPrevMatch = a0_valid && a1_valid && a0_tag === a1_tag && a0_set === a1_set
  val a0_replaceWay = wayReplacer.way(a0_set)
  val a0_way = Mux(a0_wayPrevMatch, a1_way, Mux(a0_pcMatch, a0_hitWay, a0_replaceWay))

  assert(PopCount(a0_wayMatchVec) <= 1.U, s"HistoryTable access way match should be unique in ${this.getClass.getSimpleName}")

  a1_valid := GatedValidRegNext(a0_valid, false.B)
  a1_set := RegEnable(a0_set, a0_valid)
  a1_tag := RegEnable(a0_tag, a0_valid)
  a1_way := RegEnable(a0_way, a0_valid)
  val a1_pc = RegEnable(a0_req.pc, a0_valid)
  val a1_pcMatch = RegEnable(a0_pcMatch, a0_valid)
  val a1_baseVAddr = RegEnable(a0_baseVAddr, a0_valid)
  val a1_vaMatchVec = listMap(idx => valids(a1_set)(a1_way)(idx) && entries(a1_set)(a1_way)(idx).baseVAddr === a1_baseVAddr)
  val a1_vaMatch = a1_vaMatchVec.orR
  val a1_listIdx = accessPtrs(a1_set)(a1_way).value
  val a1_lastListIdx = (accessPtrs(a1_set)(a1_way) - 1.U).value

  when(a1_valid) {
    when(a1_pcMatch) {
      wayReplacer.access(a1_set, a1_way)
      when(!a1_vaMatch) {
        accessPtrs(a1_set)(a1_way) := accessPtrs(a1_set)(a1_way) + 1.U
        hysteresis(a1_set)(a1_way) := true.B
        valids(a1_set)(a1_way)(a1_listIdx) := true.B
        entries(a1_set)(a1_way)(a1_listIdx).alloc(a1_baseVAddr, currTsp)

        a1_stat_access_update := valids(a1_set)(a1_way)(a1_listIdx)
        a1_stat_access_currVA := a1_baseVAddr
        a1_stat_access_lastVA := entries(a1_set)(a1_way)(a1_lastListIdx).baseVAddr
      }
    }/* .elsewhen(hysteresis(a1_set)(a1_way)) {
      hysteresis(a1_set)(a1_way) := false.B
      a1_stat_access_pcHysteresis := true.B
    } */.otherwise {
      wayReplacer.access(a1_set, a1_way)
      a1_stat_access_replace := true.B
      val repListIdx = 0
      entries(a1_set)(a1_way)(repListIdx).alloc(a1_baseVAddr, currTsp)
      valids(a1_set)(a1_way).map(_ := false.B)
      valids(a1_set)(a1_way)(repListIdx) := true.B
      hysteresis(a1_set)(a1_way) := true.B
      pcTags(a1_set)(a1_way) := a1_tag
      accessPtrs(a1_set)(a1_way) := 0.U.asTypeOf(new HtListPointer)
      learnPtrs(a1_set)(a1_way) := 0.U.asTypeOf(new HtListPointer)
    }
  }

  /*** search */
  val searchReq = io.search.req.bits
  val s0_valid = io.search.req.valid
  val s0_set = getIndex(searchReq.pc)
  val s0_tag = getTag(searchReq.pc)
  val s0_baseVAddr = getTrainBaseAddr2HT(searchReq.vaddr)
  val s0_wayValidVec = wayMap(w => valids(s0_set)(w).asUInt.orR)
  val s0_wayMatchVec = wayMap(w => s0_wayValidVec(w) && pcTags(s0_set)(w) === s0_tag)
  val s0_hit = s0_wayMatchVec.orR
  val s0_way = OHToUInt(s0_wayMatchVec)
  val s0_listIdx = learnPtrs(s0_set)(s0_way).value
  val s0_histEntry = entries(s0_set)(s0_way)(s0_listIdx)
  val s0_entryValid = s0_hit && valids(s0_set)(s0_way)(s0_listIdx)
  assert(PopCount(s0_wayMatchVec) <= 1.U, s"HistoryTable search way match should be unique in ${this.getClass.getSimpleName}")

  when(s0_valid && s0_hit) {
    wayReplacer.access(s0_set, s0_way)
    learnPtrs(s0_set)(s0_way) := learnPtrs(s0_set)(s0_way) + 1.U
  }

  val s1_valid = GatedValidRegNext(s0_valid, false.B)
  val s1_pc = RegEnable(searchReq.pc, s0_valid)
  val s1_baseVAddr = RegEnable(s0_baseVAddr, s0_valid)
  val s1_latency = RegEnable(searchReq.latency, s0_valid)
  val s1_histBaseVAddr = RegEnable(s0_histEntry.baseVAddr, s0_valid)
  val s1_histTsp = RegEnable(s0_histEntry.tsp, s0_valid)
  val s1_entryValid = RegEnable(s0_entryValid, s0_valid)

  val s1_diffFull = (s1_baseVAddr.zext - s1_histBaseVAddr.zext).asSInt
  val s1_originDelta = s1_diffFull(DeltaWidth - 1, 0).asSInt
  val s1_isTimely = checkTimeliness(currTsp, s1_latency, s1_histTsp)

  val s2_valid = GatedValidRegNext(s1_valid, false.B)
  val s2_pc = RegEnable(s1_pc, s1_valid)
  val s2_entryValid = RegEnable(s1_entryValid, s1_valid)
  val s2_diffFull = RegEnable(s1_diffFull, s1_valid)
  val s2_originDelta = RegEnable(s1_originDelta, s1_valid)
  val s2_isTimely = RegEnable(s1_isTimely, s1_valid)
  // by checking whether delta can be symbol extended correctly
  val s2_overflow = s2_diffFull =/= s2_originDelta.asSInt.pad(s2_diffFull.getWidth)
  val s2_dissatisfy = checkDissatisfy(s2_originDelta)
  val s2_result = WireInit(0.U.asTypeOf(new LearnDeltasLiteIO))
  s2_result.pc := s2_pc
  s2_result.valid := s2_valid && s2_entryValid && !s2_overflow && !s2_dissatisfy && s2_isTimely
  s2_result.delta := s2_originDelta

  s2_stat_find_delta := s2_valid && s2_entryValid
  s2_stat_late := !s2_isTimely
  s2_stat_overflow := s2_overflow
  s2_stat_dissatisfy := s2_dissatisfy
  s2_stat_satisfy := !s2_overflow && !s2_dissatisfy
  s2_stat_histLineVA := RegNext(s1_baseVAddr)
  s2_stat_currLineVA := RegNext(s1_histBaseVAddr)

  val s3_valid = GatedValidRegNext(s2_valid, false.B)
  val s3_result = RegEnable(s2_result, s2_valid)

  io.search.resp := s3_result
  io.search.resp.valid := s3_valid && s3_result.valid

  /*** performance counter */
  XSPerfAccumulate("access_req", a1_valid)
  XSPerfAccumulate("access_replace", a1_valid && a1_stat_access_replace)
  XSPerfAccumulate("access_update", a1_valid && a1_stat_access_update)
  XSPerfAccumulate("access_pcHysteresis", a1_valid && a1_stat_access_pcHysteresis)
  XSPerfAccumulate("search_req", io.search.req.valid)
  XSPerfAccumulate("search_resp_valid", io.search.resp.valid)
  XSPerfAccumulate("search_resp_find_total", s2_stat_find_delta)
  XSPerfAccumulate("search_resp_find_overflow", s2_stat_find_delta && s2_stat_overflow)
  XSPerfAccumulate("search_resp_find_dissatisfy", s2_stat_find_delta && !s2_stat_overflow && s2_stat_dissatisfy)
  XSPerfAccumulate("search_resp_find_late", s2_stat_find_delta && !s2_stat_overflow && !s2_stat_dissatisfy && s2_stat_late)
  XSPerfAccumulate("search_resp_find_satisfy", s2_stat_find_delta && !s2_stat_overflow && !s2_stat_dissatisfy && !s2_stat_late)

  class AccessLogDb extends Bundle {
    val pcHysteresis = Bool()
    val isReplace = Bool() // to avoid SQLite keywords
    val isUpdate = Bool() // to avoid SQLite keywords
    val currVA = UInt(HtLineVAddrWidth.W)
    val lastVA = UInt(HtLineVAddrWidth.W)
    val pc = UInt(VAddrBits.W)
  }
  val accessLog = Wire(new AccessLogDb())
  accessLog.pcHysteresis := a1_stat_access_pcHysteresis
  accessLog.isReplace := a1_stat_access_replace
  accessLog.isUpdate := a1_stat_access_update
  accessLog.currVA := a1_stat_access_currVA
  accessLog.lastVA := a1_stat_access_lastVA
  accessLog.pc := a1_pc
  val accessLogDb = ChiselDB.createTable(s"${_name}_accessLog${p(XSCoreParamsKey).HartId}", new AccessLogDb, basicDB = true)
  accessLogDb.log(data = accessLog, en = a1_valid, clock = clock, reset = reset)

  class SearchLogDb extends Bundle {
    val histLineVA = UInt(HtLineVAddrWidth.W)
    val currLineVA = UInt(HtLineVAddrWidth.W)
    val pc = UInt(VAddrBits.W)
    val calDelta = UInt(DeltaWidth.W)
  }
  val searchLog = Wire(new SearchLogDb())
  searchLog.histLineVA := s2_stat_histLineVA
  searchLog.currLineVA := s2_stat_currLineVA
  searchLog.calDelta := s2_result.delta.asUInt
  searchLog.pc := s2_result.pc
  val searchLogDb = ChiselDB.createTable(s"${_name}_searchLog${p(XSCoreParamsKey).HartId}", new SearchLogDb, basicDB = false)
  searchLogDb.log(data = searchLog, en = s2_result.valid, clock = clock, reset = reset)
}

class DeltaTable()(implicit p: Parameters) extends BertiModule {
  val u1_stat_update_isEntryHit = WireInit(false.B)
  val u1_stat_update_isEntryMiss = WireInit(false.B)
  val u1_stat_update_isEntryReplace = WireInit(false.B)
  val u1_stat_update_isDeltaHit = WireInit(false.B)
  val u1_stat_update_isDeltaMiss = WireInit(false.B)
  val u1_stat_update_isDeltaReplace = WireInit(false.B)
  val u1_stat_update_evictEntryIdx = WireInit(0.U(DtWayWidth.W)) // TODO lyq: if have chiselMap, it may be eaiser to statistic evicted data
  val u1_stat_update_evictDelta = WireInit(0.S(DeltaWidth.W)) // TODO lyq: have no idea how to output this
  val p1_stat_prefetch_isEntryHit = WireInit(false.B)
  val p1_stat_prefetch_hitGradeVec = WireInit(VecInit.fill(DeltaStatus.all.size)(false.B))
  /*** built-in function */
  // def thresholdOfReset: UInt = 16.U 
  // def thresholdOfUpdate: UInt = 10.U 
  // def thresholdOfL1PF: UInt = 8.U 
  // def thresholdOfL2PF: UInt = 5.U 
  // def thresholdOfL2PFR: UInt = 2.U 
  val thresholdOfReset = Constantin.createRecord(_name+"_thresholdOfReset", 15)    // (1 << DtCntWidth) - 1
  val thresholdOfUpdate = Constantin.createRecord(_name+"_thresholdOfUpdate", 6)  // (1 << (DtCntWidth - 1))
  val thresholdOfL1PF = Constantin.createRecord(_name+"_thresholdOfL1PF", 4)      // ((1 << DtCntWidth) * 0.65).toInt
  val thresholdOfL2PF = Constantin.createRecord(_name+"_thresholdOfL2PF", 2)      // ((1 << DtCntWidth) * 0.5).toInt
  val thresholdOfL2PFR = Constantin.createRecord(_name+"_thresholdOfL2PFR", 1)    // ((1 << DtCntWidth) * 0.35).toInt
  val l2DepthRatio = Constantin.createRecord(s"${_name}_l2DepthRatio", 2)
  def getPcTag(pc: UInt): UInt = getPCHash(pc)
  def getStatus(conf: UInt): DeltaStatus.Type = {
    val res = Wire(DeltaStatus())
    when(conf >= thresholdOfL1PF){
      res := DeltaStatus.L1_PREF
    }.elsewhen(conf > thresholdOfL2PF){
      res := DeltaStatus.L2_PREF
    }.elsewhen(conf > thresholdOfL2PFR) {
      res := DeltaStatus.L2_PREF_REPL
    }.otherwise{
      res := DeltaStatus.NO_PREF
    }
    res
  }

  /*** built-in class */
  class DeltaInfo()(implicit p: Parameters) extends BertiBundle {
    val delta = SInt(DeltaWidth.W)
    val coverageCnt = UInt(DtCntWidth.W)
    val status = DeltaStatus()

    def init(): Unit = {
      delta := 0.S
      coverageCnt := 0.U
      status := DeltaStatus.NO_PREF
    }

    def set(_delta: SInt): Unit = {
      delta := _delta
      coverageCnt := 1.U
      status := DeltaStatus.NO_PREF
    }

    def update(inc: UInt = 1.U): Unit = {
      coverageCnt := coverageCnt + inc
    }

    // enter the new cycle
    // use next to use this record
    def newCycle(next: UInt = 0.U): Unit = {
      coverageCnt := 0.U
      status := getStatus(Mux(next === 0.U, coverageCnt, next))
    }

    // enter the new status
    // use next to use this record
    def newStatus(next: UInt = 0.U): Unit = {
      status := getStatus(Mux(next === 0.U, coverageCnt, next))
    }

    def isGreaterThan(x: DeltaInfo): Bool = {
      (status > x.status) || (status === x.status && coverageCnt >= x.coverageCnt)
    }
  }

  class DeltaEntry()(implicit p: Parameters) extends BertiBundle {
    val pcTag = UInt(DtPcTagWidth.W)
    val counter = UInt(DtCntWidth.W)
    val bestDeltaIdx = UInt(DtDeltaIndexWidth.W)
    val deltaList = Vec(DtDeltaSize, new DeltaInfo())

    def init(): Unit = {
      pcTag := 0.U
      counter := 0.U
      bestDeltaIdx := 0.U
      deltaList.map(x => x.init())
    }

    def setStatus(): Unit = {
      when(counter >= thresholdOfReset){
        counter := 0.U
        deltaList.map(x => x.newCycle())
      }.elsewhen(counter >= thresholdOfUpdate){
        deltaList.map(x => x.newStatus())
      }
      // when(counter >= thresholdOfUpdate){
      //   counter := 0.U
      //   deltaList.map(x => x.newCycle())
      // }
    }

    // TODO: use next value to set status
    // Because that way you don't lose that value from the latest update?
    // But it's a little hard to record the nextDeltaCnt
    def setStatus(nextCnt: UInt, nextDeltaCnt: Seq[UInt]): Unit = {
      when(nextCnt >= thresholdOfReset){
        counter := 0.U
        deltaList.zip(nextDeltaCnt).map{case (x, cnt) => x.newCycle(cnt)}
      }.elsewhen(nextCnt >= thresholdOfUpdate){
        deltaList.zip(nextDeltaCnt).map{case (x, cnt) => x.newStatus(cnt)}
      }
    }

    def setLite(_pcTag: UInt, _delta: SInt): Unit = {
      pcTag := _pcTag
      counter := 1.U

      (0 until DtDeltaSize).map(i => deltaList(i).init())
      deltaList(0).set(_delta)
      bestDeltaIdx := 0.U
    }

    def updateLite(_delta: SInt): Unit = {
      /**
        * 1. update
        *    1. hit: match and update
        *    2. miss: select and set
        * 2. check for status reset
        */

      assert(_delta =/= 0.S, s"delta should not be 0.U when call ${Thread.currentThread().getStackTrace()(1).getMethodName} of ${this.getClass.getSimpleName}")

      // update
      val nextCounter = counter + 1.U
      counter := nextCounter

      // delta match
      val matchVec = VecInit(deltaList.map(x => x.delta === _delta)).asUInt
      val invalidVec1 = deltaList.map(x => x.delta === 0.S)
      val invalidVec2 = deltaList.map(x => x.status === DeltaStatus.NO_PREF)
      val invalidVec3 = deltaList.map(x => x.status === DeltaStatus.L2_PREF_REPL)
      // It doesn't matter if allocIdx* === bestDeltaIdx, because the status is low anyway.
      val (allocIdx1, canAlloc1) = PriorityEncoderWithFlag(invalidVec1)
      val (allocIdx2, canAlloc2) = PriorityEncoderWithFlag(invalidVec2)
      val (allocIdx3, canAlloc3) = PriorityEncoderWithFlag(invalidVec3)
      val canAlloc = canAlloc1 || canAlloc2 || canAlloc3
      val allocIdx = Mux1H(Seq(
        canAlloc1 -> allocIdx1,
        canAlloc2 -> allocIdx2,
        canAlloc3 -> allocIdx3
      ))

      when (matchVec.orR){
        val updateIdx = OHToUInt(matchVec)
        deltaList(updateIdx).update()
        when(deltaList(updateIdx).isGreaterThan(deltaList(bestDeltaIdx))){
          bestDeltaIdx := updateIdx
        }
        u1_stat_update_isDeltaHit := true.B
      }.otherwise{
        u1_stat_update_isDeltaMiss := true.B
        when(canAlloc) {
          deltaList(allocIdx).set(_delta)
          u1_stat_update_isDeltaReplace := true.B
          u1_stat_update_evictDelta := deltaList(allocIdx).delta
        } // otherwise: drop the new delta
      }

      // // method 1: check here, low power but how about performance?
      // when(nextCounter  === thresholdOfReset){
      //   deltaList.map(x => x.reset())
      // }
    }
  }

  /*** io */
  val io = IO(new Bundle{
    val learn = Input(new LearnDeltasLiteIO())
    val train = Flipped(ValidIO(new TrainReqBundle()))
    val prefetch_l1 = ValidIO(new SourcePrefetchReq())
    val prefetch_l2 = ValidIO(new SourcePrefetchReq())
  })

  /*** data structure */
  /**
    * 16-entry, fully-associative, 4-bit FIFO replacement policy.
    * Each entry:
    *   10-bit IP tag
    *   4-bit counter
    *   an array of 16 deltas (13-bit delta, 4-bit coverage, 2-bit status)
    * 
    */
  val entries = Reg(Vec(DtWaySize, new DeltaEntry()))
  val valids = RegInit(0.U.asTypeOf(Vec(DtWaySize, Bool())))
  val replacer = ReplacementPolicy.fromString("plru", DtWaySize)

  /*** processing logic */
  // 1. set status
  entries.foreach(x => x.setStatus())

  // 2. learn
  val u0_valid = io.learn.valid && io.learn.delta =/= 0.S
  val u0_pcTag = getPcTag(io.learn.pc)
  val u0_matchVec = VecInit((0 until DtWaySize).map(i => valids(i) && entries(i).pcTag === u0_pcTag)).asUInt
  val u0_hit = u0_matchVec.orR
  val u0_way = Mux(u0_hit, OHToUInt(u0_matchVec), replacer.way)

  val u1_valid = GatedValidRegNext(u0_valid, false.B)
  val u1_learn = RegEnable(io.learn, u0_valid)
  val u1_hit = RegEnable(u0_hit, u0_valid)
  val u1_way = RegEnable(u0_way, u0_valid)
  
  when(u1_valid) {
    replacer.access(u1_way)
    when(!u1_hit) {
      entries(u1_way).setLite(getPcTag(u1_learn.pc), u1_learn.delta)
      valids(u1_way) := true.B
      u1_stat_update_isEntryMiss := true.B
      u1_stat_update_isEntryReplace := true.B
      u1_stat_update_evictEntryIdx := u1_way
    }.otherwise{
      entries(u1_way).updateLite(u1_learn.delta)
      u1_stat_update_isEntryHit := true.B
    }
  }

  // 3. perfetch
  val p0_valid = io.train.valid
  val p0_pcTag = getPcTag(io.train.bits.pc)
  val p0_matchVec = VecInit((0 until DtWaySize).map(i => valids(i) && entries(i).pcTag === p0_pcTag)).asUInt
  val p0_hit = p0_matchVec.orR
  val p0_way = OHToUInt(p0_matchVec)
  val p0_deltaInfo = entries(p0_way).deltaList(entries(p0_way).bestDeltaIdx)

  when(p0_valid && p0_hit) {
    replacer.access(p0_way)
  }

  val p1_valid = GatedValidRegNext(p0_valid && p0_hit, false.B)
  val p1_train = RegEnable(io.train.bits, p0_valid && p0_hit)
  val p1_info = RegEnable(p0_deltaInfo, p0_valid && p0_hit)
  val p1_pfReq = WireInit(0.U.asTypeOf(new SourcePrefetchReq))
  val p1_l2PfVA = WireInit(0.U(VAddrBits.W))
  p1_pfReq.triggerPC := p1_train.pc
  p1_pfReq.triggerVA := p1_train.vaddr
  p1_pfReq.prefetchVA := getPrefetchVAddr(p1_train.vaddr, p1_info.delta)
  p1_pfReq.prefetchTarget := PrefetchTarget.L3.id.U
  p1_stat_prefetch_isEntryHit := p1_valid
  p1_stat_prefetch_hitGradeVec(p1_info.status.asUInt) := p1_valid
  when(p1_info.status === DeltaStatus.L1_PREF) {
    p1_pfReq.prefetchTarget := PrefetchTarget.L1.id.U
    p1_l2PfVA := getPrefetchVAddr(p1_train.vaddr, p1_info.delta, l2DepthRatio)
  }.elsewhen(p1_info.status === DeltaStatus.L2_PREF || p1_info.status === DeltaStatus.L2_PREF_REPL) {
    p1_pfReq.prefetchTarget := PrefetchTarget.L2.id.U
    p1_l2PfVA := getPrefetchVAddr(p1_train.vaddr, p1_info.delta)
  }
  val p1_pfValid = p1_valid && p1_info.status =/= DeltaStatus.NO_PREF

  val p2_valid = GatedValidRegNext(p1_pfValid, false.B)
  val p2_l2PfVA = RegEnable(p1_l2PfVA, p1_pfValid)
  val p2_pfReq = RegEnable(p1_pfReq, p1_pfValid)

  io.prefetch_l1.valid := p2_valid && p2_pfReq.prefetchTarget === PrefetchTarget.L1.id.U
  io.prefetch_l1.bits := p2_pfReq
  io.prefetch_l2.valid := p2_valid
  io.prefetch_l2.bits := p2_pfReq
  io.prefetch_l2.bits.prefetchVA := p2_l2PfVA
  io.prefetch_l2.bits.prefetchTarget := PrefetchTarget.L2.id.U

  /** performance counter */
  class DeltaInfo2Db extends Bundle {
    val delta = UInt(DeltaWidth.W)
    val coverageCnt = UInt(DtCntWidth.W)
    val status = UInt(2.W)
  }
  val deltaInfo2Db = Wire(new DeltaInfo2Db())
  deltaInfo2Db.delta := p1_info.delta.asUInt
  deltaInfo2Db.coverageCnt := p1_info.coverageCnt
  deltaInfo2Db.status := p1_info.status.asUInt
  val prefetchDeltaTable = ChiselDB.createTable(s"${_name}_prefetchDeltaTable${p(XSCoreParamsKey).HartId}", new DeltaInfo2Db, basicDB = false)
  prefetchDeltaTable.log(data = deltaInfo2Db, en = p1_pfValid, clock = clock, reset = reset)
  
  XSPerfAccumulate("learn_req", io.learn.valid)
  XSPerfAccumulate("learn_req_0", io.learn.valid && io.learn.delta === 0.S)
  XSPerfAccumulate("learn_req_non_0", io.learn.valid && io.learn.delta =/= 0.S)
  XSPerfAccumulate("train_req", io.train.valid)
  XSPerfAccumulate("prefetch_req_l1", io.prefetch_l1.valid)
  XSPerfAccumulate("prefetch_req_l2", io.prefetch_l2.valid)
  XSPerfAccumulate("stat_update_isEntryHit", u1_stat_update_isEntryHit)
  XSPerfAccumulate("stat_update_isEntryMiss", u1_stat_update_isEntryMiss)
  XSPerfAccumulate("stat_update_isEntryReplace", u1_stat_update_isEntryReplace)
  XSPerfAccumulate("stat_update_isDeltaHit", u1_stat_update_isDeltaHit)
  XSPerfAccumulate("stat_update_isDeltaMiss", u1_stat_update_isDeltaMiss)
  XSPerfAccumulate("stat_update_isDeltaReplace", u1_stat_update_isDeltaReplace)
  XSPerfAccumulate("stat_prefetch_isEntryHit", p1_stat_prefetch_isEntryHit)
  for (i <- 0 until DeltaStatus.all.size) {
    XSPerfAccumulate(s"stat_prefetch_hitGrade_${i}", p1_stat_prefetch_hitGradeVec(i))
  }

}

class DeltaPrefetchBuffer(name: String, L1Size: Int = 0, L2Size: Int = 0)(implicit p: Parameters)
extends DCacheModule {
  /*** built-in function */
  /**
    *    Address Struture and Internal Statement
    *
    * [[HasDCacheParameters]]
    * |        page              |     page offset     | @physical
    * |        ptag              |     page offset     | @physical
    * |        vtag       |(alias) set | bank | offset | @virtual
    * |        line                    |   line offset | @virtual
    * [[HasL1CacheParameters]]
    * |        block                   |  block offset | @virtual
    * |        vtag       |(alias) idx | word | offset | @virtual
    *
    */
  private val TotalSize = L1Size + L2Size
  def IndexWidth(size: Int): Int = log2Up(size)
  def VLineWidth: Int = VAddrBits - DCacheLineOffset
  def PLineWidth: Int = PAddrBits - DCacheLineOffset
  def getLine(addr: UInt): UInt = addr(addr.getWidth - 1, DCacheLineOffset)
  def sizeMap[T <: Data](n: Int)(f: Int => T) = VecInit((0 until n).map(f))

  /*** built-in class */
  class Entry()(implicit p: Parameters) extends DCacheBundle {
    val vline = UInt(VLineWidth.W)
    val pline = UInt(PLineWidth.W)
    val pvalid = Bool()
    val target = UInt(PrefetchTarget.PfTgtBits.W)
    val tlbPending = Bool()

    def getPrefetchVA: UInt = Cat(vline, 0.U(DCacheLineOffset.W))
    def getPrefetchPA: UInt = Cat(pline, 0.U(DCacheLineOffset.W))
    def getPrefetchAlias: UInt = get_alias(getPrefetchVA)
    def fromSourcePrefetchReq(src: SourcePrefetchReq): Unit = {
      this.vline := getLine(src.prefetchVA)
      this.pline := 0.U
      this.pvalid := false.B
      this.target := src.prefetchTarget
      this.tlbPending := false.B
    }
    def updateEntryMerge(target: UInt): Unit = {
      when(target < this.target) {
        this.target := target
      }
    }
    def updateTlbReq(): Unit = {
      this.tlbPending := true.B
    }
    def updateTlbResp(paddr: UInt): Unit = {
      this.pline := getLine(paddr)
      this.pvalid := true.B
      this.tlbPending := false.B
    }
  }

  /*** io */
  val io = IO(new Bundle {
    val l1_srcReq = Flipped(ValidIO(new SourcePrefetchReq()))
    val l2_srcReq = Flipped(ValidIO(new SourcePrefetchReq()))
    val tlbReq = new TlbRequestIO(nRespDups = 2)
    val pmpResp = Flipped(new PMPRespBundle())
    val l1_req = DecoupledIO(new L1PrefetchReq())
    val l2_req = DecoupledIO(new L2PrefetchReq())
    val l3_req = DecoupledIO(new L3PrefetchReq())
    // val custom = new Bundle... // TODO: how to design fields
  })

  /*** data structure */
  val entries = Reg(Vec(TotalSize, new Entry()))
  val valids = RegInit(VecInit(Seq.fill(TotalSize)(false.B)))
  val tlbReqArb = Module(new FastArbiter(new TlbReq, TotalSize)) // for timing
  val l1PfIdxArb = Module(new RRArbiterInit(UInt(IndexWidth(TotalSize).W), L1Size))
  val l2PfIdxArb = Module(new RRArbiterInit(UInt(IndexWidth(TotalSize).W), L2Size))

  /*** io default */
  io.l1_req.valid := false.B
  io.l1_req.bits := DontCare
  io.l2_req.valid := false.B
  io.l2_req.bits := DontCare
  io.l3_req.valid := false.B
  io.l3_req.bits := DontCare

  /*** processing logic */
  /******************************************************************
   * Req Entry
   *  e0: entries lookup
   *  e1: update
   ******************************************************************/
  // Enqueue helper to reduce duplication
  def enqueuePart(
    partName: String, start: Int, partSize: Int, 
    srcValid: Bool, src: SourcePrefetchReq
  ): Unit = {
    // drop old prefetch when there is no invalid entry to allocate
    val replacer = ReplacementPolicy.fromString("plru", partSize)

    val e0_fire = Wire(Bool())
    val e0_srcValid = srcValid
    val e0_src = src
    val e0_selLocal = Wire(UInt(IndexWidth(partSize).W))
    val e1_fire = RegNext(e0_fire, false.B)
    val e1_src = RegEnable(src, srcValid)
    val e1_selLocal = RegEnable(e0_selLocal, e0_fire)

    val e0_matchPrev = e1_fire && e0_srcValid && getLine(e1_src.prefetchVA) === getLine(e0_src.prefetchVA)
    val e0_matchVec = sizeMap(partSize)(i => e0_srcValid && valids(start + i) && entries(start + i).vline === getLine(e0_src.prefetchVA))
    assert(PopCount(e0_matchVec) <= 1.U, s"matchVec should not have more than one match in ${this.getClass.getSimpleName}")
    val e0_allocLocal = replacer.way
    when(e0_matchPrev) {
      e0_selLocal := e1_selLocal
    }.elsewhen(e0_matchVec.orR) {
      e0_selLocal := OHToUInt(e0_matchVec)
    }.otherwise {
      e0_selLocal := e0_allocLocal
    }
    val e0_update = e0_matchPrev || e0_matchVec.orR
    e0_fire := e0_srcValid
    when(e0_fire) { replacer.access(e0_selLocal) }

    val e1_update = RegNext(e0_fire && e0_update, false.B)
    val e1_alloc = RegNext(e0_fire && !e0_update, false.B)
    val e1_selGlobal = (e1_selLocal + start.U(IndexWidth(TotalSize).W))(IndexWidth(TotalSize)-1, 0)
    when(e1_update) {
      entries(e1_selGlobal).updateEntryMerge(e1_src.prefetchTarget)
    }.elsewhen(e1_alloc) {
      entries(e1_selGlobal).fromSourcePrefetchReq(e1_src)
      valids(e1_selGlobal) := true.B
    }

    XSPerfAccumulate(s"src_req_fire_${partName}", e0_fire)
    XSPerfAccumulate(s"src_req_fire_${partName}_update", e0_fire && e0_update)
    XSPerfAccumulate(s"src_req_fire_${partName}_alloc", e0_fire && !e0_update)

    // Debug DB logging per part
    val srcTable = ChiselDB.createTable(s"${name}_${partName}SourcePrefetch${p(XSCoreParamsKey).HartId}", new SourcePrefetchReq, basicDB = true)
    srcTable.log(data = e0_src, en = e0_fire, clock = clock, reset = reset)
  }
  
  enqueuePart("l1", 0, L1Size, io.l1_srcReq.valid, io.l1_srcReq.bits)
  enqueuePart("l2", L1Size, L2Size, io.l2_srcReq.valid, io.l2_srcReq.bits)
  assert((!io.l1_srcReq.valid || io.l1_srcReq.bits.prefetchTarget === PrefetchTarget.L1.id.U) &&
         (!io.l2_srcReq.valid || io.l2_srcReq.bits.prefetchTarget === PrefetchTarget.L2.id.U), 
         "Prefetch target does not match source request port in DeltaPrefetchBuffer")

  /******************************************************************
   * tlb
   *  s0: arbiter
   *  s1: sent tlb resp
   *  s2: receive tlb resp
   *  s3: reveive pmp resp
   ******************************************************************/
  val s0_tlbFire = tlbReqArb.io.out.fire
  val s0_tlbIdx = tlbReqArb.io.chosen
  val s1_tlbIdx = RegNext(s0_tlbIdx, 0.U(IndexWidth(TotalSize).W))
  val s2_tlbIdx = RegNext(s1_tlbIdx, 0.U(IndexWidth(TotalSize).W))
  val s3_tlbIdx = RegNext(s2_tlbIdx, 0.U(IndexWidth(TotalSize).W))

  for (i <- 0 until TotalSize) {
    tlbReqArb.io.in(i).valid := valids(i) && !entries(i).pvalid && !entries(i).tlbPending
    tlbReqArb.io.in(i).bits.vaddr := entries(i).getPrefetchVA
    tlbReqArb.io.in(i).bits.cmd := TlbCmd.read
    tlbReqArb.io.in(i).bits.isPrefetch := true.B
    if (HasShadowStack) {
      tlbReqArb.io.in(i).bits.shadowStackUser.get := false.B
    }
    tlbReqArb.io.in(i).bits.size := 3.U
    tlbReqArb.io.in(i).bits.kill := false.B
    tlbReqArb.io.in(i).bits.no_translate := false.B
    tlbReqArb.io.in(i).bits.fullva := 0.U
    tlbReqArb.io.in(i).bits.checkfullva := false.B
    tlbReqArb.io.in(i).bits.memidx := DontCare
    tlbReqArb.io.in(i).bits.debug := DontCare
    tlbReqArb.io.in(i).bits.hlvx := DontCare
    tlbReqArb.io.in(i).bits.hyperinst := DontCare
    tlbReqArb.io.in(i).bits.pmp_addr := DontCare
  }
  tlbReqArb.io.out.ready := true.B

  when(s0_tlbFire){
    entries(s0_tlbIdx).updateTlbReq()
  }
  // tlb req
  val s1_tlbReqValid = RegNext(tlbReqArb.io.out.valid, false.B)
  val s1_tlbReqBits = RegEnable(tlbReqArb.io.out.bits, s0_tlbFire)
  val s1_vaddr = RegEnable(tlbReqArb.io.out.bits.vaddr, s0_tlbFire)
  io.tlbReq.req.valid := s1_tlbReqValid
  io.tlbReq.req.bits := s1_tlbReqBits
  io.tlbReq.req_kill := false.B
  // tlb resp
  val s2_tlbRespValid = io.tlbReq.resp.valid
  val s2_tlbRespBits = io.tlbReq.resp.bits
  val s2_vaddr = RegEnable(s1_vaddr, s1_tlbReqValid)
  io.tlbReq.resp.ready := true.B
  // pmp resp
  val s3_tlbRespValid = RegNext(s2_tlbRespValid, false.B)
  val s3_tlbRespBits = RegEnable(s2_tlbRespBits, s2_tlbRespValid)
  val s3_vaddr = RegEnable(s2_vaddr, s2_tlbRespValid)
  val s3_pmpResp = io.pmpResp
  val s3_updateValid = s3_tlbRespValid && !s3_tlbRespBits.miss
  val s3_updateIndex = s3_tlbIdx
  val s3_drop1 = s3_tlbRespValid && s3_tlbRespBits.miss
  val s3_drop2 = s3_updateValid && (
    // is region addr in pmem ranges
    !PmemRanges.map(_.cover(s3_tlbRespBits.paddr.head)).reduce(_ || _) ||
    // page/access fault
    s3_tlbRespBits.excp.head.pf.ld || s3_tlbRespBits.excp.head.gpf.ld || s3_tlbRespBits.excp.head.af.ld ||
    // uncache
    s3_pmpResp.mmio || Pbmt.isUncache(s3_tlbRespBits.pbmt.head) ||
    // pmp access fault
    s3_pmpResp.ld
  )
  val s3_quit = entries(s3_updateIndex).getPrefetchVA =/= s3_vaddr // overwrite by new req
  // update
  when(s3_drop1 || s3_drop2 || s3_quit) {
    when(s3_drop1 || s3_drop2) {
      valids(s3_updateIndex) := false.B
    }
  }.elsewhen(s3_updateValid) {
    entries(s3_updateIndex).updateTlbResp(s3_tlbRespBits.paddr.head)
  }

  XSPerfAccumulate("tlb_req_fire", io.tlbReq.req.fire)
  XSPerfAccumulate("tlb_resp_fire", io.tlbReq.resp.fire)
  XSPerfAccumulate("tlb_drop_miss", s3_drop1)
  XSPerfAccumulate("tlb_drop_fault", s3_drop2)
  XSPerfAccumulate("tlb_quit_overwrite", s3_quit)
  XSPerfAccumulate("tlb_succ_update", !(s3_drop1 || s3_drop2 || s3_quit) && s3_updateValid)

  /******************************************************************
   * prefetch
   ******************************************************************/
  for (i <- 0 until L1Size) {
    l1PfIdxArb.io.in(i).valid := valids(i) && entries(i).pvalid
    l1PfIdxArb.io.in(i).bits := i.U
  }
  val l1PfIdxGlobal = l1PfIdxArb.io.out.bits
  l1PfIdxArb.io.out.ready := io.l1_req.ready
  io.l1_req.valid := l1PfIdxArb.io.out.valid && entries(l1PfIdxGlobal).target === PrefetchTarget.L1.id.U
  io.l1_req.bits.paddr := entries(l1PfIdxGlobal).getPrefetchPA
  io.l1_req.bits.vaddr := entries(l1PfIdxGlobal).getPrefetchVA
  io.l1_req.bits.confidence := 1.U
  io.l1_req.bits.is_store := false.B
  io.l1_req.bits.pf_source.value := L1_HW_PREFETCH_BERTI
  when(l1PfIdxArb.io.out.fire) {
    valids(l1PfIdxGlobal) := false.B
  }

  for (i <- 0 until L2Size) {
    val idx = L1Size + i
    l2PfIdxArb.io.in(i).valid := valids(idx) && entries(idx).pvalid
    l2PfIdxArb.io.in(i).bits := idx.U
  }
  val l2PfIdxGlobal = l2PfIdxArb.io.out.bits
  l2PfIdxArb.io.out.ready := io.l2_req.ready
  io.l2_req.valid := l2PfIdxArb.io.out.valid && entries(l2PfIdxGlobal).target === PrefetchTarget.L2.id.U
  io.l2_req.bits.addr := entries(l2PfIdxGlobal).getPrefetchPA
  io.l2_req.bits.source := MemReqSource.Prefetch2L2Berti.id.U
  when(l2PfIdxArb.io.out.fire) {
    valids(l2PfIdxGlobal) := false.B
  }

  io.l3_req.valid := false.B
  io.l3_req.bits := DontCare

  XSPerfAccumulate("pf_l1_req", io.l1_req.fire)
  XSPerfAccumulate("pf_l2_req", io.l2_req.fire)
  XSPerfAccumulate("pf_l3_req", io.l3_req.fire)

  /*** performance counter and debug */
  val sendTableL1 = ChiselDB.createTable(s"${name}_l1SendPrefetch${p(XSCoreParamsKey).HartId}", new Entry, basicDB = false)
  sendTableL1.log(data = entries(l1PfIdxGlobal), en = l1PfIdxArb.io.out.valid, clock = clock, reset = reset)

  val sendTableL2 = ChiselDB.createTable(s"${name}_l2SendPrefetch${p(XSCoreParamsKey).HartId}", new Entry, basicDB = false)
  sendTableL2.log(data = entries(l2PfIdxGlobal), en = l2PfIdxArb.io.out.valid, clock = clock, reset = reset)
}

class BertiPrefetcher()(implicit p: Parameters) extends BasePrefecher with HasBertiHelper {
  override lazy val io = IO(new BertiPrefetcherIO)

  val trainFilter = Module(new TrainFilter(TRAIN_FILTER_SIZE, name, true, true))
  val historyTable = Module(new HistoryTable())
  val deltaTable = Module(new DeltaTable())
  val prefetchBuffer = Module(new DeltaPrefetchBuffer(name, PREFETCH_FILTER_SIZE, PREFETCH_FILTER_SIZE))

  // 1. train filter
  val trainValid = trainFilter.io.trainReq.valid
  val trainFire = trainFilter.io.trainReq.fire
  val trainBits = trainFilter.io.trainReq.bits
  val demandRefill = io.refillTrain.valid && isDemand(io.refillTrain.bits.metaSource)
  val demandMiss = trainFire && trainBits.miss
  val demandPfHit = trainFire && isFromL1Prefetch(trainBits.metaSource)
  trainFilter.io.enable := io.enable
  trainFilter.io.flush := false.B
  trainFilter.io.ldTrainOpt.map(_ := io.ld_in)
  trainFilter.io.stTrainOpt.map(_ := io.st_in)
  trainFilter.io.trainReq.ready := !demandRefill

  val a0_valid = RegNext(demandMiss || demandPfHit, false.B)
  val a0_pc = RegEnable(trainBits.pc, trainFire)
  val a0_vaddr = RegEnable(trainBits.vaddr, trainFire)

  val s0_valid = RegNext(demandRefill || demandPfHit, false.B)
  val s0_pc = RegEnable(Mux(demandRefill, io.refillTrain.bits.pc, trainBits.pc), demandRefill || demandPfHit)
  val s0_vaddr = RegEnable(Mux(demandRefill, io.refillTrain.bits.vaddr, trainBits.vaddr), demandRefill || demandPfHit)
  val s0_latency = RegEnable(Mux(demandRefill, io.refillTrain.bits.refillLatency, trainBits.refillLatency), demandRefill || demandPfHit)

  // 2. history table && delta
  historyTable.io.access.valid := a0_valid
  historyTable.io.access.bits.pc := a0_pc
  historyTable.io.access.bits.vaddr := a0_vaddr

  historyTable.io.search.req.valid := s0_valid
  historyTable.io.search.req.bits.pc := s0_pc
  historyTable.io.search.req.bits.vaddr := s0_vaddr
  historyTable.io.search.req.bits.latency := s0_latency

  deltaTable.io.learn := historyTable.io.search.resp

  // 3. Prefetch
  val canPrefetch = io.enable && (demandMiss || demandPfHit)
  deltaTable.io.train.valid := canPrefetch
  deltaTable.io.train.bits := trainBits
  prefetchBuffer.io.l1_srcReq := deltaTable.io.prefetch_l1
  prefetchBuffer.io.l2_srcReq := deltaTable.io.prefetch_l2

  // 4. io
  io.tlb_req <> prefetchBuffer.io.tlbReq
  prefetchBuffer.io.pmpResp := io.pmp_resp
  io.l1_req <> prefetchBuffer.io.l1_req
  io.l2_req <> prefetchBuffer.io.l2_req
  io.l3_req <> prefetchBuffer.io.l3_req

  XSPerfAccumulate("demandMiss", demandMiss)
  XSPerfAccumulate("demandPfHit", demandPfHit)
  XSPerfAccumulate("demandRefill", demandRefill)
  XSPerfAccumulate("demandRefill_searchValid", demandRefill && historyTable.io.search.resp.valid && historyTable.io.search.resp.delta =/= 0.S)
  XSPerfAccumulate("demandPfHit_searchValid", demandPfHit && historyTable.io.search.resp.valid && historyTable.io.search.resp.delta =/= 0.S)
  XSPerfAccumulate("demandMiss_prefetchValidL1", demandMiss && deltaTable.io.prefetch_l1.valid)
  XSPerfAccumulate("demandPfHit_prefetchValidL1", demandPfHit && deltaTable.io.prefetch_l1.valid)
  XSPerfAccumulate("demandMiss_prefetchValidL2", demandMiss && deltaTable.io.prefetch_l2.valid)
  XSPerfAccumulate("demandPfHit_prefetchValidL2", demandPfHit && deltaTable.io.prefetch_l2.valid)

}
