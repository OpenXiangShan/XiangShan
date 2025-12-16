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
  ht_set_cnt: Int = 64, //8,
  ht_way_cnt: Int = 6, // 16,
  ht_replacement_policy: String = "fifo",
  dt_way_cnt: Int = 64, // 16,
  dt_delta_size: Int = 4, // 16,
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

  def PcOffsetWidth: Int = 2
  def DeltaWidth: Int = 13
  def HtPcTagWidth: Int = 7
  def HtLineVAddrWidth: Int = 24
  def HtLineOffsetWidth: Int = DCacheLineOffset
  def DtPcTagWidth: Int = 10
  def DtCntWidth: Int = 4

  def useByteAddr: Boolean = bertiParams.use_byte_addr
  def usePLRU: Boolean = bertiParams.ht_replacement_policy == "plru"
  def useFIFO: Boolean = bertiParams.ht_replacement_policy == "fifo"
  assert(usePLRU || useFIFO, s"unsupported ht replacement policy: ${bertiParams.ht_replacement_policy}")
  def HtSetSize: Int = bertiParams.ht_set_cnt
  def DtWaySize: Int = bertiParams.dt_way_cnt
  def HtWaySize: Int = bertiParams.ht_way_cnt
  def DtDeltaSize: Int = bertiParams.dt_delta_size
  def DtDeltaIndexWidth: Int = log2Up(DtDeltaSize)

  def HtSetWidth: Int = log2Up(HtSetSize)
  def DtWayWidth: Int = log2Up(DtWaySize)

  def DELTA_MIN: Int = -(1 << (DeltaWidth - 1))
  def DELTA_MAX: Int = (1 << (DeltaWidth - 1)) - 1
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

  def getPCHash(pc: UInt): UInt = (pc >> 1) ^ (pc >> 4)

  def getTrainBaseAddr(vaddr: UInt): UInt = {
    if (useByteAddr) {
      vaddr
    } else {
      _getLineVAddr(vaddr)
    }
  }

  def getPrefetchVAddr(triggerVA: UInt, delta: SInt): UInt = {
    if (useByteAddr) {
      triggerVA + _signedExtend(delta.asUInt, VAddrBits)
    } else {
      triggerVA + _signedExtend((delta.asUInt << HtLineOffsetWidth), VAddrBits)
    }
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
  val validVec = Vec(HtWaySize, Bool())
  val deltaVec = Vec(HtWaySize, SInt(DeltaWidth.W))
  val pc = UInt(VAddrBits.W)
}

class HistoryTable()(implicit p: Parameters) extends BertiModule {
  /*** static variable ***/
  val stat_find_delta = WireInit(false.B)
  val stat_overflow = WireInit(false.B)
  val stat_satisfy = WireInit(false.B)
  val stat_dissatisfy = WireInit(false.B)
  val stat_histLineVA = WireInit(0.U(HtLineVAddrWidth.W))
  val stat_currLineVA = WireInit(0.U(HtLineVAddrWidth.W))
  /*** built-in function */
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until HtWaySize).map(f))
  def getIndex(pc: UInt): UInt = getPCHash(pc)(HtSetWidth-1, 0)
  def getTag(pc: UInt): UInt = getPCHash(pc)(HtPcTagWidth + HtSetWidth - 1, HtSetWidth)
  def getTrainBaseAddr2HT(vaddr: UInt): UInt = {
    getTrainBaseAddr(vaddr)(HtLineVAddrWidth - 1, 0)
  }
  def checkDissatisfy(delta: SInt): Bool = {
    delta < (DELTA_THRESHOLD).S(DeltaWidth.W) && delta > (-DELTA_THRESHOLD).S(DeltaWidth.W)
  }
  def getDelta(lineVA1: UInt, lineVA2: UInt): (Bool, SInt) = {
    // here should handle the overflow
    val diffFull = (lineVA1.zext - lineVA2.zext).asSInt
    val overflow = diffFull < DELTA_MIN.S(DeltaWidth.W) || diffFull > DELTA_MAX.S(DeltaWidth.W)
    val dissatisfy = checkDissatisfy(diffFull)
    stat_overflow := overflow
    stat_dissatisfy := dissatisfy
    stat_satisfy := !overflow && !dissatisfy
    stat_currLineVA := lineVA1
    stat_histLineVA := lineVA2
    (stat_satisfy, diffFull)
  }

  /*** built-in class */
  class Entry()(implicit p: Parameters) extends BertiBundle {
    val pcTag = UInt(HtPcTagWidth.W)
    val baseVAddr = UInt(HtLineVAddrWidth.W)
    val tsp = UInt(LATENCY_WIDTH.W)

    def alloc(_pcTag: UInt, _baseVAddr: UInt, _tsp: UInt): Unit = {
      pcTag := _pcTag
      baseVAddr := _baseVAddr
      tsp := _tsp
    }

    def update(_baseVAddr: UInt, _tsp: UInt): Unit = {
      baseVAddr := _baseVAddr
      tsp := _tsp
    }
  }

  class HtWayPointer(implicit p: Parameters) extends CircularQueuePtr[HtWayPointer](HtWaySize) {}

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
  val entries = Reg(Vec(HtSetSize, Vec(HtWaySize, new Entry)))
  val valids = RegInit(0.U.asTypeOf(Vec(HtSetSize, Vec(HtWaySize, Bool()))))
  val decrModes = RegInit(0.U.asTypeOf(Vec(HtSetSize, Bool())))
  val currTime = GTimer()
  // PLRU: replace record
  val replacer = Option.when(usePLRU)(ReplacementPolicy.fromString("setplru", HtWaySize, HtSetSize))
  // FIFO: for FIFO replace policy
  val accessPtrs = Option.when(useFIFO)(RegInit(0.U.asTypeOf(Vec(HtSetSize, new HtWayPointer))))
  // FIFO: for easier learning policy
  val learnPtrs = Option.when(useFIFO)(RegInit(0.U.asTypeOf(Vec(HtSetSize, new HtWayPointer))))

  /*** functional function */
  def init(): Unit = {
    valids := 0.U.asTypeOf(chiselTypeOf(valids))
    accessPtrs.foreach(_ := 0.U.asTypeOf(chiselTypeOf(accessPtrs.head)))
    learnPtrs.foreach(_ := 0.U.asTypeOf(chiselTypeOf(learnPtrs.head)))
  }

  /**
    * A new entry is inserted in the history table (Write port in Figure 5)
    * either on-demand misses (Miss arrow from the L1D in Figure 5) or on hits
    * for prefetched cache lines (Hitp in Figure 5). The virtual address (VA) and
    * the IP (IP, VA arrow in Figure 5) are stored in the new entry along with
    * the current timestamp (not shown in the figure)
    * 
    * understand:
    *   1. tag match
    *   2. FIFO queue (here used)
    * 
    * // TODO lyq:
    *   How to support multi port of access for both historyTable and deltaTable.
    *   Maybe hard due to set division.
    * 
    */
  def accessPLRU(pc: UInt, vaddr: UInt): Bool = {
    // ensure option exists in this code path
    require(replacer.isDefined, "PLRU replacer must be defined when usePLRU == true")
    val isReplace = Wire(Bool())
    val set = getIndex(pc)
    val tag = getTag(pc)
    val baseVAddr = getTrainBaseAddr2HT(vaddr)
    val matchVec = wayMap(w => valids(set)(w) && entries(set)(w).pcTag === tag)
    assert(PopCount(matchVec) <= 1.U, s"matchVec should not have more than one match in ${this.getClass.getSimpleName}")
    when(matchVec.orR){
      val hitWay = OHToUInt(matchVec)
      entries(set)(hitWay).update(baseVAddr, currTime)
      replacer.get.access(set, hitWay)
      isReplace := false.B
    }.otherwise{
      val way = replacer.get.way(set)
      entries(set)(way).alloc(tag, baseVAddr, currTime)
      valids(set)(way) := true.B
      isReplace := true.B
    }
    isReplace
  }

  def searchLitePLRU(pc: UInt, vaddr: UInt, latency: UInt): LearnDeltasLiteIO = {
    // ensure option exists in this code path
    require(replacer.isDefined, "PLRU replacer must be defined when usePLRU == true")
    val res = Wire(new LearnDeltasLiteIO)
    val set = getIndex(pc)
    val tag = getTag(pc)
    val matchVec = wayMap(w => valids(set)(w) && entries(set)(w).pcTag === tag)
    assert(PopCount(matchVec) <= 1.U, s"matchVec should not have more than one match in ${this.getClass.getSimpleName}")
    when(matchVec.orR){
      val hitWay = OHToUInt(matchVec)
      val pair = getDelta(getTrainBaseAddr2HT(vaddr), entries(set)(hitWay).baseVAddr)
      stat_find_delta := latency =/= 0.U && (currTime - latency > entries(set)(hitWay).tsp)
      res.valid := latency =/= 0.U && (currTime - latency > entries(set)(hitWay).tsp) && pair._1
      res.pc := pc
      res.delta := pair._2
      replacer.get.access(set, hitWay)
    }.otherwise{
      res.valid := false.B
      res.pc := 0.U
      res.delta := 0.S
    }
    res
  }

  def accessFIFO(pc: UInt, vaddr: UInt): Bool = {
    // ensure option exists in this code path
    require(accessPtrs.isDefined, "accessPtrs must be defined when useFIFO == true")
    val isReplace = Wire(Bool())
    val set = getIndex(pc)
    val way = accessPtrs.get(set).value
    val baseVAddr = getTrainBaseAddr2HT(vaddr)
    val matchVec = wayMap(w => valids(set)(w) && entries(set)(w).baseVAddr === baseVAddr)
    when(matchVec.orR){
      isReplace := false.B
    }.otherwise {
      isReplace := valids(set)(way)
      val lastWay = (accessPtrs.get(set)-1.U).value
      decrModes(set) := valids(set)(lastWay) && baseVAddr < entries(set)(lastWay).baseVAddr
      valids(set)(way) := true.B
      entries(set)(way).alloc(
        getTag(pc),
        baseVAddr,
        currTime
      )
      accessPtrs.get(set) := accessPtrs.get(set) + 1.U
    }
    isReplace
  }

  def searchLiteFIFO(pc: UInt, vaddr: UInt, latency: UInt): LearnDeltasLiteIO = {
    // ensure option exists in this code path
    require(learnPtrs.isDefined, "learnPtrs must be defined when useFIFO == true")
    val res = Wire(new LearnDeltasLiteIO)
    val set = getIndex(pc)
    val tag = getTag(pc)
    val way = learnPtrs.get(set).value
    val pair = getDelta(getTrainBaseAddr2HT(vaddr), entries(set)(way).baseVAddr)
    stat_find_delta := valids(set)(way) && latency =/= 0.U && tag === entries(set)(way).pcTag && (currTime - latency > entries(set)(way).tsp)
    res.pc := pc
    res.valid := stat_find_delta && pair._1
    when (decrModes(set) ^ pair._2(pair._2.getWidth-1)){
      res.delta := -pair._2
    }.otherwise{
      res.delta := pair._2
    }
    learnPtrs.get(set) := learnPtrs.get(set) + 1.U
    res
  }

  def access(pc: UInt, vaddr: UInt): Bool = {
    if (usePLRU) accessPLRU(pc, vaddr) else accessFIFO(pc, vaddr)
  }
  def searchLite(pc: UInt, vaddr: UInt, latency: UInt): LearnDeltasLiteIO = {
    if (usePLRU) searchLitePLRU(pc, vaddr, latency) else searchLiteFIFO(pc, vaddr, latency)
  }

  /*** processing logic */
  val isReplace = Wire(Bool())
  when(io.access.valid){
    isReplace := this.access(io.access.bits.pc, io.access.bits.vaddr)
  }.otherwise{
    isReplace := false.B
  }

  val searchResult = Wire(new LearnDeltasLiteIO)
  when(io.search.req.valid){
    val searchReq = io.search.req.bits
    searchResult := this.searchLite(searchReq.pc, searchReq.vaddr, searchReq.latency)
  }.otherwise{
    searchResult := DontCare
    searchResult.valid := false.B
  }
  io.search.resp := searchResult
  io.search.resp.valid := searchResult.valid && !checkDissatisfy(searchResult.delta)

  /*** performance counter */
  XSPerfAccumulate("access_req", io.access.valid)
  XSPerfAccumulate("access_replace", io.access.valid && isReplace)
  XSPerfAccumulate("search_req", io.search.req.valid)
  XSPerfAccumulate("search_resp_valid", io.search.resp.valid)
  XSPerfAccumulate("search_resp_find_total", searchResult.valid)
  XSPerfAccumulate("search_resp_find_overflow", searchResult.valid && stat_overflow)
  XSPerfAccumulate("search_resp_find_dissatisfy", searchResult.valid && stat_dissatisfy)
  XSPerfAccumulate("search_resp_find_satisfy", searchResult.valid && stat_satisfy)

  class SearchLogDb extends Bundle {
    val histLineVA = UInt(HtLineVAddrWidth.W)
    val currLineVA = UInt(HtLineVAddrWidth.W)
    val calDelta = UInt(DeltaWidth.W)
  }
  val searchLog = Wire(new SearchLogDb())
  searchLog.histLineVA := stat_histLineVA
  searchLog.currLineVA := stat_currLineVA
  searchLog.calDelta := io.search.resp.delta.asUInt
  val searchLogDb = ChiselDB.createTable("berti_searchLog" + p(XSCoreParamsKey).HartId.toString, new SearchLogDb, basicDB = false)
  searchLogDb.log(data = searchLog, en = io.search.resp.valid, clock = clock, reset = reset)
}

class DeltaTable()(implicit p: Parameters) extends BertiModule {
  val stat_update_isEntryHit = WireInit(false.B)
  val stat_update_isEntryMiss = WireInit(false.B)
  val stat_update_isEntryReplace = WireInit(false.B)
  val stat_update_isDeltaHit = WireInit(false.B)
  val stat_update_isDeltaMiss = WireInit(false.B)
  val stat_update_isDeltaReplace = WireInit(false.B)
  val stat_update_evictEntryIdx = WireInit(0.U(DtWayWidth.W)) // TODO lyq: if have chiselMap, it may be eaiser to statistic evicted data
  val stat_update_evictDelta = WireInit(0.S(DeltaWidth.W)) // TODO lyq: have no idea how to output this
  val stat_prefetch_isEntryHit = WireInit(false.B)
  /*** built-in function */
  // def thresholdOfReset: UInt = 16.U 
  // def thresholdOfUpdate: UInt = 10.U 
  // def thresholdOfL1PF: UInt = 8.U 
  // def thresholdOfL2PF: UInt = 5.U 
  // def thresholdOfL2PFR: UInt = 2.U 
  val thresholdOfReset = Constantin.createRecord(_name+"_thresholdOfReset", 6)    // (1 << DtCntWidth) - 1
  val thresholdOfUpdate = Constantin.createRecord(_name+"_thresholdOfUpdate", 2)  // (1 << (DtCntWidth - 1))
  val thresholdOfL1PF = Constantin.createRecord(_name+"_thresholdOfL1PF", 4)      // ((1 << DtCntWidth) * 0.65).toInt
  val thresholdOfL2PF = Constantin.createRecord(_name+"_thresholdOfL2PF", 2)      // ((1 << DtCntWidth) * 0.5).toInt
  val thresholdOfL2PFR = Constantin.createRecord(_name+"_thresholdOfL2PFR", 1)    // ((1 << DtCntWidth) * 0.35).toInt
  def getPcTag(pc: UInt): UInt = {
    val res = getPCHash(pc)
    res(DtPcTagWidth - 1, 0)
  }
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

      when (matchVec.orR){
        val updateIdx = OHToUInt(matchVec)
        deltaList(updateIdx).update()
        when(deltaList(updateIdx).isGreaterThan(deltaList(bestDeltaIdx))){
          bestDeltaIdx := updateIdx
        }
        stat_update_isDeltaHit := true.B
      }.otherwise{
        stat_update_isDeltaMiss := true.B
        val (allocIdx1, canAlloc1) = PriorityEncoderWithFlag(invalidVec1)
        val (allocIdx2, canAlloc2) = PriorityEncoderWithFlag(invalidVec2)
        val (allocIdx3, canAlloc3) = PriorityEncoderWithFlag(invalidVec3)
        // It doesn't matter if allocIdx* === bestDeltaIdx, because the status is low anyway.
        when(canAlloc1) {
          deltaList(allocIdx1).set(_delta)
          stat_update_isDeltaReplace := true.B
          stat_update_evictDelta := deltaList(allocIdx1).delta
        }.elsewhen(canAlloc2){
          deltaList(allocIdx2).set(_delta)
          stat_update_isDeltaReplace := true.B
          stat_update_evictDelta := deltaList(allocIdx2).delta
        }.elsewhen(canAlloc3){
          deltaList(allocIdx3).set(_delta)
          stat_update_isDeltaReplace := true.B
          stat_update_evictDelta := deltaList(allocIdx3).delta
        }.otherwise{
          // drop the new delta
        }
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
    val prefetch = ValidIO(new SourcePrefetchReq())
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

  /*** functional function */
  def update(learn: LearnDeltasIO): Unit = {
    // TODO lyq: how to update when having multiple deltas?
    assert(false, "not implemented yet")
  }

  def updateLite(learn: LearnDeltasLiteIO): Unit = {
    when(learn.valid && learn.delta =/= 0.S) {
      val pcTag = getPcTag(learn.pc)
      val matchVec = VecInit((0 until DtWaySize).map(i => valids(i) && entries(i).pcTag === pcTag)).asUInt
      val hit = matchVec.orR
      when(!hit) {
        val way = replacer.way
        entries(way).setLite(pcTag, learn.delta)
        valids(way) := true.B
        stat_update_isEntryMiss := true.B
        stat_update_isEntryReplace := true.B
        stat_update_evictEntryIdx := way
      }.otherwise {
        val way = OHToUInt(matchVec)
        entries(way).updateLite(learn.delta)
        replacer.access(way)
        stat_update_isEntryHit := true.B
      }
    }
  }

  def prefetch(train: Valid[TrainReqBundle]): (Valid[SourcePrefetchReq], DeltaInfo) = {
    val res = Wire(Valid(new SourcePrefetchReq()))
    val deltaInfo = WireInit(0.U.asTypeOf(new DeltaInfo()))
    res.valid := false.B
    res.bits := DontCare
    when(train.valid){
      val pcTag = getPcTag(train.bits.pc)
      val matchOH = VecInit((0 until DtWaySize).map(i => train.valid && valids(i) && entries(i).pcTag === pcTag)).asUInt
      when(matchOH.orR){
        stat_prefetch_isEntryHit := true.B
        val way = OHToUInt(matchOH)
        replacer.access(way)
        deltaInfo := entries(way).deltaList(entries(way).bestDeltaIdx)
        
        when(deltaInfo.status =/= DeltaStatus.NO_PREF){
          res.valid := train.valid
          res.bits.triggerPC := train.bits.pc
          res.bits.triggerVA := train.bits.vaddr
          res.bits.prefetchVA := getPrefetchVAddr(train.bits.vaddr, deltaInfo.delta)
          when(deltaInfo.status === DeltaStatus.L1_PREF) {
            res.bits.prefetchTarget := PrefetchTarget.L1.id.U
          }.elsewhen(deltaInfo.status === DeltaStatus.L2_PREF || deltaInfo.status === DeltaStatus.L2_PREF_REPL){
            res.bits.prefetchTarget := PrefetchTarget.L2.id.U
          }.otherwise{
            res.bits.prefetchTarget := PrefetchTarget.L3.id.U
          }
        }
      }
    }

    (res, deltaInfo)
  }

  /*** processing logic */
  entries.foreach(x => x.setStatus())
  this.updateLite(io.learn)
  val pfRes = this.prefetch(io.train)
  io.prefetch := pfRes._1
  val deltaInfo = WireInit(0.U.asTypeOf(new DeltaInfo()))

  /** performance counter */
  class DeltaInfo2Db extends Bundle {
    val delta = UInt(DeltaWidth.W)
    val coverageCnt = UInt(DtCntWidth.W)
    val status = UInt(2.W)
  }
  val deltaInfo2Db = Wire(new DeltaInfo2Db())
  deltaInfo2Db.delta := pfRes._2.delta.asUInt
  deltaInfo2Db.coverageCnt := pfRes._2.coverageCnt
  deltaInfo2Db.status := pfRes._2.status.asUInt
  val prefetchDeltaTable = ChiselDB.createTable("berti_prefetchDeltaTable" + p(XSCoreParamsKey).HartId.toString, new DeltaInfo2Db, basicDB = false)
  prefetchDeltaTable.log(data = deltaInfo2Db, en = io.prefetch.valid, clock = clock, reset = reset)
  
  XSPerfAccumulate("learn_req", io.learn.valid)
  XSPerfAccumulate("learn_req_0", io.learn.valid && io.learn.delta === 0.S)
  XSPerfAccumulate("learn_req_non_0", io.learn.valid && io.learn.delta =/= 0.S)
  XSPerfAccumulate("train_req", io.train.valid)
  XSPerfAccumulate("prefetch_req", io.prefetch.valid)
  XSPerfAccumulate("stat_update_isEntryHit", stat_update_isEntryHit)
  XSPerfAccumulate("stat_update_isEntryMiss", stat_update_isEntryMiss)
  XSPerfAccumulate("stat_update_isEntryReplace", stat_update_isEntryReplace)
  XSPerfAccumulate("stat_update_isDeltaHit", stat_update_isDeltaHit)
  XSPerfAccumulate("stat_update_isDeltaMiss", stat_update_isDeltaMiss)
  XSPerfAccumulate("stat_update_isDeltaReplace", stat_update_isDeltaReplace)
  XSPerfAccumulate("stat_prefetch_isEntryHit", stat_prefetch_isEntryHit)

}

class DeltaPrefetchBuffer(size: Int, name: String)(implicit p: Parameters) extends DCacheModule {
  /*** built-in function */
  /**
    *    Address Struture and Internal Statement
    *
    * [[HasDCacheParameters]]
    * |        page       (alias)|  page offset    | @physical
    * |        ptag              |  page offset    | @physical
    * |        vtag       | set    | bank | offset | @virtual
    * |        line                |   line offset | @virtual
    * [[HasL1CacheParameters]]
    * |        block               |  block offset | @virtual
    * |        vtag       | idx    | word | offset | @virtual
    *
    */
  def BufferIndexWidth: Int = log2Up(size)
  def LineOffsetWidth: Int = DCacheLineOffset
  def VLineWidth: Int = VAddrBits - LineOffsetWidth
  def PLineWidth: Int = PAddrBits - LineOffsetWidth
  def getLine(addr: UInt): UInt = addr(addr.getWidth - 1, LineOffsetWidth)
  def sizeMap[T <: Data](f: Int => T) = VecInit((0 until size).map(f))

  /*** built-in class */
  class Entry()(implicit p: Parameters) extends DCacheBundle {
    val vline = UInt(VLineWidth.W)
    val pline = UInt(PLineWidth.W)
    val pvalid = Bool()
    val target = UInt(PrefetchTarget.PfTgtBits.W)

    def getPrefetchVA: UInt = Cat(vline, 0.U(LineOffsetWidth.W))
    def getPrefetchPA: UInt = Cat(pline, 0.U(LineOffsetWidth.W))
    def getPrefetchAlias: UInt = get_alias(getPrefetchPA)
    def fromSourcePrefetchReq(src: SourcePrefetchReq): Unit ={
      this.vline := getLine(src.prefetchVA)
      this.pline := 0.U
      this.pvalid := false.B
      this.target := src.prefetchTarget
    }
    def updateEntryMerge(target: UInt): Unit = {
      when(target < this.target){
        this.target := target
      }
    }
    def updateTlbResp(paddr: UInt): Unit = {
      this.pline := getLine(paddr)
      this.pvalid := true.B
    }
  }

  /*** io */
  val io = IO(new Bundle{
    val srcReq = Flipped(ValidIO(new SourcePrefetchReq()))
    val tlbReq = new TlbRequestIO(nRespDups = 2)
    val pmpResp = Flipped(new PMPRespBundle())
    val l1_req = DecoupledIO(new L1PrefetchReq())
    val l2_req = DecoupledIO(new L2PrefetchReq())
    val l3_req = DecoupledIO(new L3PrefetchReq())
    // val custom = new Bundle... // TODO: how to design fields
  })

  /*** data structure */
  val entries = Reg(Vec(size, new Entry()))
  val valids = RegInit(0.U.asTypeOf(Vec(size, Bool())))
  // drop old prefetch when there is no invalid entry to allocate
  val replacer = ReplacementPolicy.fromString("plru", size)
  val tlbReqArb = Module(new RRArbiterInit(new TlbReq, size))
  val pfIdxArb = Module(new RRArbiterInit(UInt(BufferIndexWidth.W), size))
  
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
  // predefine
  val e0_fire = Wire(Bool())
  val e0_srcValid = io.srcReq.valid
  val e0_src = io.srcReq.bits
  val e0_selIdx = Wire(UInt(BufferIndexWidth.W))
  val e1_fire = RegNext(e0_fire)
  val e1_src = RegEnable(io.srcReq.bits, io.srcReq.valid)
  val e1_selIdx = RegEnable(e0_selIdx, e0_fire)
  // e0
  val e0_matchPrev = e1_fire && e0_srcValid && getLine(e1_src.prefetchVA) === getLine(e0_src.prefetchVA)
  val e0_matchVec = sizeMap(i => e0_srcValid && valids(i) && entries(i).vline === getLine(e0_src.prefetchVA))
  assert(PopCount(e0_matchVec) <= 1.U, s"matchVec should not have more than one match in ${this.getClass.getSimpleName}")
  val e0_allocIdx = replacer.way
  when(e0_matchPrev){
    e0_selIdx := e1_selIdx
  }.elsewhen(e0_matchVec.orR){
    e0_selIdx := OHToUInt(e0_matchVec)
  }.otherwise{
    e0_selIdx := e0_allocIdx
  }
  val e0_update =  e0_matchPrev || e0_matchVec.orR
  e0_fire := e0_srcValid
  when(e0_fire){
    replacer.access(e0_selIdx)
  }

  // e1
  val e1_update = RegNext(e0_fire && e0_update)
  val e1_alloc = RegNext(e0_fire && !e0_update)
  when(e1_update){
    entries(e1_selIdx).updateEntryMerge(e1_src.prefetchTarget)
  }.elsewhen(e1_alloc){
    entries(e1_selIdx).fromSourcePrefetchReq(e1_src)
    valids(e1_selIdx) := true.B
  }
  XSPerfAccumulate("src_req_fire", e0_fire)
  XSPerfAccumulate("src_req_fire_update", e0_fire && e0_update)
  XSPerfAccumulate("src_req_fire_alloc", e0_fire && !e0_update)

  /******************************************************************
   * tlb
   *  s0: arbiter
   *  s1: sent tlb resp
   *  s2: receive tlb resp
   *  s3: reveive pmp resp
   ******************************************************************/
  val s0_tlbFireOH = VecInit(tlbReqArb.io.in.map(_.fire))
  // control
  val s0_tlbFire = s0_tlbFireOH.orR
  val s1_tlbFire = RegNext(s0_tlbFire)
  val s2_tlbFire = RegNext(s1_tlbFire)
  // data
  val s1_tlbFireOH = RegEnable(s0_tlbFireOH, 0.U.asTypeOf(s0_tlbFireOH), s0_tlbFire)
  val s2_tlbFireOH = RegEnable(s1_tlbFireOH, 0.U.asTypeOf(s0_tlbFireOH), s1_tlbFire)
  val s3_tlbFireOH = RegEnable(s2_tlbFireOH, 0.U.asTypeOf(s0_tlbFireOH), s2_tlbFire)
  val s0_notSelectOH = sizeMap(i => !s1_tlbFireOH(i) && !s2_tlbFireOH(i) && !s3_tlbFireOH(i))
  for(i <- 0 until size) {
    tlbReqArb.io.in(i).valid := valids(i) && !entries(i).pvalid && s0_notSelectOH(i)
    tlbReqArb.io.in(i).bits.vaddr := entries(i).getPrefetchVA
    tlbReqArb.io.in(i).bits.cmd := TlbCmd.read
    tlbReqArb.io.in(i).bits.isPrefetch := true.B
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
  // tlb req
  val s1_tlbReqValid = RegNext(tlbReqArb.io.out.valid)
  val s1_tlbReqBits = RegEnable(tlbReqArb.io.out.bits, tlbReqArb.io.out.valid)
  val s1_vaddr = RegEnable(tlbReqArb.io.out.bits.vaddr, tlbReqArb.io.out.valid)
  io.tlbReq.req.valid := s1_tlbReqValid
  io.tlbReq.req.bits := s1_tlbReqBits
  io.tlbReq.req_kill := false.B
  // tlb resp
  val s2_tlbRespValid = io.tlbReq.resp.valid
  val s2_tlbRespBits = io.tlbReq.resp.bits
  val s2_vaddr = RegEnable(s1_vaddr, s1_tlbReqValid)
  io.tlbReq.resp.ready := true.B
  // pmp resp
  val s3_tlbRespValid = RegNext(s2_tlbRespValid)
  val s3_tlbRespBits = RegEnable(s2_tlbRespBits, s2_tlbRespValid)
  val s3_vaddr = RegEnable(s2_vaddr, s2_tlbRespValid)
  val s3_pmpResp = io.pmpResp
  val s3_updateValid = s3_tlbRespValid && !s3_tlbRespBits.miss
  val s3_updateIndex = OHToUInt(s3_tlbFireOH.asUInt)
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
  when(s3_drop1 || s3_drop2 || s3_quit){
    when(s3_drop1 || s3_drop2){
      valids(s3_updateIndex) := false.B
    }
  }.elsewhen(s3_updateValid){
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
   *  p0: arbiter and send pf req
   * 
   * TODO: prefetch may not ready, how about setting replay counter?
   ******************************************************************/
  for(i <- 0 until size){
    pfIdxArb.io.in(i).valid := valids(i) && entries(i).pvalid
    pfIdxArb.io.in(i).bits := i.U
  }
  val pfIdx = pfIdxArb.io.out.bits
  pfIdxArb.io.out.ready := true.B
  switch(entries(pfIdx).target){
    is(PrefetchTarget.L1.id.U){
      pfIdxArb.io.out.ready := io.l1_req.ready
      io.l1_req.valid := pfIdxArb.io.out.valid
      io.l1_req.bits.paddr := entries(pfIdx).getPrefetchPA
      io.l1_req.bits.alias := entries(pfIdx).getPrefetchAlias
      io.l1_req.bits.confidence := 1.U
      io.l1_req.bits.is_store := false.B
      io.l1_req.bits.pf_source.value := L1_HW_PREFETCH_BERTI
    }
    is(PrefetchTarget.L2.id.U){
      pfIdxArb.io.out.ready := io.l2_req.ready
      io.l2_req.valid := pfIdxArb.io.out.valid
      io.l2_req.bits.addr := entries(pfIdx).getPrefetchPA
      io.l2_req.bits.source := MemReqSource.Prefetch2L2Berti.id.U
    }
    is(PrefetchTarget.L3.id.U){
      pfIdxArb.io.out.ready := io.l3_req.ready
      io.l3_req.valid := pfIdxArb.io.out.valid
      io.l3_req.bits.addr := entries(pfIdx).getPrefetchPA
      io.l3_req.bits.source := MemReqSource.Prefetch2L3Berti.id.U
    }
  }
  // update
  when(pfIdxArb.io.out.fire){
    valids(pfIdx) := false.B
  }
  XSPerfAccumulate("pf_l1_req", io.l1_req.fire)
  XSPerfAccumulate("pf_l2_req", io.l2_req.fire)
  XSPerfAccumulate("pf_l3_req", io.l3_req.fire)
  
  /*** performance counter and debug */
  val srcTable = ChiselDB.createTable(
    "berti_source_pf_req" + p(XSCoreParamsKey).HartId.toString,
    new SourcePrefetchReq, basicDB = false
  )
  srcTable.log(
    data = e0_src,
    en = e0_fire,
    clock = clock,
    reset = reset
  )

  val sendTable = ChiselDB.createTable(
    "berti_send_pf_req" + p(XSCoreParamsKey).HartId.toString,
    new Entry, basicDB = false
  )
  sendTable.log(
    data = entries(pfIdx),
    en = pfIdxArb.io.out.valid,
    clock = clock,
    reset = reset
  )
}

class BertiPrefetcher()(implicit p: Parameters) extends BasePrefecher with HasBertiHelper {
  override lazy val io = IO(new BertiPrefetcherIO)

  val trainFilter = Module(new NewTrainFilter(TRAIN_FILTER_SIZE, name, true, true))
  val historyTable = Module(new HistoryTable())
  val detlaTable = Module(new DeltaTable())
  val prefetchBuffer = Module(new DeltaPrefetchBuffer(PREFETCH_FILTER_SIZE, name))

  // 1. train filter
  val demandRefill = io.refillTrain.valid && isDemand(io.refillTrain.bits.metaSource)
  trainFilter.io.enable := io.enable
  trainFilter.io.flush := false.B
  trainFilter.io.ldTrainOpt.map(_ := io.ld_in)
  trainFilter.io.stTrainOpt.map(_ := io.st_in)
  trainFilter.io.trainReq.ready := !demandRefill

  // 2. history table && delta
  val trainValid = trainFilter.io.trainReq.valid
  val trainBits = trainFilter.io.trainReq.bits
  val demandMiss = trainValid && trainBits.miss
  val demandPfHit = trainValid && isFromBerti(trainBits.metaSource)

  historyTable.io.access.valid := demandMiss || demandPfHit
  historyTable.io.access.bits.pc := trainBits.pc
  historyTable.io.access.bits.vaddr := trainBits.vaddr

  historyTable.io.search.req.valid := demandRefill || demandPfHit
  historyTable.io.search.req.bits.pc := Mux(demandRefill, io.refillTrain.bits.pc, trainBits.pc)
  historyTable.io.search.req.bits.vaddr := Mux(demandRefill, io.refillTrain.bits.vaddr, trainBits.vaddr)
  historyTable.io.search.req.bits.latency := Mux(demandRefill, io.refillTrain.bits.refillLatency, trainBits.refillLatency)

  detlaTable.io.learn := historyTable.io.search.resp

  // 3. Prefetch
  val canPrefetch = io.enable && (demandMiss || demandPfHit)
  detlaTable.io.train.valid := canPrefetch
  detlaTable.io.train.bits := trainBits
  prefetchBuffer.io.srcReq := detlaTable.io.prefetch

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
  XSPerfAccumulate("demandMiss_prefetchValid", demandMiss && detlaTable.io.prefetch.valid)
  XSPerfAccumulate("demandPfHit_prefetchValid", demandPfHit && detlaTable.io.prefetch.valid)

}
