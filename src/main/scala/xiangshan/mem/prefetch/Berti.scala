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
  ht_set_cnt: Int = 8,
  ht_way_cnt: Int = 16,
  dt_way_cnt: Int = 16,
  dt_delta_size: Int = 16,
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

  def HtSetSize: Int = bertiParams.ht_set_cnt
  def DtWaySize: Int = bertiParams.dt_way_cnt
  def HtWaySize: Int = bertiParams.ht_way_cnt
  def DtDeltaSize: Int = bertiParams.dt_delta_size
  def DtDeltaIndexWidth: Int = log2Up(DtDeltaSize)

  def HtSetWidth: Int = log2Up(HtSetSize)
}

abstract class BertiBundle(implicit p: Parameters) extends XSBundle with HasBertiHelper
abstract class BertiModule(implicit p: Parameters) extends XSModule with HasBertiHelper

object DeltaStatus extends ChiselEnum {
  val NO_PREF, L1_PREF, L2_PREF, L2_PREF_REPL = Value
}

class HTSearchReq(implicit p: Parameters) extends BertiBundle {
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val latency = UInt(LATENCY_WIDTH.W)
}
class LearnDeltasLiteIO(implicit p: Parameters) extends BertiBundle {
  val valid = Bool()
  val delta = UInt(DeltaWidth.W)
  val pc = UInt(VAddrBits.W)
}

class LearnDeltasIO(implicit p: Parameters) extends BertiBundle {
  val validVec = Vec(HtWaySize, Bool())
  val deltaVec = Vec(HtWaySize, UInt(DeltaWidth.W))
  val pc = UInt(VAddrBits.W)
}

class HistoryTable()(implicit p: Parameters) extends BertiModule {
  /*** built-in function */
  def getIndex(pc: UInt): UInt = pc(HtSetWidth + PcOffsetWidth - 1, PcOffsetWidth)
  def getTag(pc: UInt): UInt = pc(HtPcTagWidth + HtSetWidth + PcOffsetWidth - 1, HtSetWidth + HtSetWidth + PcOffsetWidth)
  def getLineVAddr(vaddr: UInt): UInt = {
    vaddr(HtLineVAddrWidth + HtLineOffsetWidth - 1, HtLineOffsetWidth)
  }
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until HtWaySize).map(f))

  /*** built-in class */
  class Entry()(implicit p: Parameters) extends BertiBundle {
    val pcTag = UInt(HtPcTagWidth.W)
    val lineVAddr = UInt(HtLineVAddrWidth.W)
    val tsp = UInt(LATENCY_WIDTH.W)

    def alloc(_pcTag: UInt, _lineVAddr: UInt, _tsp: UInt): Unit = {
      pcTag := _pcTag
      lineVAddr := _lineVAddr
      tsp := _tsp
    }

    def update(_lineVAddr: UInt, _tsp: UInt): Unit = {
      lineVAddr := _lineVAddr
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
  val replacer = ReplacementPolicy.fromString("setplru", HtWaySize, HtSetSize)
  /* 
  // TODO lyq: turn replace policy to FIFO by accessPtrs and learnPtrs?
  val accessPtrs = RegInit(0.U.asTypeOf(Vec(HtSetSize, new HtWayPointer)))
  val learnPtrs = RegInit(0.U.asTypeOf(Vec(HtSetSize, new HtWayPointer)))
  */
  val currTime = GTimer()

  /*** functional function */
  def init(): Unit = {
    valids := 0.U.asTypeOf(chiselTypeOf(valids))
    // accessPtrs := 0.U.asTypeOf(chiselTypeOf(accessPtrs))
    // learnPtrs := 0.U.asTypeOf(chiselTypeOf(learnPtrs))
  }

  /**
    * A new entry is inserted in the history table (Write port in Figure 5)
    * either on-demand misses (Miss arrow from the L1D in Figure 5) or on hits
    * for prefetched cache lines (Hitp in Figure 5). The virtual address (VA) and
    * the IP (IP, VA arrow in Figure 5) are stored in the new entry along with
    * the current timestamp (not shown in the figure)
    * 
    * understand:
    *   1. tag match (here used)
    *   2. FIFO queue (?)
    * 
    * // TODO lyq: how to support multi port of access. Maybe hard due to set division
    * 
    */
  def access(pc: UInt, vaddr: UInt): Unit = {
    val set = getIndex(pc)
    val tag = getTag(pc)
    val lineVaddr = getLineVAddr(vaddr)

    val matchVec = wayMap(w => valids(set)(w) && entries(set)(w).pcTag === tag)
    assert(PopCount(matchVec) <= 1.U, s"matchVec should not have more than one match in ${this.getClass.getSimpleName}")

    when(matchVec.orR){
      val hitWay = OHToUInt(matchVec)
      entries(set)(hitWay).update(lineVaddr, currTime)
      replacer.access(set, hitWay)
    }.otherwise{
      val way = replacer.way(set)
      entries(set)(way).alloc(tag, lineVaddr, currTime)
      valids(set)(way) := true.B
    }
  }

  def searchLite(pc: UInt, vaddr: UInt, latency: UInt): LearnDeltasLiteIO = {
    val res = Wire(new LearnDeltasLiteIO)
    val set = getIndex(pc)
    val tag = getTag(pc)
    
    val matchVec = wayMap(w => valids(set)(w) && entries(set)(w).pcTag === tag)
    assert(PopCount(matchVec) <= 1.U, s"matchVec should not have more than one match in ${this.getClass.getSimpleName}")

    when(matchVec.orR){
      val hitWay = OHToUInt(matchVec)
      res.valid := latency =/= 0.U && (currTime - latency > entries(set)(hitWay).tsp)
      res.pc := pc
      res.delta := getLineVAddr(vaddr) - entries(set)(hitWay).lineVAddr
      // replacer.access(set, hitWay) // FIFO policy should not access when searching
    }.otherwise{
      res.valid := false.B
      res.pc := 0.U
      res.delta := 0.U
    }

    res
  }

  /*** processing logic */
  when(io.access.valid){
    this.access(io.access.bits.pc, io.access.bits.vaddr)
  }
  when(io.search.req.valid){
    val searchReq = io.search.req.bits
    io.search.resp := this.searchLite(searchReq.pc, searchReq.vaddr, searchReq.latency)
  }.otherwise{
    io.search.resp := DontCare
    io.search.resp.valid := false.B
  }

  /*** performance counter */
  XSPerfAccumulate("access_req", io.access.valid)
  XSPerfAccumulate("search_req", io.search.req.valid)
  XSPerfAccumulate("search_resp", io.search.resp.valid)

}

class DeltaTable()(implicit p: Parameters) extends BertiModule{
  /*** built-in function */
  def thresholdOfMax: Int = (1 << DtCntWidth) - 1
  def thresholdOfHalf: Int = (1 << (DtCntWidth - 1)) - 1
  def thresholdOfReset: Int = thresholdOfMax
  def thresholdOfUpdate: Int = thresholdOfHalf
  def thresholdOfL1PF: Int = ((1 << DtCntWidth) * 0.65).toInt
  def thresholdOfL2PF: Int = ((1 << DtCntWidth) * 0.5).toInt
  def thresholdOfL2PFR: Int = ((1 << DtCntWidth) * 0.35).toInt
  def thresholdOfReplace: Int = ((1 << DtCntWidth) * 0.5).toInt
  def getPcTag(pc: UInt): UInt = {
    val pcNoOffset = pc >> PcOffsetWidth
    val res = (pcNoOffset >> 1) ^ (pcNoOffset >> 4)
    res(DtPcTagWidth - 1, 0)
  }
  def getStatus(conf: UInt): DeltaStatus.Type = {
    val res = Wire(DeltaStatus())
    when(conf >= thresholdOfL1PF.U){
      res := DeltaStatus.L1_PREF
    }.elsewhen(conf > thresholdOfL2PF.U){
      res := DeltaStatus.L2_PREF
    }.elsewhen(conf > thresholdOfL2PFR.U) {
      res := DeltaStatus.L2_PREF_REPL
    }.otherwise{
      res := DeltaStatus.NO_PREF
    }
    res
  }

  /*** built-in class */
  class DeltaInfo()(implicit p: Parameters) extends BertiBundle {
    val delta = UInt(DeltaWidth.W)
    val coverageCnt = UInt(DtCntWidth.W)
    val status = DeltaStatus()

    def init(): Unit = {
      delta := 0.U
      coverageCnt := 0.U
      status := DeltaStatus.NO_PREF
    }

    def set(_delta: UInt): Unit = {
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
      when(counter >= thresholdOfReset.U){
        counter := 0.U
        deltaList.map(x => x.newCycle())
      }.elsewhen(counter >= thresholdOfUpdate.U){
        deltaList.map(x => x.newStatus())
      }
    }

    // TODO: use next to use this record
    // it's a little hard to record the nextDeltaCnt
    def setStatus(nextCnt: UInt, nextDeltaCnt: Seq[UInt]): Unit = {
      when(nextCnt >= thresholdOfReset.U){
        counter := 0.U
        deltaList.zip(nextDeltaCnt).map{case (x, cnt) => x.newCycle(cnt)}
      }.elsewhen(counter >= thresholdOfUpdate.U){
        deltaList.zip(nextDeltaCnt).map{case (x, cnt) => x.newStatus(cnt)}
      }
    }

    def setLite(_pcTag: UInt, _delta: UInt): Unit = {
      pcTag := _pcTag
      counter := 1.U

      (0 until DtDeltaSize).map(i => deltaList(i).init())
      deltaList(0).set(_delta)
      bestDeltaIdx := 0.U
    }

    def updateLite(_delta: UInt): Unit = {
      /**
        * 1. update
        *    1. hit: match and update
        *    2. miss: select and set
        * 2. check for status reset
        */

      assert(_delta =/= 0.U, s"delta should not be 0.U when call ${Thread.currentThread().getStackTrace()(1).getMethodName} of ${this.getClass.getSimpleName}")

      // update
      val nextCounter = counter + 1.U
      counter := nextCounter

      // delta match
      val matchVec = VecInit(deltaList.map(x => x.delta === _delta)).asUInt
      val invalidVec1 = deltaList.map(x => x.coverageCnt === 0.U)
      val invalidVec2 = deltaList.map(x => x.status === DeltaStatus.L2_PREF_REPL || x.status === DeltaStatus.NO_PREF)

      when (matchVec.orR){
        val updateIdx = OHToUInt(matchVec)
        deltaList(updateIdx).update()
        when(deltaList(updateIdx).coverageCnt >= deltaList(bestDeltaIdx).coverageCnt){
          bestDeltaIdx := updateIdx
        }
      }.otherwise{
        val (allocIdx1, canAlloc1) = PriorityEncoderWithFlag(invalidVec1)
        val (allocIdx2, canAlloc2) = PriorityEncoderWithFlag(invalidVec2)
        // It doesn't matter if allocIdx* === bestDeltaIdx, because the status is low anyway.
        when(canAlloc1) {
          deltaList(allocIdx1).set(_delta)
        }.elsewhen(canAlloc2){
          deltaList(allocIdx2).set(_delta)
        }.otherwise{
          // drop the new delta
        }
      }

      // // method 1: check here, low power but how about performance?
      // when(nextCounter  === thresholdOfReset.U){
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
  def updateLite(learn: LearnDeltasLiteIO): Unit = {
    when(learn.valid && learn.delta =/= 0.U) {
      val pcTag = getPcTag(learn.pc)
      val matchVec = VecInit((0 until DtWaySize).map(i => valids(i) && entries(i).pcTag === pcTag)).asUInt
      val hit = matchVec.orR
      when(hit) {
        val way = OHToUInt(matchVec)
        entries(way).updateLite(learn.delta)
        replacer.access(way)
      }.otherwise {
        val way = replacer.way
        entries(way).setLite(pcTag, learn.delta)
        valids(way) := true.B
      }
    }
  }

  def prefetch(train: Valid[TrainReqBundle]): Valid[SourcePrefetchReq] = {
    val res = Wire(Valid(new SourcePrefetchReq()))
    res.valid := false.B
    res.bits := DontCare
    when(train.valid){
      val pcTag = getPcTag(train.bits.pc)
      val matchOH = VecInit((0 until DtWaySize).map(i => train.valid && valids(i) && entries(i).pcTag === pcTag)).asUInt
      when(matchOH.orR){
        val way = OHToUInt(matchOH)
        replacer.access(way)
        val deltaInfo = entries(way).deltaList(entries(way).bestDeltaIdx)
        
        when(deltaInfo.status =/= DeltaStatus.NO_PREF){
          res.valid := train.valid
          res.bits.triggerPC := train.bits.pc
          res.bits.triggerVA := train.bits.vaddr
          res.bits.prefetchVA := train.bits.vaddr + deltaInfo.delta
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

    res
  }

  /*** processing logic */
  this.updateLite(io.learn)
  io.prefetch := this.prefetch(io.train)

  entries.foreach(x => x.setStatus())

  /** performance counter */
  XSPerfAccumulate("learn_req", io.learn.valid)
  XSPerfAccumulate("learn_req_0", io.learn.valid && io.learn.delta === 0.U)
  XSPerfAccumulate("learn_req_non_0", io.learn.valid && io.learn.delta =/= 0.U)
  XSPerfAccumulate("train_req", io.train.valid)
  XSPerfAccumulate("prefetch_req", io.prefetch.valid)

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
  // drop new prefetch when there is no invalid entry to allocate
  // so there is no need of replacer
  // val replacer = ReplacementPolicy.fromString("plru", size)
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
  val e1_srcValid = RegNext(io.srcReq.valid)
  val e1_src = RegEnable(io.srcReq.bits, io.srcReq.valid)
  val e1_selIdx = RegEnable(e0_selIdx, e0_fire)
  // e0
  val e0_invalidVec = sizeMap(i => !valids(i))
  val (e0_allocIdx, e0_canAlloc) = PriorityEncoderWithFlag(e0_invalidVec)
  val e0_matchPrev = e1_srcValid && e0_srcValid && getLine(e1_src.prefetchVA) === getLine(e0_src.prefetchVA)
  val e0_matchVec = sizeMap(i => e0_srcValid && valids(i) && entries(i).vline === getLine(e0_src.prefetchVA))
  assert(PopCount(e0_matchVec) <= 1.U, s"matchVec should not have more than one match in ${this.getClass.getSimpleName}")

  e0_fire := e0_srcValid && (e0_matchPrev || e0_matchVec.orR || e0_canAlloc)
  val e0_update = e0_srcValid && (e0_matchPrev || e0_matchVec.orR)
  val e0_alloc = e0_srcValid && e0_canAlloc
  when(e0_matchPrev){
    e0_selIdx := e1_selIdx
  }.elsewhen(e0_matchVec.orR){
    e0_selIdx := OHToUInt(e0_matchVec)
  }.elsewhen(e0_canAlloc){
    e0_selIdx := e0_allocIdx
  }.otherwise{
    e0_selIdx := 0.U
  }
  
  // e1
  val e1_update = RegNext(e0_update)
  val e1_alloc = RegNext(e0_alloc)
  when(e1_update){
    entries(e1_selIdx).updateEntryMerge(e1_src.prefetchTarget)
  }.elsewhen(e1_alloc){
    entries(e1_selIdx).fromSourcePrefetchReq(e1_src)
    valids(e1_selIdx) := true.B
  }
  XSPerfAccumulate("src_req_fire", e0_fire)
  XSPerfAccumulate("src_req_fire_update", e0_update)
  XSPerfAccumulate("src_req_fire_alloc", e0_alloc)

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
  val s1_tlbFireOH = RegEnable(s0_tlbFireOH, s0_tlbFire)
  val s2_tlbFireOH = RegEnable(s1_tlbFireOH, s1_tlbFire)
  val s3_tlbFireOH = RegEnable(s2_tlbFireOH, s2_tlbFire)
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
  io.tlbReq.req.valid := s1_tlbReqValid
  io.tlbReq.req.bits := s1_tlbReqBits
  io.tlbReq.req_kill := false.B
  // tlb resp
  val s2_tlbRespValid = io.tlbReq.resp.valid
  val s2_tlbRespBits = io.tlbReq.resp.bits
  io.tlbReq.resp.ready := true.B
  // pmp resp
  val s3_tlbRespValid = RegNext(s2_tlbRespValid)
  val s3_tlbRespBits = RegEnable(s2_tlbRespBits, s2_tlbRespValid)
  val s3_pmpResp = io.pmpResp
  val s3_updateValid = s3_tlbRespValid && !s3_tlbRespBits.miss
  val s3_updateIndex = OHToUInt(s3_tlbFireOH.asUInt)
  val s3_drop1 = s3_tlbRespValid && s3_tlbRespBits.miss
  val s3_drop2 = s3_updateValid && (
    // page/access fault
    s3_tlbRespBits.excp.head.pf.ld || s3_tlbRespBits.excp.head.gpf.ld || s3_tlbRespBits.excp.head.af.ld ||
    // uncache
    s3_pmpResp.mmio || Pbmt.isUncache(s3_tlbRespBits.pbmt.head) ||
    // pmp access fault
    s3_pmpResp.ld
  )
  // update
  when(s3_drop1 || s3_drop2){
    valids(s3_updateIndex) := false.B
  }.elsewhen(s3_updateValid){
    entries(s3_updateIndex).updateTlbResp(s3_tlbRespBits.paddr.head)
  }
  XSPerfAccumulate("tlb_req_fire", io.tlbReq.req.fire)
  XSPerfAccumulate("tlb_resp_fire", io.tlbReq.resp.fire)
  XSPerfAccumulate("tlb_drop_miss", s3_drop1)
  XSPerfAccumulate("tlb_drop_fault", s3_drop2)
  XSPerfAccumulate("tlb_succ_update", !(s3_drop1 || s3_drop2) && s3_updateValid)

  /******************************************************************
   * prefetch
   *  p0: arbiter and send pf req
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
  XSPerfAccumulate("pf_l1_req", io.l1_req.valid)
  XSPerfAccumulate("pf_l2_req", io.l2_req.valid)
  XSPerfAccumulate("pf_l3_req", io.l3_req.valid)
  
  /*** performance counter and debug */
  val srcTable = ChiselDB.createTable(
    "berti_source_pf_req" + p(XSCoreParamsKey).HartId.toString,
    new SourcePrefetchReq, basicDB = true
  )
  srcTable.log(
    data = e0_src,
    en = e0_fire,
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
  io.l2_req.valid := prefetchBuffer.io.l2_req.valid
  io.l2_req.bits := prefetchBuffer.io.l2_req.bits
  io.l3_req.valid := prefetchBuffer.io.l3_req.valid
  io.l3_req.bits := prefetchBuffer.io.l3_req.bits
  prefetchBuffer.io.l2_req.ready := true.B
  prefetchBuffer.io.l3_req.ready := true.B

  XSPerfAccumulate("demandMiss", demandMiss)
  XSPerfAccumulate("demandPfHit", demandPfHit)
  XSPerfAccumulate("demandRefill", demandRefill)
}
