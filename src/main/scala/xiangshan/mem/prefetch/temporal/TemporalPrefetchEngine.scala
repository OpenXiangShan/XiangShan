/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 *
 *
 * Acknowledgement
 *
 * This implementation is inspired by several key papers:
 * [1] Hao Wu, Krishnendra Nathella, Joseph Pusdesris, Dam Sunwoo, Akanksha Jain, and Calvin Lin. "[Temporal
 * Prefetching Without the Off-Chip Metadata.](https://doi.org/10.1145/3352460.3358300)" 52nd Annual IEEE/ACM
 * International Symposium on Microarchitecture (MICRO). 2019.
 * [2] Sam Ainsworth, and Lev Mukhanov. "[Triangel: A High-Performance, Accurate, Timely On-Chip Temporal Prefetcher.]
 * (https://doi.org/10.1109/ISCA59077.2024.00090)" ACM/IEEE 51st Annual International Symposium on Computer
 * Architecture (ISCA). 2024.
 * *************************************************************************************
 */

package xiangshan.mem.prefetch.temporal

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, XSPerfAccumulate, XSPerfHistogram}
import utility.sram.SRAMTemplate
import coupledL2.utils.SetAssocReplacer
import xiangshan.{XSBundle, XSModule}
import xiangshan.cache.HasDCacheParameters
import xiangshan.mem.prefetch.TemporalPrefetcherParams

// Ported from coupledL2 TemporalPrefetch.scala at 1bdc4b86; all state is private to L1.
trait HasTPParams extends HasDCacheParameters {
  lazy val tpParams = prefetcherSeq.collectFirst { case t: TemporalPrefetcherParams => t }.get
  private lazy val tpPhysicalAddressBits = PAddrBits
  // Store the virtual alias next to each physical address, including successors on other pages.
  def fullAddressBits = tpPhysicalAddressBits + 2
  def offsetBits = blockOffBits
  def hitCountWidth = 3
  def tpTableAssoc = tpParams.tpTableAssoc
  def tpTableNrSet = tpParams.tpTableEntries / tpTableAssoc
  def tpTableSetBits = log2Ceil(tpTableNrSet)
  def tpEntryMaxLen = 512 / (tpPhysicalAddressBits - offsetBits)
  def tpEntryLenBits = log2Ceil(tpEntryMaxLen + 1)
  def tpTableReplacementPolicy = tpParams.replacementPolicy
  def debug = tpParams.debug
  def vaddrBits = VAddrBits
  def tpTrainQueueDepth = tpParams.tpTrainQueueDepth
  def dataReadQueueDepth = tpParams.dataReadQueueDepth
  def dataWriteQueueDepth = tpParams.dataWriteQueueDepth
  def tpDataQueueDepth = tpParams.tpDataQueueDepth
  def tpMetaWQueueDepth = tpParams.tpMetaWQueueDepth
  def tpMetaResetQueueDepth = tpParams.tpMetaResetQueueDepth
  def metaDataLength = fullAddressBits - offsetBits

  def pcHashWidth = tpParams.pcHashWidth
  def pcAddrHashWidth = Math.max(metaDataLength, pcHashWidth)

  // sampler filter parameters
  def samplerFilterAssoc = tpParams.samplerFilterAssoc
  def samplerFilterNrSet = tpParams.samplerFilterEntries / samplerFilterAssoc
  def samplerFilterSetBits = log2Ceil(samplerFilterNrSet)
  def samplerFilterReplacementPolicy = tpParams.samplerFilterReplacementPolicy
  def trainQueueDepth = tpParams.trainQueueDepth
  def filteredCntWidth = 6
  def filteredCntThrottle = 31
  // sampler table parameters
  def samplerTableAssoc = tpParams.samplerTableAssoc
  def samplerTableNrSet = tpParams.samplerTableEntries / samplerTableAssoc
  def samplerTableSetBits = log2Ceil(samplerTableNrSet)
  def samplerTableMatchCntWidth = tpParams.samplerTableMatchCntWidth
  def samplerTableReplacementPolicy = tpParams.samplerTableReplacementPolicy
  // recorder table parameters
  def recorderTableAssoc = tpParams.recorderTableAssoc
  def recorderTableNrSet = tpParams.recorderTableEntries / recorderTableAssoc
  def recorderTableSetBits = log2Ceil(recorderTableNrSet)
  def recorderTableReplacementPolicy = tpParams.recorderTableReplacementPolicy
  // confidence table parameters
  def confTableAssoc = tpParams.confTableAssoc
  def confTableNrSet = tpParams.confTableEntries / confTableAssoc
  def confTableSetBits = log2Ceil(confTableNrSet)
  def confTableReplacementPolicy = tpParams.confTableReplacementPolicy
  def confReqQueueDepth = tpParams.confReqQueueDepth
  def accConfWidth = 6

  require(tpEntryMaxLen > 8)
  require(pcHashWidth > samplerFilterSetBits && pcHashWidth > confTableSetBits)
}

abstract class TPBundle(implicit p: Parameters) extends XSBundle with HasTPParams
abstract class TPModule(implicit p: Parameters) extends XSModule with HasTPParams
abstract class TPmetaBundle(implicit p: Parameters) extends TPBundle
abstract class TPmetaModule(implicit p: Parameters) extends TPModule

/** Address tokens are Cat(virtual alias[1:0], physical byte address). */
class PrefetchTrain(implicit p: Parameters) extends TPBundle {
  val addr = UInt(fullAddressBits.W)
  val vaddr = UInt(VAddrBits.W)
  val pc = UInt(VAddrBits.W)
  val hit = Bool()
  val prefetched = Bool()
}

class PrefetchFeedBack(implicit p: Parameters) extends TPBundle {
  val pc = UInt(VAddrBits.W)
  val hit = Bool()
  val late = Bool()
}

class PrefetchReq(implicit p: Parameters) extends TPBundle {
  val addr = UInt(fullAddressBits.W)
  val hitCount = UInt(hitCountWidth.W)
}

class TPmetaReq(implicit p: Parameters) extends TPBundle {
  val set = UInt(tpTableSetBits.W)
  val way = UInt(log2Ceil(tpTableAssoc).W)
  val wmode = Bool()
  val rawData = Vec(tpEntryMaxLen, UInt(metaDataLength.W))
  val length = UInt(tpEntryLenBits.W)
  val hitCount = UInt(hitCountWidth.W)
}

class TPmetaResp(implicit p: Parameters) extends TPBundle {
  val rawData = Vec(tpEntryMaxLen, UInt(metaDataLength.W))
  val length = UInt(tpEntryLenBits.W)
  val hitCount = UInt(hitCountWidth.W)
}

class TPmetaIO(implicit p: Parameters) extends TPmetaBundle {
  val req = Flipped(DecoupledIO(new TPmetaReq()))
  val resp = ValidIO(new TPmetaResp())
}

class metaEntry(implicit p:Parameters) extends TPmetaBundle {
  val rawData = Vec(tpEntryMaxLen, UInt((fullAddressBits - offsetBits).W))
  val length = UInt(tpEntryLenBits.W)
}

class TPmeta(implicit p: Parameters) extends TPmetaModule
{
  val io = IO(new TPmetaIO())
  val tpDataTable = Module(
    new SRAMTemplate(
      new metaEntry(),
      set = tpTableNrSet,
      way = tpTableAssoc,
      shouldReset = false,
      singlePort = true)
  )

  val readReqValid = io.req.valid && !io.req.bits.wmode
  val writeReqValid = io.req.valid && io.req.bits.wmode

  tpDataTable.io.r.req.valid := readReqValid
  tpDataTable.io.r.req.bits.setIdx := io.req.bits.set

  val wdata = Wire(new metaEntry())
  wdata.rawData := io.req.bits.rawData
  wdata.length := io.req.bits.length
  tpDataTable.io.w.apply(
    valid = writeReqValid,
    data = wdata,
    setIdx = io.req.bits.set,
    waymask = UIntToOH(io.req.bits.way)
  )

  io.req.ready := Mux(io.req.bits.wmode, tpDataTable.io.w.req.ready, tpDataTable.io.r.req.ready)
  val readReqFire = io.req.fire && !io.req.bits.wmode
  val readReqValidReg = RegNext(readReqFire, false.B)
  val readReqReg = RegEnable(io.req.bits, readReqFire)

  val rdata = Reg(new metaEntry())
  when(readReqValidReg) {
    rdata := tpDataTable.io.r.resp.data(readReqReg.way)
  }

  io.resp.valid := RegNext(readReqValidReg, false.B)
  io.resp.bits.rawData := rdata.rawData
  io.resp.bits.length := rdata.length
  io.resp.bits.hitCount := RegNext(readReqReg).hitCount
}

class filteredEntry(implicit p: Parameters) extends TPBundle {
  val pc = UInt(pcHashWidth.W)
  val lastAddr = UInt(metaDataLength.W)
  val currAddr = UInt(metaDataLength.W)
  val cnt = UInt(filteredCntWidth.W)
}

class filterTableEntry(implicit p: Parameters) extends TPBundle {
  val valid = Bool()
  val pcTag = UInt((pcHashWidth - samplerFilterSetBits).W)
  val lastAddr = UInt(metaDataLength.W)
  val cnt = UInt(filteredCntWidth.W)
  val epoch = UInt(4.W)

  def apply(valid: Bool, tag: UInt, addr: UInt, cnt: UInt, epoch: UInt) = {
    val entry = Wire(new filterTableEntry)
    entry.valid := valid
    entry.pcTag := tag
    entry.lastAddr := addr
    entry.cnt := cnt
    entry.epoch := epoch
    entry
  }
}

class trainEntry(implicit p: Parameters) extends TPBundle {
  val valid = Bool()
  val pc = UInt(pcHashWidth.W)
  val addr = UInt(metaDataLength.W)
}

class SamplerFilter(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain()))
    val trained = DecoupledIO(new filteredEntry())
  })

  def hashPC(pc: UInt): UInt = {
    val shifted = (pc >> 2).pad(3 * pcHashWidth)
    val folded = shifted(pcHashWidth - 1, 0) ^
      shifted(2 * pcHashWidth - 1, pcHashWidth) ^
      shifted(3 * pcHashWidth - 1, 2 * pcHashWidth)
    (folded ^ (folded >> 11) ^ (folded << 13) ^ (folded >> 5))(pcHashWidth - 1, 0)
  }

  private val resultQueue = Module(new Queue(new filteredEntry(), tpTrainQueueDepth))
  io.trained <> resultQueue.io.deq

  def parsePaddr(x: UInt): (UInt, UInt) = {
    (x(x.getWidth - 1, samplerFilterSetBits), x(samplerFilterSetBits - 1, 0))
  }

  val trainQueue = Module(new Queue(new trainEntry(), trainQueueDepth + 1, pipe = false, flow = false))

  val filterTable = Module(
    new SRAMTemplate(
      new filterTableEntry(),
      set = samplerFilterNrSet,
      way = samplerFilterAssoc,
      shouldReset = false,
      singlePort = true
    )
  )
  val repl = new SetAssocReplacer(samplerFilterNrSet, samplerFilterAssoc, samplerFilterReplacementPolicy)

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((samplerFilterNrSet - 1).U)
  val filterEpoch = RegInit(0.U(4.W))
  val filterEpochCnt = RegInit(0.U(15.W))
  val filterEpochPeriodMinusOne = 32767.U(15.W)

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }
  when(io.train.fire) {
    when(filterEpochCnt === filterEpochPeriodMinusOne) {
      filterEpochCnt := 0.U
      filterEpoch := filterEpoch + 1.U
    }.otherwise {
      filterEpochCnt := filterEpochCnt + 1.U
    }
  }


  /* ------- stage pre ------- */
  // handle SRAM RW conflict
  val filterTableWValid = Wire(Bool())
  trainQueue.io.enq.valid := io.train.valid && resetFinish
  io.train.ready := trainQueue.io.enq.ready && resetFinish
  trainQueue.io.enq.bits.valid := io.train.valid
  trainQueue.io.enq.bits.pc := hashPC(io.train.bits.pc)
  trainQueue.io.enq.bits.addr := io.train.bits.addr >> offsetBits
  val queuedFilterSet = parsePaddr(trainQueue.io.deq.bits.pc)._2

  /* ------- stage 0 ------- */
  // use hash pc query filter table
  val s0_valid = trainQueue.io.deq.fire
  val pc_s0 = trainQueue.io.deq.bits.pc
  val trainAddr_s0 = trainQueue.io.deq.bits.addr
  val currAddr_s0 = WireInit(0.U(metaDataLength.W))
  currAddr_s0 := trainAddr_s0


  val filterTableRValid_s0 = s0_valid
  val (pcTag_s0, pcSet_s0) = parsePaddr(pc_s0)
  val filterRecord_s1 = filterTable.io.r(filterTableRValid_s0, pcSet_s0).resp.data

  /* ------- stage 1 ------- */
  // compare pc
  val s1_valid = RegNext(s0_valid, false.B)
  val pc_s1 = RegEnable(pc_s0, s0_valid)
  val currAddr_s1 = RegEnable(currAddr_s0, s0_valid)
  val (pcTag_s1, pcSet_s1) = parsePaddr(pc_s1)

  val tagMatchVec_s1 = filterRecord_s1.map(_.pcTag === pcTag_s1)
  val validVec_s1 = filterRecord_s1.map(_.valid)
  val hitVec_s1 = tagMatchVec_s1.zip(validVec_s1).map(x => x._1 && x._2)
  val hit_s1 = Cat(hitVec_s1).orR

  when(s1_valid) {
    assert(PopCount(hitVec_s1) <= 1.U)
  }

  val hitWay_s1 = OHToUInt(hitVec_s1)
  val victimWay_s1 = repl.way(pcSet_s1)
  val way_s1 = Mux(hit_s1, hitWay_s1, victimWay_s1)
  val lastAddr_s1 = filterRecord_s1(way_s1).lastAddr
  val epochMatch_s1 = filterRecord_s1(way_s1).epoch === filterEpoch
  val decayedCnt_s1 = Mux(epochMatch_s1, filterRecord_s1(way_s1).cnt, filterRecord_s1(way_s1).cnt >> 1)
  val cnt_s1 = Mux(hit_s1, decayedCnt_s1, 0.U)
  val epoch_s1 = filterEpoch

  when(s1_valid) {
    repl.access(pcSet_s1, way_s1)
  }

  /* ------- stage 2 ------- */
  // 1. miss: generate new entry; 2. hit: update entry & generate pair for sampler table
  // update sampler filter table
  // TODO: add "cnt" to preserve frequent pc
  val s2_valid = RegNext(s1_valid, false.B)
  val pc_s2 = RegEnable(pc_s1, s1_valid)
  val currAddr_s2 = RegEnable(currAddr_s1, s1_valid)
  val lastAddr_s2 = RegEnable(lastAddr_s1, s1_valid)
  val cnt_s2 = RegEnable(cnt_s1, s1_valid)
  val hit_s2 = RegEnable(hit_s1, s1_valid)
  val way_s2 = RegEnable(way_s1, s1_valid)
  val epoch_s2 = RegEnable(epoch_s1, s1_valid)
  val (pcTag_s2, pcSet_s2) = parsePaddr(pc_s2)
  val updateCnt = Mux(hit_s2 && !cnt_s2.andR, cnt_s2 + 1.U, cnt_s2)

  val updateEntry = WireInit(new filterTableEntry().apply(true.B, pcTag_s2, currAddr_s2, updateCnt, epoch_s2))
  val replEntry = WireInit(new filterTableEntry().apply(true.B, pcTag_s2, currAddr_s2, 0.U, epoch_s2))
  val resetEntry = WireInit(new filterTableEntry().apply(false.B, 0.U, 0.U, 0.U, 0.U))

  val filterTableWValid_s2 =  s2_valid || !resetFinish
  val filterTableWSet_s2 = Mux(resetFinish, pcSet_s2, resetIdx)
  val filterTableWWayOH_s2 = Mux(resetFinish, UIntToOH(way_s2), Fill(samplerFilterAssoc, true.B))
  val filterTableWEntry_s2 = Mux(resetFinish, Mux(hit_s2, updateEntry, replEntry), resetEntry)
  filterTableWValid := filterTableWValid_s2
  val filterTableSameSetBlocked = s1_valid && queuedFilterSet === pcSet_s1 || s2_valid && queuedFilterSet === pcSet_s2
  val resultCredit = (resultQueue.io.count +& PopCount(Seq(s1_valid, s2_valid))) < tpTrainQueueDepth.U
  trainQueue.io.deq.ready := !filterTableWValid && !filterTableSameSetBlocked && resultCredit

  filterTable.io.w.apply(
    valid = filterTableWValid_s2,
    data = filterTableWEntry_s2,
    setIdx = filterTableWSet_s2,
    waymask = filterTableWWayOH_s2
  )

  resultQueue.io.enq.valid := s2_valid && hit_s2
  resultQueue.io.enq.bits.pc := pc_s2
  resultQueue.io.enq.bits.lastAddr := lastAddr_s2
  resultQueue.io.enq.bits.currAddr := currAddr_s2
  resultQueue.io.enq.bits.cnt := cnt_s2

  assert(!(filterTable.io.r.req.valid && filterTable.io.w.req.valid))
  assert(resultQueue.io.enq.ready || !resultQueue.io.enq.valid)

  XSPerfAccumulate("l1_tp_filter_trained_valid", io.train.valid)
  XSPerfAccumulate("l1_tp_filter_hit", hit_s2 & s2_valid)
  XSPerfAccumulate("l1_tp_filter_miss", !hit_s2 & s2_valid)

  val filterDB = ChiselDB.createTable("l1_tpfilter", new filteredEntry(), basicDB = true)
  val filterPt = Wire(new filteredEntry())
  filterPt := resultQueue.io.enq.bits
  filterDB.log(filterPt, resultQueue.io.enq.fire, "", clock, reset)
}

class trainedPair(implicit p: Parameters) extends TPBundle {
  val pc = UInt(pcHashWidth.W)
  val addr1 = UInt(metaDataLength.W)
  val addr2 = UInt(metaDataLength.W)
}

class samplerTableEntry(implicit p: Parameters) extends TPBundle {
  val valid = Bool()
  val baseTag = UInt((metaDataLength - samplerTableSetBits).W)
  val targetAddr = UInt(metaDataLength.W)
  val pc = UInt(pcHashWidth.W)
  val matchCnt = UInt(samplerTableMatchCntWidth.W)

  def apply(valid: Bool, base: UInt, target: UInt, pc: UInt, cnt: UInt) = {
    val entry = Wire(new samplerTableEntry)
    entry.valid := valid
    entry.baseTag := base
    entry.targetAddr := target
    entry.pc := pc
    entry.matchCnt := cnt
    entry
  }
}

class SamplerTable(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new filteredEntry()))
    val trained = DecoupledIO(new trainedPair())
  })

  private val resultQueue = Module(new Queue(new trainedPair(), tpTrainQueueDepth))
  io.trained <> resultQueue.io.deq

  def parsePaddr(x: UInt): (UInt, UInt) = {
    (x(x.getWidth-1, samplerTableSetBits), x(samplerTableSetBits - 1, 0))
  }

  val samplerTable = Module(
    new SRAMTemplate(
      new samplerTableEntry(),
      set = samplerTableNrSet,
      way = samplerTableAssoc,
      shouldReset = false,
      singlePort = true
    )
  )
  val repl = new SetAssocReplacer(samplerTableNrSet, samplerTableAssoc, samplerTableReplacementPolicy)

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((samplerTableNrSet - 1).U)

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }

  /* ------- stage pre ------- */
  // Queue inputs while the single-port sampler table is occupied by writes.
  val samplerTableWValid = Wire(Bool())
  val trainQueue = Module(new Queue(new filteredEntry(), trainQueueDepth + 1, pipe = false, flow = false))
  trainQueue.io.enq.valid := io.train.valid && resetFinish
  io.train.ready := trainQueue.io.enq.ready && resetFinish
  trainQueue.io.enq.bits := io.train.bits
  val queuedSamplerSet = parsePaddr(trainQueue.io.deq.bits.lastAddr)._2

  assert(resultQueue.io.enq.ready || !resultQueue.io.enq.valid)

  /* ------- stage 0 ------- */
  // query samplerTable
  val s0_valid = trainQueue.io.deq.fire
  val baseAddr_s0 = trainQueue.io.deq.bits.lastAddr
  val targetAddr_s0 = trainQueue.io.deq.bits.currAddr
  val pc_s0 = trainQueue.io.deq.bits.pc
  val cnt_s0 = trainQueue.io.deq.bits.cnt

  val samplerTableRValid = s0_valid
  val (baseTag_s0, baseSet_s0) = parsePaddr(baseAddr_s0)
  val pairs_s1 = samplerTable.io.r(samplerTableRValid, baseSet_s0).resp.data

  /* ------- stage 1 ------- */
  // parse baseAddr to judge whether hit; choose victim way
  val s1_valid = RegNext(s0_valid, false.B)
  val baseAddr_s1 = RegEnable(baseAddr_s0, s0_valid)
  val targetAddr_s1 = RegEnable(targetAddr_s0, s0_valid)
  val pc_s1 = RegEnable(pc_s0, s0_valid)
  val cnt_s1 = RegEnable(cnt_s0, s0_valid)

  val (baseTag_s1, baseSet_s1) = parsePaddr(baseAddr_s1)
  val tagMatchVec_s1 = pairs_s1.map(_.baseTag === baseTag_s1)
  val validVec_s1 = pairs_s1.map(_.valid)
  val hitVec_s1 = tagMatchVec_s1.zip(validVec_s1).map(x => x._1 && x._2)
  val hit_s1 = Cat(hitVec_s1).orR

  when(s1_valid) {
    assert(PopCount(hitVec_s1) <= 1.U)
  }

  val hitWay_s1 = OHToUInt(hitVec_s1)
  val victimWay_s1 = repl.way(baseSet_s1)
  val way_s1 = Mux(hit_s1, hitWay_s1, victimWay_s1)
  val lastPair_s1 = pairs_s1(way_s1)

  when(s1_valid) {
    repl.access(baseSet_s1, way_s1)
  }

  /* ------- stage 2 ------- */
  // (1) hit: compare targetAddr with hit entry's targetAddr; generate new entry
  // (2) miss: generate new entry
  // hit & match: update hit entry state; send pair to recorder
  // hit & missmatch: update hit entry
  // miss: replace victim entry
  val s2_valid = RegNext(s1_valid, false.B)
  val baseAddr_s2 = RegEnable(baseAddr_s1, s1_valid)
  val targetAddr_s2 = RegEnable(targetAddr_s1, s1_valid)
  val pc_s2 = RegEnable(pc_s1, s1_valid)
  val cnt_s2 = RegEnable(cnt_s1, s1_valid)
  val hit_s2 = RegEnable(hit_s1, s1_valid)
  val way_s2 = RegEnable(way_s1, s1_valid)
  val lastPair_s2 = RegEnable(lastPair_s1, s1_valid)

  val cntValid_s2 = cnt_s2 >= filteredCntThrottle.U && s2_valid

  val match_s2 = lastPair_s2.targetAddr === targetAddr_s2
  val pcMatch_s2 = pc_s2 === lastPair_s2.pc
  val matchValid_s2 = match_s2 && pcMatch_s2
  val recordValid_s2 = (matchValid_s2 || lastPair_s2.matchCnt.orR) && hit_s2 && s2_valid

  val (baseTag_s2, baseSet_s2) = parsePaddr(baseAddr_s2)
  val maxMatch = lastPair_s2.matchCnt.andR
  val minMatch = !lastPair_s2.matchCnt.orR
  val updateMatchCnt = Mux(match_s2 && pcMatch_s2 && !maxMatch, lastPair_s2.matchCnt + 1.U,
    Mux(match_s2 && !pcMatch_s2 && !maxMatch, lastPair_s2.matchCnt,
      Mux(!match_s2 && !minMatch, lastPair_s2.matchCnt - 1.U, lastPair_s2.matchCnt)))
  val updateEntry = WireInit(new samplerTableEntry().apply(true.B, baseTag_s2, targetAddr_s2, pc_s2, updateMatchCnt))
  val replEntry = WireInit(new samplerTableEntry().apply(true.B, baseTag_s2, targetAddr_s2, pc_s2, 0.U))
  val resetEntry = WireInit(new samplerTableEntry().apply(false.B, 0.U, 0.U, 0.U, 0.U))

  val samplerTableWValid_s2 = recordValid_s2 || cntValid_s2 || !hit_s2 && s2_valid || !resetFinish
  val samplerTableWSet_s2 = Mux(resetFinish, baseSet_s2, resetIdx)
  val samplerTableWWayOH_s2 = Mux(resetFinish, UIntToOH(way_s2), Fill(samplerTableAssoc, true.B))
  val samplerTableWEntry_s2 = Mux(resetFinish, Mux(hit_s2, updateEntry, replEntry), resetEntry)

  samplerTableWValid := samplerTableWValid_s2
  val samplerTableSameSetBlocked = s1_valid && queuedSamplerSet === baseSet_s1 || s2_valid && queuedSamplerSet === baseSet_s2
  val resultCredit = (resultQueue.io.count +& PopCount(Seq(s1_valid, s2_valid))) < tpTrainQueueDepth.U
  trainQueue.io.deq.ready := !samplerTableWValid && !samplerTableSameSetBlocked && resultCredit

  samplerTable.io.w.apply(
    valid = samplerTableWValid_s2,
    data = samplerTableWEntry_s2,
    setIdx = samplerTableWSet_s2,
    waymask = samplerTableWWayOH_s2
  )

  resultQueue.io.enq.valid := recordValid_s2 || cntValid_s2
  resultQueue.io.enq.bits.pc := pc_s2
  resultQueue.io.enq.bits.addr1 := baseAddr_s2
  resultQueue.io.enq.bits.addr2 := targetAddr_s2


  XSPerfAccumulate("l1_tp_sampler_table_trained_valid", resultQueue.io.enq.fire)
  XSPerfAccumulate("l1_tp_sampler_table_hit", s2_valid && hit_s2)
  XSPerfAccumulate("l1_tp_sampler_table_hit_pc_match", s2_valid && hit_s2 && pcMatch_s2)
  XSPerfAccumulate("l1_tp_sampler_table_hit_addr_match", s2_valid && hit_s2 && match_s2)
  // XSPerfHistogram's stop is exclusive; include every saturating counter value.
  XSPerfHistogram("l1_tp_sampler_table_matchCnt", perfCnt = lastPair_s2.matchCnt, enable = s2_valid && hit_s2, start = 0, stop = 1 << samplerTableMatchCntWidth, step = 1)
  XSPerfAccumulate("l1_tp_sampler_table_record_valid", recordValid_s2)
  XSPerfAccumulate("l1_tp_sampler_table_cnt_valid", cntValid_s2)
  XSPerfAccumulate("l1_tp_sampler_table_W", samplerTableWValid)

  val samplerDB = ChiselDB.createTable("l1_tpsampler", new trainedPair(), basicDB = true)
  val samplerPt = Wire(new trainedPair())
  samplerPt := resultQueue.io.enq.bits
  samplerDB.log(samplerPt, resultQueue.io.enq.fire, "", clock, reset)
}

class trainedRecord(implicit p: Parameters) extends TPBundle {
  val pc = UInt(pcHashWidth.W)
  val data = Vec(tpEntryMaxLen, UInt(metaDataLength.W))
  val length = UInt(tpEntryLenBits.W)
  val trigger = UInt(metaDataLength.W)
}

class recorderTableEntry(implicit p: Parameters) extends TPBundle {
  val valid = Bool()
  val pcTag = UInt((pcHashWidth - recorderTableSetBits).W)
  val data = Vec(tpEntryMaxLen, UInt(metaDataLength.W))
  val index = UInt(tpEntryLenBits.W)
  val trigger = UInt(metaDataLength.W)

  private val payloadWidth =
    1 +
      (pcHashWidth - recorderTableSetBits) +
      tpEntryMaxLen * metaDataLength +
      tpEntryLenBits +
      metaDataLength

  val padding = UInt(sramPaddingWidth(payloadWidth, recorderTableAssoc).W)

  def sramPaddingWidth(dataWidth: Int, way: Int): Int = {
    def ok(w: Int): Boolean = {
      val maskSegments = utility.sram.SramInfo(w, way, bist = false).sramMaskBits
      (w * way) % maskSegments == 0
    }

    Iterator.from(0).find(pad => ok(dataWidth + pad)).get
  }

  def apply(valid: Bool, tag: UInt, data: Vec[UInt], index: UInt, trigger: UInt, pad: UInt) = {
    val entry = Wire(new recorderTableEntry)
    entry.valid := valid
    entry.pcTag := tag
    entry.data := data
    entry.index := index
    entry.trigger := trigger
    entry.padding := pad
    entry
  }
}

class RecorderTable(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val pair = Flipped(DecoupledIO(new trainedPair()))
    val record = DecoupledIO(new trainedRecord())
  })

  private val resultQueue = Module(new Queue(new trainedRecord(), tpTrainQueueDepth))
  io.record <> resultQueue.io.deq

  def parsePaddr(x: UInt): (UInt, UInt) = {
    (x(x.getWidth-1, recorderTableSetBits), x(recorderTableSetBits - 1, 0))
  }

  val recordThres = tpEntryMaxLen.U

  val recorderTable = Module(
    new SRAMTemplate(
      new recorderTableEntry(),
      set = recorderTableNrSet,
      way = recorderTableAssoc,
      shouldReset = false,
      singlePort = true
    )
  )
  val repl =  new SetAssocReplacer(recorderTableNrSet, recorderTableAssoc, recorderTableReplacementPolicy)

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((recorderTableNrSet - 1).U)

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }

  /* ------- stage pre ------- */
  // Queue inputs while the single-port recorder table is occupied by writes.
  val recorderTableWValid = Wire(Bool())
  val pairQueue = Module(new Queue(new trainedPair(), trainQueueDepth + 1, pipe = false, flow = false))
  pairQueue.io.enq.valid := io.pair.valid && resetFinish
  XSPerfAccumulate("l1_tp_recorder_input_addr_zero", io.pair.valid && (io.pair.bits.addr1 === 0.U || io.pair.bits.addr2 === 0.U))
  io.pair.ready := pairQueue.io.enq.ready && resetFinish
  pairQueue.io.enq.bits := io.pair.bits
  val queuedRecorderSet = parsePaddr(pairQueue.io.deq.bits.pc)._2

  assert(resultQueue.io.enq.ready || !resultQueue.io.enq.valid)

  /* ------- stage 0 ------- */
  // query recorderTable
  val s0_valid = pairQueue.io.deq.fire
  val addr1_s0 = pairQueue.io.deq.bits.addr1
  val addr2_s0 = pairQueue.io.deq.bits.addr2
  val pc_s0 = pairQueue.io.deq.bits.pc

  val recorderTableRValid = s0_valid
  val (pcTag_s0, pcSet_s0) = parsePaddr(pc_s0)
  val recorders_s1 = recorderTable.io.r(recorderTableRValid, pcSet_s0).resp.data

  /* ------- stage 1 ------- */
  // parse pc to judge whether hit; choose victim way
  val s1_valid = RegNext(s0_valid, false.B)
  val addr1_s1 = RegEnable(addr1_s0, s0_valid)
  val addr2_s1 = RegEnable(addr2_s0, s0_valid)
  val pc_s1 = RegEnable(pc_s0, s0_valid)

  val (pcTag_s1, pcSet_s1) = parsePaddr(pc_s1)
  val tagMatchVec_s1 = recorders_s1.map(_.pcTag === pcTag_s1)
  val validVec_s1 = recorders_s1.map(_.valid)
  val hitVec_s1 = tagMatchVec_s1.zip(validVec_s1).map(x => x._1 && x._2)
  val hit_s1 = Cat(hitVec_s1).orR

  when(s1_valid) {
    assert(PopCount(hitVec_s1) <= 1.U)
  }

  val hitWay_s1 = OHToUInt(hitVec_s1)
  val victimWay_s1 = repl.way(pcSet_s1)
  val way_s1 = Mux(hit_s1, hitWay_s1, victimWay_s1)
  val recorder_s1 = recorders_s1(way_s1)

  when(s1_valid) {
    repl.access(pcSet_s1, way_s1)
  }

  /* ------- stage 2 ------- */
  // hit: update record
  // miss: replace
  // if full or replaced, output record
  val s2_valid = RegNext(s1_valid, false.B)
  val addr1_s2 = RegEnable(addr1_s1, s1_valid)
  val addr2_s2 = RegEnable(addr2_s1, s1_valid)
  val pc_s2 = RegEnable(pc_s1, s1_valid)
  val hit_s2 = RegEnable(hit_s1 && recorder_s1.trigger =/= 0.U, s1_valid)
  assert(!(s1_valid && hit_s1 && recorder_s1.trigger === 0.U))
  val way_s2 = RegEnable(way_s1, s1_valid)
  val recorder_s2 = RegEnable(recorder_s1, s1_valid)
  val recorderIdx_s2 = RegEnable(recorder_s1.index, s1_valid)
  val recorderData_s2 = RegEnable(recorder_s1.data, s1_valid)
  val recorderValid_s2 = RegEnable(recorder_s1.valid, s1_valid)

  val recorderIdx_s3 = RegInit(0.U(tpEntryLenBits.W))
  val recorderData_s3 = RegInit(VecInit(Seq.fill(tpEntryMaxLen)(0.U(metaDataLength.W))))
  val recorderTrigger_s3 = RegInit(0.U(metaDataLength.W))
  val recorderChain_s3 = RegInit(false.B)

  val recordData_s2 = recorder_s2.data
  val recordAddr1HitVec = recordData_s2.map(_ === addr1_s2)
  val recordAddr2HitVec = recordData_s2.map(_ === addr2_s2)
  val addr1Unique_s2 = !Cat(recordAddr1HitVec).orR
  val addr2Unique_s2 = !Cat(recordAddr2HitVec).orR

  when(s2_valid && hit_s2) {
    recorderData_s3 := recorderData_s2
    recorderTrigger_s3 := recorder_s2.trigger
    recorderChain_s3 := false.B

    when(addr1Unique_s2 ^ addr2Unique_s2) {
      recorderData_s3(recorderIdx_s2) := Mux(addr1Unique_s2, addr1_s2, addr2_s2)
      when(recorderIdx_s2 === (recordThres - 1.U)) {
        recorderIdx_s3 := recordThres
      }.otherwise {
        recorderIdx_s3 := recorderIdx_s2 + 1.U
      }
    }.elsewhen(addr1Unique_s2 && addr2Unique_s2) {
      recorderData_s3(recorderIdx_s2) := addr1_s2
      when(recorderIdx_s2 === (recordThres - 1.U)) {
        recorderIdx_s3 := recordThres
        recorderChain_s3 := true.B
      }.elsewhen(recorderIdx_s2 === (recordThres - 2.U)) {
        recorderData_s3(recorderIdx_s2 + 1.U) := addr2_s2
        recorderIdx_s3 := recordThres
      }.otherwise {
        recorderData_s3(recorderIdx_s2 + 1.U) := addr2_s2
        recorderIdx_s3 := recorderIdx_s2 + 2.U
      }
    }.otherwise {
      recorderIdx_s3 := recorderIdx_s2
    }
  }.elsewhen(s2_valid && !hit_s2) {
    recorderIdx_s3 := recorderIdx_s2
    recorderData_s3 := recorderData_s2
    recorderTrigger_s3 := recorder_s2.trigger
    recorderChain_s3 := false.B
  }

  /* ------- stage 3 ------- */
  val s3_valid = RegNext(s2_valid, false.B)
  val addr1_s3 = RegEnable(addr1_s2, s2_valid)
  val addr2_s3 = RegEnable(addr2_s2, s2_valid)
  val pc_s3 = RegEnable(pc_s2, s2_valid)
  val recorderValid_s3 = RegEnable(recorderValid_s2, s2_valid)
  val hit_s3 = RegEnable(hit_s2, s2_valid)
  val way_s3 = RegEnable(way_s2, s2_valid)
  val recorderPcTag_s3 = RegEnable(recorder_s2.pcTag, s2_valid)

  val (pcTag_s3, pcSet_s3) = parsePaddr(pc_s3)
  val victimPC_s3 = Cat(recorderPcTag_s3, pcSet_s3)
  val full_s3 = recorderIdx_s3 === recordThres
  val fullValid_s3 = full_s3 && hit_s3
  val replValid_s3 = !hit_s3 && recorderValid_s3 && recorderIdx_s3 > 8.U
  val recordValid_s3 = s3_valid && (fullValid_s3 || replValid_s3)
  val recordFull_s3 = s3_valid && full_s3 && hit_s3

  val replEntryData_s3 = WireInit(VecInit(Seq.fill(tpEntryMaxLen)(0.U(metaDataLength.W))))
  replEntryData_s3(0) := addr2_s3
  val resetEntryData_s3 = WireInit(VecInit(Seq.fill(tpEntryMaxLen)(0.U(metaDataLength.W))))

  val updateEntry = WireInit(new recorderTableEntry().apply(true.B, pcTag_s3, recorderData_s3, recorderIdx_s3, recorderTrigger_s3, 0.U))
  val replEntry = WireInit(new recorderTableEntry().apply(true.B, pcTag_s3, replEntryData_s3, 1.U, addr1_s3, 0.U))
  val resetEntry = WireInit(new recorderTableEntry().apply(false.B, 0.U, resetEntryData_s3, 0.U, 0.U, 0.U))

  val recorderTableWValid_s3 = s3_valid || !resetFinish
  val recorderTableWSet_s3 = Mux(resetFinish, pcSet_s3, resetIdx)
  val recorderTableWWayOH_s3 = Mux(resetFinish, UIntToOH(way_s3), Fill(recorderTableAssoc, true.B))
  val chainFull_s3 = recorderChain_s3 && recordFull_s3
  val keepEntry_s3 = !recordFull_s3 || chainFull_s3
  val writeEntry_s3 = Mux(hit_s3 && !chainFull_s3, updateEntry, replEntry)
  val recorderTableWEntry_s3 = Mux(resetFinish && keepEntry_s3, writeEntry_s3, resetEntry)
  assert(!(recorderTableWValid_s3 && recorderTableWEntry_s3.valid && recorderTableWEntry_s3.trigger === 0.U))

  recorderTableWValid := recorderTableWValid_s3
  val recorderTableSameSetBlocked = s1_valid && queuedRecorderSet === pcSet_s1 ||
    s2_valid && queuedRecorderSet === parsePaddr(pc_s2)._2 ||
    s3_valid && queuedRecorderSet === pcSet_s3
  val resultCredit = (resultQueue.io.count +& PopCount(Seq(s1_valid, s2_valid, s3_valid))) < tpTrainQueueDepth.U
  pairQueue.io.deq.ready := !recorderTableWValid && !recorderTableSameSetBlocked && resultCredit

  recorderTable.io.w.apply(
    valid = recorderTableWValid_s3,
    data = recorderTableWEntry_s3,
    setIdx = recorderTableWSet_s3,
    waymask = recorderTableWWayOH_s3
  )

  resultQueue.io.enq.valid := recordValid_s3
  resultQueue.io.enq.bits.pc := Mux(recordFull_s3, pc_s3, victimPC_s3)
  resultQueue.io.enq.bits.data := recorderData_s3
  resultQueue.io.enq.bits.length := recorderIdx_s3
  resultQueue.io.enq.bits.trigger := recorderTrigger_s3

  assert(!(recorderTableWValid_s3 && recorderTableWEntry_s3.valid && (recorderTableWEntry_s3.index >= recordThres)))

  when(resultQueue.io.enq.valid) {
    assert(resultQueue.io.enq.bits.trigger =/= 0.U)
    assert(resultQueue.io.enq.bits.length =/= 0.U)
  }

  when(s3_valid) {
    assert(recorderIdx_s3 < (recordThres + 1.U))
  }

  XSPerfAccumulate("l1_tp_recorder_valid", io.record.valid)
  XSPerfAccumulate("l1_tp_recorder_addr1_unique", s2_valid && addr1Unique_s2)
  XSPerfAccumulate("l1_tp_recorder_addr2_unique", s2_valid && addr2Unique_s2)
  XSPerfAccumulate("l1_tp_recorder_table_w", recorderTableWValid)
  // The updated index can equal tpEntryMaxLen when this access completes a record.
  XSPerfHistogram("l1_tp_recorder_index", perfCnt = updateEntry.index, enable = hit_s3 && s3_valid, start = 0, stop = tpEntryMaxLen + 1, step = 1)

  val recorderDB = ChiselDB.createTable("l1_tprecorder", new trainedRecord(), basicDB = true)
  val recorderPt = Wire(new trainedRecord())
  recorderPt := resultQueue.io.enq.bits
  recorderDB.log(recorderPt, resultQueue.io.enq.fire, "", clock, reset)
}

class Sampler(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain()))
    val trained = DecoupledIO(new trainedRecord())
  })

  val filterTable = Module(new SamplerFilter())
  val samplerTable = Module(new SamplerTable())
  val recorderTable = Module(new RecorderTable())

  filterTable.io.train <> io.train
  samplerTable.io.train <> filterTable.io.trained
  recorderTable.io.pair <> samplerTable.io.trained
  io.trained <> recorderTable.io.record

}

class confTableEntry(implicit p: Parameters) extends TPBundle {
  val valid = Bool()
  val tag = UInt((pcHashWidth - confTableSetBits).W) // pc
  val issueConf = UInt(accConfWidth.W)
  val metaConf = UInt(accConfWidth.W)

  def apply(valid: Bool, tag: UInt, issueConf: UInt, metaConf: UInt) = {
    val entry = Wire(new confTableEntry)
    entry.valid := valid
    entry.tag := tag
    entry.issueConf := issueConf
    entry.metaConf := metaConf
    entry
  }
}

object ConfReqType {
  def width = 2
  def newMeta = 0.U(width.W)
  def issue = 1.U(width.W)
  def feedback = 2.U(width.W)
}

object ConfUpdateCond {
  def width = 3
  def newMeta = 0.U(width.W)
  def alloc = 1.U(width.W)
  def missPenalty = 2.U(width.W)
  def reward = 3.U(width.W)
  def noEffect = 4.U(width.W)
}

class confReq(implicit p: Parameters) extends TPBundle {
  val pc = UInt(pcHashWidth.W)
  val reqType = UInt(ConfReqType.width.W)
  val pfHit = Bool()
  val pfLate = Bool()
  val pfMiss = Bool()
  val pfCnt = UInt(accConfWidth.W)
}

class confResp(implicit  p: Parameters) extends TPBundle {
  val issue = Bool()
  val metaUpdate = Bool()
  val reqType = UInt(ConfReqType.width.W)
}

class confDBEntry(implicit p: Parameters) extends TPBundle {
  val pc = UInt(pcHashWidth.W)
  val updateCond = UInt(ConfUpdateCond.width.W)
  val issueConf = UInt(accConfWidth.W)
  val metaConf = UInt(accConfWidth.W)
}

class confTable(implicit p:Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new confReq()))
    val resp = ValidIO(new confResp())
  })

  def parsePC(x: UInt): (UInt, UInt) = {
    (x(x.getWidth-1, confTableSetBits), x(confTableSetBits - 1, 0))
  }

  val confTable = Module(
    new SRAMTemplate(
      new confTableEntry(),
      set = confTableNrSet,
      way = confTableAssoc,
      shouldReset = false,
      singlePort = true
    )
  )
  val repl = new SetAssocReplacer(confTableNrSet, confTableAssoc, confTableReplacementPolicy)

  val reqQueue = Module(new Queue(new confReq(), confReqQueueDepth, pipe = false, flow = false))

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((confTableNrSet - 1).U)
  val resetCnt = RegInit(100000.U)
  // Global sparse probes keep low-confidence PCs recoverable after phase changes.
  val confProbePeriod = 64
  val issueProbeCnt = RegInit(0.U(log2Ceil(confProbePeriod).W))
  val metaProbeCnt = RegInit(0.U(log2Ceil(confProbePeriod).W))

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }.elsewhen(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }

  when(resetCnt === 0.U) {
    resetCnt := 100000.U
    resetIdx := (confTableNrSet - 1).U
    resetFinish := false.B
  }.elsewhen(io.req.fire && (io.req.bits.reqType === ConfReqType.newMeta)) {
    resetCnt := resetCnt - 1.U
  }

  reqQueue.io.enq <> io.req

  /* ------- stage 0 ------- */
  // query confTable
  val reqQueueDeqValid = reqQueue.io.deq.valid
  val reqQueueDeqBits = reqQueue.io.deq.bits
  val (_, queuedSet_s0) = parsePC(reqQueueDeqBits.pc)
  val s0_valid = reqQueue.io.deq.fire
  val req_s0 = reqQueue.io.deq.bits
  val (tag_s0, set_s0) = parsePC(req_s0.pc)
  val reqType_s0 = req_s0.reqType
  val pfHit_s0 = req_s0.pfHit
  val pfLate_s0 = req_s0.pfLate
  val pfMiss_s0 = req_s0.pfMiss
  val pfCnt_s0 = req_s0.pfCnt

  val confs = confTable.io.r(s0_valid, set_s0).resp.data

  /* ------- stage 1 ------- */
  // parse pc to judge whether hit; choose victim way
  val s1_valid = RegNext(s0_valid, false.B)
  val tag_s1 = RegEnable(tag_s0, s0_valid)
  val set_s1 = RegEnable(set_s0, s0_valid)
  val reqType_s1 = RegEnable(reqType_s0, s0_valid)
  val pfHit_s1 = RegEnable(pfHit_s0, s0_valid)
  val pfLate_s1 = RegEnable(pfLate_s0, s0_valid)
  val pfMiss_s1 = RegEnable(pfMiss_s0, s0_valid)
  val pfCnt_s1 = RegEnable(pfCnt_s0, s0_valid)
  val needResp_s1 = RegEnable(reqType_s0 === ConfReqType.issue || reqType_s0 === ConfReqType.newMeta, s0_valid)

  val tagMatchVec_s1 = confs.map(_.tag === tag_s1)
  val validVec_s1 = confs.map(_.valid)
  val hitVec_s1 = tagMatchVec_s1.zip(validVec_s1).map(x => x._1 && x._2)
  val hit_s1 = Cat(hitVec_s1).orR
  when(s1_valid) {
    assert(PopCount(hitVec_s1) <= 1.U)
  }

  val hitWay_s1 = OHToUInt(hitVec_s1)
  val victimWay_s1 = repl.way(set_s1)
  val way_s1 = Mux(hit_s1, hitWay_s1, victimWay_s1)
  val conf_s1 = confs(way_s1)
  // Keep reqQueue ready independent of SRAM read data. Any s1 request may write in s2:
  // feedback always writes, while issue/newMeta write on miss.
  val s1MayWriteConf = reqType_s1 === ConfReqType.feedback ||
    reqType_s1 === ConfReqType.issue ||
    reqType_s1 === ConfReqType.newMeta
  val sameSetWriteHazard_s1 = resetFinish && reqQueueDeqValid && s1_valid && set_s1 === queuedSet_s0 &&
    s1MayWriteConf

  when(s1_valid) {
    repl.access(set_s1, way_s1)
  }

  /* ------- stage 2 ------- */
  // (1) hit: update conf && resp(if need
  // (2) miss: generate new entry
  val s2_valid = RegNext(s1_valid, false.B)
  val tag_s2 = RegEnable(tag_s1, s1_valid)
  val set_s2 = RegEnable(set_s1, s1_valid)
  val way_s2 = RegEnable(way_s1, s1_valid)
  val conf_s2 = RegEnable(conf_s1, s1_valid)
  val hit_s2 = RegEnable(hit_s1, s1_valid)
  val reqType_s2 = RegEnable(reqType_s1, s1_valid)
  val pfHit_s2 = RegEnable(pfHit_s1, s1_valid)
  val pfLate_s2 = RegEnable(pfLate_s1, s1_valid)
  val pfMiss_s2 = RegEnable(pfMiss_s1, s1_valid)
  val pfCnt_s2 = RegEnable(pfCnt_s1, s1_valid)
  val needResp_s2 = RegEnable(needResp_s1, s1_valid)

  val isNewMeta_s2 = reqType_s2 === ConfReqType.newMeta
  val isFeedback_s2 = reqType_s2 === ConfReqType.feedback
  val initConf = (1 << (accConfWidth - 1)).U(accConfWidth.W)
  val maxConf = ((1 << accConfWidth) - 1).U(accConfWidth.W)
  val issueAccuracyPercent = 25
  val metaAccuracyPercent = 40
  val issueThreshold = initConf
  val metaThreshold = initConf
  val feedbackReward = 1.U(accConfWidth.W)
  val metaFeedbackReward = 1.U(accConfWidth.W)
  def satInc(x: UInt, step: UInt): UInt = Mux(x > maxConf - step, maxConf, x + step)
  def satDec(x: UInt, step: UInt): UInt = Mux(x < step, 0.U, x - step)

  // Charge confidence by the target useful-prefetch ratio of one issued group.
  val issueMissPenaltyTable = VecInit((0 until (1 << accConfWidth)).map { cnt =>
    ((cnt * issueAccuracyPercent + 99) / 100).U(accConfWidth.W)
  })
  val metaMissPenaltyTable = VecInit((0 until (1 << accConfWidth)).map { cnt =>
    ((cnt * metaAccuracyPercent + 99) / 100).U(accConfWidth.W)
  })
  val issueMissPenalty = issueMissPenaltyTable(pfCnt_s2)
  val metaMissPenalty = metaMissPenaltyTable(pfCnt_s2)
  val nextIssueConf = WireDefault(conf_s2.issueConf)
  val nextMetaConf = WireDefault(conf_s2.metaConf)
  when(isFeedback_s2) {
    when(pfMiss_s2) {
      nextIssueConf := satDec(conf_s2.issueConf, issueMissPenalty)
      nextMetaConf := satDec(conf_s2.metaConf, metaMissPenalty)
    }.elsewhen(pfHit_s2 || pfLate_s2) {
      nextIssueConf := satInc(conf_s2.issueConf, feedbackReward)
      when(pfHit_s2) {
        nextMetaConf := satInc(conf_s2.metaConf, metaFeedbackReward)
      }
    }
  }

  val issueFeedbackMissInit = satDec(initConf, issueMissPenalty)
  val feedbackHitInit = satInc(initConf, feedbackReward)
  val issueInitConf = Mux(
    isFeedback_s2 && pfMiss_s2,
    issueFeedbackMissInit,
    Mux(isFeedback_s2 && (pfHit_s2 || pfLate_s2), feedbackHitInit, initConf)
  )
  val metaInitConf = Mux(
    isFeedback_s2 && pfMiss_s2,
    satDec(initConf, metaMissPenalty),
    Mux(
      isFeedback_s2 && pfHit_s2,
      satInc(initConf, metaFeedbackReward),
      initConf
    )
  )
  val updateEntry = WireInit(new confTableEntry().apply(true.B, tag_s2, nextIssueConf, nextMetaConf))
  val replEntry = WireInit(new confTableEntry().apply(true.B, tag_s2, issueInitConf, metaInitConf))
  val resetEntry = WireInit(new confTableEntry().apply(false.B, 0.U, 0.U, 0.U))

  val confTableWValid_s2 = s2_valid && (isFeedback_s2 || !hit_s2) || !resetFinish
  // A read must wait while the single-port table is writing. Same-set reads also wait for the
  // in-flight stage-1 update decision to prevent duplicate tag allocations on back-to-back misses.
  reqQueue.io.deq.ready := !confTableWValid_s2 && !sameSetWriteHazard_s1
  val confTableWSet_s2 = Mux(resetFinish, set_s2, resetIdx)
  val confTableWWayOH_s2 = Mux(resetFinish, UIntToOH(way_s2), Fill(confTableAssoc, true.B))
  val confTableWEntry_s2 = Mux(resetFinish, Mux(hit_s2, updateEntry, replEntry), resetEntry)

  val confEntryAlloc_s2 = resetFinish && s2_valid && !hit_s2
  val confEntryUpdate_s2 = resetFinish && s2_valid && hit_s2 &&
    isFeedback_s2 && (pfMiss_s2 || pfHit_s2 || pfLate_s2)
  val confUpdateCond_s2 = WireDefault(ConfUpdateCond.noEffect)
  when(isNewMeta_s2) {
    confUpdateCond_s2 := ConfUpdateCond.newMeta
  }.elsewhen(isFeedback_s2 && pfMiss_s2) {
    confUpdateCond_s2 := ConfUpdateCond.missPenalty
  }.elsewhen(isFeedback_s2 && (pfHit_s2 || pfLate_s2)) {
    confUpdateCond_s2 := ConfUpdateCond.reward
  }.elsewhen(!hit_s2) {
    confUpdateCond_s2 := ConfUpdateCond.alloc
  }

  confTable.io.w.apply(
    valid = confTableWValid_s2,
    data = confTableWEntry_s2,
    setIdx = confTableWSet_s2,
    waymask = confTableWWayOH_s2
  )

  val issueConfPass = hit_s2 && conf_s2.issueConf >= issueThreshold
  val metaConfPass = hit_s2 && conf_s2.metaConf >= metaThreshold
  val issueProbeFire = s2_valid && reqType_s2 === ConfReqType.issue && hit_s2 && !issueConfPass && issueProbeCnt.andR
  val metaProbeFire = s2_valid && reqType_s2 === ConfReqType.newMeta && hit_s2 && !metaConfPass && metaProbeCnt.andR
  when(s2_valid && reqType_s2 === ConfReqType.issue && hit_s2 && !issueConfPass) {
    issueProbeCnt := issueProbeCnt + 1.U
  }
  when(s2_valid && reqType_s2 === ConfReqType.newMeta && hit_s2 && !metaConfPass) {
    metaProbeCnt := metaProbeCnt + 1.U
  }

  io.resp.valid := s2_valid && needResp_s2
  io.resp.bits.issue := Mux(hit_s2, issueConfPass || issueProbeFire, true.B)
  // Cold misses still install the first meta; low-confidence hits get sparse probes for recovery.
  io.resp.bits.metaUpdate := Mux(hit_s2, metaConfPass || metaProbeFire, true.B)
  io.resp.bits.reqType := reqType_s2

  XSPerfAccumulate("l1_tp_conf_table_pf_hit", io.req.fire && io.req.bits.pfHit)
  XSPerfAccumulate("l1_tp_conf_table_pf_late", io.req.fire && io.req.bits.pfLate)
  XSPerfAccumulate("l1_tp_conf_table_pf_miss", io.req.fire && io.req.bits.pfMiss)
  XSPerfAccumulate("l1_tp_conf_table_pf_issue", io.req.fire && (io.req.bits.reqType === ConfReqType.issue))
  XSPerfAccumulate("l1_tp_conf_table_new_meta", io.req.fire && (io.req.bits.reqType === ConfReqType.newMeta))
  XSPerfAccumulate("l1_tp_conf_table_resp", io.resp.valid)
  XSPerfAccumulate("l1_tp_conf_table_resp_issue", io.resp.valid && io.resp.bits.reqType === ConfReqType.issue && io.resp.bits.issue)
  XSPerfAccumulate("l1_tp_conf_table_resp_meta_update", io.resp.valid && io.resp.bits.reqType === ConfReqType.newMeta && io.resp.bits.metaUpdate)
  XSPerfAccumulate("l1_tp_conf_table_issue_probe", issueProbeFire)
  XSPerfAccumulate("l1_tp_conf_table_meta_probe", metaProbeFire)

  val confDB = ChiselDB.createTable("l1_tpconf", new confDBEntry(), basicDB = true)
  val confPt = Wire(new confDBEntry())
  confPt.pc := Cat(tag_s2, set_s2)
  confPt.updateCond := confUpdateCond_s2
  confPt.issueConf := confTableWEntry_s2.issueConf
  confPt.metaConf := confTableWEntry_s2.metaConf
  confDB.log(confPt, confEntryAlloc_s2 || confEntryUpdate_s2, "", clock, reset)
}

class tpMetaEntry(implicit p:Parameters) extends TPBundle {
  val valid = Bool()
  val tag = UInt((pcAddrHashWidth - tpTableSetBits).W)
}

class tpDataEntry(implicit p:Parameters) extends TPBundle {
  val rawData = Vec(tpEntryMaxLen, UInt((fullAddressBits - offsetBits).W))
  val length = UInt(tpEntryLenBits.W)
  val hitCount = UInt(hitCountWidth.W)
}

class tpMetaResetEntry(implicit p:Parameters) extends TPBundle {
  val set = UInt(tpTableSetBits.W)
  val way = UInt(log2Ceil(tpTableAssoc).W)
  val tag = UInt((pcAddrHashWidth - tpTableSetBits).W)
  val hitCount = UInt(hitCountWidth.W)
}

class trainBundle(implicit p: Parameters) extends TPBundle { //db
  val vaddr = UInt(vaddrBits.W)
  val paddr = UInt(fullAddressBits.W)
  val hit = Bool()
  val prefetched = Bool()
  val metahit = Bool()
  val pc = UInt(pcHashWidth.W)
}


class TemporalPrefetchEngine(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val enable = Input(Bool())
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new PrefetchReq)
    val feedBack = Flipped(DecoupledIO(new PrefetchFeedBack))
  })

  def parseIndex(x: UInt): (UInt, UInt) = {
    (x(x.getWidth-1, tpTableSetBits), x(tpTableSetBits-1, 0))
  }

  def hashPC(pc: UInt): UInt = {
    val shifted = (pc >> 2).pad(3 * pcHashWidth)
    val folded = shifted(pcHashWidth - 1, 0) ^
      shifted(2 * pcHashWidth - 1, pcHashWidth) ^
      shifted(3 * pcHashWidth - 1, 2 * pcHashWidth)
    (folded ^ (folded >> 11) ^ (folded << 13) ^ (folded >> 5))(pcHashWidth - 1, 0)
  }

  def mixTPMetaIndex(pc: UInt, trigger: UInt): UInt = {
    val pcPadded = pc.pad(pcAddrHashWidth)
    val triggerPadded = trigger.pad(pcAddrHashWidth)
    val pcRot = Cat(pcPadded(pcAddrHashWidth - 6, 0), pcPadded(pcAddrHashWidth - 1, pcAddrHashWidth - 5))

    (triggerPadded ^
      (triggerPadded >> tpTableSetBits).pad(pcAddrHashWidth) ^
      (triggerPadded >> (2 * tpTableSetBits)).pad(pcAddrHashWidth) ^
      (triggerPadded >> (3 * tpTableSetBits)).pad(pcAddrHashWidth) ^
      pcRot ^
      (pcPadded >> 6).pad(pcAddrHashWidth))(pcAddrHashWidth - 1, 0)
  }

  val tpmeta = Module(new TPmeta())
  val tpMetaTable = Module(
    new SRAMTemplate(
      new tpMetaEntry(),
      set = tpTableNrSet,
      way = tpTableAssoc,
      shouldReset = false,
      singlePort = true,
      hasMbist = hasMbist,
      hasSramCtl = hasSramCtl
    )
  )
  // Scalar counters avoid cloning a parameterized bundle for every one of the 16K entries.
  val hitCount = RegInit(VecInit(Seq.fill(tpTableNrSet)(VecInit(Seq.fill(tpTableAssoc)(0.U(hitCountWidth.W))))))
  val sampler = Module(new Sampler())
  val confTable = Module(new confTable())
  val trainQueue = Module(new Queue(new PrefetchTrain(), tpTrainQueueDepth, pipe = false, flow = false))
  val dataReadQueue = Module(new Queue(new TPmetaReq(), dataReadQueueDepth, pipe = false, flow = false))
  val dataWriteQueue = Module(new Queue(new TPmetaReq(), dataWriteQueueDepth, pipe = false, flow = false))
  val tpDataQueue = Module(new Queue(new tpDataEntry(), tpDataQueueDepth + 1, pipe = false, flow = false))
  // Carries the hashed PC for each accepted tpData read request.
  val dataReadPCQueue = Module(new Queue(UInt(pcHashWidth.W), dataReadQueueDepth, pipe = false, flow = false))
  // Holds PC sideband entries for tpData SRAM reads whose responses have not returned yet.
  val tpDataPCPendingQueue = Module(new Queue(UInt(pcHashWidth.W), dataReadQueueDepth, pipe = false, flow = false))
  // Aligns returned tpData entries with their source PC until the send/drop decision is made.
  val tpDataPCQueue = Module(new Queue(UInt(pcHashWidth.W), tpDataQueueDepth + 1, pipe = false, flow = false))
  val metaWQueue = Module(new Queue(new trainedRecord(), tpMetaWQueueDepth, pipe = false, flow = false))
  val tpMetaResetQueue = Module(new Queue(new tpMetaResetEntry(), tpMetaResetQueueDepth, pipe = false, flow = false))
  // Stores issue decisions from confTable; entries are consumed in lockstep with tpDataQueue.
  val confRespQueue = Module(new Queue(new confResp(), confReqQueueDepth + 1, pipe = false, flow = false))
  // Buffers confidence checks for candidate tpMeta records before they are installed.
  val confNewMetaQueue = Module(new Queue(new confReq(), confReqQueueDepth, pipe = false, flow = false))
  // Holds candidate tpMeta records while their newMeta confidence lookup is in flight.
  val metaConfPendingQueue = Module(new Queue(new trainedRecord(), tpMetaWQueueDepth, pipe = false, flow = false))
  // Holds candidate tpMeta records that passed confidence gating and can update tpMeta/tpData.
  val metaInstallQueue = Module(new Queue(new trainedRecord(), tpMetaWQueueDepth, pipe = false, flow = false))
  // Buffers confidence lookup requests for meta-hit tpData reads.
  val confIssueQueue = Module(new Queue(new confReq(), confReqQueueDepth, pipe = false, flow = false))
  // Buffers synthetic miss penalties charged when a prefetch group is actually issued.
  val confIssueMissQueue = Module(new Queue(new confReq(), confReqQueueDepth, pipe = false, flow = false))
  // Buffers real cache feedback rewards for TP prefetch hits or late hits.
  val confFeedbackQueue = Module(new Queue(new confReq(), confReqQueueDepth, pipe = false, flow = false))
  val confFeedbackArb = Module(new Arbiter(new confReq(), 2))
  val confReqArb = Module(new Arbiter(new confReq(), 3))
  val pendingPfCnt = RegInit(0.U(log2Ceil(tpDataQueueDepth + 2).W))
  // Reserve data queue space from read enqueue through group dequeue; launch keeps two pipeline slots free.
  // Tracks outstanding issue lookups so confRespQueue space is reserved before reads are launched.
  val pendingConfRespCnt = RegInit(0.U(log2Ceil(confReqQueueDepth + 2).W))
  // Reserves metaInstallQueue space for in-flight newMeta confidence responses.
  val pendingNewMetaRespCnt = RegInit(0.U(log2Ceil(tpMetaWQueueDepth + 2).W))

  val repl = new SetAssocReplacer(tpTableNrSet, tpTableAssoc, tpTableReplacementPolicy)

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((tpTableNrSet - 1).U)

  val recordThres = tpEntryMaxLen.U
  val tpThrottleCycles = tpParams.throttleCycles.U
  val enable = io.enable

  trainQueue.io.enq.valid := io.train.fire
  trainQueue.io.enq.bits := io.train.bits
  val dataReadQueueHasCredit = dataReadQueue.io.count < (dataReadQueueDepth - 2).U
  val dataReadPCQueueHasCredit = dataReadPCQueue.io.count < (dataReadQueueDepth - 2).U
  val confIssueQueueHasCredit = confIssueQueue.io.count < (confReqQueueDepth - 2).U
  val confRespQueueHasCredit = pendingConfRespCnt < (confReqQueueDepth - 2).U
  val tpMetaWriteValid = Wire(Bool())
  val tpMetaTrainSameSetBlocked = Wire(Bool())
  val tpMetaInstallSameSetBlocked = Wire(Bool())
  trainQueue.io.deq.ready := !(tpMetaWriteValid || tpMetaTrainSameSetBlocked || metaInstallQueue.io.deq.valid) &&
    dataReadQueueHasCredit && dataReadPCQueueHasCredit && confIssueQueueHasCredit &&
    confRespQueueHasCredit && pendingPfCnt < (tpDataQueueDepth - 2).U // metaW first

  // from sampler
  metaWQueue.io.enq <> sampler.io.trained
  val metaInstallReserveAvailable =
    (metaInstallQueue.io.count +& pendingNewMetaRespCnt) < tpMetaWQueueDepth.U
  metaWQueue.io.deq.ready := confNewMetaQueue.io.enq.ready && metaConfPendingQueue.io.enq.ready &&
    metaInstallReserveAvailable
  metaConfPendingQueue.io.enq.valid := metaWQueue.io.deq.fire
  metaConfPendingQueue.io.enq.bits := metaWQueue.io.deq.bits
  assert(metaConfPendingQueue.io.enq.ready || !metaWQueue.io.deq.fire)
  metaInstallQueue.io.deq.ready := !tpMetaWriteValid && !tpMetaInstallSameSetBlocked

  confReqArb.io.in(0) <> confNewMetaQueue.io.deq
  confReqArb.io.in(1) <> confIssueQueue.io.deq
  confFeedbackArb.io.in(0) <> confIssueMissQueue.io.deq
  confFeedbackArb.io.in(1) <> confFeedbackQueue.io.deq
  confReqArb.io.in(2) <> confFeedbackArb.io.out
  confTable.io.req <> confReqArb.io.out

  /* Stage 0: query tpMetaTable */

  val train_s0 = trainQueue.io.deq.bits
  val trainValid_s0 = trainQueue.io.deq.fire && train_s0.pc.orR
  val trainPaddr = train_s0.addr
  val trainMeta = trainPaddr >> offsetBits
  val trainPC = hashPC(train_s0.pc)
  val trainIndex = mixTPMetaIndex(trainPC, trainMeta.asUInt)
  val queuedTrainSet = parseIndex(trainIndex)._2

  val metaWValid_s0 = metaInstallQueue.io.deq.fire
  val metaWRecord_s0 = metaInstallQueue.io.deq.bits
  val metaWRecordIndex = mixTPMetaIndex(metaWRecord_s0.pc, metaWRecord_s0.trigger)
  val queuedMetaInstallSet = parseIndex(metaWRecordIndex)._2

  val s0_valid = trainValid_s0 || metaWValid_s0
  val index = Mux(metaWValid_s0, metaWRecordIndex, trainIndex)
  val (tag_s0, set_s0) = parseIndex(index)
  val metas = tpMetaTable.io.r(s0_valid, set_s0).resp.data // get in s1

  // to sampler
  sampler.io.train.valid := io.train.fire && io.train.bits.pc.orR
  sampler.io.train.bits := io.train.bits


  /* Stage 1: parse tpMeta to judge hit or miss, choose the victim */

  val s1_valid = RegNext(s0_valid, false.B)
  val tag_s1 = RegEnable(tag_s0, s0_valid)
  val set_s1 = RegEnable(set_s0, s0_valid)
  val train_s1 = RegEnable(train_s0, s0_valid)
  val metaWValid_s1 = RegEnable(metaWValid_s0, s0_valid)
  val metaWRecord_s1 = RegEnable(metaWRecord_s0, s0_valid)

  val tagMatchVec = metas.map(_.tag === tag_s1)
  val metaValidVec = metas.map(_.valid === true.B)

  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)
  val hitWay = OHToUInt(hitVec)

  val hit_s1 = Cat(hitVec).orR
  val baseVictimWay_s1 = repl.way(set_s1)
  val way_s1 = Mux(hit_s1, hitWay, baseVictimWay_s1)
  val hitCount_s1 = hitCount(set_s1)(way_s1)
  when(s1_valid) { assert(PopCount(hitVec) <= 1.U) }

  when(s1_valid && hit_s1) {
    repl.access(set_s1, way_s1)
  }.elsewhen(s1_valid && metaWValid_s1) {
    repl.access(set_s1, way_s1)
  }

  val pfIssue = s1_valid && hit_s1 && !metaWValid_s1

  // Queue hit-count updates for meta hits.
  tpMetaResetQueue.io.enq.valid := pfIssue
  tpMetaResetQueue.io.enq.bits.set := set_s1
  tpMetaResetQueue.io.enq.bits.way := way_s1
  tpMetaResetQueue.io.enq.bits.tag := tag_s1
  tpMetaResetQueue.io.enq.bits.hitCount := Mux(hitCount_s1.andR, hitCount_s1, hitCount_s1 + 1.U)

  /* Stage 2: access tpData on meta hit, record it on meta miss */

  val s2_valid = RegNext(s1_valid, false.B)
  val hit_s2 = RegEnable(hit_s1 && !metaWValid_s1, false.B, s1_valid) // metaW should be miss
  val way_s2 = RegEnable(way_s1, s1_valid)
  val set_s2 = RegEnable(set_s1, s1_valid)
  val tag_s2 = RegEnable(tag_s1, s1_valid)
  val train_s2 = RegEnable(train_s1, s1_valid)
  val hitCount_s2 = RegEnable(hitCount_s1, s1_valid)
  val metaWValid_s2 = RegEnable(metaWValid_s1, false.B, s1_valid)
  val metaWRecord_s2 = RegEnable(metaWRecord_s1, s1_valid)

  // Delay tpMeta installation writeback to s2, matching samplerTable/recorderTable style:
  // s1 consumes SRAM read data to pick the way, while s2 drives the single-port write.
  val tpTableWValid = s2_valid && metaWValid_s2
  val metaWEntry = Wire(new tpMetaEntry())
  metaWEntry.valid := true.B
  metaWEntry.tag := tag_s2
  when(!resetFinish) {
    metaWEntry.valid := false.B
    metaWEntry.tag := 0.U
  }
  val tpTableWLength = metaWRecord_s2.length
  val tpTableWSet = Mux(resetFinish, set_s2, resetIdx)
  val tpTableWWay = way_s2
  val tpTableWWayOH = Mux(resetFinish, UIntToOH(tpTableWWay), Fill(tpTableAssoc, true.B))

  tpMetaWriteValid := tpTableWValid || !resetFinish
  tpMetaTable.io.w.apply(tpMetaWriteValid, metaWEntry, tpTableWSet, tpTableWWayOH)

  val metaInstallHitCountWrite = tpTableWValid
  when(metaInstallHitCountWrite) {
    hitCount(set_s2)(way_s2) := 0.U
  }.elsewhen(tpMetaResetQueue.io.deq.fire) {
    hitCount(tpMetaResetQueue.io.deq.bits.set)(tpMetaResetQueue.io.deq.bits.way) := tpMetaResetQueue.io.deq.bits.hitCount
  }

  dataWriteQueue.io.enq.valid := tpTableWValid
  dataWriteQueue.io.enq.bits.wmode := true.B
  dataWriteQueue.io.enq.bits.rawData.zip(metaWRecord_s2.data).foreach(x => x._1 := x._2(metaDataLength - 1, 0))
  dataWriteQueue.io.enq.bits.length := tpTableWLength
  dataWriteQueue.io.enq.bits.set := tpTableWSet
  dataWriteQueue.io.enq.bits.way := tpTableWWay
  dataWriteQueue.io.enq.bits.hitCount := 0.U // DontCare
  assert(dataWriteQueue.io.enq.ready || !dataWriteQueue.io.enq.valid)

  // Same-set reads are held only for older meta-install accesses that will write in s2.
  tpMetaTrainSameSetBlocked := resetFinish &&
    (s1_valid && metaWValid_s1 && queuedTrainSet === set_s1 ||
      s2_valid && metaWValid_s2 && queuedTrainSet === set_s2)
  tpMetaInstallSameSetBlocked := resetFinish &&
    (s1_valid && metaWValid_s1 && queuedMetaInstallSet === set_s1 ||
      s2_valid && metaWValid_s2 && queuedMetaInstallSet === set_s2)

  // dataReadQueue enqueue
  val canAllocConfResp = pendingConfRespCnt < (confReqQueueDepth + 1).U
  dataReadQueue.io.enq.valid := s2_valid && hit_s2 && dataReadPCQueue.io.enq.ready &&
    confIssueQueue.io.enq.ready && canAllocConfResp
  dataReadQueue.io.enq.bits.set := set_s2
  dataReadQueue.io.enq.bits.way := way_s2
  dataReadQueue.io.enq.bits.wmode := false.B
  dataReadQueue.io.enq.bits.rawData := DontCare
  dataReadQueue.io.enq.bits.length := 0.U //DontCare
  dataReadQueue.io.enq.bits.hitCount := hitCount_s2
  dataReadPCQueue.io.enq.valid := dataReadQueue.io.enq.fire
  dataReadPCQueue.io.enq.bits := hashPC(train_s2.pc)
  assert(dataReadPCQueue.io.enq.ready || !dataReadQueue.io.enq.fire)
  // Credits reserved before s0 guarantee that no non-stallable s2 hit is lost.
  assert(!(s2_valid && hit_s2) || dataReadQueue.io.enq.fire)


  /* Async Stage: try to fetch or write tpData */

  // dataReadQueue/dataWriteQueue dequeue
  val readSidebandReady = dataReadPCQueue.io.deq.valid && tpDataPCPendingQueue.io.enq.ready
  val readReqValid = dataReadQueue.io.deq.valid && readSidebandReady
  val writeReqValid = dataWriteQueue.io.deq.valid
  dataReadQueue.io.deq.ready := tpmeta.io.req.ready && !writeReqValid && readSidebandReady
  dataWriteQueue.io.deq.ready := tpmeta.io.req.ready
  dataReadPCQueue.io.deq.ready := dataReadQueue.io.deq.fire
  tpDataPCPendingQueue.io.enq.valid := dataReadQueue.io.deq.fire
  tpDataPCPendingQueue.io.enq.bits := dataReadPCQueue.io.deq.bits
  assert(dataReadPCQueue.io.deq.valid || !dataReadQueue.io.deq.fire)
  assert(tpDataPCPendingQueue.io.enq.ready || !tpDataPCPendingQueue.io.enq.valid)

  // tpmeta_port req assignment, priority: write > read
  tpmeta.io.req.valid := readReqValid || writeReqValid
  tpmeta.io.req.bits := Mux(writeReqValid, dataWriteQueue.io.deq.bits, dataReadQueue.io.deq.bits)


  /* Async Stage: get tpMeta and insert it into tpDataQueue */
  when(dataReadQueue.io.enq.fire && !tpDataQueue.io.deq.fire) {
    pendingPfCnt := pendingPfCnt + 1.U
  }.elsewhen(!dataReadQueue.io.enq.fire && tpDataQueue.io.deq.fire) {
    pendingPfCnt := pendingPfCnt - 1.U
  }
  assert(pendingPfCnt <= (tpDataQueueDepth + 1).U)
  assert(!tpDataQueue.io.deq.fire || pendingPfCnt =/= 0.U)

  val tpDataRespValid = tpmeta.io.resp.valid
  tpDataQueue.io.enq.valid := tpDataRespValid && tpDataPCPendingQueue.io.deq.valid && tpDataPCQueue.io.enq.ready
  tpDataQueue.io.enq.bits.rawData := tpmeta.io.resp.bits.rawData
  tpDataQueue.io.enq.bits.length := tpmeta.io.resp.bits.length
  tpDataQueue.io.enq.bits.hitCount := tpmeta.io.resp.bits.hitCount
  tpDataPCQueue.io.enq.valid := tpDataQueue.io.enq.fire
  tpDataPCQueue.io.enq.bits := tpDataPCPendingQueue.io.deq.bits
  tpDataPCPendingQueue.io.deq.ready := tpDataQueue.io.enq.fire
  assert(tpDataQueue.io.enq.ready || !tpDataQueue.io.enq.valid) // tpDataQueue is never full
  assert(tpDataPCPendingQueue.io.deq.valid || !tpDataRespValid)
  assert(tpDataPCQueue.io.enq.ready || !tpDataRespValid)

  tpMetaResetQueue.io.deq.ready := !metaInstallHitCountWrite && (resetFinish || pendingPfCnt.andR)
  assert(tpMetaResetQueue.io.enq.ready || !tpMetaResetQueue.io.enq.valid)

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }


  /* Send prefetch request */

  val do_sending = RegInit(false.B)
  val sending_idx = RegInit(0.U(tpEntryLenBits.W))
  val sending_data = Reg(Vec(tpEntryMaxLen, UInt((fullAddressBits - offsetBits).W)))
  val sending_length = RegInit(0.U(tpEntryLenBits.W))
  val sending_hitCount = RegInit(0.U(hitCountWidth.W))
  val sending_throttle = RegInit(0.U(4.W))

  val sending_valid = do_sending && sending_throttle === tpThrottleCycles
  val current_sending_data = Cat(sending_data(sending_idx), 0.U(offsetBits.W))

  val tpDataSidebandValid = confRespQueue.io.deq.valid && tpDataPCQueue.io.deq.valid
  val tpDataIssueAllowed = confRespQueue.io.deq.bits.issue
  val tpDataNeedIssueMiss = tpDataQueue.io.deq.valid && tpDataSidebandValid &&
    tpDataIssueAllowed && tpDataQueue.io.deq.bits.length =/= 0.U
  tpDataQueue.io.deq.ready := (!do_sending || !enable) && tpDataSidebandValid &&
    (!tpDataNeedIssueMiss || confIssueMissQueue.io.enq.ready)
  confRespQueue.io.deq.ready := tpDataQueue.io.deq.fire
  tpDataPCQueue.io.deq.ready := tpDataQueue.io.deq.fire
  val tpDataIssueFire = tpDataQueue.io.deq.fire && (tpDataQueue.io.deq.bits.length =/= 0.U) && tpDataIssueAllowed && enable

  when(tpDataIssueFire) {
    sending_data := tpDataQueue.io.deq.bits.rawData
    sending_length := tpDataQueue.io.deq.bits.length
    sending_hitCount := tpDataQueue.io.deq.bits.hitCount
    sending_idx := 0.U
    do_sending := true.B
  }
  when(do_sending && (sending_throttle =/= tpThrottleCycles)) {
    sending_throttle := sending_throttle + 1.U
  }

  when(io.req.fire) {
    sending_idx := sending_idx + 1.U
    sending_throttle := 0.U
    if (debug) {
      printf("[L1TP] sending data: %x\n", current_sending_data)
    }
    when(sending_idx === (recordThres-1.U) || sending_idx === (sending_length - 1.U)) {
      do_sending := false.B
    }
  }

  // Hold a group and its current address until ready; queue pressure cannot replace it.
  when(!enable) {
    do_sending := false.B
    sending_throttle := 0.U
  }
  io.req.valid := enable && sending_valid
  io.req.bits.addr := current_sending_data
  io.req.bits.hitCount := sending_hitCount

  io.train.ready := enable && resetFinish && trainQueue.io.enq.ready && sampler.io.train.ready
  io.feedBack.ready := enable && resetFinish && confFeedbackQueue.io.enq.ready

  // confidence table
  val pfHit = io.feedBack.bits.hit
  val pfLate = io.feedBack.bits.late
  val pfMiss = false.B // pfMiss is charged on issue via confIssueMissQueue.
  confNewMetaQueue.io.enq.valid := metaWQueue.io.deq.valid && metaConfPendingQueue.io.enq.ready &&
    metaInstallReserveAvailable
  confNewMetaQueue.io.enq.bits.pc := metaWQueue.io.deq.bits.pc
  confNewMetaQueue.io.enq.bits.reqType := ConfReqType.newMeta
  confNewMetaQueue.io.enq.bits.pfHit := false.B
  confNewMetaQueue.io.enq.bits.pfLate := false.B
  confNewMetaQueue.io.enq.bits.pfMiss := false.B
  confNewMetaQueue.io.enq.bits.pfCnt := 0.U

  confIssueQueue.io.enq.valid := dataReadQueue.io.enq.fire
  confIssueQueue.io.enq.bits.pc := hashPC(train_s2.pc)
  confIssueQueue.io.enq.bits.reqType := ConfReqType.issue
  confIssueQueue.io.enq.bits.pfHit := false.B
  confIssueQueue.io.enq.bits.pfLate := false.B
  confIssueQueue.io.enq.bits.pfMiss := false.B
  confIssueQueue.io.enq.bits.pfCnt := 0.U

  confIssueMissQueue.io.enq.valid := tpDataIssueFire
  confIssueMissQueue.io.enq.bits.pc := tpDataPCQueue.io.deq.bits
  confIssueMissQueue.io.enq.bits.reqType := ConfReqType.feedback
  confIssueMissQueue.io.enq.bits.pfHit := false.B
  confIssueMissQueue.io.enq.bits.pfLate := false.B
  confIssueMissQueue.io.enq.bits.pfMiss := true.B
  confIssueMissQueue.io.enq.bits.pfCnt := tpDataQueue.io.deq.bits.length.pad(accConfWidth)

  confFeedbackQueue.io.enq.valid := io.feedBack.fire
  confFeedbackQueue.io.enq.bits.pc := hashPC(io.feedBack.bits.pc)
  confFeedbackQueue.io.enq.bits.reqType := ConfReqType.feedback
  confFeedbackQueue.io.enq.bits.pfHit := pfHit
  confFeedbackQueue.io.enq.bits.pfLate := pfLate
  confFeedbackQueue.io.enq.bits.pfMiss := pfMiss
  confFeedbackQueue.io.enq.bits.pfCnt := 1.U

  val confRespIsIssue = confTable.io.resp.valid && confTable.io.resp.bits.reqType === ConfReqType.issue
  val confRespIsNewMeta = confTable.io.resp.valid && confTable.io.resp.bits.reqType === ConfReqType.newMeta
  metaConfPendingQueue.io.deq.ready := confRespIsNewMeta
  metaInstallQueue.io.enq.valid := confRespIsNewMeta && metaConfPendingQueue.io.deq.valid &&
    confTable.io.resp.bits.metaUpdate
  metaInstallQueue.io.enq.bits := metaConfPendingQueue.io.deq.bits
  assert(metaConfPendingQueue.io.deq.valid || !confRespIsNewMeta)
  assert(metaInstallQueue.io.enq.ready || !metaInstallQueue.io.enq.valid)
  when(confNewMetaQueue.io.enq.fire && !confRespIsNewMeta) {
    pendingNewMetaRespCnt := pendingNewMetaRespCnt + 1.U
  }.elsewhen(!confNewMetaQueue.io.enq.fire && confRespIsNewMeta) {
    pendingNewMetaRespCnt := pendingNewMetaRespCnt - 1.U
  }

  confRespQueue.io.enq.valid := confRespIsIssue
  confRespQueue.io.enq.bits.issue := confTable.io.resp.bits.issue
  confRespQueue.io.enq.bits.metaUpdate := false.B
  confRespQueue.io.enq.bits.reqType := confTable.io.resp.bits.reqType
  when(confIssueQueue.io.enq.fire && !confRespQueue.io.deq.fire) {
    pendingConfRespCnt := pendingConfRespCnt + 1.U
  }.elsewhen(!confIssueQueue.io.enq.fire && confRespQueue.io.deq.fire) {
    pendingConfRespCnt := pendingConfRespCnt - 1.U
  }
  assert(confRespQueue.io.enq.ready || !confRespQueue.io.enq.valid)
  assert(pendingConfRespCnt <= (confReqQueueDepth + 1).U)
  assert(!confRespQueue.io.deq.fire || pendingConfRespCnt =/= 0.U)
  assert(pendingNewMetaRespCnt <= tpMetaWQueueDepth.U)
  assert(!confRespIsNewMeta || pendingNewMetaRespCnt =/= 0.U)

  /* Performance collection */
  val hitCountS1 = WireInit(0.U(hitCountWidth.W))
  hitCountS1 := hitCount_s1
  XSPerfHistogram("l1_tp_meta_hit_count", perfCnt = hitCountS1, enable = hit_s1 && s1_valid, start = 0, stop = 1 << hitCountWidth, step = 1)
  XSPerfAccumulate("l1_tpHit", tpMetaResetQueue.io.enq.valid)

  XSPerfHistogram("l1_tp_pf_count", perfCnt = io.req.bits.hitCount, enable = io.req.valid, start = 0, stop = 1 << hitCountWidth, step = 1)

  val trainDB = ChiselDB.createTable("l1_tptrain", new trainBundle(), basicDB = true)
  val trainPt = Wire(new trainBundle())
  trainPt.vaddr := train_s2.vaddr
  trainPt.paddr := train_s2.addr
  trainPt.hit := train_s2.hit
  trainPt.prefetched := train_s2.prefetched
  trainPt.metahit := hit_s2
  trainPt.pc := hashPC(train_s2.pc)

  val sendDB = ChiselDB.createTable("l1_tpsend", new tpDataEntry(), basicDB = true)
  val sendPt = Wire(new tpDataEntry())
  sendPt.rawData := tpDataQueue.io.deq.bits.rawData
  sendPt.length := tpDataQueue.io.deq.bits.length
  sendPt.hitCount := tpDataQueue.io.deq.bits.hitCount

  trainDB.log(trainPt, s2_valid, "", clock, reset)
  sendDB.log(sendPt, tpDataIssueFire, "", clock, reset)
}
