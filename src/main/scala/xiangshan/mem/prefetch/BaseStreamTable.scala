/***************************************************************************************
* Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
***************************************************************************************/

package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.ValidPseudoLRU
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, GTimer, XSPerfAccumulate, XSPerfHistogram}
import xiangshan._

class BaseStreamTableEntry(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  val regionTag = UInt(REGION_TAG_BITS.W)
  val bitVec = UInt(BIT_VEC_WITDH.W)
  val count = UInt(log2Ceil(BIT_VEC_WITDH + 1).W)
  val active = Bool()
  val decr = Bool()
  val valid = Bool()
}

/** One event row for the optional BSmT training trace. */
class BaseStreamTableTrainDBEntry(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  val timeCnt = UInt(64.W)
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val mdtHit = Bool()
  val mdtIdx = UInt(log2Up(MDP_MDT_ENTRIES).W)
  val mdtConf = UInt(MDP_COUNTER_BITS.W)
  val mdtImm = UInt(MDP_IMM_BITS.W)
  val mdtImmConf = UInt(MDP_COUNTER_BITS.W)
  val mdtChainImm = UInt(MDP_IMM_BITS.W)
  val mdtChainValid = Bool()
  val regionTag = UInt(REGION_TAG_BITS.W)
  val regionBit = UInt(REGION_BITS.W)
  val tableHit = Bool()
  val tableIdx = UInt(log2Up(MDP_BSMT_ENTRIES).W)
  val plusNeighborActive = Bool()
  val minusNeighborActive = Bool()
  val oldBitVec = UInt(BIT_VEC_WITDH.W)
  val oldCount = UInt(log2Ceil(BIT_VEC_WITDH + 1).W)
  val oldActive = Bool()
  val oldDecr = Bool()
  val newLine = Bool()
  val newBitVec = UInt(BIT_VEC_WITDH.W)
  val newCount = UInt(log2Ceil(BIT_VEC_WITDH + 1).W)
  val newActive = Bool()
  val newDecr = Bool()
  val activatedByThreshold = Bool()
  val activatedByNeighbor = Bool()
  val ambiguousNeighbors = Bool()
  val allocated = Bool()
  val generated = Bool()
  val prefetchVaddr = UInt(VAddrBits.W)
}

/** MDP Base Stream Table (BSmT).
  *
  * This is a sixteen-entry, MDP-gated version of the region bit-vector stream
  * detector in L1StreamPrefetcher.  It intentionally emits one exact base-load
  * address rather than a region request: the line is fetched by the common MDP
  * buffer, while the byte offset is retained for the subsequent chasing load.
  *
  * Pipeline correspondence with MemoryDependencePrefetcher:
  *   - s0 samples `train` and `mdtInfo`, then queries current/adjacent regions;
  *   - s1 is the only BSmT update/allocation stage and decides whether to send;
  *   - s2 emits `streamPf`, aligned with BSeT `stridePf` at MDP train1 s2.
  *
  * The default one-line lookahead is a correctness-first policy.  It is a
  * constructor parameter so a configuration can tune timeliness without
  * changing the training algorithm.
  */
class BaseStreamTable(
  entryCount: Int = 16,
  depthBlocks: Int = 1
)(implicit p: Parameters) extends XSModule with HasMdpParameters {
  require(entryCount == 16, "MDP BaseStreamTable must contain exactly sixteen entries")
  require(depthBlocks >= 1, "BSmT lookahead depth must be at least one cache block")

  private val activeThreshold = BIT_VEC_WITDH - 4
  require(activeThreshold >= 1 && activeThreshold <= BIT_VEC_WITDH)

  val io = IO(new Bundle {
    val train = Flipped(DecoupledIO(new TrainReqBundle))
    val mdtInfo = Input(new MdpMdtInfoBundle)
    // See BSeT: a concurrent MDT write may invalidate a one-shot chain hop.
    val mdtWriteActive = Input(Bool())
    val streamPf = ValidIO(new MdpSourcePrefetchReq)
  })

  private val entries = RegInit(VecInit(Seq.fill(entryCount)(
    0.U.asTypeOf(new BaseStreamTableEntry)
  )))
  private val replacer = new ValidPseudoLRU(entryCount)

  // s0: exact region matching avoids the false activation possible with only
  // the hashed region tag used by the larger generic stream prefetcher.
  private val s0RegionTag = get_region_tag(io.train.bits.vaddr)
  private val s0RegionBit = get_region_bits(io.train.bits.vaddr)
  private val s0PlusRegionTag = s0RegionTag + 1.U
  private val s0MinusRegionTag = s0RegionTag - 1.U
  private val s0MatchVec = VecInit(entries.map(e => e.valid && e.regionTag === s0RegionTag))
  private val s0PlusMatchVec = VecInit(entries.map(e => e.valid && e.regionTag === s0PlusRegionTag))
  private val s0MinusMatchVec = VecInit(entries.map(e => e.valid && e.regionTag === s0MinusRegionTag))
  private val s0Hit = s0MatchVec.asUInt.orR
  private val s0PlusHit = s0PlusMatchVec.asUInt.orR
  private val s0PlusActive = Mux1H(
    s0PlusMatchVec,
    entries.map(e => e.active && e.count >= activeThreshold.U)
  )
  private val s0MinusActive = Mux1H(
    s0MinusMatchVec,
    entries.map(e => e.active && e.count >= activeThreshold.U)
  )
  private val invalidVec = VecInit(entries.map(!_.valid))
  private val replaceCandidates = Mux(
    invalidVec.asUInt.orR,
    invalidVec,
    VecInit(Seq.fill(entryCount)(true.B))
  )
  private val replaceIdx = replacer.way(replaceCandidates.reverse)._2
  // ValidPseudoLRU's selected-way width is not inferred until elaboration.
  // Give the table index an explicit width so RegEnable below is well typed.
  private val s0Idx = Wire(UInt(log2Up(entryCount).W))
  s0Idx := Mux(s0Hit, OHToUInt(s0MatchVec), replaceIdx)
  private val s0Entry = entries(s0Idx)
  private val s0BitOH = UIntToOH(s0RegionBit, BIT_VEC_WITDH)

  // s1: capture train/mdtInfo atomically and perform the sole table write.
  private val s1Valid = RegInit(false.B)
  io.train.ready := !s1Valid
  private val s1Train = RegEnable(io.train.bits, io.train.fire)
  private val s1MdtInfo = RegEnable(io.mdtInfo, io.train.fire)
  private val s1RegionTag = RegEnable(s0RegionTag, io.train.fire)
  private val s1RegionBit = RegEnable(s0RegionBit, io.train.fire)
  private val s1BitOH = RegEnable(s0BitOH, io.train.fire)
  private val s1Hit = RegEnable(s0Hit, io.train.fire)
  private val s1PlusHit = RegEnable(s0PlusHit, io.train.fire)
  private val s1PlusActive = RegEnable(s0PlusActive, io.train.fire)
  private val s1MinusActive = RegEnable(s0MinusActive, io.train.fire)
  private val s1Idx = RegEnable(s0Idx, io.train.fire)
  private val s1OldEntry = RegEnable(s0Entry, io.train.fire)

  when(io.train.fire) {
    s1Valid := true.B
  }.elsewhen(s1Valid) {
    s1Valid := false.B
  }

  private val oldLineSeen = s1Hit && (s1OldEntry.bitVec & s1BitOH).orR
  private val newLine = !oldLineSeen
  private val incrementedCount = Mux(
    s1OldEntry.count === BIT_VEC_WITDH.U,
    s1OldEntry.count,
    s1OldEntry.count + 1.U
  )
  private val newCount = Mux(!s1Hit, 1.U, Mux(newLine, incrementedCount, s1OldEntry.count))
  private val newBitVec = Mux(!s1Hit, s1BitOH, s1OldEntry.bitVec | s1BitOH)
  private val anyActiveNeighbor = s1PlusActive || s1MinusActive
  private val ambiguousNeighbors = s1PlusActive && s1MinusActive
  private val oldActive = s1Hit && s1OldEntry.active
  private val activatedByThreshold = !oldActive && newCount >= activeThreshold.U
  private val activatedByNeighbor = !oldActive && anyActiveNeighbor
  private val newActive = Mux(
    !s1Hit,
    anyActiveNeighbor || newCount >= activeThreshold.U,
    s1OldEntry.active || newCount >= activeThreshold.U || anyActiveNeighbor
  )
  // Match L1StreamPrefetcher direction learning: any observed +1-region entry
  // is decreasing evidence, and once learned the direction remains decreasing.
  private val newDecr = Mux(s1Hit, s1OldEntry.decr || s1PlusHit, s1PlusHit)
  private val canPrefetch = s1Valid && s1MdtInfo.hit &&
    s1MdtInfo.conf >= MDP_CONF_THRESHOLD.U &&
    s1MdtInfo.immConf >= MDP_IMM_CONF_THRESHOLD.U && newLine && newActive

  when(s1Valid && s1MdtInfo.hit) {
    val entry = entries(s1Idx)
    entry.regionTag := s1RegionTag
    entry.bitVec := newBitVec
    entry.count := newCount
    entry.active := newActive
    entry.decr := newDecr
    entry.valid := true.B
    replacer.access(s1Idx)
  }

  // s2: retain the original byte offset while looking ahead by whole lines.
  private val s2Valid = RegNext(canPrefetch, false.B)
  private val s2Train = RegEnable(s1Train, canPrefetch)
  private val s2Imm = RegEnable(s1MdtInfo.imm, canPrefetch)
  private val s2ChainImm = RegEnable(s1MdtInfo.chainImm, canPrefetch)
  // Match BSeT: a train0 write concurrent with s1 invalidates this old hop.
  // Keep the stream carrier, while preventing the stale second hop from s2.
  private val s2ChainValid = RegEnable(s1MdtInfo.chainValid && !io.mdtWriteActive, canPrefetch)
  private val s2ChainLoadSize = RegEnable(s1MdtInfo.chainLoadSize, canPrefetch)
  private val s2ChainLoadUnsigned = RegEnable(s1MdtInfo.chainLoadUnsigned, canPrefetch)
  private val s2ChainSendable = s2ChainValid && !io.mdtWriteActive
  private val s2Decr = RegEnable(newDecr, canPrefetch)
  private val depthBytes = (depthBlocks * blockBytes).U(VAddrBits.W)
  private val s2PrefetchVaddr = Mux(
    s2Decr,
    s2Train.vaddr - depthBytes,
    s2Train.vaddr + depthBytes
  )(VAddrBits - 1, 0)

  io.streamPf.valid := s2Valid
  io.streamPf.bits.triggerPC := s2Train.pc
  io.streamPf.bits.triggerVA := s2Train.vaddr
  io.streamPf.bits.prefetchVA := s2PrefetchVaddr
  io.streamPf.bits.pfSource := L1_HW_PREFETCH_MDP_STREAM
  io.streamPf.bits.mdpPfHint := true.B
  io.streamPf.bits.mdpImm := s2Imm
  io.streamPf.bits.mdpVaddr := s2PrefetchVaddr
  io.streamPf.bits.mdpLoadSize := s2Train.loadSize
  io.streamPf.bits.mdpLoadUnsigned := s2Train.loadUnsigned
  io.streamPf.bits.mdpChainImm := Mux(s2ChainSendable, s2ChainImm, 0.U)
  io.streamPf.bits.mdpChainValid := s2ChainSendable
  io.streamPf.bits.mdpChainLoadSize := Mux(s2ChainSendable, s2ChainLoadSize, 0.U)
  io.streamPf.bits.mdpChainLoadUnsigned := s2ChainSendable && s2ChainLoadUnsigned
  io.streamPf.bits.mdpOrigin := MdpPfOrigin.stream

  // Optional detailed trace: it is compiled out unless explicitly promoted to
  // a basic DB for a dedicated smoke-test build.
  private val trainTable = ChiselDB.createTable(
    s"mdpBSmTTrain_hart${p(XSCoreParamsKey).HartId}",
    new BaseStreamTableTrainDBEntry,
    basicDB = false
  )
  private val trainLog = Wire(new BaseStreamTableTrainDBEntry)
  private val s1TargetVaddr = Mux(
    newDecr,
    s1Train.vaddr - depthBytes,
    s1Train.vaddr + depthBytes
  )(VAddrBits - 1, 0)
  trainLog.timeCnt := GTimer()
  trainLog.pc := s1Train.pc
  trainLog.vaddr := s1Train.vaddr
  trainLog.mdtHit := s1MdtInfo.hit
  trainLog.mdtIdx := s1MdtInfo.index
  trainLog.mdtConf := s1MdtInfo.conf
  trainLog.mdtImm := s1MdtInfo.imm
  trainLog.mdtImmConf := s1MdtInfo.immConf
  trainLog.mdtChainImm := s1MdtInfo.chainImm
  trainLog.mdtChainValid := s1MdtInfo.chainValid
  trainLog.regionTag := s1RegionTag
  trainLog.regionBit := s1RegionBit
  trainLog.tableHit := s1Hit
  trainLog.tableIdx := s1Idx
  trainLog.plusNeighborActive := s1PlusActive
  trainLog.minusNeighborActive := s1MinusActive
  trainLog.oldBitVec := s1OldEntry.bitVec
  trainLog.oldCount := s1OldEntry.count
  trainLog.oldActive := s1OldEntry.active
  trainLog.oldDecr := s1OldEntry.decr
  trainLog.newLine := newLine
  trainLog.newBitVec := Mux(s1MdtInfo.hit, newBitVec, s1OldEntry.bitVec)
  trainLog.newCount := Mux(s1MdtInfo.hit, newCount, s1OldEntry.count)
  trainLog.newActive := Mux(s1MdtInfo.hit, newActive, s1OldEntry.active)
  trainLog.newDecr := Mux(s1MdtInfo.hit, newDecr, s1OldEntry.decr)
  trainLog.activatedByThreshold := s1MdtInfo.hit && activatedByThreshold
  trainLog.activatedByNeighbor := s1MdtInfo.hit && activatedByNeighbor
  trainLog.ambiguousNeighbors := ambiguousNeighbors
  trainLog.allocated := s1MdtInfo.hit && !s1Hit
  trainLog.generated := canPrefetch
  trainLog.prefetchVaddr := s1TargetVaddr
  trainTable.log(trainLog, s1Valid, "mdp-bsmt", clock, reset)

  assert(PopCount(s0MatchVec) <= 1.U, "BSmT current-region lookup must match at most one entry")
  assert(PopCount(s0PlusMatchVec) <= 1.U, "BSmT +1-region lookup must match at most one entry")
  assert(PopCount(s0MinusMatchVec) <= 1.U, "BSmT -1-region lookup must match at most one entry")
  when(io.train.fire) {
    assert(io.mdtInfo.hashPC === pc_hash_tag(io.train.bits.pc),
      "BSmT train and MDT lookup must refer to the same PC")
  }
  when(s1Valid && s1MdtInfo.hit) {
    assert(newCount >= 1.U && newCount <= BIT_VEC_WITDH.U,
      "BSmT distinct-line count must stay within the region width")
    assert(PopCount(newBitVec) === newCount,
      "BSmT bit vector and distinct-line count must agree")
  }
  when(io.streamPf.valid) {
    assert(io.streamPf.bits.mdpPfHint, "BSmT prefetch must carry the MDP hint marker")
    assert(io.streamPf.bits.pfSource === L1_HW_PREFETCH_MDP_STREAM,
      "BSmT prefetch must be classified as mdpStream")
    assert(
      io.streamPf.bits.prefetchVA(DCacheLineOffset - 1, 0) ===
        io.streamPf.bits.triggerVA(DCacheLineOffset - 1, 0),
      "BSmT prefetch must preserve the base load byte offset"
    )
  }

  XSPerfAccumulate("mdp_bsmt_train_valid", io.train.valid)
  XSPerfAccumulate("mdp_bsmt_train_fire", io.train.fire)
  XSPerfAccumulate("mdp_bsmt_train_blocked", io.train.valid && !io.train.ready)
  XSPerfAccumulate("mdp_bsmt_mdt_hit", s1Valid && s1MdtInfo.hit)
  XSPerfAccumulate("mdp_bsmt_mdt_miss_skip", s1Valid && !s1MdtInfo.hit)
  XSPerfAccumulate("mdp_bsmt_imm_unstable_skip", s1Valid && s1MdtInfo.hit &&
    s1MdtInfo.conf >= MDP_CONF_THRESHOLD.U &&
    s1MdtInfo.immConf < MDP_IMM_CONF_THRESHOLD.U)
  XSPerfAccumulate("mdp_bsmt_entry_hit", s1Valid && s1MdtInfo.hit && s1Hit)
  XSPerfAccumulate("mdp_bsmt_entry_alloc", s1Valid && s1MdtInfo.hit && !s1Hit)
  XSPerfAccumulate("mdp_bsmt_new_line", s1Valid && s1MdtInfo.hit && newLine)
  XSPerfAccumulate("mdp_bsmt_duplicate_line", s1Valid && s1MdtInfo.hit && !newLine)
  XSPerfAccumulate("mdp_bsmt_plus_neighbor_active", s1Valid && s1MdtInfo.hit && s1PlusActive)
  XSPerfAccumulate("mdp_bsmt_minus_neighbor_active", s1Valid && s1MdtInfo.hit && s1MinusActive)
  XSPerfAccumulate("mdp_bsmt_ambiguous_neighbors", s1Valid && s1MdtInfo.hit && ambiguousNeighbors)
  XSPerfAccumulate("mdp_bsmt_activate_threshold", s1Valid && s1MdtInfo.hit && activatedByThreshold)
  XSPerfAccumulate("mdp_bsmt_activate_neighbor", s1Valid && s1MdtInfo.hit && activatedByNeighbor)
  XSPerfAccumulate("mdp_bsmt_pf_generated", io.streamPf.valid)
  XSPerfAccumulate("mdp_bsmt_chain_capture_write_block",
    canPrefetch && s1MdtInfo.chainValid && io.mdtWriteActive)
  XSPerfAccumulate("mdp_bsmt_chain_write_block",
    io.streamPf.valid && s2ChainValid && io.mdtWriteActive)
  XSPerfAccumulate("mdp_bsmt_pf_generated_incr", io.streamPf.valid && !s2Decr)
  XSPerfAccumulate("mdp_bsmt_pf_generated_decr", io.streamPf.valid && s2Decr)
  XSPerfHistogram(
    "mdp_bsmt_valid_entries",
    PopCount(entries.map(_.valid)),
    true.B,
    0,
    entryCount + 1,
    1
  )
}
