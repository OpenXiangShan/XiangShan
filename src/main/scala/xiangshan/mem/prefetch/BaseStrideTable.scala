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
import utility.{ChiselDB, GTimer, XSPerfAccumulate}
import xiangshan._

/** MDT lookup result sampled together with a train1 request.
  *
  * `hashPC` is carried for an alignment assertion at the table boundary.  The
  * entry index is not needed by the datapath, but makes table-training traces
  * directly joinable with MDT traces.
  */
class MdpMdtInfoBundle(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  val hit = Bool()
  val index = UInt(log2Up(MDP_MDT_ENTRIES).W)
  val hashPC = UInt(HASH_TAG_WIDTH.W)
  val conf = UInt(MDP_COUNTER_BITS.W)
  val imm = UInt(MDP_IMM_BITS.W)
  val chainImm = UInt(MDP_IMM_BITS.W)
  val chainValid = Bool()
  val chainLoadSize = UInt(2.W)
  val chainLoadUnsigned = Bool()
}

class BaseStrideTableEntry(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  val hashPC = UInt(HASH_TAG_WIDTH.W)
  val prevVaddr = UInt(VAddrBits.W)
  val stride = UInt(VAddrBits.W)
  val decr = Bool()
  val conf = UInt(MDP_COUNTER_BITS.W)
  val valid = Bool()
}

/** One event row for the optional BSeT training trace. */
class BaseStrideTableTrainDBEntry(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  val timeCnt = UInt(64.W)
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val mdtHit = Bool()
  val mdtIdx = UInt(log2Up(MDP_MDT_ENTRIES).W)
  val mdtConf = UInt(MDP_COUNTER_BITS.W)
  val mdtImm = UInt(MDP_IMM_BITS.W)
  val mdtChainImm = UInt(MDP_IMM_BITS.W)
  val mdtChainValid = Bool()
  val tableHit = Bool()
  val tableIdx = UInt(log2Up(MDP_BST_ENTRIES).W)
  val oldPrevVaddr = UInt(VAddrBits.W)
  val oldStride = UInt(VAddrBits.W)
  val oldDecr = Bool()
  val oldConf = UInt(MDP_COUNTER_BITS.W)
  val delta = UInt(VAddrBits.W)
  val deltaDecr = Bool()
  val strideValid = Bool()
  val strideMatch = Bool()
  val allocated = Bool()
  val replacedStride = Bool()
  val newPrevVaddr = UInt(VAddrBits.W)
  val newStride = UInt(VAddrBits.W)
  val newDecr = Bool()
  val newConf = UInt(MDP_COUNTER_BITS.W)
  val generated = Bool()
  val prefetchVaddr = UInt(VAddrBits.W)
}

/** MDP Base Stride Table (BSeT).
  *
  * Pipeline correspondence with MemoryDependencePrefetcher:
  *   - s0: `train` and the parallel `mdtInfo` are sampled atomically; BSeT is
  *     queried and the observed address delta is calculated.
  *   - s1: the selected entry is updated or allocated.  This is the sole BSeT
  *     write stage and corresponds to MDP train1 s1.
  *   - s2: a confident matching stride becomes `stridePf`, corresponding to
  *     MDP train1 s2.  The target remains an exact byte address so a later MSHR
  *     refill extracts the dependent load from the correct byte offset.
  *
  * A one-cycle bubble between accepted training requests preserves read/write
  * ordering for consecutive updates to the same entry.
  */
class BaseStrideTable(implicit p: Parameters) extends XSModule with HasMdpParameters {
  require(
    MDP_BST_ENTRIES >= 2 && (MDP_BST_ENTRIES & (MDP_BST_ENTRIES - 1)) == 0,
    "BSeT entry count must be a power of two and at least two"
  )

  val io = IO(new Bundle {
    val train = Flipped(DecoupledIO(new TrainReqBundle))
    val mdtInfo = Input(new MdpMdtInfoBundle)
    val stridePf = ValidIO(new MdpSourcePrefetchReq)
  })

  private val entries = RegInit(VecInit(Seq.fill(MDP_BST_ENTRIES)(
    0.U.asTypeOf(new BaseStrideTableEntry)
  )))
  private val replacer = new ValidPseudoLRU(MDP_BST_ENTRIES)

  // s0: query BSeT and calculate the absolute address delta.
  private val s0Hash = pc_hash_tag(io.train.bits.pc)
  private val s0MatchVec = VecInit(entries.map(e => e.valid && e.hashPC === s0Hash))
  private val s0Hit = s0MatchVec.asUInt.orR
  private val invalidVec = VecInit(entries.map(!_.valid))
  private val replaceCandidates = Mux(
    invalidVec.asUInt.orR,
    invalidVec,
    VecInit(Seq.fill(MDP_BST_ENTRIES)(true.B))
  )
  private val replaceIdx = replacer.way(replaceCandidates.reverse)._2
  // ValidPseudoLRU's selected-way width is not inferred until elaboration.
  // Give the table index an explicit width so RegEnable below is well typed.
  private val s0Idx = Wire(UInt(log2Up(MDP_BST_ENTRIES).W))
  s0Idx := Mux(s0Hit, OHToUInt(s0MatchVec), replaceIdx)
  private val s0Entry = entries(s0Idx)
  private val s0DeltaPlus = io.train.bits.vaddr -& s0Entry.prevVaddr
  private val s0Decr = s0DeltaPlus(VAddrBits)
  private val s0Delta = Mux(
    s0Decr,
    s0Entry.prevVaddr - io.train.bits.vaddr,
    s0DeltaPlus(VAddrBits - 1, 0)
  )
  private val s0StrideValid = s0Delta =/= 0.U
  private val s0StrideMatch = s0Delta === s0Entry.stride && s0Decr === s0Entry.decr

  // s1: capture exactly one train/mdtInfo pair and serialize the table write.
  private val s1Valid = RegInit(false.B)
  io.train.ready := !s1Valid
  private val s1Train = RegEnable(io.train.bits, io.train.fire)
  private val s1MdtInfo = RegEnable(io.mdtInfo, io.train.fire)
  private val s1Hash = RegEnable(s0Hash, io.train.fire)
  private val s1Hit = RegEnable(s0Hit, io.train.fire)
  private val s1Idx = RegEnable(s0Idx, io.train.fire)
  private val s1OldEntry = RegEnable(s0Entry, io.train.fire)
  private val s1Delta = RegEnable(s0Delta, io.train.fire)
  private val s1Decr = RegEnable(s0Decr, io.train.fire)
  private val s1StrideValid = RegEnable(s0StrideValid, io.train.fire)
  private val s1StrideMatch = RegEnable(s0StrideMatch, io.train.fire)

  when(io.train.fire) {
    s1Valid := true.B
  }.elsewhen(s1Valid) {
    s1Valid := false.B
  }

  private val confInc = Mux(
    s1OldEntry.conf === mdpCounterMax.U,
    s1OldEntry.conf,
    s1OldEntry.conf + 1.U
  )
  private val confDec = Mux(s1OldEntry.conf === 0.U, 0.U, s1OldEntry.conf - 1.U)
  private val replaceStride = s1StrideValid && !s1StrideMatch && s1OldEntry.conf <= 1.U
  private val newStride = Mux(!s1Hit, 0.U, Mux(replaceStride, s1Delta, s1OldEntry.stride))
  private val newDecr = Mux(!s1Hit, false.B, Mux(replaceStride, s1Decr, s1OldEntry.decr))
  private val newConf = Mux(
    !s1Hit,
    0.U,
    Mux(!s1StrideValid, s1OldEntry.conf, Mux(s1StrideMatch, confInc, confDec))
  )

  private val canPrefetch = s1Valid && s1MdtInfo.hit &&
    s1MdtInfo.conf >= MDP_CONF_THRESHOLD.U && s1Hit &&
    s1StrideValid && s1StrideMatch && confInc >= MDP_BST_CONF_THRESHOLD.U

  when(s1Valid && s1MdtInfo.hit) {
    val entry = entries(s1Idx)
    when(s1Hit) {
      when(s1StrideValid) {
        when(s1StrideMatch) {
          entry.conf := confInc
        }.otherwise {
          entry.conf := confDec
          when(s1OldEntry.conf <= 1.U) {
            entry.stride := s1Delta
            entry.decr := s1Decr
          }
        }
      }
      entry.prevVaddr := s1Train.vaddr
    }.otherwise {
      entry.hashPC := s1Hash
      entry.prevVaddr := s1Train.vaddr
      entry.stride := 0.U
      entry.decr := false.B
      entry.conf := 0.U
      entry.valid := true.B
    }
    replacer.access(s1Idx)
  }

  // s2: register the verified old stride and calculate the exact target.
  private val s2Valid = RegNext(canPrefetch, false.B)
  private val s2Train = RegEnable(s1Train, canPrefetch)
  private val s2Stride = RegEnable(s1OldEntry.stride, canPrefetch)
  private val s2Decr = RegEnable(s1OldEntry.decr, canPrefetch)
  private val s2Imm = RegEnable(s1MdtInfo.imm, canPrefetch)
  private val s2ChainImm = RegEnable(s1MdtInfo.chainImm, canPrefetch)
  private val s2ChainValid = RegEnable(s1MdtInfo.chainValid, canPrefetch)
  private val s2ChainLoadSize = RegEnable(s1MdtInfo.chainLoadSize, canPrefetch)
  private val s2ChainLoadUnsigned = RegEnable(s1MdtInfo.chainLoadUnsigned, canPrefetch)
  private val s2PrefetchVaddr = Mux(
    s2Decr,
    s2Train.vaddr - s2Stride,
    s2Train.vaddr + s2Stride
  )(VAddrBits - 1, 0)

  io.stridePf.valid := s2Valid
  io.stridePf.bits.triggerPC := s2Train.pc
  io.stridePf.bits.triggerVA := s2Train.vaddr
  io.stridePf.bits.prefetchVA := s2PrefetchVaddr
  io.stridePf.bits.pfSource := L1_HW_PREFETCH_MDP_STRIDE
  io.stridePf.bits.mdpPfHint := true.B
  io.stridePf.bits.mdpImm := s2Imm
  io.stridePf.bits.mdpVaddr := s2PrefetchVaddr
  io.stridePf.bits.mdpLoadSize := s2Train.loadSize
  io.stridePf.bits.mdpLoadUnsigned := s2Train.loadUnsigned
  io.stridePf.bits.mdpChainImm := s2ChainImm
  io.stridePf.bits.mdpChainValid := s2ChainValid
  io.stridePf.bits.mdpChainLoadSize := s2ChainLoadSize
  io.stridePf.bits.mdpChainLoadUnsigned := s2ChainLoadUnsigned
  io.stridePf.bits.mdpOrigin := MdpPfOrigin.stride

  // Optional detailed trace: BSeT/BSmT tables are intentionally not basic DBs.
  private val trainTable = ChiselDB.createTable(
    s"mdpBSeTTrain_hart${p(XSCoreParamsKey).HartId}",
    new BaseStrideTableTrainDBEntry,
    basicDB = false
  )
  private val trainLog = Wire(new BaseStrideTableTrainDBEntry)
  trainLog.timeCnt := GTimer()
  trainLog.pc := s1Train.pc
  trainLog.vaddr := s1Train.vaddr
  trainLog.mdtHit := s1MdtInfo.hit
  trainLog.mdtIdx := s1MdtInfo.index
  trainLog.mdtConf := s1MdtInfo.conf
  trainLog.mdtImm := s1MdtInfo.imm
  trainLog.mdtChainImm := s1MdtInfo.chainImm
  trainLog.mdtChainValid := s1MdtInfo.chainValid
  trainLog.tableHit := s1Hit
  trainLog.tableIdx := s1Idx
  trainLog.oldPrevVaddr := s1OldEntry.prevVaddr
  trainLog.oldStride := s1OldEntry.stride
  trainLog.oldDecr := s1OldEntry.decr
  trainLog.oldConf := s1OldEntry.conf
  trainLog.delta := s1Delta
  trainLog.deltaDecr := s1Decr
  trainLog.strideValid := s1StrideValid
  trainLog.strideMatch := s1StrideMatch
  trainLog.allocated := s1MdtInfo.hit && !s1Hit
  trainLog.replacedStride := s1MdtInfo.hit && s1Hit && replaceStride
  trainLog.newPrevVaddr := Mux(s1MdtInfo.hit, s1Train.vaddr, s1OldEntry.prevVaddr)
  trainLog.newStride := Mux(s1MdtInfo.hit, newStride, s1OldEntry.stride)
  trainLog.newDecr := Mux(s1MdtInfo.hit, newDecr, s1OldEntry.decr)
  trainLog.newConf := Mux(s1MdtInfo.hit, newConf, s1OldEntry.conf)
  trainLog.generated := canPrefetch
  trainLog.prefetchVaddr := Mux(
    s1OldEntry.decr,
    s1Train.vaddr - s1OldEntry.stride,
    s1Train.vaddr + s1OldEntry.stride
  )(VAddrBits - 1, 0)
  trainTable.log(trainLog, s1Valid, "mdp-bset", clock, reset)

  assert(PopCount(s0MatchVec) <= 1.U, "BSeT lookup must match at most one entry")
  when(io.train.fire) {
    assert(io.mdtInfo.hashPC === s0Hash, "BSeT train and MDT lookup must refer to the same PC")
  }
  when(io.stridePf.valid) {
    assert(io.stridePf.bits.mdpPfHint, "BSeT prefetch must carry the MDP hint marker")
    assert(io.stridePf.bits.pfSource === L1_HW_PREFETCH_MDP_STRIDE,
      "BSeT prefetch must be classified as mdpStride")
  }

  XSPerfAccumulate("mdp_bset_train_valid", io.train.valid)
  XSPerfAccumulate("mdp_bset_train_fire", io.train.fire)
  XSPerfAccumulate("mdp_bset_train_blocked", io.train.valid && !io.train.ready)
  XSPerfAccumulate("mdp_bset_mdt_hit", s1Valid && s1MdtInfo.hit)
  XSPerfAccumulate("mdp_bset_mdt_miss_skip", s1Valid && !s1MdtInfo.hit)
  XSPerfAccumulate("mdp_bset_entry_hit", s1Valid && s1MdtInfo.hit && s1Hit)
  XSPerfAccumulate("mdp_bset_entry_alloc", s1Valid && s1MdtInfo.hit && !s1Hit)
  XSPerfAccumulate("mdp_bset_delta_zero", s1Valid && s1MdtInfo.hit && s1Hit && !s1StrideValid)
  XSPerfAccumulate("mdp_bset_stride_match", s1Valid && s1MdtInfo.hit && s1Hit && s1StrideValid && s1StrideMatch)
  XSPerfAccumulate("mdp_bset_stride_mismatch", s1Valid && s1MdtInfo.hit && s1Hit && s1StrideValid && !s1StrideMatch)
  XSPerfAccumulate("mdp_bset_conf_inc", s1Valid && s1MdtInfo.hit && s1Hit && s1StrideValid && s1StrideMatch)
  XSPerfAccumulate("mdp_bset_conf_dec", s1Valid && s1MdtInfo.hit && s1Hit && s1StrideValid && !s1StrideMatch)
  XSPerfAccumulate("mdp_bset_stride_replace", s1Valid && s1MdtInfo.hit && s1Hit && replaceStride)
  XSPerfAccumulate("mdp_bset_pf_generated", io.stridePf.valid)
}
