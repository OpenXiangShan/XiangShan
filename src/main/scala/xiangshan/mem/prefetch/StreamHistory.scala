/***************************************************************************************
 * Copyright (c) 2026 Beijing Institute of Open Source Chip
 * XiangShan is licensed under Mulan PSL v2.
 * http://license.coscl.org.cn/MulanPSL2
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND.
 ***************************************************************************************/
package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters
import utility._

/** Compile-time policies; there is no runtime algorithm mux or duplicate table. */
case class StreamHistoryParams(policy: Int = 1) {
  require(policy >= 1 && policy <= 3)
  val entries    = if (policy == 3) 32 else 64
  val ways       = 4
  val sets       = entries / ways
  val setBits    = log2Ceil(sets)
  val indexBits  = log2Ceil(entries)
  val lengthBits = if (policy == 2) 10 else 8
  def tagBits(vaBits: Int): Int = if (policy == 1) vaBits - 1 - setBits else 24
  def entryBits(vaBits: Int): Int =
    tagBits(vaBits) + (vaBits - 10) + 2 * lengthBits + 14 + (if (policy == 1) 0 else lengthBits)
  // Context identity (3 x 64b + V + privilege), conservative PC transport,
  // two pointer/epoch/valid/progress stages and spare control bits are charged.
  def storageBits(vaBits: Int): Int =
    entries * entryBits(vaBits) + sets * 3 + 2 * (indexBits + 4) + 195 + 500 + 16
}

case object StreamHistoryKey extends Field[Option[StreamHistoryParams]](Some(StreamHistoryParams()))

class StreamHistoryEntry(c: StreamHistoryParams, vaBits: Int) extends Bundle {
  val valid           = Bool()
  val tag             = UInt(c.tagBits(vaBits).W)
  val frontier        = UInt((vaBits - 10).W)
  val span            = UInt(c.lengthBits.W) // distance from first region, not covered-region count
  val offset          = UInt(4.W)
  val streamDirection = UInt(2.W)            // 0 unknown, 1 increasing, 2 decreasing
  val censored        = Bool()
  val overflow        = Bool()
  val epoch           = UInt(2.W)
  val length          = UInt(c.lengthBits.W)
  val second          = if (c.policy != 1) Some(UInt(c.lengthBits.W)) else None
  val historyDecr     = Bool()
  val confidence      = UInt(2.W)
}

class StreamHistoryCandidate(vaBits: Int) extends Bundle {
  val valid  = Bool()
  val region = UInt((vaBits - 10).W)
  val bitmap = UInt(16.W)
}

/** One accepted training request per cycle. Query is aligned to native Stream S2.
  * All hits observe demand, including cache hits; only native-eligible misses/hits
  * allocate new PC entries. Never modifies the native bitmap/count/activation.
  */
class StreamHistory(c: StreamHistoryParams, vaBits: Int)(implicit p: Parameters) extends Module {
  require(vaBits == 50, "Re-audit history/context/PC storage before changing VA width")
  require(c.storageBits(vaBits) <= 8192, "Stream history exceeds 1 KiB per core")
  val regionBits = vaBits - 10
  val io = IO(new Bundle {
    val train = Flipped(Valid(new Bundle {
      val pc       = UInt(vaBits.W)
      val vaddr    = UInt(vaBits.W)
      val eligible = Bool()
    }))
    val flush      = Input(Bool())
    val candidates = Input(Vec(3, new StreamHistoryCandidate(vaBits)))
    val allow      = Output(Vec(3, Bool()))
    // Combinational inspection ports, pruned in production when unconnected.
    val inspect      = Output(new StreamHistoryEntry(c, vaBits))
    val inspectIndex = Output(UInt(c.indexBits.W))
    val applied      = Output(Vec(3, Bool()))
  })
  val table = RegInit(VecInit(Seq.fill(c.entries)(0.U.asTypeOf(new StreamHistoryEntry(c, vaBits)))))
  require(table.head.getWidth == c.entryBits(vaBits))
  val plru = RegInit(VecInit(Seq.fill(c.sets)(0.U(3.W))))
  // TrainFilter clears on the next edge after flush: quarantine its old output.
  val flushDelayed = RegNext(io.flush, false.B)
  val flush        = io.flush || flushDelayed
  val pc           = io.train.bits.pc
  val set          = ((pc >> 1) ^ (pc >> (1 + c.setBits)) ^ (pc >> (12 + c.setBits)))(c.setBits - 1, 0)
  val tag          = pc(c.setBits + c.tagBits(vaBits), 1 + c.setBits)
  val matches = VecInit((0 until 4).map(w =>
    table(Cat(set, w.U(2.W))).valid &&
      table(Cat(set, w.U(2.W))).tag === tag
  ))
  val empty    = VecInit((0 until 4).map(w => !table(Cat(set, w.U(2.W))).valid))
  val hit      = matches.asUInt.orR
  val tree     = plru(set)
  val victim   = Cat(tree(2), Mux(tree(2), tree(1), tree(0)))
  val way      = Mux(hit, OHToUInt(matches), Mux(empty.asUInt.orR, PriorityEncoder(empty), victim))
  val index    = Cat(set, way)
  val old      = table(index)
  val next     = WireDefault(old)
  val accepted = io.train.valid && !flush
  val update   = accepted && (hit || io.train.bits.eligible)
  val allocate = update && !hit
  val region   = io.train.bits.vaddr(vaBits - 1, 10)
  val block    = io.train.bits.vaddr(vaBits - 1, 6)
  def start(e: StreamHistoryEntry): UInt = Mux(e.streamDirection === 2.U, e.frontier + e.span, e.frontier - e.span)
  def prediction(e: StreamHistoryEntry): UInt =
    if (c.policy == 3) Mux(e.length > e.second.get, e.length, e.second.get) else e.length
  def incConf(v: UInt): UInt = Mux(v === 3.U, v, v + 1.U)
  val firstBlock = Cat(start(old), old.offset)
  val adjacent = region === old.frontier ||
    (region > old.frontier && region - old.frontier === 1.U) ||
    (region < old.frontier && old.frontier - region === 1.U)
  val close   = update && hit && !adjacent
  val rounded = ((old.span +& 2.U) >> 1) << 1
  val complete = close && !old.censored && !old.overflow && old.streamDirection =/= 0.U &&
    rounded <= ((1 << c.lengthBits) - 1).U
  val seed          = complete && (old.confidence === 0.U || old.historyDecr =/= (old.streamDirection === 2.U))
  val oldPrediction = prediction(old)
  val sample        = rounded(c.lengthBits - 1, 0)

  when(complete) {
    next.historyDecr := old.streamDirection === 2.U
    next.confidence  := incConf(old.confidence)
    when(seed) {
      next.length := sample
      next.second.foreach(_ := 0.U)
      next.confidence := 1.U
    }.otherwise {
      if (c.policy == 1) {
        next.length := Mux(sample > old.length, sample, old.length)
        val supported = sample >= ((old.length +& 1.U) >> 1) && sample <= old.length
        next.confidence := Mux(supported, incConf(old.confidence), 1.U)
      } else if (c.policy == 2) {
        when(sample >= old.length || sample === old.second.get) {
          next.length     := sample
          next.second.get := 0.U
        }.otherwise(next.second.get := sample)
      } else {
        when(sample =/= old.length && sample =/= old.second.get) {
          next.second.get := old.length
          next.length     := sample
          next.confidence := 1.U
        }
      }
    }
  }
  val initialDecr = block < firstBlock && firstBlock - block >= 2.U
  val initialIncr = block > firstBlock && block - firstBlock >= 2.U
  val direction =
    Mux(old.streamDirection =/= 0.U, old.streamDirection, Mux(initialDecr, 2.U, Mux(initialIncr, 1.U, 0.U)))
  val advance = adjacent && ((direction === 1.U && region > old.frontier) ||
    (direction === 2.U && region < old.frontier))
  when(allocate || close) {
    next.valid           := true.B
    next.tag             := tag
    next.frontier        := region
    next.span            := 0.U
    next.offset          := block(3, 0)
    next.streamDirection := 0.U
    next.censored        := allocate
    next.overflow        := false.B
    next.epoch           := old.epoch + 1.U
    when(allocate) {
      next.length := 0.U
      next.second.foreach(_ := 0.U)
      next.confidence  := 0.U
      next.historyDecr := false.B
    }
  }.otherwise {
    next.streamDirection := direction
    when(advance) {
      next.frontier := region
      next.span     := Mux(old.span.andR, old.span, old.span + 1.U)
      next.overflow := old.overflow || old.span >= ((1 << c.lengthBits) - 2).U
    }
  }
  // Split/learn precedes mismatch, just as in the causal reference model.
  val updated      = WireDefault(next)
  val currentStart = start(next)
  val overrun = Mux(
    next.historyDecr,
    region <= currentStart && currentStart - region >= prediction(next),
    region >= currentStart && region - currentStart >= prediction(next)
  )
  val directionMismatch = next.streamDirection =/= 0.U && (next.streamDirection === 2.U) =/= next.historyDecr
  val mismatch          = update && next.confidence =/= 0.U && (overrun || directionMismatch)
  if (c.policy == 1) { when(mismatch)(updated.confidence := 0.U) }

  when(update) {
    table(index) := updated
    plru(set)    := Cat(!way(1), Mux(way(1), !way(0), tree(1)), Mux(!way(1), !way(0), tree(0)))
    assert(PopCount(matches) <= 1.U)
  }
  when(flush)(table.foreach { e => e.valid := false.B; e.epoch := e.epoch + 1.U })
  io.inspect      := Mux(update, updated, old)
  io.inspectIndex := index

  // Captured progress is relative to the history direction, not the unreliable
  // native decr_mode. Within the epoch, start and history direction are stable.
  val newFirst = Cat(currentStart, next.offset)
  val qualified =
    Mux(next.historyDecr, block <= newFirst && newFirst - block >= 2.U, block >= newFirst && block - newFirst >= 2.U)
  val s1Valid     = RegNext(update && io.train.bits.eligible, false.B)
  val s1Index     = RegEnable(index, update)
  val s1Epoch     = RegEnable(updated.epoch, update)
  val s1Qualified = RegEnable(qualified, update)
  val s2Valid     = RegNext(s1Valid && !flush, false.B)
  val s2Index     = RegEnable(s1Index, s1Valid)
  val s2Epoch     = RegEnable(s1Epoch, s1Valid)
  val s2Qualified = RegEnable(s1Qualified, s1Valid)
  val live        = table(s2Index)
  val pred        = WireDefault(prediction(live))
  val liveStart   = start(live)
  val identity    = s2Valid && live.valid && live.epoch === s2Epoch && !flush
  val directionOK = live.streamDirection =/= 0.U && (live.streamDirection === 2.U) === live.historyDecr
  val bypass      = if (c.policy == 1) false.B else live.span >= pred
  val common      = identity && directionOK && !live.overflow && !bypass && s2Qualified
  for (level <- 0 until 3) {
    val candidate = io.candidates(level)
    val threshold = if (c.policy == 1 && level == 0) 3 else 1
    val lengthOK  = if (c.policy == 1 && level == 0) pred >= 2.U && pred <= 32.U else true.B
    val applied   = common && live.confidence >= threshold.U && lengthOK
    // Distance comparisons avoid endpoint overflow and work in both directions.
    val inRange = Mux(
      live.historyDecr,
      candidate.region <= liveStart && liveStart - candidate.region < pred,
      candidate.region >= liveStart && candidate.region - liveStart < pred
    )
    io.allow(level)   := !applied || inRange
    io.applied(level) := applied && candidate.valid
    val native  = candidate.valid && !reset.asBool
    val blocked = native && applied && !inRange
    val prefix  = s"history_l${level + 1}_"
    XSPerfAccumulate(prefix + "candidate_groups", native)
    XSPerfAccumulate(prefix + "candidate_blocks", Mux(native, PopCount(candidate.bitmap), 0.U))
    XSPerfAccumulate(prefix + "applied_groups", native && applied)
    XSPerfAccumulate(prefix + "blocked_groups", blocked)
    XSPerfAccumulate(prefix + "blocked_blocks", Mux(blocked, PopCount(candidate.bitmap), 0.U))
    XSPerfAccumulate(prefix + "allowed_blocks", Mux(native && !blocked, PopCount(candidate.bitmap), 0.U))
    XSPerfAccumulate(prefix + "bypass_identity", native && !identity)
    XSPerfAccumulate(prefix + "bypass_direction", native && identity && !directionOK)
    XSPerfAccumulate(prefix + "bypass_progress", native && identity && !s2Qualified)
    XSPerfAccumulate(prefix + "bypass_overflow", native && identity && live.overflow)
    XSPerfAccumulate(prefix + "bypass_overrun", native && identity && bypass)
    XSPerfAccumulate(prefix + "bypass_confidence", native && identity && live.confidence < threshold.U)
    XSPerfAccumulate(prefix + "bypass_length", native && identity && !lengthOK)
    for (conf <- 0 to 3) {
      XSPerfAccumulate(
        prefix + s"c${conf}_candidate_blocks",
        Mux(native && identity && live.confidence === conf.U, PopCount(candidate.bitmap), 0.U)
      )
      XSPerfAccumulate(
        prefix + s"c${conf}_blocked_blocks",
        Mux(blocked && live.confidence === conf.U, PopCount(candidate.bitmap), 0.U)
      )
    }
    XSPerfHistogram(prefix + "predicted_regions", pred, native && applied, 0, 64, 4)
  }
  // Simulation-only bounded shadow. A later accepted demand to a blocked VA
  // is a proxy, NOT a counterfactual useful refill. No production state/cam.
  if (p(PerfCounterOptionsKey).enablePerfPrint) {
    for (level <- 0 until 3) {
      val regions   = Reg(Vec(8, UInt(regionBits.W)))
      val masks     = RegInit(VecInit(Seq.fill(8)(0.U(16.W))))
      val pointer   = RegInit(0.U(3.W))
      val candidate = io.candidates(level)
      val insert    = candidate.valid && !io.allow(level) && !flush
      val matching  = VecInit((0 until 8).map(i => masks(i).orR && regions(i) === candidate.region))
      val unused    = VecInit(masks.map(m => !m.orR))
      val slot = Mux(matching.asUInt.orR, OHToUInt(matching), Mux(unused.asUInt.orR, PriorityEncoder(unused), pointer))
      val demandBit = UIntToOH(block(3, 0), 16)
      val seen = VecInit((0 until 8).map(i =>
        accepted && masks(i).orR && regions(i) === region &&
          (masks(i) & demandBit).orR
      ))
      for (i <- 0 until 8) { when(seen(i))(masks(i) := masks(i) & ~demandBit) }
      when(insert) {
        regions(slot) := candidate.region
        val retained   = Mux(matching.asUInt.orR, masks(slot), 0.U)
        val sameDemand = accepted && region === candidate.region
        masks(slot)                        := (retained | candidate.bitmap) & Mux(sameDemand, ~demandBit, "hffff".U)
        when(!matching.asUInt.orR)(pointer := pointer + 1.U)
      }
      when(flush)(masks.foreach(_ := 0.U))
      val prefix = s"history_l${level + 1}_shadow_"
      XSPerfAccumulate(prefix + "demand_blocks", PopCount(seen))
      XSPerfAccumulate(prefix + "eligible_demand_blocks", Mux(io.train.bits.eligible, PopCount(seen), 0.U))
      XSPerfAccumulate(
        prefix + "unobserved_eviction_blocks",
        Mux(insert && !matching.asUInt.orR, PopCount(masks(slot) & Mux(seen(slot), ~demandBit, "hffff".U)), 0.U)
      )
      XSPerfAccumulate(prefix + "unobserved_flush_blocks", Mux(flush, PopCount(masks.asUInt), 0.U))
      XSPerfAccumulate(
        prefix + "new_blocks",
        Mux(insert, PopCount(candidate.bitmap & ~Mux(matching.asUInt.orR, masks(slot), 0.U)), 0.U)
      )
    }
  }
  XSPerfAccumulate("history_train", accepted)
  XSPerfAccumulate("history_train_eligible", accepted && io.train.bits.eligible)
  XSPerfAccumulate("history_hit", accepted && hit)
  XSPerfAccumulate("history_allocate", allocate)
  XSPerfAccumulate("history_capacity_censor", allocate && old.valid)
  XSPerfAccumulate("history_admission_bypass", accepted && !hit && !io.train.bits.eligible)
  XSPerfAccumulate("history_split", close)
  XSPerfAccumulate("history_complete", complete)
  XSPerfAccumulate("history_censored_close", close && old.censored)
  XSPerfAccumulate("history_unknown_direction_close", close && old.streamDirection === 0.U)
  XSPerfAccumulate("history_unrepresentable_close", close && (old.overflow || rounded > ((1 << c.lengthBits) - 1).U))
  XSPerfAccumulate("history_seed", seed)
  XSPerfAccumulate("history_demand_overrun", mismatch && overrun)
  XSPerfAccumulate("history_direction_mismatch", mismatch && directionMismatch)
  XSPerfAccumulate("history_repeat_region", update && hit && region === old.frontier)
  XSPerfAccumulate("history_backtrack", update && hit && adjacent && !advance && region =/= old.frontier)
  XSPerfAccumulate("history_advance", update && hit && advance)
  XSPerfAccumulate("history_flush", io.flush)
  if (c.policy == 2) {
    XSPerfAccumulate("history_short_pending", complete && !seed && sample < old.length && sample =/= old.second.get)
    XSPerfAccumulate("history_short_commit", complete && !seed && sample < old.length && sample === old.second.get)
    XSPerfAccumulate("history_grow_commit", complete && !seed && sample > old.length)
  }
  if (c.policy == 3) {
    XSPerfAccumulate("history_distinct_insert", complete && !seed && sample =/= old.length && sample =/= old.second.get)
    XSPerfAccumulate(
      "history_distinct_confirm",
      complete && !seed && (sample === old.length || sample === old.second.get)
    )
  }
  val compare = complete && old.confidence =/= 0.U && old.historyDecr === (old.streamDirection === 2.U)
  XSPerfAccumulate("history_length_compare", compare)
  XSPerfAccumulate("history_length_exact", compare && oldPrediction === sample)
  XSPerfAccumulate("history_length_under", compare && oldPrediction < sample)
  XSPerfAccumulate("history_length_over", compare && oldPrediction > sample)
  XSPerfAccumulate("history_length_under_regions", Mux(compare && oldPrediction < sample, sample - oldPrediction, 0.U))
  XSPerfAccumulate("history_length_over_regions", Mux(compare && oldPrediction > sample, oldPrediction - sample, 0.U))
  XSPerfAccumulate("history_complete_regions", Mux(complete, old.span +& 1.U, 0.U))
  XSPerfAccumulate("history_rounding_extra_regions", Mux(complete, rounded - (old.span +& 1.U), 0.U))
  XSPerfHistogram("history_completed_regions", old.span +& 1.U, complete, 0, 128, 4)
}
