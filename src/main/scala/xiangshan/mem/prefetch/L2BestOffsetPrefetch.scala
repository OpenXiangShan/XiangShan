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

import utility.{ChiselDB, CircularQueuePtr, Constantin, HasCircularQueuePtrHelper, MemReqSource, ParallelPriorityMux, RRArbiterInit, ReplacementPolicy, SRAMTemplate}
import org.chipsalliance.cde.config.Parameters
import chisel3.DontCare.:=
import chisel3.{util, _}
import chisel3.util._
import utils.XSPerfAccumulate
import scopt.Read
import utility.MemReqSource.reqSourceBits
import xiangshan.{XSBundle, XSModule}
import xiangshan.cache.HasDCacheParameters
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO}
import xiangshan.mem.{HasL1PrefetchSourceParameter, L2PrefetchTrainBundle, LdPrefetchTrainBundle, StPrefetchTrainBundle}

case class BOPParameters(
  rrTableEntries: Int = 256,
  rrTagBits:      Int = 12,
  scoreBits:      Int = 5,
  roundMax:       Int = 50,
  scoreBad:       Int = 1,
  dQEntries: Int = 16,
  dQLatency: Int = 175,
  dQMaxLatency: Int = 256,
  offsetList: Seq[Int] = Seq(
    -256, -250, -243, -240, -225, -216, -200,
    -192, -180, -162, -160, -150, -144, -135, -128,
    -125, -120, -108, -100, -96, -90, -81, -80,
    -75, -72, -64, -60, -54, -50, -48, -45,
    -40, -36, -32, -30, -27, -25, -24, -20,
    -18, -16, -15, -12, -10, -9, -8, -6,
    -5, -4, -3, -2, -1,
    1, 2, 3, 4, 5, 6, 8,
    9, 10, 12, 15, 16, 18, 20, 24,
    25, 27, 30, 32, 36, 40, 45, 48,
    50, 54, 60, 64, 72, 75, 80, 81,
    90, 96, 100, 108, 120, 125, 128, 135,
    144, 150, 160, 162, 180, 192, 200, 216,
    225, 240, 243, 250/*, 256*/
  )
) extends PrefetcherParams

// TODO lyq: L1 Prefetch normal form is already has this. So it is no need to set a HasPrefetcherHelper again --> need to be removed
trait HasPrefetcherHelper extends HasCircularQueuePtrHelper with HasDCacheParameters {
  // filter
  val TRAIN_FILTER_SIZE = 4
  val REQ_FILTER_SIZE = 16
  val TLB_REPLAY_CNT = 10

  // parameters
  val BLK_ADDR_RAW_WIDTH = 10
  val REGION_SIZE = 1024
  val PAGE_OFFSET = log2Ceil(cacheParams.pageSize)
  val VADDR_HASH_WIDTH = 5

  // vaddr:
  // |       tag               |     index     |    offset    |
  // |       block addr                        | block offset |
  // |       region addr       |        region offset         |
  val BLOCK_OFFSET = DCacheLineOffset
  val REGION_OFFSET = log2Up(REGION_SIZE)
  val REGION_BLKS = REGION_SIZE / blockBytes
  val INDEX_BITS = log2Up(REGION_BLKS)
  val TAG_BITS = VAddrBits - REGION_OFFSET
  val PTAG_BITS = PAddrBits - REGION_OFFSET
  val BLOCK_ADDR_BITS = VAddrBits - BLOCK_OFFSET

  // hash related
  val HASH_TAG_WIDTH = VADDR_HASH_WIDTH + BLK_ADDR_RAW_WIDTH

  def get_region_tag(vaddr: UInt) = {
    require(vaddr.getWidth == VAddrBits)
    vaddr(vaddr.getWidth - 1, REGION_OFFSET)
  }

  def get_ptag(vaddr: UInt) = {
    require(vaddr.getWidth == PAddrBits)
    vaddr(vaddr.getWidth - 1, REGION_OFFSET)
  }

  def get_index(addr: UInt) = {
    require(addr.getWidth >= REGION_OFFSET)
    addr(REGION_OFFSET - 1, BLOCK_OFFSET)
  }

  def get_index_oh(vaddr: UInt): UInt = {
    UIntToOH(get_index(vaddr))
  }

  def get_block_vaddr(vaddr: UInt): UInt = {
    vaddr(vaddr.getWidth - 1, BLOCK_OFFSET)
  }

  def _vaddr_hash(x: UInt): UInt = {
    val width = VADDR_HASH_WIDTH
    val low = x(width - 1, 0)
    val mid = x(2 * width - 1, width)
    val high = x(3 * width - 1, 2 * width)
    low ^ mid ^ high
  }

  def block_hash_tag(vaddr: UInt): UInt = {
    val blk_addr = get_block_vaddr(vaddr)
    val low = blk_addr(BLK_ADDR_RAW_WIDTH - 1, 0)
    val high = blk_addr(BLK_ADDR_RAW_WIDTH - 1 + 3 * VADDR_HASH_WIDTH, BLK_ADDR_RAW_WIDTH)
    val high_hash = _vaddr_hash(high)
    Cat(high_hash, low)
  }

  def region_hash_tag(vaddr: UInt): UInt = {
    val region_tag = get_region_tag(vaddr)
    val low = region_tag(BLK_ADDR_RAW_WIDTH - 1, 0)
    val high = region_tag(BLK_ADDR_RAW_WIDTH - 1 + 3 * VADDR_HASH_WIDTH, BLK_ADDR_RAW_WIDTH)
    val high_hash = _vaddr_hash(high)
    Cat(high_hash, low)
  }

  def region_to_block_addr(tag: UInt, index: UInt): UInt = {
    Cat(tag, index)
  }

  def toBinary(n: Int): String = n match {
    case 0 | 1 => s"$n"
    case _ => s"${toBinary(n / 2)}${n % 2}"
  }
}

trait HasBOPParams extends HasPrefetcherHelper {
  val bopParams = coreParams.prefetcher.get.asInstanceOf[BOPParameters]

  // train address space: virtual or physical
  val virtualTrain = true
  val offsetBits = DCacheLineOffset
  val fullAddrBits = if(virtualTrain) VAddrBits else PAddrBits
  val noOffsetAddrBits = fullAddrBits - offsetBits
  override val REQ_FILTER_SIZE = 16

  // Best offset
  val defaultMinAddrBits = offsetBits + log2Up(bopParams.rrTableEntries) + bopParams.rrTagBits
  val defaultConfig = fullAddrBits >= defaultMinAddrBits

  val rrTableEntries = if (defaultConfig) bopParams.rrTableEntries else 2
  val rrIdxBits = log2Up(rrTableEntries)
  val rrTagBits = if (defaultConfig) bopParams.rrTagBits else (fullAddrBits - offsetBits - rrIdxBits)
  val scoreBits = bopParams.scoreBits
  val roundMax = bopParams.roundMax
  val scoreBad = bopParams.scoreBad
  val scoreInit = 0
  val offsetList = bopParams.offsetList
  val dQEntries = bopParams.dQEntries
  val dQLatency = bopParams.dQLatency
  val dQMaxLatency = bopParams.dQMaxLatency

  val scores = offsetList.length
  val offsetWidth = log2Up(offsetList.max) + 2 // -32 <= offset <= 31
  val roundBits = log2Up(roundMax)
  val scoreMax = (1 << (scoreBits+1)) - 1
  val scoreTableIdxBits = log2Up(scores)
  val sourceIdBits = reqSourceBits

  def signedExtend(x: UInt, width: Int): UInt = {
    if (x.getWidth >= width) {
      x
    } else {
      Cat(Fill(width - x.getWidth, x.head(1)), x)
    }
  }
}

abstract class BOPBundle(implicit p: Parameters) extends XSBundle with HasBOPParams
abstract class BOPModule(implicit p: Parameters) extends XSModule with HasBOPParams

class ScoreTableEntry(implicit p: Parameters) extends BOPBundle {
  // val offset = UInt(offsetWidth.W)
  val score = UInt(scoreBits.W)

  def apply(score: UInt) = {
    val entry = Wire(this)
    // entry.offset := offset
    entry.score := score
    entry
  }
}

class TestOffsetReq(implicit p: Parameters) extends BOPBundle {
  // find whether (X-d) is in recent request table
  val addr = UInt(fullAddrBits.W)
  val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(scoreTableIdxBits.W)
}

class TestOffsetResp(implicit p: Parameters) extends BOPBundle {
  // val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(scoreTableIdxBits.W)
  val hit = Bool()
}

class TestOffsetBundle(implicit p: Parameters) extends BOPBundle {
  val req = DecoupledIO(new TestOffsetReq)
  val resp = Flipped(DecoupledIO(new TestOffsetResp))
}

class RecentRequestTable(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle {
    val w = Flipped(DecoupledIO(UInt(fullAddrBits.W)))
    val r = Flipped(new TestOffsetBundle)
  })

  // RR table is direct mapped, accessed through a hash function, each entry holding a partial tag.
  //        +----------+---------------+---------------+----------------------+
  // paddr: |  ......  |  8-bit hash2  |  8-bit hash1  |  6-bit cache offset  |
  //        +----------+---------------+---------------+----------------------+
  //        +-------+------------------+---------------+----------------------+
  //    or: |  ...  |    12-bit tag    |  8-bit hash1  |  6-bit cache offset  |
  //        +-------+------------------+---------------+----------------------+
  def lineAddr(addr: UInt) = addr(fullAddrBits - 1, offsetBits)
  def hash1(addr:    UInt) = lineAddr(addr)(rrIdxBits - 1, 0)
  def hash2(addr:    UInt) = lineAddr(addr)(2 * rrIdxBits - 1, rrIdxBits)
  def idx(addr:      UInt) = hash1(addr) ^ hash2(addr)
  def tag(addr:      UInt) = lineAddr(addr)(rrTagBits + rrIdxBits - 1, rrIdxBits)
  def rrTableEntry() = new Bundle {
    val valid = Bool()
    val tag = UInt(rrTagBits.W)
  }

  val rrTable = Module(
    new SRAMTemplate(rrTableEntry(), set = rrTableEntries, way = 1, shouldReset = true, singlePort = true)
  )

  val wAddr = io.w.bits
  rrTable.io.w.req.valid := io.w.valid && !io.r.req.valid
  rrTable.io.w.req.bits.setIdx := idx(wAddr)
  rrTable.io.w.req.bits.data(0).valid := true.B
  rrTable.io.w.req.bits.data(0).tag := tag(wAddr)

  val rAddr = io.r.req.bits.addr - signedExtend((io.r.req.bits.testOffset << offsetBits), fullAddrBits)
  val rData = Wire(rrTableEntry())
  rrTable.io.r.req.valid := io.r.req.fire
  rrTable.io.r.req.bits.setIdx := idx(rAddr)
  rData := rrTable.io.r.resp.data(0)

  assert(!RegNext(io.w.fire && io.r.req.fire), "single port SRAM should not read and write at the same time")

  io.w.ready := rrTable.io.w.req.ready && !io.r.req.valid
  io.r.req.ready := true.B
  io.r.resp.valid := RegNext(rrTable.io.r.req.fire, false.B)
  io.r.resp.bits.ptr := RegNext(io.r.req.bits.ptr)
  io.r.resp.bits.hit := rData.valid && rData.tag === RegNext(tag(rAddr))

}

class OffsetScoreTable(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(UInt(fullAddrBits.W)))
    val prefetchOffset = Output(UInt(offsetWidth.W))
    val prefetchDisable = Output(Bool())
    val test = new TestOffsetBundle
  })

  val prefetchOffset = RegInit(2.U(offsetWidth.W))
  val prefetchScore = RegInit(scoreInit.U(scoreBits.W))
  val disable = RegInit(false.B)
  // score table
  // val st = RegInit(VecInit(offsetList.map(off => (new ScoreTableEntry).apply(off.U, 0.U))))
  val st = RegInit(VecInit(Seq.fill(scores)((new ScoreTableEntry).apply(0.U))))
  val offList = WireInit(VecInit(offsetList.map(off => off.S(offsetWidth.W).asUInt)))
  val ptr = RegInit(0.U(scoreTableIdxBits.W))
  val round = RegInit(0.U(roundBits.W))

  val bestOffset = RegInit(2.U(offsetWidth.W)) // the entry with the highest score while traversing
  val bestScore = RegInit(scoreInit.U(scoreBits.W))
  val testOffset = offList(ptr)
  // def winner(e1: ScoreTableEntry, e2: ScoreTableEntry): ScoreTableEntry = {
  //   val w = Wire(new ScoreTableEntry)
  //   w := Mux(e1.score > e2.score, e1, e2)
  //   w
  // }

  val s_idle :: s_learn :: Nil = Enum(2)
  val state = RegInit(s_idle)

  // 1. At the start of a learning phase
  // All the scores are reset to 0.
  // At the end of every learning phase, the prefetch offset is updated as the one with the highest score.
  when(state === s_idle) {
    st.foreach(_.score := 0.U)
    ptr := 0.U
    round := 0.U
    bestScore := scoreInit.U
    prefetchOffset := bestOffset
    prefetchScore := bestScore
    state := s_learn
    // updating disable when a learning phase ends
    disable := bestScore < scoreBad.U
  }
  XSPerfAccumulate("bop_enable", !disable)
  XSPerfAccumulate("bop_disable", disable)

  // 2. During a learning phase
  // On every eligible L2 read access (miss or prefetched hit), we test an offset d_i from the list.
  // If X-d_i hits in the RR table, the score of offset d_i is incremented. During a round, each offset
  // in the list is test once. When all the offsets in the list have been tested, the current round is
  // finished, and a new round begins from offset d_1 again.
  // The current learning phase finishes at the end of a round when:
  // (1) one of the score equals SCOREMAX, or
  // (2) the number of rounds equals ROUNDMAX.
  when(state === s_learn) {
    when(io.test.req.fire) {
      val roundFinish = ptr === (scores - 1).U
      ptr := Mux(roundFinish, 0.U, ptr + 1.U)
      round := Mux(roundFinish, round + 1.U, round)
    }

    // (2) the number of rounds equals ROUNDMAX.
    when(round >= roundMax.U) {
      state := s_idle
    }

    when(io.test.resp.fire && io.test.resp.bits.hit) {
      val oldScore = st(io.test.resp.bits.ptr).score
      val newScore = oldScore + 1.U
      val offset = offList(io.test.resp.bits.ptr)
      st(io.test.resp.bits.ptr).score := newScore
      // bestOffset := winner((new ScoreTableEntry).apply(offset, newScore), bestOffset)
      val renewOffset = newScore > bestScore
      bestOffset := Mux(renewOffset, offset, bestOffset)
      bestScore := Mux(renewOffset, newScore, bestScore)
      // (1) one of the score equals SCOREMAX
      when(newScore >= scoreMax.U) {
        state := s_idle
      }
    }
  }

  io.req.ready := state === s_learn
  io.prefetchOffset := prefetchOffset
  io.prefetchDisable := disable
  io.test.req.valid := state === s_learn && io.req.valid
  io.test.req.bits.addr := io.req.bits
  io.test.req.bits.testOffset := testOffset
  io.test.req.bits.ptr := ptr
  io.test.resp.ready := true.B

  for (off <- offsetList) {
    if (off < 0) {
      XSPerfAccumulate("best_offset_neg_" + (-off).toString + "_learning_phases",
        Mux(state === s_idle, (bestOffset === off.S(offsetWidth.W).asUInt).asUInt, 0.U))
    } else {
      XSPerfAccumulate("best_offset_pos_" + off.toString + "_learning_phases",
        Mux(state === s_idle, (bestOffset === off.U).asUInt, 0.U))
    }
  }

  /*
  class DBEntry extends Bundle{
    val off = UInt(offsetWidth.W)
    val score = UInt(scoreBits.W)
  }
  object DBEntry{
    def apply(off: UInt, score: UInt): DBEntry= {
      val dbEntry = Wire(new DBEntry)
      dbEntry.off := off
      dbEntry.score := score
      dbEntry
    }
  }
  val table = ChiselDB. createTable("L2BopScoreTable", new DBEntry, basicDB = true)
  val enTalbe = Constantin.createRecord("isWriteL2BopTable", 1.U)
  table.log(
    data = DBEntry(offList(ptr), st(ptr).score),
    en = enTalbe.orR,
    site = "BestOffsetPrefetch",
    clock = clock,
    reset = reset
  )
  */

}

class BopReqBundle(implicit p: Parameters) extends BOPBundle{
  val full_vaddr = UInt(VAddrBits.W)
  val base_vaddr = UInt(noOffsetAddrBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  val isBOP = Bool()
}


/*BopReqFilter
class BopReqFilterEntry(implicit p: Parameters) extends BOPBundle {
  // region as the unit of record can reduce TLB translation times
  val valid = Bool()
  // for tlb req
  val hash_vtag = UInt(HASH_TAG_WIDTH.W)
  val region_vaddr = UInt(TAG_BITS.W)
  // for tlb resp
  val paddr_valid = Bool()
  val region_paddr = UInt(PTAG_BITS.W)
  // for pf req
  val idx_oh = UInt(REGION_BLKS.W)
  val idx_sent_oh = UInt(REGION_BLKS.W) // if without filter then not use it
  val needT_vec = Vec(REGION_BLKS, Bool())
  val source_vec = Vec(REGION_BLKS, UInt(sourceIdBits.W))
  // FIXME lyq: add base_vaddr

  // Decide whether to avoid sending requests that have already been sent
  def filter: Bool = false.B

  def reset(x: UInt): Unit = {
    valid := false.B
    // Avoid the same hash_vtag for all entries
    hash_vtag := x
    region_vaddr := x
    paddr_valid := false.B
    region_paddr := 0.U
    idx_oh := 0.U
    idx_sent_oh := 0.U
    needT_vec.map(_ := false.B)
    source_vec.map(_ := 0.U)
  }

  def excp_invalid() = {
    paddr_valid := false.B
    idx_oh := 0.U
    idx_sent_oh := 0.U
  }

  def fromBopReqBundle(req: BopReqBundle) = {
    val idx = get_index(req.full_vaddr)
    valid := true.B
    hash_vtag := region_hash_tag(req.full_vaddr)
    region_vaddr := get_region_tag(req.full_vaddr)
    paddr_valid := false.B
    region_paddr := 0.U
    idx_oh := UIntToOH(idx)
    idx_sent_oh := 0.U
    needT_vec.map(_ := false.B)
    source_vec.map(_ := 0.U)
    needT_vec(idx) := req.needT
    source_vec(idx) := req.source
  }

  def toPrefetchReq(): PrefetchReq = {
    val req = Wire(new PrefetchReq)
    val idx = PriorityEncoder(idx_oh).asTypeOf(UInt(INDEX_BITS.W))
    req.tag := parseFullAddress(get_pf_paddr())._1
    req.set := parseFullAddress(get_pf_paddr())._2
    req.vaddr.foreach(_ := get_pf_vaddr())
    req.needT := needT_vec(idx)
    req.source := source_vec(idx)
    req.source := MemReqSource.Prefetch2L2BOP.id.U
    req
  }

  def can_send_pf(): Bool = {
    Mux(
      filter,
      valid && paddr_valid && (idx_oh & (~idx_sent_oh)).orR,
      valid && paddr_valid && idx_oh.orR
    )
  }

  def get_pf_paddr(): UInt = {
    val idx = PriorityEncoder(idx_oh).asTypeOf(UInt(INDEX_BITS.W))
    Cat(region_paddr, idx, 0.U(BLOCK_OFFSET.W))
  }

  def get_pf_vaddr(): UInt = {
    val idx = PriorityEncoder(idx_oh).asTypeOf(UInt(INDEX_BITS.W))
    Cat(region_vaddr, idx)
  }

  def get_tlb_vaddr(): UInt = {
    Cat(region_vaddr, 0.U(REGION_OFFSET.W))
  }

  def update_paddr(paddr: UInt) = {
    paddr_valid := true.B
    region_paddr := get_ptag(paddr)
  }

  def update_sent(upd_idx_sent_oh: UInt): Unit ={
    val next_idx_oh = Wire(idx_oh.cloneType)
    when(filter){
      next_idx_oh := idx_oh & (~(idx_sent_oh | upd_idx_sent_oh))
    }.otherwise{
      next_idx_oh := idx_oh & (~upd_idx_sent_oh)
    }
    idx_oh := next_idx_oh
    idx_sent_oh := idx_sent_oh | upd_idx_sent_oh
    when(!next_idx_oh.orR){
      valid := false.B
    }
  }

  def update_idx_oh(upd_idx_oh: UInt) = {
    idx_oh := idx_oh | upd_idx_oh
  }

}

class PrefetchReqFilter(implicit p: Parameters) extends BOPModule{
  val io = IO(new Bundle() {
    val in_req = Flipped(ValidIO(new BopReqBundle))
    val tlb_req = new L2ToL1TlbIO(nRespDups = 1)
    val out_req = DecoupledIO(new PrefetchReq)
  })

  def wayMap[T <: Data](f: Int => T) = VecInit((0 until REQ_FILTER_SIZE).map(f))

  val entries = Seq.fill(REQ_FILTER_SIZE)(Reg(new BopReqFilterEntry))
  val replacement = ReplacementPolicy.fromString("plru", REQ_FILTER_SIZE)
  val tlb_req_arb = Module(new RRArbiterInit(new L2TlbReq, REQ_FILTER_SIZE))
  val pf_req_arb = Module(new RRArbiterInit(new PrefetchReq, REQ_FILTER_SIZE))

  val prev_in_valid = RegNext(io.in_req.valid, false.B)
  val prev_in_req = RegEnable(io.in_req.bits, io.in_req.valid)
  val prev_in_flag = get_region_tag(prev_in_req.full_vaddr) // region_hash_tag(prev_in_req.vaddr)

  io.tlb_req.req <> tlb_req_arb.io.out
  io.tlb_req.req_kill := false.B
  io.tlb_req.resp.ready := true.B
  io.out_req <> pf_req_arb.io.out

  val s1_valid = Wire(Bool())
  val s1_hit = Wire(Bool())
  val s1_tlb_fire_oh = Wire(UInt(REQ_FILTER_SIZE.W))
  val s1_evicted_oh = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  val s2_valid = Wire(Bool())


  /* s0: entries look up */
  val s0_in_req = io.in_req.bits
  // Initially, region_vaddr is used as flags instead of hash_vtag
  val s0_in_flag = get_region_tag(s0_in_req.full_vaddr) // region_hash_tag(s0_in_req.vaddr)
  val s0_conflict_prev = prev_in_valid && s0_in_flag === prev_in_flag
  val s0_match_oh = VecInit(entries.indices.map(i =>
    entries(i).valid && entries(i).region_vaddr === s0_in_flag && !s1_evicted_oh(i)
  )).asUInt
  val s0_req_valid = io.in_req.valid && !s0_conflict_prev
  val s0_match = Cat(s0_match_oh).orR
  val s0_hit = s0_req_valid && s0_match

  val s0_invalid_vec = wayMap(w => !entries(w).valid)
  val s0_has_invalid_way = s0_invalid_vec.asUInt.orR
  val s0_invalid_oh = ParallelPriorityMux(s0_invalid_vec.zipWithIndex.map(x => x._1 -> UIntToOH(x._2.U(REQ_FILTER_SIZE.W))))
  val s0_replace_oh = Mux(s0_has_invalid_way, s0_invalid_oh, UIntToOH(replacement.way))

  val s0_tlb_fire_oh = VecInit(tlb_req_arb.io.in.map(_.fire)).asUInt
  val s0_pf_fire_oh = VecInit(pf_req_arb.io.in.map(_.fire)).asUInt
  val s0_access_way = Mux(s0_match, OHToUInt(s0_match_oh), OHToUInt(s0_replace_oh))
  when(s0_req_valid){
    replacement.access(s0_access_way)
  }

  /* s1 update and replace */
  s1_valid := RegNext(s0_req_valid, false.B)
  s1_hit := RegEnable(s0_hit, false.B, s0_req_valid)
  val s1_in_req = RegEnable(s0_in_req, s0_req_valid)
  val s1_replace_oh = RegEnable(s0_replace_oh, s0_req_valid && !s0_hit)
  val s1_match_oh = RegEnable(s0_match_oh, s0_req_valid && s0_hit)
  s1_tlb_fire_oh := RegNext(s0_tlb_fire_oh, 0.U)
  val s1_alloc_entry = Wire(new BopReqFilterEntry)
  s1_alloc_entry.fromBopReqBundle(s1_in_req)
  (0 until REGION_BLKS).map{i => s1_evicted_oh(i) := s1_valid && !s1_hit && s1_replace_oh(i)}

  /* s2 tlb resp update */
  s2_valid := RegNext(s1_valid, false.B)
  val s2_replace_oh = RegNext(s1_replace_oh, 0.U)
  val s2_tlb_fire_oh = RegNext(s1_tlb_fire_oh, 0.U)
  val s2_evicted_oh = RegNext(s1_evicted_oh)

  /* entry update */
  val alloc = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  val update = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  val pf_fired = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  val tlb_fired = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  for ((e, i) <- entries.zipWithIndex){
    alloc(i) := s1_evicted_oh(i)
    update(i) := s1_valid && s1_hit && s1_match_oh(i)
    pf_fired(i) := s0_pf_fire_oh(i)
    // FIXME lyq: Because TLB is sent by region, TLB exceptions cannot be accurate to every prefetch request
    tlb_fired(i) := s2_tlb_fire_oh(i) && !io.tlb_req.resp.bits.miss && !s2_evicted_oh(i)

    when(tlb_fired(i)){
      e.update_paddr(io.tlb_req.resp.bits.paddr.head)
    }
    when(update(i)){
      e.update_idx_oh(s1_alloc_entry.idx_oh)
    }
    when(pf_fired(i)){
      e.update_sent(get_index_oh(Cat(pf_req_arb.io.in(i).bits.vaddr.getOrElse(0.U), 0.U(BLOCK_OFFSET.W))))
    }
    when(alloc(i)){
      e := s1_alloc_entry
    }
  }

  /* tlb & pf */
  for((e, i) <- entries.zipWithIndex){
    tlb_req_arb.io.in(i).valid := e.valid && !s1_tlb_fire_oh(i) && !s2_tlb_fire_oh(i) && !e.paddr_valid && !s1_evicted_oh(i)
    tlb_req_arb.io.in(i).bits.vaddr := e.get_tlb_vaddr()
    // BUG lyq: region unit read? or write? need data to validate it
    tlb_req_arb.io.in(i).bits.cmd := TlbCmd.read
    tlb_req_arb.io.in(i).bits.size := 3.U
    tlb_req_arb.io.in(i).bits.kill := false.B
    tlb_req_arb.io.in(i).bits.no_translate := false.B

    pf_req_arb.io.in(i).valid := e.valid && !s1_evicted_oh(i) && e.can_send_pf()
    pf_req_arb.io.in(i).bits := e.toPrefetchReq()
  }

  when(s1_valid && s1_hit){ assert(PopCount(s1_match_oh) === 1.U, "entries multi hit") }
  XSPerfAccumulate("recv_req", io.in_req.valid)
  XSPerfAccumulate("tlb_req", io.tlb_req.req.fire)
  XSPerfAccumulate("tlb_miss", io.tlb_req.resp.bits.miss)
  XSPerfAccumulate("tlb_excp",
    io.tlb_req.resp.bits.excp.head.pf.st || io.tlb_req.resp.bits.excp.head.af.st ||
      io.tlb_req.resp.bits.excp.head.pf.ld || io.tlb_req.resp.bits.excp.head.af.ld)
  XSPerfAccumulate("entry_alloc", PopCount(alloc))
  XSPerfAccumulate("entry_update", PopCount(update))
  XSPerfAccumulate("entry_tlb_fire", PopCount(tlb_fired))
  XSPerfAccumulate("entry_pf_fire", PopCount(pf_fired))

  // reset meta to avoid muti-hit problem
  for(i <- 0 until REQ_FILTER_SIZE) {
    when(reset.asBool) {
      entries(i).reset(i.U)
    }
  }
  /*
  class L2BopEntry(implicit p: Parameters) extends BOPBundle {
    val idx = UInt(REQ_FILTER_SIZE.W)
    val vaddr = UInt(VAddrBits.W)
    val region_num = UInt(REGION_BLKS.W)
    val region_vaddr = UInt(TAG_BITS.W)
    val needT = Bool()
    val source = UInt(sourceIdBits.W)
  }
  val isWriteL2BOPTable = WireInit(Constantin.createRecord("isWriteL2BOPTable", initValue = 0.U))
  val l2BOPTable = ChiselDB.createTable("L2BOPTable", new L2BopEntry)
  for (i <- 0 until REQ_FILTER_SIZE){
    when(pf_fired(i)){
      val data = Wire(new L2BopEntry)
      val req = entries(i).toPrefetchReq()
      data.idx := i.U
      data.vaddr := req.vaddr.getOrElse(0.U)
      data.region_num := req.vaddr.getOrElse(0.U)(INDEX_BITS - 1, 0)
      data.region_vaddr := entries(i).region_vaddr
      data.needT := req.needT
      data.source := req.source
      l2BOPTable.log(
        data = data,
        en = isWriteL2BOPTable.orR && pf_fired(i),
        site = "L2BOPTable",
        clock = clock,
        reset = reset
      )
    }
  }
  */
}
*/

class BopReqBufferEntry(implicit p: Parameters) extends BOPBundle {
  val valid = Bool()
  // for tlb req
  val paddrValid = Bool()
  val vaddrNoOffset = UInt((VAddrBits-offsetBits).W)
  val baseVaddr = UInt((VAddrBits-offsetBits).W)
  val paddrNoOffset = UInt(VAddrBits.W)
  val replayEn = Bool()
  val replayCnt = UInt(4.W)
  // for pf req
  val needT = Bool()
  val source = UInt(sourceIdBits.W)

  def reset(x: UInt): Unit = {
    valid := false.B
    paddrValid := false.B
    vaddrNoOffset := 0.U
    baseVaddr := 0.U
    paddrNoOffset := 0.U
    replayEn := false.B
    replayCnt := 0.U
    needT := false.B
    source := 0.U
  }

  def fromBopReqBundle(req: BopReqBundle) = {
    valid := true.B
    paddrValid := false.B
    vaddrNoOffset := get_block_vaddr(req.full_vaddr)
    baseVaddr := req.base_vaddr
    replayEn := false.B
    replayCnt := 0.U
    paddrNoOffset := 0.U
    needT := req.needT
    source := req.source
  }

  def isEqualBopReq(req: BopReqBundle) = {
    // FIXME lyq: the comparision logic is very complicated, is there a way to simplify
    valid &&
      vaddrNoOffset === get_block_vaddr(req.full_vaddr) &&
      baseVaddr === req.base_vaddr &&
      needT === req.needT &&
      source === req.source
  }

  def toPrefetchReq(): L2PrefetchReq = {
    val req = Wire(new L2PrefetchReq)
    req.addr := get_pf_paddr()
    req.source := source
    req.needT := needT
    req
  }

  def can_send_pf(): Bool = {
    valid && paddrValid
  }

  def get_pf_paddr(): UInt = {
    Cat(paddrNoOffset, 0.U(offsetBits.W))
  }

  def get_tlb_vaddr(): UInt = {
    Cat(vaddrNoOffset, 0.U(offsetBits.W))
  }

  def update_paddr(paddr: UInt) = {
    paddrValid := true.B
    paddrNoOffset := paddr(paddr.getWidth-1, offsetBits)
    replayEn := false.B
    replayCnt := 0.U
  }

  def update_sent(): Unit ={
    valid := false.B
  }

  def update_excp(): Unit = {
    valid := false.B
  }
}

class PrefetchReqBuffer(implicit p: Parameters) extends BOPModule{
  val io = IO(new Bundle() {
    val in_req = Flipped(DecoupledIO(new BopReqBundle))
    val tlb_req = new TlbRequestIO(nRespDups = 2)
    val out_req = DecoupledIO(new L2PrefetchReq)
  })

  def wayMap[T <: Data](f: Int => T) = VecInit((0 until REQ_FILTER_SIZE).map(f))
  def get_flag(vaddr: UInt) = get_block_vaddr(vaddr)

  // if full then drop new req, so there is no need to use s1_evicted_oh & replacement
  val entries = Seq.fill(REQ_FILTER_SIZE)(Reg(new BopReqBufferEntry))
  //val replacement = ReplacementPolicy.fromString("plru", REQ_FILTER_SIZE)
  val tlb_req_arb = Module(new RRArbiterInit(new TlbReq, REQ_FILTER_SIZE))
  val pf_req_arb = Module(new RRArbiterInit(new L2PrefetchReq, REQ_FILTER_SIZE))

  io.tlb_req.req <> tlb_req_arb.io.out
  io.tlb_req.req_kill := false.B
  io.tlb_req.resp.ready := true.B
  io.in_req.ready := true.B
  io.out_req <> pf_req_arb.io.out

  /* s0: entries look up */
  val prev_in_valid = RegNext(io.in_req.valid, false.B)
  val prev_in_req = RegEnable(io.in_req.bits, io.in_req.valid)
  val prev_in_flag = get_flag(prev_in_req.full_vaddr)

  val s0_in_req = io.in_req.bits
  val s0_in_flag = get_flag(s0_in_req.full_vaddr)
  val s0_conflict_prev = prev_in_valid && s0_in_flag === prev_in_flag
  // FIXME lyq: the comparision logic is very complicated, is there a way to simplify
  val s0_match_oh = VecInit(entries.indices.map(i =>
    entries(i).valid && entries(i).vaddrNoOffset === s0_in_flag &&
      entries(i).needT === s0_in_req.needT && entries(i).source === s0_in_req.source &&
      entries(i).baseVaddr === s0_in_req.base_vaddr
  )).asUInt
  val s0_match = Cat(s0_match_oh).orR

  val s0_invalid_vec = wayMap(w => !entries(w).valid)
  val s0_has_invalid_way = s0_invalid_vec.asUInt.orR
  val s0_invalid_oh = ParallelPriorityMux(s0_invalid_vec.zipWithIndex.map(x => x._1 -> UIntToOH(x._2.U(REQ_FILTER_SIZE.W))))

  val s0_req_valid = io.in_req.valid && !s0_conflict_prev && !s0_match && s0_has_invalid_way
  val s0_tlb_fire_oh = VecInit(tlb_req_arb.io.in.map(_.fire)).asUInt
  val s0_pf_fire_oh = VecInit(pf_req_arb.io.in.map(_.fire)).asUInt
  //val s0_access_way = Mux(s0_match, OHToUInt(s0_match_oh), OHToUInt(s0_replace_oh))
  //when(s0_req_valid){
  //  replacement.access(s0_access_way)
  //}
  XSPerfAccumulate("recv_req", io.in_req.valid)
  XSPerfAccumulate("recv_req_drop_conflict", io.in_req.valid && s0_conflict_prev)
  XSPerfAccumulate("recv_req_drop_match", io.in_req.valid && !s0_conflict_prev && s0_match)
  XSPerfAccumulate("recv_req_drop_full", io.in_req.valid && !s0_conflict_prev && !s0_match && !s0_has_invalid_way)


  /* s1 update and replace */
  val s1_valid = RegNext(s0_req_valid, false.B)
  val s1_in_req = RegEnable(s0_in_req, s0_req_valid)
  val s1_invalid_oh = RegEnable(s0_invalid_oh, 0.U, s0_req_valid)
  val s1_pf_fire_oh = RegNext(s0_pf_fire_oh, 0.U)
  val s1_tlb_fire_oh = RegNext(s0_tlb_fire_oh, 0.U)
  val s1_alloc_entry = Wire(new BopReqBufferEntry)
  s1_alloc_entry.fromBopReqBundle(s1_in_req)

  /* entry update */
  val alloc = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  val miss_drop = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  val pf_fired = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  val tlb_fired = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  for ((e, i) <- entries.zipWithIndex){
    alloc(i) := s1_valid && s1_invalid_oh(i)
    pf_fired(i) := s0_pf_fire_oh(i)
    val exp = s1_tlb_fire_oh(i) && io.tlb_req.resp.valid &&
      ((e.needT && (io.tlb_req.resp.bits.excp.head.pf.st || io.tlb_req.resp.bits.excp.head.af.st)) ||
        (!e.needT && (io.tlb_req.resp.bits.excp.head.pf.ld || io.tlb_req.resp.bits.excp.head.af.ld)))
    val miss = s1_tlb_fire_oh(i) && io.tlb_req.resp.valid && io.tlb_req.resp.bits.miss
    tlb_fired(i) := s1_tlb_fire_oh(i) && io.tlb_req.resp.valid && !io.tlb_req.resp.bits.miss && !exp
    miss_drop(i) := miss && e.replayEn

    // old data: update replayCnt
    when(e.valid && e.replayCnt.orR) {
      e.replayCnt := e.replayCnt - 1.U
    }
    // recent data: update tlb resp
    when(tlb_fired(i)){
      e.update_paddr(io.tlb_req.resp.bits.paddr.head)
    }.elsewhen(exp){
      e.update_excp()
    }.elsewhen(miss){ // miss
      when(e.replayEn){ e.reset(i.U) }
        .otherwise{
          e.replayCnt := TLB_REPLAY_CNT.U
          e.replayEn := true.B
        }
    }
    // issue data: update pf
    when(pf_fired(i)){
      e.update_sent()
    }
    // new data: update data
    when(alloc(i)){
      e := s1_alloc_entry
    }
  }

  /* tlb & pf */
  for((e, i) <- entries.zipWithIndex){
    //tlb_req_arb.io.in(i).valid := e.valid && !s1_tlb_fire_oh(i) && !s2_tlb_fire_oh(i) && !e.paddrValid && !s1_evicted_oh(i)
    tlb_req_arb.io.in(i).valid := e.valid && !e.paddrValid && !s1_tlb_fire_oh(i) && !e.replayCnt.orR
    tlb_req_arb.io.in(i).bits.vaddr := e.get_tlb_vaddr()
    when(e.needT) {
      tlb_req_arb.io.in(i).bits.cmd := TlbCmd.write
    }.otherwise{
      tlb_req_arb.io.in(i).bits.cmd := TlbCmd.read
    }
    tlb_req_arb.io.in(i).bits.size := 3.U
    tlb_req_arb.io.in(i).bits.kill := false.B
    tlb_req_arb.io.in(i).bits.no_translate := false.B
    tlb_req_arb.io.in(i).bits.memidx := DontCare
    tlb_req_arb.io.in(i).bits.debug := DontCare

    pf_req_arb.io.in(i).valid := e.can_send_pf()
    pf_req_arb.io.in(i).bits := e.toPrefetchReq()
  }

  // reset meta to avoid muti-hit problem
  for (i <- 0 until REQ_FILTER_SIZE) {
    when(reset.asBool) {
      entries(i).reset(i.U)
    }
  }

  XSPerfAccumulate("tlb_req", io.tlb_req.req.valid)
  XSPerfAccumulate("tlb_miss", io.tlb_req.resp.valid && io.tlb_req.resp.bits.miss)
  XSPerfAccumulate("tlb_excp",
    io.tlb_req.resp.valid && (
      io.tlb_req.resp.bits.excp.head.pf.st || io.tlb_req.resp.bits.excp.head.af.st ||
        io.tlb_req.resp.bits.excp.head.pf.ld || io.tlb_req.resp.bits.excp.head.af.ld
      ))
  XSPerfAccumulate("entry_alloc", PopCount(alloc))
  XSPerfAccumulate("entry_miss_drop", PopCount(miss_drop))
  XSPerfAccumulate("entry_merge", io.in_req.valid && s0_match)
  XSPerfAccumulate("entry_pf_fire", PopCount(pf_fired))

  /*
  val enTalbe = Constantin.createRecord("isWriteL2BopTable", 1.U)
  val l2BOPTable = ChiselDB. createTable("L2BOPTable", new BopReqBufferEntry, basicDB = true)
  for (i <- 0 until REQ_FILTER_SIZE){
    when(alloc(i)){
      l2BOPTable.log(
        data = entries(i),
        en = enTalbe.orR && pf_fired(i),
        site = "L2BOPTable",
        clock = clock,
        reset = reset
      )
    }
  }
  */
}

class DelayQueue(implicit p: Parameters) extends  BOPModule{
  val io = IO(new Bundle(){
    val in = Flipped(DecoupledIO(UInt(noOffsetAddrBits.W)))
    val out = DecoupledIO(UInt(fullAddrBits.W))
    // only record `fullAddrBits - offsetBits` bits
    // out.bits = Cat(record, 0.U(offsetBits))
  })

  /* Setting */
  val IdxWidth = log2Up(dQEntries)
  val LatencyWidth = log2Up(dQMaxLatency)
  class Entry extends Bundle{
    val addrNoOffset = UInt(noOffsetAddrBits.W)
    val cnt = UInt(LatencyWidth.W)
  }
  val queue = RegInit(VecInit(Seq.fill(dQEntries)(0.U.asTypeOf(new Entry))))
  val valids = RegInit(VecInit(Seq.fill(dQEntries)(false.B)))
  val head = RegInit(0.U(IdxWidth.W))
  val tail = RegInit(0.U(IdxWidth.W))
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last
  val outValid = !empty && !queue(head).cnt.orR && valids(head)

  /* In & Out */
  val setDqLatency = Constantin.createRecord("DelayQueueLatency", dQLatency.asUInt)
  when(io.in.valid && !full) {
    // if queue is full, we drop the new request
    queue(tail).addrNoOffset := io.in.bits
    queue(tail).cnt := setDqLatency
    valids(tail) := true.B
    tail := tail + 1.U

    /*
    // if full, drop the old request
    when(full && !io.deq.ready) {
      head := head + 1.U
    }
    */
  }
  when(outValid && io.out.ready) {
    valids(head) := false.B
    head := head + 1.U
  }
  io.in.ready := true.B
  io.out.valid := outValid
  io.out.bits := Cat(queue(head).addrNoOffset, 0.U(offsetBits.W))

  /* Update */
  for(i <- 0 until dQEntries){
    when(queue(i).cnt.orR){
      queue(i).cnt := queue(i).cnt - 1.U
    }
  }

  /* Perf */
  XSPerfAccumulate("full", full)
  XSPerfAccumulate("empty", empty)
  XSPerfAccumulate("entryNumber", PopCount(valids.asUInt))
  XSPerfAccumulate("inNumber", io.in.valid)
  XSPerfAccumulate("outNumber", io.out.valid)

  /*
  class DBEntry extends Bundle{
    val value = UInt(fullAddrBits.W)
  }
  object DBEntry{
    def apply(v: UInt):DBEntry = {
      val dbe = Wire(new DBEntry)
      dbe.value := v
      dbe
    }
  }
  val table = ChiselDB. createTable("L2BopDelayQueueOut", new DBEntry, basicDB = true)
  val enTalbe = Constantin.createRecord("isWriteL2BopDelayQueueOut", 1.U)
  table.log(
    data = DBEntry(io.out.bits),
    en = enTalbe.orR && io.out.valid,
    site = "BestOffsetPrefetch",
    clock = clock,
    reset = reset
  )
  */
}


class BOPTrainFilter()(implicit p: Parameters) extends XSModule with HasBOPParams with HasTrainFilterHelper {
  val io = IO(new Bundle() {
    val ld_in = Flipped(Vec(exuParameters.LduCnt, ValidIO(new LdPrefetchTrainBundle())))
    // val st_in = Flipped(Vec(exuParameters.StuCnt, ValidIO(new StPrefetchTrainBundle())))
    val l2_in = Flipped(Vec(1, ValidIO(new L2PrefetchTrainBundle())))

    val trainReq = DecoupledIO(new PrefetchReqBundle())
  })
  // TODO lyq: multi input
  class Ptr(implicit p: Parameters) extends CircularQueuePtr[Ptr](p => TRAIN_FILTER_SIZE){}
  object Ptr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): Ptr = {
      val ptr = Wire(new Ptr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val entries = RegInit(VecInit(Seq.fill(TRAIN_FILTER_SIZE){ (0.U.asTypeOf(new PrefetchReqBundle())) }))
  val valids = RegInit(VecInit(Seq.fill(TRAIN_FILTER_SIZE){ (false.B) }))

  val enqLen = exuParameters.LduCnt + 1
  val enqPtrExt = RegInit(VecInit((0 until enqLen).map(_.U.asTypeOf(new Ptr))))
  val deqPtrExt = RegInit(0.U.asTypeOf(new Ptr))
  
  val deqPtr = WireInit(deqPtrExt.value)

  require(TRAIN_FILTER_SIZE >= enqLen)

  // order 应该考虑 valid 的情况，否则会出现 invalide entry -> 虽然对资源影响不大，就一个周期
  val ld_reorder = reorder(io.ld_in)
  val reqs = ld_reorder.map(_.bits.asPrefetchReqBundle()) ++ io.l2_in.map(_.bits.asPrefetchReqBundle())
  val reqsV = ld_reorder.map(_.valid) ++ io.l2_in.map(_.valid)
  val needAlloc = Wire(Vec(enqLen, Bool()))
  val canAlloc = Wire(Vec(enqLen, Bool()))

  for(i <- (0 until enqLen)) {
    val req = reqs(i)
    val reqV = reqsV(i)
    val index = PopCount(needAlloc.take(i))
    val allocPtr = enqPtrExt(index)
    val entry_match = Cat(entries.zip(valids).map {
      case(e, v) => v && block_hash_tag(e.vaddr) === block_hash_tag(req.vaddr)
    }).orR
    val prev_enq_match = if(i == 0) false.B else Cat(reqs.zip(reqsV).take(i).map {
      case(pre, pre_v) => pre_v && block_hash_tag(pre.vaddr) === block_hash_tag(req.vaddr)
    }).orR

    needAlloc(i) := reqV && !entry_match && !prev_enq_match
    canAlloc(i) := needAlloc(i) && allocPtr >= deqPtrExt

    when(canAlloc(i)) {
      valids(allocPtr.value) := true.B
      entries(allocPtr.value) := req
    }
  }
  val allocNum = PopCount(canAlloc)

  enqPtrExt.foreach{case x => x := x + allocNum}

  io.trainReq.valid := false.B
  io.trainReq.bits := DontCare
  valids.zip(entries).zipWithIndex.foreach {
    case((valid, entry), i) => {
      when(deqPtr === i.U) {
        io.trainReq.valid := valid
        io.trainReq.bits := entry
      }
    }
  }

  when(io.trainReq.valid) {
    valids(deqPtr) := false.B
    deqPtrExt := deqPtrExt + 1.U
  }

  XSPerfAccumulate("train_filter_full", PopCount(valids) === (TRAIN_FILTER_SIZE).U)
  XSPerfAccumulate("train_filter_half", PopCount(valids) >= (TRAIN_FILTER_SIZE / 2).U)
  XSPerfAccumulate("train_filter_empty", PopCount(valids) === 0.U)

  val raw_enq_pattern = Cat(reqsV)
  val filtered_enq_pattern = Cat(needAlloc)
  val actual_enq_pattern = Cat(canAlloc)
  XSPerfAccumulate("train_filter_enq", allocNum > 0.U)
  XSPerfAccumulate("train_filter_deq", io.trainReq.fire)
  override def toBinary(n: Int): String = n match {
    case 0|1 => s"$n"
    case _   => s"${toBinary(n/2)}${n%2}"
  }
  for(i <- 0 until (1 << enqLen)) {
    XSPerfAccumulate(s"train_filter_raw_enq_pattern_${toBinary(i)}", raw_enq_pattern === i.U)
    XSPerfAccumulate(s"train_filter_filtered_enq_pattern_${toBinary(i)}", filtered_enq_pattern === i.U)
    XSPerfAccumulate(s"train_filter_actual_enq_pattern_${toBinary(i)}", actual_enq_pattern === i.U)
  }
}

class L2BestOffsetPrefetch(implicit p: Parameters) extends BasePrefecher with HasBOPParams with HasL1PrefetchSourceParameter{
  /*
  TODO lyq: do not use needT and source data --> all use `tlb.cmd.Read`
  now it is just a Frame, which is needed to complete on signal details
  */
  val io_l2PfConn = IO(new L2PrefetchConnectIO())

  val trainFilter = Module(new BOPTrainFilter)
  val delayQueue = Module(new DelayQueue)
  val rrTable = Module(new RecentRequestTable)
  val scoreTable = Module(new OffsetScoreTable)
  val reqFilter = Module(new PrefetchReqBuffer)

  trainFilter.io.ld_in <> io.ld_in
  trainFilter.io.l2_in <> DontCare
  trainFilter.io.l2_in.head <> io_l2PfConn.train

  /* s0 train */
  val s0_fire = scoreTable.io.req.fire
  val s1_fire = WireInit(false.B)
  val s0_ready, s1_ready = WireInit(false.B)
  val prefetchOffset = scoreTable.io.prefetchOffset
  val prefetchDisable = scoreTable.io.prefetchDisable
  val s0_trainVaddr = trainFilter.io.trainReq.bits.vaddr
  val s0_trainValid = trainFilter.io.trainReq.valid
  val s0_trainVaddrNoOff = s0_trainVaddr(s0_trainVaddr.getWidth - 1, DCacheLineOffset)
  val s0_reqVaddr = s0_trainVaddr + signedExtend((prefetchOffset << offsetBits), fullAddrBits)

  rrTable.io.r <> scoreTable.io.test
  rrTable.io.w <> delayQueue.io.out
  scoreTable.io.req.valid := s0_trainValid
  scoreTable.io.req.bits := s0_trainVaddr
  // TODO lyq: how to get refill data when bop is disable
  delayQueue.io.in.valid := s0_trainValid
  delayQueue.io.in.bits := s0_trainVaddrNoOff

  /* s1 get or send req */
  val s1_req_valid = RegInit(false.B)
  val s1_trainVaddr = RegEnable(s0_trainVaddr, s0_fire)
  val s1_reqVaddr = RegEnable(s0_reqVaddr, s0_fire)

  // pipeline control signal
  when(s0_fire) {
    s1_req_valid := true.B
  }.elsewhen(s1_fire){
    s1_req_valid := false.B
  }

  s0_ready := s1_ready || !s1_req_valid
  s1_ready := reqFilter.io.in_req.ready || !s1_req_valid
  s1_fire := s1_ready && s1_req_valid

  trainFilter.io.trainReq.ready := scoreTable.io.req.ready && delayQueue.io.in.ready && s0_ready
  io.tlb_req.resp.ready := true.B
  reqFilter.io.out_req.ready := true.B

  /* s1 send tlb req */
  when(prefetchDisable) {
    reqFilter.io.in_req.valid := false.B
    reqFilter.io.in_req.bits := DontCare
  }.otherwise {
    reqFilter.io.in_req.valid := s1_req_valid
    reqFilter.io.in_req.bits.full_vaddr := s1_reqVaddr
    reqFilter.io.in_req.bits.base_vaddr := s1_reqVaddr
    // TODO lyq: it is always false?
    reqFilter.io.in_req.bits.needT := false.B
    reqFilter.io.in_req.bits.isBOP := true.B
    reqFilter.io.in_req.bits.source := MemReqSource.Prefetch2L2BOP.id.U
  }

  io.tlb_req <> reqFilter.io.tlb_req
  io.l1_req <> DontCare
  io.l2_req.valid := reqFilter.io.out_req.valid
  io.l2_req.bits := reqFilter.io.out_req.bits
  io.l3_req <> DontCare


  for (off <- offsetList) {
    if (off < 0) {
      XSPerfAccumulate("best_offset_neg_" + (-off).toString, prefetchOffset === off.S(offsetWidth.W).asUInt)
    } else {
      XSPerfAccumulate("best_offset_pos_" + off.toString, prefetchOffset === off.U)
    }
  }
  XSPerfAccumulate("l2_req", io.l2_req.fire)
  XSPerfAccumulate("train", trainFilter.io.trainReq.fire)
  XSPerfAccumulate("bop_train_stall_for_st_not_ready", trainFilter.io.trainReq.valid && !scoreTable.io.req.ready)
  XSPerfAccumulate("bop_train_stall_for_tlb_not_ready", trainFilter.io.trainReq.valid && !io.tlb_req.req.ready)
}