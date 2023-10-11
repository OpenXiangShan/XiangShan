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

package xiangshan.cache.prefetch

import org.chipsalliance.cde.config.{Parameters, Field}
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.mmu.{HasTlbConst}
import utils._
import utility._

case object BOPParamsKey extends Field[BOPParameters]

case class BOPParameters(
  rrTableEntries: Int,
  rrTagBits: Int,
  scoreBits: Int,
  roundMax: Int,
  badScore: Int,
  // TODO: Is 256-offset necessary, which will cross pages?
  offsetList: Seq[Int] = Seq(
      1,   2,   3,   4,   5,   6,   8,   9,  10,  12,
     15,  16/*,  18,  20,  24,  25,  27,  30,  32,  36,
     40,  45,  48,  50,  54,  60,  64,  72,  75,  80,
     81,  90,  96, 100, 108, 120, 125, 128, 135, 144,
    150, 160, 162, 180, 192, 200, 216, 225, 240, 243,
    250, 256*/
  ),
  blockBytes: Int,
  nEntries: Int
) {
  def scores = offsetList.length
  def offsetWidth = log2Up(offsetList(scores - 1)) + 1
  def rrIdxBits = log2Up(rrTableEntries)
  def roundBits = log2Up(roundMax)
  def scoreMax = (1 << scoreBits) - 1
  def totalWidth = log2Up(nEntries) // id's width
}

class ScoreTableEntry(implicit p: Parameters) extends PrefetchBundle {
  val offset = UInt(bopParams.offsetWidth.W)
  val score = UInt(bopParams.scoreBits.W)

  def apply(offset: UInt, score: UInt) = {
    val entry = Wire(new ScoreTableEntry)
    entry.offset := offset
    entry.score := score
    entry
  }

  override def toPrintable: Printable = { p"${offset}:${score}" }
}

class TestOffsetReq(implicit p: Parameters) extends PrefetchBundle {
  // find whether (X-d) is in recent request table
  val addr = UInt(PAddrBits.W) // X
  val testOffset = UInt(bopParams.offsetWidth.W) // d
  val ptr = UInt(log2Up(bopParams.scores).W) // index of testOffset in offsetList

  override def toPrintable: Printable = {
    p"addr=0x${Hexadecimal(addr)} off=${testOffset} ptr=${ptr}"
  }
}

class TestOffsetResp(implicit p: Parameters) extends PrefetchBundle {
  val testOffset = UInt(bopParams.offsetWidth.W)
  val ptr = UInt(log2Up(bopParams.scores).W)
  val hit = Bool()

  override def toPrintable: Printable = {
    p"pff=${testOffset} ptr=${ptr} hit=${hit}"
  }
}

class TestOffsetBundle(implicit p: Parameters) extends PrefetchBundle {
  val req = DecoupledIO(new TestOffsetReq)
  val resp = Flipped(DecoupledIO(new TestOffsetResp))

  override def toPrintable: Printable = {
    p"req: v=${req.valid} r=${req.ready} ${req.bits} " +
      p"resp: v=${resp.valid} r=${resp.ready} ${resp.bits}"
  }
}

class BestOffsetPrefetchReq(implicit p: Parameters) extends PrefetchReq {
  val id = UInt(bopParams.totalWidth.W)

  override def toPrintable: Printable = {
    p"addr=0x${Hexadecimal(addr)} w=${write} id=0x${Hexadecimal(id)}"
  }
}

class BestOffsetPrefetchResp(implicit p: Parameters) extends PrefetchResp {
  val id = UInt(bopParams.totalWidth.W)

  override def toPrintable: Printable = {
    p"id=0x${Hexadecimal(id)}"
  }
}

class BestOffsetPrefetchFinish(implicit p: Parameters) extends PrefetchFinish {
  val id = UInt(bopParams.totalWidth.W)

  override def toPrintable: Printable = {
    p"id=0x${Hexadecimal(id)}"
  }
}

class BestOffsetPrefetchIO(implicit p: Parameters) extends PrefetchBundle {
  val train = Flipped(ValidIO(new PrefetchTrain))
  val req = DecoupledIO(new BestOffsetPrefetchReq)
  val resp = Flipped(DecoupledIO(new BestOffsetPrefetchResp))
  val finish = DecoupledIO(new BestOffsetPrefetchFinish)

  override def toPrintable: Printable = {
    p"train: v=${train.valid} ${train.bits} " +
      p"req: v=${req.valid} r=${req.ready} ${req.bits} " +
      p"resp: v=${resp.valid} r=${resp.ready} ${resp.bits} " +
      p"finish: v=${finish.valid} r=${finish.ready} ${finish.bits}"
  }
}

class RecentRequestTable(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val w = Flipped(DecoupledIO(UInt(PAddrBits.W)))
    val r = Flipped(new TestOffsetBundle)
  })
  def rrIdxBits = bopParams.rrIdxBits
  def rrTagBits = bopParams.rrTagBits
  def rrTableEntries = bopParams.rrTableEntries
  def blockBytes = bopParams.blockBytes
  // RR table is direct mapped, accessed through a hash function, each entry holding a partial tag.
  //        +----------+---------------+---------------+----------------------+
  // paddr: |  ......  |  8-bit hash2  |  8-bit hash1  |  6-bit cache offset  |
  //        +----------+---------------+---------------+----------------------+
  //        +-------+------------------+---------------+----------------------+
  //    or: |  ...  |    12-bit tag    |  8-bit hash1  |  6-bit cache offset  |
  //        +-------+------------------+---------------+----------------------+
  def lineAddr(addr: UInt) = addr(PAddrBits - 1, log2Up(blockBytes))
  def hash1(addr: UInt) = lineAddr(addr)(rrIdxBits - 1, 0)
  def hash2(addr: UInt) = lineAddr(addr)(2 * rrIdxBits - 1, rrIdxBits)
  def idx(addr: UInt) = hash1(addr) ^ hash2(addr)
  def tag(addr: UInt) = lineAddr(addr)(rrTagBits + rrIdxBits - 1, rrIdxBits)
  def rrTableEntry() = new Bundle {
    val valid = Bool()
    val tag = UInt(rrTagBits.W)

    override def toPrintable: Printable = {
      p"${valid} ${Hexadecimal(tag)}"
    }
  }

  val rrTable = Module(new SRAMTemplate(rrTableEntry(), set = rrTableEntries, way = 1, shouldReset = true, singlePort = true))

  val wAddr = io.w.bits
  rrTable.io.w.req.valid := io.w.valid && !io.r.req.valid
  rrTable.io.w.req.bits.setIdx := idx(wAddr)
  rrTable.io.w.req.bits.data(0).valid := true.B
  rrTable.io.w.req.bits.data(0).tag := tag(wAddr)

  val rAddr = io.r.req.bits.addr - (io.r.req.bits.testOffset << log2Up(blockBytes))
  val rData = Wire(rrTableEntry())
  rrTable.io.r.req.valid := io.r.req.fire
  rrTable.io.r.req.bits.setIdx := idx(rAddr)
  rData := rrTable.io.r.resp.data(0)

  val rwConflict = io.w.fire && io.r.req.fire// && idx(wAddr) === idx(rAddr)
  // when (rwConflict) {
  //   rrTable.io.r.req.valid := false.B
  // }
  // when (RegNext(rwConflict)) {
  //   rData.valid := true.B
  //   rData.tag := RegNext(tag(wAddr))
  // }

  io.w.ready := rrTable.io.w.req.ready && !io.r.req.valid
  io.r.req.ready := true.B
  io.r.resp.valid := RegNext(rrTable.io.r.req.fire)
  io.r.resp.bits.testOffset := RegNext(io.r.req.bits.testOffset)
  io.r.resp.bits.ptr := RegNext(io.r.req.bits.ptr)
  io.r.resp.bits.hit := rData.valid && rData.tag === RegNext(tag(rAddr))

  assert(!RegNext(rwConflict), "single port SRAM should not read and write at the same time")

  // debug info
  XSDebug(io.w.fire, p"io.write: v=${io.w.valid} addr=0x${Hexadecimal(io.w.bits)}\n")
  XSDebug(p"io.read: ${io.r}\n")
  XSDebug(io.w.fire, p"wAddr=0x${Hexadecimal(wAddr)} idx=${Hexadecimal(idx(wAddr))} tag=${Hexadecimal(tag(wAddr))}\n")
  XSDebug(io.r.req.fire, p"rAddr=0x${Hexadecimal(rAddr)} idx=${Hexadecimal(idx(rAddr))} rData=${rData}\n")

}

class OffsetScoreTable(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(UInt(PAddrBits.W))) // req addr from L1
    val prefetchOffset = Output(UInt(bopParams.offsetWidth.W))
    val test = new TestOffsetBundle
  })

  def offsetWidth = bopParams.offsetWidth
  def offsetList = bopParams.offsetList
  def scores = bopParams.scores
  def roundBits = bopParams.roundBits
  def roundMax = bopParams.roundMax
  def scoreMax = bopParams.scoreMax
  def badScore = bopParams.badScore

  val prefetchOffset = RegInit(2.U(offsetWidth.W)) // best offset is 1, that is, a next-line prefetcher as initialization
  val st = RegInit(VecInit(offsetList.map(off => (new ScoreTableEntry).apply(off.U, 0.U))))
  val ptr = RegInit(0.U(log2Up(scores).W))
  val round = RegInit(0.U(roundBits.W))

  val bestOffset = RegInit((new ScoreTableEntry).apply(2.U, 0.U)) // the entry with the highest score while traversing
  val testOffset = WireInit(st(ptr).offset)
  def winner(e1: ScoreTableEntry, e2: ScoreTableEntry): ScoreTableEntry = {
    val w = Wire(new ScoreTableEntry)
    w := Mux(e1.score > e2.score, e1, e2)
    w
  }

  val s_idle :: s_learn :: Nil = Enum(2)
  val state = RegInit(s_idle)

  // 1. At the start of a learning phase
  // All the scores are reset to 0.
  // At the end of every learning phase, the prefetch offset is updated as the one with the highest score.
  when (state === s_idle) {
    st.foreach(_.score := 0.U)
    ptr := 0.U
    round := 0.U
    bestOffset.score := badScore.U
    prefetchOffset := bestOffset.offset
    state := s_learn
  }

  // 2. During a learning phase
  // On every eligible L2 read access (miss or prefetched hit), we test an offset d_i from the list.
  // If X-d_i hits in the RR table, the score of offset d_i is incremented. During a round, each offset
  // in the list is test once. When all the offsets in the list have been tested, the current round is
  // finished, and a new round begins from offset d_1 again.
  // The current learning phase finishes at the end of a round when:
  // (1) one of the score equals SCOREMAX, or
  // (2) the number of rounds equals ROUNDMAX.
  when (state === s_learn) {
    when (io.test.req.fire) {
      val roundFinish = ptr === (scores - 1).U
      ptr := Mux(roundFinish, 0.U, ptr + 1.U)
      round := Mux(roundFinish, round + 1.U, round)

      XSDebug(p"test offset ${testOffset} req fire\n")
    }

    // (2) the number of rounds equals ROUNDMAX.
    when (round >= roundMax.U) {
      state := s_idle
      XSDebug(p"round reaches roundMax(${roundMax.U})\n")
    }

    when (io.test.resp.fire && io.test.resp.bits.hit) {
      val oldEntry = st(io.test.resp.bits.ptr)
      val oldScore = oldEntry.score
      val newScore = oldScore + 1.U
      val offset = oldEntry.offset
      st(io.test.resp.bits.ptr).score := newScore
      bestOffset := winner((new ScoreTableEntry).apply(offset, newScore), bestOffset)
      // (1) one of the score equals SCOREMAX
      when (newScore >= scoreMax.U) {
        state := s_idle
        XSDebug(p"newScore reaches scoreMax(${scoreMax.U})\n")
      }

      XSDebug(p"test offset ${offset} resp fire and hit. score ${oldScore} -> ${newScore}\n")
    }
  }

  io.req.ready := true.B
  io.prefetchOffset := prefetchOffset
  io.test.req.valid := state === s_learn && io.req.fire
  io.test.req.bits.addr := io.req.bits
  io.test.req.bits.testOffset := testOffset
  io.test.req.bits.ptr := ptr
  io.test.resp.ready := true.B

  XSDebug(p"state=${state} prefetchOffset=${prefetchOffset} ptr=${ptr} round=${round} bestOffset=${bestOffset} testOffset=${testOffset}\n")
  // score table
  XSDebug(p"OffsetScoreTable(idx:offset:score) as follows:\n")
  for (i <- 0 until scores) {
    if (i % 8 == 0) { XSDebug(p"${i.U}:${st(i)}\t") }
    else if (i % 8 == 7 || i == scores - 1) { XSDebug(false, true.B, p"${i.U}:${st(i)}\n") }
    else { XSDebug(false, true.B, p"${i.U}:${st(i)}\t") }
  }
  XSDebug(io.req.fire, p"receive req from L1. io.req.bits=0x${Hexadecimal(io.req.bits)}\n")
}

class BestOffsetPrefetchEntry(implicit p: Parameters) extends PrefetchModule with HasTlbConst {
  val io = IO(new Bundle {
    val id = Input(UInt(bopParams.totalWidth.W))
    val prefetchOffset = Input(UInt(bopParams.offsetWidth.W))
    val pft = new BestOffsetPrefetchIO
    val inflight = ValidIO(UInt(PAddrBits.W))
    val writeRRTable = DecoupledIO(UInt(PAddrBits.W))
  })

  def blockBytes = bopParams.blockBytes
  def getBlock(addr: UInt) = addr(PAddrBits - 1, log2Up(blockBytes))
  def getBlockAddr(addr: UInt) = Cat(getBlock(addr), 0.U(log2Up(blockBytes).W))
  def getPageNum(addr: UInt) = addr(PAddrBits - 1, offLen)

  val s_idle :: s_req :: s_resp :: s_write_recent_req :: s_finish :: Nil = Enum(5)
  val state = RegInit(s_idle)
  val req = RegInit(0.U.asTypeOf(new PrefetchReq))
  val baseAddr = RegInit(0.U(PAddrBits.W))
  val baseBlock = getBlock(io.pft.train.bits.addr)
  val nextBlock = baseBlock + io.prefetchOffset
  val nextAddr = Cat(nextBlock, 0.U(log2Up(blockBytes).W))
  val crossPage = getPageNum(nextAddr) =/= getPageNum(io.pft.train.bits.addr)

  when (state === s_idle) {
    when (io.pft.train.valid) {
      // state := s_req
      state := Mux(crossPage, s_idle, s_req)
      req.addr := nextAddr
      req.write := io.pft.train.bits.write
      baseAddr := getBlockAddr(io.pft.train.bits.addr)
      XSDebug(crossPage, p"prefetch addr 0x${nextAddr} cross page, ignore this!\n")
    }
  }

  when (state === s_req) {
    when (io.pft.req.fire) {
      state := s_resp
    }
  }

  when (state === s_resp) {
    when (io.pft.resp.fire) {
      state := s_write_recent_req
    }
  }

  when (state === s_write_recent_req) {
    when (io.writeRRTable.fire) {
      state := s_finish
    }
  }

  when (state === s_finish) {
    when (io.pft.finish.fire) {
      state := s_idle
    }
  }

  io.pft.req.valid := state === s_req
  io.pft.req.bits.addr := req.addr
  io.pft.req.bits.write := req.write
  io.pft.req.bits.id := io.id
  io.pft.resp.ready := state === s_resp
  io.pft.finish.valid := state === s_finish
  io.pft.finish.bits.id := io.id
  io.inflight.valid := state =/= s_idle
  io.inflight.bits := req.addr
  io.writeRRTable.valid := state === s_write_recent_req
  io.writeRRTable.bits := baseAddr // write this into recent request table

  XSDebug(p"bopEntry ${io.id}: state=${state} prefetchOffset=${io.prefetchOffset} inflight=${io.inflight.valid} 0x${Hexadecimal(io.inflight.bits)} writeRRTable: ${io.writeRRTable.valid} 0x${Hexadecimal(io.writeRRTable.bits)} baseAddr=0x${Hexadecimal(baseAddr)} nextAddr=0x${Hexadecimal(nextAddr)} crossPage=${crossPage} req: ${req}\n")
  XSDebug(p"bopEntry ${io.id}: io.pft: ${io.pft}\n")
}

class BestOffsetPrefetch(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new BestOffsetPrefetchIO)

  def nEntries = bopParams.nEntries
  def blockBytes = bopParams.blockBytes
  def getBlockAddr(addr: UInt) = Cat(addr(PAddrBits - 1, log2Up(blockBytes)), 0.U(log2Up(blockBytes).W))
  val scoreTable = Module(new OffsetScoreTable)
  val rrTable = Module(new RecentRequestTable)
  val reqArb = Module(new Arbiter(new BestOffsetPrefetchReq, nEntries))
  val finishArb = Module(new Arbiter(new BestOffsetPrefetchFinish, nEntries))
  val writeRRTableArb = Module(new Arbiter(UInt(PAddrBits.W), nEntries))

  val entryReadyIdx = Wire(UInt(log2Up(nEntries).W))
  val inflightMatchVec = Wire(Vec(nEntries, Bool()))

  val bopEntries = (0 until nEntries).map { i =>
    val bopEntry = Module(new BestOffsetPrefetchEntry)

    bopEntry.io.id := i.U
    bopEntry.io.prefetchOffset := scoreTable.io.prefetchOffset

    bopEntry.io.pft.train.valid := io.train.valid && i.U === entryReadyIdx && !inflightMatchVec.asUInt.orR
    bopEntry.io.pft.train.bits := io.train.bits

    reqArb.io.in(i) <> bopEntry.io.pft.req
    bopEntry.io.pft.resp.valid := io.resp.valid && i.U === io.resp.bits.id
    bopEntry.io.pft.resp.bits := io.resp.bits
    finishArb.io.in(i) <> bopEntry.io.pft.finish

    writeRRTableArb.io.in(i) <> bopEntry.io.writeRRTable

    bopEntry
  }

  entryReadyIdx := PriorityEncoder(bopEntries.map { e => !e.io.inflight.valid })
  (0 until nEntries).foreach(i =>
    inflightMatchVec(i) := bopEntries(i).io.inflight.valid && bopEntries(i).io.inflight.bits === getBlockAddr(io.train.bits.addr)
  )

  io.req <> reqArb.io.out
  io.resp.ready := VecInit(bopEntries.zipWithIndex.map { case (e, i) => i.U === io.resp.bits.id && e.io.pft.resp.ready }).asUInt.orR
  io.finish <> finishArb.io.out
  rrTable.io.w <> writeRRTableArb.io.out
  rrTable.io.r <> scoreTable.io.test
  scoreTable.io.req.valid := io.train.valid
  scoreTable.io.req.bits := getBlockAddr(io.train.bits.addr)

  XSDebug(p"io: ${io}\n")
  XSDebug(p"entryReadyIdx=${entryReadyIdx} inflightMatchVec=${Binary(inflightMatchVec.asUInt)}\n")

}
