package xiangshan.cache.prefetch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import utils._

case class BOPParameters(
  rrTableEntries: Int,
  rrTagBits: Int,
  scoreBits: Int,
  roundMax: Int,
  badScore: Int,
  scores: Int = 52,
  offsetList: Seq[Int] = Seq(
      1,   2,   3,   4,   5,   6,   8,   9,  10,  12,
     15,  16,  18,  20,  24,  25,  27,  30,  32,  36,
     40,  45,  48,  50,  54,  60,  64,  72,  75,  80,
     81,  90,  96, 100, 108, 120, 125, 128, 135, 144,
    150, 160, 162, 180, 192, 200, 216, 225, 240, 243,
    250, 256
  ),
  blockBytes: Int
) {
  def offsetWidth = log2Up(offsetList(scores - 1)) + 1
  def rrIdxBits = log2Up(rrTableEntries)
  def roundBits = log2Up(roundMax)
  def scoreMax = (1 << scoreBits) - 1
}

class ScoreTableEntry(p: BOPParameters) extends PrefetchBundle {
  val offset = UInt(p.offsetWidth.W)
  val score = UInt(p.scoreBits.W)

  def apply(offset: UInt, score: UInt) = {
    val entry = new ScoreTableEntry(p)
    entry.offset := offset
    entry.score := score
    entry
  }

  override def toPrintable: Printable = { p"${offset}:${score}" }
  override def cloneType: this.type = (new ScoreTableEntry(p)).asInstanceOf[this.type]
}

class TestOffsetReq(p: BOPParameters) extends PrefetchBundle {
  // find whether (X-d) is in recent request table
  val addr = UInt(PAddrBits.W) // X
  val testOffset = UInt(p.offsetWidth.W) // d
  val ptr = UInt(log2Up(p.scores).W) // index of testOffset in offsetList

  override def toPrintable: Printable = {
    p"addr=0x${Hexadecimal(addr)} off=${testOffset} ptr=${ptr}"
  }
  override def cloneType: this.type = (new TestOffsetReq(p)).asInstanceOf[this.type]
}

class TestOffsetResp(p: BOPParameters) extends PrefetchBundle {
  val testOffset = UInt(p.offsetWidth.W)
  val ptr = UInt(log2Up(p.scores).W)
  val hit = Bool()

  override def toPrintable: Printable = {
    p"pff=${testOffset} ptr=${ptr} hit=${hit}"
  }
  override def cloneType: this.type = (new TestOffsetResp(p)).asInstanceOf[this.type]
}

class TestOffsetBundle(p: BOPParameters) extends PrefetchBundle {
  val req = DecoupledIO(new TestOffsetReq(p))
  val resp = Flipped(DecoupledIO(new TestOffsetResp(p)))

  override def toPrintable: Printable = {
    p"req: v=${req.valid} r=${req.ready} ${req.bits} " +
      p"resp: v=${resp.valid} r=${resp.ready} ${resp.bits}"
  }
  override def cloneType: this.type = (new TestOffsetBundle(p)).asInstanceOf[this.type]
}

class RecentRequestTable(p: BOPParameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val w = Flipped(ValidIO(UInt(PAddrBits.W)))
    val r = Flipped(new TestOffsetBundle(p))
  })
  def rrIdxBits = p.rrIdxBits
  def rrTagBits = p.rrTagBits
  def rrTableEntries = p.rrTableEntries
  def blockBytes = p.blockBytes
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

  val rrTable = Module(new SRAMWrapper("RR_Table", rrTableEntry(), set = rrTableEntries, way = 1, shouldReset = true))

  val wAddr = io.w.bits
  rrTable.io.w.req.valid := io.w.valid
  rrTable.io.w.req.bits.setIdx := idx(wAddr)
  rrTable.io.w.req.bits.data.valid := true.B
  rrTable.io.w.req.bits.data.tag := tag(wAddr)

  val rAddr = io.r.req.bits.addr - (io.r.req.bits.testOffset << log2Up(blockBytes))
  val rData = Wire(rrTableEntry())
  rrTable.io.r.req.valid := io.r.req.fire()
  rrTable.io.r.req.bits.setIdx := idx(rAddr)
  rData := rrTable.io.r.resp.data(0)

  val rwConflict = io.w.valid && io.r.req.fire() && idx(wAddr) === idx(rAddr)
  when (rwConflict) {
    rrTable.io.r.req.valid := false.B
  }
  when (RegNext(rwConflict)) {
    rData.valid := true.B
    rData.tag := RegNext(tag(wAddr))
  }

  io.r.req.ready := true.B
  io.r.resp.valid := RegNext(io.r.req.fire())
  io.r.resp.bits.testOffset := RegNext(io.r.req.bits.testOffset)
  io.r.resp.bits.ptr := RegNext(io.r.req.bits.ptr)
  io.r.resp.bits.hit := rData.valid && rData.tag === RegNext(tag(rAddr))

  // debug info
  XSDebug(io.w.valid, p"io.write: v=${io.w.valid} addr=0x${Hexadecimal(io.w.bits)}\n")
  XSDebug(p"io.read: ${io.r}\n")
  XSDebug(io.w.valid, p"wAddr=0x${Hexadecimal(wAddr)} idx=${Hexadecimal(idx(wAddr))} tag=${Hexadecimal(tag(wAddr))}\n")
  XSDebug(io.r.req.fire(), p"rAddr=0x${Hexadecimal(rAddr)} idx=${Hexadecimal(idx(rAddr))} rData=${rData}\n")
  XSDebug(rwConflict, p"write and read conflict!\n")

}

class OffsetScoreTable(p: BOPParameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val prefetchOffset = Output(UInt(p.offsetWidth.W))
    val test = new TestOffsetBundle(p)
  })

  def offsetWidth = p.offsetWidth
  def offsetList = p.offsetList
  def scores = p.scores
  def roundBits = p.roundBits
  def roundMax = p.roundMax
  def scoreMax = p.scoreMax

  val prefetchOffset = RegInit(1.U(offsetWidth)) // best offset is 1, this is, a next-line prefetcher as initialization
  val st = RegInit(VecInit(offsetList.map(off => new ScoreTableEntry(p).apply(off.U, 0.U))))
  val ptr = RegInit(0.U(log2Up(scores).W))
  val round = RegInit(0.U(roundBits.W))

  val bestOffset = RegInit(new ScoreTableEntry(p).apply(1.U, 0.U)) // the entry with the highest score while traversing
  val testOffset = WireInit(0.U(offsetWidth.W))
  def winner(e1: ScoreTableEntry, e2: ScoreTableEntry): ScoreTableEntry = {
    val w = new ScoreTableEntry(p)
    w := Mux(e1.score > e2.score, e1, e2)
    w
  }

  val s_idle :: s_learn :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)

  // 1. At the start of a learning phase
  // All the scores are reset to 0.
  when (state === s_idle) {
    when (ptr =/= scores.U) {
      st(ptr).score := 0.U
      ptr := ptr + 1.U
    }.otherwise {
      ptr := 0.U
      state := s_learn
    }
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
    testOffset := st(ptr).offset
    when (io.test.req.fire()) {
      val roundFinish = ptr === (scores - 1).U
      ptr := Mux(roundFinish, 0.U, ptr + 1.U)
      round := Mux(roundFinish, round + 1.U, round)
    }

    // (2) the number of rounds equals ROUNDMAX.
    when (round === roundMax.U) {
      state := s_finish
    }

    when (io.test.resp.fire() && io.test.resp.bits.hit) {
      val oldEntry = st(io.test.resp.bits.ptr)
      val oldScore = oldEntry.score
      val newScore = oldScore + 1.U
      val offset = oldEntry.offset
      st(io.test.resp.bits.ptr).score := newScore
      bestOffset := winner(new ScoreTableEntry(p).apply(offset, newScore), bestOffset)
      // (1) one of the score equals SCOREMAX
      when (newScore === scoreMax.U) {
        state := s_finish
      }
    }
  }

  // 3. At the end of every learning phase, the prefetch offset is updated as the one with the highest score.
  when (state === s_finish) {
    prefetchOffset := bestOffset.offset
    ptr := 0.U
    round := 0.U
    bestOffset.offset := 1.U
    bestOffset.score := 0.U
    state := s_idle
  }

  io.prefetchOffset := prefetchOffset
  io.test.req.valid := state === s_learn && round =/= roundMax.U
  io.test.req.bits.addr := DontCare // assign this outside the score table
  io.test.req.bits.testOffset := testOffset
  io.test.req.bits.ptr := ptr
}
