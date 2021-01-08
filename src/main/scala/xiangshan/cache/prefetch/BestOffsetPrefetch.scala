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
}

class ScoreTableEntry(p: BOPParameters) extends PrefetchBundle {
  val offset = UInt(p.offsetWidth.W)
  val score = UInt(p.scoreBits.W)

  def apply(offset: UInt, score: UInt) = {
    this.offset := offset
    this.score := score
  }

  override def cloneType: this.type = (new ScoreTableEntry(p)).asInstanceOf[this.type]
}

class TestOffsetReq(p: BOPParameters) extends PrefetchBundle {
  // find whether (X-d) is in recent request table
  val addr = UInt(PAddrBits.W) // X
  val testOffset = UInt(p.offsetWidth.W) // d
  val ptr = UInt(log2Up(p.scores).W) // index of testOffset in offsetList

  override def cloneType: this.type = (new TestOffsetReq(p)).asInstanceOf[this.type]
}

class TestOffsetResp(p: BOPParameters) extends PrefetchBundle {
  val testOffset = UInt(p.offsetWidth.W)
  val ptr = UInt(log2Up(p.scores).W)
  val hit = Bool()

  override def cloneType: this.type = (new TestOffsetResp(p)).asInstanceOf[this.type]
}

class TestOffsetBundle(p: BOPParameters) extends PrefetchBundle {
  val req = DecoupledIO(new TestOffsetReq(p))
  val resp = Flipped(DecoupledIO(new TestOffsetResp(p)))

  override def cloneType: this.type = (new TestOffsetBundle(p)).asInstanceOf[this.type]
}

class RecentRequestTable(p: BOPParameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val w = Flipped(ValidIO(UInt(PAddrBits.W)))
    val r = Flipped(new TestOffsetBundle(p))
  })
  def rrIdxBits = p.rrIdxBits
  // RR table is direct mapped, accessed through a hash function, each entry holding a partial tag.
  //        +----------+---------------+---------------+----------------------+
  // paddr: |  ......  |  8-bit hash2  |  8-bit hash1  |  6-bit cache offset  |
  //        +----------+---------------+---------------+----------------------+
  //        +-------+------------------+---------------+----------------------+
  //    or: |  ...  |    12-bit tag    |  8-bit hash1  |  6-bit cache offset  |
  //        +-------+------------------+---------------+----------------------+
  def lineAddr(addr: UInt) = addr(PAddrBits - 1, log2Up(p.blockBytes))
  def hash1(addr: UInt) = lineAddr(addr)(rrIdxBits - 1, 0)
  def hash2(addr: UInt) = lineAddr(addr)(2 * rrIdxBits - 1, rrIdxBits)
}
