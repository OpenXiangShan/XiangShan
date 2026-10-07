package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}

// 复用真实 MainPipe/PB 的连接，仅增加定向激励和观测端口。
class PBMovePipelineTop(implicit p: Parameters) extends PBMainTestTop {
  val stream = IO(new Bundle {
    val allocAddr = Input(UInt(PAddrBits.W))
    val fillByte = Input(UInt(8.W))
    val loadStart, allowMove = Input(Bool())
    val issued = Output(Valid(UInt(PBIdBits.W)))
    val issueAddr = Output(UInt(PAddrBits.W))
    val returned, done = Output(Valid(UInt(PBIdBits.W)))
    val writeSet = Output(UInt(idxBits.W))
    val inFlight = Output(UInt(log2Ceil(PBEntries + 1).W))
  })
  pb.io.mshr.allocReq.bits.addr := stream.allocAddr
  pb.io.mshr.allocReq.bits.vaddr := stream.allocAddr
  pb.io.mshr.refillReq.bits.data := Fill(cfg.blockBytes, stream.fillByte)
  for (lane <- 0 until 3) {
    val port = pb.io.load(lane)
    val loadAddr = addr + (lane * cfg.blockBytes).U
    port.s1_paddr.valid := RegNext(stream.loadStart, false.B)
    port.s1_paddr.bits := loadAddr
    port.s2_use := port.s2_dataResp.valid && port.s2_dataResp.bits.hit
  }
  // 先积累多个已使用条目，再同时开放 MainPipe 搬运入口。
  mp.io.pb.s0_moveReq.valid := pb.io.pipe.s0_moveReq.valid && stream.allowMove
  pb.io.pipe.s0_moveReq.ready := mp.io.pb.s0_moveReq.ready && stream.allowMove
  stream.issued.valid := pb.io.pipe.s0_moveReq.fire
  stream.issued.bits := pb.io.pipe.s0_moveReq.bits.entryId
  stream.issueAddr := pb.io.pipe.s0_moveReq.bits.paddr
  stream.returned.valid := pb.io.pipe.s2_dataResp.fire
  stream.returned.bits := pb.io.pipe.s2_dataResp.bits.entryId
  stream.done := pb.io.pipe.s3_moveDone
  stream.writeSet := mp.io.meta_write.bits.idx
  stream.inFlight := PopCount(pb.observed.moveLocked)
}

class PBMovePipelineTest extends AnyFlatSpec with ChiselSim {
  private val base = new DefaultConfig
  implicit private val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  behavior of "PB moves through the real MainPipe"

  for (stallS3 <- Seq(false, true)) {
    it should s"accept consecutive moves and keep their data and completions distinct${if (stallS3) " across an S3 stall" else ""}" in {
      simulate(new PBMovePipelineTop) { c =>
        c.io.alloc.poke(false.B); c.io.fill.poke(false.B)
        c.io.store.poke(false.B); c.io.probe.poke(false.B)
        c.io.corrupt.poke(false.B); c.io.btot.poke(false.B)
        c.io.wbStall.poke(false.B); c.io.otherStore.poke(false.B)
        c.io.dirty.poke(false.B); c.io.block.poke(false.B)
        c.io.readReady.poke(true.B); c.io.commit.poke((!stallS3).B)
        c.stream.allowMove.poke(false.B); c.stream.loadStart.poke(false.B)
        c.stream.allocAddr.poke(0.U); c.stream.fillByte.poke(0.U)
        c.reset.poke(true.B); c.clock.step(2); c.reset.poke(false.B); c.clock.step()
        val baseAddr = BigInt("80001000", 16)
        for (i <- 0 until 3) {
          c.stream.allocAddr.poke((baseAddr + i * 64).U)
          c.io.alloc.poke(true.B); c.clock.step(); c.io.alloc.poke(false.B)
          c.stream.fillByte.poke((0x30 + i).U)
          c.io.fill.poke(true.B); c.clock.step(); c.io.fill.poke(false.B)
        }
        c.stream.loadStart.poke(true.B); c.clock.step(); c.stream.loadStart.poke(false.B)
        c.clock.step(4)
        c.stream.allowMove.poke(true.B)
        val issued = scala.collection.mutable.ArrayBuffer.empty[(Int, BigInt, BigInt)]
        val returned = scala.collection.mutable.ArrayBuffer.empty[BigInt]
        val done = scala.collection.mutable.ArrayBuffer.empty[BigInt]
        var peakInFlight = 0
        for (cycle <- 0 until 24) {
          if (cycle == 6) c.io.commit.poke(true.B)
          if (c.stream.issued.valid.peek().litToBoolean) {
            val id = c.stream.issued.bits.peek().litValue
            assert(!issued.exists(_._2 == id))
            issued += ((cycle, id, c.stream.issueAddr.peek().litValue))
          }
          if (c.stream.returned.valid.peek().litToBoolean) {
            returned += c.stream.returned.bits.peek().litValue
          }
          if (c.stream.done.valid.peek().litToBoolean) {
            assert(!stallS3 || cycle >= 6)
            val id = c.stream.done.bits.peek().litValue
            val index = ((issued.find(_._2 == id).get._3 - baseAddr) / 64).toInt
            c.io.write.expect(true.B)
            c.io.data.expect(BigInt(f"${0x30 + index}%02x" * 64, 16).U)
            // DefaultConfig 为 256 sets；0x80001000 的 VPN 折叠得到 alias=3。
            c.stream.writeSet.expect((192 + index).U)
            assert(!done.contains(id))
            done += id
          }
          peakInFlight = peakInFlight.max(c.stream.inFlight.peek().litValue.toInt)
          if (stallS3 && cycle == 5) {
            assert(issued.size == 3 && done.isEmpty)
            c.stream.inFlight.expect(3.U)
          }
          c.clock.step()
        }
        assert(issued.size == 3)
        assert(issued.sliding(2).forall(pair => pair(1)._1 == pair(0)._1 + 1))
        assert(returned.toSeq == issued.map(_._2).toSeq)
        assert(done.toSeq == issued.map(_._2).toSeq)
        assert(peakInFlight == 3)
        c.stream.inFlight.expect(0.U)
        c.io.live.expect(false.B)
      }
    }
  }
}
