// Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
//
// XiangShan is licensed under Mulan PSL v2.
// See https://license.coscl.org.cn/MulanPSL2 for details.

package xiangshan.frontend.ftq.prefetch

import chisel3._
import chisel3.simulator.EphemeralSimulator._ // scalastyle:ignore
import org.scalatest.flatspec.AnyFlatSpec
import top.ArgParser
import utility.PerfCounterOptionsKey
import xiangshan.XSCoreParamsKey
import xiangshan.XSTileKey

class PdipTriggerTest extends AnyFlatSpec {
  behavior of "PDIP trigger ownership and training re-read"

  it should "keep each candidate key across redirects, slot reuse, and pending SRAM writes" in {
    val (config, _, _) = ArgParser.parse(Array("--config", "DefaultConfig"))
    val core = config(XSTileKey).head
    val params = config.alterPartial {
      case XSCoreParamsKey => core.copy(frontendParameters = core.frontendParameters.copy(
        ftqParameters = core.frontendParameters.ftqParameters.copy(
          FtqSize = 8,
          pdipParameters = PdipParameters(NumSet = 4, NumWay = 2)
        )
      ))
      case PerfCounterOptionsKey => config(PerfCounterOptionsKey).copy(enablePerfPrint = false, enablePerfDB = false)
    }
    simulate(new Pdip()(params)) { dut =>
      val observed = scala.collection.mutable.ArrayBuffer.empty[BigInt]
      def step(n: Int = 1): Unit = (0 until n).foreach { _ =>
        dut.io.req.foreach { port =>
          if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
            observed += port.bits.vAddr.addr.peek().litValue * 2
          }
        }
        dut.clock.step()
      }
      def defaults(): Unit = {
        dut.io.ftqEnqueue.valid.poke(false.B)
        dut.io.ftqEnqueue.bits.ptr.value.poke(0.U)
        dut.io.ftqEnqueue.bits.ptr.flag.poke(false.B)
        dut.io.commit.valid.poke(false.B)
        dut.io.commit.bits.ptr.value.poke(0.U)
        dut.io.commit.bits.ptr.flag.poke(false.B)
        dut.io.commit.bits.startPc.addr.poke(0.U)
        dut.io.redirect.valid.poke(false.B)
        dut.io.redirect.bits.ptr.value.poke(7.U)
        dut.io.redirect.bits.ptr.flag.poke(false.B)
        dut.io.redirect.bits.startPc.addr.poke(0.U)
        dut.io.fetchPtr.value.poke(7.U)
        dut.io.fetchPtr.flag.poke(false.B)
        dut.io.fecCandidates.foreach { port =>
          port.valid.poke(false.B)
          port.bits.ftqIdx.value.poke(0.U)
          port.bits.ftqIdx.flag.poke(false.B)
          port.bits.miss.foreach(_.poke(false.B))
        }
        dut.io.req.foreach(_.ready.poke(true.B))
      }
      def reset(): Unit = {
        defaults()
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        step(8) // Includes the four-cycle SRAM initialization sweep.
        observed.clear()
      }
      def enqueue(idx: Int = 1, flag: Boolean = false): Unit = {
        dut.io.ftqEnqueue.bits.ptr.value.poke(idx.U)
        dut.io.ftqEnqueue.bits.ptr.flag.poke(flag.B)
        dut.io.ftqEnqueue.valid.poke(true.B)
        step()
        dut.io.ftqEnqueue.valid.poke(false.B)
      }
      def candidate(mask: Int, idx: Int = 1, flag: Boolean = false): Unit = {
        val port = dut.io.fecCandidates.head
        port.bits.ftqIdx.value.poke(idx.U)
        port.bits.ftqIdx.flag.poke(flag.B)
        port.bits.miss.zipWithIndex.foreach { case (bit, i) => bit.poke(((mask & (1 << i)) != 0).B) }
        port.valid.poke(true.B)
        step()
        port.valid.poke(false.B)
      }
      def commit(target: BigInt, idx: Int = 1, flag: Boolean = false): Unit = {
        dut.io.commit.bits.startPc.addr.poke((target / 2).U)
        dut.io.commit.bits.ptr.value.poke(idx.U)
        dut.io.commit.bits.ptr.flag.poke(flag.B)
        dut.io.commit.valid.poke(true.B)
        step()
        dut.io.commit.valid.poke(false.B)
      }
      def redirect(key: BigInt, flushIdx: Int = 7): Unit = {
        dut.io.redirect.bits.startPc.addr.poke((key / 2).U)
        dut.io.redirect.bits.ptr.value.poke(flushIdx.U)
        dut.io.fetchPtr.value.poke(flushIdx.U)
        dut.io.redirect.valid.poke(true.B)
        step()
        dut.io.redirect.valid.poke(false.B)
      }
      def query(key: BigInt): Seq[BigInt] = {
        observed.clear()
        redirect(key)
        step(8)
        observed.toSeq
      }
      def train(key: BigInt, target: BigInt, mask: Int = 1): Unit = {
        redirect(key)
        step(8)
        enqueue()
        candidate(mask)
        commit(target)
        step(8)
      }
      val a = BigInt(0x1040) // set 1, tag 16 for the small test table
      val b = BigInt(0x2040) // same set, different tag
      val c = BigInt(0x3040)
      val p = BigInt(0x8080) // distinct trigger/target sets and tags
      val q = BigInt(0x9080)
      val r = BigInt(0xa080)

      defaults()
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      redirect(a) // SRAM is still clearing; this unaccepted query must not fabricate a response.
      step(8)
      assert(observed.isEmpty)

      reset()
      enqueue() // No trigger before the first redirect: consume without learning.
      candidate(1)
      commit(p)
      step(8)
      assert(query(a).isEmpty)

      reset()
      redirect(a)
      step(4)
      enqueue()
      candidate(1)
      dut.io.commit.bits.ptr.value.poke(1.U)
      dut.io.commit.bits.startPc.addr.poke((p / 2).U)
      dut.io.commit.valid.poke(true.B)
      redirect(b, flushIdx = 1) // A simultaneous flush wins over commit promotion.
      dut.io.commit.valid.poke(false.B)
      step(8)
      assert(query(a).isEmpty)

      reset()
      redirect(a)
      step(4)
      enqueue()
      candidate(1)
      enqueue() // A BPU override rewrites the same pointer without changing the wrap flag.
      commit(p)
      step(8)
      assert(query(a).isEmpty)

      reset()
      redirect(a)
      step(4)
      enqueue()
      redirect(b) // Older FTQ entry survives, but reports its candidate only after B.
      step(4)
      candidate(2)
      commit(p)
      step(8)
      assert(query(a) == Seq(p + 64))
      assert(query(b).isEmpty)
      assert(query(p).isEmpty)

      reset()
      redirect(a)
      step(4)
      enqueue()
      candidate(1)
      commit(p)
      redirect(b) // The training slot must keep A while B owns the SRAM read port.
      redirect(b)
      step(8)
      assert(query(a) == Seq(p))
      assert(query(b).isEmpty)

      reset()
      redirect(a)
      step(4)
      enqueue()
      candidate(1)
      redirect(b, flushIdx = 1)
      candidate(1) // Late wrong-path notification after invalidation.
      commit(p)
      step(8)
      assert(query(a).isEmpty)

      reset()
      redirect(a)
      step(4)
      enqueue()
      candidate(1)
      enqueue(flag = true) // Reuse after a wrap clears both old candidate bits and ownership.
      candidate(1, flag = false)
      commit(p, flag = true)
      step(8)
      assert(query(a).isEmpty)
      enqueue(flag = true)
      candidate(1, flag = true)
      commit(q, flag = true)
      step(8)
      assert(query(a) == Seq(q))

      reset()
      train(a, p)
      redirect(a)
      step(8)
      enqueue()
      candidate(2)
      commit(p)
      step(2) // Training read then response: write buffer valid, SRAM still has mask 01.
      assert(query(a) == Seq(p, p + 64)) // Same-key forwarding must expose the updated mask.

      reset()
      train(a, p)
      train(b, q) // Two valid tags fill the set; A is now the victim.
      redirect(c)
      step(8)
      enqueue()
      candidate(1)
      commit(r)
      step(2) // C is in the write buffer, A is still physically in SRAM.
      assert(query(a).isEmpty, "the buffered replacement must hide its evicted tag")
      assert(query(c) == Seq(r))
      assert(query(b) == Seq(q))

      // Output retries and a training re-read share the physical SRAM but not the held query data.
      reset()
      train(a, p, mask = 3)
      observed.clear()
      dut.io.req(1).ready.poke(false.B)
      redirect(a)
      step(4)
      assert(observed.toSeq == Seq(p), "an accepted output lane must not be sent twice")
      enqueue()
      candidate(2)
      commit(p)
      step(8)
      dut.io.req(1).valid.expect(true.B)
      dut.io.req(1).bits.vAddr.addr.expect(((p + 64) / 2).U)
      dut.io.req(1).ready.poke(true.B)
      step(4)
      assert(observed.toSeq == Seq(p, p + 64))
      assert(query(a) == Seq(p, p + 64))
    }
  }
}
