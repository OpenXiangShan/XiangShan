/***************************************************************************************
 * Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
 *
 * XiangShan is licensed under Mulan PSL v2.
 ***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import freechips.rocketchip.tilelink.TLPermissions
import oceanus.compactchi.CCHIOpcode
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}

class CCHISpecMissBufferTest extends AnyFlatSpec with ChiselSim {
  private def parameters(channels: Int): Parameters = {
    val base = new DefaultConfig
    val core = base(XSTileKey).head
    base.alterPartial {
      case XSCoreParamsKey => core.copy(
        dcacheParametersOpt = core.dcacheParametersOpt.map(_.copy(nCCHIChannels = channels)),
        cchiUpstream = L1CCHIUpstream(channels)
      )
    }
  }

  private def zero(data: Data): Unit = data match {
    case b: Bool => b.poke(false.B)
    case u: UInt => u.poke(0.U)
    case r: Record => r.elements.values.foreach(zero)
    case v: Vec[_] => v.foreach(zero)
    case other => throw new IllegalArgumentException(s"Unexpected input: $other")
  }

  behavior of "CCHI speculative request buffering"

  for (channels <- Seq(1, 2)) {
    it should s"resolve S2 requests before L2 and preserve traffic with $channels channel(s)" in {
      import chisel3.simulator.HasSimulator.simulators.verilator
      simulate(new CCHISpecMissBuffer()(parameters(channels))) { dut =>
        dut.io.normal.foreach { req => zero(req.bits); req.valid.poke(false.B) }
        dut.io.spec.candidate.foreach { req => zero(req.bits); req.valid.poke(false.B) }
        dut.io.spec.resolve.foreach(zero)
        dut.io.out.foreach(_.ready.poke(true.B))
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)

        def defaults(): Unit = {
          dut.io.normal.foreach(_.valid.poke(false.B))
          dut.io.spec.candidate.foreach(_.valid.poke(false.B))
          dut.io.spec.resolve.foreach(_.valid.poke(false.B))
        }
        def candidate(lane: Int, id: Int, ch: Int): Unit = {
          val req = dut.io.spec.candidate(lane)
          req.valid.poke(true.B)
          req.bits.id.poke(id.U)
          req.bits.channel.poke(ch.U)
          req.bits.alias.poke(1.U)
          req.bits.vaddr.poke((0x80000000L + id * 64).U)
          req.bits.paddr.poke((0x80000000L + id * 64).U)
          req.bits.grow.poke(TLPermissions.NtoB)
        }
        def resolve(lane: Int, id: Int, commit: Boolean): Unit = {
          dut.io.spec.resolve(lane).valid.poke(true.B)
          dut.io.spec.resolve(lane).bits.id.poke(id.U)
          dut.io.spec.resolve(lane).bits.commit.poke(commit.B)
        }
        def normal(id: Int): Unit = {
          dut.io.normal(0).valid.poke(true.B)
          dut.io.normal(0).bits.TxnID.poke(id.U)
          dut.io.normal(0).bits.Addr.poke((0x90000000L + id * 64).U)
        }

        // S1 acceptance must not expose an unresolved request to L2 in S2.
        candidate(0, 1, 0)
        dut.io.spec.candidate(0).ready.expect(true.B)
        dut.io.out(0).valid.expect(false.B)
        dut.clock.step()
        defaults()
        dut.io.out(0).valid.expect(false.B)
        resolve(0, 1, commit = false)
        dut.io.spec.committed(0).expect(false.B)
        dut.io.out(0).valid.expect(false.B)
        dut.clock.step()
        defaults()
        dut.io.out(0).valid.expect(false.B)

        // A head request can depart in the same cycle as its S2 commit.
        val ch = channels - 1
        candidate(0, 2, ch)
        dut.clock.step()
        defaults()
        resolve(0, 2, commit = true)
        dut.io.spec.committed(0).expect(true.B)
        dut.io.out(ch).valid.expect(true.B)
        dut.io.out(ch).bits.TxnID.expect(2.U)
        dut.io.out(ch).bits.SrcID.expect(ch.U)
        dut.io.out(ch).bits.TagAlias.expect(1.U)
        dut.io.out(ch).bits.Addr.expect((0x80000000L + 2 * 64).U)
        dut.io.out(ch).bits.Opcode.expect(CCHIOpcode.ReadShared.U)
        dut.io.out(ch).bits.ExpCompData.expect(true.B)
        dut.clock.step()
        defaults()
        dut.io.out(ch).valid.expect(false.B)

        // Commit survives backpressure; reusing the lane must only cancel the new ID.
        dut.io.out(0).ready.poke(false.B)
        candidate(0, 3, 0)
        dut.clock.step()
        defaults()
        resolve(0, 3, commit = true)
        dut.clock.step()
        defaults()
        candidate(0, 4, 0)
        dut.clock.step()
        defaults()
        resolve(0, 4, commit = false)
        dut.clock.step()
        defaults()
        dut.io.out(0).valid.expect(true.B)
        dut.io.out(0).bits.TxnID.expect(3.U)
        dut.clock.step(3)
        dut.io.out(0).bits.TxnID.expect(3.U)
        dut.io.out(0).ready.poke(true.B)
        dut.clock.step()
        dut.io.out(0).valid.expect(false.B)

        // Cancel a non-head candidate without disturbing an older ordinary request.
        dut.io.out(0).ready.poke(false.B)
        normal(10)
        dut.clock.step()
        defaults()
        candidate(0, 5, 0)
        dut.clock.step()
        defaults()
        resolve(0, 5, commit = false)
        dut.clock.step()
        defaults()
        dut.io.out(0).valid.expect(true.B)
        dut.io.out(0).bits.TxnID.expect(10.U)
        dut.io.out(0).ready.poke(true.B)
        dut.clock.step()
        dut.io.out(0).valid.expect(false.B)

        // A full queue cannot grant a candidate that would have no cancelable slot.
        dut.io.out(0).ready.poke(false.B)
        normal(11)
        dut.clock.step()
        normal(12)
        dut.clock.step()
        defaults()
        candidate(0, 6, 0)
        dut.io.spec.candidate(0).ready.expect(false.B)
        dut.clock.step()
        defaults()
        dut.io.out(0).ready.poke(true.B)
        dut.io.out(0).bits.TxnID.expect(11.U)
        dut.clock.step()
        dut.io.out(0).bits.TxnID.expect(12.U)
        dut.clock.step()
        dut.io.out(0).valid.expect(false.B)

        // Simultaneous normal/speculative contenders must produce only one enqueue.
        dut.io.out(0).ready.poke(false.B)
        normal(13)
        candidate(0, 9, 0)
        val specWon = dut.io.spec.candidate(0).ready.peek().litToBoolean
        dut.io.normal(0).ready.expect((!specWon).B)
        dut.clock.step()
        defaults()
        if (specWon) {
          resolve(0, 9, commit = false)
          dut.io.out(0).valid.expect(false.B)
          dut.clock.step()
          defaults()
          dut.io.out(0).valid.expect(false.B)
        } else {
          dut.io.out(0).valid.expect(true.B)
          dut.io.out(0).bits.TxnID.expect(13.U)
          dut.io.out(0).ready.poke(true.B)
          dut.clock.step()
        }
        dut.io.out(0).ready.poke(true.B)

        // A commit behind ordinary traffic is remembered after resolve disappears.
        dut.io.out(0).ready.poke(false.B)
        normal(14)
        dut.clock.step()
        defaults()
        candidate(0, 10, 0)
        dut.clock.step()
        defaults()
        resolve(0, 10, commit = true)
        dut.io.spec.committed(0).expect(true.B)
        dut.clock.step()
        defaults()
        dut.io.out(0).bits.TxnID.expect(14.U)
        dut.io.out(0).ready.poke(true.B)
        dut.clock.step()
        dut.io.out(0).valid.expect(true.B)
        dut.io.out(0).bits.TxnID.expect(10.U)
        dut.clock.step()
        dut.io.out(0).valid.expect(false.B)

        if (channels == 2) {
          // Two lanes on distinct channels resolve independently in the same S2.
          candidate(0, 7, 0)
          candidate(1, 8, 1)
          dut.io.spec.candidate(0).ready.expect(true.B)
          dut.io.spec.candidate(1).ready.expect(true.B)
          dut.clock.step()
          defaults()
          resolve(0, 7, commit = true)
          resolve(1, 8, commit = false)
          dut.io.out(0).valid.expect(true.B)
          dut.io.out(1).valid.expect(false.B)
          dut.io.spec.committed(0).expect(true.B)
          dut.io.spec.committed(1).expect(false.B)
          dut.clock.step()
          defaults()
        }
        dut.io.out.foreach(_.valid.expect(false.B))
      }
    }
  }
}
