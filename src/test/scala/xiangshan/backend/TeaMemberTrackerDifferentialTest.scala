/***************************************************************************************
* Copyright (c) 2024-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend

import chisel3._
import chisel3.simulator.Settings
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import scala.sys.process._
import scala.util.Random
import xiangshan._
import xiangshan.backend.rob._

class TeaMemberTrackerDifferential(implicit p: Parameters) extends TeaMemberTracker(3, 2) {
  val check = IO(Input(Bool()))
  private val reference = Module(new TeaMemberTrackerReference(3, 2))
  reference.io.enable := io.enable
  reference.io.cycle := io.cycle
  reference.io.addrTransType := io.addrTransType
  reference.io.alloc := io.alloc
  reference.io.enqPsv := io.enqPsv
  reference.io.writeback := io.writeback
  reference.io.events := io.events
  reference.io.redirect := io.redirect
  reference.io.slotReady := io.slotReady
  reference.io.commits := io.commits
  reference.io.head := io.head
  reference.io.readPtr := io.readPtr
  reference.io.readPc := io.readPc
  reference.io.markStalled := io.markStalled
  when(check) {
    assert(io.read.asUInt === reference.io.read.asUInt, "TEA native read mismatch")
    assert(io.stalled.asUInt === reference.io.stalled.asUInt, "TEA native stalled mismatch")
    assert(io.committed.asUInt === reference.io.committed.asUInt, "TEA native committed mismatch")
    assert(io.finals.asUInt === reference.io.finals.asUInt, "TEA native finals mismatch")
  }
}

class TeaMemberTrackerDifferentialTest extends XSTester {
  behavior of "TEA native event history"
  private val trackerConfig = config.alterPartial {
    case XSCoreParamsKey => config(XSCoreParamsKey).copy(RobSize = 8, RenameWidth = 4, CommitWidth = 2)
  }
  private val nativeSettings = {
    val directory = Files.createDirectories(Paths.get("build/chiselsim/tea-native-differential")).toAbsolutePath
    val source = directory.resolve("member-tracker.cpp")
    val obj = directory.resolve("member-tracker.o")
    Files.writeString(source, TeaMemberTrackerNative.cpp("TeaMemberTrackerNative_8_4_2_3_2_0_eval", 8, 4, 2, 3, 2))
    require(Seq("g++", "-std=c++17", "-O2", "-c", source.toString, "-o", obj.toString).! == 0)
    Settings.default[TeaMemberTrackerDifferential].withLibraryPaths(Seq(obj.toString))
  }

  it should "match every RTL output across conflicting events, flushes and ROB reuse" in {
    simulate(new TeaMemberTrackerDifferential()(trackerConfig), settings = nativeSettings) { dut =>
      val random = new Random(0x7ea)
      case class Identity(entry: Int, flag: Boolean, former: Boolean, rank: Int)
      def ptr(port: RobPtr, id: Identity): Unit = {
        port.value.poke(id.entry.U)
        port.flag.poke(id.flag.B)
        port.isFormer.poke(id.former.B)
      }
      def identity(): Identity = Identity(random.nextInt(8), random.nextBoolean(), random.nextBoolean(), random.nextInt(4))
      def clear(cycle: Int): Unit = {
        dut.io.cycle.poke(cycle.U)
        dut.io.alloc.foreach(_.valid.poke(false.B))
        dut.io.enqPsv.foreach(_.valid.poke(false.B))
        dut.io.writeback.foreach(_.valid.poke(false.B))
        dut.io.events.foreach(_.valid.poke(false.B))
        dut.io.commits.foreach(_.valid.poke(false.B))
        dut.io.redirect.valid.poke(false.B)
        dut.io.markStalled.poke(false.B)
      }
      def allocate(lane: Int, id: Identity, cycle: Int): Unit = {
        val a = dut.io.alloc(lane)
        a.valid.poke(true.B)
        ptr(a.bits.robIdx, id)
        a.bits.rank.poke(id.rank.U)
        a.bits.owner.poke(random.nextInt(4).U)
        a.bits.pc.poke((0x80000000L + cycle * 64L + lane * 4L).U)
        a.bits.psv.poke(random.nextInt(256).U)
        a.bits.numWB.poke(random.nextInt(5).U)
      }
      dut.io.enable.poke(true.B)
      dut.io.readPc.foreach(_.poke("h80000000".U))
      dut.check.poke(false.B)
      dut.io.addrTransType.bare.poke(true.B)
      dut.io.addrTransType.sv39.poke(false.B)
      dut.io.addrTransType.sv39x4.poke(false.B)
      dut.io.addrTransType.sv48.poke(false.B)
      dut.io.addrTransType.sv48x4.poke(false.B)
      ptr(dut.io.head, Identity(0, false, true, 0))
      dut.io.readPtr.foreach(p => ptr(p, Identity(0, false, true, 0)))
      dut.io.commits.foreach(c => ptr(c.bits, Identity(0, false, true, 0)))
      dut.io.slotReady.foreach(_.foreach(_.poke(false.B)))
      // Initialize unused payloads too: computing rows expose all PC/PSV lanes.
      for (entry <- 0 until 8) {
        clear(entry)
        for (rank <- 0 until 4) allocate(rank, Identity(entry, false, rank % 2 == 0, rank), entry)
        dut.clock.step()
      }
      dut.check.poke(true.B)
      var recent = Vector.tabulate(4)(r => Identity(7, false, r % 2 == 0, r))
      for (cycle <- 8 until 2008) {
        clear(cycle)
        val chosen = if (random.nextBoolean()) recent(random.nextInt(recent.size)) else identity()
        ptr(dut.io.head, chosen)
        dut.io.readPtr.foreach(p => ptr(p, if (random.nextBoolean()) chosen else identity()))
        dut.io.readPc.foreach(_.poke((0x80000000L + random.nextInt(cycle + 1) * 64L + random.nextInt(4) * 4L).U))
        dut.io.slotReady.foreach(_.foreach(_.poke(random.nextBoolean().B)))
        dut.io.markStalled.poke(random.nextBoolean().B)
        if (random.nextInt(3) != 0) {
          val base = identity()
          recent = Vector.tabulate(1 + random.nextInt(4))(r => base.copy(rank = r, former = random.nextBoolean()))
          recent.zipWithIndex.foreach { case (id, lane) => allocate(lane, id, cycle) }
        }
        dut.io.enqPsv.foreach { u =>
          val id = if (random.nextInt(4) != 0) recent(random.nextInt(recent.size)) else identity()
          u.valid.poke(random.nextBoolean().B)
          ptr(u.bits.robIdx, id)
          u.bits.rank.poke(id.rank.U)
          u.bits.mask.poke(random.nextInt(256).U)
        }
        dut.io.writeback.foreach { w =>
          val id = if (random.nextInt(4) != 0) chosen else identity()
          w.valid.poke(random.nextBoolean().B)
          ptr(w.bits.robIdx, id)
          w.bits.rank.poke(id.rank.U)
        }
        dut.io.events.foreach { e =>
          e.valid.poke(random.nextBoolean().B)
          ptr(e.bits.robIdx, if (random.nextBoolean()) chosen else recent.head)
          e.bits.mask.poke(random.nextInt(256).U)
        }
        for (lane <- 0 until 2) {
          val id = if (lane == 0) chosen else identity()
          for (slot <- 0 until 2) {
            dut.io.commits(2 * lane + slot).valid.poke(random.nextBoolean().B)
            ptr(dut.io.commits(2 * lane + slot).bits, id.copy(former = slot == 0))
          }
        }
        dut.io.redirect.valid.poke((random.nextInt(5) == 0).B)
        ptr(dut.io.redirect.bits.robIdx, chosen)
        dut.io.redirect.bits.level.poke(random.nextBoolean().B)
        dut.clock.step()
      }
    }
  }
}
