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
import xiangshan._
import xiangshan.backend.rob.{RobPtr, TeaMemberTracker}

class TeaMemberTrackerTest extends XSTester {
  behavior of "TEA architectural member tracking"

  private val trackerConfig = config.alterPartial {
    case XSCoreParamsKey => config(XSCoreParamsKey).copy(RobSize = 8, RenameWidth = 4, CommitWidth = 2)
  }
  private val sv39TrackerConfig = trackerConfig.alterPartial {
    case XSCoreParamsKey => trackerConfig(XSCoreParamsKey).copy(HasHExtension = false, EnableSv48 = false)
  }

  private def ptr(port: RobPtr, former: Boolean = true, flag: Boolean = false): Unit = {
    port.value.poke(0.U)
    port.flag.poke(flag.B)
    port.isFormer.poke(former.B)
  }

  private def init(dut: TeaMemberTracker): Unit = {
    dut.io.cycle.poke(10.U)
    dut.io.addrTransType.bare.poke(true.B)
    dut.io.addrTransType.sv39.poke(false.B)
    dut.io.addrTransType.sv39x4.poke(false.B)
    dut.io.addrTransType.sv48.poke(false.B)
    dut.io.addrTransType.sv48x4.poke(false.B)
    dut.io.alloc.foreach(_.valid.poke(false.B))
    dut.io.enqPsv.foreach(_.valid.poke(false.B))
    dut.io.writeback.foreach(_.valid.poke(false.B))
    dut.io.events.foreach(_.valid.poke(false.B))
    dut.io.commits.foreach(_.valid.poke(false.B))
    dut.io.commits.foreach(x => ptr(x.bits))
    dut.io.redirect.valid.poke(false.B)
    dut.io.slotReady.foreach(_.foreach(_.poke(false.B)))
    dut.io.markStalled.poke(false.B)
    ptr(dut.io.head)
    dut.io.readPtr.foreach(x => ptr(x))
  }

  private def alloc(dut: TeaMemberTracker, rank: Int, former: Boolean = true,
                    owner: Int = -1, numWB: Int = 1, psv: Int = 0): Unit = {
    val port = dut.io.alloc(rank)
    port.valid.poke(true.B)
    ptr(port.bits.robIdx, former)
    port.bits.rank.poke(rank.U)
    port.bits.owner.poke((if (owner < 0) rank else owner).U)
    port.bits.numWB.poke(numWB.U)
    port.bits.pc.poke((0x80001000L + 4 * rank).U)
    port.bits.psv.poke(psv.U)
  }

  it should "attribute a long simple run to unfinished B and retire every member" in {
    simulate(new TeaMemberTracker(3, 2)(trackerConfig)) { dut =>
      init(dut)
      alloc(dut, 0, psv = 1 << TeaEvent.DR_L1)
      alloc(dut, 1)
      alloc(dut, 2)
      dut.clock.step()
      dut.io.alloc.foreach(_.valid.poke(false.B))
      dut.io.stalled.bits.rank.expect(0.U)
      for ((rank, port) <- Seq(0 -> 0, 2 -> 1)) {
        dut.io.writeback(port).valid.poke(true.B)
        ptr(dut.io.writeback(port).bits.robIdx)
        dut.io.writeback(port).bits.rank.poke(rank.U)
      }
      dut.clock.step()
      dut.io.writeback.foreach(_.valid.poke(false.B))
      dut.io.stalled.valid.expect(true.B)
      dut.io.stalled.bits.rank.expect(1.U)
      dut.io.stalled.bits.allocationCycle.expect(10.U)
      dut.io.markStalled.poke(true.B)
      dut.clock.step(5)
      dut.io.markStalled.poke(false.B)
      dut.io.writeback(0).valid.poke(true.B)
      ptr(dut.io.writeback(0).bits.robIdx)
      dut.io.writeback(0).bits.rank.poke(1.U)
      dut.clock.step()
      dut.io.writeback(0).valid.poke(false.B)
      dut.io.stalled.valid.expect(false.B)
      dut.io.commits(0).valid.poke(true.B)
      for (rank <- 0 until 3) {
        dut.io.committed(rank).valid.expect(true.B)
        dut.io.committed(rank).bits.pc.expect((0x80001000L + 4 * rank).U)
        dut.io.committed(rank).bits.psv.expect((if (rank == 0) 1 << TeaEvent.DR_L1 else 0).U)
        dut.io.finals(rank).valid.expect((rank == 1).B)
      }
      dut.io.finals(1).bits.cancelled.expect(false.B)
      dut.io.finals(1).bits.member.target.rank.expect(1.U)
      dut.clock.step()
      dut.io.finals.foreach(_.valid.expect(false.B))
    }
  }

  it should "track an undispatched latter, merge late events, and cancel without aliasing ROB reuse" in {
    simulate(new TeaMemberTracker(3, 2)(trackerConfig)) { dut =>
      init(dut)
      alloc(dut, 0, numWB = 0, psv = 1 << TeaEvent.DR_L1)
      alloc(dut, 1, former = false)
      dut.clock.step()
      dut.io.alloc.foreach(_.valid.poke(false.B))
      dut.io.slotReady(0)(0).poke(true.B)
      dut.io.stalled.valid.expect(true.B)
      dut.io.stalled.bits.rank.expect(1.U)
      dut.io.markStalled.poke(true.B)
      dut.clock.step()
      dut.io.markStalled.poke(false.B)
      dut.io.enqPsv(0).valid.poke(true.B)
      ptr(dut.io.enqPsv(0).bits.robIdx, former = false)
      dut.io.enqPsv(0).bits.rank.poke(1.U)
      dut.io.enqPsv(0).bits.mask.poke((1 << TeaEvent.DR_SQ).U)
      dut.io.events(0).valid.poke(true.B)
      ptr(dut.io.events(0).bits.robIdx, former = false)
      dut.io.events(0).bits.mask.poke((1 << TeaEvent.ST_TLB).U)
      dut.clock.step()
      dut.io.enqPsv(0).valid.poke(false.B)
      dut.io.events(0).bits.mask.poke((1 << TeaEvent.FL_EX).U)
      ptr(dut.io.readPtr(0), former = false)
      val latterPsv = (1 << TeaEvent.DR_SQ) | (1 << TeaEvent.ST_TLB) | (1 << TeaEvent.FL_EX)
      dut.io.read(0).psv.expect(latterPsv.U)
      dut.io.read(1).psv.expect((1 << TeaEvent.DR_L1).U)
      dut.io.redirect.valid.poke(true.B)
      ptr(dut.io.redirect.bits.robIdx)
      dut.io.redirect.bits.level.poke(false.B) // flush after former, preserving it
      dut.io.finals(1).valid.expect(true.B)
      dut.io.finals(1).bits.cancelled.expect(true.B)
      dut.io.finals(1).bits.member.psv.expect(latterPsv.U)
      dut.clock.step()
      dut.io.redirect.valid.poke(false.B)
      dut.io.events(0).valid.poke(false.B)
      dut.io.commits(0).valid.poke(true.B)
      dut.io.committed(0).valid.expect(true.B)
      dut.io.committed(1).valid.expect(false.B)
      dut.clock.step()
      dut.io.commits(0).valid.poke(false.B)
      dut.io.cycle.poke(40.U)
      alloc(dut, 0)
      dut.io.alloc(0).bits.robIdx.flag.poke(true.B)
      ptr(dut.io.head, flag = true)
      dut.io.slotReady(0)(0).poke(false.B)
      dut.clock.step()
      dut.io.alloc(0).valid.poke(false.B)
      dut.io.stalled.bits.allocationCycle.expect(40.U)
      dut.io.finals.foreach(_.valid.expect(false.B))
    }
  }

  it should "share fusion execution readiness while retaining follower and eliminated move PCs" in {
    simulate(new TeaMemberTracker(3, 2)(trackerConfig)) { dut =>
      init(dut)
      alloc(dut, 0)
      alloc(dut, 1, owner = 0, numWB = 0, psv = 1 << TeaEvent.DR_TLB)
      alloc(dut, 2, numWB = 0)
      dut.clock.step()
      dut.io.alloc.foreach(_.valid.poke(false.B))
      dut.io.stalled.bits.rank.expect(0.U)
      dut.io.writeback(0).valid.poke(true.B)
      ptr(dut.io.writeback(0).bits.robIdx)
      dut.io.writeback(0).bits.rank.poke(0.U)
      dut.clock.step()
      dut.io.writeback(0).valid.poke(false.B)
      dut.io.stalled.valid.expect(false.B)
      dut.io.commits(0).valid.poke(true.B)
      dut.io.committed.take(3).foreach(_.valid.expect(true.B))
      dut.io.committed(1).bits.pc.expect("h80001004".U)
      dut.io.committed(1).bits.psv.expect((1 << TeaEvent.DR_TLB).U)
      dut.io.committed(2).bits.pc.expect("h80001008".U)
    }
  }

  it should "use functional completion for singleton multi-uop instructions" in {
    simulate(new TeaMemberTracker(3, 2)(trackerConfig)) { dut =>
      init(dut)
      alloc(dut, 0)
      dut.clock.step()
      dut.io.alloc(0).valid.poke(false.B)
      dut.io.writeback(0).valid.poke(true.B)
      ptr(dut.io.writeback(0).bits.robIdx)
      dut.io.writeback(0).bits.rank.poke(0.U)
      dut.clock.step()
      dut.io.writeback(0).valid.poke(false.B)
      dut.io.stalled.valid.expect(true.B)
      dut.io.slotReady(0)(0).poke(true.B)
      dut.io.stalled.valid.expect(false.B)
    }
  }

  it should "preserve full PCs using the allocation-time address translation mode" in {
    simulate(new TeaMemberTracker(3, 2)(sv39TrackerConfig)) { dut =>
      init(dut)
      dut.io.addrTransType.bare.poke(false.B)
      dut.io.addrTransType.sv39.poke(true.B)
      alloc(dut, 0)
      dut.io.alloc(0).bits.pc.poke("h7f80001000".U)
      dut.clock.step()
      dut.io.alloc(0).valid.poke(false.B)
      dut.io.read(0).pc.expect("hffffffff80001000".U)

      dut.io.commits(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.commits(0).valid.poke(false.B)
      dut.io.addrTransType.bare.poke(true.B)
      dut.io.addrTransType.sv39.poke(false.B)
      alloc(dut, 0)
      dut.io.alloc(0).bits.robIdx.flag.poke(true.B)
      dut.io.alloc(0).bits.pc.poke("h7f80001000".U)
      ptr(dut.io.head, flag = true)
      ptr(dut.io.readPtr(0), flag = true)
      dut.clock.step()
      dut.io.alloc(0).valid.poke(false.B)
      dut.io.read(0).pc.expect("h0000007f80001000".U)
    }
  }
}
