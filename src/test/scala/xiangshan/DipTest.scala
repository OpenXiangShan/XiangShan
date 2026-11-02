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

package xiangshan

import chisel3._
import xiangshan.backend.rob.RobPtr

class DipTest extends XSTester {
  behavior of "DIP dispatch sampling"
  private val dipConfig = config.alterPartial {
    case XSCoreParamsKey => config(XSCoreParamsKey).copy(RobSize = 8, RenameWidth = 4, CommitWidth = 2)
  }
  private def ptr(port: RobPtr): Unit = {
    port.flag.poke(false.B)
    port.value.poke(0.U)
    port.isFormer.poke(true.B)
  }
  private def init(dut: DipSampler): Unit = {
    dut.io.enable.poke(true.B)
    dut.io.period.poke(1.U)
    dut.io.dispatch.validMask.poke(0.U)
    dut.io.dispatch.fireMask.poke(0.U)
    dut.io.dispatch.stallCause.poke(0.U)
    dut.io.dispatch.uops.zipWithIndex.foreach { case (uop, lane) =>
      ptr(uop.robIdx)
      uop.pc.poke((0x1000 + 4 * lane).U)
      uop.psv.poke(0.U)
    }
    dut.io.rob.cycle.poke(0.U)
    dut.io.rob.retired.poke(0.U)
    dut.io.rob.commits.foreach { commit =>
      commit.valid.poke(false.B)
      ptr(commit.bits)
    }
    dut.io.rob.flush.valid.poke(false.B)
    ptr(dut.io.rob.flush.bits.robIdx)
    dut.io.rob.flush.bits.self.poke(false.B)
    dut.io.rob.flush.bits.pc.poke(0x2000.U)
    dut.io.rob.flush.bits.cause.poke(1.U)
  }

  it should "compact fired lanes and attribute all remaining capacity to the oldest blocked uop" in {
    simulate(new DipSampler()(dipConfig)) { dut =>
      init(dut)
      dut.io.dispatch.validMask.poke("b1100".U)
      dut.io.dispatch.fireMask.poke("b0100".U)
      dut.io.dispatch.stallCause.poke(3.U) // ROB + IntIQ, retaining both causes
      dut.io.trace.valid.expect(true.B)
      dut.io.trace.bits.slots(0).state.expect(DipState.base.U)
      dut.io.trace.bits.slots(0).pc.expect(0x1008.U)
      dut.io.trace.bits.slots.tail.foreach { slot =>
        slot.state.expect(DipState.stall.U)
        slot.cause.expect(3.U)
        slot.kind.expect(DipTargetKind.rob.U)
        slot.pc.expect(0x100c.U)
      }
    }
  }

  it should "retain a flush cause until a recovery uop enters dispatch" in {
    simulate(new DipSampler()(dipConfig)) { dut =>
      init(dut)
      dut.io.rob.flush.valid.poke(true.B)
      dut.io.trace.bits.slots.foreach { slot =>
        slot.state.expect(DipState.misspeculation.U)
        slot.pc.expect(0x2000.U)
      }
      dut.clock.step()
      dut.io.rob.flush.valid.poke(false.B)
      dut.clock.step(5)
      dut.io.trace.bits.slots.foreach(_.state.expect(DipState.misspeculation.U))
      // Arrival ends recovery even when ROB backpressure prevents dispatch.
      dut.io.dispatch.validMask.poke(1.U)
      dut.io.dispatch.stallCause.poke(1.U)
      dut.io.trace.bits.slots.foreach { slot =>
        slot.state.expect(DipState.stall.U)
        slot.pc.expect(0x1000.U)
      }
      dut.clock.step()
      dut.io.dispatch.validMask.poke(0.U)
      dut.io.dispatch.stallCause.poke(0.U)
      dut.io.trace.bits.slots.foreach { slot =>
        slot.state.expect(DipState.frontend.U)
        slot.kind.expect(DipTargetKind.successor.U)
      }
    }
  }

  it should "sample periodically without exporting unsampled lifecycle events" in {
    simulate(new DipSampler()(dipConfig)) { dut =>
      init(dut)
      dut.io.period.poke(3.U)
      for (cycle <- 0 until 7) {
        dut.io.trace.valid.expect((cycle % 3 == 0).B)
        dut.clock.step()
      }
      dut.io.enable.poke(false.B)
      dut.io.rob.commits(0).valid.poke(true.B)
      dut.io.trace.valid.expect(false.B)
      dut.io.finals.foreach(_.valid.expect(false.B))
      dut.clock.step()
      dut.io.rob.commits(0).valid.poke(false.B)
      dut.io.enable.poke(true.B)
      dut.io.dispatch.stallCause.poke((BigInt(1) << DipCause.rename).U)
      dut.io.trace.valid.expect(true.B)
      dut.io.trace.bits.slots.foreach { slot =>
        slot.state.expect(DipState.stall.U)
        slot.kind.expect(DipTargetKind.successor.U)
        slot.cause.expect((BigInt(1) << DipCause.rename).U)
      }
    }
  }
  it should "emit terminal events only for selected targets, including after sampling stops" in {
    simulate(new DipSampler()(dipConfig)) { dut =>
      init(dut)
      dut.io.period.poke(10.U)
      dut.io.dispatch.validMask.poke(15.U)
      dut.io.dispatch.fireMask.poke(15.U)
      dut.io.dispatch.uops.foreach(_.robIdx.value.poke(1.U))
      dut.clock.step()
      dut.io.rob.cycle.poke(1.U)
      dut.io.dispatch.validMask.poke(1.U)
      dut.io.dispatch.fireMask.poke(1.U)
      dut.io.dispatch.uops(0).robIdx.value.poke(2.U)
      dut.io.trace.valid.expect(false.B)
      dut.io.finals.foreach(_.valid.expect(false.B))
      dut.clock.step()
      dut.io.rob.cycle.poke(2.U)
      dut.io.enable.poke(false.B)
      dut.io.dispatch.validMask.poke(0.U)
      dut.io.dispatch.fireMask.poke(0.U)
      dut.io.rob.commits(0).valid.poke(true.B)
      dut.io.rob.commits(0).bits.value.poke(2.U)
      dut.io.finals.foreach(_.valid.expect(false.B))
      dut.clock.step()
      dut.io.rob.cycle.poke(3.U)
      dut.io.rob.commits(0).bits.value.poke(1.U)
      dut.io.finals(0).valid.expect(true.B)
      dut.io.finals(0).bits.target.generation.expect(0.U)
      dut.clock.step()
      dut.io.rob.commits(0).valid.poke(false.B)
      dut.io.finals.foreach(_.valid.expect(false.B))
    }
  }

  it should "bind frontend waiting once and create a new generation after ROB reuse" in {
    simulate(new DipSampler()(dipConfig)) { dut =>
      init(dut)
      dut.clock.step()
      dut.io.rob.cycle.poke(1.U)
      dut.io.dispatch.validMask.poke(1.U)
      dut.io.dispatch.fireMask.poke(1.U)
      dut.io.dispatch.uops(0).robIdx.value.poke(1.U)
      dut.io.dispatch.uops(0).psv.poke(3.U)
      dut.io.finals(5).valid.expect(true.B)
      dut.io.finals(5).bits.target.generation.expect(0.U)
      dut.io.finals(5).bits.next.generation.expect(1.U)
      dut.io.finals(5).bits.cause.expect(3.U)
      dut.clock.step()
      dut.io.rob.cycle.poke(2.U)
      dut.io.enable.poke(false.B)
      dut.io.dispatch.validMask.poke(0.U)
      dut.io.dispatch.fireMask.poke(0.U)
      dut.io.rob.commits(0).valid.poke(true.B)
      dut.io.rob.commits(0).bits.value.poke(1.U)
      dut.io.finals(0).valid.expect(true.B)
      dut.io.finals(0).bits.target.generation.expect(1.U)
      dut.clock.step()
      dut.io.rob.cycle.poke(3.U)
      dut.io.rob.commits(0).valid.poke(false.B)
      dut.io.enable.poke(true.B)
      dut.io.dispatch.validMask.poke(1.U)
      dut.io.dispatch.fireMask.poke(1.U)
      dut.io.dispatch.uops(0).robIdx.flag.poke(true.B)
      dut.io.trace.bits.slots(0).target.generation.expect(3.U)
      dut.clock.step()
      dut.io.rob.cycle.poke(4.U)
      dut.io.enable.poke(false.B)
      dut.io.dispatch.validMask.poke(0.U)
      dut.io.dispatch.fireMask.poke(0.U)
      dut.io.rob.commits(0).valid.poke(true.B)
      dut.io.rob.commits(0).bits.flag.poke(true.B)
      dut.io.finals(0).bits.target.generation.expect(3.U)
    }
  }

  it should "track nested flush causes and settle self-flushed slots at a later commit" in {
    simulate(new DipSampler()(dipConfig)) { dut =>
      init(dut)
      dut.io.dispatch.validMask.poke(1.U)
      dut.io.dispatch.fireMask.poke(1.U)
      dut.io.dispatch.uops(0).robIdx.value.poke(3.U)
      dut.clock.step()
      dut.io.rob.cycle.poke(1.U)
      dut.io.dispatch.validMask.poke(0.U)
      dut.io.dispatch.fireMask.poke(0.U)
      dut.io.rob.flush.valid.poke(true.B)
      dut.io.rob.flush.bits.robIdx.value.poke(2.U)
      dut.io.finals(4).valid.expect(true.B)
      dut.io.finals(4).bits.target.generation.expect(1.U)
      dut.clock.step()
      dut.io.rob.cycle.poke(2.U)
      dut.io.rob.flush.bits.robIdx.value.poke(1.U)
      dut.io.rob.flush.bits.robIdx.isFormer.poke(false.B)
      dut.io.rob.flush.bits.self.poke(true.B)
      dut.io.finals(4).valid.expect(true.B)
      dut.io.finals(4).bits.target.generation.expect(2.U)
      dut.clock.step()
      dut.io.rob.cycle.poke(3.U)
      dut.io.rob.flush.valid.poke(false.B)
      dut.io.rob.commits(0).valid.poke(true.B)
      dut.io.rob.commits(0).bits.value.poke(2.U)
      dut.io.finals(6).valid.expect(true.B)
      dut.io.finals(6).bits.kind.expect(DipEventKind.watermark.U)
      dut.io.trace.bits.slots.foreach(_.kind.expect(DipTargetKind.resolved.U))
      dut.clock.step()
      dut.io.rob.cycle.poke(4.U)
      dut.io.rob.commits(0).valid.poke(false.B)
      dut.io.finals.foreach(_.valid.expect(false.B))
      dut.io.trace.bits.slots.foreach(_.kind.expect(DipTargetKind.resolved.U))
    }
  }

}
