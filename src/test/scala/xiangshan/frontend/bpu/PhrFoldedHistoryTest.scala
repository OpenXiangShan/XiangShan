// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import chisel3.simulator.EphemeralSimulator._ // scalastyle:ignore
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import top.MinimalConfig
import xiangshan.XSCoreParamsKey
import xiangshan.XSTileKey
import xiangshan.frontend.bpu.history.phr.PhrFoldedHistory
import xiangshan.frontend.bpu.history.phr.PhrModule

// Checks that PhrFoldedHistory.update stays equivalent to a from-scratch fold
// once it is generalized to advance by a runtime-selected num <= maxUpdateNum,
// instead of always advancing by exactly maxUpdateNum.
class PhrFoldedHistoryTest extends AnyFlatSpec {
  private val baseConfig = new MinimalConfig
  private val config: Parameters = baseConfig.alterPartial({
    case XSCoreParamsKey => baseConfig(XSTileKey).head
  })

  // the folded history is constructed to support advancing by up to maxUpdateNum in one update,
  // and every update call in this test advances by either shamt or maxUpdateNum
  private val shamt:        Int = config(XSCoreParamsKey).frontendParameters.bpuParameters.phrParameters.Shamt
  private val maxUpdateNum: Int = 2 * shamt
  // foldedLength/historyLength are picked so that HistoryLength > FoldedLength (needOldestBits branch)
  // and every oldest-evicted bit wraps around at least once, exercising the xor logic fully
  private val foldedLength:  Int = maxUpdateNum + 2
  private val historyLength: Int = 2 * foldedLength + 1
  private val foldInfo = new FoldedHistoryInfo(historyLength, foldedLength)

  class TestModule(implicit p: Parameters) extends PhrModule with PhrHelper {
    class CtrlIO extends Bundle {
      val valid:     Bool = Input(Bool())
      val useMaxNum: Bool = Input(Bool()) // selects num = maxUpdateNum instead of num = shamt for this advance
      val newBits:   UInt = Input(UInt(maxUpdateNum.W))
    }
    val ctrl: CtrlIO = IO(new CtrlIO)

    class ResultIO extends Bundle {
      val incrementalFolded: UInt = Output(UInt(foldedLength.W))
      val referenceFolded:   UInt = Output(UInt(foldedLength.W))
    }
    val res: ResultIO = IO(new ResultIO)

    // raw path history window, newest bit at position 0, matching the convention
    // PhrFoldedHistory.oldestBitToGetFromPhr expects (oldest bits live at the top of the window)
    private val phrWindow     = RegInit(0.U(historyLength.W))
    private val foldedHistReg = RegInit(0.U.asTypeOf(new PhrFoldedHistory(foldInfo, maxUpdateNum)))

    // fixed high slice of the pre-shift window: the bits that would be evicted by a full
    // maxUpdateNum advance. This does not depend on which num is actually used this cycle.
    private val oldestBits = VecInit((0 until maxUpdateNum).map(i => phrWindow(historyLength - 1 - i)))

    // no path-hash contribution to mix in for this test: sized to PathHashHighWidth so the
    // Cat(hashHigh, 0.U(maxUpdateNum.W)) built inside update() is wide enough to bit-extract from
    private val noHashHigh = 0.U(PathHashHighWidth.W)

    // two separate compile-time-Int update() calls, muxed by runtime control,
    // mirroring how a future caller would advance by either shamt or maxUpdateNum
    private val updatedWithShamt  = foldedHistReg.update(oldestBits, shamt, ctrl.newBits(shamt - 1, 0), noHashHigh)
    private val updatedWithMaxNum = foldedHistReg.update(oldestBits, maxUpdateNum, ctrl.newBits, noHashHigh)

    private val advanceNum    = Mux(ctrl.useMaxNum, maxUpdateNum.U, shamt.U)
    private val shamtMask     = ((BigInt(1) << shamt) - 1).U(maxUpdateNum.W)
    private val maxNumMask    = ((BigInt(1) << maxUpdateNum) - 1).U(maxUpdateNum.W)
    private val maskedBits    = ctrl.newBits & Mux(ctrl.useMaxNum, maxNumMask, shamtMask)
    private val nextPhrWindow = ((phrWindow << advanceNum).asUInt | maskedBits)(historyLength - 1, 0)

    when(ctrl.valid) {
      phrWindow                := nextPhrWindow
      foldedHistReg.foldedHist := Mux(ctrl.useMaxNum, updatedWithMaxNum.foldedHist, updatedWithShamt.foldedHist)
    }

    res.incrementalFolded := foldedHistReg.foldedHist
    res.referenceFolded   := computeFoldedHist(phrWindow, foldedLength)(historyLength)
  }

  behavior of "PhrFoldedHistory.update"
  it should "match a from-scratch folded history when num varies between shamt and maxUpdateNum" in {
    implicit val p: Parameters = config
    simulate(new TestModule) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(5)
      dut.reset.poke(false.B)
      dut.clock.step(1)

      // reset state is trivially consistent (both sides fold to zero)
      dut.res.incrementalFolded.expect(dut.res.referenceFolded.peek())

      for (_ <- 0 until 500) {
        val useMaxNum = Random.nextBoolean()
        val newBits   = BigInt(maxUpdateNum, Random)

        dut.ctrl.valid.poke(true.B)
        dut.ctrl.useMaxNum.poke(useMaxNum.B)
        dut.ctrl.newBits.poke(newBits.U(maxUpdateNum.W))
        dut.clock.step(1)

        dut.res.incrementalFolded.expect(dut.res.referenceFolded.peek())
      }
    }
  }
}
