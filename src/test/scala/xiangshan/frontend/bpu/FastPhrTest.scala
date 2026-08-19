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
import xiangshan.frontend.bpu.history.fastphr.FastPhr
import xiangshan.frontend.bpu.history.fastphr.FastPhrModule
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

// Checks that FastPhr's incrementally maintained folded histories stay equivalent to a
// from-scratch fold of the same raw shift-register window, across random token/numBlocksOH sequences.
class FastPhrTest extends AnyFlatSpec {
  private val baseConfig = new MinimalConfig
  private val config: Parameters = baseConfig.alterPartial({
    case XSCoreParamsKey => baseConfig(XSTileKey).head
  })

  private val shamt:        Int = config(XSCoreParamsKey).frontendParameters.bpuParameters.phrParameters.Shamt
  private val pathHashWidth: Int =
    config(XSCoreParamsKey).frontendParameters.bpuParameters.phrParameters.PathHashWidth
  // a group carries one path hash per block, each one Shamt above the next; MaxPredictionNum is 2
  private val tokenWidth: Int = pathHashWidth + shamt

  class TestModule(implicit p: Parameters) extends FastPhrModule with PhrHelper {
    class CtrlIO extends Bundle {
      val valid:       Bool      = Input(Bool())
      val token:       UInt      = Input(UInt(TokenWidth.W))
      val numBlocksOH: Vec[Bool] = Input(Vec(3, Bool()))
      val s2Fire:      Bool      = Input(Bool())

      val redirectValid: Bool = Input(Bool())
      val redirectPhr:   UInt = Input(UInt(WindowLength.W))

      val overrideValid:       Bool      = Input(Bool())
      val overrideToken:       UInt      = Input(UInt(TokenWidth.W))
      val overrideNumBlocksOH: Vec[Bool] = Input(Vec(3, Bool()))
    }
    val ctrl: CtrlIO = IO(new CtrlIO)

    private val foldedWidth: Int = (new PhrAllFoldedHistories(FastFoldedHistoryInfo, MaxUpdateNum)).getWidth

    class ResultIO extends Bundle {
      val dutFolded:       UInt = Output(UInt(foldedWidth.W))
      val referenceFolded: UInt = Output(UInt(foldedWidth.W))
    }
    val res: ResultIO = IO(new ResultIO)

    // an independent from-scratch-fold oracle for an arbitrary externally supplied window, used
    // by the redirect/override tests to check FastPhr's recovered state without relying on any of
    // FastPhr's own incremental fold maintenance (foldStep/refold)
    class ExpectFoldIO extends Bundle {
      val window: UInt = Input(UInt(WindowLength.W))
      val folded: UInt = Output(UInt(foldedWidth.W))
    }
    val expectFold: ExpectFoldIO = IO(new ExpectFoldIO)

    private val fastPhr = Module(new FastPhr)
    fastPhr.io.valid                := ctrl.valid
    fastPhr.io.token                := ctrl.token
    fastPhr.io.numBlocksOH          := ctrl.numBlocksOH
    fastPhr.io.s2Fire               := ctrl.s2Fire
    fastPhr.io.redirect.valid       := ctrl.redirectValid
    fastPhr.io.redirect.phr         := ctrl.redirectPhr
    fastPhr.io.overrideValid        := ctrl.overrideValid
    fastPhr.io.overrideToken        := ctrl.overrideToken
    fastPhr.io.overrideNumBlocksOH  := ctrl.overrideNumBlocksOH

    // mirrors FastPhr's own steady-state shift-register update exactly, so a from-scratch fold of
    // this window can be checked against the DUT's incrementally maintained folded history.
    // this mirror covers steady advance only; the redirect/override tests instead check the
    // recovered state against expectFold, an oracle independent of FastPhr's own recovery logic
    private val referenceWindow = RegInit(0.U(WindowLength.W))
    private val referenceWindowNext = Mux1H(
      ctrl.numBlocksOH,
      Seq(
        referenceWindow,
        ((referenceWindow << Shamt).asUInt ^ ctrl.token(PathHashWidth - 1, 0))(WindowLength - 1, 0),
        ((referenceWindow << (2 * Shamt)).asUInt ^ ctrl.token)(WindowLength - 1, 0)
      )
    )
    when(ctrl.valid) {
      referenceWindow := referenceWindowNext
    }

    private val referenceFolded =
      WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(FastFoldedHistoryInfo, MaxUpdateNum)))
    FastFoldedHistoryInfo.foreach { info =>
      referenceFolded.getHistWithInfo(info).foldedHist :=
        computeFoldedHist(referenceWindow, info.FoldedLength)(info.HistoryLength)
    }

    private val expectFolded =
      WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(FastFoldedHistoryInfo, MaxUpdateNum)))
    FastFoldedHistoryInfo.foreach { info =>
      expectFolded.getHistWithInfo(info).foldedHist :=
        computeFoldedHist(expectFold.window, info.FoldedLength)(info.HistoryLength)
    }

    res.dutFolded       := fastPhr.io.foldedHist.asUInt
    res.referenceFolded := referenceFolded.asUInt
    expectFold.folded   := expectFolded.asUInt
  }

  // drives every recovery/steady control input to its inactive default; every test starts from
  // this so an untouched control line can never leave a port undriven
  private def clearControls(dut: TestModule): Unit = {
    dut.ctrl.valid.poke(false.B)
    for (i <- 0 until 3) dut.ctrl.numBlocksOH(i).poke(false.B)
    dut.ctrl.token.poke(0.U(tokenWidth.W))
    dut.ctrl.s2Fire.poke(false.B)
    dut.ctrl.redirectValid.poke(false.B)
    dut.ctrl.redirectPhr.poke(0.U)
    dut.ctrl.overrideValid.poke(false.B)
    for (i <- 0 until 3) dut.ctrl.overrideNumBlocksOH(i).poke(false.B)
    dut.ctrl.overrideToken.poke(0.U(tokenWidth.W))
  }

  behavior of "FastPhr"
  it should "keep its resident folded histories equivalent to a from-scratch fold of the cached path history window" in {
    implicit val p: Parameters = config
    simulate(new TestModule) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(5)
      dut.reset.poke(false.B)
      dut.clock.step(1)
      clearControls(dut)

      // reset state is trivially consistent (both sides fold to zero)
      dut.res.dutFolded.expect(dut.res.referenceFolded.peek())

      for (_ <- 0 until 500) {
        val numBlocks = Random.nextInt(3)
        val token     = BigInt(tokenWidth, Random)

        dut.ctrl.valid.poke(true.B)
        for (i <- 0 until 3) {
          dut.ctrl.numBlocksOH(i).poke((i == numBlocks).B)
        }
        dut.ctrl.token.poke(token.U(tokenWidth.W))
        dut.clock.step(1)

        dut.res.dutFolded.expect(dut.res.referenceFolded.peek())
      }
    }
  }

  it should "reload from an externally reconstructed window on redirect, overriding any pending steady advance" in {
    implicit val p: Parameters = config
    simulate(new TestModule) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(5)
      dut.reset.poke(false.B)
      dut.clock.step(1)
      clearControls(dut)

      for (_ <- 0 until 200) {
        // a steady advance is requested on every cycle too, so a redirect landing on the same
        // cycle must be seen to win over it
        val numBlocks   = Random.nextInt(3)
        val token       = BigInt(tokenWidth, Random)
        val redirectPhr = BigInt(dut.WindowLength, Random)

        dut.ctrl.valid.poke(true.B)
        for (i <- 0 until 3) dut.ctrl.numBlocksOH(i).poke((i == numBlocks).B)
        dut.ctrl.token.poke(token.U(tokenWidth.W))

        dut.ctrl.redirectValid.poke(true.B)
        dut.ctrl.redirectPhr.poke(redirectPhr.U(dut.WindowLength.W))
        dut.expectFold.window.poke(redirectPhr.U(dut.WindowLength.W))

        dut.clock.step(1)

        dut.res.dutFolded.expect(dut.expectFold.folded.peek())
      }
    }
  }

  it should "restore the snapshot belonging to the group in s3 and replay a corrected group on override" in {
    implicit val p: Parameters = config
    simulate(new TestModule) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(5)
      dut.reset.poke(false.B)
      dut.clock.step(1)
      clearControls(dut)

      // mirrors FastPhr's own 3-way shift exactly, using plain Scala BigInt arithmetic; kept
      // independent of the DUT so the test does not read back any DUT-internal state
      def advance(base: BigInt, numBlocks: Int, token: BigInt): BigInt = {
        val shiftAmt = numBlocks * shamt
        if (shiftAmt == 0) {
          base
        } else {
          // a group of n blocks folds in n path hashes, each one Shamt above the next
          val usedWidth = dut.PathHashWidth + (numBlocks - 1) * shamt
          val tokenMask = (BigInt(1) << usedWidth) - 1
          ((base << shiftAmt) ^ (token & tokenMask)) & ((BigInt(1) << dut.WindowLength) - 1)
        }
      }

      // explicit Scala-side bookkeeping of the cached window's history: window mirrors FastPhr's
      // phr, snapS2 the state captured as a group leaves s1, and snapS3 the state belonging to
      // the group now in s3, which is what an override recovery replays from
      var window: BigInt = 0
      var snapS2: BigInt = 0
      var snapS3: BigInt = 0

      // s2Fire trails valid by one cycle, as a group moves s1 -> s2 -> s3. The random gaps in
      // valid are the point of this test: they make "advances ago" and "position in the pipeline"
      // disagree, so a snapshot that merely counted advances would replay from the wrong group
      var previousValid = false

      for (_ <- 0 until 20) {
        val valid     = Random.nextBoolean()
        val s2Fire    = previousValid
        val numBlocks = if (valid) Random.nextInt(3) else 0
        val token     = BigInt(tokenWidth, Random)

        dut.ctrl.valid.poke(valid.B)
        dut.ctrl.s2Fire.poke(s2Fire.B)
        for (i <- 0 until 3) dut.ctrl.numBlocksOH(i).poke((i == numBlocks).B)
        dut.ctrl.token.poke(token.U(tokenWidth.W))
        dut.clock.step(1)

        // both snapshot stages sample their sources in the same cycle, so capture before mutating
        val previousWindow = window
        val previousSnapS2 = snapS2
        if (valid) {
          snapS2 = previousWindow
          window = advance(previousWindow, numBlocks, token)
        }
        if (s2Fire) snapS3 = previousSnapS2

        previousValid = valid

        dut.res.dutFolded.expect(dut.res.referenceFolded.peek())
      }

      // fire the override with a random corrected group; the expected result folds a
      // from-scratch window built by re-advancing snapS3, independent of FastPhr's own
      // incremental fold maintenance
      val correctedNumBlocks = Random.nextInt(3)
      val correctedToken     = BigInt(tokenWidth, Random)
      val expectedWindow     = advance(snapS3, correctedNumBlocks, correctedToken)

      dut.ctrl.valid.poke(false.B)
      dut.ctrl.s2Fire.poke(false.B)
      dut.ctrl.overrideValid.poke(true.B)
      for (i <- 0 until 3) dut.ctrl.overrideNumBlocksOH(i).poke((i == correctedNumBlocks).B)
      dut.ctrl.overrideToken.poke(correctedToken.U(tokenWidth.W))
      dut.expectFold.window.poke(expectedWindow.U(dut.WindowLength.W))

      dut.clock.step(1)

      dut.res.dutFolded.expect(dut.expectFold.folded.peek())
    }
  }
}
