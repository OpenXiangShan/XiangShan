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
import utility.LogUtilsOptions
import utility.LogUtilsOptionsKey
import utility.PerfCounterOptions
import utility.PerfCounterOptionsKey
import utility.XSPerfLevel
import xiangshan.DebugOptionsKey
import xiangshan.XSCoreParamsKey
import xiangshan.XSTileKey
import xiangshan.frontend.PcInit
import xiangshan.frontend.bpu.history.phr.Helpers
import xiangshan.frontend.bpu.history.phr.Phr
import xiangshan.frontend.bpu.history.phr.PhrModule

class PhrGroupTest extends AnyFlatSpec {
  private val baseConfig = new MinimalConfig
  private val config: Parameters = baseConfig.alterPartial({
    case XSCoreParamsKey =>
      val core = baseConfig(XSTileKey).head
      core.copy(frontendParameters = core.frontendParameters.copy(MaxPredictionNum = 2))
  }).alter((site, here, up) => {
    // Phr logs and counts, which top-level elaboration otherwise configures
    case LogUtilsOptionsKey => LogUtilsOptions(
        here(DebugOptionsKey).EnableDebug,
        here(DebugOptionsKey).EnablePerfDebug,
        here(DebugOptionsKey).FPGAPlatform,
        here(DebugOptionsKey).EnableXMR
      )
    case PerfCounterOptionsKey => PerfCounterOptions(
        enablePerfPrint = false,
        enablePerfDB = false,
        perfLevel = XSPerfLevel.withName(here(DebugOptionsKey).PerfLevel),
        perfDBHartID = 0
      )
  })

  // Wraps Phr with plain ports: s1 groups whose blocks all have their branch at position 0 of an aligned start pc, so
  // a block's cfi pc is its start pc, and a not-taken redirect that restores a recorded meta.
  class TestModule(implicit p: Parameters) extends PhrModule with Helpers {
    class InIO extends Bundle {
      val valid:       Bool      = Bool()
      val startPc:     UInt      = UInt(VAddrBits.W)
      val blockValid:  Vec[Bool] = Vec(MaxPredictionNum, Bool())
      val blockTaken:  Vec[Bool] = Vec(MaxPredictionNum, Bool())
      val blockTarget: Vec[UInt] = Vec(MaxPredictionNum, UInt(VAddrBits.W))

      val redirect:         Bool = Bool()
      val redirectPtrFlag:  Bool = Bool()
      val redirectPtrValue: UInt = UInt(log2Ceil(PhrHistoryLength).W)
      val redirectLowBits:  UInt = UInt(PathHashHighWidth.W)

      // the history the Scala model expects once this cycle's update is applied, newest bit at position 0
      val refWindow: UInt = UInt(PhrHistoryLength.W)
    }
    class OutIO extends Bundle {
      val foldedMatch:  Bool = Bool()
      val laterPtrFlag: Bool = Bool()
      val laterPtrValue: UInt = UInt(log2Ceil(PhrHistoryLength).W)
      val laterLowBits: UInt = UInt(PathHashHighWidth.W)
    }
    val in:  InIO  = IO(Input(new InIO))
    val out: OutIO = IO(Output(new OutIO))

    private val phr = Module(new Phr)

    phr.io.train := 0.U.asTypeOf(phr.io.train)
    phr.io.train.stageCtrl.s0_fire := true.B
    phr.io.train.stageCtrl.s1_fire := true.B
    phr.io.train.stageCtrl.s2_fire := true.B
    phr.io.train.stageCtrl.s3_fire := true.B

    phr.io.train.redirect.valid                       := in.redirect
    phr.io.train.redirect.bits.taken                  := false.B
    phr.io.train.redirect.bits.meta.phr.phrPtr.flag   := in.redirectPtrFlag
    phr.io.train.redirect.bits.meta.phr.phrPtr.value  := in.redirectPtrValue
    phr.io.train.redirect.bits.meta.phr.phrLowBits    := in.redirectLowBits

    phr.io.s1Train.valid   := in.valid
    phr.io.s1Train.startPc := PcInit(in.startPc)
    phr.io.s1Train.blocks.zipWithIndex.foreach { case (block, i) =>
      block.valid            := in.blockValid(i)
      block.bits             := 0.U.asTypeOf(block.bits)
      block.bits.taken       := in.blockTaken(i)
      block.bits.cfiPosition := 0.U
      block.bits.target      := PcInit(in.blockTarget(i)).zeroGuard
    }

    phr.io.commit := 0.U.asTypeOf(phr.io.commit)

    out.foldedMatch   := phr.io.s0_foldedPhr.asUInt === computeAllFoldedPhr(in.refWindow).asUInt
    out.laterPtrFlag  := phr.io.s1_laterPhrMeta.head.phrPtr.flag
    out.laterPtrValue := phr.io.s1_laterPhrMeta.head.phrPtr.value
    out.laterLowBits  := phr.io.s1_laterPhrMeta.head.phrLowBits
  }

  behavior of "Phr"
  it should "advance by every taken block of a group and recover to where a later block started" in {
    implicit val p: Parameters = config
    val phrParams   = config(XSCoreParamsKey).frontendParameters.bpuParameters.phrParameters
    val shamt       = phrParams.Shamt
    val pathHashW   = phrParams.PathHashWidth
    val historyLen  = config(XSCoreParamsKey).frontendParameters.getPhrHistoryLength
    val windowMask  = (BigInt(1) << historyLen) - 1
    val vaddrBits   = 39
    require(pathHashW == 15, "the model below mirrors Phr's path hash, which is defined for a 15-bit hash")

    // Phr's path hash: pc[9:1] placed at bit 4, XOR target[16:2]
    def pathHash(pc: BigInt, target: BigInt): BigInt =
      ((((pc >> 1) & 0x1ff) << 4) ^ ((target >> 2) & 0x7fff)) & 0x7fff
    def advance(window: BigInt, pc: BigInt, target: BigInt): BigInt =
      ((window << shamt) ^ pathHash(pc, target)) & windowMask
    // aligned so that a branch at position 0 sits at the start pc
    def randomPc(): BigInt = BigInt(vaddrBits - 8, Random) << 7

    simulate(new TestModule) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(5)
      dut.reset.poke(false.B)

      var window = BigInt(0)
      // a block 1 meta and the history it should rebuild, recorded when a group carried a second block
      var recorded: Option[(Boolean, BigInt, BigInt, BigInt)] = None
      var sinceRecorded = 0
      var redirects     = 0

      for (_ <- 0 until 2000) {
        val doRedirect = recorded.isDefined && sinceRecorded >= 3 && Random.nextInt(4) == 0
        dut.in.redirect.poke(doRedirect.B)
        dut.in.valid.poke((!doRedirect).B)

        if (doRedirect) {
          val (flag, value, lowBits, restored) = recorded.get
          dut.in.redirectPtrFlag.poke(flag.B)
          dut.in.redirectPtrValue.poke(value.U)
          dut.in.redirectLowBits.poke(lowBits.U)
          window = restored
          recorded = None
          redirects += 1
        } else {
          val startPc = randomPc()
          val target0 = randomPc()
          val target1 = randomPc()
          val taken0  = Random.nextBoolean()
          val valid1  = taken0 && Random.nextBoolean()
          val taken1  = valid1 && Random.nextBoolean()
          dut.in.startPc.poke(startPc.U)
          dut.in.blockValid(0).poke(true.B)
          dut.in.blockTaken(0).poke(taken0.B)
          dut.in.blockTarget(0).poke(target0.U)
          dut.in.blockValid(1).poke(valid1.B)
          dut.in.blockTaken(1).poke(taken1.B)
          dut.in.blockTarget(1).poke(target1.U)

          val afterBlock0 = if (taken0) advance(window, startPc, target0) else window
          if (valid1) {
            recorded = Some(
              (
                dut.out.laterPtrFlag.peek().litToBoolean,
                dut.out.laterPtrValue.peek().litValue,
                dut.out.laterLowBits.peek().litValue,
                afterBlock0
              )
            )
            sinceRecorded = 0
          }
          window = if (taken1) advance(afterBlock0, target0, target1) else afterBlock0
          sinceRecorded += 1
        }

        dut.in.refWindow.poke(window.U(historyLen.W))
        dut.out.foldedMatch.expect(true.B)
        dut.clock.step(1)
      }
      assert(redirects > 50, s"too few redirects exercised: $redirects")
    }
  }
}
