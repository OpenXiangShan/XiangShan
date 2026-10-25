/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] James E. Smith, and Andrew R. Pleszkun. "[Implementation of precise interrupts in pipelined processors.]
* (https://dl.acm.org/doi/10.5555/327010.327125)" 12th Annual International Symposium on Computer Architecture (ISCA).
* 1985.
***************************************************************************************/

package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import difftest.common._
import difftest.plugin.topdown._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils._
import xiangshan._
import xiangshan.PerfDebugInfo
import xiangshan.backend.{BackendParams, Bundles, GPAMemEntry, RatToVecExcpMod, RegWriteFromRab, VecExcpInfo}
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.FuType
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfo, LsTopdownInfo}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.{SnapshotGenerator, CompressType, NoCompressReason}
import xiangshan.backend.rob.RobBundles._
import xiangshan.backend.trace._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.decode.TruthTable
import xiangshan.TopDownCounters._
import xiangshan.backend.dispatch._

final class RobEntryState(implicit val p: Parameters) extends HasXSParameter {
  private def saturatingDecPacked(value: UInt, decRaw: UInt): UInt = {
    val compareWidth = math.max(value.getWidth, decRaw.getWidth)
    val valueExt = value.pad(compareWidth)
    val decExt = decRaw.pad(compareWidth)
    val clippedDec = Mux(decExt > valueExt, valueExt, decExt)
    (valueExt - clippedDec)(value.getWidth - 1, 0)
  }

  /**
   * [ROB-45]
   * Combinational completion transition shared by stored entries and read bypass.
   * NORMAL uses a wide former count; compressed entries pack two smaller counts.
   * A latter squash converts the surviving former count back to NORMAL encoding.
   * Former exceptional completion clears both packed counts; latter exceptional
   * completion clears only latter. Allocation initializes only an invalid entry.
   * This function adds no registers; callers establish the cycle boundary.
   */
  def calcNextPackedUopState(
    entryValid: Bool,
    entryUopState: UInt,
    entryCompressType: UInt,
    redirectClearsLatter: Bool,
    needFlushWriteBackFormer: Bool,
    needFlushWriteBackLater: Bool,
    instCanEnqFlag: Bool,
    enqEntryPairType: UInt,
    enqFormerWBNum: UInt,
    enqLatterWBNum: UInt,
    formerWbCntRaw: UInt,
    latterWbCntRaw: UInt
  ): UInt = {
    val normalFormerState = entryUopState(NormalUopNumWidth - 1, 0)
    val compressedFormerState = entryUopState(CompressedSlotUopNumWidth - 1, 0)
    val compressedLatterState = entryUopState(
      2 * CompressedSlotUopNumWidth - 1,
      CompressedSlotUopNumWidth
    )
    val currentIsNormal = CompressType.isNORMAL(entryCompressType)
    val currentFormerForNormal = Mux(
      currentIsNormal,
      normalFormerState,
      compressedFormerState.pad(NormalUopNumWidth)
    )

    val normalFormerAfterWb = saturatingDecPacked(currentFormerForNormal, formerWbCntRaw)
    val compressedFormerAfterWb = saturatingDecPacked(compressedFormerState, formerWbCntRaw)
    val compressedLatterAfterWb = saturatingDecPacked(compressedLatterState, latterWbCntRaw)

    val normalFormerNext = Mux(needFlushWriteBackFormer, 0.U, normalFormerAfterWb)
    val compressedFormerNext = Mux(needFlushWriteBackFormer, 0.U, compressedFormerAfterWb)
    val compressedLatterNext = Mux(
      needFlushWriteBackLater || redirectClearsLatter,
      0.U,
      compressedLatterAfterWb
    )

    val nextEntryPairType = Mux(
      redirectClearsLatter,
      CompressType.NORMAL,
      entryCompressType
    )
    val nextNormalState = Cat(0.U(1.W), normalFormerNext)
    val nextCompressedState = Cat(compressedLatterNext, compressedFormerNext)
    val stateAfterWriteback = Mux(
      CompressType.isNORMAL(nextEntryPairType),
      nextNormalState,
      nextCompressedState
    )
    val currentCanonicalState = Mux(
      currentIsNormal,
      Cat(0.U(1.W), normalFormerState),
      Cat(compressedLatterState, compressedFormerState)
    )
    val clearBothByFormerExceptionFlush =
      needFlushWriteBackFormer && CompressType.isNotNORMAL(entryCompressType)
    val enqState = encodeUopState(enqEntryPairType, enqFormerWBNum, enqLatterWBNum)

    val nextState = Wire(UInt(entryUopState.getWidth.W))
    nextState := currentCanonicalState
    when(!entryValid && instCanEnqFlag) {
      nextState := enqState
    }.elsewhen(entryValid) {
      nextState := Mux(
        clearBothByFormerExceptionFlush,
        0.U(entryUopState.getWidth.W),
        stateAfterWriteback
      )
    }
    nextState
  }

}
