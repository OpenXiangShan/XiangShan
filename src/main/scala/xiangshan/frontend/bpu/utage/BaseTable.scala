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

package xiangshan.frontend.bpu.utage

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SeqToAugmentedSeq
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility.UIntToMask
import utility.XSPerfAccumulate
import xiangshan.backend.datapath.DataConfig.VAddrBits
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BranchInfo
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories

class MicroBaseTable(
    val numSets: Int
)(implicit p: Parameters) extends MicroTageModule with Helpers {
  class MicroBaseTableIO extends MicroTageBundle {
    class MicroBaseReq extends Bundle {
      val startPc: PrunedAddr = new PrunedAddr(VAddrBits)
    }
    class MicroBaseResp extends Bundle {
      val taken:       Bool = Bool()
      val cfiPosition: UInt = UInt(CfiPositionWidth.W)
    }
    class MicroBaseUpdate extends Bundle {
      val startPc:  PrunedAddr             = new PrunedAddr(VAddrBits)
      val branches: Vec[Valid[BranchInfo]] = Vec(ResolveEntryBranchNumber, Valid(new BranchInfo))
    }
    val req:    MicroBaseReq           = Input(new MicroBaseReq)
    val resp:   MicroBaseResp          = Output(new MicroBaseResp)
    val update: Valid[MicroBaseUpdate] = Input(Valid(new MicroBaseUpdate))
  }
  class MicroBaseEntry() extends MicroTageBundle {
    val valid:       Bool            = Bool()
    val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
    val counter:     SaturateCounter = new SaturateCounter(2)
  }
  val io = IO(new MicroBaseTableIO)
  private val oddEntries =
    RegInit(VecInit(Seq.fill(numSets / 2)(0.U.asTypeOf(Vec(FetchBlockAlignInstNum, new MicroBaseEntry)))))
  private val evenEntries =
    RegInit(VecInit(Seq.fill(numSets / 2)(0.U.asTypeOf(Vec(FetchBlockAlignInstNum, new MicroBaseEntry)))))
  private val readNeedRotate = io.req.startPc.toUInt(FetchBlockSizeWidth - 1)
  private val readIdx     = io.req.startPc.toUInt(log2Ceil(numSets / 2) + FetchBlockSizeWidth - 1, FetchBlockSizeWidth)
  private val readOddIdx  = Mux(readNeedRotate, readIdx + 1.U, readIdx)
  private val readEvenIdx = readIdx
  private val alignTakenMask =
    UIntToMask(io.req.startPc.toUInt(FetchBlockAlignWidth - 1, instOffsetBits), FetchBlockAlignInstNum)
  private val takenMask        = Cat(0.U(FetchBlockAlignInstNum.W), alignTakenMask)
  private val readOddEntryVec  = oddEntries(readOddIdx)
  private val readEvenEntryVec = evenEntries(readEvenIdx)
  private val readEntryVec =
    Mux(readNeedRotate, VecInit(readEvenEntryVec ++ readOddEntryVec), VecInit(readOddEntryVec ++ readEvenEntryVec))
  private val takenVec     = readEntryVec.map(entry => entry.valid && entry.counter.isPositive).asUInt
  private val realTakenVec = (~takenMask) & takenVec
  private val readEntry    = Mux1H(PriorityEncoderOH(realTakenVec), readEntryVec)
  private val hitTaken     = realTakenVec.orR

  private val testRealTakenVec = RegInit(0.U(32.W))
  private val testRealReadIdx  = RegInit(0.U(log2Ceil(numSets).W))
  private val testReadEntryVec = RegInit(0.U.asTypeOf(Vec(32, new MicroBaseEntry)))
  testRealTakenVec := realTakenVec
  testRealReadIdx  := readIdx
  testReadEntryVec := readEntryVec
  dontTouch(testRealTakenVec)
  dontTouch(testRealReadIdx)
  dontTouch(testReadEntryVec)

  io.resp.taken := hitTaken
  io.resp.cfiPosition := Cat(
    readEntry.cfiPosition(CfiPositionWidth - 1) ^ readNeedRotate,
    readEntry.cfiPosition(CfiPositionWidth - 2, 0)
  )

  // update
  private val trainIdx =
    io.update.bits.startPc.toUInt(log2Ceil(numSets / 2) + FetchBlockSizeWidth - 1, FetchBlockSizeWidth)
  private val trainNeedRotate = io.update.bits.startPc.toUInt(FetchBlockSizeWidth - 1)
  private val trainOddIdx     = Mux(trainNeedRotate, trainIdx + 1.U, trainIdx)
  private val trainEvenIdx    = trainIdx
  private val oddOldCounters  = oddEntries(trainOddIdx).map(_.counter)
  private val evenOldCounters = evenEntries(trainEvenIdx).map(_.counter)
  // private val oldCounters = oddOldCounters ++ evenOldCounters
  private val oldCounters =
    Mux(trainNeedRotate, VecInit(evenOldCounters ++ oddOldCounters), VecInit(oddOldCounters ++ evenOldCounters))
  private val t1_branches       = io.update.bits.branches
  private val t1_needTrainValid = Wire(Vec(FetchBlockInstNum, Bool()))
  private val t1_newEntry       = Wire(Vec(FetchBlockInstNum, new MicroBaseEntry))
  t1_needTrainValid.zip(t1_newEntry).zipWithIndex.foreach { case ((needUpdate, newEntry), position) =>
    val hitMask = t1_branches.map { branch =>
      branch.valid && branch.bits.attribute.isConditional && position.U === branch.bits.cfiPosition
    }
    val taken = Mux1H(hitMask, t1_branches.map(_.bits.taken))
    needUpdate             := hitMask.reduce(_ || _)
    newEntry.valid         := true.B
    newEntry.cfiPosition   := position.U
    newEntry.counter.value := oldCounters(position).getUpdate(taken)
  }
  for (i <- 0 until FetchBlockAlignInstNum) {
    when(((t1_needTrainValid(i) && !trainNeedRotate) || (t1_needTrainValid(
      FetchBlockAlignInstNum + i
    ) && trainNeedRotate)) && io.update.valid) {
      oddEntries(trainOddIdx)(i) := Mux(trainNeedRotate, t1_newEntry(i + FetchBlockAlignInstNum), t1_newEntry(i))
      oddEntries(trainOddIdx)(i).cfiPosition := i.U
    }
    when(((t1_needTrainValid(FetchBlockAlignInstNum + i) && !trainNeedRotate) || (t1_needTrainValid(
      i
    ) && trainNeedRotate)) && io.update.valid) {
      // evenEntries(trainEvenIdx)(i) := t1_newEntry(i + FetchBlockAlignInstNum)
      evenEntries(trainEvenIdx)(i) := Mux(trainNeedRotate, t1_newEntry(i), t1_newEntry(i + FetchBlockAlignInstNum))
      evenEntries(trainEvenIdx)(i).cfiPosition := (i + FetchBlockAlignInstNum).U
    }
  }

  private val testTrainOddIdx     = RegInit(0.U(log2Ceil(numSets).W))
  private val testTrainEvenIdx    = RegInit(0.U(log2Ceil(numSets).W))
  private val testNewEntry        = RegInit(0.U.asTypeOf(Vec(FetchBlockInstNum, new MicroBaseEntry)))
  private val testTrainNeedRotate = RegInit(false.B)

  testTrainOddIdx     := trainOddIdx
  testTrainEvenIdx    := trainEvenIdx
  testNewEntry        := t1_newEntry
  testTrainNeedRotate := trainNeedRotate

  dontTouch(testTrainOddIdx)
  dontTouch(testTrainEvenIdx)
  dontTouch(testNewEntry)
  dontTouch(testTrainNeedRotate)
}
