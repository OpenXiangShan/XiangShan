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

package xiangshan.frontend.ifu

import chisel3._
import chisel3.util._
import math.pow
import org.chipsalliance.cde.config.Parameters
import utility.ParallelOR
import utility.ParallelPriorityEncoder
import xiangshan.ValidUndirectioned
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BranchAttribute

class PredChecker(implicit p: Parameters) extends IfuModule {
  class PredCheckerIO extends IfuBundle {
    class PredCheckerReq(implicit p: Parameters) extends IfuBundle {
      // Input data is offset-adjusted for IBuffer enqueue.
      val instrJumpOffset: Vec[PrunedAddr]    = Vec(IBufferEnqueueWidth, PrunedAddr(VAddrBits))
      val instrValid:      Vec[Bool]          = Vec(IBufferEnqueueWidth, Bool())
      val instrPds:        Vec[PreDecodeInfo] = Vec(IBufferEnqueueWidth, new PreDecodeInfo)
      val instrPc:         Vec[PrunedAddr]    = Vec(IBufferEnqueueWidth, PrunedAddr(VAddrBits))
      val isPredTaken:     Vec[Bool]          = Vec(IBufferEnqueueWidth, Bool())
      val ignore:          Vec[Bool]          = Vec(IfuAlignWidth, Bool())
      val shiftNum:        UInt               = UInt(log2Ceil(IfuAlignWidth).W)

      val firstPredTakenIdx:  Valid[UInt] = Valid(UInt(log2Ceil(IBufferEnqueueWidth).W))
      val secondPredTakenIdx: Valid[UInt] = Valid(UInt(log2Ceil(IBufferEnqueueWidth).W))

      val firstTarget:      PrunedAddr = PrunedAddr(VAddrBits)
      val secondTarget:     PrunedAddr = PrunedAddr(VAddrBits)
      val selectFetchBlock: Vec[Bool]  = Vec(IBufferEnqueueWidth, Bool())
      val invalidTaken:     Vec[Bool]  = Vec(IBufferEnqueueWidth, Bool())
      val instrEndOffset:   Vec[UInt]  = Vec(IBufferEnqueueWidth, UInt(FetchBlockInstOffsetWidth.W))
    }

    class PredCheckerResp(implicit p: Parameters) extends IfuBundle {
      // to Ibuffer write port  (stage 1) ---- Output data is offset-adjusted for IBuffer enqueue.
      class S1Out(implicit p: Parameters) extends IfuBundle {
        val fixedTwoFetchRange: Vec[Bool] = Vec(IBufferEnqueueWidth, Bool())
        val fixedTwoFetchTaken: Vec[Bool] = Vec(IBufferEnqueueWidth, Bool())
      }
      // to Ftq write back port (stage 2) ---- Output data with offset removed for FTQ write-back
      class S2Out(implicit p: Parameters) extends IfuBundle {
        val fixedFirst:    FinalPredCheckResult = new FinalPredCheckResult
        val fixedSecond:   FinalPredCheckResult = new FinalPredCheckResult
        val perfFaultType: Vec[UInt]            = Vec(FetchPorts, PreDecodeFaultType())
      }
      val stage1Out: S1Out = new S1Out
      val stage2Out: S2Out = new S2Out
    }
    val req:  Valid[PredCheckerReq] = Flipped(ValidIO(new PredCheckerReq))
    val resp: PredCheckerResp       = Output(new PredCheckerResp)
  }

  val io: PredCheckerIO = IO(new PredCheckerIO)

  private val (firstTakenIdx, firstPredTaken) =
    (io.req.bits.firstPredTakenIdx.bits, io.req.bits.firstPredTakenIdx.valid)
  private val (secondTakenIdx, secondPredTaken) =
    (io.req.bits.secondPredTakenIdx.bits, io.req.bits.secondPredTakenIdx.valid)

  private val firstPredTarget  = io.req.bits.firstTarget
  private val secondPredTarget = io.req.bits.secondTarget
  private val selectFetchBlock = io.req.bits.selectFetchBlock
  private val invalidTaken     = io.req.bits.invalidTaken
  private val instrEndOffset   = io.req.bits.instrEndOffset
  private val isPredTaken      = io.req.bits.isPredTaken

  // Entries made invalid by offset are marked with 'ignore'
  private val ignore = VecInit((0 until IBufferEnqueueWidth).map { i =>
    if (i < IfuAlignWidth) io.req.bits.ignore(i)
    else false.B
  })

  private val instrValid            = io.req.bits.instrValid
  private val valid                 = io.req.valid
  private val shiftNum              = io.req.bits.shiftNum
  private val (pds, pc, jumpOffset) = (io.req.bits.instrPds, io.req.bits.instrPc, io.req.bits.instrJumpOffset)

  private val jalFaultVec, jalrFaultVec, retFaultVec, notCfiTaken = Wire(Vec(IBufferEnqueueWidth, Bool()))

  /** Remask faults can occur alongside other faults, whereas other faults are mutually exclusive.
    * Therefore, for non-remask faults, a fixed fault mask must be used to ensure that only one fault
    * is detected and redirected to the FTQ.
    * The logic first checks for remask faults, and then applies the fixed range for a secondary check.
    */

  // Stage 1: detect remask fault
  /** first check: remask Fault */
  jalFaultVec := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isJal && instrValid(i) && !isPredTaken(i) && !ignore(i)
  })
  jalrFaultVec := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isJalr && !pd.isRet && instrValid(i) && !isPredTaken(i) && !ignore(i)
  })
  retFaultVec := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isRet && instrValid(i) && !isPredTaken(i) && !ignore(i)
  })
  private val remaskFault =
    VecInit((0 until IBufferEnqueueWidth).map(i =>
      jalFaultVec(i) || jalrFaultVec(i) || retFaultVec(i) || invalidTaken(i)
    ))
  private val remaskIdx  = ParallelPriorityEncoder(remaskFault.asUInt)
  private val needRemask = ParallelOR(remaskFault)
  // NOTE: we need a minimal pow2 that greater than IBufferEnqueueWidth, so we do a log2Up then pow(2)
  private val fixedRange =
    instrValid.asUInt & (
      Fill(IBufferEnqueueWidth, !needRemask) |
        (Fill(pow(2, log2Up(IBufferEnqueueWidth)).toInt, 1.U(1.W)) >> (~remaskIdx).asUInt).asUInt
    )
  // Adjust this if one IBuffer input entry is later removed
  // require(
  //   isPow2(IBufferEnqueueWidth),
  //   "If IBufferEnqueueWidth does not satisfy the power of 2," +
  //     "expression: Fill(IBufferEnqueueWidth, 1.U(1.W)) >> ~remaskIdx is not right !!"
  // )

  io.resp.stage1Out.fixedTwoFetchRange := fixedRange.asTypeOf(Vec(IBufferEnqueueWidth, Bool()))

  private val fixedTwoFetchFirstTaken = VecInit(pds.zipWithIndex.map { case (pd, i) =>
    instrValid(i) && fixedRange(i) && (
      pd.isRet || pd.isJal || pd.isJalr ||
        (isPredTaken(i) && !selectFetchBlock(i) && !pd.notCFI)
    ) && !ignore(i)
  })

  private val fixedTwoFetchSecondTaken = VecInit(pds.zipWithIndex.map { case (pd, i) =>
    instrValid(i) && fixedRange(i) && (
      pd.isRet || pd.isJal || pd.isJalr ||
        (isPredTaken(i) && selectFetchBlock(i) && !pd.notCFI)
    ) && !ignore(i)
  })
  io.resp.stage1Out.fixedTwoFetchTaken := VecInit(fixedTwoFetchFirstTaken.zip(fixedTwoFetchSecondTaken).map {
    case (firstTaken, secondTaken) =>
      firstTaken || secondTaken
  })

  /** second check: faulse prediction fault and target fault */
  notCfiTaken := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    fixedRange(i) && instrValid(i) && pd.notCFI && isPredTaken(i) && !ignore(i)
  })

  private val fixedFirstTakenInstrIdx =
    WireDefault(0.U.asTypeOf(ValidUndirectioned(UInt(FetchBlockInstOffsetWidth.W))))
  private val fixedSecondTakenInstrIdx =
    WireDefault(0.U.asTypeOf(ValidUndirectioned(UInt(FetchBlockInstOffsetWidth.W))))
  fixedFirstTakenInstrIdx.valid := ParallelOR(fixedTwoFetchFirstTaken)
  fixedFirstTakenInstrIdx.bits  := ParallelPriorityEncoder(fixedTwoFetchFirstTaken)

  fixedSecondTakenInstrIdx.valid := ParallelOR(fixedTwoFetchSecondTaken)
  fixedSecondTakenInstrIdx.bits  := ParallelPriorityEncoder(fixedTwoFetchSecondTaken)

  private val mispredInstrIdx = WireDefault(0.U.asTypeOf(ValidUndirectioned(UInt(log2Ceil(IBufferEnqueueWidth).W))))
  private val stage1Fault = VecInit.tabulate(IBufferEnqueueWidth)(i =>
    jalFaultVec(i) || jalrFaultVec(i) || retFaultVec(i) || notCfiTaken(i) || invalidTaken(i)
  )
  mispredInstrIdx.valid := ParallelOR(stage1Fault)
  mispredInstrIdx.bits  := ParallelPriorityEncoder(stage1Fault)

  private val jumpTargets = VecInit(pds.zipWithIndex.map { case (pd, i) =>
    (pc(i) + jumpOffset(i)).asTypeOf(PrunedAddr(VAddrBits))
  })
  private val seqTargets =
    VecInit((0 until IBufferEnqueueWidth).map(i => pc(i) + Mux(pds(i).isRVC || invalidTaken(i), 2.U, 4.U)))

  private val mispredIsJump =
    instrValid(mispredInstrIdx.bits) &&
      mispredInstrIdx.valid &&
      (pds(mispredInstrIdx.bits).isJal || pds(mispredInstrIdx.bits).isBr)

  private val firstFinalIdx   = Mux(mispredInstrIdx.valid, mispredInstrIdx.bits, firstTakenIdx)
  private val firstFinalIsRVC = pds(firstFinalIdx).isRVC
  private val firstAttribute  = WireDefault(0.U.asTypeOf(new BranchAttribute))
  firstAttribute.branchType := pds(firstFinalIdx).brType
  firstAttribute.rasAction  := Cat(pds(firstFinalIdx).isCall, pds(firstFinalIdx).isRet)
  /* *****************************************************************************
   * PredChecker Stage 2
   * ***************************************************************************** */
  private val mispredIdxNext            = RegEnable(mispredInstrIdx, io.req.valid)
  private val mispredIsFirstBlockNext   = RegEnable(!selectFetchBlock(mispredInstrIdx.bits), io.req.valid)
  private val mispredInstrEndOffsetNext = RegEnable(instrEndOffset(mispredInstrIdx.bits), io.req.valid)
  private val mispredIsJumpNext         = RegEnable(mispredIsJump, io.req.valid)

  private val fixedFirstTakenInstrIdxNext  = RegEnable(fixedFirstTakenInstrIdx, io.req.valid)
  private val fixedSecondTakenInstrIdxNext = RegEnable(fixedSecondTakenInstrIdx, io.req.valid)
  private val instrEndOffsetNext           = RegEnable(instrEndOffset, io.req.valid)
  private val firstTakenIdxNext            = RegEnable(firstTakenIdx, io.req.valid)
  private val secondTakenIdxNext           = RegEnable(secondTakenIdx, io.req.valid)
  private val firstPredTargetNext          = RegEnable(firstPredTarget, io.req.valid)
  private val secondPredTargetNext         = RegEnable(secondPredTarget, io.req.valid)

  private val jumpTargetsNext = RegEnable(jumpTargets, io.req.valid)
  private val seqTargetsNext  = RegEnable(seqTargets, io.req.valid)

  private val firstPredTakenNext  = RegEnable(firstPredTaken, io.req.valid)
  private val firstFinalIsRVCNext = RegEnable(firstFinalIsRVC, io.req.valid)
  private val firstAttributeNext  = RegEnable(firstAttribute, io.req.valid)
  private val fixedRangeNext      = RegEnable(fixedRange, io.req.valid)
  // --------- These registers are only for performance debugging purposes ---------------------/
  private val jalFaultVecNext  = RegEnable(jalFaultVec, io.req.valid)
  private val jalrFaultVecNext = RegEnable(jalrFaultVec, io.req.valid)
  private val retFaultVecNext  = RegEnable(retFaultVec, io.req.valid)
  private val notCFITakenNext  = RegEnable(notCfiTaken, io.req.valid)
  private val invalidTakenNext = RegEnable(invalidTaken, io.req.valid)

  private val fixFirstMispred  = mispredIdxNext.valid && mispredIsFirstBlockNext
  private val fixSecondMispred = mispredIdxNext.valid && !mispredIsFirstBlockNext
  private val fixedFirstRawInstrRange =
    Fill(FetchBlockInstNum, !fixFirstMispred) |
      (Fill(FetchBlockInstNum, 1.U(1.W)) >> (~mispredInstrEndOffsetNext(
        FetchBlockInstOffsetWidth - 1,
        0
      )).asUInt).asUInt

  private val fixedSecondRawInstrRange =
    Fill(FetchBlockInstNum, !fixSecondMispred) |
      (Fill(FetchBlockInstNum, 1.U(1.W)) >> (~mispredInstrEndOffsetNext(
        FetchBlockInstOffsetWidth - 1,
        0
      )).asUInt).asUInt

  private val mispredTarget =
    Mux(mispredIsJumpNext, jumpTargetsNext(mispredIdxNext.bits), seqTargetsNext(mispredIdxNext.bits))

  // TODO: Need to rethink this interface
  io.resp.stage2Out.fixedFirst.target       := Mux(fixFirstMispred, mispredTarget, firstPredTargetNext)
  io.resp.stage2Out.fixedFirst.misIdx.valid := fixFirstMispred
  io.resp.stage2Out.fixedFirst.misIdx.bits  := Mux(fixFirstMispred, mispredIdxNext.bits, firstTakenIdxNext)
  io.resp.stage2Out.fixedFirst.cfiIdx.valid := fixedFirstTakenInstrIdxNext.valid
  io.resp.stage2Out.fixedFirst.cfiIdx.bits  := fixedFirstTakenInstrIdxNext.bits
  io.resp.stage2Out.fixedFirst.instrRange   := fixedFirstRawInstrRange
  io.resp.stage2Out.fixedFirst.invalidTaken := fixFirstMispred && invalidTakenNext(mispredIdxNext.bits)
  io.resp.stage2Out.fixedFirst.isRVC        := firstFinalIsRVCNext
  io.resp.stage2Out.fixedFirst.attribute    := firstAttributeNext

  io.resp.stage2Out.fixedSecond.target       := Mux(fixSecondMispred, mispredTarget, secondPredTargetNext)
  io.resp.stage2Out.fixedSecond.misIdx.valid := fixSecondMispred
  io.resp.stage2Out.fixedSecond.misIdx.bits  := Mux(fixSecondMispred, mispredIdxNext.bits, secondTakenIdxNext)
  io.resp.stage2Out.fixedSecond.cfiIdx.valid := fixedSecondTakenInstrIdxNext.valid
  io.resp.stage2Out.fixedSecond.cfiIdx.bits  := fixedSecondTakenInstrIdxNext.bits
  io.resp.stage2Out.fixedSecond.instrRange   := fixedSecondRawInstrRange
  // FIXME: second fetch block invalid taken
  io.resp.stage2Out.fixedSecond.invalidTaken := false.B
  io.resp.stage2Out.fixedSecond.isRVC        := false.B
  io.resp.stage2Out.fixedSecond.attribute    := DontCare

  private val faultType = MuxCase(
    PreDecodeFaultType.NoFault,
    Seq(
      jalFaultVecNext(mispredIdxNext.bits)  -> PreDecodeFaultType.JalFault,
      jalrFaultVecNext(mispredIdxNext.bits) -> PreDecodeFaultType.JalrFault,
      retFaultVecNext(mispredIdxNext.bits)  -> PreDecodeFaultType.RetFault,
      notCFITakenNext(mispredIdxNext.bits)  -> PreDecodeFaultType.NotCfiFault,
      invalidTakenNext(mispredIdxNext.bits) -> PreDecodeFaultType.InvalidTaken
    )
  )

  io.resp.stage2Out.perfFaultType(0) := Mux(fixFirstMispred, faultType, PreDecodeFaultType.NoFault)
  io.resp.stage2Out.perfFaultType(1) := Mux(fixSecondMispred, faultType, PreDecodeFaultType.NoFault)
}
