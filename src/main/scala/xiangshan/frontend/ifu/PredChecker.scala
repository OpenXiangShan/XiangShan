// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import org.chipsalliance.cde.config.Parameters
import utility.ParallelOR
import utility.ParallelPosteriorityEncoder
import utility.ParallelPriorityEncoder
import utility.XSError
import xiangshan.ValidUndirectioned
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr

class PredChecker(implicit p: Parameters) extends IfuModule {
  class PredCheckerIO extends IfuBundle {
    class PredCheckerReq(implicit p: Parameters) extends IfuBundle {
      val instrJumpOffset:      Vec[PrunedAddr]    = Vec(PredictWidth, PrunedAddr(VAddrBits))
      val instrValid:           Vec[Bool]          = Vec(PredictWidth, Bool())
      val instrPds:             Vec[PreDecodeInfo] = Vec(PredictWidth, new PreDecodeInfo)
      val instrPc:              Vec[PrunedAddr]    = Vec(PredictWidth, PrunedAddr(VAddrBits))
      val tempPreLastIsHalfRvi: Bool               = Bool()

      val firstFtqPreTakenIdx:  Valid[UInt] = Valid(UInt(log2Ceil(PredictWidth).W))
      val secondFtqPreTakenIdx: Valid[UInt] = Valid(UInt(log2Ceil(PredictWidth).W))

      val firstTarget:      PrunedAddr = PrunedAddr(VAddrBits)
      val secondTarget:     PrunedAddr = PrunedAddr(VAddrBits)
      val selectFetchBlock: Vec[Bool]  = Vec(PredictWidth, Bool())
      val invalidTaken:     Vec[Bool]  = Vec(PredictWidth, Bool())
      val instrOffset:      Vec[UInt]  = Vec(PredictWidth, UInt(log2Ceil(PredictWidth).W))
    }

    class PredCheckerResp(implicit p: Parameters) extends IfuBundle {
      // to Ibuffer write port  (stage 1)
      class S1Out(implicit p: Parameters) extends IfuBundle {
        val fixedTwoFetchRange: Vec[Bool] = Vec(PredictWidth, Bool())
        val fixedTwoFetchTaken: Vec[Bool] = Vec(PredictWidth, Bool())
      }
      // to Ftq write back port (stage 2)
      class S2Out(implicit p: Parameters) extends IfuBundle {
        val fixedTwoFetchTarget: Vec[PrunedAddr] = Vec(PredictWidth, PrunedAddr(VAddrBits))
        val twoFetchJalTarget:   Vec[PrunedAddr] = Vec(PredictWidth, PrunedAddr(VAddrBits))
        val twoFetchFaultType:   Vec[UInt]       = Vec(PredictWidth, PreDecodeFaultType())

        val fixedFirstMissPredIdx:       Valid[UInt] = Valid(UInt(log2Ceil(PredictWidth).W))
        val fixedSecondMissPredIdx:      Valid[UInt] = Valid(UInt(log2Ceil(PredictWidth).W))
        val fixedFirstBubbleInstrRange:  UInt        = UInt(PredictWidth.W)
        val fixedSecondBubbleInstrRange: UInt        = UInt(PredictWidth.W)

        val fixedFirstTakenIdx:  Valid[UInt] = Valid(UInt(log2Ceil(PredictWidth).W))
        val fixedSecondTakenIdx: Valid[UInt] = Valid(UInt(log2Ceil(PredictWidth).W))
      }
      val stage1Out: S1Out = new S1Out
      val stage2Out: S2Out = new S2Out
    }

    val req:  Valid[PredCheckerReq] = Flipped(ValidIO(new PredCheckerReq))
    val resp: PredCheckerResp       = Output(new PredCheckerResp)
  }

  val io: PredCheckerIO = IO(new PredCheckerIO)

  private val (firstTakenIdx, firstPredTaken) =
    (io.req.bits.firstFtqPreTakenIdx.bits, io.req.bits.firstFtqPreTakenIdx.valid)
  private val (secondTakenIdx, secondPredTaken) =
    (io.req.bits.secondFtqPreTakenIdx.bits, io.req.bits.secondFtqPreTakenIdx.valid)
  private val firstPredTarget  = io.req.bits.firstTarget
  private val secondPredTarget = io.req.bits.secondTarget
  private val selectFetchBlock = io.req.bits.selectFetchBlock
  private val invalidTaken     = io.req.bits.invalidTaken
  private val instrOffset      = io.req.bits.instrOffset

  private val tempIgnore = WireDefault(VecInit.fill(PredictWidth)(false.B))
  tempIgnore(0) := io.req.bits.tempPreLastIsHalfRvi

  private val instrValid            = io.req.bits.instrValid
  private val valid                 = io.req.valid
  private val (pds, pc, jumpOffset) = (io.req.bits.instrPds, io.req.bits.instrPc, io.req.bits.instrJumpOffset)

  private val jalFaultVec, jalrFaultVec, retFaultVec, targetFault, notCfiTaken =
    Wire(Vec(PredictWidth, Bool()))

  /** remask fault may appear together with other faults, but other faults are exclusive
   * so other f ault mast use fixed mask to keep only one fault would be found and redirect to Ftq
   * we first detecct remask fault and then use fixedRange to do second check
   **/

  // Stage 1: detect remask fault
  /** first check: remask Fault */
  jalFaultVec := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isJal && instrValid(i) && (((firstTakenIdx > i.U && firstPredTaken || !firstPredTaken) && !selectFetchBlock(
      i
    )) ||
      ((secondTakenIdx > i.U && secondPredTaken || !secondPredTaken) && selectFetchBlock(i))) && !tempIgnore(i)
  })
  jalrFaultVec := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isJalr && !pd.isRet && instrValid(
      i
    ) && (((firstTakenIdx > i.U && firstPredTaken || !firstPredTaken) && !selectFetchBlock(i)) ||
      ((secondTakenIdx > i.U && secondPredTaken || !secondPredTaken) && selectFetchBlock(i))) && !tempIgnore(i)
  })
  retFaultVec := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isRet && instrValid(i) && (((firstTakenIdx > i.U && firstPredTaken || !firstPredTaken) && !selectFetchBlock(
      i
    )) ||
      ((secondTakenIdx > i.U && secondPredTaken || !secondPredTaken) && selectFetchBlock(i))) && !tempIgnore(i)
  })
  private val remaskFault =
    VecInit((0 until PredictWidth).map(i => jalFaultVec(i) || jalrFaultVec(i) || retFaultVec(i) || invalidTaken(i)))
  private val remaskIdx  = ParallelPriorityEncoder(remaskFault.asUInt)
  private val needRemask = ParallelOR(remaskFault)
  private val fixedRange =
    instrValid.asUInt & (Fill(PredictWidth, !needRemask) | Fill(PredictWidth, 1.U(1.W)) >> ~remaskIdx)

  require(
    isPow2(PredictWidth),
    "If PredictWidth does not statisfy the power of 2," +
      "expression: Fill(PredictWidth, 1.U(1.W)) >> ~remaskIdx is not right !!"
  )

  io.resp.stage1Out.fixedTwoFetchRange := fixedRange.asTypeOf(Vec(PredictWidth, Bool()))

  val fixedFirstTaken = VecInit(pds.zipWithIndex.map { case (pd, i) =>
    instrValid(i) && fixedRange(
      i
    ) && (pd.isRet || pd.isJal || pd.isJalr || (firstTakenIdx === i.U && firstPredTaken && !pd.notCFI)) && !selectFetchBlock(
      i
    ) && !tempIgnore(i)
  })
  val fixedSecondTaken = VecInit(pds.zipWithIndex.map { case (pd, i) =>
    instrValid(i) && fixedRange(
      i
    ) && (pd.isRet || pd.isJal || pd.isJalr || (secondTakenIdx === i.U && secondPredTaken && !pd.notCFI)) && selectFetchBlock(
      i
    ) && !tempIgnore(i)
  })
  io.resp.stage1Out.fixedTwoFetchTaken := VecInit(fixedFirstTaken.zip(fixedSecondTaken).map {
    case (firstTaken, secondTaken) =>
      firstTaken || secondTaken
  })

  private val fixedFirstTakenInstrIdx  = WireDefault(0.U.asTypeOf(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  private val fixedSecondTakenInstrIdx = WireDefault(0.U.asTypeOf(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  fixedFirstTakenInstrIdx.valid := ParallelOR(fixedFirstTaken)
  fixedFirstTakenInstrIdx.bits  := ParallelPriorityEncoder(fixedFirstTaken)

  fixedSecondTakenInstrIdx.valid := ParallelOR(fixedSecondTaken)
  fixedSecondTakenInstrIdx.bits  := ParallelPriorityEncoder(fixedSecondTaken)

  /** second check: faulse prediction fault and target fault */
  notCfiTaken := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    fixedRange(i) && instrValid(i) && pd.notCFI && ((i.U === firstTakenIdx && firstPredTaken && !selectFetchBlock(i)) ||
      (i.U === secondTakenIdx && secondPredTaken && selectFetchBlock(i))) && !tempIgnore(i)
  })

  XSError(
    valid && instrValid(0) && (remaskFault(0) || (firstPredTaken && (firstTakenIdx === 0.U))) && tempIgnore(0),
    "Half instruction cases exceeded expectations"
  )

  private val jumpTargets = VecInit(pds.zipWithIndex.map { case (pd, i) =>
    (pc(i) + jumpOffset(i)).asTypeOf(PrunedAddr(VAddrBits))
  })
  private val seqTargets =
    VecInit((0 until PredictWidth).map(i => pc(i) + Mux(pds(i).isRVC, 2.U, 4.U)))

  // Stage 2: detect target fault
  /** target calculation: in the next stage  */
  private val fixedRangeNext       = RegEnable(fixedRange, io.req.valid)
  private val instrValidNext       = RegEnable(instrValid, io.req.valid)
  private val firstPredTakenNext   = RegEnable(firstPredTaken, io.req.valid)
  private val secondPredTakenNext  = RegEnable(secondPredTaken, io.req.valid)
  private val firstTakenIdxNext    = RegEnable(firstTakenIdx, io.req.valid)
  private val secondTakenIdxNext   = RegEnable(secondTakenIdx, io.req.valid)
  private val firstPredTargetNext  = RegEnable(firstPredTarget, io.req.valid)
  private val secondPredTargetNext = RegEnable(secondPredTarget, io.req.valid)

  private val jumpTargetsNext  = RegEnable(jumpTargets, io.req.valid)
  private val seqTargetsNext   = RegEnable(seqTargets, io.req.valid)
  private val pdsNext          = RegEnable(pds, io.req.valid)
  private val jalFaultVecNext  = RegEnable(jalFaultVec, io.req.valid)
  private val jalrFaultVecNext = RegEnable(jalrFaultVec, io.req.valid)
  private val retFaultVecNext  = RegEnable(retFaultVec, io.req.valid)
  private val notCFITakenNext  = RegEnable(notCfiTaken, io.req.valid)
  private val invalidTakenNext = RegEnable(invalidTaken, io.req.valid)
  private val instrOffsetNext  = RegEnable(instrOffset, io.req.valid)
//  private val firstBubbleRangeNext      = RegEnable(firstBubbleRange, io.req.valid)
//  private val secondBubbleRangeNext     = RegEnable(secondBubbleRange, io.req.valid)
  private val fixedFirstTakenInstrIdxNext  = RegEnable(fixedFirstTakenInstrIdx, io.req.valid)
  private val fixedSecondTakenInstrIdxNext = RegEnable(fixedSecondTakenInstrIdx, io.req.valid)

  targetFault := VecInit(pdsNext.zipWithIndex.map { case (pd, i) =>
    fixedRangeNext(i) && instrValidNext(
      i
    ) && (pd.isJal || pd.isBr) && ((firstTakenIdxNext === i.U && firstPredTakenNext && (firstPredTargetNext =/= jumpTargetsNext(
      i
    ))) ||
      (secondTakenIdxNext === i.U && secondPredTakenNext && (secondPredTargetNext =/= jumpTargetsNext(i))))
  })

  // private val misPreOffset  = WireDefault(0.U.asTypeOf(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  private val misPreInstrIdx = WireDefault(0.U.asTypeOf(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  private val stage1Fault = VecInit.tabulate(PredictWidth)(i =>
    jalFaultVec(i) || jalrFaultVec(i) || retFaultVec(i) || notCfiTaken(i) || invalidTaken(i)
  )
  misPreInstrIdx.valid := ParallelOR(stage1Fault)
  misPreInstrIdx.bits  := ParallelPriorityEncoder(stage1Fault)

  private val misPreIdxNext       = RegEnable(misPreInstrIdx, io.req.valid)
  private val misIsFirstBlockNext = RegEnable(!selectFetchBlock(misPreInstrIdx.bits), io.req.valid)
  private val misInstrOffset      = RegEnable(instrOffset(misPreInstrIdx.bits), io.req.valid)

  io.resp.stage2Out.twoFetchFaultType.zipWithIndex.foreach { case (faultType, i) =>
    faultType := MuxCase(
      PreDecodeFaultType.NoFault,
      Seq(
        jalFaultVecNext(i)  -> PreDecodeFaultType.JalFault,
        jalrFaultVecNext(i) -> PreDecodeFaultType.JalrFault,
        retFaultVecNext(i)  -> PreDecodeFaultType.RetFault,
        targetFault(i)      -> PreDecodeFaultType.TargetFault,
        notCFITakenNext(i)  -> PreDecodeFaultType.NotCfiFault,
        invalidTakenNext(i) -> PreDecodeFaultType.InvalidTaken
      )
    )
  }

  io.resp.stage2Out.fixedTwoFetchTarget.zipWithIndex.foreach { case (target, i) =>
    target := Mux(jalFaultVecNext(i) || targetFault(i), jumpTargetsNext(i), seqTargetsNext(i))
  }
  io.resp.stage2Out.twoFetchJalTarget.zipWithIndex.foreach { case (target, i) => target := jumpTargetsNext(i) }

  val fixedFirstMissPredIdx  = WireDefault(0.U.asTypeOf(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  val fixedSecondMissPredIdx = WireDefault(0.U.asTypeOf(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  fixedFirstMissPredIdx.valid := (misPreIdxNext.valid && misIsFirstBlockNext) || targetFault(firstTakenIdxNext)
  fixedFirstMissPredIdx.bits  := Mux(misPreIdxNext.valid && misIsFirstBlockNext, misPreIdxNext.bits, firstTakenIdxNext)

  fixedSecondMissPredIdx.valid := (misPreIdxNext.valid && !misIsFirstBlockNext) || targetFault(secondTakenIdxNext)
  fixedSecondMissPredIdx.bits := Mux(misPreIdxNext.valid && !misIsFirstBlockNext, misPreIdxNext.bits, firstTakenIdxNext)

  io.resp.stage2Out.fixedFirstMissPredIdx  := fixedFirstMissPredIdx
  io.resp.stage2Out.fixedSecondMissPredIdx := fixedSecondMissPredIdx

  // io.resp.stage2Out.fixedFirstBubbleRange   := fixedFirstBubbleRange
  // io.resp.stage2Out.fixedSecondBubbleRange  := fixedSecondBubbleRange

  io.resp.stage2Out.fixedFirstTakenIdx  := fixedFirstTakenInstrIdxNext
  io.resp.stage2Out.fixedSecondTakenIdx := fixedSecondTakenInstrIdxNext

  io.resp.stage2Out.fixedFirstBubbleInstrRange := Fill(
    PredictWidth,
    !(misPreIdxNext.valid && misIsFirstBlockNext)
  ) | Fill(PredictWidth, 1.U(1.W)) >> ~misInstrOffset(log2Ceil(PredictWidth) - 1, 0)
  io.resp.stage2Out.fixedSecondBubbleInstrRange := Fill(
    PredictWidth,
    !(misPreIdxNext.valid && !misIsFirstBlockNext)
  ) | Fill(PredictWidth, 1.U(1.W)) >> ~misInstrOffset(log2Ceil(PredictWidth) - 1, 0)
}
