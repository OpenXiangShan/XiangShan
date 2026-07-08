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
import freechips.rocketchip.util.SeqToAugmentedSeq
import math.pow
import org.chipsalliance.cde.config.Parameters
import utility.ParallelOR
import utility.ParallelPriorityEncoder
import utility.XSPerfAccumulate
import utils.SeqUtils.prefixOr
import xiangshan.ValidUndirectioned
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BranchAttribute

class PredChecker(implicit p: Parameters) extends IfuModule {
  class PredCheckerIO extends IfuBundle {
    class PredCheckerReq(implicit p: Parameters) extends IfuBundle {
      val expandedInstrVec: Vec[Instruction]   = Vec(IBufferEnqueueWidth, new Instruction)
      val jumpOffsetVec:    Vec[PrunedAddr]    = Vec(IBufferEnqueueWidth, PrunedAddr(VAddrBits))
      val pdInfoVec:        Vec[PreDecodeInfo] = Vec(IBufferEnqueueWidth, new PreDecodeInfo)
      val instrPcVec:       Vec[PrunedAddr]    = Vec(IBufferEnqueueWidth, PrunedAddr(VAddrBits))
    }

    class PredCheckerResp(implicit p: Parameters) extends IfuBundle {
      // to Ibuffer write port  (stage 1) ---- Output data is offset-adjusted for IBuffer enqueue.
      class S1Out(implicit p: Parameters) extends IfuBundle {
        val fixedInstrValid: Vec[Bool] = Vec(IBufferEnqueueWidth, Bool())
        val fixedTaken:      Vec[Bool] = Vec(IBufferEnqueueWidth, Bool())
      }
      // to Ftq write back port (stage 2) ---- Output data with offset removed for FTQ write-back
      class S2Out(implicit p: Parameters) extends IfuBundle {
        val checkerRedirect: Valid[PredCheckRedirect] = Valid(new PredCheckRedirect)
        val perfFaultType:   Vec[UInt]                = Vec(FetchPorts, PreDecodeFaultType())
      }
      val stage1Out: S1Out = new S1Out
      val stage2Out: S2Out = new S2Out
    }
    val req:  Valid[PredCheckerReq] = Flipped(ValidIO(new PredCheckerReq))
    val resp: PredCheckerResp       = Output(new PredCheckerResp)
  }

  val io: PredCheckerIO = IO(new PredCheckerIO)

  private val expandedInstrVec = io.req.bits.expandedInstrVec
  private val endOffsetVec     = VecInit(expandedInstrVec.map(_.endOffset))
  private val blockSel         = VecInit(expandedInstrVec.map(_.blockSel))
  private val instrValid       = VecInit(expandedInstrVec.map(_.valid))
  private val isPredTaken      = VecInit(expandedInstrVec.map(_.isPredTaken))
  private val invalidTaken     = VecInit(expandedInstrVec.map(_.invalidTaken))

  private val pdInfoVec     = io.req.bits.pdInfoVec
  private val instrPcVec    = io.req.bits.instrPcVec
  private val jumpOffsetVec = io.req.bits.jumpOffsetVec

  private val jalFaultVec, jalrFaultVec, retFaultVec, notCfiTaken = Wire(Vec(IBufferEnqueueWidth, Bool()))

  /** Remask faults can occur alongside other faults, whereas other faults are mutually exclusive.
    * Therefore, for non-remask faults, a fixed fault mask must be used to ensure that only one fault
    * is detected and redirected to the FTQ.
    * The logic first checks for remask faults, and then applies the fixed range for a secondary check.
    */

  // Stage 1: detect remask fault
  /** first check: remask Fault */
  jalFaultVec := VecInit(pdInfoVec.zipWithIndex.map { case (pd, i) =>
    pd.brAttribute.isDirect && instrValid(i) && !isPredTaken(i)
  })
  jalrFaultVec := VecInit(pdInfoVec.zipWithIndex.map { case (pd, i) =>
    pd.brAttribute.isIndirect && !pd.brAttribute.hasPop && instrValid(i) && !isPredTaken(i)
  })
  retFaultVec := VecInit(pdInfoVec.zipWithIndex.map { case (pd, i) =>
    pd.brAttribute.hasPop && instrValid(i) && !isPredTaken(i)
  })
  notCfiTaken := VecInit(pdInfoVec.zipWithIndex.map { case (pd, i) =>
    instrValid(i) && pd.notCFI && isPredTaken(i)
  })

  private val remaskFault =
    VecInit((0 until IBufferEnqueueWidth).map(i =>
      jalFaultVec(i) || jalrFaultVec(i) || retFaultVec(i) || invalidTaken(i) || notCfiTaken(i)
    ))
  // Timing optimization: prefixOr is implemented as a parallel prefix tree (recursive bisection),
  // which retains all valid instruction entries before the first fault occurs (including the fault position itself).
  private val maskFaultOrBefore = false.B +: prefixOr(remaskFault)

  dontTouch(remaskFault.asUInt)

  // keep entries before and including the first remask fault
  private val fixedRange = VecInit((0 until IBufferEnqueueWidth).map {
    i => instrValid(i) && !maskFaultOrBefore(i)
  }).asUInt

  private val fixedInstrValid = VecInit((0 until IBufferEnqueueWidth).map(i =>
    expandedInstrVec(i).valid && fixedRange(i)
  ))
  io.resp.stage1Out.fixedInstrValid := fixedInstrValid

  private val fixedTakenVec = VecInit(pdInfoVec.zipWithIndex.map { case (pd, i) =>
    pd.brAttribute.isIndirect || pd.brAttribute.isDirect || (isPredTaken(i) && !pd.notCFI)
  })
  io.resp.stage1Out.fixedTaken := fixedTakenVec

  private val mispredIdx = WireDefault(0.U.asTypeOf(ValidUndirectioned(UInt(log2Ceil(IBufferEnqueueWidth).W))))
  private val stage1Fault = VecInit.tabulate(IBufferEnqueueWidth)(i =>
    jalFaultVec(i) || jalrFaultVec(i) || retFaultVec(i) || notCfiTaken(i) || invalidTaken(i)
  )
  dontTouch(stage1Fault.asUInt)
  mispredIdx.valid := ParallelOR(stage1Fault)
  mispredIdx.bits  := ParallelPriorityEncoder(stage1Fault)

  private val seqTargets = VecInit((0 until IBufferEnqueueWidth).map(i =>
    instrPcVec(i) + Mux(pdInfoVec(i).isRVC || invalidTaken(i), 2.U, 4.U)
  ))

  private val jumpTargets = VecInit(pdInfoVec.zipWithIndex.map { case (pd, i) =>
    (instrPcVec(i) + jumpOffsetVec(i)).asTypeOf(PrunedAddr(VAddrBits))
  })

  private val fixedIsJump =
    instrValid(mispredIdx.bits) &&
      mispredIdx.valid &&
      (pdInfoVec(mispredIdx.bits).isJal || pdInfoVec(mispredIdx.bits).isBr)

  private val finalIsRVC        = expandedInstrVec(mispredIdx.bits).isRvc
  private val finalInvalidTaken = invalidTaken(mispredIdx.bits)
  private val finalNotCfiTaken  = notCfiTaken(mispredIdx.bits)
  private val finalSelectBlock  = expandedInstrVec(mispredIdx.bits).blockSel
  private val finalPc           = instrPcVec(mispredIdx.bits)
  private val finalAttribute    = pdInfoVec(mispredIdx.bits).brAttribute
  private val fixedTaken        = fixedTakenVec(mispredIdx.bits)

  // The actual end of the prediction block is the instruction before invalidTaken.
  private val endOffset = endOffsetVec(mispredIdx.bits)

  private val mispredIdxNext       = RegEnable(mispredIdx, io.req.valid)
  private val finalIsRVCNext       = RegEnable(finalIsRVC, io.req.valid)
  private val finalAttributeNext   = RegEnable(finalAttribute, io.req.valid)
  private val invalidTakenNext     = RegEnable(finalInvalidTaken, io.req.valid)
  private val finalSelectBlockNext = RegEnable(finalSelectBlock, io.req.valid)
  private val jumpTargetsNext      = RegEnable(jumpTargets, io.req.valid)
  private val seqTargetsNext       = RegEnable(seqTargets, io.req.valid)
  private val fixedIsJumpNext      = RegEnable(fixedIsJump, io.req.valid)
  private val endOffsetNext        = RegEnable(endOffset, io.req.valid)
  private val finalPcNext          = RegEnable(finalPc, io.req.valid)
  private val wbValid              = RegNext(io.req.valid, init = false.B)

  private val fixedTarget = Mux(
    fixedIsJumpNext,
    jumpTargetsNext(mispredIdxNext.bits),
    seqTargetsNext(mispredIdxNext.bits)
  )

  io.resp.stage2Out.checkerRedirect.valid             := mispredIdxNext.valid && wbValid
  io.resp.stage2Out.checkerRedirect.bits.target       := fixedTarget
  io.resp.stage2Out.checkerRedirect.bits.misIdx       := mispredIdxNext
  io.resp.stage2Out.checkerRedirect.bits.taken        := fixedTaken
  io.resp.stage2Out.checkerRedirect.bits.isRVC        := finalIsRVCNext
  io.resp.stage2Out.checkerRedirect.bits.attribute    := Mux(invalidTakenNext, BranchAttribute.None, finalAttributeNext)
  io.resp.stage2Out.checkerRedirect.bits.selectBlock  := finalSelectBlockNext
  io.resp.stage2Out.checkerRedirect.bits.invalidTaken := invalidTakenNext
  io.resp.stage2Out.checkerRedirect.bits.mispredPc    := finalPcNext
  // FIXME: Not a reliable block-end marker; special cases may have only half a branch predicted.(invalidTaken)
  io.resp.stage2Out.checkerRedirect.bits.endOffset := endOffsetNext

  // --------- These registers are only for performance debugging purposes ---------------------/
  private val jalFaultVecNext  = RegEnable(jalFaultVec, io.req.valid)
  private val jalrFaultVecNext = RegEnable(jalrFaultVec, io.req.valid)
  private val retFaultVecNext  = RegEnable(retFaultVec, io.req.valid)
  private val notCFITakenNext  = RegEnable(notCfiTaken, io.req.valid)

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

  io.resp.stage2Out.perfFaultType(0) := Mux(!finalSelectBlockNext, faultType, PreDecodeFaultType.NoFault)
  io.resp.stage2Out.perfFaultType(1) := Mux(finalSelectBlockNext, faultType, PreDecodeFaultType.NoFault)
}
