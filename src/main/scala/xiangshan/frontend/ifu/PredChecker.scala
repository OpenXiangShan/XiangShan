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
import utility.ParallelPriorityEncoder
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr

class PredChecker(implicit p: Parameters) extends IfuModule {
  class PredCheckerIO extends IfuBundle {
    class PredCheckerReq(implicit p: Parameters) extends IfuBundle {
      val ftqOffset:  Valid[UInt]        = Valid(UInt(log2Ceil(PredictWidth).W))
      val jumpOffset: Vec[PrunedAddr]    = Vec(PredictWidth, PrunedAddr(VAddrBits))
      val target:     PrunedAddr         = PrunedAddr(VAddrBits)
      val instrRange: Vec[Bool]          = Vec(PredictWidth, Bool())
      val instrValid: Vec[Bool]          = Vec(PredictWidth, Bool())
      val pds:        Vec[PreDecodeInfo] = Vec(PredictWidth, new PreDecodeInfo)
      val pc:         Vec[PrunedAddr]    = Vec(PredictWidth, PrunedAddr(VAddrBits))
    }

    class PredCheckerResp(implicit p: Parameters) extends IfuBundle {
      // to Ibuffer write port  (stage 1)
      class S1Out(implicit p: Parameters) extends IfuBundle {
        val fixedRange: Vec[Bool] = Vec(PredictWidth, Bool())
        val fixedTaken: Vec[Bool] = Vec(PredictWidth, Bool())
      }
      // to Ftq write back port (stage 2)
      class S2Out(implicit p: Parameters) extends IfuBundle {
        val fixedTarget:   Vec[PrunedAddr] = Vec(PredictWidth, PrunedAddr(VAddrBits))
        val jalTarget:     Vec[PrunedAddr] = Vec(PredictWidth, PrunedAddr(VAddrBits))
        val fixedMissPred: Vec[Bool]       = Vec(PredictWidth, Bool())
        val faultType:     Vec[UInt]       = Vec(PredictWidth, PreDecodeFaultType())
      }
      val stage1Out: S1Out = new S1Out
      val stage2Out: S2Out = new S2Out
    }

    val req:  Valid[PredCheckerReq] = Flipped(ValidIO(new PredCheckerReq))
    val resp: PredCheckerResp       = Output(new PredCheckerResp)
  }

  val io: PredCheckerIO = IO(new PredCheckerIO)

  private val (takenIdx, predTaken)    = (io.req.bits.ftqOffset.bits, io.req.bits.ftqOffset.valid)
  private val predTarget               = io.req.bits.target
  private val (instrRange, instrValid) = (io.req.bits.instrRange, io.req.bits.instrValid)
  private val (pds, pc, jumpOffset)    = (io.req.bits.pds, io.req.bits.pc, io.req.bits.jumpOffset)

  private val jalFaultVec, jalrFaultVec, retFaultVec, targetFault, notCfiTaken, invalidTaken =
    Wire(Vec(PredictWidth, Bool()))

  /** remask fault may appear together with other faults, but other faults are exclusive
   * so other f ault mast use fixed mask to keep only one fault would be found and redirect to Ftq
   * we first detecct remask fault and then use fixedRange to do second check
   **/

  // Stage 1: detect remask fault
  /** first check: remask Fault */
  jalFaultVec := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isJal && instrRange(i) && instrValid(i) && (takenIdx > i.U && predTaken || !predTaken)
  })
  jalrFaultVec := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isJalr && !pd.isRet && instrRange(i) && instrValid(i) && (takenIdx > i.U && predTaken || !predTaken)
  })
  retFaultVec := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isRet && instrRange(i) && instrValid(i) && (takenIdx > i.U && predTaken || !predTaken)
  })
  private val remaskFault =
    VecInit((0 until PredictWidth).map(i => jalFaultVec(i) || jalrFaultVec(i) || retFaultVec(i)))
  private val remaskIdx  = ParallelPriorityEncoder(remaskFault.asUInt)
  private val needRemask = ParallelOR(remaskFault)
  private val fixedRange =
    instrRange.asUInt & (Fill(PredictWidth, !needRemask) | Fill(PredictWidth, 1.U(1.W)) >> ~remaskIdx)

  require(
    isPow2(PredictWidth),
    "If PredictWidth does not satisfy the power of 2," +
      "expression: Fill(PredictWidth, 1.U(1.W)) >> ~remaskIdx is not right !!"
  )

  io.resp.stage1Out.fixedRange := fixedRange.asTypeOf(Vec(PredictWidth, Bool()))

  io.resp.stage1Out.fixedTaken := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    instrValid(i) && fixedRange(i) && (pd.isRet || pd.isJal || pd.isJalr || takenIdx === i.U && predTaken && !pd.notCFI)
  })

  /** second check: faulse prediction fault and target fault */
  notCfiTaken := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    fixedRange(i) && instrValid(i) && i.U === takenIdx && pd.notCFI && predTaken
  })
  invalidTaken := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    fixedRange(i) && !instrValid(i) && i.U === takenIdx && predTaken
  })

  private val jumpTargets = VecInit(pds.zipWithIndex.map { case (pd, i) =>
    (pc(i) + jumpOffset(i)).asTypeOf(PrunedAddr(VAddrBits))
  })
  private val seqTargets =
    VecInit((0 until PredictWidth).map(i => pc(i) + Mux(pds(i).isRVC || !instrValid(i), 2.U, 4.U)))

  // Stage 2: detect target fault
  /** target calculation: in the next stage  */
  private val fixedRangeNext   = RegEnable(fixedRange, io.req.valid)
  private val instrValidNext   = RegEnable(instrValid, io.req.valid)
  private val takenIdxNext     = RegEnable(takenIdx, io.req.valid)
  private val predTakenNext    = RegEnable(predTaken, io.req.valid)
  private val predTargetNext   = RegEnable(predTarget, io.req.valid)
  private val jumpTargetsNext  = RegEnable(jumpTargets, io.req.valid)
  private val seqTargetsNext   = RegEnable(seqTargets, io.req.valid)
  private val pdsNext          = RegEnable(pds, io.req.valid)
  private val jalFaultVecNext  = RegEnable(jalFaultVec, io.req.valid)
  private val jalrFaultVecNext = RegEnable(jalrFaultVec, io.req.valid)
  private val retFaultVecNext  = RegEnable(retFaultVec, io.req.valid)
  private val notCFITakenNext  = RegEnable(notCfiTaken, io.req.valid)
  private val invalidTakenNext = RegEnable(invalidTaken, io.req.valid)

  targetFault := VecInit(pdsNext.zipWithIndex.map { case (pd, i) =>
    fixedRangeNext(i) && instrValidNext(
      i
    ) && (pd.isJal || pd.isBr) && takenIdxNext === i.U && predTakenNext && (predTargetNext =/= jumpTargetsNext(i))
  })

  io.resp.stage2Out.faultType.zipWithIndex.foreach { case (faultType, i) =>
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

  io.resp.stage2Out.fixedMissPred.zipWithIndex.foreach { case (missPred, i) =>
    missPred := jalFaultVecNext(i) || jalrFaultVecNext(i) || retFaultVecNext(i) || notCFITakenNext(i) ||
      invalidTakenNext(i) || targetFault(i)
  }
  io.resp.stage2Out.fixedTarget.zipWithIndex.foreach { case (target, i) =>
    target := Mux(jalFaultVecNext(i) || targetFault(i), jumpTargetsNext(i), seqTargetsNext(i))
  }
  io.resp.stage2Out.jalTarget.zipWithIndex.foreach { case (target, i) => target := jumpTargetsNext(i) }

}
