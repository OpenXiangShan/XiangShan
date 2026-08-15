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
***************************************************************************************/

package xiangshan.backend.rename.freelist

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._


class MEFreeList(size: Int, commitWidth: Int)(implicit p: Parameters) extends BaseFreeList(size, commitWidth) with HasPerfEvents {
  val msr = IO(new Bundle {
    val hold = Input(UInt(size.W))
    val release = Input(UInt(size.W))
    val claimReq = Input(Vec(RenameWidth, Bool()))
    val claimFire = Input(Vec(RenameWidth, Bool()))
    val claimPReg = Input(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val held = Output(UInt(size.W))
    val freeCount = Output(UInt(log2Ceil(size + 1).W))
  })

  val freeList = RegInit(VecInit(
    // originally {1, 2, ..., size - 1} are free. Register 0-31 are mapped to x0.
    Seq.tabulate(size - 1)(i => (i + 1).U(PhyRegIdxWidth.W)) :+ 0.U(PhyRegIdxWidth.W)))

  val tailPtr = RegInit(FreeListPtr(false, size - 1))

  val doWalkRename = io.walk && io.doAllocate && !io.redirect
  val doNormalRename = io.canAllocate && io.doAllocate && !io.redirect
  val doRename = doWalkRename || doNormalRename
  val doCommit = io.commit.doCommit

  val heldMask = RegInit(0.U(size.W))
  val heldBeforeClaim = (heldMask & ~msr.release) | msr.hold
  val msrClaimReqMask = VecInit((0 until RenameWidth).map { lane =>
    Mux(msr.claimReq(lane), UIntToOH(msr.claimPReg(lane), size), 0.U(size.W))
  }).reduce(_ | _)
  val msrClaimFireMask = VecInit((0 until RenameWidth).map { lane =>
    Mux(msr.claimFire(lane), UIntToOH(msr.claimPReg(lane), size), 0.U(size.W))
  }).reduce(_ | _)
  val heldMaskNext = heldBeforeClaim & ~msrClaimFireMask
  heldMask := heldMaskNext
  msr.held := heldMask

  assert(!heldMask(0), "x0 may not be held by the Squash Log")
  assert(!msr.hold(0), "the Squash Log attempted to hold x0")
  assert((msr.release & ~heldMask).asUInt === 0.U,
    "the Squash Log attempted to release a PReg it does not hold")
  assert((msr.hold & heldMask & ~msr.release).asUInt === 0.U,
    "the Squash Log attempted to hold an already-held PReg")
  assert((msrClaimReqMask & ~heldBeforeClaim).asUInt === 0.U,
    "rename attempted to claim a PReg not held by the Squash Log")
  assert(PopCount(msrClaimReqMask) === PopCount(msr.claimReq),
    "two rename lanes attempted to claim the same held PReg")
  assert((msr.claimFire.asUInt & ~msr.claimReq.asUInt) === 0.U,
    "integer freelist received an MSR claim fire without a request")
  assert(PopCount(heldMaskNext) <= MsrConfig.MaxHeldPRegs.U,
    "the Squash Log exceeded its held-PReg admission limit")

  /**
    * Held PRegs remain in the circular queue so snapshot pointer semantics stay
    * intact. Allocation selects the first non-held PReg and swaps it into the
    * current head position before advancing the head. A release becomes visible
    * to ordinary allocation in the following cycle, which avoids a combinational
    * loop through ROB enqueue; an explicit claim may still consume a held PReg.
    */
  val rawFreeRegCnt = distanceBetween(tailPtr, headPtr)
  val heldForAllocation = heldMask | msr.hold
  val operationReq = VecInit(io.allocateReq.zip(msr.claimReq).map { case (allocate, claim) =>
    allocate || claim
  })
  val operationCount = PopCount(operationReq)
  val laneStages = Seq.fill(RenameWidth + 1)(Wire(Vec(size, UInt(PhyRegIdxWidth.W))))
  val laneSelectedPhyReg = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  laneStages.head := freeList

  for (lane <- 0 until RenameWidth) {
    val priorOperationCount = PopCount(operationReq.take(lane))
    val candidateIndex = VecInit.tabulate(size) { offset =>
      (headPtr + priorOperationCount + offset.U).value
    }
    val candidateValid = VecInit.tabulate(size) { offset =>
      val candidatePReg = laneStages(lane)(candidateIndex(offset))
      priorOperationCount + offset.U < rawFreeRegCnt && Mux(
        msr.claimReq(lane),
        candidatePReg === msr.claimPReg(lane) && heldForAllocation(candidatePReg),
        !heldForAllocation(candidatePReg)
      )
    }
    val candidateOH = PriorityEncoderOH(candidateValid.asUInt)
    val selectedIndex = Mux1H(candidateOH, candidateIndex)
    val headIndex = (headPtr + priorOperationCount).value

    laneSelectedPhyReg(lane) := laneStages(lane)(selectedIndex)
    laneStages(lane + 1) := laneStages(lane)
    when(doNormalRename && operationReq(lane)) {
      assert(candidateOH.orR, "integer freelist could not find the requested allocation candidate")
      laneStages(lane + 1)(headIndex) := laneStages(lane)(selectedIndex)
      laneStages(lane + 1)(selectedIndex) := laneStages(lane)(headIndex)
    }
  }

  for (i <- 0 until RenameWidth) {
    io.allocatePhyReg(i) := laneSelectedPhyReg(i)
    when(doNormalRename && io.allocateReq(i)) {
      assert(!heldForAllocation(io.allocatePhyReg(i)), "integer freelist allocated a held PReg")
    }
    when(msr.claimReq(i)) {
      assert(msr.claimPReg(i) =/= 0.U, "MSR attempted to claim p0")
      assert(!io.allocateReq(i), "MSR claim lane also requested an ordinary PReg allocation")
    }
    when(doNormalRename && msr.claimReq(i)) {
      assert(io.allocatePhyReg(i) === msr.claimPReg(i),
        "integer freelist did not allocate the claimed held PReg")
    }
    when(msr.claimFire(i)) {
      assert(doNormalRename, "MSR claim fired while the integer freelist was not advancing")
    }
  }
  // update arch head pointer
  val archAlloc = io.commit.archAlloc

  val numArchAllocate = PopCount(archAlloc)
  val archHeadPtrNew  = archHeadPtr + numArchAllocate
  val archHeadPtrNext = Mux(doCommit, archHeadPtrNew, archHeadPtr)
  archHeadPtr := archHeadPtrNext

  // update head pointer
  val numAllocate = Mux(io.walk, PopCount(io.walkReq), operationCount)
  val headPtrNew   = Mux(lastCycleRedirect, redirectedHeadPtr, headPtr + numAllocate)
  val headPtrOHNew = Mux(lastCycleRedirect, redirectedHeadPtrOH, headPtrOHVec(numAllocate))
  val headPtrNext   = Mux(doRename, headPtrNew, headPtr)
  val headPtrOHNext = Mux(doRename, headPtrOHNew, headPtrOH)
  headPtr   := headPtrNext
  headPtrOH := headPtrOHNext

  /**
    * Deallocation: when refCounter becomes zero, the register can be released to freelist
    */
  val freeListAfterAllocation = Wire(Vec(size, UInt(PhyRegIdxWidth.W)))
  freeListAfterAllocation := freeList
  when(doNormalRename) {
    freeListAfterAllocation := laneStages.last
  }
  val freePtr = VecInit(Seq.tabulate(commitWidth)(i => tailPtr + PopCount(io.freeReq.take(i))))
  for (i <- 0 until size) {
    val freeReqOH = VecInit(io.freeReq.zipWithIndex.map { case (w, idx) =>
      w && freePtr(idx).value === i.U
    })
    val freePhyReg = Mux1H(freeReqOH, io.freePhyReg)
    freeList(i) := Mux(freeReqOH.asUInt.orR, freePhyReg, freeListAfterAllocation(i))
  }

  // update tail pointer
  val tailPtrNext = tailPtr + PopCount(io.freeReq)
  tailPtr := tailPtrNext

  val rawFreeRegCntNext = Mux(doWalkRename && !lastCycleRedirect, distanceBetween(tailPtrNext, headPtr) - PopCount(io.walkReq),
                   Mux(doNormalRename,                     distanceBetween(tailPtrNext, headPtr) - operationCount,
                                                           distanceBetween(tailPtrNext, headPtr)))
  val heldRegCntNext = PopCount(heldMaskNext)
  val freeRegCnt = Mux(rawFreeRegCntNext >= heldRegCntNext,
    rawFreeRegCntNext - heldRegCntNext,
    0.U)
  val currentFreeRegCnt = Mux(rawFreeRegCnt >= PopCount(heldForAllocation),
    rawFreeRegCnt - PopCount(heldForAllocation),
    0.U)
  msr.freeCount := Mux(rawFreeRegCnt >= PopCount(heldMask),
    rawFreeRegCnt - PopCount(heldMask),
    0.U)
  val freeRegCntReg = RegNext(freeRegCnt)
  io.canAllocate := freeRegCntReg >= RenameWidth.U && currentFreeRegCnt >= RenameWidth.U

  if(backendParams.debugEn){
    val debugArchHeadPtr = RegNext(RegNext(archHeadPtr, FreeListPtr(false, 0)), FreeListPtr(false, 0)) // two-cycle delay from refCounter
    val debugArchRAT = RegNext(RegNext(io.debug_rat.get, VecInit(Seq.fill(32)(0.U(PhyRegIdxWidth.W)))), VecInit(Seq.fill(32)(0.U(PhyRegIdxWidth.W))))
    val debugUniqPR = Seq.tabulate(32)(i => i match {
      case 0 => true.B
      case _ => !debugArchRAT.take(i).map(_ === debugArchRAT(i)).reduce(_ || _)
    })
    XSError(distanceBetween(tailPtr, debugArchHeadPtr) +& PopCount(debugUniqPR) =/= size.U, "Integer physical register should be in either arch RAT or arch free list\n")
  }

  QueuePerf(size = size, utilization = freeRegCntReg, full = freeRegCntReg === 0.U)

  XSPerfAccumulate("allocation_blocked_cycle", !io.canAllocate)
  XSPerfAccumulate("can_alloc_wrong", !io.canAllocate && freeRegCnt >= RenameWidth.U)
  val msrHeldPRegCount = PopCount(heldMask)
  val msrHeldPRegPeak = RegInit(0.U(log2Ceil(size + 1).W))
  val msrHeldPRegPeakIncrease = Mux(
    msrHeldPRegCount > msrHeldPRegPeak,
    msrHeldPRegCount - msrHeldPRegPeak,
    0.U
  )
  when(msrHeldPRegCount > msrHeldPRegPeak) {
    msrHeldPRegPeak := msrHeldPRegCount
  }
  XSPerfAccumulate("msr_held_preg_occupancy", msrHeldPRegCount)
  XSPerfAccumulate("msr_held_preg_peak", msrHeldPRegPeakIncrease)
  XSPerfAccumulate("msr_held_preg_cycle", heldMask.orR)
  XSPerfAccumulate("msr_freelist_hold_blocked_cycle", rawFreeRegCnt >= RenameWidth.U && !io.canAllocate)

  val perfEvents = Seq(
    ("me_freelist_1_4_valid", freeRegCntReg <  (size / 4).U                                     ),
    ("me_freelist_2_4_valid", freeRegCntReg >= (size / 4).U && freeRegCntReg <= (size / 2).U    ),
    ("me_freelist_3_4_valid", freeRegCntReg >= (size / 2).U && freeRegCntReg <= (size * 3 / 4).U),
    ("me_freelist_4_4_valid", freeRegCntReg >= (size * 3 / 4).U                                 ),
  )
  generatePerfEvent()
}
