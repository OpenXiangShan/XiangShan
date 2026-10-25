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

package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.SnapshotGenerator
import xiangshan.backend.rob.RobDeqPtrWrapper._

class RobDeqPtrWrapper(implicit p: Parameters, params: BackendParams) extends XSModule with HasCircularQueuePtrHelper {
  val in = IO(new In)
  val out = IO(new Out)

  val bankAddrWidth = log2Up(CommitWidth)
  val deqPtrVec = RegInit(VecInit((0 until CommitWidth).map(i => RobPtr(false.B, i.U))))
  val deqPosition = deqPtrVec(0).value(bankAddrWidth - 1, 0)

  // for exceptions (flushPipe included) and interrupts:
  // only consider the first instruction
  val intrEnable = in.intrBitSetReg && !in.hasNoSpecExec && in.interruptSafe
  val exceptionEnable = in.deq_w(deqPosition) && in.exception_state.valid && in.exception_state.bits.not_commit && in.exception_state.bits.robIdx.isSameEntry(deqPtrVec(0))
  val redirectOutValid = in.state === 0.U && in.deq_v(deqPosition) && (intrEnable || exceptionEnable)

  // for normal commits: only to consider when there're no exceptions
  // we don't need to consider whether the first instruction has exceptions since it wil trigger exceptions.
  val realCommitLast = deqPtrVec(0).lineHeadPtr.addEntries(Fill(bankAddrWidth, 1.U))
  val realCommitLastSlot = WireInit(realCommitLast)
  realCommitLastSlot.isFormer := false.B
  val commit_exception = in.exception_state.valid && !in.exception_state.bits.robIdx.isAfterSlot(realCommitLastSlot)
  val canCommit = VecInit((0 until CommitWidth).map(i => in.deq_v(i) && in.deq_w(i) || in.hasCommitted(i)))
  val normalCommitCnt = PriorityEncoder(canCommit.map(c => !c) :+ true.B) - PopCount(in.hasCommitted)
  // when in.intrBitSetReg or there're possible exceptions in these instructions,
  // only one instruction is allowed to commit
  val allowOnlyOne = in.allowOnlyOneCommit
  val commitCnt = Mux(allowOnlyOne, canCommit(realCommitLast.value), normalCommitCnt)
  val allowOnlyOneCond = Wire(chiselTypeOf(out.canCommitPriorityCond))
  allowOnlyOneCond.zipWithIndex.map{ case (value,i) => {
    if (i==0) value := false.B
    else value := Mux((i-1).U === deqPosition, canCommit(deqPosition), false.B)
  }
  }
  out.canCommitPriorityCond := Mux(allowOnlyOne, allowOnlyOneCond, VecInit(canCommit.map(c => !c) :+ true.B))

  val commitDeqPtrAll = VecInit((0 until 2 * CommitWidth).map(i => deqPtrVec(0).lineHeadPtr.addEntries(i.U).asFormer))
  val commitDeqPtrVec = Wire(chiselTypeOf(deqPtrVec))
  for (i <- 0 until CommitWidth){
    commitDeqPtrVec(i) := PriorityMuxDefault(out.canCommitPriorityCond.zip(commitDeqPtrAll.drop(i).take(CommitWidth+1)), deqPtrVec(i))
  }
  val deqPtrVec_next = Mux(in.state === 0.U && !redirectOutValid && !in.blockCommit, commitDeqPtrVec, deqPtrVec)

  deqPtrVec := deqPtrVec_next

  out.nextPtrVec := deqPtrVec_next
  out.ptrVec      := deqPtrVec
  out.commitCnt := commitCnt
  out.commitEn := in.state === 0.U && !redirectOutValid && !in.blockCommit

  XSInfo(in.state === 0.U && commitCnt > 0.U, "retired %d insts\n", commitCnt)

  if(backendParams.debugEn){
    dontTouch(commitDeqPtrVec)
    dontTouch(commitDeqPtrAll)
    dontTouch(allowOnlyOneCond)
    dontTouch(out.canCommitPriorityCond)
    dontTouch(redirectOutValid)
  }

}

object RobDeqPtrWrapper {
  class In(implicit p: Parameters, params: BackendParams) extends XSBundle {
    // ROB controller mode.
    val state = Input(UInt(2.W))

    // Commit window eligibility.
    val deq_v = Vec(CommitWidth, Input(Bool()))
    val deq_w = Vec(CommitWidth, Input(Bool()))

    // Commit window progress.
    val hasCommitted = Vec(CommitWidth, Input(Bool()))
    val allCommitted = Input(Bool())

    // Serialization state.
    val hasNoSpecExec = Input(Bool())

    // Single-entry commit restriction.
    val allowOnlyOneCommit = Input(Bool())

    // Commit inhibition.
    val blockCommit = Input(Bool())

    // Oldest retained exception.
    val exception_state = Flipped(ValidIO(new RobExceptionInfo(ExceptionNO.exceptionGenSet(params))))

    // Interrupt qualification.
    val intrBitSetReg = Input(Bool())
    val interruptSafe = Input(Bool())
  }

  class Out(implicit p: Parameters, params: BackendParams) extends XSBundle {
    // Retirement count.
    val commitCnt = Output(UInt(log2Up(CommitWidth+1).W))

    // Retirement prefix selection.
    val canCommitPriorityCond = Output(Vec(CommitWidth+1,Bool()))

    // Retirement enable.
    val commitEn = Output(Bool())

    // Dequeue pointer state.
    val ptrVec = Vec(CommitWidth, Output(new RobPtr))
    val nextPtrVec = Vec(CommitWidth, Output(new RobPtr))
  }
}
