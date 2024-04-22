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
import xiangshan.frontend.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfo, LsTopdownInfo}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.SnapshotGenerator

class NewRobDeqPtrWrapper(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // for commits/flush
    val state = Input(UInt(2.W))
    val deq_v = Vec(CommitWidth, Input(Bool()))
    val deq_w = Vec(CommitWidth, Input(Bool()))
    val hasCommitted = Vec(CommitWidth, Input(Bool()))
    val allCommitted = Input(Bool())
    val exception_state = Flipped(ValidIO(new RobExceptionInfo))
    // for flush: when exception occurs, reset deqPtrs to range(0, CommitWidth)
    val intrBitSetReg = Input(Bool())
    val hasNoSpecExec = Input(Bool())
    val interrupt_safe = Input(Bool())
    val blockCommit = Input(Bool())
    // output: the CommitWidth deqPtr
    val out = Vec(CommitWidth, Output(new RobPtr))
    val next_out = Vec(CommitWidth, Output(new RobPtr))
    val commitCnt = Output(UInt(log2Up(CommitWidth+1).W))
    val canCommitPriorityCond = Output(Vec(CommitWidth+1,Bool()))
    val commitEn = Output(Bool())
  })

  val bankAddrWidth = log2Up(CommitWidth)
  val deqPtrVec = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new RobPtr))))
  val deqPosition = deqPtrVec(0).value(bankAddrWidth - 1, 0)

  // for exceptions (flushPipe included) and interrupts:
  // only consider the first instruction
  val intrEnable = io.intrBitSetReg && !io.hasNoSpecExec && io.interrupt_safe
  val exceptionEnable = io.deq_w(deqPosition) && io.exception_state.valid && io.exception_state.bits.not_commit && io.exception_state.bits.robIdx === deqPtrVec(0)
  val redirectOutValid = io.state === 0.U && io.deq_v(deqPosition) && (intrEnable || exceptionEnable)

  // for normal commits: only to consider when there're no exceptions
  // we don't need to consider whether the first instruction has exceptions since it wil trigger exceptions.
  val realCommitLast = deqPtrVec(0).lineHeadPtr + Fill(bankAddrWidth, 1.U)
  val commit_exception = io.exception_state.valid && !isAfter(io.exception_state.bits.robIdx, realCommitLast)
  val canCommit = VecInit((0 until CommitWidth).map(i => io.deq_v(i) && io.deq_w(i) || io.hasCommitted(i)))
  val normalCommitCnt = PriorityEncoder(canCommit.map(c => !c) :+ true.B) - PopCount(io.hasCommitted)
  // when io.intrBitSetReg or there're possible exceptions in these instructions,
  // only one instruction is allowed to commit
  val allowOnlyOne = commit_exception || io.intrBitSetReg
  val commitCnt = Mux(allowOnlyOne, canCommit(realCommitLast.value), normalCommitCnt)
  val allowOnlyOneCond = Wire(chiselTypeOf(io.canCommitPriorityCond))
  allowOnlyOneCond.zipWithIndex.map{ case (value,i) => {
    if (i==0) value := false.B
    else value := Mux((i-1).U === deqPosition, canCommit(deqPosition), false.B)
  }
  }
  io.canCommitPriorityCond := Mux(allowOnlyOne, allowOnlyOneCond, VecInit(canCommit.map(c => !c) :+ true.B))

  val commitDeqPtrAll = VecInit((0 until 2*CommitWidth).map{case i => deqPtrVec(0).lineHeadPtr + i.U})
  val commitDeqPtrVec = Wire(chiselTypeOf(deqPtrVec))
  for (i <- 0 until CommitWidth){
    commitDeqPtrVec(i) := PriorityMuxDefault(io.canCommitPriorityCond.zip(commitDeqPtrAll.drop(i).take(CommitWidth+1)), deqPtrVec(i))
  }
  val deqPtrVec_next = Mux(io.state === 0.U && !redirectOutValid && !io.blockCommit, commitDeqPtrVec, deqPtrVec)

  deqPtrVec := deqPtrVec_next

  io.next_out := deqPtrVec_next
  io.out      := deqPtrVec
  io.commitCnt := commitCnt
  io.commitEn := io.state === 0.U && !redirectOutValid && !io.blockCommit

  when (io.state === 0.U) {
    XSInfo(io.state === 0.U && commitCnt > 0.U, "retired %d insts\n", commitCnt)
  }
  if(backendParams.debugEn){
    dontTouch(commitDeqPtrVec)
    dontTouch(commitDeqPtrAll)
    dontTouch(allowOnlyOneCond)
    dontTouch(io.canCommitPriorityCond)
    dontTouch(redirectOutValid)
  }

}