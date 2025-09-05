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

package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.Resolve
import xiangshan.frontend.bpu.BpuTrain

class ResolveQueueCommit(implicit p: Parameters) extends FtqModule {

  private def TemporaryConstant = 3 // FIXME: Should be BJU number

  class ResolveQueueIO extends Bundle {
    val backendResolve: Vec[Valid[Resolve]] = Input(Vec(TemporaryConstant, Valid(new Resolve)))
    val commit:         Bool                = Input(Bool())
    val commitPtr:      FtqPtr              = Input(new FtqPtr)
    val redirect:       Bool                = Input(Bool())
    val redirectPtr:    FtqPtr              = Input(new FtqPtr)
    val bpuTrain:       Valid[ResolveEntry] = Output(Valid(new ResolveEntry))
  }

  val io: ResolveQueueIO = IO(new ResolveQueueIO)

  private val mem = RegInit(0.U.asTypeOf(Vec(ResolveQueueSize, new ResolveEntry)))

  io.backendResolve.zipWithIndex.foreach { case (branch, i) =>
    when(branch.valid) {
      val ftqIdx = branch.bits.ftqIdx
      val previousHit = PopCount(io.backendResolve.take(i).map(previousBranch =>
        previousBranch.valid && previousBranch.bits.ftqIdx === ftqIdx
      ))
      val lastValid = mem(ftqIdx.value).branches.lastIndexWhere(_.valid)
      mem(ftqIdx.value).branches(lastValid + previousHit) := branch.bits
    }
  }

  when(io.redirect) {
//    mem.foreach(entry => when(entry.ftqIdx))
  }

  io.bpuTrain := mem(io.commitPtr.value)
  when(io.commit) {
    mem(io.commitPtr.value).branches.foreach(_.valid := false.B)
  }
}
