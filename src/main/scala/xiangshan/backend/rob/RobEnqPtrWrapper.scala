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
import xiangshan.backend.Bundles.{ExceptionInfo, ExuOutput}
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.SnapshotGenerator
import xiangshan.backend.rob.RobEnqPtrWrapper._

class RobEnqPtrWrapper(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val in = IO(new In)
  val out = IO(new Out)

  val enqPtrVec = RegInit(VecInit.tabulate(RenameWidth)(i => RobPtr(false.B, i.U)))

  // enqueue
  val canAccept = in.allowEnqueue && !in.hasBlockBackward
  val dispatchNum = Mux(canAccept, PopCount(in.enq), 0.U)

  for ((ptr, i) <- enqPtrVec.zipWithIndex) {
    when(in.redirect.valid) {
      ptr := Mux(
        in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself(),
        in.redirect.bits.robIdx.addEntries(i.U).asFormer,
        in.redirect.bits.robIdx.addEntries((i + 1).U).asFormer
      )
    }.otherwise {
      ptr := ptr.addEntries(dispatchNum).asFormer
    }
  }

  out.ptrVec := enqPtrVec

}

object RobEnqPtrWrapper {
  class In(implicit p: Parameters) extends XSBundle {
    // Entry requests.
    val enq = Vec(RenameWidth, Input(Bool()))

    // Enqueue permission.
    val allowEnqueue = Input(Bool())

    // Recovery boundary.
    val redirect = Input(Valid(new Redirect))

    // Serialization barrier.
    val hasBlockBackward = Input(Bool())
  }

  class Out(implicit p: Parameters) extends XSBundle {
    // Candidate entry pointers.
    val ptrVec = Output(Vec(RenameWidth, new RobPtr))
  }
}
