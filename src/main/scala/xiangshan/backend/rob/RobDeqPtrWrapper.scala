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
import xiangshan.{XSBundle, XSModule}
import xiangshan.backend.BackendParams
import xiangshan.backend.rob.RobDeqPtrWrapper._

class RobDeqPtrWrapper(implicit p: Parameters, params: BackendParams) extends XSModule {
  val in = IO(new In)
  val out = IO(new Out)

  val ptrs = RegInit(VecInit((0 until CommitWidth).map(i => RobPtr(false.B, i.U))))
  val candidates = ptrs.toSeq ++ ptrs.map(_.addEntries(CommitWidth.U))
  for (i <- 0 until CommitWidth) {
    out.nextPtrVec(i) := Mux1H(in.advanceOH, candidates.slice(i, i + CommitWidth + 1))
  }
  ptrs := out.nextPtrVec
  out.ptrVec := ptrs
}

object RobDeqPtrWrapper {
  class In(implicit p: Parameters, params: BackendParams) extends XSBundle {
    // Bit k advances the head by k retired entries.
    val advanceOH = Input(UInt((CommitWidth + 1).W))
  }

  class Out(implicit p: Parameters, params: BackendParams) extends XSBundle {
    val ptrVec = Output(Vec(CommitWidth, new RobPtr))
    val nextPtrVec = Output(Vec(CommitWidth, new RobPtr))
  }
}
