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
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.bpu.BranchAttribute

// FIXME: this seems not used, maybe remove it later
class F3PreDecode(implicit p: Parameters) extends IfuModule with PreDecodeHelper {
  class F3PreDecodeIO(implicit p: Parameters) extends IfuBundle {
    val instr: Vec[UInt]          = Input(Vec(FetchBlockInstNum, UInt(32.W)))
    val pd:    Vec[PreDecodeInfo] = Output(Vec(FetchBlockInstNum, new PreDecodeInfo))
  }
  val io: F3PreDecodeIO = IO(new F3PreDecodeIO)

  io.pd.zipWithIndex.foreach { case (pd, i) =>
    pd.valid       := DontCare
    pd.isRVC       := DontCare
    pd.brAttribute := BranchAttribute.decode(io.instr(i))
  }
}
