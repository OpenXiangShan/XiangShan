/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._
import org.chipsalliance.cde.config.Parameters

class DiffRenameTable(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val diffCommits = Input(new DiffCommitIO)
  })

  val int_table = RegInit(VecInit.fill    (IntLogicRegs)(0.U(PhyRegIdxWidth.W)))
  val fp_table  = RegInit(VecInit.tabulate(FpLogicRegs) (_.U(PhyRegIdxWidth.W)))
  val vec_table = RegInit(VecInit.tabulate(VecLogicRegs)(_.U(PhyRegIdxWidth.W)))
  val v0_table  = RegInit(VecInit.tabulate(V0LogicRegs) (_.U(PhyRegIdxWidth.W)))
  val vl_table  = RegInit(VecInit.tabulate(VlLogicRegs) (_.U(PhyRegIdxWidth.W)))

  when (io.diffCommits.isCommit) {
    for ((valid, info) <- io.diffCommits.commitValid zip io.diffCommits.info) {
      when(valid) {
        when(info.rfWen)  { int_table(info.ldest) := info.pdest }
        when(info.fpWen)  { fp_table (info.ldest) := info.pdest }
        when(info.vecWen) { vec_table(info.ldest) := info.pdest }
        when(info.v0Wen)  { v0_table (info.ldest) := info.pdest }
        when(info.vlWen)  { vl_table (info.ldest) := info.pdest }
      }
    }
  }
}
