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

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility.LookupTree
import xiangshan._

class BranchModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val func = Input(FuOpType())
    val pred_taken = Input(Bool())
    val taken, mispredict = Output(Bool())
  })
  val (src1, src2, func) = (io.src(0), io.src(1), io.func)

  val subModule = Module(new SubModule)
  subModule.io.src(0) := src1
  subModule.io.src(1) := src2
  val sub  = subModule.io.sub
  val sltu    = !sub(XLEN)
  val slt     = src1(XLEN - 1) ^ src2(XLEN - 1) ^ sltu
  val xor     = src1 ^ src2
  // branch
  val branchOpTable = List(
    BRUOpType.getBranchType(ALUOpType.beq)  -> !xor.orR,
    BRUOpType.getBranchType(ALUOpType.blt)  -> slt,
    BRUOpType.getBranchType(ALUOpType.bltu) -> sltu
  )
  val taken = LookupTree(BRUOpType.getBranchType(func), branchOpTable) ^ BRUOpType.isBranchInvert(func)

  io.taken := taken
  io.mispredict := io.pred_taken ^ taken
}
