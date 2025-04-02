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
import utility.SignExt
import xiangshan.HasXSParameter
import xiangshan.backend.decode.isa.predecode.PreDecodeInst
import xiangshan.frontend.BrType
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit

trait PreDecodeHelper extends HasXSParameter {
  def isRVC(inst: UInt): Bool = inst(1, 0) =/= 3.U

  def isLink(reg: UInt): Bool = reg === 1.U || reg === 5.U

  def getBrType(inst: UInt): UInt =
    ListLookup(inst, List(BrType.notCFI), PreDecodeInst.brTable).head

  def getBrInfo(inst: UInt): (UInt, Bool, Bool) = {
    val brType = getBrType(inst)
    val rd     = Mux(isRVC(inst), inst(12), inst(11, 7))
    val rs     = Mux(isRVC(inst), Mux(brType === BrType.jal, 0.U, inst(11, 7)), inst(19, 15))
    val isCall = (brType === BrType.jal && !isRVC(inst) || brType === BrType.jalr) && isLink(rd) // Only for RV64
    val isRet  = brType === BrType.jalr && isLink(rs) && !isCall
    (brType, isCall, isRet)
  }

  def getJalOffset(inst: UInt, isRvc: Bool): PrunedAddr = {
    val rvcOffset = Cat(inst(12), inst(8), inst(10, 9), inst(6), inst(7), inst(2), inst(11), inst(5, 3), 0.U(1.W))
    val rviOffset = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))
    val maxWidth  = rviOffset.getWidth
    PrunedAddrInit(SignExt(Mux(isRvc, SignExt(rvcOffset, maxWidth), SignExt(rviOffset, maxWidth)), VAddrBits))
  }

  def getBrOffset(inst: UInt, isRvc: Bool): PrunedAddr = {
    val rvcOffset = Cat(inst(12), inst(6, 5), inst(2), inst(11, 10), inst(4, 3), 0.U(1.W))
    val rviOffset = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W))
    val maxWidth  = rviOffset.getWidth
    PrunedAddrInit(SignExt(Mux(isRvc, SignExt(rvcOffset, maxWidth), SignExt(rviOffset, maxWidth)), VAddrBits))
  }
}

trait FetchBlockHelper extends HasXSParameter {
  def getBasicBlockIdx(pc: PrunedAddr, start: PrunedAddr): UInt = {
    val byteOffset = (pc - start).toUInt
    (byteOffset - instBytes.U)(log2Ceil(PredictWidth), instOffsetBits)
  }
}
