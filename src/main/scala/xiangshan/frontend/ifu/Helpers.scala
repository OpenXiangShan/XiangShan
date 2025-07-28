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
import xiangshan.frontend.icache.HasICacheParameters

trait PreDecodeHelper extends HasXSParameter {
  def isRVC(inst: UInt): Bool = inst(1, 0) =/= 3.U

  def isLink(reg: UInt): Bool = reg === 1.U || reg === 5.U

  def getBrType(inst: UInt): UInt =
    ListLookup(inst, List(BrType.NotCfi), PreDecodeInst.brTable).head

  def getBrInfo(inst: UInt): (UInt, Bool, Bool) = {
    val brType = getBrType(inst)
    val rd     = Mux(isRVC(inst), inst(12), inst(11, 7))
    val rs     = Mux(isRVC(inst), Mux(brType === BrType.Jal, 0.U, inst(11, 7)), inst(19, 15))
    val isCall = (brType === BrType.Jal && !isRVC(inst) || brType === BrType.Jalr) && isLink(rd) // Only for RV64
    val isRet  = brType === BrType.Jalr && isLink(rs) && !isCall
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

trait FetchBlockHelper extends HasXSParameter with HasICacheParameters {
  def getBasicBlockIdx(pc: PrunedAddr, start: PrunedAddr): UInt = {
    val byteOffset = (pc - start).toUInt
    (byteOffset - instBytes.U)(log2Ceil(PredictWidth), instOffsetBits)
  }

  def isNextLine(pc: PrunedAddr, startAddr: PrunedAddr): Bool =
    startAddr(blockOffBits) ^ pc(blockOffBits)

  def isLastInLine(pc: PrunedAddr): Bool =
    pc(blockOffBits - 1, 0) === "b111110".U
}

trait IfuHelper extends HasXSParameter with HasIfuParameters {
  def catPC(low: UInt, high: UInt, high1: UInt): PrunedAddr =
    PrunedAddrInit(Mux(
      low(PcCutPoint),
      Cat(high1, low(PcCutPoint - 1, 0)),
      Cat(high, low(PcCutPoint - 1, 0))
    ))

  def catPC(lowVec: Vec[UInt], high: UInt, high1: UInt): Vec[PrunedAddr] =
    VecInit(lowVec.map(catPC(_, high, high1)))

  def cutICacheData(cacheline: UInt): Vec[UInt] = {
    // FIXME: !HasCExtension
    require(HasCExtension)
    val result  = Wire(Vec(ICacheLineBytes / 2, UInt(32.W)))
    val dataVec = cacheline.asTypeOf(Vec(ICacheLineBytes / 2, UInt(16.W)))
    (0 until ICacheLineBytes / 2 - 1).foreach(i =>
      result(i) := Cat(dataVec(i + 1), dataVec(i))
    )
    result(ICacheLineBytes / 2 - 1) := Cat(dataVec(0), dataVec(ICacheLineBytes / 2 - 1))
    result
  }

  def alignData[T <: Data](indataVec: Vec[T], shiftNum: UInt, prevIsHalf: Bool, default: T): Vec[T] = {
    require(shiftNum.getWidth == 2)
    val dataVec = VecInit((0 until IBufEnqWidth).map(i =>
      if (i < indataVec.length) indataVec(i) else 0.U.asTypeOf(default)
    ))
    val out = WireDefault(VecInit.fill(IBufEnqWidth)(0.U.asTypeOf(default)))
    for (i <- 0 until IBufEnqWidth) {
      out(i) := MuxLookup(shiftNum, 0.U.asTypeOf(default))(Seq(
        0.U -> Mux(prevIsHalf, if (i == IBufEnqWidth - 1) 0.U.asTypeOf(default) else dataVec(i + 1), dataVec(i)),
        1.U -> Mux(prevIsHalf, dataVec(i), if (i == 0) 0.U.asTypeOf(default) else dataVec(i - 1)),
        2.U -> Mux(
          prevIsHalf,
          if (i < 1) 0.U.asTypeOf(default) else dataVec(i - 1),
          if (i < 2) 0.U.asTypeOf(default) else dataVec(i - 2)
        ),
        3.U -> Mux(
          prevIsHalf,
          if (i < 2) 0.U.asTypeOf(default) else dataVec(i - 2),
          if (i < 3) 0.U.asTypeOf(default) else dataVec(i - 3)
        )
      ))
    }
    out
  }

}
