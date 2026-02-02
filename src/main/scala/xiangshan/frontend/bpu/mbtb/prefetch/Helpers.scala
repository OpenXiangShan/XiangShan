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

package xiangshan.frontend.bpu.mbtb.prefetch

import chisel3._
import chisel3.util._
import utility.SignExt
import utils.AddrField
import xiangshan.HasXSParameter
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.TargetFixHelper

trait Helpers extends HasPrefetchBtbParameters
    with HasXSParameter with TargetFixHelper {

  val addrFields = AddrField(
    Seq(
      ("BlockOffset", FetchBlockSizeWidth),
      ("BankIdx", BankIdxLen),
      ("setIdx", SetIdxLen),
      ("tag", TagWidth)
    ),
    maxWidth = Option(VAddrBits),
    extraFields = Seq(
      ("replacerSetIdx", FetchBlockSizeWidth, SetIdxLen),
      ("targetLower", instOffsetBits, TargetWidth),
      ("position", instOffsetBits, FetchBlockAlignWidth),
      ("cfiPosition", instOffsetBits, FetchBlockSizeWidth)
    )
  )
  def getTargetUpper(pc: PrunedAddr): UInt =
    pc(pc.length - 1, addrFields.getEnd("targetLower") + 1)
  def getSetIndex(pc: PrunedAddr): UInt =
    addrFields.extract("setIdx", pc)

  def getBankIndex(pc: PrunedAddr): UInt =
    addrFields.extract("BankIdx", pc)

  def getTag(pc: PrunedAddr): UInt =
    addrFields.extract("tag", pc)
  def getBlockPcUpper(pc: PrunedAddr): UInt =
    pc(pc.length - 1, FetchBlockSizeWidth)

  def getPosition(pc: PrunedAddr): UInt =
    addrFields.extract("position", pc)

  def getBlockPc(pc: PrunedAddr): PrunedAddr =
    PrunedAddrInit(Cat(
      getBlockPcUpper(pc),
      0.U(FetchBlockSizeWidth.W)
    ))
  def getCfiPc(pc: PrunedAddr, pos: UInt): PrunedAddr =
    PrunedAddrInit(Cat(
      getBlockPcUpper(pc),
      pos,
      0.U
    ))
}
trait SBDHelper extends HasPrefetchBtbParameters {
  def isRVC(inst: UInt): Bool = inst(1, 0) =/= 3.U

  def getJalOffset(inst: UInt, isRvc: Bool): PrunedAddr = {
    val rvcOffset = Cat(inst(12), inst(8), inst(10, 9), inst(6), inst(7), inst(2), inst(11), inst(5, 3), 0.U(1.W))
    val rviOffset = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))
    val maxWidth  = rviOffset.getWidth
    PrunedAddrInit(SignExt(Mux(isRvc, SignExt(rvcOffset, maxWidth), SignExt(rviOffset, maxWidth)), 20))
  }
  def isValidInstr(inst: UInt): Bool = {
    val opcode      = inst(6, 0)
    val validOpcode = Seq(3.U, 15.U, 19.U, 23.U, 27.U, 35.U, 47.U, 51.U, 55.U, 59.U, 99.U, 103.U, 111.U, 115.U)
    val isValid     = Wire(Bool())
    isValid := validOpcode.map(_ === opcode).reduce(_ || _)
    isValid
  }
  def getBrOffset(inst: UInt, isRvc: Bool): PrunedAddr = {
    val rvcOffset = Cat(inst(12), inst(6, 5), inst(2), inst(11, 10), inst(4, 3), 0.U(1.W))
    val rviOffset = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W))
    val maxWidth  = rviOffset.getWidth
    PrunedAddrInit(SignExt(Mux(isRvc, SignExt(rvcOffset, maxWidth), SignExt(rviOffset, maxWidth)), 20))
  }
}
