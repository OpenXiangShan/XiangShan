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
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.TargetFixHelper

trait Helpers extends HasPrefetchBtbParameters
    with HasXSParameter with TargetFixHelper with HalfAlignHelper {

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
      ("cfiPosition", instOffsetBits, log2Ceil(FetchBlockInstNum))
    )
  )
  def getTargetUpper(pc: PrunedAddr): UInt =
    pc(pc.length - 1, addrFields.getEnd("targetLower") + 1)
  def getSetIndex(pc: PrunedAddr): UInt =
    addrFields.extract("setIdx", pc)
  def getReplacerSetIndex(pc: PrunedAddr): UInt =
    addrFields.extract("replacerSetIdx", pc)
  def getBankIndex(pc: PrunedAddr): UInt =
    addrFields.extract("BankIdx", pc)

  def getTag(pc: PrunedAddr): UInt =
    addrFields.extract("tag", pc)
  def getBlockPcUpper(pc: PrunedAddr): UInt =
    pc(pc.length - 1, FetchBlockSizeWidth)
  def getAlignPcUpper(pc: PrunedAddr): UInt =
    pc(pc.length - 1, FetchBlockAlignWidth)
  def getPosition(pc: PrunedAddr): UInt =
    addrFields.extract("cfiPosition", pc)
  def getCfiAlignedPosition(pc: PrunedAddr): UInt = {
    val halfAlign = pc(FetchBlockSizeWidth - 1)
    val position  = addrFields.extract("position", pc)
    Cat(~halfAlign, position)
  }

  def getBlockPc(pc: PrunedAddr): PrunedAddr =
    PrunedAddrInit(Cat(
      getBlockPcUpper(pc),
      0.U(FetchBlockSizeWidth.W)
    ))
  def getAlignPc(pc: PrunedAddr): PrunedAddr =
    PrunedAddrInit(Cat(
      getAlignPcUpper(pc),
      0.U(FetchBlockAlignWidth.W)
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

  // TODO: add more detail decode and retime to s0
  def isValidInstr(inst: UInt): Bool = {

    val opcode = inst(6, 0)
    val f3     = inst(14, 12)

    val f7_reserved_zero = (inst(31) === 0.U) && (inst(29, 25) === 0.U)

    val shift64_reserved_zero = (inst(31) === 0.U) && (inst(29, 26) === 0.U)

    val bit30_valid = (inst(30) === 0.U) || (f3 === 0.U) || (f3 === 5.U)

    val shift_bit30_valid = (inst(30) === 0.U) || (f3 === 5.U)

    val rtype_f7_valid   = f7_reserved_zero && bit30_valid
    val shift64_f7_valid = shift64_reserved_zero && shift_bit30_valid
    val shift32_f7_valid = f7_reserved_zero && shift_bit30_valid

    val is_op32_f3 = (f3 === 0.U) || (f3 === 1.U) || (f3 === 5.U)

    val is_legal = MuxLookup(opcode, false.B)(Seq(
      "b0000011".U -> (f3 =/= 7.U),                                                            // LOAD
      "b0100011".U -> (f3 <= 3.U),                                                             // STORE
      "b0001111".U -> (f3 <= 1.U),                                                             // MISC-MEM
      "b0010011".U -> Mux(f3 === 1.U || f3 === 5.U, shift64_f7_valid, true.B),                 // OP-IMM
      "b0110011".U -> rtype_f7_valid,                                                          // OP
      "b0011011".U -> (is_op32_f3 && Mux(f3 === 1.U || f3 === 5.U, shift32_f7_valid, true.B)), // OP-IMM-32
      "b0111011".U -> (is_op32_f3 && rtype_f7_valid),                                          // OP-32
      "b1100011".U -> (f3(2, 1) =/= 1.U),                                                      // BRANCH
      "b1100111".U -> (f3 === 0.U),                                                            // JALR
      "b1101111".U -> true.B,                                                                  // JAL
      "b0010111".U -> true.B,                                                                  // AUIPC
      "b0110111".U -> true.B,                                                                  // LUI
      "b1110011".U -> true.B                                                                   // SYSTEM
    ))
    is_legal
  }
  def getBrOffset(inst: UInt, isRvc: Bool): PrunedAddr = {
    val rvcOffset = Cat(inst(12), inst(6, 5), inst(2), inst(11, 10), inst(4, 3), 0.U(1.W))
    val rviOffset = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W))
    val maxWidth  = rviOffset.getWidth
    PrunedAddrInit(SignExt(Mux(isRvc, SignExt(rvcOffset, maxWidth), SignExt(rviOffset, maxWidth)), 20))
  }
}
