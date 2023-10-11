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

package xiangshan.backend.decode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.Instructions
import utility._
import utils._
import xiangshan._
import xiangshan.backend.fu.FuType
import xiangshan.backend.Bundles.DecodedInst
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields

abstract class BaseFusionCase(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends DecodeUnitConstants {
  require(pair.length == 2)

  protected val inst1: XSInstBitFields = instr(0).asTypeOf(new XSInstBitFields)
  protected val inst2: XSInstBitFields = instr(1).asTypeOf(new XSInstBitFields)

  protected def instr: Seq[UInt] = pair.map(_.bits)
  protected def pairValid: Bool = VecInit(pair.map(_.valid)).asUInt.andR
  protected def instr1Rs1: UInt = inst1.RS1
  protected def instr1Rs2: UInt = inst1.RS2
  protected def instr1Rd: UInt = inst1.RD
  def instr2Rs1: UInt = inst2.RS1
  def instr2Rs2: UInt = inst2.RS2
  protected def instr2Rd: UInt = inst2.RD
  protected def withSameDest: Bool = instr1Rd === instr2Rd
  def destToRs1: Bool = instr1Rd === instr2Rs1
  protected def destToRs2: Bool = instr1Rd === instr2Rs2

  protected def getInstrTable(pat: BitPat): List[BitPat] = {
    // Only these instructions can be fused now
    val allDecodeTable = XDecode.table ++ X64Decode.table ++ BDecode.table
    allDecodeTable.filter(_._1 == pat).map(_._2).head
  }
  // Must sync these indices with MicroOp.decode
  protected def getInstrFuType(pat: BitPat): BitPat = getInstrTable(pat)(3)
  protected def getInstrFuOpType(pat: BitPat): BitPat = getInstrTable(pat)(4)
  protected def getInstrSrc1Type(pat: BitPat): BitPat = getInstrTable(pat)(0)
  protected def getInstrSrc2Type(pat: BitPat): BitPat = getInstrTable(pat)(1)

  def isValid: Bool
  // To optimize the timing, only these control signals can be affected by instruction fusion.
  def thisInstr: Option[BitPat] = None
  def fusedInstr: Option[BitPat] = None
  // By default, None means unchanged.
  private def compareAndGet(func: BitPat => BitPat): Option[Int] = {
    if (fusedInstr.isDefined) {
      require(thisInstr.isDefined, "thisInstr must be defined to infer the ctrl signals")
      val fused = func(fusedInstr.get)
      // Only when the two instructions have different control field, we make it not None.
      if (fused != func(thisInstr.get)) Some(fused.value.toInt) else None
    } else None
  }
  // We assume only fuType, fuOpType, lsrc2 may be changed now.
  def fuType: Option[Int] = compareAndGet(getInstrFuType)
  def fuOpType: Option[UInt => UInt] = {
    val t = compareAndGet(getInstrFuOpType)
    if (t.isDefined) Some((_: UInt) => t.get.U) else None
  }
  def src2Type: Option[Int] = compareAndGet(getInstrSrc2Type)
  def selImm: Option[UInt] = None
  def lsrc2NeedZero: Boolean = false
  def lsrc2NeedMux: Boolean = false
  def lsrc2MuxResult: UInt = Mux(destToRs1, instr2Rs2, instr2Rs1)

  def fusionName: String
}

// There are 2 types of instruction fusion.
// (1) fused into a fixed new instruction with new control signals.
// (2) the first instruction's op is replaced with another one.

// Case: clear upper 32 bits / get lower 32 bits
// Source: `slli r1, r0, 32` + `srli r1, r1, 32`
// Target: `add.uw r1, r0, zero` (pseudo instruction: `zext.w r1, r0`)
class FusedAdduw(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && inst1.SHAMT6 === 32.U
  def inst2Cond = instr(1) === Instructions.SRLI && inst2.SHAMT6 === 32.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  override def thisInstr: Option[BitPat] = Some(Instructions.SLLI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.ADD_UW)
  override def lsrc2NeedZero: Boolean = true

  def fusionName: String = "slli32_srli32"
}

// Case: clear upper 48 bits / get lower 16 bits
// Source: `slli r1, r0, 48` + `srli r1, r1, 48`
// Target: `zext.h r1, r0`
class FusedZexth(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && inst1.SHAMT6 === 48.U
  def inst2Cond = instr(1) === Instructions.SRLI && inst2.SHAMT6 === 48.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  override def thisInstr: Option[BitPat] = Some(Instructions.SLLI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.PACKW)
  override def lsrc2NeedZero: Boolean = true

  def fusionName: String = "slli48_srli48"
}

// Another case of Zext.h
// Source: `slliw r1, r0, 16` + `srliw r1, r1, 16`
// Target: `zext.h r1, r0`
class FusedZexth1(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends FusedZexth(pair) {
  override def inst1Cond: Bool = instr(0) === Instructions.SLLIW && inst1.SHAMT5 === 16.U
  override def inst2Cond: Bool = instr(1) === Instructions.SRLIW && inst2.SHAMT5 === 16.U

  override def thisInstr: Option[BitPat] = Some(Instructions.SLLIW)

  override def fusionName: String = "slliw16_srliw16"
}

// Case: sign-extend a 16-bit number
// Source: `slliw r1, r0, 16` + `sraiw r1, r1, 16`
// Target: `sext.h r1, r0`
class FusedSexth(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLIW && inst1.SHAMT5 === 16.U
  def inst2Cond = instr(1) === Instructions.SRAIW && inst2.SHAMT5 === 16.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  override def thisInstr: Option[BitPat] = Some(Instructions.SLLIW)
  override def fusedInstr: Option[BitPat] = Some(Instructions.SEXT_H)
  override def lsrc2NeedZero: Boolean = true

  def fusionName: String = "slliw16_sraiw16"
}

// Case: shift left by one and add
// Source: `slli r1, r0, 1` + `add r1, r1, r2`
// Target: `sh1add r1, r0, r2`
class FusedSh1add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && inst1.SHAMT6 === 1.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.SLLI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.SH1ADD)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "slli1_add"
}

// Case: shift left by two and add
// Source: `slli r1, r0, 2` + `add r1, r1, r2`
// Target: `sh2add r1, r0, r2`
class FusedSh2add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && inst1.SHAMT6 === 2.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.SLLI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.SH2ADD)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "slli2_add"
}

// Case: shift left by three and add
// Source: `slli r1, r0, 3` + `add r1, r1, r2`
// Target: `sh3add r1, r0, r2`
class FusedSh3add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && inst1.SHAMT6 === 3.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.SLLI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.SH3ADD)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "slli3_add"
}

// Case: shift zero-extended word left by one
// Source: `slli r1, r0, 32` + `srli r1, r1, 31`
// Target: `szewl1 r1, r0` (customized internal opcode)
class FusedSzewl1(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && inst1.SHAMT6 === 32.U
  def inst2Cond = instr(1) === Instructions.SRLI && inst2.SHAMT6 === 31.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.szewl1)

  def fusionName: String = "slli32_srli31"
}

// Case: shift zero-extended word left by two
// Source: `slli r1, r0, 32` + `srli r1, r1, 30`
// Target: `szewl2 r1, r0` (customized internal opcode)
class FusedSzewl2(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && inst1.SHAMT6 === 32.U
  def inst2Cond = instr(1) === Instructions.SRLI && inst2.SHAMT6 === 30.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.szewl2)

  def fusionName: String = "slli32_srli30"
}

// Case: shift zero-extended word left by three
// Source: `slli r1, r0, 32` + `srli r1, r1, 29`
// Target: `szewl3 r1, r0` (customized internal opcode)
class FusedSzewl3(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && inst1.SHAMT6 === 32.U
  def inst2Cond = instr(1) === Instructions.SRLI && inst2.SHAMT6 === 29.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.szewl3)

  def fusionName: String = "slli32_srli29"
}

// Case: get the second byte
// Source: `srli r1, r0, 8` + `andi r1, r1, 255`
// Target: `byte2 r1, r0` (customized internal opcode)
class FusedByte2(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SRLI && inst1.SHAMT6 === 8.U
  def inst2Cond = instr(1) === Instructions.ANDI && inst2.IMM12 === 255.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.byte2)

  def fusionName: String = "srli8_andi255"
}

// Case: shift left by four and add
// Source: `slli r1, r0, 4` + `add r1, r1, r2`
// Target: `sh4add r1, r0, r2` (customized internal opcode)
class FusedSh4add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && inst1.SHAMT6 === 4.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.SLLI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.ADD)
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.sh4add)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "slli4_add"
}

// Case: shift right by 29 and add
// Source: `srli r1, r0, 29` + `add r1, r1, r2`
// Target: `sr29add r1, r0, r2` (customized internal opcode)
class FusedSr29add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SRLI && inst1.SHAMT6 === 29.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.SRLI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.ADD)
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.sr29add)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "srli29_add"
}

// Case: shift right by 30 and add
// Source: `srli r1, r0, 30` + `add r1, r1, r2`
// Target: `sr30add r1, r0, r2` (customized internal opcode)
class FusedSr30add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SRLI && inst1.SHAMT6 === 30.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.SRLI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.ADD)
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.sr30add)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "srli30_add"
}

// Case: shift right by 31 and add
// Source: `srli r1, r0, 31` + `add r1, r1, r2`
// Target: `sr31add r1, r0, r2` (customized internal opcode)
class FusedSr31add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SRLI && inst1.SHAMT6 === 31.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.SRLI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.ADD)
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.sr31add)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "srli31_add"
}

// Case: shift right by 32 and add
// Source: `srli r1, r0, 32` + `add r1, r1, r2`
// Target: `sr32add r1, r0, r2` (customized internal opcode)
class FusedSr32add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SRLI && inst1.SHAMT6 === 32.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.SRLI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.ADD)
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.sr32add)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "srli32_add"
}

// Case: add one if odd, otherwise unchanged
// Source: `andi r1, r0, 1`` + `add r1, r1, r2`
// Target: `oddadd r1, r0, r2` (customized internal opcode)
class FusedOddadd(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.ANDI && inst1.IMM12 === 1.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.ANDI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.ADD)
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.oddadd)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "andi1_add"
}

// Case: add one if odd (in word format), otherwise unchanged
// Source: `andi r1, r0, 1`` + `addw r1, r1, r2`
// Target: `oddaddw r1, r0, r2` (customized internal opcode)
class FusedOddaddw(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.ANDI && inst1.IMM12 === 1.U
  def inst2Cond = instr(1) === Instructions.ADDW

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.ANDI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.ADD)
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.oddaddw)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "andi1_addw"
}

// Case: addw and extract its lower 8 bits (fused into addwbyte)
class FusedAddwbyte(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends BaseFusionCase(pair) {
  // the first instruction is a ALUOpType.addw
  // According to DecodeUnit.scala, only ADDIW and ADDW are ALUOpType.addw, which are used for inst1Cond.
  def inst1Cond = instr(0) === Instructions.ADDIW || instr(0) === Instructions.ADDW
  def inst2Cond = instr(1) === Instructions.ANDI && inst2.IMM12 === 0xff.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.addwbyte)

  def fusionName: String = "addw_andi255"
}

// Case: addw and extract its lower 1 bit (fused into addwbit)
class FusedAddwbit(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends FusedAddwbyte(pair) {

  override def inst2Cond = instr(1) === Instructions.ANDI && inst2.IMM12 === 0x1.U

  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.addwbit)

  override def fusionName: String = "addw_andi1"
}

// Case: addw and zext.h (fused into addwzexth)
class FusedAddwzexth(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends FusedAddwbyte(pair) {

  override def inst2Cond = instr(1) === Instructions.ZEXT_H

  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.addwzexth)

  override def fusionName: String = "addw_zexth"
}

// Case: addw and sext.h (fused into addwsexth)
class FusedAddwsexth(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends FusedAddwbyte(pair) {

  override def inst2Cond = instr(1) === Instructions.SEXT_H

  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.addwsexth)

  override def fusionName: String = "addw_sexth"
}

// Case: logic operation and extract its LSB

class FusedLogiclsb(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends BaseFusionCase(pair) {
  // the first instruction is a logic (and, or, xor, orcb)
  // (1) def ANDI               = BitPat("b?????????????????111?????0010011")
  // (2) def AND                = BitPat("b0000000??????????111?????0110011")
  // (3) def ORI                = BitPat("b?????????????????110?????0010011")
  // (4) def OR                 = BitPat("b0000000??????????110?????0110011")
  // (5) def XORI               = BitPat("b?????????????????100?????0010011")
  // (6) def XOR                = BitPat("b0000000??????????100?????0110011")
  // (7) def ORC_B              = BitPat("b001010000111?????101?????0010011")
  val logicInstrList = Seq(Instructions.ANDI, Instructions.AND, Instructions.ORI, Instructions.OR,
    Instructions.XORI, Instructions.XOR, Instructions.ORC_B)
  def inst1Cond = VecInit(logicInstrList.map(_ === instr(0))).asUInt.orR
  def inst2Cond = instr(1) === Instructions.ANDI && inst2.IMM12 === 1.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  override def fuOpType: Option[UInt => UInt] = Some(ALUOpType.logicToLsb)

  def fusionName: String = "logic_andi1"
}

class FusedLogicZexth(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends FusedLogiclsb(pair) {

  override def inst2Cond = instr(1) === Instructions.ZEXT_H
  override def fuOpType: Option[UInt => UInt] = Some(ALUOpType.logicToZexth)

  override def fusionName: String = "logic_zexth"
}

// Case: OR(Cat(src1(63, 8), 0.U(8.W)), src2)
// Source: `andi r1, r0, -256`` + `or r1, r1, r2`
class FusedOrh48(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.ANDI && inst1.IMM12 === 0xf00.U
  def inst2Cond = instr(1) === Instructions.OR

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.ANDI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.OR)
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.orh48)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "andi_f00_or"
}

// Case: mul 7bit data with 32-bit data
// Source: `andi r1, r0, 127`` + `mulw r1, r1, r2`
// Target: `mulw7 r1, r0, r2`
class FusedMulw7(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.ANDI && inst1.IMM12 === 127.U
  def inst2Cond = instr(1) === Instructions.MULW

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  override def thisInstr: Option[BitPat] = Some(Instructions.ANDI)
  override def fusedInstr: Option[BitPat] = Some(Instructions.MULW)
  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => MDUOpType.mulw7)
  override def lsrc2NeedMux: Boolean = true

  def fusionName: String = "andi127_mulw"
}

// Case: get 32 bits imm
// Source: `lui r1, 0xffffa`` + `addi r1, r1, 1`
// Target: `lui32 r1, 0xffffa001` (customized internal opcode)
class FusedLui32(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.LUI
  def inst2Cond = instr(1) === Instructions.ADDI

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1

  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.lui32add)
  override def selImm: Option[UInt] = Some(SelImm.IMM_LUI32)

  def fusionName: String = "lui_addi"

  XSDebug(isValid, p"[fusedLui32] instr0=${Hexadecimal(instr(0))} instr1=${Hexadecimal(instr(1))}\n")
}

// Case: get 32 bits imm (in word format)
// Source: `lui r1, 0xffffa`` + `addiw r1, r1, 1`
// Target: `lui32 r1, 0xffffa001` (customized internal opcode)
class FusedLui32w(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.LUI
  def inst2Cond = instr(1) === Instructions.ADDIW

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1

  override def fuOpType: Option[UInt => UInt] = Some((_: UInt) => ALUOpType.lui32addw)
  override def selImm: Option[UInt] = Some(SelImm.IMM_LUI32)

  def fusionName: String = "lui_addiw"

  XSDebug(isValid, p"[fusedLui32w] instr0=${Hexadecimal(instr(0))} instr1=${Hexadecimal(instr(1))}\n")
}

class FusionDecodeInfo extends Bundle {
  val rs2FromRs1 = Output(Bool())
  val rs2FromRs2 = Output(Bool())
  val rs2FromZero = Output(Bool())
}

class FusionDecodeReplace extends Bundle {
  val fuType = Valid(FuType())
  val fuOpType = Valid(FuOpType())
  val lsrc2 = Valid(UInt(6.W))
  val src2Type = Valid(SrcType())
  val selImm = Valid(SelImm())

  def update(cs: DecodedInst): Unit = {
    when (fuType.valid) {
      cs.fuType := fuType.bits
    }
    when (fuOpType.valid) {
      cs.fuOpType := fuOpType.bits
    }
    when (lsrc2.valid) {
      cs.lsrc(1) := lsrc2.bits
    }
    when (src2Type.valid) {
      cs.srcType(1) := src2Type.bits
    }
    when (selImm.valid) {
      cs.selImm := selImm.bits
    }
  }
}

class FusionDecoder(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // T0: detect instruction fusions in these instructions
    val in = Vec(DecodeWidth, Flipped(ValidIO(UInt(32.W))))
    val inReady = Vec(DecodeWidth - 1, Input(Bool())) // dropRight(1)
    // T1: decode result
    val dec = Vec(DecodeWidth - 1, Input(new DecodedInst)) // dropRight(1)
    // T1: whether an instruction fusion is found
    val out = Vec(DecodeWidth - 1, ValidIO(new FusionDecodeReplace)) // dropRight(1)
    val info = Vec(DecodeWidth - 1, new FusionDecodeInfo) // dropRight(1)
    // T1: fused instruction needs to be cleared
    val clear = Vec(DecodeWidth, Output(Bool()))
  })

  io.clear.head := false.B

  val instrPairs = io.in.dropRight(1).zip(io.in.drop(1)).map(x => Seq(x._1, x._2))
  instrPairs.zip(io.dec).zip(io.out).zipWithIndex.foreach{ case (((pair, dec), out), i) =>
    val fusionList = Seq(
      new FusedAdduw(pair),
      new FusedZexth(pair),
      new FusedZexth1(pair),
      new FusedSexth(pair),
      new FusedSh1add(pair),
      new FusedSh2add(pair),
      new FusedSh3add(pair),
      new FusedSzewl1(pair),
      new FusedSzewl2(pair),
      new FusedSzewl3(pair),
      new FusedByte2(pair),
      new FusedSh4add(pair),
      new FusedSr29add(pair),
      new FusedSr30add(pair),
      new FusedSr31add(pair),
      new FusedSr32add(pair),
      new FusedOddadd(pair),
      new FusedOddaddw(pair),
      new FusedOrh48(pair),
      new FusedMulw7(pair),
      new FusedAddwbyte(pair),
      new FusedAddwbit(pair),
      new FusedAddwzexth(pair),
      new FusedAddwsexth(pair),
      new FusedLogiclsb(pair),
      new FusedLogicZexth(pair),
      new FusedLui32(pair),
      new FusedLui32w(pair)
    )
    val fire = io.in(i).valid && io.inReady(i)
    val instrPairValid = RegEnable(VecInit(pair.map(_.valid)).asUInt.andR, false.B, io.inReady(i))
    val fusionVec = RegEnable(VecInit(fusionList.map(_.isValid)), fire)
    val thisCleared = io.clear(i)
    out.valid := instrPairValid && !thisCleared && fusionVec.asUInt.orR
    XSError(instrPairValid && PopCount(fusionVec) > 1.U, "more then one fusion matched\n")
    def connectByInt(field: FusionDecodeReplace => Valid[UInt], replace: Seq[Option[Int]]): Unit = {
      field(out.bits).valid := false.B
      field(out.bits).bits := DontCare
      val replaceVec = fusionVec.zip(replace).filter(_._2.isDefined)
      if (replaceVec.nonEmpty) {
        // constant values are grouped together for better timing.
        val replEnable = VecInit(replaceVec.map(_._1)).asUInt.orR
        val replTypes = replaceVec.map(_._2.get).distinct
        val replSel = replTypes.map(t => VecInit(replaceVec.filter(_._2.get == t).map(_._1)).asUInt.orR)
        field(out.bits).valid := replEnable
        field(out.bits).bits := Mux1H(replSel, replTypes.map(_.U))
      }
    }
    def connectByUInt(field: FusionDecodeReplace => Valid[UInt], replace: Seq[Option[UInt]], needReg: Boolean): Unit = {
      field(out.bits).valid := false.B
      field(out.bits).bits := DontCare
      val replaceVec = if (needReg) fusionVec.zip(replace).filter(_._2.isDefined).map(x => (x._1, RegEnable(x._2.get, fire))) else fusionVec.zip(replace).filter(_._2.isDefined).map(x => (x._1, x._2.get))
      if (replaceVec.nonEmpty) {
        val replEnable = VecInit(replaceVec.map(_._1)).asUInt.orR
        val replTypes = replaceVec.map(_._2).distinct
        val replSel = replTypes.map(t => VecInit(replaceVec.filter(_._2 == t).map(_._1)).asUInt.orR)
        field(out.bits).valid := replEnable
        field(out.bits).bits := Mux1H(replSel, replTypes)
      }
    }
    def connectByUIntFunc(
      field: FusionDecodeReplace => Valid[UInt],
      csField: DecodedInst => UInt,
      replace: Seq[Option[UInt => UInt]]
    ): Unit = {
      field(out.bits).valid := false.B
      field(out.bits).bits := DontCare
      val replaceVec = fusionVec.zip(replace).filter(_._2.isDefined).map(x => (x._1, x._2.get(csField(dec))))
      if (replaceVec.nonEmpty) {
        val replEnable = VecInit(replaceVec.map(_._1)).asUInt.orR
        // constant values are grouped together for better timing.
        val constReplVec = replaceVec.filter(_._2.isLit).map(x => (x._1, x._2.litValue))
        val constReplTypes = constReplVec.map(_._2).distinct
        val constReplEnable = constReplTypes.map(t => VecInit(constReplVec.filter(_._2 == t).map(_._1)).asUInt.orR)
        val constReplResult = Mux1H(constReplEnable, constReplTypes.map(_.U))
        // non-constant values have to be processed naively.
        val noLitRepl = replaceVec.filterNot(_._2.isLit)
        field(out.bits).valid := replEnable
        field(out.bits).bits := Mux(VecInit(noLitRepl.map(_._1)).asUInt.orR, Mux1H(noLitRepl), constReplResult)
      }
    }
    connectByInt((x: FusionDecodeReplace) => x.fuType, fusionList.map(_.fuType))
    connectByUIntFunc((x: FusionDecodeReplace) => x.fuOpType, (x: DecodedInst) => x.fuOpType, fusionList.map(_.fuOpType))
    connectByInt((x: FusionDecodeReplace) => x.src2Type, fusionList.map(_.src2Type))
    connectByUInt((x: FusionDecodeReplace) => x.selImm, fusionList.map(_.selImm), false)

    val src2WithZero = VecInit(fusionVec.zip(fusionList.map(_.lsrc2NeedZero)).filter(_._2).map(_._1)).asUInt.orR
    val src2WithMux = VecInit(fusionVec.zip(fusionList.map(_.lsrc2NeedMux)).filter(_._2).map(_._1)).asUInt.orR
    io.info(i).rs2FromZero := src2WithZero
    io.info(i).rs2FromRs1 := src2WithMux && !RegEnable(fusionList.head.destToRs1, fire)
    io.info(i).rs2FromRs2 := src2WithMux && RegEnable(fusionList.head.destToRs1, fire)
    out.bits.lsrc2.valid := src2WithMux || src2WithZero
    when (src2WithMux) {
      out.bits.lsrc2.bits := RegEnable(fusionList.head.lsrc2MuxResult, fire)
    }.otherwise {//elsewhen (src2WithZero) {
      out.bits.lsrc2.bits := 0.U
    }
    // TODO: assume every instruction fusion clears the second instruction now
    io.clear(i + 1) := out.valid
    val lastFire = RegNext(fire)
    fusionList.zip(fusionVec).foreach { case (f, v) =>
      XSPerfAccumulate(s"case_${f.fusionName}_$i", instrPairValid && !thisCleared && v && lastFire)
    }
    XSPerfAccumulate(s"conflict_fusion_$i", instrPairValid && thisCleared && fusionVec.asUInt.orR && lastFire)

    XSDebug(out.valid, p"[fusion] valid ${i}, outvalid: ${out.bits.fuType.valid} ${out.bits.fuOpType.valid} ${out.bits.src2Type.valid} ${out.bits.lsrc2.valid} ${out.bits.selImm.valid}\n")
    XSDebug(out.valid, p"[fusion] valid ${i}, outbits: ${out.bits.fuType.bits} ${out.bits.fuOpType.bits} ${out.bits.src2Type.bits} ${out.bits.lsrc2.bits} ${out.bits.selImm.bits}\n")
  }

  XSPerfAccumulate("fused_instr", PopCount(io.out.zipWithIndex.map{ case (x, i) => x.valid && RegNext(io.in(i).valid && io.inReady(i)) }))
}
