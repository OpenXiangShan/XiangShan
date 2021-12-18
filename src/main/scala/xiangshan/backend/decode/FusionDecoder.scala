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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import xiangshan._
import utils._

abstract class BaseFusionCase(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends DecodeUnitConstants {
  require(pair.length == 2)

  protected def instr: Seq[UInt] = pair.map(_.bits)
  protected def pairValid: Bool = VecInit(pair.map(_.valid)).asUInt().andR()
  protected def instr1Rs1: UInt = instr(0)(RS1_MSB, RS1_LSB)
  protected def instr1Rs2: UInt = instr(0)(RS2_MSB, RS2_LSB)
  protected def instr1Rd: UInt = instr(0)(RD_MSB, RD_LSB)
  protected def instr2Rs1: UInt = instr(1)(RS1_MSB, RS1_LSB)
  protected def instr2Rs2: UInt = instr(1)(RS2_MSB, RS2_LSB)
  protected def instr2Rd: UInt = instr(1)(RD_MSB, RD_LSB)
  protected def withSameDest: Bool = instr1Rd === instr2Rd
  protected def destToRs1: Bool = instr1Rd === instr2Rs1
  protected def destToRs2: Bool = instr1Rd === instr2Rs2

  protected def getBaseCS(pat: BitPat): CtrlSignals = {
    val allDecodeTable = XDecode.table ++ X64Decode.table ++ BDecode.table
    val baseTable = allDecodeTable.filter(_._1 == pat).map(_._2).head
    val cs = Wire(new CtrlSignals)
    cs := DontCare
    cs.decode(baseTable)
    // For simple instruction fusions, we assume their destination registers are the same.
    cs.ldest := instr1Rd
    cs
  }

  def isValid: Bool
  // TODO: optimize timing
  def target: CtrlSignals
  // clear the next instruction
  // def needClear: Boolean = true
  def fusionName: String
}

// Case: clear upper 32 bits / get lower 32 bits
// Source: `slli r1, r0, 32` + `srli r1, r1, 32`
// Target: `add.uw r1, r0, zero` (pseudo instruction: `zext.w r1, r0`)
class FusedAdduw(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && instr(0)(25, 20) === 32.U
  def inst2Cond = instr(1) === Instructions.SRLI && instr(1)(25, 20) === 32.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.ADDU_W)
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := 0.U
    cs
  }

  def fusionName: String = "slli32_srli32"
}

// Case: clear upper 48 bits / get lower 16 bits
// Source: `slli r1, r0, 48` + `srli r1, r1, 48`
// Target: `zext.h r1, r0`
class FusedZexth(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && instr(0)(25, 20) === 48.U
  def inst2Cond = instr(1) === Instructions.SRLI && instr(1)(25, 20) === 48.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.PACKW)
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := 0.U
    cs
  }

  def fusionName: String = "slli48_srli48"
}

// Another case of Zext.h
// Source: `slliw r1, r0, 16` + `srliw r1, r1, 16`
// Target: `zext.h r1, r0`
class FusedZexth1(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends FusedZexth(pair) {
  override def inst1Cond: Bool = instr(0) === Instructions.SLLIW && instr(0)(24, 20) === 16.U
  override def inst2Cond: Bool = instr(1) === Instructions.SRLIW && instr(1)(24, 20) === 16.U

  override def fusionName: String = "slliw16_srliw16"
}

// Case: sign-extend a 16-bit number
// Source: `slliw r1, r0, 16` + `sraiw r1, r1, 16`
// Target: `sext.h r1, r0`
class FusedSexth(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLIW && instr(0)(24, 20) === 16.U
  def inst2Cond = instr(1) === Instructions.SRAIW && instr(1)(24, 20) === 16.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SEXT_H)
    cs.lsrc(0) := instr1Rs1
    cs
  }

  def fusionName: String = "slliw16_sraiw16"
}

// Case: shift left by one and add
// Source: `slli r1, r0, 1` + `add r1, r1, r2`
// Target: `sh1add r1, r0, r2`
class FusedSh1add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && instr(0)(25, 20) === 1.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SH1ADD)
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "slli1_add"
}

// Case: shift left by two and add
// Source: `slli r1, r0, 2` + `add r1, r1, r2`
// Target: `sh2add r1, r0, r2`
class FusedSh2add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && instr(0)(25, 20) === 2.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SH2ADD)
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "slli2_add"
}

// Case: shift left by three and add
// Source: `slli r1, r0, 3` + `add r1, r1, r2`
// Target: `sh3add r1, r0, r2`
class FusedSh3add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && instr(0)(25, 20) === 3.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SH3ADD)
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "slli3_add"
}

// Case: shift zero-extended word left by one
// Source: `slli r1, r0, 32` + `srli r1, r0, 31`
// Target: `szewl1 r1, r0` (customized internal opcode)
class FusedSzewl1(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && instr(0)(25, 20) === 32.U
  def inst2Cond = instr(1) === Instructions.SRLI && instr(1)(25, 20) === 31.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SEXT_H)
    // replace the fuOpType with szewl1
    cs.fuOpType := ALUOpType.szewl1
    cs.lsrc(0) := instr1Rs1
    cs
  }

  def fusionName: String = "slli32_srli31"
}

// Case: shift zero-extended word left by two
// Source: `slli r1, r0, 32` + `srli r1, r0, 30`
// Target: `szewl2 r1, r0` (customized internal opcode)
class FusedSzewl2(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && instr(0)(25, 20) === 32.U
  def inst2Cond = instr(1) === Instructions.SRLI && instr(1)(25, 20) === 30.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SEXT_H)
    // replace the fuOpType with szewl2
    cs.fuOpType := ALUOpType.szewl2
    cs.lsrc(0) := instr1Rs1
    cs
  }

  def fusionName: String = "slli32_srli30"
}

// Case: shift zero-extended word left by three
// Source: `slli r1, r0, 32` + `srli r1, r0, 29`
// Target: `szewl3 r1, r0` (customized internal opcode)
class FusedSzewl3(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && instr(0)(25, 20) === 32.U
  def inst2Cond = instr(1) === Instructions.SRLI && instr(1)(25, 20) === 29.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SEXT_H)
    // replace the fuOpType with szewl3
    cs.fuOpType := ALUOpType.szewl3
    cs.lsrc(0) := instr1Rs1
    cs
  }

  def fusionName: String = "slli32_srli29"
}

// Case: get the second byte
// Source: `srli r1, r0, 8` + `andi r1, r1, 255`
// Target: `byte2 r1, r0` (customized internal opcode)
class FusedByte2(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SRLI && instr(0)(25, 20) === 8.U
  def inst2Cond = instr(1) === Instructions.ANDI && instr(1)(31, 20) === 255.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SEXT_H)
    // replace the fuOpType with byte2
    cs.fuOpType := ALUOpType.byte2
    cs.lsrc(0) := instr1Rs1
    cs
  }

  def fusionName: String = "srli8_andi255"
}

// Case: shift left by four and add
// Source: `slli r1, r0, 4` + `add r1, r1, r2`
// Target: `sh4add r1, r0, r2` (customized internal opcode)
class FusedSh4add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SLLI && instr(0)(25, 20) === 4.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SH3ADD)
    // replace the fuOpType with sh4add
    cs.fuOpType := ALUOpType.sh4add
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "slli4_add"
}

// Case: shift right by 29 and add
// Source: `srli r1, r0, 29` + `add r1, r1, r2`
// Target: `sr29add r1, r0, r2` (customized internal opcode)
class FusedSr29add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SRLI && instr(0)(25, 20) === 29.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SH3ADD)
    // replace the fuOpType with sr29add
    cs.fuOpType := ALUOpType.sr29add
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "srli29_add"
}

// Case: shift right by 30 and add
// Source: `srli r1, r0, 30` + `add r1, r1, r2`
// Target: `sr30add r1, r0, r2` (customized internal opcode)
class FusedSr30add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SRLI && instr(0)(25, 20) === 30.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SH3ADD)
    // replace the fuOpType with sr30add
    cs.fuOpType := ALUOpType.sr30add
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "srli30_add"
}

// Case: shift right by 31 and add
// Source: `srli r1, r0, 31` + `add r1, r1, r2`
// Target: `sr31add r1, r0, r2` (customized internal opcode)
class FusedSr31add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SRLI && instr(0)(25, 20) === 31.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SH3ADD)
    // replace the fuOpType with sr31add
    cs.fuOpType := ALUOpType.sr31add
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "srli31_add"
}

// Case: shift right by 32 and add
// Source: `srli r1, r0, 32` + `add r1, r1, r2`
// Target: `sr32add r1, r0, r2` (customized internal opcode)
class FusedSr32add(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.SRLI && instr(0)(25, 20) === 32.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SH3ADD)
    // replace the fuOpType with sr32add
    cs.fuOpType := ALUOpType.sr32add
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "srli32_add"
}

// Case: add one if odd, otherwise unchanged
// Source: `andi r1, r0, 1`` + `add r1, r1, r2`
// Target: `oddadd r1, r0, r2` (customized internal opcode)
class FusedOddadd(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.ANDI && instr(0)(31, 20) === 1.U
  def inst2Cond = instr(1) === Instructions.ADD

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SH3ADD)
    // replace the fuOpType with oddadd
    cs.fuOpType := ALUOpType.oddadd
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "andi1_add"
}

// Case: add one if odd (in word format), otherwise unchanged
// Source: `andi r1, r0, 1`` + `addw r1, r1, r2`
// Target: `oddaddw r1, r0, r2` (customized internal opcode)
class FusedOddaddw(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.ANDI && instr(0)(31, 20) === 1.U
  def inst2Cond = instr(1) === Instructions.ADDW

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.SH3ADD)
    // replace the fuOpType with oddaddw
    cs.fuOpType := ALUOpType.oddaddw
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "andi1_addw"
}

// Case: addw and extract its lower 8 bits (fused into addwbyte)
class FusedAddwbyte(pair: Seq[Valid[UInt]], csPair: Option[Seq[CtrlSignals]])(implicit p: Parameters)
  extends BaseFusionCase(pair) {
  // the first instruction is a ALUOpType.addw
  // According to DecodeUnit.scala, only ADDIW and ADDW are ALUOpType.addw, which are used for inst1Cond.
  def inst1Cond = instr(0) === Instructions.ADDIW || instr(0) === Instructions.ADDW
  def inst2Cond = instr(1) === Instructions.ANDI && instr(1)(31, 20) === 0xff.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  def target: CtrlSignals = {
    val cs = WireInit(csPair.get(0))
    // replace the fuOpType with addwbyte
    cs.fuOpType := ALUOpType.addwbyte
    cs
  }

  def fusionName: String = "addw_andi255"
}

// Case: addw and extract its lower 1 bit (fused into addwbit)
class FusedAddwbit(pair: Seq[Valid[UInt]], csPair: Option[Seq[CtrlSignals]])(implicit p: Parameters)
  extends FusedAddwbyte(pair, csPair) {

  override def inst2Cond = instr(1) === Instructions.ANDI && instr(1)(31, 20) === 0x1.U
  override def target: CtrlSignals = {
    val cs = WireInit(csPair.get(0))
    // replace the fuOpType with addwbit
    cs.fuOpType := ALUOpType.addwbit
    cs
  }

  override def fusionName: String = "addw_andi1"
}

// Case: addw and zext.h (fused into addwzexth)
class FusedAddwzexth(pair: Seq[Valid[UInt]], csPair: Option[Seq[CtrlSignals]])(implicit p: Parameters)
  extends FusedAddwbyte(pair, csPair) {

  override def inst2Cond = instr(1) === Instructions.ZEXT_H
  override def target: CtrlSignals = {
    val cs = WireInit(csPair.get(0))
    // replace the fuOpType with addwzexth
    cs.fuOpType := ALUOpType.addwzexth
    cs
  }

  override def fusionName: String = "addw_zexth"
}

// Case: addw and sext.h (fused into addwsexth)
class FusedAddwsexth(pair: Seq[Valid[UInt]], csPair: Option[Seq[CtrlSignals]])(implicit p: Parameters)
  extends FusedAddwbyte(pair, csPair) {

  override def inst2Cond = instr(1) === Instructions.SEXT_H
  override def target: CtrlSignals = {
    val cs = WireInit(csPair.get(0))
    // replace the fuOpType with addwsexth
    cs.fuOpType := ALUOpType.addwsexth
    cs
  }

  override def fusionName: String = "addw_sexth"
}

// Case: logic operation and extract its LSB

class FusedLogiclsb(pair: Seq[Valid[UInt]], csPair: Option[Seq[CtrlSignals]])(implicit p: Parameters)
  extends BaseFusionCase(pair) {
  require(csPair.isDefined)

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
  def inst2Cond = instr(1) === Instructions.ANDI && instr(1)(31, 20) === 1.U

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && destToRs1
  def target: CtrlSignals = {
    val cs = WireInit(csPair.get(0))
    // change the opType to lsb format
    cs.fuOpType := ALUOpType.logicToLsb(csPair.get(0).fuOpType)
    cs
  }

  def fusionName: String = "logic_andi1"
}

class FusedLogicZexth(pair: Seq[Valid[UInt]], csPair: Option[Seq[CtrlSignals]])(implicit p: Parameters)
  extends FusedLogiclsb(pair, csPair) {

  override def inst2Cond = instr(1) === Instructions.ZEXT_H
  override def target: CtrlSignals = {
    val cs = WireInit(csPair.get(0))
    // change the opType to lzext format
    cs.fuOpType := ALUOpType.logicToZexth(csPair.get(0).fuOpType)
    cs
  }

  override def fusionName: String = "logic_zexth"
}

// Case: OR(Cat(src1(63, 8), 0.U(8.W)), src2)
// Source: `andi r1, r0, -256`` + `or r1, r1, r2`
class FusedOrh48(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.ANDI && instr(0)(31, 20) === 0xf00.U
  def inst2Cond = instr(1) === Instructions.OR

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    val cs = getBaseCS(Instructions.OR)
    // replace the fuOpType with orh48
    cs.fuOpType := ALUOpType.orh48
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "andi_f00_or"
}

// Case: mul 7bit data with 32-bit data
// Source: `andi r1, r0, 127`` + `mulw r1, r1, r2`
// Target: `mulw7 r1, r0, r2`
class FusedMulw7(pair: Seq[Valid[UInt]])(implicit p: Parameters)
  extends BaseFusionCase(pair) {
  def inst1Cond = instr(0) === Instructions.ANDI && instr(0)(31, 20) === 127.U
  def inst2Cond = instr(1) === Instructions.MULW

  def isValid: Bool = inst1Cond && inst2Cond && withSameDest && (destToRs1 || destToRs2)
  def target: CtrlSignals = {
    // use MULW as the base
    val cs = getBaseCS(Instructions.MULW)
    // replace the fuOpType with mulw7
    cs.fuOpType := MDUOpType.mulw7
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs
  }

  def fusionName: String = "andi127_mulw"
}

class FusionDecoder(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // detect instruction fusions in these instructions
    val in = Vec(DecodeWidth, Flipped(ValidIO(UInt(32.W))))
    val dec = Vec(DecodeWidth, Input(new CtrlSignals()))
    // whether an instruction fusion is found
    val out = Vec(DecodeWidth - 1, DecoupledIO(new CtrlSignals))
    // fused instruction needs to be cleared
    val clear = Vec(DecodeWidth, Output(Bool()))
  })

  io.clear.head := false.B

  val instrPairs = io.in.dropRight(1).zip(io.in.drop(1)).map(x => Seq(x._1, x._2))
  val csPairs = io.dec.dropRight(1).zip(io.dec.drop(1)).map(x => Seq(x._1, x._2))
  instrPairs.zip(csPairs).zip(io.out).zipWithIndex.foreach{ case (((pair, cs), out), i) =>
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
      new FusedAddwbyte(pair, Some(cs)),
      new FusedAddwbit(pair, Some(cs)),
      new FusedAddwzexth(pair, Some(cs)),
      new FusedAddwsexth(pair, Some(cs)),
      new FusedLogiclsb(pair, Some(cs)),
      new FusedLogicZexth(pair, Some(cs))
    )
    val pairValid = VecInit(pair.map(_.valid)).asUInt().andR
    val thisCleared = io.clear(i)
    val fusionVec = VecInit(fusionList.map(_.isValid))
    out.valid := pairValid && !thisCleared && fusionVec.asUInt().orR()
    XSError(PopCount(fusionVec) > 1.U, "more then one fusion matched\n")
    out.bits := Mux1H(fusionVec, fusionList.map(_.target))
    // TODO: assume every instruction fusion clears the second instruction now
    io.clear(i + 1) := out.valid
    fusionList.zip(fusionVec).foreach { case (f, v) =>
      XSPerfAccumulate(s"case_${f.fusionName}_$i", pairValid && !thisCleared && v && out.ready)
    }
    XSPerfAccumulate(s"conflict_fusion_$i", pairValid && thisCleared && fusionVec.asUInt().orR() && out.ready)
  }

  XSPerfAccumulate("fused_instr", PopCount(io.out.map(_.fire)))
}
