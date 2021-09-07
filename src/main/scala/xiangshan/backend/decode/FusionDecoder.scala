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

abstract class BaseFusionCase(pair: Seq[Valid[UInt]])(implicit p: Parameters) extends DecodeUnitConstants {
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
    val adduwTable = XDecode.table.filter(_._1 == Instructions.ADDU_W).map(_._2).head
    val cs = Wire(new CtrlSignals)
    cs := DontCare
    cs.decode(adduwTable)
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := 0.U
    cs.ldest := instr1Rd
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
    val zexthTable = XDecode.table.filter(_._1 == Instructions.ZEXT_H).map(_._2).head
    val cs = Wire(new CtrlSignals)
    cs := DontCare
    cs.decode(zexthTable)
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := 0.U
    cs.ldest := instr1Rd
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
    val sexthTable = XDecode.table.filter(_._1 == Instructions.SEXT_H).map(_._2).head
    val cs = Wire(new CtrlSignals)
    cs := DontCare
    cs.decode(sexthTable)
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := 0.U
    cs.ldest := instr1Rd
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
    val sh1addTable = XDecode.table.filter(_._1 == Instructions.SH1ADD).map(_._2).head
    val cs = Wire(new CtrlSignals)
    cs := DontCare
    cs.decode(sh1addTable)
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs.ldest := instr1Rd
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
    val sh2addTable = XDecode.table.filter(_._1 == Instructions.SH2ADD).map(_._2).head
    val cs = Wire(new CtrlSignals)
    cs := DontCare
    cs.decode(sh2addTable)
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs.ldest := instr1Rd
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
    val sh3addTable = XDecode.table.filter(_._1 == Instructions.SH3ADD).map(_._2).head
    val cs = Wire(new CtrlSignals)
    cs := DontCare
    cs.decode(sh3addTable)
    cs.lsrc(0) := instr1Rs1
    cs.lsrc(1) := Mux(destToRs1, instr2Rs2, instr2Rs1)
    cs.ldest := instr1Rd
    cs
  }

  def fusionName: String = "slli3_add"
}

class FusionDecoder(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // detect instruction fusions in these instructions
    val in = Vec(DecodeWidth, Flipped(ValidIO(UInt(32.W))))
    // whether an instruction fusion is found
    val out = Vec(DecodeWidth - 1, DecoupledIO(new CtrlSignals))
    // fused instruction needs to be cleared
    val clear = Vec(DecodeWidth, Output(Bool()))
  })

  io.clear.head := false.B

  val instrPairs = io.in.dropRight(1).zip(io.in.drop(1)).map(x => Seq(x._1, x._2))
  instrPairs.zip(io.out).zipWithIndex.foreach{ case ((pair, out), i) =>
    val fusionList = Seq(
      new FusedAdduw(pair),
      new FusedZexth(pair),
      new FusedZexth1(pair),
      new FusedSexth(pair),
      new FusedSh1add(pair),
      new FusedSh2add(pair),
      new FusedSh3add(pair),
    )
    val pairValid = VecInit(pair.map(_.valid)).asUInt().andR
    val thisCleared = io.clear(i)
    val fusionVec = VecInit(fusionList.map(_.isValid))
    out.valid := pairValid && !thisCleared && fusionVec.asUInt().orR()
    out.bits := PriorityMux(fusionVec, fusionList.map(_.target))
    // TODO: assume every instruction fusion clears the second instruction now
    io.clear(i + 1) := out.valid
    fusionList.zip(fusionVec).foreach { case (f, v) =>
      XSPerfAccumulate(s"${f.fusionName}_$i", pairValid && !thisCleared && v)
    }
  }

  XSPerfAccumulate("fused_instr", PopCount(io.out.map(_.fire)))
}
