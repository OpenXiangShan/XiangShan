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

import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.backend.decode.isa.bitfield.{InstVType, XSInstBitFields}

abstract class Imm(val len: Int, val typEncode: UInt) {
  def toImm32(minBits: UInt): UInt = do_toImm32(minBits(len - 1, 0))
  def extract(width: Int)(minBits: UInt): UInt = ???
  def do_toImm32(minBits: UInt): UInt
  def minBitsFromInstr(instr: UInt): UInt
}

case class Imm_I() extends Imm(12, SelImm.IMM_I) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits(len - 1, 0), 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 20))
}

case class Imm_S() extends Imm(12, SelImm.IMM_S) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 25), instr(11, 7))
}

case class Imm_B() extends Imm(12, SelImm.IMM_SB) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31), instr(7), instr(30, 25), instr(11, 8))
}

case class Imm_U() extends Imm(20, SelImm.IMM_U) {
  override def do_toImm32(minBits: UInt): UInt = Cat(minBits(len - 1, 0), 0.U(12.W))

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(31, 12)
  }
}

case class Imm_J() extends Imm(20, SelImm.IMM_UJ) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(31), instr(19, 12), instr(20), instr(30, 25), instr(24, 21))
  }
}

case class Imm_Z() extends Imm(12 + 5 + 5, SelImm.IMM_Z) {
  override def do_toImm32(minBits: UInt): UInt = minBits

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(11, 7), instr(19, 15), instr(31, 20))
  }

  def getCSRAddr(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(11, 0)
  }

  def getRS1(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(16, 12)
  }

  def getRD(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(21, 17)
  }

  def getImm5(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(16, 12)
  }
}

case class Imm_OPIVIS() extends Imm(5, SelImm.IMM_OPIVIS) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def extract(width: Int)(imm: UInt): UInt = SignExt(imm.take(5), width)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(19, 15)
  }
}

case class Imm_OPIVIU() extends Imm(5, SelImm.IMM_OPIVIU) {
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)

  override def extract(width: Int)(imm: UInt): UInt = ZeroExt(imm.take(5), width)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(19, 15)
  }
}

case class Imm_FI() extends Imm(5, SelImm.IMM_FI) {
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)

  override def extract(width: Int)(imm: UInt): UInt = ZeroExt(imm.take(5), width)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(19, 15)
  }
}

case class Imm_VSETVLI() extends Imm(11, SelImm.IMM_VSETVLI) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(30, 20)
  }

  /**
   * get VType from extended imm
   * @param extedImm
   * @return VType
   */
  def getVType(extedImm: UInt): InstVType = {
    val vtype = Wire(new InstVType)
    vtype := extedImm(10, 0).asTypeOf(new InstVType)
    vtype
  }

  def getVTypei(imm: UInt): UInt = {
    imm(10, 0)
  }
}

case class Imm_VSETIVLI() extends Imm(15, SelImm.IMM_VSETIVLI) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    val rvInst: XSInstBitFields = instr.asTypeOf(new XSInstBitFields)
    val uimm5 = rvInst.UIMM_VSETIVLI
    val vtype8 = rvInst.ZIMM_VSETIVLI
    Cat(uimm5, vtype8)
  }

  /**
   * get VType from extended imm
   * @param extedImm
   * @return VType
   */
  def getVType(extedImm: UInt): InstVType = {
    val vtype = Wire(new InstVType)
    vtype := extedImm(9, 0).asTypeOf(new InstVType)
    vtype
  }

  def getVTypei(imm: UInt): UInt = {
    imm(9, 0)
  }

  def getAvl(extedImm: UInt): UInt = {
    extedImm(14, 10)
  }
}

case class Imm_LUI32() extends Imm(32, SelImm.IMM_LUI32) {
  override def do_toImm32(minBits: UInt): UInt = minBits(31, 0)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(31, 0)
  }
}

case class Imm_VRORVI() extends Imm(6, SelImm.IMM_VRORVI) {
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(26), instr(19, 15))
  }
}

object ImmUnion {
  val I = Imm_I()
  val S = Imm_S()
  val B = Imm_B()
  val U = Imm_U()
  val J = Imm_J()
  val Z = Imm_Z()
  val OPIVIS = Imm_OPIVIS()
  val OPIVIU = Imm_OPIVIU()
  val FI = Imm_FI()
  val VSETVLI = Imm_VSETVLI()
  val VSETIVLI = Imm_VSETIVLI()
  val LUI32 = Imm_LUI32()
  val VRORVI = Imm_VRORVI()

  // do not add special type lui32 to this, keep ImmUnion max len being 20.
  val imms = Seq(I, S, B, U, J, Z, OPIVIS, OPIVIU, FI, VSETVLI, VSETIVLI, VRORVI)
  val immSelMap = Seq(
    SelImm.IMM_I,
    SelImm.IMM_S,
    SelImm.IMM_SB,
    SelImm.IMM_U,
    SelImm.IMM_UJ,
    SelImm.IMM_Z,
    SelImm.IMM_OPIVIS,
    SelImm.IMM_OPIVIU,
    SelImm.IMM_FI,
    SelImm.IMM_VSETVLI,
    SelImm.IMM_VSETIVLI,
    SelImm.IMM_VRORVI,
  ).zip(imms)
}
