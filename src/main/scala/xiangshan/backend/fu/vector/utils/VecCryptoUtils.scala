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

package xiangshan.backend.fu.vector.utils

import chisel3._
import chisel3.util._
import xiangshan._
import utility._
import xiangshan.backend.fu.util._
import org.chipsalliance.cde.config
import org.chipsalliance.cde.config.Parameters

/**
 * Vector AES encryption/decription utils
 */

// AES shift rows operation for 128-bit data
object AES128ShiftRowsFwd {
  def apply(src: UInt): UInt = {
    require(src.getWidth == 128)
    /** split [[src]] into 4 32-bit words */
    val srcVec = VecInit(src.asBools.grouped(32).map(VecInit(_).asUInt).toSeq)
    val dstVec = Wire(Vec(4, UInt(32.W)))

    // shift rows
    dstVec(0) := Cat(srcVec(3)(31, 24), srcVec(2)(23, 16), srcVec(1)(15, 8), srcVec(0)(7, 0))
    dstVec(1) := Cat(srcVec(0)(31, 24), srcVec(3)(23, 16), srcVec(2)(15, 8), srcVec(1)(7, 0))
    dstVec(2) := Cat(srcVec(1)(31, 24), srcVec(0)(23, 16), srcVec(3)(15, 8), srcVec(2)(7, 0))
    dstVec(3) := Cat(srcVec(2)(31, 24), srcVec(1)(23, 16), srcVec(0)(15, 8), srcVec(3)(7, 0))

    Cat(dstVec.reverse) // generate the output
  }
}

// AES inverse shift rows operation for 128-bit data
object AES128ShiftRowsInv {
  def apply(src: UInt): UInt = {
    require(src.getWidth == 128)
    /** split [[src]] into 4 32-bit words */
    val srcVec = VecInit(src.asBools.grouped(32).map(VecInit(_).asUInt).toSeq)
    val dstVec = Wire(Vec(4, UInt(32.W)))

    // shift rows
    dstVec(0) := Cat(srcVec(1)(31, 24), srcVec(2)(23, 16), srcVec(3)(15, 8), srcVec(0)(7, 0))
    dstVec(1) := Cat(srcVec(2)(31, 24), srcVec(3)(23, 16), srcVec(0)(15, 8), srcVec(1)(7, 0))
    dstVec(2) := Cat(srcVec(3)(31, 24), srcVec(0)(23, 16), srcVec(1)(15, 8), srcVec(2)(7, 0))
    dstVec(3) := Cat(srcVec(0)(31, 24), srcVec(1)(23, 16), srcVec(2)(15, 8), srcVec(3)(7, 0))

    Cat(dstVec.reverse) // generate the output
  }
}

class AES128SubBytesBidirectionIO extends Bundle {
  val src         = Input(UInt(128.W))
  val isZeroRound = Input(Bool())
  val isFwd       = Input(Bool())
  val isFwd_1s    = Input(Bool())
  val regEnable   = Input(Bool())
  val result      = Output(UInt(128.W)) // the 2nd cycle
}

class AES128SubBytesBidirection extends Module {
  val io = IO(new AES128SubBytesBidirectionIO)

  val isFwd = io.isFwd
  val srcBytes = io.src.asTypeOf(Vec(16, UInt(8.W)))

  // 3 layer of modules for both direction, with the shared middle layer
  val aesSboxOut = Wire(Vec(16, UInt(8.W)))

  (srcBytes zip aesSboxOut).foreach { case (in, out) =>
    val aesSBox1FwdOut = SboxAesTop(in)
    val aesSBox1InvOut = SboxIaesTop(in)

    val aesSBox2In     = (aesSBox1FwdOut zip aesSBox1InvOut).map { case (fwd, inv) => Mux(isFwd, fwd, inv) }
    val aesSBox2Out    = Reg(Vec(18, Bool())) // 1 cycle delay
    // val aesSBox2Out    = Wire(Vec(18, Bool())) // no delay
    when (io.regEnable) {
      aesSBox2Out := SboxInv(aesSBox2In)
    }

    val aesSbox3FwdOut  = SboxAesOut(aesSBox2Out)
    val aesSbox3InvOut  = SboxIaesOut(aesSBox2Out)
    out := Mux(io.isFwd_1s, aesSbox3FwdOut, aesSbox3InvOut)
    // out := Mux(isFwd, aesSbox3FwdOut, aesSbox3InvOut)
  }

  io.result := Cat(aesSboxOut.reverse)
}

/*
object AES128MixColumns {
  // for performing GF multiplicaiton
  def xt2(x: UInt): UInt = {
    require(x.getWidth == 8)
    val result = Wire(UInt(8.W))
    result := (x << 1).asUInt ^ Mux(x(7), 0x1b.U, 0.U)
    result // output
  }

  def xt3(x: UInt): UInt = {
    require(x.getWidth == 8)
    val result = Wire(UInt(8.W))
    result := x ^ xt2(x)
    result // output
  }

  /* Multiply 8-bit field element by 4-bit value for AES MixCols step */
  def gfmul(x: UInt, y: UInt): UInt = {
    require(x.getWidth == 8 && y.getWidth == 4)
    val result = Wire(UInt(8.W))
    result := Mux(y(0),             x   , 0.U(8.W)) ^
              Mux(y(1),         xt2(x)  , 0.U(8.W)) ^
              Mux(y(2),     xt2(xt2(x)) , 0.U(8.W)) ^
              Mux(y(3), xt2(xt2(xt2(x))), 0.U(8.W))
    result // output
  }
}

// AES mix columns operation for 128-bit data
object AES128MixColumnsFwd {
  def xt2(int: UInt): UInt = AES128MixColumns.xt2(int)
  def xt3(int: UInt): UInt = AES128MixColumns.xt3(int)

  private def AES128MixColumnFwd(src: UInt): UInt = {
    require(src.getWidth == 32)
    val srcBytes = Wire(Vec(4, UInt(8.W)))
    val dstBytes = Wire(Vec(4, UInt(8.W)))

    for (i <- 0 until 4) {
      srcBytes(i) := src(8 * (i + 1) - 1, 8 * i)
    }
    dstBytes(0) := xt2(srcBytes(0)) ^ xt3(srcBytes(1)) ^    (srcBytes(2)) ^    (srcBytes(3));
    dstBytes(1) :=    (srcBytes(0)) ^ xt2(srcBytes(1)) ^ xt3(srcBytes(2)) ^    (srcBytes(3));
    dstBytes(2) :=    (srcBytes(0)) ^    (srcBytes(1)) ^ xt2(srcBytes(2)) ^ xt3(srcBytes(3));
    dstBytes(3) := xt3(srcBytes(0)) ^    (srcBytes(1)) ^    (srcBytes(2)) ^ xt2(srcBytes(3));

    // output
    Cat(dstBytes.reverse)
  }

  def apply(srcVec: Vec[UInt]): Vec[UInt] = {
    require(srcVec.length == 4)
    srcVec.foreach { src => require(src.getWidth == 32) }
    val dstVec = Wire(Vec(4, UInt(32.W)))

    // mix columns as output
    dstVec := srcVec.map(AES128MixColumnFwd)
    dstVec
  }

  def apply(src: UInt): UInt = {
    require(src.getWidth == 128)
    val srcVec = VecInit((0 until 4).map(i => src(32 * (i + 1) - 1, 32 * i)))
    val dstVec = apply(srcVec)
    Cat(dstVec.reverse)
  }
}

object AES128MixColumnsInv {
  def gfmul(x: UInt, y: UInt): UInt = AES128MixColumns.gfmul(x, y)

  private val invMat: Seq[Seq[Int]] = Seq(
    Seq(0xE, 0xB, 0xD, 0x9),
    Seq(0x9, 0xE, 0xB, 0xD),
    Seq(0xD, 0x9, 0xE, 0xB),
    Seq(0xB, 0xD, 0x9, 0xE),
  )

  private def AES128MixColumnInv(src: UInt): UInt = {
    require(src.getWidth == 32)
    val srcBytes = Wire(Vec(4, UInt(8.W)))
    val dstBytes = Wire(Vec(4, UInt(8.W)))

    for (i <- 0 until 4) {
      srcBytes(i) := src(8 * (i + 1) - 1, 8 * i)
    }
    for (i <- 0 until 4) {
      dstBytes(i) := srcBytes zip invMat(i) map { case (x, y) => gfmul(x, y.U) } reduce (_ ^ _)
    }

    // output
    Cat(dstBytes.reverse)
  }

  def apply(srcVec: Vec[UInt]): Vec[UInt] = {
    require(srcVec.length == 4)
    srcVec.foreach { src => require(src.getWidth == 32) }
    val dstVec = Wire(Vec(4, UInt(32.W)))

    // mix columns as output
    dstVec := srcVec.map(AES128MixColumnInv)
    dstVec
  }

  def apply(src: UInt): UInt = {
    require(src.getWidth == 128)
    val srcVec = VecInit((0 until 4).map(i => src(32 * (i + 1) - 1, 32 * i)))
    val dstVec = apply(srcVec)
    Cat(dstVec.reverse)
  }
}
*/
