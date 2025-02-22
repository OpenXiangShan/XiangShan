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

package xiangshan.backend.fu.vector

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import utility._
import xiangshan.backend.fu.util._
import xiangshan.backend.fu.vector.utils._

/**
 * AES-128 excryption/decryption module for vector unit
 * @param p
 */
class AES128EncryptDecrypt(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val state     = Input(UInt(128.W))
    val rkey      = Input(UInt(128.W))
    val op        = Input(UInt(2.W)) // 00: dec middle, 01: dec final, 10: enc middle, 11: enc final
    val regEnable = Input(Bool())
    val result    = Output(UInt(128.W))
  })

  val state        = io.state
  val rkey         = io.rkey
  val isFwd        = io.op(1)
  val isFinalRound = io.op(0)
  val regEnable    = io.regEnable

  val decFinal  = Wire(UInt(128.W))
  val decMiddle = Wire(UInt(128.W))
  val encFinal  = Wire(UInt(128.W))
  val encMiddle = Wire(UInt(128.W))

  val srDec = Wire(UInt(128.W))
  val srEnc = Wire(UInt(128.W))
  val ark   = Wire(UInt(128.W))

  // sub-bytes
  val sbox = Module(new AES128SubBytesBidirection)
  sbox.io.src       := Mux(isFwd, srEnc, srDec)
  sbox.io.isFwd     := isFwd
  sbox.io.regEnable := regEnable
  val sb = sbox.io.result

  // decryption final round
  srDec := AES128ShiftRowsInv(state)
  ark := sb ^ rkey
  decFinal := ark

  // decryption middle round
  decMiddle := Cat((0 until 4).map(i =>
    MixInv((4 * i until 4 * (i + 1)).map(j =>
      ark(8 * (j + 1) - 1, 8 * j)
    ))).reverse
  )
  XLEN.W

  // encryption final round
  srEnc := AES128ShiftRowsFwd(state) // since shift-rows and sub-bytes commute
  encFinal := ark

  // encryption middle round
  val mixEncMiddle = Cat((0 until 4).map(i =>
    MixFwd((4 * i until 4 * (i + 1)).map(j =>
      sb(8 * (j + 1) - 1, 8 * j)
    ))).reverse
  )
  encMiddle := mixEncMiddle ^ rkey

  io.result := Mux(
    isFinalRound,
    Mux(isFwd, encFinal, decFinal),
    Mux(isFwd, encMiddle, decMiddle)
  )
}
