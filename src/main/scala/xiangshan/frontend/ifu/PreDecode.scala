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
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr

class PreDecodeResp(implicit p: Parameters) extends XSBundle with PreDecodeHelper {
  val pd           = Vec(PredictWidth, new PreDecodeInfo)
  val hasHalfValid = Vec(PredictWidth, Bool())
  // val expInstr = Vec(PredictWidth, UInt(32.W))
  val instr      = Vec(PredictWidth, UInt(32.W))
  val jumpOffset = Vec(PredictWidth, PrunedAddr(VAddrBits))
//  val hasLastHalf = Bool()
  val triggered = Vec(PredictWidth, TriggerAction())
}

class PreDecode(implicit p: Parameters) extends XSModule with PreDecodeHelper {
  val io = IO(new Bundle() {
    val in  = Input(ValidIO(new IfuToPreDecode))
    val out = Output(new PreDecodeResp)
  })

  val data = io.in.bits.data
//  val lastHalfMatch = io.in.lastHalfMatch
  val validStart, validEnd     = Wire(Vec(PredictWidth, Bool()))
  val h_validStart, h_validEnd = Wire(Vec(PredictWidth, Bool()))

  val validStart_half, validEnd_half     = Wire(Vec(PredictWidth, Bool()))
  val h_validStart_half, h_validEnd_half = Wire(Vec(PredictWidth, Bool()))

  val validStart_halfPlus1, validEnd_halfPlus1     = Wire(Vec(PredictWidth, Bool()))
  val h_validStart_halfPlus1, h_validEnd_halfPlus1 = Wire(Vec(PredictWidth, Bool()))

  val validStart_diff, validEnd_diff     = Wire(Vec(PredictWidth, Bool()))
  val h_validStart_diff, h_validEnd_diff = Wire(Vec(PredictWidth, Bool()))

  val currentIsRVC = Wire(Vec(PredictWidth, Bool()))

  validStart_half.map(_ := false.B)
  validEnd_half.map(_ := false.B)
  h_validStart_half.map(_ := false.B)
  h_validEnd_half.map(_ := false.B)

  validStart_halfPlus1.map(_ := false.B)
  validEnd_halfPlus1.map(_ := false.B)
  h_validStart_halfPlus1.map(_ := false.B)
  h_validEnd_halfPlus1.map(_ := false.B)

  val rawInsts = if (HasCExtension) VecInit((0 until PredictWidth).map(i => Cat(data(i + 1), data(i))))
  else VecInit((0 until PredictWidth).map(i => data(i)))

  for (i <- 0 until PredictWidth) {
    val inst = WireInit(rawInsts(i))
    // val expander       = Module(new RVCExpander)
    currentIsRVC(i) := isRVC(inst)
    val currentPC = io.in.bits.pc(i)
    // expander.io.in             := inst

    val (brType, isCall, isRet) = getBrInfo(inst)
    val jalOffset               = getJalOffset(inst, currentIsRVC(i))
    val brOffset                = getBrOffset(inst, currentIsRVC(i))

    io.out.hasHalfValid(i) := h_validStart(i)

    io.out.triggered(i) := DontCare // VecInit(Seq.fill(10)(false.B))

    io.out.pd(i).valid := validStart(i)
    io.out.pd(i).isRVC := currentIsRVC(i)

    // for diff purpose only
    io.out.pd(i).brType := brType
    io.out.pd(i).isCall := isCall
    io.out.pd(i).isRet  := isRet

    // io.out.expInstr(i)         := expander.io.out.bits
    io.out.instr(i)      := inst
    io.out.jumpOffset(i) := Mux(io.out.pd(i).isBr, brOffset, jalOffset)
  }

  // the first half is always reliable
  for (i <- 0 until PredictWidth / 2) {
    val lastIsValidEnd = if (i == 0) { true.B }
    else { validEnd(i - 1) || !HasCExtension.B }
    validStart(i) := (lastIsValidEnd || !HasCExtension.B)
    validEnd(i)   := validStart(i) && currentIsRVC(i) || !validStart(i) || !HasCExtension.B

    // prepared for last half match
    val h_lastIsValidEnd = if (i == 0) { false.B }
    else { h_validEnd(i - 1) || !HasCExtension.B }
    h_validStart(i) := (h_lastIsValidEnd || !HasCExtension.B)
    h_validEnd(i)   := h_validStart(i) && currentIsRVC(i) || !h_validStart(i) || !HasCExtension.B
  }

  for (i <- 0 until PredictWidth) {
    val lastIsValidEnd = if (i == 0) { true.B }
    else { validEnd_diff(i - 1) || !HasCExtension.B }
    validStart_diff(i) := (lastIsValidEnd || !HasCExtension.B)
    validEnd_diff(i)   := validStart_diff(i) && currentIsRVC(i) || !validStart_diff(i) || !HasCExtension.B

    // prepared for last half match
    val h_lastIsValidEnd = if (i == 0) { false.B }
    else { h_validEnd_diff(i - 1) || !HasCExtension.B }
    h_validStart_diff(i) := (h_lastIsValidEnd || !HasCExtension.B)
    h_validEnd_diff(i)   := h_validStart_diff(i) && currentIsRVC(i) || !h_validStart_diff(i) || !HasCExtension.B
  }

  // assume PredictWidth / 2 is a valid start
  for (i <- PredictWidth / 2 until PredictWidth) {
    val lastIsValidEnd = if (i == PredictWidth / 2) { true.B }
    else { validEnd_half(i - 1) || !HasCExtension.B }
    validStart_half(i) := (lastIsValidEnd || !HasCExtension.B)
    validEnd_half(i)   := validStart_half(i) && currentIsRVC(i) || !validStart_half(i) || !HasCExtension.B

    // prepared for last half match
    val h_lastIsValidEnd = if (i == PredictWidth / 2) { true.B }
    else { h_validEnd_half(i - 1) || !HasCExtension.B }
    h_validStart_half(i) := (h_lastIsValidEnd || !HasCExtension.B)
    h_validEnd_half(i)   := h_validStart_half(i) && currentIsRVC(i) || !h_validStart_half(i) || !HasCExtension.B
  }

  // assume PredictWidth / 2 + 1 is a valid start (and PredictWidth / 2 is last half of RVI)
  for (i <- PredictWidth / 2 + 1 until PredictWidth) {
    val lastIsValidEnd = if (i == PredictWidth / 2 + 1) { true.B }
    else { validEnd_halfPlus1(i - 1) || !HasCExtension.B }
    validStart_halfPlus1(i) := (lastIsValidEnd || !HasCExtension.B)
    validEnd_halfPlus1(i) := validStart_halfPlus1(i) && currentIsRVC(i) || !validStart_halfPlus1(i) || !HasCExtension.B

    // prepared for last half match
    val h_lastIsValidEnd = if (i == PredictWidth / 2 + 1) { true.B }
    else { h_validEnd_halfPlus1(i - 1) || !HasCExtension.B }
    h_validStart_halfPlus1(i) := (h_lastIsValidEnd || !HasCExtension.B)
    h_validEnd_halfPlus1(i) := h_validStart_halfPlus1(i) && currentIsRVC(i) || !h_validStart_halfPlus1(
      i
    ) || !HasCExtension.B
  }
  validStart_halfPlus1(PredictWidth / 2) := false.B // could be true but when true we select half, not halfPlus1
  validEnd_halfPlus1(PredictWidth / 2)   := true.B

  // assume h_PredictWidth / 2 is an end
  h_validStart_halfPlus1(PredictWidth / 2) := false.B // could be true but when true we select half, not halfPlus1
  h_validEnd_halfPlus1(PredictWidth / 2)   := true.B

  // if PredictWidth / 2 - 1 is a valid end, PredictWidth / 2 is a valid start
  for (i <- PredictWidth / 2 until PredictWidth) {
    validStart(i)   := Mux(validEnd(PredictWidth / 2 - 1), validStart_half(i), validStart_halfPlus1(i))
    validEnd(i)     := Mux(validEnd(PredictWidth / 2 - 1), validEnd_half(i), validEnd_halfPlus1(i))
    h_validStart(i) := Mux(h_validEnd(PredictWidth / 2 - 1), h_validStart_half(i), h_validStart_halfPlus1(i))
    h_validEnd(i)   := Mux(h_validEnd(PredictWidth / 2 - 1), h_validEnd_half(i), h_validEnd_halfPlus1(i))
  }

  val validStartMismatch        = Wire(Bool())
  val validEndMismatch          = Wire(Bool())
  val validH_ValidStartMismatch = Wire(Bool())
  val validH_ValidEndMismatch   = Wire(Bool())

  validStartMismatch        := validStart.zip(validStart_diff).map { case (a, b) => a =/= b }.reduce(_ || _)
  validEndMismatch          := validEnd.zip(validEnd_diff).map { case (a, b) => a =/= b }.reduce(_ || _)
  validH_ValidStartMismatch := h_validStart.zip(h_validStart_diff).map { case (a, b) => a =/= b }.reduce(_ || _)
  validH_ValidEndMismatch   := h_validEnd.zip(h_validEnd_diff).map { case (a, b) => a =/= b }.reduce(_ || _)

  XSError(io.in.valid && validStartMismatch, p"validStart mismatch\n")
  XSError(io.in.valid && validEndMismatch, p"validEnd mismatch\n")
  XSError(io.in.valid && validH_ValidStartMismatch, p"h_validStart mismatch\n")
  XSError(io.in.valid && validH_ValidEndMismatch, p"h_validEnd mismatch\n")

//  io.out.hasLastHalf := !io.out.pd(PredictWidth - 1).isRVC && io.out.pd(PredictWidth - 1).valid

  for (i <- 0 until PredictWidth) {
    XSDebug(
      true.B,
      p"instr ${Hexadecimal(io.out.instr(i))}, " +
        p"validStart ${Binary(validStart(i))}, " +
        p"validEnd ${Binary(validEnd(i))}, " +
        p"isRVC ${Binary(io.out.pd(i).isRVC)}, " +
        p"brType ${Binary(io.out.pd(i).brType)}, " +
        p"isRet ${Binary(io.out.pd(i).isRet)}, " +
        p"isCall ${Binary(io.out.pd(i).isCall)}\n"
    )
  }
}
