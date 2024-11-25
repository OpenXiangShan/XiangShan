/** *************************************************************************************
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
 * ************************************************************************************* */

package xiangshan.frontend.tracertl

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.XSModule
import xiangshan.frontend.tracertl.{TraceRTLChoose, TraceRTLDontCareValue}
import utility.XSError

object FPFormat {
  def fp16 = "01".U
  def fp32 = "10".U
  def fp64 = "11".U
}

abstract class TraceDummyIntFpDividerBase(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val start_valid_i = Input(Bool())
    val start_ready_o = Output(Bool())
    val flush_i = Input(Bool())
    val format_i = Input(UInt(2.W))
    val is_sqrt_i = Input(Bool())
    val finish_valid_o = Output(Bool())
    val finish_ready_i = Input(Bool())

    // un-connected io
    // val fp_aIsFpCanonicalNAN = Input(Bool())
    // val fp_bIsFpCanonicalNAN = Input(Bool())
  })

  val busyState = RegInit(false.B)
  // val finishStateReg = RegInit(false.B)
  // val finishStateWire = WireInit(finishStateReg)
  val finishState = WireInit(false.B)
  val exeMaxCycle = RegInit(0.U(8.W))
  val exeCurCycle = RegInit(0.U(8.W))

  when (io.start_ready_o && io.start_valid_i) {
    busyState := true.B
    exeMaxCycle := getCycle(io.format_i, io.is_sqrt_i)
    exeCurCycle := 1.U
  }

  when (busyState && !finishState) {
    exeCurCycle := exeCurCycle + 1.U
  }

  finishState := busyState && (exeCurCycle === exeMaxCycle)

  when (io.finish_ready_i && io.finish_valid_o) {
    busyState := false.B
    exeCurCycle := 0.U
  }

  when (io.flush_i) {
    busyState := false.B
    finishState := false.B
  }

  io.start_ready_o := !busyState
  io.finish_valid_o := finishState

  def getCycle(format: UInt, is_sqrt: Bool): UInt = {
    return 0.U
  }

  XSError(busyState && (exeCurCycle > exeMaxCycle), "exeCurCycle should not be large than exeMaxCycle")
}

class TraceDummyIntDivider(implicit p: Parameters) extends TraceDummyIntFpDividerBase {

  override def getCycle(format: UInt, is_sqrt: Bool): UInt = {
    return Mux(format === 3.U, 10.U, 6.U)
  }
}

class TraceDummyFpDivider(implicit p: Parameters) extends TraceDummyIntFpDividerBase {

  override def getCycle(format: UInt, is_sqrt: Bool): UInt = {
    Mux(is_sqrt,
      Mux(format === 3.U, 15.U, 10.U),
      Mux(format === 3.U, 12.U, 7.U))
  }
}
