/***************************************************************************************
 * Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package device

import chisel3._
import chisel3.Bundle
import chisel3.Input
import chisel3.Module
import chisel3.Output
import chisel3.UInt
import chisel3.util.RegEnable
import chisel3.util.ValidIO
import freechips.rocketchip.util.AsyncResetSynchronizerShiftReg

class TimeAsync extends Module { // work with destination clock
  val io = IO(new Bundle {
    val i_time = Input(ValidIO(UInt(64.W)))
    val o_time = Output(ValidIO(UInt(64.W)))
  })
  // async for i_time vld
  val time_vld      = AsyncResetSynchronizerShiftReg(io.i_time.valid, 3, 0)
  val time_vld_1dly = RegNext(time_vld, false.B)
  val time_vld_xor  = time_vld ^ time_vld_1dly
  val time_vld_o    = RegNext(time_vld_xor, false.B)
  val time_o        = RegEnable(io.i_time.bits, 0.U(64.W), time_vld_xor)
  io.o_time.valid := time_vld_o
  io.o_time.bits  := time_o
}
// code about timer vld gen : Q<=~D
class TimeVldGen extends Module { // work with reference clock,sync with syscnt
  val io = IO(new Bundle {
    val i_time = Input(UInt(64.W))
    val o_time = Output(ValidIO(UInt(64.W)))
  })
  io.o_time.bits  := io.i_time
  io.o_time.valid := io.i_time(0)
}
