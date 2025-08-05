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

package device.standalone

import chisel3._
import chisel3.util._
import device.SYSCNT
import device.SYSCNTParams
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import org.chipsalliance.cde.config.Parameters
import utility.IntBuffer

class StandAloneSYSCNT(
    useTL:       Boolean = false,
    baseAddress: BigInt,
    addrWidth:   Int,
    dataWidth:   Int = 64,
    hartNum:     Int
)(implicit p: Parameters) extends StandAloneDevice(
      useTL,
      baseAddress,
      addrWidth,
      dataWidth,
      hartNum
    ) {

  private def clintParam = SYSCNTParams(baseAddress)
  def addressSet: AddressSet = clintParam.address

  private val clint = LazyModule(new SYSCNT(clintParam, dataWidth / 8))
  clint.node := xbar

  class StandAloneSYSCNTImp(outer: StandAloneSYSCNT)(implicit p: Parameters) extends StandAloneDeviceImp(outer) {
    val rtc_clock = IO(Input(Clock()))
    val rtc_reset = IO(Input(AsyncReset()))
    val io = IO(new Bundle {
      val update_en    = Input(Bool())
      val update_value = Input(UInt(64.W))
      val stop_en      = Input(Bool())
      val time         = Output(ValidIO(UInt(64.W)))
    })
    outer.clint.module.rtc_clock       := rtc_clock
    outer.clint.module.rtc_reset       := rtc_reset
    outer.clint.module.bus_clock       := clock
    outer.clint.module.bus_reset       := reset
    outer.clint.module.io.stop_en      := io.stop_en
    outer.clint.module.io.update_en    := io.update_en
    outer.clint.module.io.update_value := io.update_value
    io.time                            := outer.clint.module.io.time
  }

  override lazy val module = new StandAloneSYSCNTImp(this)

}
