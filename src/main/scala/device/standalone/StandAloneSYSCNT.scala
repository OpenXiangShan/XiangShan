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

package device.standalone

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.interrupts._
import utility.IntBuffer

class StandAloneSYSCNT (
  useTL: Boolean = false,
  baseAddress: BigInt,
  addrWidth: Int,
  dataWidth: Int = 64,
  hartNum: Int
)(implicit p: Parameters) extends StandAloneDevice(
  useTL, baseAddress, addrWidth, dataWidth, hartNum
) {

  private def clintParam = SYSCNTParams(baseAddress)
  def addressSet: AddressSet = clintParam.address

  private val clint = LazyModule(new SYSCNT(clintParam, dataWidth / 8))
  clint.node := xbar

  class StandAloneSYSCNTImp(outer: StandAloneSYSCNT)(implicit p: Parameters) extends StandAloneDeviceImp(outer) {
    val io = IO(new Bundle {
      val rtcTick = Input(Bool())
      val stopen = Input(Bool())
      val time = Output(ValidIO(UInt(64.W)))
    })
    outer.clint.module.io.rtcTick := io.rtcTick
    outer.clint.module.io.stopen := io.stopen
    io.time := outer.clint.module.io.time
  }

  override lazy val module = new StandAloneSYSCNTImp(this)

}
