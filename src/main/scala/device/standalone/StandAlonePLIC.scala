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
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.interrupts._

class StandAlonePLIC (
  useTL: Boolean = false,
  baseAddress: BigInt,
  addrWidth: Int,
  dataWidth: Int = 64,
  hartNum: Int
)(implicit p: Parameters) extends StandAloneDevice(
  useTL, baseAddress, addrWidth, dataWidth, hartNum
) with BindingScope {

  private def plicParam = PLICParams(baseAddress)
  def addressSet: AddressSet = plicParam.address

  private val plic = LazyModule(new TLPLIC(plicParam, dataWidth / 8))
  plic.node := xbar

  // interrupts
  val plicIntNode = IntSinkNode(IntSinkPortSimple(hartNum * 2, 1))
  plicIntNode :*= plic.intnode
  val int = InModuleBody(plicIntNode.makeIOs())

}
