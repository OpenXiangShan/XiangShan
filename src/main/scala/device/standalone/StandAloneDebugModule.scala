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
import device.XSDebugModuleParams
import system.SoCParamsKey
import xiangshan.XSCoreParamsKey
import xiangshan.XSTileKey
import device.DebugModule

class StandAloneDebugModule (
  useTL: Boolean = false,
  baseAddress: BigInt,
  addrWidth: Int,
  dataWidth: Int = 64,
  hartNum: Int
)(implicit p: Parameters) extends StandAloneDevice(
  useTL, baseAddress, addrWidth, dataWidth, hartNum
) with HasMasterInterface {

  def addressSet: AddressSet = AddressSet(XSDebugModuleParams.apply(p(XSTileKey).head.XLEN).baseAddress, 0xfff)

  val debugModule = LazyModule(new DebugModule(hartNum)(p))
  debugModule.debug.node := xbar
  debugModule.debug.dmInner.dmInner.sb2tlOpt.foreach(masternode := _.node)

  // interrupts
  val debugModuleIntNode = IntSinkNode(IntSinkPortSimple(hartNum, 1))
  debugModuleIntNode :*= debugModule.debug.dmOuter.dmOuter.intnode
  val int = InModuleBody(debugModuleIntNode.makeIOs())

  class StandAloneDebugModuleImp(val outer: StandAloneDebugModule)(implicit p: Parameters) extends StandAloneDeviceImp(outer) {
    val io = IO(new outer.debugModule.DebugModuleIO)
    io <> outer.debugModule.module.io
  }

  override lazy val module = new StandAloneDebugModuleImp(this)

}
