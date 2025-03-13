/***************************************************************************************
* Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024-2025 Institute of Computing Technology, Chinese Academy of Sciences
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
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util.AsyncResetSynchronizerShiftReg
import system.SoCParamsKey
import xiangshan.XSCoreParamsKey
import xiangshan.XSTileKey
import device.DebugModule
import utility.{IntBuffer, RegNextN, ResetGen}
import freechips.rocketchip.tilelink.TLWidthWidget

class StandAloneDebugModule (
  useTL: Boolean = false,
  baseAddress: BigInt,
  addrWidth: Int,
  dataWidth: Int = 64,
  hartNum: Int
)(implicit p: Parameters) extends StandAloneDevice(
  useTL, baseAddress, addrWidth, dataWidth, hartNum
) with HasMasterInterface {

  def masterAddrWidth: Int = 48

  def addressSet: AddressSet = p(DebugModuleKey).get.address

  val debugModule = LazyModule(new DebugModule(hartNum)(p))
  debugModule.debug.node := xbar
  debugModule.debug.dmInner.dmInner.sb2tlOpt.foreach(masternode := TLWidthWidget(1) := _.node)
  // interrupts
  val debugModuleIntNode = IntSinkNode(IntSinkPortSimple(hartNum, 1))
  debugModuleIntNode :*= IntBuffer() :*= debugModule.debug.dmOuter.dmOuter.intnode
  val int = InModuleBody(debugModuleIntNode.makeIOs())

  class StandAloneDebugModuleImp(val outer: StandAloneDebugModule)(implicit p: Parameters) extends StandAloneDeviceRawImp(outer) {
    val io = IO(new outer.debugModule.DebugModuleIO)
    childClock := io.clock
    childReset := io.reset.asAsyncReset
    io <> outer.debugModule.module.io
    outer.debugModule.module.io.reset := io.reset.asAsyncReset
    outer.debugModule.module.io.debugIO.reset := io.debugIO.reset.asAsyncReset
    outer.debugModule.module.io.debugIO.systemjtag.foreach(
      _.reset := (withClockAndReset(io.debugIO.systemjtag.get.jtag.TCK, io.debugIO.systemjtag.get.reset.asAsyncReset) { ResetGen() })
    )
    withClockAndReset(io.clock, io.reset.asAsyncReset) {
      outer.debugModule.module.io.resetCtrl.hartIsInReset := AsyncResetSynchronizerShiftReg(io.resetCtrl.hartIsInReset, 3, 0)
      io.resetCtrl.hartResetReq.foreach(req =>
        req := RegNext(outer.debugModule.module.io.resetCtrl.hartResetReq.getOrElse(0.U.asTypeOf(req)), 0.U.asTypeOf(req)))
    }
  }

  override lazy val module = new StandAloneDebugModuleImp(this)

}
