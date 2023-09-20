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

package device

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet
import utils._
import utility._
import difftest._

class AXI4UART
(
  address: Seq[AddressSet]
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false, _extra = new UARTIO)
{
  override lazy val module = new AXI4SlaveModuleImp[UARTIO](this){
    val rxfifo = RegInit(0.U(32.W))
    val txfifo = Reg(UInt(32.W))
    val stat = RegInit(1.U(32.W))
    val ctrl = RegInit(0.U(32.W))

    io.extra.get.out.valid := (waddr(3,0) === 4.U && in.w.fire)
    io.extra.get.out.ch := in.w.bits.data(7,0)
    io.extra.get.in.valid := (raddr(3,0) === 0.U && in.r.fire)

    val mapping = Map(
      RegMap(0x0, io.extra.get.in.ch, RegMap.Unwritable),
      RegMap(0x4, txfifo),
      RegMap(0x8, stat),
      RegMap(0xc, ctrl)
    )

    RegMap.generate(mapping, raddr(3,0), in.r.bits.data,
      waddr(3,0), in.w.fire, in.w.bits.data, MaskExpand(in.w.bits.strb >> waddr(2,0))
    )
  }
}
