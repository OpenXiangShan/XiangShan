/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

// we support 256 interrupt bits by default
class IntrGenIO extends Bundle {
  val intrVec = Output(UInt(256.W))
}

class AXI4IntrGenerator
(
  address: Seq[AddressSet]
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false, _extra = new IntrGenIO)
{

  override lazy val module = new AXI4SlaveModuleImp(this){

    val intrReg = RegInit(VecInit(Seq.fill(8)(0.U(32.W))))
    io.extra.get.intrVec := Cat(intrReg.reverse)

    when (in.w.fire()) {
      intrReg(waddr(4, 2)) := in.w.bits.data(31, 0)
    }

    in.r.bits.data := intrReg(raddr)
  }
}
