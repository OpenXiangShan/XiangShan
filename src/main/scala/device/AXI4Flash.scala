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
import difftest.common.DifftestFlash
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet

class AXI4Flash
(
  address: Seq[AddressSet]
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false)
{

  override lazy val module = new AXI4SlaveModuleImp(this){
    val beatBits = log2Ceil(node.portParams.head.beatBytes)
    def getOffset(addr: UInt) = Cat(addr(15, beatBits), 0.U(beatBits.W))

    val flash = DifftestFlash()
    flash.en := in.ar.fire
    flash.addr := Cat(0.U(16.W), getOffset(raddr))

    in.r.bits.data := flash.data
  }
}
