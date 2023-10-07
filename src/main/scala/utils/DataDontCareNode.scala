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

package utils

import chisel3._
import org.chipsalliance.cde.config.Parameters
import chisel3.util.DecoupledIO
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBundle, TLClientNode, TLIdentityNode, TLMasterParameters, TLMasterPortParameters}

class DataDontCareNode(a: Boolean = false, b: Boolean = false, c: Boolean = false, d: Boolean = false)(implicit p: Parameters) extends LazyModule  {

  val node = TLIdentityNode()

  val n = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(
      TLMasterParameters.v1("DataDontCareNode")
    )
  )))

  lazy val module = new LazyModuleImp(this) with HasTLDump{
    val (out, _) = node.out(0)
    val (in, _) = node.in(0)

    if (a) {
      out.a.bits.data := DontCare
    }
    if (b) {
      in.b.bits.data := DontCare
    }
    if (c) {
      out.c.bits.data := DontCare
    }
    if (d) {
      in.d.bits.data := DontCare
    }
  }
}

object DataDontCareNode {
  def apply(a: Boolean = false, b: Boolean = false, c: Boolean = false, d: Boolean = false)(implicit p: Parameters): TLIdentityNode = {
    val dataDontCareNode = LazyModule(new DataDontCareNode(a, b, c, d))
    dataDontCareNode.node
  }
}
