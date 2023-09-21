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

class DebugIdentityNode()(implicit p: Parameters) extends LazyModule  {

  val node = TLIdentityNode()

  val n = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(
      TLMasterParameters.v1("debug node")
    )
  )))

  lazy val module = new LazyModuleImp(this) with HasTLDump {
    val (out, _) = node.out(0)
    val (in, _) = node.in(0)

    def debug(t: TLBundle, valid: Boolean = false): Unit ={
      def fire[T <: Data](x: DecoupledIO[T]) = if(valid) x.valid else x.fire
      val channels = Seq(t.a, t.b, t.c, t.d, t.e)
      channels.foreach(c =>
        when(fire(c)){
          XSDebug(" isFire:%d ",c.fire)
          c.bits.dump
        }
      )
    }
    debug(in, false)
  }
}

object DebugIdentityNode {
  def apply()(implicit p: Parameters): TLIdentityNode = {
    val identityNode = LazyModule(new DebugIdentityNode())
    identityNode.node
  }
}
