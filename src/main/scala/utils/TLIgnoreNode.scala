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

package utils

import chisel3._
import chipsalliance.rocketchip.config.Parameters
import chisel3.util.DecoupledIO
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class TLIgnoreNode()(implicit p: Parameters) extends LazyModule  {

  val xfer = TransferSizes(1, 64)

  val node : TLAdapterNode = TLAdapterNode(
    clientFn  = { _ => TLClientPortParameters(Seq(TLClientParameters(
      name          = s"Ignore",
      sourceId      = IdRange(0, 1),
      supportsProbe = xfer)))
    },
    managerFn = { m => TLManagerPortParameters(
      managers = m.managers.map { m => m.copy(
        regionType         = if (m.regionType >= RegionType.UNCACHED) RegionType.CACHED else m.regionType,
        supportsAcquireB   = xfer,
        supportsAcquireT   = xfer,
        supportsArithmetic = xfer,
        supportsLogical    = xfer,
        supportsGet        = xfer,
        supportsPutFull    = xfer,
        supportsPutPartial = xfer,
        supportsHint       = xfer,
        alwaysGrantsT      = false,
        fifoId             = None)
      },
      beatBytes  = 32,
      endSinkId  = 1,
      minLatency = 1)
    }
  )

  lazy val module = new LazyModuleImp(this) {
    for ((out, _) <- node.out) {
      out := DontCare
      out.a.valid := false.B
      out.b.ready := true.B
      out.c.valid := false.B
      out.d.ready := true.B
      out.e.valid := false.B
    }
    for ((in, _) <- node.in) {
      in := DontCare
      in.a.ready := true.B
      in.b.valid := false.B
      in.c.ready := true.B
      in.d.valid := false.B
      in.e.ready := true.B
    }
  }
}

object TLIgnoreNode {
  def apply()(implicit p: Parameters): TLAdapterNode = {
    val ignoreNode = LazyModule(new TLIgnoreNode())
    ignoreNode.node
  }
}
