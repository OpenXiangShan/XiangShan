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

package xiangshan.cache.axi

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import xscache.coupledL2.{MemBackTypeMM, MemPageTypeNC}

/** Accept PMA user fields on the xbar slave side; optionally strip them toward downstream. */
class AXI4PMAUserAdapter(stripUser: Boolean = false)(implicit p: Parameters) extends LazyModule {
  private val pmaKeys = Seq(MemBackTypeMM, MemPageTypeNC)

  val node = AXI4AdapterNode(
    slaveFn = sp => sp.copy(requestKeys = pmaKeys ++ sp.requestKeys.filterNot(pmaKeys.contains)),
    masterFn = mp => mp
  )

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out).foreach { case ((in, _), (out, _)) =>
      if (stripUser) {
        in.ar.ready := out.ar.ready
        out.ar.valid := in.ar.valid
        Connectable.waiveUnmatched(out.ar.bits, in.ar.bits) match { case (lhs, rhs) => lhs :<= rhs }

        in.aw.ready := out.aw.ready
        out.aw.valid := in.aw.valid
        Connectable.waiveUnmatched(out.aw.bits, in.aw.bits) match { case (lhs, rhs) => lhs :<= rhs }

        out.w :<>= in.w
        in.r :<>= out.r
        in.b :<>= out.b
      } else {
        out :<>= in
      }
    }
  }
}

object AXI4PMAUserAdapter {
  def apply(stripUser: Boolean = false)(implicit p: Parameters, valName: ValName): AXI4Node =
    LazyModule(new AXI4PMAUserAdapter(stripUser)).node
}
