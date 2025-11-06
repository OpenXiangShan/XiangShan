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

package bus.axi4

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.util._

class AXI4ToAXI4Lite(inType: AXI4) extends Module {
  val in = IO(Flipped(inType))
  val out = IO(new AXI4Lite)

  def connect(lite: Data, full: Data): Unit = {
    (lite, full) match {
      case (e1: Element, e2: Element) =>
        e1 <> e2
      case (r1: Record, r2: Record) =>
        r2 <> DontCare
        for((s, d) <- r1.elements){
          connect(d, r2.elements(s))
        }
    }
  }

  connect(out, in)
}

object AXI4ToAXI4Lite {
  def apply(in: AXI4): AXI4Lite = {
    val m = Module(new AXI4ToAXI4Lite(in.cloneType))
    m.in <> in
    m.out
  }
}
