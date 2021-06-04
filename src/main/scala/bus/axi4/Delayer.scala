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

// See LICENSE.SiFive for license details.

package bus.axi4

import chisel3._
import chisel3.util._
import utils._

class AXI4Delayer[T <: AXI4Lite](latency: Int = 0, _type: T = new AXI4) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(_type)
    val out = Flipped(Flipped(_type))
  })

  io.out.ar <> LatencyPipe(io.in.ar, latency)
  io.out.aw <> LatencyPipe(io.in.aw, latency)
  io.out.w  <> io.in.w
  io.in.b   <> io.out.b
  io.in.r   <> io.out.r
}
