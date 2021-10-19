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
package xiangshan.cache.mmu

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import xiangshan.{SfenceBundle, XSModule}
import utils._

class L2TlbPrefetchIO(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val in = Flipped(ValidIO(new Bundle {
    val vpn = UInt(vpnLen.W)
  }))
  val out = DecoupledIO(new Bundle {
    val vpn = UInt(vpnLen.W)
    val source = UInt(bSourceWidth.W)
  })
}

class L2TlbPrefetch(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new L2TlbPrefetchIO())

  val flush = io.sfence.valid || io.csr.satp.changed
  val next_line = RegEnable(get_next_line(io.in.bits.vpn), io.in.valid)
  val v = ValidHold(io.in.valid && !flush, io.out.fire(), flush)

  io.out.valid := v
  io.out.bits.vpn := next_line
  io.out.bits.source := prefetchID.U
}
