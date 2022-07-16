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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.internal.naming.chiselName
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._

/** L2TLB Miss Queue
  * delay slot for reqs that pde miss in page cache
  * if pde hit in page cache, go to LLPTW instead.
  */
class L2TlbMQBundle(implicit p: Parameters) extends L2TlbInnerBundle

class L2TlbMQIO(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val in = Flipped(Decoupled(new L2TlbMQBundle()))
  val out = Decoupled(new L2TlbMQBundle())
}

class L2TlbMissQueue(implicit p: Parameters) extends XSModule with HasPtwConst {
  require(MissQueueSize >= (l2tlbParams.ifilterSize + l2tlbParams.dfilterSize))
  val io = IO(new L2TlbMQIO())

  io.out <> Queue(io.in, MissQueueSize, flush = Some(io.sfence.valid || io.csr.satp.changed))
}
