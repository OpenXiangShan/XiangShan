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

package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import utility.MemReqSource
import xiangshan._
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.mem.{L1PrefetchReq, LdPrefetchTrainBundle}

class PrefetcherIO()(implicit p: Parameters) extends XSBundle {
  val ld_in = Flipped(Vec(exuParameters.LduCnt, ValidIO(new LdPrefetchTrainBundle())))
  val tlb_req = new TlbRequestIO(nRespDups = 2)
  val l2_req = ValidIO(new Bundle() {
    val addr = UInt(PAddrBits.W)
    val source = UInt(MemReqSource.reqSourceBits.W)
  })
  val l1_req = DecoupledIO(new L1PrefetchReq())
  val enable = Input(Bool())
}

trait PrefetcherParams

abstract class BasePrefecher()(implicit p: Parameters) extends XSModule {
  val io = IO(new PrefetcherIO())
}