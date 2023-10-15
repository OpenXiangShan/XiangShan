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
import org.chipsalliance.cde.config.Parameters
import utility.MemReqSource
import xiangshan._
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.mem.{L1PrefetchReq, L2PrefetchTrainBundle, LdPrefetchTrainBundle, StPrefetchTrainBundle}

class L2PrefetchReq(implicit p: Parameters) extends XSBundle {
  val addr = UInt(PAddrBits.W)
  val source = UInt(MemReqSource.reqSourceBits.W)
  val needT = Bool()
}

class L2PrefetchConnectIO(implicit p:Parameters) extends XSBundle{
  val train = Flipped(ValidIO(new L2PrefetchTrainBundle()))
}

class PrefetcherIO()(implicit p: Parameters) extends XSBundle {
  val ld_in = Flipped(Vec(exuParameters.LduCnt, ValidIO(new LdPrefetchTrainBundle())))
  val st_in = Flipped(Vec(exuParameters.StuCnt, ValidIO(new StPrefetchTrainBundle())))
  val tlb_req = new TlbRequestIO(nRespDups = 2)
  val l1_req = DecoupledIO(new L1PrefetchReq())
  val l2_req = ValidIO(new L2PrefetchReq())
  val l3_req = ValidIO(UInt(PAddrBits.W)) // TODO: l3 pf source
  val enable = Input(Bool())
}

class PrefetchReqBundle()(implicit p: Parameters) extends XSBundle {
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val pc    = UInt(VAddrBits.W)
  val needT = Bool()
}

trait PrefetcherParams

abstract class BasePrefecher()(implicit p: Parameters) extends XSModule {
  val io = IO(new PrefetcherIO())

  io.l3_req.valid := false.B
  io.l3_req.bits  := DontCare
}