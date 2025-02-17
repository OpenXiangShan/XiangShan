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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.MemReqSource
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.mem.L1PrefetchReq
import xiangshan.mem.Bundles.LsPrefetchTrainBundle
import xiangshan.cache.mmu.TlbRequestIO
import coupledL2.PrefetchCtrlFromCore

class PrefetchCtrl(implicit p: Parameters) extends XSBundle {
  val l1I_pf_enable = Bool()
  val l2_pf_enable = Bool()
  val l1D_pf_enable = Bool()
  val l1D_pf_train_on_hit = Bool()
  val l1D_pf_enable_agt = Bool()
  val l1D_pf_enable_pht = Bool()
  val l1D_pf_active_threshold = UInt(4.W)
  val l1D_pf_active_stride = UInt(6.W)
  val l1D_pf_enable_stride = Bool()
  val l2_pf_store_only = Bool()
  val l2_pf_recv_enable = Bool()
  val l2_pf_pbop_enable = Bool()
  val l2_pf_vbop_enable = Bool()
  val l2_pf_tp_enable = Bool()

  def toL2PrefetchCtrl(): PrefetchCtrlFromCore = {
    val res = Wire(new PrefetchCtrlFromCore)
    res.l2_pf_master_en := l2_pf_enable
    res.l2_pf_recv_en := l2_pf_recv_enable
    res.l2_pbop_en := l2_pf_pbop_enable
    res.l2_vbop_en := l2_pf_vbop_enable
    res.l2_tp_en := l2_pf_tp_enable
    res
  }
}

class L2PrefetchReq(implicit p: Parameters) extends XSBundle {
  val addr = UInt(PAddrBits.W)
  val source = UInt(MemReqSource.reqSourceBits.W)
}

class PrefetcherIO()(implicit p: Parameters) extends XSBundle {
  val ld_in = Flipped(Vec(backendParams.LdExuCnt, ValidIO(new LsPrefetchTrainBundle())))
  val st_in = Flipped(Vec(backendParams.StaExuCnt, ValidIO(new LsPrefetchTrainBundle())))
  val tlb_req = new TlbRequestIO(nRespDups = 2)
  val pmp_resp = Flipped(new PMPRespBundle())
  val l1_req = DecoupledIO(new L1PrefetchReq())
  val l2_req = ValidIO(new L2PrefetchReq())
  val l3_req = ValidIO(UInt(PAddrBits.W)) // TODO: l3 pf source
  val enable = Input(Bool())
}

class PrefetchReqBundle()(implicit p: Parameters) extends XSBundle {
  val vaddr       = UInt(VAddrBits.W)
  val paddr       = UInt(PAddrBits.W)
  val pc          = UInt(VAddrBits.W)
  val miss        = Bool()
  val pfHitStream = Bool()
}

trait PrefetcherParams

abstract class BasePrefecher()(implicit p: Parameters) extends XSModule {
  val io = IO(new PrefetcherIO())

  io.l3_req.valid := false.B
  io.l3_req.bits  := DontCare
}
