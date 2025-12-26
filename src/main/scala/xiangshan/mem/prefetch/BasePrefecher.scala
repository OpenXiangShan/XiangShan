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
import coupledL2.PrefetchCtrlFromCore
import org.chipsalliance.cde.config.Parameters
import utility.MemReqSource
import xiangshan._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache._
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.mem.Bundles.LsPrefetchTrainBundle
import xiangshan.mem.L1PrefetchReq

object PrefetchTarget extends Enumeration{
  val L1 = Value("toL1")
  val L2 = Value("toL2")
  val L3 = Value("toL3")

  val PfTgtCnt = Value("PrefetchTargetCount")
  val PfTgtBits = log2Ceil(PfTgtCnt.id)
}

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
  val l2_pf_delay_latency = UInt(10.W)
  val berti_enable = Bool()

  def toL2PrefetchCtrl(): PrefetchCtrlFromCore = {
    val res = Wire(new PrefetchCtrlFromCore)
    res.l2_pf_master_en := l2_pf_enable
    res.l2_pf_recv_en := l2_pf_recv_enable
    res.l2_pbop_en := l2_pf_pbop_enable
    res.l2_vbop_en := l2_pf_vbop_enable
    res.l2_tp_en := l2_pf_tp_enable
    res.l2_pf_delay_latency := l2_pf_delay_latency
    res
  }
}

class L2PrefetchReq(implicit p: Parameters) extends XSBundle {
  val addr = UInt(PAddrBits.W)
  val source = UInt(MemReqSource.reqSourceBits.W)
}

class L3PrefetchReq(implicit p: Parameters) extends L2PrefetchReq

class TrainReqBundle()(implicit p: Parameters) extends DCacheBundle {
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val pc = UInt(VAddrBits.W)
  val miss = Bool()
  val metaSource = UInt(L1PfSourceBits.W)
  val refillLatency = UInt(LATENCY_WIDTH.W)
}

class SourcePrefetchReq()(implicit p: Parameters) extends DCacheBundle {
  val triggerPC = UInt(VAddrBits.W)
  val triggerVA = UInt(VAddrBits.W)
  val prefetchVA = UInt(VAddrBits.W)
  val prefetchTarget = UInt(PrefetchTarget.PfTgtBits.W)
}

class PrefetcherIO()(implicit p: Parameters) extends XSBundle {
  val enable = Input(Bool())
  val ld_in = Flipped(Vec(backendParams.LdExuCnt, ValidIO(new LsPrefetchTrainBundle())))
  val st_in = Flipped(Vec(backendParams.StaExuCnt, ValidIO(new LsPrefetchTrainBundle())))
  val tlb_req = new TlbRequestIO(nRespDups = 2)
  val pmp_resp = Flipped(new PMPRespBundle())
  val l1_req = DecoupledIO(new L1PrefetchReq())
  val l2_req = DecoupledIO(new L2PrefetchReq())
  val l3_req = DecoupledIO(new L3PrefetchReq())
}

class BertiPrefetcherIO()(implicit p: Parameters) extends PrefetcherIO {
  val refillTrain = Flipped(ValidIO(new TrainReqBundle()))
}

class PrefetchReqBundle()(implicit p: Parameters) extends XSBundle with HasDCacheParameters {
  val vaddr       = UInt(VAddrBits.W)
  val paddr       = UInt(PAddrBits.W)
  val pc          = UInt(VAddrBits.W)
  val accessVec  = UInt((blockBytes/DCacheWordBytes).W)
  val miss        = Bool()
  val pfHitStream = Bool()
}

abstract class BasePrefecher()(implicit p: Parameters) extends XSModule
  with PrefetcherParams
  with HasDCacheParameters
{
  lazy val io: PrefetcherIO = IO(new PrefetcherIO())

  // By default, there are no tlb transformations, l2_req and l3_req
  io.tlb_req.req.valid := false.B
  io.tlb_req.req.bits := DontCare
  io.tlb_req.req_kill := false.B
  io.tlb_req.resp.ready := true.B
  io.l2_req.valid := false.B
  io.l2_req.bits  := DontCare
  io.l3_req.valid := false.B
  io.l3_req.bits  := DontCare
}
