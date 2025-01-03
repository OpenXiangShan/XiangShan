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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.tile.HasFPUParameters
import freechips.rocketchip.tilelink._
import utils._
import utility._
import system.SoCParamsKey
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.fu._
import xiangshan.backend.fu.FuType._
import xiangshan.backend.fu.util.{HasCSRConst, SdtrigExt}
import xiangshan.backend.{BackendToTopBundle, TopToBackendBundle}
import xiangshan.backend.rob.{RobDebugRollingIO, RobPtr, RobLsqIO}
import xiangshan.backend.datapath.NewPipelineConnect
import xiangshan.backend.fu.NewCSR.TriggerUtil
import xiangshan.mem._
import xiangshan.mem.mdp._
import xiangshan.mem.prefetch._
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.mmu._
import coupledL2.{PrefetchRecv}

class MemPrefetcherIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
  val fromCtrl = new Bundle() {
    val csr = Flipped(new CustomCSRCtrlIO)
  }

  // from
  val fromBackend = Input(new Bundle() {
    val loadPc = Vec(LduCnt, UInt(VAddrBits.W))
    val storePc = Vec(StaCnt, UInt(VAddrBits.W))
    val hybridPc = Vec(HyuCnt, UInt(VAddrBits.W))

    val l2PfqBusy = Bool()
  })
  val fromDCache = new Bundle() {
    val evict = Flipped(DecoupledIO(new AGTEvictReq))
    val pfCtrl = Input(new PrefetchControlBundle)
  }
  val fromMemExuBlock = new Bundle() {
    val train = Vec(LdExuCnt + StaCnt, Flipped(new LsPrefetchTrainIO))
    val trainL1 = Vec(LdExuCnt, Flipped(new LsPrefetchTrainIO))
  }
  val fromTlb = Vec(2, Flipped(DecoupledIO(new TlbResp(2)))) // port 0: stream, port 1: l1 prefetch

  // to
  val toMemExuBlock = new Bundle() {
    val trainReq = Decoupled(new L1PrefetchReq())
  }
  val toTlb = Vec(2, new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  })
}

class MemPrefetcher(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasL1PrefetchSourceParameter
  with HasMemBlockParameters {

  val l2PfSenderOpt = coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new PrefetchRecv)
  )
  val l3PfSenderOpt = if (p(SoCParamsKey).L3CacheParamsOpt.nonEmpty) coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new huancun.PrefetchRecv)
  ) else None

  lazy val module = new MemPrefetcherImp

  class MemPrefetcherImp extends LazyModuleImp(this) {
    val io = IO(new MemPrefetcherIO)

    private val fromCtrl = io.fromCtrl
    private val fromBackend = io.fromBackend
    private val fromDCache = io.fromDCache
    private val (fromMemExuBlock, toMemExuBlock) = (io.fromMemExuBlock, io.toMemExuBlock)
    private val (fromTlb, toTlb) = (io.fromTlb, io.toTlb)

    val hartId = p(XSCoreParamsKey).HartId

    fromDCache.evict.ready := false.B
    val prefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
      case _: SMSParams =>
        val sms = Module(new SMSPrefetcher())
        sms.io_agt_en := GatedRegNextN(fromCtrl.csr.l1D_pf_enable_agt, 2, Some(false.B))
        sms.io_pht_en := GatedRegNextN(fromCtrl.csr.l1D_pf_enable_pht, 2, Some(false.B))
        sms.io_act_threshold := GatedRegNextN(fromCtrl.csr.l1D_pf_active_threshold, 2, Some(12.U))
        sms.io_act_stride := GatedRegNextN(fromCtrl.csr.l1D_pf_active_stride, 2, Some(30.U))
        sms.io_stride_en := false.B
        sms.io_dcache_evict <> fromDCache.evict
        sms.io.tlb_req.req <> toTlb(0).req
        sms.io.tlb_req.req_kill <> toTlb(0).req_kill
        sms.io.tlb_req.resp <> fromTlb(0)
        sms
    }
    val l1PrefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
      case _ =>
        val l1Prefetcher = Module(new L1Prefetcher())
        l1Prefetcher.io.enable := Constantin.createRecord(s"enableL1StreamPrefetcher$hartId", initValue = true)
        l1Prefetcher.pf_ctrl <> fromDCache.pfCtrl
        l1Prefetcher.l2PfqBusy := fromBackend.l2PfqBusy
        l1Prefetcher.io.tlb_req.req <> toTlb(1).req
        l1Prefetcher.io.tlb_req.req_kill <> toTlb(1).req_kill
        l1Prefetcher.io.tlb_req.resp <> fromTlb(1)

        // stride will train on miss or prefetch hit
        val sourcePc = fromBackend.loadPc ++ fromBackend.hybridPc
        l1Prefetcher.stride_train.zip(fromMemExuBlock.trainL1).zip(sourcePc).foreach {
          case ((pf, source), sourcePc) =>
            pf.valid := source.req.valid && source.req.bits.isFirstIssue && (
              source.req.bits.miss || isFromStride(source.req.bits.metaPrefetch)
            )
            pf.bits := source.req.bits
            pf.bits.uop.pc := RegEnable(RegEnable(sourcePc, source.s1_prefetchSpec), source.s2_prefetchSpec)
        }

        val l1PrefetcherReq = Pipeline(
          in = l1Prefetcher.io.l1_req,
          depth = 1,
          pipe = false,
          name = Some("pf_queue_to_ldu_reg")
        )
        toMemExuBlock.trainReq <> l1PrefetcherReq
        l1Prefetcher
    }

    // load/store prefetch to l2 cache
    prefetcherOpt.foreach{ pf => pf.io.l1_req.ready := false.B }
    prefetcherOpt.foreach(smsPf => {
      l1PrefetcherOpt.foreach(l1Pf => {
        val smsPfToL2 = DelayNWithValid(smsPf.io.l2_req, 2)
        val l1PfToL2 = DelayNWithValid(l1Pf.io.l2_req, 2)

        l2PfSenderOpt.get.out.head._1.addr_valid := smsPfToL2.valid || l1PfToL2.valid
        l2PfSenderOpt.get.out.head._1.addr := Mux(l1PfToL2.valid, l1PfToL2.bits.addr, smsPfToL2.bits.addr)
        l2PfSenderOpt.get.out.head._1.pf_source := Mux(l1PfToL2.valid, l1PfToL2.bits.source, smsPfToL2.bits.source)
        l2PfSenderOpt.get.out.head._1.l2_pf_en := RegNextN(fromCtrl.csr.l2_pf_enable, 2, Some(true.B))

        smsPf.io.enable := RegNextN(fromCtrl.csr.l1D_pf_enable, 2, Some(false.B))

        val l2Trace = Wire(new LoadPfDbBundle)
        l2Trace.paddr := l2PfSenderOpt.get.out.head._1.addr
        val table = ChiselDB.createTable(s"L2PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
        table.log(l2Trace, l1PfToL2.valid, "StreamPrefetchTrace", clock, reset)
        table.log(l2Trace, !l1PfToL2.valid && smsPfToL2.valid, "L2PrefetchTrace", clock, reset)

        val l1PfToL3 = ValidIODelay(l1Pf.io.l3_req, 4)
        l3PfSenderOpt.foreach(_.out.head._1.addr_valid := l1PfToL3.valid)
        l3PfSenderOpt.foreach(_.out.head._1.addr := l1PfToL3.bits)
        l3PfSenderOpt.foreach(_.out.head._1.l2_pf_en := RegNextN(fromCtrl.csr.l2_pf_enable, 4, Some(true.B)))

        val l3Trace = Wire(new LoadPfDbBundle)
        l3Trace.paddr := l3PfSenderOpt.map(_.out.head._1.addr).getOrElse(0.U)
        val l3_table = ChiselDB.createTable(s"L3PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
        l3_table.log(l3Trace, l1PfToL3.valid, "StreamPrefetchTrace", clock, reset)

        XSPerfAccumulate("prefetchFireL2", l2PfSenderOpt.get.out.head._1.addr_valid)
        XSPerfAccumulate("prefetchFireL3", l3PfSenderOpt.map(_.out.head._1.addr_valid).getOrElse(false.B))
        XSPerfAccumulate("l1PfFireL2", l1PfToL2.valid)
        XSPerfAccumulate("smsFireL2", !l1PfToL2.valid && smsPfToL2.valid)
        XSPerfAccumulate("smsBlockByL1Pf", l1PfToL2.valid && smsPfToL2.valid)
      })
    })

    // prfetch train: [[MemExuBlock]] -> prefetch
    val pfTrainOnHit = RegNextN(fromCtrl.csr.l1D_pf_train_on_hit, 2, Some(true.B))
    val prefetchSourcePc = fromBackend.loadPc ++ fromBackend.hybridPc ++ fromBackend.storePc
    prefetcherOpt.foreach { pfMod =>
      val prefetchReqs = pfMod.io.ld_in ++ pfMod.io.st_in
      prefetchReqs.zip(fromMemExuBlock.train).zip(prefetchSourcePc).zipWithIndex.foreach {
        case (((pf, source), sourcePc), i) =>
          pf.valid := Mux(pfTrainOnHit,
            source.req.valid,
            source.req.valid & source.req.bits.isFirstIssue && source.req.bits.miss
          )
          pf.bits := source.req.bits
          pf.bits.uop.pc := RegEnable(RegEnable(sourcePc, source.s1_prefetchSpec), source.s2_prefetchSpec)
      }
    }

    // only for load train L1
    l1PrefetcherOpt.foreach { pfMod =>
      pfMod.io.ld_in.zip(fromMemExuBlock.trainL1).foreach {
        case (pf, source) =>
          pf.valid := source.req.valid && source.req.bits.isFirstIssue
          pf.bits  := source.req.bits
      }
    }
    l1PrefetcherOpt.foreach { pfMod =>
      pfMod.io.st_in.foreach {
        case pf =>
          pf.valid := false.B
          pf.bits  := DontCare
      }
    }

    // reset tree of MemBlock
    if (p(DebugOptionsKey).ResetGen) {
      val resetTree = ResetGenNode((if (prefetcherOpt.isDefined) Seq(ModuleNode(prefetcherOpt.get)) else Nil) ++
        (if (l1PrefetcherOpt.isDefined) Seq(ModuleNode(l1PrefetcherOpt.get)) else Nil))
      ResetGen(resetTree, reset, sim = false)
    }
  }
}
