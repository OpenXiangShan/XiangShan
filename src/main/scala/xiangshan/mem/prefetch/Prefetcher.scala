/***************************************************************************************
* Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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
import coupledL2.PrefetchRecv

class PrefetcherIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
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
    val train = Vec(LdExuCnt + StaCnt, Flipped(ValidIO(new LdPrefetchTrainBundle)))
    val trainL1 = Vec(LdExuCnt, Flipped(ValidIO(new LdPrefetchTrainBundle)))
    val s2PtrChasing = Vec(LdExuCnt, Input(Bool()))
    val s1PrefetchSpec = Vec(LdExuCnt, Input(Bool()))
    val s2PrefetchSpec = Vec(LdExuCnt, Input(Bool()))
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

class Prefetcher(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasL1PrefetchSourceParameter
  with HasMemBlockParameters {

  val l2PfSenderOpt = coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new PrefetchRecv)
  )
  val l3PfSenderOpt = if (p(SoCParamsKey).L3CacheParamsOpt.nonEmpty) coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new huancun.PrefetchRecv)
  ) else None

  lazy val module = new PrefetcherImp

  class PrefetcherImp extends LazyModuleImp(this) {
    val io = IO(new PrefetcherIO)

    private def smsPfModTlbPort = 0
    private def L1PfTlbPort = 1

    val hartId = p(XSCoreParamsKey).HartId

    io.fromDCache.evict.ready := false.B
    val prefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
      case _: SMSParams =>
        val smsPfMod = Module(new SMSPrefetcher())
        smsPfMod.io_agt_en := GatedRegNextN(io.fromCtrl.csr.pf_ctrl.l1D_pf_enable_agt, 2, Some(false.B))
        smsPfMod.io_pht_en := GatedRegNextN(io.fromCtrl.csr.pf_ctrl.l1D_pf_enable_pht, 2, Some(false.B))
        smsPfMod.io_act_threshold := GatedRegNextN(io.fromCtrl.csr.pf_ctrl.l1D_pf_active_threshold, 2, Some(12.U))
        smsPfMod.io_act_stride := GatedRegNextN(io.fromCtrl.csr.pf_ctrl.l1D_pf_active_stride, 2, Some(30.U))
        smsPfMod.io_stride_en := false.B
        smsPfMod.io_dcache_evict <> io.fromDCache.evict
        smsPfMod.io.tlb_req.req <> io.toTlb(smsPfModTlbPort).req
        smsPfMod.io.tlb_req.req_kill <> io.toTlb(smsPfModTlbPort).req_kill
        smsPfMod.io.tlb_req.resp <> io.fromTlb(smsPfModTlbPort)
        smsPfMod
    }
    val l1PrefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
      case _ =>
        val pfMod = Module(new L1Prefetcher())
        pfMod.io.enable := Constantin.createRecord(s"enableL1StreamPrefetcher$hartId", initValue = true)
        pfMod.pf_ctrl <> io.fromDCache.pfCtrl
        pfMod.l2PfqBusy := io.fromBackend.l2PfqBusy
        pfMod.io.tlb_req.req <> io.toTlb(L1PfTlbPort).req
        pfMod.io.tlb_req.req_kill <> io.toTlb(L1PfTlbPort).req_kill
        pfMod.io.tlb_req.resp <> io.fromTlb(L1PfTlbPort)

        // stride will train on miss or prefetch hit
        val sourcePc = io.fromBackend.loadPc ++ io.fromBackend.hybridPc
        pfMod.stride_train.zip(io.fromMemExuBlock.trainL1).zip(sourcePc).zipWithIndex.foreach {
          case (((mod, source), sourcePc), i) =>
            mod.valid := source.valid && source.bits.isFirstIssue && (
              source.bits.miss || isFromStride(source.bits.meta_prefetch)
            )
            mod.bits := source.bits
            mod.bits.uop.pc := Mux(
              io.fromMemExuBlock.s2PtrChasing(i),
              RegEnable(sourcePc, io.fromMemExuBlock.s2PrefetchSpec(i)),
              RegEnable(
                RegEnable(sourcePc, io.fromMemExuBlock.s1PrefetchSpec(i)),
                io.fromMemExuBlock.s2PrefetchSpec(i)
              )
            )
        }

        val l1PrefetcherReq = Pipeline(
          in = pfMod.io.l1_req,
          depth = 1,
          pipe = false,
          name = Some("pf_queue_to_ldu_reg")
        )
        io.toMemExuBlock.trainReq <> l1PrefetcherReq
        pfMod
    }

    // load/store prefetch to l2 cache
    prefetcherOpt.foreach{ pfMod => pfMod.io.l1_req.ready := false.B }
    prefetcherOpt.foreach(smsPfMod => {
      l1PrefetcherOpt.foreach(l1Pf => {
        val smsPfModToL2 = DelayNWithValid(smsPfMod.io.l2_req, 2)
        val l1PfToL2 = DelayNWithValid(l1Pf.io.l2_req, 2)

        l2PfSenderOpt.get.out.head._1.addr_valid := smsPfModToL2.valid || l1PfToL2.valid
        l2PfSenderOpt.get.out.head._1.addr := Mux(l1PfToL2.valid, l1PfToL2.bits.addr, smsPfModToL2.bits.addr)
        l2PfSenderOpt.get.out.head._1.pf_source := Mux(l1PfToL2.valid, l1PfToL2.bits.source, smsPfModToL2.bits.source)
        l2PfSenderOpt.get.out.head._1.l2_pf_en := RegNextN(io.fromCtrl.csr.pf_ctrl.l2_pf_enable, 2, Some(true.B))

        smsPfMod.io.enable := RegNextN(io.fromCtrl.csr.pf_ctrl.l1D_pf_enable, 2, Some(false.B))

        val l2Trace = Wire(new LoadPfDbBundle)
        l2Trace.paddr := l2PfSenderOpt.get.out.head._1.addr
        val table = ChiselDB.createTable(s"L2PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
        table.log(l2Trace, l1PfToL2.valid, "StreamPrefetchTrace", clock, reset)
        table.log(l2Trace, !l1PfToL2.valid && smsPfModToL2.valid, "L2PrefetchTrace", clock, reset)

        val l1PfToL3 = ValidIODelay(l1Pf.io.l3_req, 4)
        l3PfSenderOpt.foreach(_.out.head._1.addr_valid := l1PfToL3.valid)
        l3PfSenderOpt.foreach(_.out.head._1.addr := l1PfToL3.bits)
        l3PfSenderOpt.foreach(_.out.head._1.l2_pf_en := RegNextN(io.fromCtrl.csr.pf_ctrl.l2_pf_enable, 4, Some(true.B)))

        val l3Trace = Wire(new LoadPfDbBundle)
        l3Trace.paddr := l3PfSenderOpt.map(_.out.head._1.addr).getOrElse(0.U)
        val l3Table = ChiselDB.createTable(s"L3PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
        l3Table.log(l3Trace, l1PfToL3.valid, "StreamPrefetchTrace", clock, reset)

        XSPerfAccumulate("prefetchFireL2", l2PfSenderOpt.get.out.head._1.addr_valid)
        XSPerfAccumulate("prefetchFireL3", l3PfSenderOpt.map(_.out.head._1.addr_valid).getOrElse(false.B))
        XSPerfAccumulate("l1PfFireL2", l1PfToL2.valid)
        XSPerfAccumulate("smsFireL2", !l1PfToL2.valid && smsPfModToL2.valid)
        XSPerfAccumulate("smsBlockByL1Pf", l1PfToL2.valid && smsPfModToL2.valid)
      })
    })

    // prfetch train: [[MemExuBlock]] -> prefetch
    val pfTrainOnHit = RegNextN(io.fromCtrl.csr.pf_ctrl.l1D_pf_train_on_hit, 2, Some(true.B))
    val prefetchSourcePc = io.fromBackend.loadPc ++ io.fromBackend.hybridPc ++ io.fromBackend.storePc
    prefetcherOpt.foreach { pfMod =>
      val prefetchReqs = pfMod.io.ld_in ++ pfMod.io.st_in
      prefetchReqs.zip(io.fromMemExuBlock.train).zip(prefetchSourcePc).zipWithIndex.foreach {
        case (((sink, source), sourcePc), i) =>
          sink.valid := Mux(pfTrainOnHit,
            source.valid,
            source.valid & source.bits.isFirstIssue && source.bits.miss
          )
          sink.bits := source.bits
          sink.bits.uop.pc := Mux(
            io.fromMemExuBlock.s2PtrChasing(i),
            RegEnable(sourcePc, io.fromMemExuBlock.s2PrefetchSpec(i)),
            RegEnable(
              RegEnable(sourcePc, io.fromMemExuBlock.s1PrefetchSpec(i)),
              io.fromMemExuBlock.s2PrefetchSpec(i)
            )
          )
      }
    }

    // only for load train L1
    l1PrefetcherOpt.foreach { pfMod =>
      pfMod.io.ld_in.zip(io.fromMemExuBlock.trainL1).foreach {
        case (sink, source) =>
          sink.valid := source.valid && source.bits.isFirstIssue
          sink.bits := source.bits
      }
    }
    l1PrefetcherOpt.foreach { pfMod =>
      pfMod.io.st_in.foreach {
        case req =>
          req.valid := false.B
          req.bits  := DontCare
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
