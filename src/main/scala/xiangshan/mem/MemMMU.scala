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
import xiangshan._
import xiangshan.backend.fu._
import xiangshan.backend.fu.FuType._
import xiangshan.backend.fu.util.{HasCSRConst, SdtrigExt}
import xiangshan.mem._
import xiangshan.mem.mdp._
import xiangshan.mem.prefetch.{BasePrefecher, L1Prefetcher, SMSParams, SMSPrefetcher}
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.mmu._

class MemMMUIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
  val fromCtrl = new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    val csr = Flipped(new CustomCSRCtrlIO)
    val redirect = Flipped(ValidIO(new Redirect))
  }
  // from
  val fromFrontend = new Bundle() {
    val itlb = Flipped(new TlbPtwIO())
  }
  val fromBackend = new Bundle() {
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val l2TlbReq = new Bundle() {
      val req = Flipped(DecoupledIO(new TlbReq))
      val req_kill = Input(Bool())
    }
  }
  val fromMemExuBlock = new Bundle() {
    val tlbReq = Vec(MemAddrExtCnt, new Bundle() {
      val req = Flipped(DecoupledIO(new TlbReq))
      val req_kill = Input(Bool())
    }) // lda + hyu + sta
  }
  val fromPrefetch = new Bundle() {
    val tlbReq = Vec(2, new Bundle() {
      val req = Flipped(DecoupledIO(new TlbReq))
      val req_kill = Input(Bool())
    }) // 2 prfetchers
  }
  // to
  val toBackend = new Bundle() {
    val l2TlbResp = DecoupledIO(new TlbResp(2))
    val l2PmpResp = new PMPRespBundle
  }
  val toMemExuBlock = new Bundle() {
    val tlbResp = Vec(MemAddrExtCnt, DecoupledIO(new TlbResp(2)))
    val pmpResp = Vec(MemAddrExtCnt, new PMPRespBundle())
    val hint = Vec(MemAddrExtCnt, new TlbHintReq)
  }
  val toPrefetch = new Bundle() {
    val tlbResp = Vec(2, DecoupledIO(new TlbResp(2)))
    val pmpResp = Vec(2, new PMPRespBundle())
  }
  val tlbHint = new TlbHintIO
  val robHeadVaddr = Input(Valid(UInt(VAddrBits.W)))
  val robHeadMissInDTlb = Output(Bool())
}

class MemMMU(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasMemBlockParameters
  with HasTlbConst
  with HasCSRConst {

  val ptw = LazyModule(new L2TLBWrapper())
  val ptwToL2Buffer = if (!coreParams.softPTW) LazyModule(new TLBuffer) else null
  if (!coreParams.softPTW) {
    ptwToL2Buffer.node := ptw.node
  }

  lazy val module = new MemMMUImp


  class MemMMUImp extends LazyModuleImp(this) with HasMemBlockParameters with HasPerfEvents {

    val io = IO(new MemMMUIO)

    val ldTlb = Seq(Module(new TLBNonBlock(LduCnt + HyuCnt + 1, 2, ldtlbParams)))
    val stTlb = Seq(Module(new TLBNonBlock(StaCnt, 1, sttlbParams)))
    val pfTlb = Seq(Module(new TLBNonBlock(2, 2, pftlbParams)))
    val pmp = Module(new PMP())
    val pmpCheckers = Seq.fill(DTlbSize)(Module(new PMPChecker(4, leaveHitMux = true)))
    val ptwMod = ptw.module

    // ptw
    val sfence = RegNext(RegNext(io.fromBackend.sfence))
    val tlbcsr = RegNext(RegNext(io.fromBackend.tlbCsr))
    val ptwio = Wire(new VectorTlbPtwIO(DTlbSize))

    ptwMod.io.hartId := io.fromCtrl.hartId
    ptwMod.io.sfence <> sfence
    ptwMod.io.csr.tlb <> tlbcsr
    ptwMod.io.csr.distribute_csr <> io.fromCtrl.csr.distribute_csr

    val ptwRespNext = RegEnable(ptwio.resp.bits, ptwio.resp.valid)
    val ptwRespV = RegNext(ptwio.resp.valid && !(sfence.valid && tlbcsr.satp.changed && tlbcsr.vsatp.changed && tlbcsr.hgatp.changed), init = false.B)
    ptwio.resp.ready := true.B

    // tlb
    val tlbs = (ldTlb ++ stTlb ++ pfTlb).map(_.io)
    val tlbRequestors = tlbs.map(_.requestor).flatten
    tlbs.map(_.hartId := io.fromCtrl.hartId)
    tlbs.map(_.sfence := sfence)
    tlbs.map(_.csr := tlbcsr)
    tlbs.map(_.flushPipe.map(a => a := false.B)) // non-block doesn't need
    tlbs.map(_.redirect := io.fromCtrl.redirect)
    if (refillBothTlb) {
      require(ldtlbParams.outReplace == sttlbParams.outReplace)
      require(ldtlbParams.outReplace == hytlbParams.outReplace)
      require(ldtlbParams.outReplace == pftlbParams.outReplace)
      require(ldtlbParams.outReplace)

      val replace = Module(new TlbReplace(DTlbSize, ldtlbParams))
      replace.io.apply_sep(tlbs.map(_.replace), ptwio.resp.bits.data.s1.entry.tag)
    } else {
      // TODO: there will be bugs in TlbReplace when outReplace enable, since the order of Hyu is not right.
      if (ldtlbParams.outReplace) {
        val replaceLd = Module(new TlbReplace(LduCnt + 1, ldtlbParams))
        replaceLd.io.apply_sep(ldTlb.map(_.io.replace), ptwio.resp.bits.data.s1.entry.tag)
      }
      if (hytlbParams.outReplace) {
        val replaceHy = Module(new TlbReplace(HyuCnt, hytlbParams))
        replaceHy.io.apply_sep(ldTlb.map(_.io.replace), ptwio.resp.bits.data.s1.entry.tag)
      }
      if (sttlbParams.outReplace) {
        val replaceSt = Module(new TlbReplace(StaCnt, sttlbParams))
        replaceSt.io.apply_sep(stTlb.map(_.io.replace), ptwio.resp.bits.data.s1.entry.tag)
      }
      if (pftlbParams.outReplace) {
        val replacePf = Module(new TlbReplace(2, pftlbParams))
        replacePf.io.apply_sep(pfTlb.map(_.io.replace), ptwio.resp.bits.data.s1.entry.tag)
      }
    }

    tlbs.flatMap(a => a.ptw.req).zipWithIndex.foreach {
      case (ptwReq, i) =>
        ptwReq.ready := ptwio.req(i).ready
        ptwio.req(i).bits := ptwReq.bits
         val vectorHit =
          if (refillBothTlb)
            Cat(ptwRespNext.vector).orR
          else if (i < TlbEndVec(dtlb_ld_idx))
            Cat(ptwRespNext.vector.slice(TlbStartVec(dtlb_ld_idx), TlbEndVec(dtlb_ld_idx))).orR
          else if (i < TlbEndVec(dtlb_st_idx))
            Cat(ptwRespNext.vector.slice(TlbStartVec(dtlb_st_idx), TlbEndVec(dtlb_st_idx))).orR
          else
            Cat(ptwRespNext.vector.slice(TlbStartVec(dtlb_pf_idx), TlbEndVec(dtlb_pf_idx))).orR
        ptwio.req(i).valid := ptwReq.valid && !(ptwRespV && vectorHit &&
          ptwRespNext.data.hit(ptwReq.bits.vpn, tlbcsr.satp.asid, tlbcsr.vsatp.asid, tlbcsr.hgatp.vmid,
            allType = true, ignoreAsid = true))
    }

    tlbs.foreach(_.ptw.resp.bits := ptwRespNext.data)

    if (refillBothTlb) {
      tlbs.foreach(_.ptw.resp.valid := ptwRespV && Cat(ptwRespNext.vector).orR)
    } else {
      ldTlb.foreach {
        case mod =>
          mod.io.ptw.resp.valid := ptwRespV &&
            Cat(ptwRespNext.vector.slice(TlbStartVec(dtlb_ld_idx), TlbEndVec(dtlb_ld_idx))).orR
      }
      stTlb.foreach {
        case mod =>
          mod.io.ptw.resp.valid := ptwRespV &&
            Cat(ptwRespNext.vector.slice(TlbStartVec(dtlb_st_idx), TlbEndVec(dtlb_st_idx))).orR
      }
      pfTlb.foreach {
        case mod=> mod.io.ptw.resp.valid := ptwRespV &&
          Cat(ptwRespNext.vector.slice(TlbStartVec(dtlb_pf_idx), TlbEndVec(dtlb_pf_idx))).orR
      }
    }

    ldTlb.foreach {
      case mod =>
        mod.io.ptw.resp.bits.getGpa := Cat(ptwRespNext.getGpa.take(LduCnt + HyuCnt + 1)).orR
    }
    stTlb.foreach {
      case mod =>
        mod.io.ptw.resp.bits.getGpa := Cat(ptwRespNext.getGpa.slice(LduCnt + HyuCnt + 1, LduCnt + HyuCnt + 1 + StaCnt)).orR
    }
    pfTlb.foreach {
      case mod =>
        mod.io.ptw.resp.bits.getGpa := Cat(ptwRespNext.getGpa.drop(LduCnt + HyuCnt + 1 + StaCnt)).orR
    }

    val dtlbRepeater  = PTWNewFilter(ldtlbParams.fenceDelay, ptwio, ptwMod.io.tlb(1), sfence, tlbcsr,
      l2tlbParams.dfilterSize)
    val itlbRepeater3 = PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, io.fromFrontend.itlb,
      ptwMod.io.tlb(0), sfence, tlbcsr)

    io.tlbHint <> dtlbRepeater.io.hint.get

    val tlbreplay = WireInit(VecInit(Seq.fill(LdExuCnt)(false.B)))
    val tlbreplayReg = GatedValidRegNext(tlbreplay)
    val ld0TlbReplayReg = GatedValidRegNext(ldTlb(0).io.tlbreplay)
    if (backendParams.debugEn){ dontTouch(tlbreplay) }
    for (i <- 0 until LdExuCnt) {
      tlbreplay(i) := ldTlb(0).io.ptw.req(i).valid && ptwRespNext.vector(0) && ptwRespV &&
        ptwRespNext.data.hit(ldTlb(0).io.ptw.req(i).bits.vpn, tlbcsr.satp.asid, tlbcsr.vsatp.asid, tlbcsr.hgatp.vmid,
          allType = true, ignoreAsid = true)
    }
    io.toMemExuBlock.hint.foreach(_ := DontCare)
    io.toMemExuBlock.hint.zip(dtlbRepeater.io.hint.get.req).zipWithIndex.foreach {
      case ((sink, source), i) =>
        sink.full := source.full || tlbreplayReg(i) || ld0TlbReplayReg(i)
        sink.id := source.id
    }

    // pmp
    pmpCheckers.zip(tlbs.map(_.pmp).flatten).foreach {
      case (p, tlb) =>
        p.io.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, tlb)
        require(p.io.req.bits.size.getWidth == tlb.bits.size.getWidth)
    }
    pmp.io.distribute_csr <> io.fromCtrl.csr.distribute_csr

    // mem execute block tlb requests
    tlbRequestors.patch(StreamDTLBPortIndex, Nil, 1).zip(io.fromMemExuBlock.tlbReq).foreach {
      case (sink, source) =>
        sink.req <> source.req
        sink.req_kill <> source.req_kill
    }

    // tlb responses
    io.toMemExuBlock.tlbResp.zip(tlbRequestors.patch(StreamDTLBPortIndex, Nil, 1)).foreach {
      case (sink, source) =>
        sink.valid := source.resp.valid
        source.resp.ready := sink.ready

        if (source.resp.bits.paddr.length == 1) {
          sink.bits.paddr.foreach {
            case paddr => paddr := source.resp.bits.paddr(0)
          }
          sink.bits.gpaddr.foreach {
            case gpaddr => gpaddr := source.resp.bits.gpaddr(0)
          }
          sink.bits.pbmt.foreach {
            case pbmt => pbmt := source.resp.bits.pbmt(0)
          }
          sink.bits.excp.foreach {
            case excp => excp := source.resp.bits.excp(0)
          }
          // TODO: other style?
          sink.bits.fullva := source.resp.bits.fullva
          sink.bits.miss := source.resp.bits.miss
          sink.bits.fastMiss := source.resp.bits.fastMiss
          sink.bits.isForVSnonLeafPTE := source.resp.bits.isForVSnonLeafPTE
          sink.bits.ptwBack := source.resp.bits.ptwBack
          sink.bits.memidx := source.resp.bits.memidx
          sink.bits.debug := source.resp.bits.debug
        } else {
          sink.bits := source.resp.bits
        }
    }

    // pmp responses
    io.toMemExuBlock.pmpResp.zip(pmpCheckers.map(_.io.resp).patch(StreamDTLBPortIndex, Nil, 1)).foreach {
      case (sink, source) =>
        sink <> source
    }

    // prefetch requests
    tlbRequestors(PrefetcherDTLBPortIndex).req <> io.fromPrefetch.tlbReq(0).req
    tlbRequestors(PrefetcherDTLBPortIndex).req_kill <> io.fromMemExuBlock.tlbReq(0).req_kill
    io.toPrefetch.tlbResp(0) <> tlbRequestors(PrefetcherDTLBPortIndex).resp
    io.toPrefetch.pmpResp(0) <> pmpCheckers(PrefetcherDTLBPortIndex).io.resp

    // stream request
    tlbRequestors(StreamDTLBPortIndex).req <> io.fromPrefetch.tlbReq(1).req
    tlbRequestors(StreamDTLBPortIndex).req_kill <> io.fromPrefetch.tlbReq(1).req_kill
    io.toPrefetch.tlbResp(1) <> tlbRequestors(StreamDTLBPortIndex).resp
    io.toPrefetch.pmpResp(1) <> pmpCheckers(StreamDTLBPortIndex).io.resp

    // l2 tlb request
    tlbRequestors(L2toL1DLBPortIndex).req <> io.fromBackend.l2TlbReq.req
    tlbRequestors(L2toL1DLBPortIndex).req_kill <> io.fromBackend.l2TlbReq.req_kill
    io.toBackend.l2TlbResp <> tlbRequestors(L2toL1DLBPortIndex).resp
    io.toBackend.l2PmpResp <> pmpCheckers(L2toL1DLBPortIndex).io.resp
    tlbRequestors(L2toL1DLBPortIndex).resp.ready := true.B

    // topdown
    dtlbRepeater.io.debugTopDown.robHeadVaddr := io.robHeadVaddr
    io.robHeadMissInDTlb := dtlbRepeater.io.rob_head_miss_in_tlb

    // reset tree of MemBlock
    if (p(DebugOptionsKey).ResetGen) {
      val leftResetTree = ResetGenNode(
        Seq(
          ModuleNode(ptwMod),
          ModuleNode(ptwToL2Buffer.module),
        )
        ++ ldTlb.map(mod => ModuleNode(mod))
        ++ stTlb.map(mod => ModuleNode(mod))
      )
      val rightResetTree = ResetGenNode(
        Seq(
          ModuleNode(pmp)
        )
        ++ pmpCheckers.map(ModuleNode(_))
        ++ pfTlb.map(mod => ModuleNode(mod))
      )
      ResetGen(leftResetTree, reset, sim = false)
      ResetGen(rightResetTree, reset, sim = false)
    }

    val perfEventsPTW = if (!coreParams.softPTW) {
      ptwMod.getPerfEvents
    } else {
      Seq()
    }
    val perfEvents = perfEventsPTW.map(x => ("PTW_" + x._1, x._2))
    generatePerfEvent()
  }

}
