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

  def genTlbReq() = new Bundle() {
    val req = Flipped(DecoupledIO(new TlbReq))
    val req_kill = Input(Bool())
  }
  val fromBackend = new Bundle() {
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val l2TlbReq = genTlbReq()
  }
  val fromMemExuBlock = new Bundle() {
    val tlbReq = Vec(MemAddrExtCnt, genTlbReq()) // lda + hyu + sta
  }
  val fromPrefetch = new Bundle() {
    val tlbReq = Vec(2, genTlbReq()) // 2 prfetchers
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
    val tlbResp = Vec(MemAddrExtCnt, DecoupledIO(new TlbResp(2)))
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

    private val fromCtrl = io.fromCtrl
    private val fromFrontend = io.fromFrontend
    private val (fromBackend, toBackend) = (io.fromBackend, io.toBackend)
    private val (fromMemExuBlock, toMemExuBlock) = (io.fromMemExuBlock, io.toMemExuBlock)
    private val (fromPrefetch, toPrefetch) = (io.fromPrefetch, io.toPrefetch)

    val ldTlb = Seq(Module(new TLBNonBlock(LduCnt + HyuCnt + 1, 2, ldtlbParams)))
    val stTlb = Seq(Module(new TLBNonBlock(StaCnt, 1, sttlbParams)))
    val pfTlb = Seq(Module(new TLBNonBlock(2, 2, pftlbParams)))
    val pmp = Module(new PMP())
    val pmpCheckers = Seq.fill(DTlbSize)(Module(new PMPChecker(4, leaveHitMux = true)))
    val ptwMod = ptw.module

    // ptw
    val sfence = RegNext(RegNext(fromBackend.sfence))
    val tlbcsr = RegNext(RegNext(fromBackend.tlbCsr))
    val ptwio = Wire(new VectorTlbPtwIO(DTlbSize))

    ptwMod.io.hartId := fromCtrl.hartId
    ptwMod.io.sfence <> sfence
    ptwMod.io.csr.tlb <> tlbcsr
    ptwMod.io.csr.distribute_csr <> fromCtrl.csr.distribute_csr

    val ptwRespNext = RegEnable(ptwio.resp.bits, ptwio.resp.valid)
    val ptwRespV = RegNext(ptwio.resp.valid && !(sfence.valid && tlbcsr.satp.changed && tlbcsr.vsatp.changed && tlbcsr.hgatp.changed), init = false.B)
    ptwio.resp.ready := true.B

    // tlb
    val tlbs = (ldTlb ++ stTlb ++ pfTlb).map(_.io)
    val tlbReqs = tlbs.map(_.requestor).flatten
    tlbs.map(_.hartId := fromCtrl.hartId)
    tlbs.map(_.sfence := sfence)
    tlbs.map(_.csr := tlbcsr)
    tlbs.map(_.flushPipe.map(a => a := false.B)) // non-block doesn't need
    tlbs.map(_.redirect := fromCtrl.redirect)
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
      case (tlb, i) =>
        tlb.ready := ptwio.req(i).ready
        ptwio.req(i).bits := tlb.bits
         val vectorHit =
          if (refillBothTlb)
            Cat(ptwRespNext.vector).orR
          else if (i < TlbEndVec(ldIdx))
            Cat(ptwRespNext.vector.slice(TlbStartVec(ldIdx), TlbEndVec(ldIdx))).orR
          else if (i < TlbEndVec(stIdx))
            Cat(ptwRespNext.vector.slice(TlbStartVec(stIdx), TlbEndVec(stIdx))).orR
          else
            Cat(ptwRespNext.vector.slice(TlbStartVec(pfIdx), TlbEndVec(pfIdx))).orR
        ptwio.req(i).valid := tlb.valid && !(ptwRespV && vectorHit &&
          ptwRespNext.data.hit(tlb.bits.vpn, tlbcsr.satp.asid, tlbcsr.vsatp.asid, tlbcsr.hgatp.vmid,
            allType = true, ignoreAsid = true))
    }

    tlbs.foreach(_.ptw.resp.bits := ptwRespNext.data)

    if (refillBothTlb) {
      tlbs.foreach(_.ptw.resp.valid := ptwRespV && Cat(ptwRespNext.vector).orR)
    } else {
      ldTlb.foreach {
        case tlb =>
          tlb.io.ptw.resp.valid := ptwRespV &&
            Cat(ptwRespNext.vector.slice(TlbStartVec(ldIdx), TlbEndVec(ldIdx))).orR
      }
      stTlb.foreach {
        case tlb =>
          tlb.io.ptw.resp.valid := ptwRespV &&
            Cat(ptwRespNext.vector.slice(TlbStartVec(stIdx), TlbEndVec(stIdx))).orR
      }
      pfTlb.foreach {
        case tlb => tlb.io.ptw.resp.valid := ptwRespV &&
          Cat(ptwRespNext.vector.slice(TlbStartVec(pfIdx), TlbEndVec(pfIdx))).orR
      }
    }

    ldTlb.foreach {
      case tlb =>
        tlb.io.ptw.resp.bits.getGpa := Cat(ptwRespNext.getGpa.take(LduCnt + HyuCnt + 1)).orR
    }
    stTlb.foreach {
      case tlb =>
        tlb.io.ptw.resp.bits.getGpa := Cat(ptwRespNext.getGpa.slice(LduCnt + HyuCnt + 1, LduCnt + HyuCnt + 1 + StaCnt)).orR
    }
    pfTlb.foreach {
      case tlb =>
        tlb.io.ptw.resp.bits.getGpa := Cat(ptwRespNext.getGpa.drop(LduCnt + HyuCnt + 1 + StaCnt)).orR
    }

    val dtlbRepeater  = PTWNewFilter(ldtlbParams.fenceDelay, ptwio, ptwMod.io.tlb(1), sfence, tlbcsr,
      l2tlbParams.dfilterSize)
    val itlbRepeater3 = PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, fromFrontend.itlb,
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
    toMemExuBlock.hint.foreach(_ := DontCare)
    toMemExuBlock.hint.zip(dtlbRepeater.io.hint.get.req).zipWithIndex.foreach {
      case ((sink, source), i) =>
        sink.full := source.full || tlbreplayReg(i) || ld0TlbReplayReg(i)
        sink.id := source.id
    }

    // pmp
    pmp.io.distribute_csr <> fromCtrl.csr.distribute_csr
    pmpCheckers.zip(tlbs.map(_.pmp).flatten).foreach {
      case (p, tlb) =>
        p.io.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, tlb)
        require(p.io.req.bits.size.getWidth == tlb.bits.size.getWidth)
    }

    // l2 tlb request
    tlbReqs(L2toL1DLBPortIndex).req <> fromBackend.l2TlbReq.req
    tlbReqs(L2toL1DLBPortIndex).req_kill <> fromBackend.l2TlbReq.req_kill
    toBackend.l2TlbResp <> tlbReqs(L2toL1DLBPortIndex).resp
    toBackend.l2PmpResp <> pmpCheckers(L2toL1DLBPortIndex).io.resp
    tlbReqs(L2toL1DLBPortIndex).resp.ready := true.B

    // prefetch requests
    tlbReqs(PrefetcherDTLBPortIndex).req <> fromPrefetch.tlbReq(0).req
    tlbReqs(PrefetcherDTLBPortIndex).req_kill <> fromMemExuBlock.tlbReq(0).req_kill
    toPrefetch.tlbResp(0) <> tlbReqs(PrefetcherDTLBPortIndex).resp

    tlbReqs(StreamDTLBPortIndex).req <> fromMemExuBlock.tlbReq(1).req
    tlbReqs(StreamDTLBPortIndex).req_kill <> fromMemExuBlock.tlbReq(1).req_kill
    toPrefetch.tlbResp(1) <> tlbReqs(StreamDTLBPortIndex).resp

    // mem execute block tlb requests
    tlbReqs.zip(fromMemExuBlock.tlbReq).foreach {
      case (sink, source) =>
        sink.req <> source.req
        sink.req_kill <> source.req_kill
    }

    // tlb responses
    toMemExuBlock.tlbResp.zip(tlbReqs).foreach {
      case (sink, source) =>
        if (source.resp.bits.paddr.length == 1) {
          sink.valid := source.resp.valid
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
          sink <> source.resp
        }
    }

    // pmp responses
    toMemExuBlock.pmpResp.zip(pmpCheckers.map(_.io.resp)).foreach {
      case (sink, source) =>
        sink <> source
    }

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
