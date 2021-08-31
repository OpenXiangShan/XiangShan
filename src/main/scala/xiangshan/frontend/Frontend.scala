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

package xiangshan.frontend
import utils._
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.prefetch.L1plusPrefetcher
import xiangshan.cache.mmu.{TlbRequestIO, TlbPtwIO}
import xiangshan.backend.fu.HasExceptionNO
import system.L1CacheErrorInfo


class Frontend()(implicit p: Parameters) extends LazyModule with HasXSParameter{

  val instrUncache = LazyModule(new InstrUncache())

  lazy val module = new FrontendImp(this)
}


class FrontendImp (outer: Frontend) extends LazyModuleImp(outer)
  with HasL1plusCacheParameters
  with HasXSParameter
  with HasExceptionNO
{
  val io = IO(new Bundle() {
    val icacheMemAcq = DecoupledIO(new L1plusCacheReq)
    val icacheMemGrant = Flipped(DecoupledIO(new L1plusCacheResp))
    val l1plusFlush = Output(Bool())
    val fencei = Input(Bool())
    val ptw = new TlbPtwIO
    val backend = new FrontendToBackendIO
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val error  = new L1CacheErrorInfo
    val frontendInfo = new Bundle {
      val ibufFull  = Output(Bool())
    }
  })

  val ifu = Module(new IFU)
  val ibuffer =  Module(new Ibuffer)
  val l1plusPrefetcher = Module(new L1plusPrefetcher)
  val instrUncache = outer.instrUncache.module

  val needFlush = io.backend.redirect_cfiUpdate.valid

  // from backend
  ifu.io.redirect <> io.backend.redirect_cfiUpdate
  ifu.io.bp_ctrl <> RegNext(io.csrCtrl.bp_ctrl)
  ifu.io.commitUpdate <> io.backend.commit_cfiUpdate
  ifu.io.ftqEnqPtr <> io.backend.ftqEnqPtr
  ifu.io.ftqLeftOne <> io.backend.ftqLeftOne
  // to icache
  val grantClientId = clientId(io.icacheMemGrant.bits.id)
  val grantEntryId = entryId(io.icacheMemGrant.bits.id)
  ifu.io.icacheMemGrant.valid := io.icacheMemGrant.valid && grantClientId === icacheMissQueueId.U
  ifu.io.icacheMemGrant.bits := io.icacheMemGrant.bits
  ifu.io.icacheMemGrant.bits.id := Cat(0.U(clientIdWidth.W), grantEntryId)
  l1plusPrefetcher.io.mem_grant.valid := io.icacheMemGrant.valid && grantClientId === l1plusPrefetcherId.U
  l1plusPrefetcher.io.mem_grant.bits := io.icacheMemGrant.bits
  l1plusPrefetcher.io.mem_grant.bits.id := Cat(0.U(clientIdWidth.W), grantEntryId)
  assert(RegNext(!l1plusPrefetcher.io.mem_grant.valid || (l1plusPrefetcher.io.mem_grant.ready && grantClientId === l1plusPrefetcherId.U)))
  io.icacheMemGrant.ready := Mux(grantClientId === icacheMissQueueId.U,
    ifu.io.icacheMemGrant.ready,
    l1plusPrefetcher.io.mem_grant.ready)
  ifu.io.fencei := RegNext(io.fencei)


  instrUncache.io.req <> ifu.io.mmio_acquire
  instrUncache.io.resp <> ifu.io.mmio_grant
  instrUncache.io.flush <> ifu.io.mmio_flush
  // to tlb
  ifu.io.sfence := RegNext(io.sfence)
  ifu.io.tlbCsr := RegNext(io.tlbCsr)
  // from icache and l1plus prefetcher
  io.l1plusFlush := ifu.io.l1plusFlush
  l1plusPrefetcher.io.in.valid := ifu.io.prefetchTrainReq.valid
  l1plusPrefetcher.io.in.bits := ifu.io.prefetchTrainReq.bits
  l1plusPrefetcher.io.enable := RegNext(io.csrCtrl.l1plus_pf_enable)
  val memAcquireArb = Module(new Arbiter(new L1plusCacheReq, nClients))
  memAcquireArb.io.in(icacheMissQueueId) <> ifu.io.icacheMemAcq
  memAcquireArb.io.in(icacheMissQueueId).bits.id := Cat(icacheMissQueueId.U(clientIdWidth.W),
    entryId(ifu.io.icacheMemAcq.bits.id))
  memAcquireArb.io.in(l1plusPrefetcherId) <> l1plusPrefetcher.io.mem_acquire
  memAcquireArb.io.in(l1plusPrefetcherId).bits.id := Cat(l1plusPrefetcherId.U(clientIdWidth.W),
    entryId(l1plusPrefetcher.io.mem_acquire.bits.id))
  io.icacheMemAcq <> memAcquireArb.io.out
  // itlb to ptw
  io.ptw <> ifu.io.ptw
  // ifu to ibuffer
  ibuffer.io.in <> ifu.io.fetchPacket
  // backend to ibuffer
  ibuffer.io.flush := needFlush
  // ibuffer to backend
  io.backend.cfVec <> ibuffer.io.out
  // ifu to backend
  io.backend.fetchInfo <> ifu.io.toFtq

  io.error <> RegNext(RegNext(ifu.io.error))

  // for(out <- ibuffer.io.out){
  //   XSInfo(out.fire(),
  //     p"inst:${Hexadecimal(out.bits.instr)} pc:${Hexadecimal(out.bits.pc)}\n"
  //   )
  // }

  val frontendBubble = PopCount((0 until DecodeWidth).map(i => io.backend.cfVec(i).ready && !ibuffer.io.out(i).valid))
  XSPerfAccumulate("FrontendBubble", frontendBubble)
  val issueSlots = PopCount((0 until DecodeWidth).map(i => io.backend.cfVec(i).ready && ibuffer.io.out(i).valid))
  for (i <- 1 until DecodeWidth) {
    assert(io.backend.cfVec(i).ready === io.backend.cfVec(0).ready)
  }

  val fetchStatus = RegInit(0.U(1.W))  // 0 for latency; 1 for bandwidth
  fetchStatus := Mux(ifu.io.fetchPacket.valid, Mux(ifu.io.fetchPacket.bits.mask === 0.U, 0.U, 1.U), 0.U)

  val frontendOutCnt = PopCount(ibuffer.io.out.map(_.valid))
  val backendAccept = io.backend.cfVec(0).ready
  val latencySlotT1 = Mux(backendAccept && (frontendOutCnt === 0.U), DecodeWidth.U, 0.U)
  val bandWidthSlotT1 = frontendBubble - latencySlotT1

  val frontendOutBubble = DecodeWidth.U - frontendOutCnt
  val latencySlotT2 = Mux(backendAccept && (fetchStatus === 0.U), frontendOutBubble, 0.U)
  val bandWidthSlotT2 = Mux(backendAccept && (fetchStatus === 1.U), frontendOutBubble, 0.U)
  when (backendAccept && (fetchStatus === 1.U)) {
    assert(frontendOutBubble =/= DecodeWidth.U)
  }

  val redirectLevelLatch = HoldUnless(io.backend.redirect_cfiUpdate.bits.level, needFlush)
  val refillCnt = RegInit(0.U(2.W))
  when (needFlush || refillCnt =/= 0.U) {
    refillCnt := refillCnt + 1.U
  }
  when (refillCnt === 3.U) {
    refillCnt := 0.U
  }
  val frontendRefill = (refillCnt =/= 0.U)
  val isWalk = WireInit(false.B)
  ExcitingUtils.addSink(isWalk, "TMA_backendiswalk")
  val recoveryCondition = frontendRefill || isWalk
  val recoveryBubble = Mux(recoveryCondition, frontendBubble, 0.U)

  XSPerfAccumulate("TMA_frontendBubble", frontendBubble)
  XSPerfAccumulate("TMA_top_totalslot", DecodeWidth.U)
  XSPerfAccumulate("TMA_top_issueslot", issueSlots)
  XSPerfAccumulate("TMA_frontend_latencyT1", latencySlotT1)
  XSPerfAccumulate("TMA_frontend_bandwidthT1", bandWidthSlotT1)
  XSPerfAccumulate("TMA_frontend_latencyT2", latencySlotT2)
  XSPerfAccumulate("TMA_frontend_bandwidthT2", bandWidthSlotT2)
  XSPerfAccumulate("TMA_frontend_recoveryBubble", recoveryBubble)
  XSPerfAccumulate("TMA_frontend_recoveryBubbleBp", Mux(redirectLevelLatch === 0.U, recoveryBubble, 0.U))
  XSPerfAccumulate("TMA_frontend_recoveryBubbleLv", Mux(redirectLevelLatch =/= 0.U, recoveryBubble, 0.U))

  io.frontendInfo.ibufFull := RegNext(ibuffer.io.full)
}
