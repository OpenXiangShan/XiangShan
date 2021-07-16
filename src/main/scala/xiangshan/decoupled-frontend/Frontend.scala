/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
    val backend = new FrontendToCtrlIO
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val error  = new L1CacheErrorInfo
    val frontendInfo = new Bundle {
      val ibufFull  = Output(Bool())
      val bpuInfo = new Bundle {
        val bpRight = Output(UInt(XLEN.W))
        val bpWrong = Output(UInt(XLEN.W))
      }
    }
  })

  val bpu = Module(new FakeBPU)
  val ifu = Module(new NewIFU)
  val ibuffer =  Module(new Ibuffer)
  val l1plusPrefetcher = Module(new L1plusPrefetcher)
  val icacheMeta = Module(new ICacheMetaArray)
  val icacheData = Module(new ICacheDataArray)
  val icacheMissQueue = Module(new ICacheMissQueue)
  val instrUncache = outer.instrUncache.module
  val ftq = Module(new Ftq)

  val needFlush = io.backend.redirect_cfiUpdate.valid

  //IFU-Ftq
  ifu.io.ftqInter.fromFtq <> ftq.io.toIfu
  ftq.io.fromIfu          <> ifu.io.ftqInter.toFtq
  bpu.io.ftq_to_bpu       <> ftq.io.toBpu
  ftq.io.fromBpu          <> bpu.io.bpu_to_ftq
  //IFU-ICache
  ifu.io.icacheInter.toIMeta      <> icacheMeta.io.read
  ifu.io.icacheInter.fromIMeta    <> icacheMeta.io.readResp
  ifu.io.icacheInter.toIData      <> icacheData.io.read
  ifu.io.icacheInter.fromIData    <> icacheData.io.readResp

  for(i <- 0 until 2){
    ifu.io.icacheInter.fromMissQueue(i) <> icacheMissQueue.io.resp(i) 
    icacheMissQueue.io.req(i)           <> ifu.io.icacheInter.toMissQueue(i)
  }

  icacheMissQueue.io.flush := ifu.io.ftqInter.fromFtq.redirect.valid

  ifu.io.iTLBInter.resp <> DontCare
  ifu.io.iTLBInter.req.ready := true.B 

  //IFU-Ibuffer
  ifu.io.toIbuffer    <> ibuffer.io.in

  //ICache
  icacheMeta.io.write <> icacheMissQueue.io.meta_write
  icacheData.io.write <> icacheMissQueue.io.data_write

  // to icache
  val grantClientId = clientId(io.icacheMemGrant.bits.id)
  val grantEntryId = entryId(io.icacheMemGrant.bits.id)

  l1plusPrefetcher.io.mem_grant.valid := io.icacheMemGrant.valid && grantClientId === l1plusPrefetcherId.U
  l1plusPrefetcher.io.mem_grant.bits := io.icacheMemGrant.bits
  l1plusPrefetcher.io.mem_grant.bits.id := Cat(0.U(clientIdWidth.W), grantEntryId)
  assert(RegNext(!l1plusPrefetcher.io.mem_grant.valid || (l1plusPrefetcher.io.mem_grant.ready && grantClientId === l1plusPrefetcherId.U)))
  io.icacheMemGrant.ready := Mux(grantClientId === icacheMissQueueId.U,
    icacheMissQueue.io.mem_grant.ready,
    l1plusPrefetcher.io.mem_grant.ready)
  //ifu.io.fencei := RegNext(io.fencei)
  icacheMissQueue.io.mem_grant.valid  :=  io.icacheMemGrant.valid 
  icacheMissQueue.io.mem_grant.bits   :=  io.icacheMemGrant.bits

  ftq.io.fromBackend <> io.backend.toFtq
  io.backend.fromFtq <> ftq.io.toBackend
  io.frontendInfo.bpuInfo <> ftq.io.bpuInfo


  instrUncache.io.req   <> DontCare
  instrUncache.io.resp  <> DontCare
  instrUncache.io.flush <> DontCare
  // to tlb


  // from icache and l1plus prefetcher
  io.l1plusFlush := DontCare
  l1plusPrefetcher.io.in.valid := DontCare
  l1plusPrefetcher.io.in.bits := DontCare
  l1plusPrefetcher.io.enable := RegNext(io.csrCtrl.l1plus_pf_enable)
  val memAcquireArb = Module(new Arbiter(new L1plusCacheReq, nClients))
  memAcquireArb.io.in(icacheMissQueueId) <> icacheMissQueue.io.mem_acquire
  memAcquireArb.io.in(icacheMissQueueId).bits.id := Cat(icacheMissQueueId.U(clientIdWidth.W),
    entryId(icacheMissQueue.io.mem_acquire.bits.id))
  memAcquireArb.io.in(l1plusPrefetcherId) <> l1plusPrefetcher.io.mem_acquire
  memAcquireArb.io.in(l1plusPrefetcherId).bits.id := Cat(l1plusPrefetcherId.U(clientIdWidth.W),
    entryId(l1plusPrefetcher.io.mem_acquire.bits.id))
  io.icacheMemAcq <> memAcquireArb.io.out
  // itlb to ptw
  io.ptw <> DontCare
  // backend to ibuffer
  ibuffer.io.flush := needFlush
  // ibuffer to backend
  io.backend.cfVec <> ibuffer.io.out

  io.error <> DontCare

  val frontendBubble = PopCount((0 until DecodeWidth).map(i => io.backend.cfVec(i).ready && !ibuffer.io.out(i).valid))
  XSPerfAccumulate("FrontendBubble", frontendBubble)

  io.frontendInfo.ibufFull := RegNext(ibuffer.io.full)
}
