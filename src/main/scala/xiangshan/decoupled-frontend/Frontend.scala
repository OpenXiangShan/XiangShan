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
import xiangshan.cache.mmu.{TlbRequestIO, TlbPtwIO,TLB}
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
    val ptw = new TlbPtwIO(2)
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

  //decouped-frontend modules
  val bpu     = Module(new Predictor)
  val ifu     = Module(new NewIFU)
  val ibuffer =  Module(new Ibuffer)
  val ftq = Module(new Ftq)
  //icache
  val icacheMeta      = Module(new ICacheMetaArray)
  val icacheData      = Module(new ICacheDataArray)
  val icacheMissQueue = Module(new ICacheMissQueue)
  io.ptw <> TLB(
    in = Seq(ifu.io.iTLBInter(0), ifu.io.iTLBInter(1)),
    sfence = io.sfence,
    csr = io.tlbCsr,
    width = 2,
    isDtlb = false,
    shouldBlock = true
  )  
  //TODO: modules need to be removed
  val instrUncache = outer.instrUncache.module

  val needFlush = io.backend.toFtq.stage3Redirect.valid

  //IFU-Ftq
  ifu.io.ftqInter.fromFtq <> ftq.io.toIfu
  ftq.io.fromIfu          <> ifu.io.ftqInter.toFtq
  bpu.io.ftq_to_bpu       <> ftq.io.toBpu
  ftq.io.fromBpu          <> bpu.io.bpu_to_ftq
  //IFU-ICache
  ifu.io.icacheInter.toIMeta    <>      icacheMeta.io.read
  ifu.io.icacheInter.fromIMeta  <>      icacheMeta.io.readResp
  ifu.io.icacheInter.toIData    <>      icacheData.io.read
  ifu.io.icacheInter.fromIData  <>      icacheData.io.readResp


  for(i <- 0 until 2){
    ifu.io.icacheInter.fromMissQueue(i) <> icacheMissQueue.io.resp(i) 
    icacheMissQueue.io.req(i)           <> ifu.io.icacheInter.toMissQueue(i)
  }

  icacheMissQueue.io.flush := ifu.io.ftqInter.fromFtq.redirect.valid

  //IFU-Ibuffer
  ifu.io.toIbuffer    <> ibuffer.io.in

  //ICache
  icacheMeta.io.write <> icacheMissQueue.io.meta_write
  icacheData.io.write <> icacheMissQueue.io.data_write

  io.icacheMemGrant.ready := icacheMissQueue.io.mem_grant.ready
  icacheMissQueue.io.mem_grant.valid  :=  io.icacheMemGrant.valid 
  icacheMissQueue.io.mem_grant.bits   :=  io.icacheMemGrant.bits

  ftq.io.fromBackend <> io.backend.toFtq
  io.backend.fromFtq <> ftq.io.toBackend
  io.frontendInfo.bpuInfo <> ftq.io.bpuInfo

  io.icacheMemAcq <> icacheMissQueue.io.mem_acquire
  ibuffer.io.flush := needFlush
  io.backend.cfVec <> ibuffer.io.out

  instrUncache.io.req   <> DontCare
  instrUncache.io.resp  <> DontCare
  instrUncache.io.flush <> DontCare
  io.error <> DontCare

  val frontendBubble = PopCount((0 until DecodeWidth).map(i => io.backend.cfVec(i).ready && !ibuffer.io.out(i).valid))
  if (!env.FPGAPlatform && env.EnablePerfDebug) {
    XSPerfAccumulate("FrontendBubble", frontendBubble)
  }

  io.frontendInfo.ibufFull := RegNext(ibuffer.io.full)
}
