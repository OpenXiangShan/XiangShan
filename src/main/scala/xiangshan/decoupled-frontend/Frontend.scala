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

  val instrUncache  = LazyModule(new InstrUncache())
  val icache        = LazyModule(new ICache())

  lazy val module = new FrontendImp(this)
}


class FrontendImp (outer: Frontend) extends LazyModuleImp(outer)
  with HasL1plusCacheParameters
  with HasXSParameter
  with HasExceptionNO
{
  val io = IO(new Bundle() {
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
  val icache       = outer.icache.module

  val needFlush = io.backend.toFtq.stage3Redirect.valid

  //IFU-Ftq
  ifu.io.ftqInter.fromFtq <> ftq.io.toIfu
  ftq.io.fromIfu          <> ifu.io.ftqInter.toFtq
  bpu.io.ftq_to_bpu       <> ftq.io.toBpu
  ftq.io.fromBpu          <> bpu.io.bpu_to_ftq
  //IFU-ICache
  ifu.io.icacheInter.toIMeta    <>      icache.io.metaRead.req
  ifu.io.icacheInter.fromIMeta  <>      icache.io.metaRead.resp
  ifu.io.icacheInter.toIData    <>      icache.io.dataRead.req
  ifu.io.icacheInter.fromIData  <>      icache.io.dataRead.resp

  for(i <- 0 until 2){
    ifu.io.icacheInter.toMissQueue(i)         <> icache.io.missQueue.req(i)
    ifu.io.icacheInter.fromMissQueue(i)       <> icache.io.missQueue.resp(i)
  }

  icache.io.missQueue.flush := ifu.io.ftqInter.fromFtq.redirect.valid || (ifu.io.ftqInter.toFtq.pdWb.valid && ifu.io.ftqInter.toFtq.pdWb.bits.misOffset.valid)

  //IFU-Ibuffer
  ifu.io.toIbuffer    <> ibuffer.io.in

  ftq.io.fromBackend <> io.backend.toFtq
  io.backend.fromFtq <> ftq.io.toBackend
  io.frontendInfo.bpuInfo <> ftq.io.bpuInfo

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
