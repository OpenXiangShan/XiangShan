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
import xiangshan.cache.mmu.{TLB, TlbPtwIO, TlbRequestIO}
import xiangshan.backend.fu.{HasExceptionNO, PMP, PMPChecker,PFEvent}
import system.L1CacheErrorInfo


class Frontend()(implicit p: Parameters) extends LazyModule with HasXSParameter{

  val instrUncache  = LazyModule(new InstrUncache())
  val icache        = LazyModule(new ICache())

  lazy val module = new FrontendImp(this)
}


class FrontendImp (outer: Frontend) extends LazyModuleImp(outer)
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
    val perfEvents      = Output(new PerfEventsBundle(numCSRPCntFrontend))
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

  //PFEvent
  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := io.csrCtrl.distribute_csr
  // pmp
  val pmp = Module(new PMP())
  val pmp_check = VecInit(Seq.fill(2)(Module(new PMPChecker(3, sameCycle = true)).io))
  pmp.io.distribute_csr := io.csrCtrl.distribute_csr
  for (i <- pmp_check.indices) {
    pmp_check(i).env.pmp  := pmp.io.pmp
    pmp_check(i).env.mode := io.tlbCsr.priv.imode
    pmp_check(i).req <> ifu.io.pmp(i).req
    ifu.io.pmp(i).resp <> pmp_check(i).resp
  }

  io.ptw <> TLB(
    in = Seq(ifu.io.iTLBInter(0), ifu.io.iTLBInter(1)),
    sfence = io.sfence,
    csr = io.tlbCsr,
    width = 2,
    shouldBlock = true,
    itlbParams
  )
  //TODO: modules need to be removed
  val instrUncache = outer.instrUncache.module
  val icache       = outer.icache.module

  icache.io.fencei := RegNext(io.fencei)

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
//  val numPCntFrontend = 64

  val frontendBubble = PopCount((0 until DecodeWidth).map(i => io.backend.cfVec(i).ready && !ibuffer.io.out(i).valid))
  XSPerfAccumulate("FrontendBubble", frontendBubble)
  io.frontendInfo.ibufFull := RegNext(ibuffer.io.full)


  val hpmEvents = Wire(new PerfEventsBundle(numPCntFrontend))
  for(i <- 0 until numPCntFrontend ) {
    hpmEvents.PerfEvents(i).incr_step := DontCare
  }
  //hpmEvents.PerfEvents.map(_.incr_step) := frontendBubble +: ifu.io.perfEvents.PerfEvents.map(_.incr_step) + ibuffer.io.perfEvents.PerfEvents.map(_.incr_step) + 

  //hpmEvents.PerfEvents := ifu.io.perfEvents.PerfEvents + ibuffer.io.perfEvents.PerfEvents + ftq.io.perfEvents.PerfEvents + bpu.io.perfEvents.PerfEvents

  hpmEvents.PerfEvents(0 ).incr_step   := frontendBubble
  hpmEvents.PerfEvents(1 ).incr_step   := ifu.io.perfEvents.PerfEvents(1).incr_step   
  hpmEvents.PerfEvents(2 ).incr_step   := ifu.io.perfEvents.PerfEvents(2).incr_step   
  hpmEvents.PerfEvents(3 ).incr_step   := ifu.io.perfEvents.PerfEvents(3).incr_step   
  hpmEvents.PerfEvents(4 ).incr_step   := ifu.io.perfEvents.PerfEvents(4).incr_step   
  hpmEvents.PerfEvents(5 ).incr_step   := ifu.io.perfEvents.PerfEvents(5).incr_step   
  hpmEvents.PerfEvents(6 ).incr_step   := ifu.io.perfEvents.PerfEvents(6).incr_step   
  hpmEvents.PerfEvents(7 ).incr_step   := ifu.io.perfEvents.PerfEvents(7).incr_step   
  hpmEvents.PerfEvents(8 ).incr_step   := ifu.io.perfEvents.PerfEvents(8).incr_step   
  hpmEvents.PerfEvents(9 ).incr_step   := ifu.io.perfEvents.PerfEvents(9).incr_step   
  hpmEvents.PerfEvents(10).incr_step  := ifu.io.perfEvents.PerfEvents(10).incr_step  
  hpmEvents.PerfEvents(11).incr_step  := ifu.io.perfEvents.PerfEvents(11).incr_step  
  hpmEvents.PerfEvents(12).incr_step  := ifu.io.perfEvents.PerfEvents(12).incr_step  
  hpmEvents.PerfEvents(13).incr_step  := ifu.io.perfEvents.PerfEvents(13).incr_step  
  hpmEvents.PerfEvents(14).incr_step  := ifu.io.perfEvents.PerfEvents(14).incr_step  
  hpmEvents.PerfEvents(15).incr_step  := ifu.io.perfEvents.PerfEvents(15).incr_step  
  hpmEvents.PerfEvents(16).incr_step  := ibuffer.io.perfEvents.PerfEvents(16).incr_step  
  hpmEvents.PerfEvents(17).incr_step  := ibuffer.io.perfEvents.PerfEvents(17).incr_step  
  hpmEvents.PerfEvents(18).incr_step  := ibuffer.io.perfEvents.PerfEvents(18).incr_step  
  hpmEvents.PerfEvents(19).incr_step  := ibuffer.io.perfEvents.PerfEvents(19).incr_step  
  hpmEvents.PerfEvents(20).incr_step  := ibuffer.io.perfEvents.PerfEvents(20).incr_step  
  hpmEvents.PerfEvents(21).incr_step  := ibuffer.io.perfEvents.PerfEvents(21).incr_step  
  hpmEvents.PerfEvents(22).incr_step  := ibuffer.io.perfEvents.PerfEvents(22).incr_step  
  hpmEvents.PerfEvents(23).incr_step  := ibuffer.io.perfEvents.PerfEvents(23).incr_step  
  hpmEvents.PerfEvents(24).incr_step  := icache.io.perfEvents.PerfEvents(24).incr_step   
  hpmEvents.PerfEvents(25).incr_step  := icache.io.perfEvents.PerfEvents(25).incr_step   
  hpmEvents.PerfEvents(26).incr_step  := ftq.io.perfEvents.PerfEvents(26).incr_step   
  hpmEvents.PerfEvents(27).incr_step  := ftq.io.perfEvents.PerfEvents(27).incr_step   
  hpmEvents.PerfEvents(28).incr_step  := ftq.io.perfEvents.PerfEvents(28).incr_step   
  hpmEvents.PerfEvents(29).incr_step  := ftq.io.perfEvents.PerfEvents(29).incr_step   
  hpmEvents.PerfEvents(30).incr_step  := ftq.io.perfEvents.PerfEvents(30).incr_step   
  hpmEvents.PerfEvents(31).incr_step  := ftq.io.perfEvents.PerfEvents(31).incr_step   
  hpmEvents.PerfEvents(32).incr_step  := ftq.io.perfEvents.PerfEvents(32).incr_step   
  hpmEvents.PerfEvents(33).incr_step  := ftq.io.perfEvents.PerfEvents(33).incr_step   
  hpmEvents.PerfEvents(34).incr_step  := ftq.io.perfEvents.PerfEvents(34).incr_step   
  hpmEvents.PerfEvents(35).incr_step  := ftq.io.perfEvents.PerfEvents(35).incr_step   
  hpmEvents.PerfEvents(36).incr_step  := ftq.io.perfEvents.PerfEvents(36).incr_step   
  hpmEvents.PerfEvents(37).incr_step  := ftq.io.perfEvents.PerfEvents(37).incr_step   
  hpmEvents.PerfEvents(38).incr_step  := ftq.io.perfEvents.PerfEvents(38).incr_step   
  hpmEvents.PerfEvents(39).incr_step  := ftq.io.perfEvents.PerfEvents(39).incr_step   
  hpmEvents.PerfEvents(40).incr_step  := ftq.io.perfEvents.PerfEvents(40).incr_step   
  hpmEvents.PerfEvents(41).incr_step  := ftq.io.perfEvents.PerfEvents(41).incr_step   
  hpmEvents.PerfEvents(42).incr_step  := ftq.io.perfEvents.PerfEvents(42).incr_step   
  hpmEvents.PerfEvents(43).incr_step  := ftq.io.perfEvents.PerfEvents(43).incr_step   
  hpmEvents.PerfEvents(44).incr_step  := ftq.io.perfEvents.PerfEvents(44).incr_step   
  hpmEvents.PerfEvents(45).incr_step  := ftq.io.perfEvents.PerfEvents(45).incr_step   
  hpmEvents.PerfEvents(46).incr_step  := ftq.io.perfEvents.PerfEvents(46).incr_step   
  hpmEvents.PerfEvents(47).incr_step  := ftq.io.perfEvents.PerfEvents(47).incr_step   
  hpmEvents.PerfEvents(48).incr_step  := bpu.io.perfEvents.PerfEvents(48).incr_step
  hpmEvents.PerfEvents(49).incr_step  := bpu.io.perfEvents.PerfEvents(49).incr_step
  hpmEvents.PerfEvents(50).incr_step  := bpu.io.perfEvents.PerfEvents(50).incr_step
  hpmEvents.PerfEvents(51).incr_step  := bpu.io.perfEvents.PerfEvents(51).incr_step
  hpmEvents.PerfEvents(52).incr_step  := bpu.io.perfEvents.PerfEvents(52).incr_step
  hpmEvents.PerfEvents(53).incr_step  := bpu.io.perfEvents.PerfEvents(53).incr_step
  hpmEvents.PerfEvents(54).incr_step  := bpu.io.perfEvents.PerfEvents(54).incr_step
  hpmEvents.PerfEvents(55).incr_step  := bpu.io.perfEvents.PerfEvents(55).incr_step
//
//  hpmEvents.PerfEvents := (ifu.io.perfEvents.PerfEvents) ++ ibuffer.io.perfEvents.PerfEvents ++ icache.io.perfEvents.PerfEvents ++ ftq.io.perfEvents.PerfEvents ++ bpu.io.perfEvents.PerfEvents
             
  //for(i <- 0 until numCSRPCntFrontend ) {
  //  io.perfEvents.PerfEvents(i).incr_step := DontCare
  //}
  val hpm_frontend = Module(new HPerfmonitor(numPCntFrontend,numCSRPCntFrontend))
  hpm_frontend.io.HPMEvent(0) := pfevent.io.HPMEvent(0)
  hpm_frontend.io.HPMEvent(1) := pfevent.io.HPMEvent(1)
  hpm_frontend.io.HPMEvent(2) := pfevent.io.HPMEvent(2)
  hpm_frontend.io.HPMEvent(3) := pfevent.io.HPMEvent(3)
  hpm_frontend.io.HPMEvent(4) := pfevent.io.HPMEvent(4)
  hpm_frontend.io.HPMEvent(5) := pfevent.io.HPMEvent(5)
  hpm_frontend.io.HPMEvent(6) := pfevent.io.HPMEvent(6)
  hpm_frontend.io.HPMEvent(7) := pfevent.io.HPMEvent(7)
  hpm_frontend.io.Events_sets := hpmEvents
  io.perfEvents := hpm_frontend.io.Events_selected
}
