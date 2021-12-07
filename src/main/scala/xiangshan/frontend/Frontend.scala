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
import xiangshan.frontend.icache._
import xiangshan.cache.mmu.{TlbRequestIO, TlbPtwIO,TLB}
import xiangshan.backend.fu.{HasExceptionNO, PMP, PMPChecker, PFEvent}


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
    val csrUpdate = new DistributedCSRUpdateReq
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
  val instrUncache = outer.instrUncache.module
  val icache       = outer.icache.module
  val bpu     = Module(new Predictor)
  val ifu     = Module(new NewIFU)
  val ibuffer =  Module(new Ibuffer)
  val ftq = Module(new Ftq)

  //PFEvent
  val pfevent = Module(new PFEvent)
  val tlbCsr = RegNext(io.tlbCsr)
  pfevent.io.distribute_csr := io.csrCtrl.distribute_csr

  // trigger
  ifu.io.frontendTrigger := io.csrCtrl.frontend_trigger
  val triggerEn = io.csrCtrl.trigger_enable
  ifu.io.csrTriggerEnable := VecInit(triggerEn(0), triggerEn(1), triggerEn(6), triggerEn(8))

  // pmp
  val pmp = Module(new PMP())
  val pmp_check = VecInit(Seq.fill(2)(Module(new PMPChecker(3, sameCycle = true)).io))
  pmp.io.distribute_csr := io.csrCtrl.distribute_csr
  for (i <- pmp_check.indices) {
    pmp_check(i).apply(tlbCsr.priv.imode, pmp.io.pmp, pmp.io.pma, icache.io.pmp(i).req)
    icache.io.pmp(i).resp <> pmp_check(i).resp
  }

  io.ptw <> TLB(
    in = Seq(icache.io.itlb(0), icache.io.itlb(1)),
    sfence = io.sfence,
    csr = tlbCsr,
    width = 2,
    shouldBlock = true,
    itlbParams
  )

  icache.io.fencei := RegNext(io.fencei)

  val needFlush = io.backend.toFtq.stage3Redirect.valid

  //IFU-Ftq
  ifu.io.ftqInter.fromFtq <> ftq.io.toIfu
  ftq.io.fromIfu          <> ifu.io.ftqInter.toFtq
  bpu.io.ftq_to_bpu       <> ftq.io.toBpu
  ftq.io.fromBpu          <> bpu.io.bpu_to_ftq
  //IFU-ICache
  for(i <- 0 until 2){
    ifu.io.icacheInter(i).req       <>      icache.io.fetch(i).req
    icache.io.fetch(i).req <> ifu.io.icacheInter(i).req
    ifu.io.icacheInter(i).resp <> icache.io.fetch(i).resp
  }
  icache.io.stop := ifu.io.icacheStop

  ifu.io.icachePerfInfo := icache.io.perfInfo

  //icache.io.missQueue.flush := ifu.io.ftqInter.fromFtq.redirect.valid || (ifu.io.ftqInter.toFtq.pdWb.valid && ifu.io.ftqInter.toFtq.pdWb.bits.misOffset.valid)

  icache.io.csr.distribute_csr <> io.csrCtrl.distribute_csr
  icache.io.csr.update <> io.csrUpdate

  //IFU-Ibuffer
  ifu.io.toIbuffer    <> ibuffer.io.in

  ftq.io.fromBackend <> io.backend.toFtq
  io.backend.fromFtq <> ftq.io.toBackend
  io.frontendInfo.bpuInfo <> ftq.io.bpuInfo

  ifu.io.rob_commits <> io.backend.toFtq.rob_commits

  ibuffer.io.flush := needFlush
  io.backend.cfVec <> ibuffer.io.out

  instrUncache.io.req   <> ifu.io.uncacheInter.toUncache
  ifu.io.uncacheInter.fromUncache <> instrUncache.io.resp
  instrUncache.io.flush := false.B//icache.io.missQueue.flush
  io.error <> DontCare

  val frontendBubble = PopCount((0 until DecodeWidth).map(i => io.backend.cfVec(i).ready && !ibuffer.io.out(i).valid))
  XSPerfAccumulate("FrontendBubble", frontendBubble)
  io.frontendInfo.ibufFull := RegNext(ibuffer.io.full)

  if(print_perfcounter){
    val ifu_perf     = ifu.perfEvents.map(_._1).zip(ifu.perfinfo.perfEvents.perf_events)
    val ibuffer_perf = ibuffer.perfEvents.map(_._1).zip(ibuffer.perfinfo.perfEvents.perf_events)
    val icache_perf  = icache.perfEvents.map(_._1).zip(icache.perfinfo.perfEvents.perf_events)
    val ftq_perf     = ftq.perfEvents.map(_._1).zip(ftq.perfinfo.perfEvents.perf_events)
    val bpu_perf     = bpu.perfEvents.map(_._1).zip(bpu.perfinfo.perfEvents.perf_events)
    val perfEvents = ifu_perf ++ ibuffer_perf ++ icache_perf ++ ftq_perf ++ bpu_perf

    for (((perf_name,perf),i) <- perfEvents.zipWithIndex) {
      println(s"frontend perf $i: $perf_name")
    }
  }

  val hpmEvents = ifu.perfinfo.perfEvents.perf_events ++ ibuffer.perfinfo.perfEvents.perf_events ++
                  icache.perfinfo.perfEvents.perf_events ++ ftq.perfinfo.perfEvents.perf_events ++
                  bpu.perfinfo.perfEvents.perf_events
  val perf_length = hpmEvents.length
  val csrevents = pfevent.io.hpmevent.slice(0,8)
  val perfinfo = IO(new Bundle(){
    val perfEvents        = Output(new PerfEventsBundle(csrevents.length))
  })
  val hpm_frontend = Module(new HPerfmonitor(perf_length,csrevents.length))
  hpm_frontend.io.hpm_event := csrevents
  hpm_frontend.io.events_sets.perf_events := hpmEvents
  perfinfo.perfEvents := RegNext(hpm_frontend.io.events_selected)
}
