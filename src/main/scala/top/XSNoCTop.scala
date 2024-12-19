/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package top

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import system._
import device._
import org.chipsalliance.cde.config._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import coupledL2.tl2chi.{PortIO, CHIAsyncBridgeSink}
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.util.{AsyncQueueSource, AsyncQueueParams}
import chisel3.experimental.{annotate, ChiselAnnotation}
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation

class XSNoCTop()(implicit p: Parameters) extends BaseXSSoc with HasSoCParameter
{
  override lazy val desiredName: String = "XSTop"

  ResourceBinding {
    val width = ResourceInt(2)
    val model = "freechips,rocketchip-unknown"
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "compat").bind(ResourceString(model + "-dev"))
    Resource(ResourceAnchors.soc, "compat").bind(ResourceString(model + "-soc"))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc, "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))
    def bindManagers(xbar: TLNexusNode) = {
      ManagerUnification(xbar.edges.in.head.manager.managers).foreach{ manager =>
        manager.resources.foreach(r => r.bind(manager.toResource))
      }
    }
  }

  require(enableCHI)

  // xstile
  val core_with_l2 = LazyModule(new XSTileWrap()(p.alter((site, here, up) => {
    case XSCoreParamsKey => tiles.head
    case PerfCounterOptionsKey => up(PerfCounterOptionsKey).copy(perfDBHartID = tiles.head.HartId)
  })))

  // imsic bus top
  val u_imsic_bus_top = LazyModule(new imsic_bus_top(
    useTL = soc.IMSICUseTL,
    baseAddress = (0x3A800000, 0x3B000000)
  ))

  // interrupts
  val clintIntNode = IntSourceNode(IntSourcePortSimple(1, 1, 2))
  val debugIntNode = IntSourceNode(IntSourcePortSimple(1, 1, 1))
  val plicIntNode = IntSourceNode(IntSourcePortSimple(1, 2, 1))
  val nmiIntNode = IntSourceNode(IntSourcePortSimple(1, 1, (new NonmaskableInterruptIO).elements.size))
  val beuIntNode = IntSinkNode(IntSinkPortSimple(1, 1))
  core_with_l2.clintIntNode := clintIntNode
  core_with_l2.debugIntNode := debugIntNode
  core_with_l2.plicIntNode :*= plicIntNode
  core_with_l2.nmiIntNode := nmiIntNode
  beuIntNode := core_with_l2.beuIntNode
  val clint = InModuleBody(clintIntNode.makeIOs())
  val debug = InModuleBody(debugIntNode.makeIOs())
  val plic = InModuleBody(plicIntNode.makeIOs())
  val nmi = InModuleBody(nmiIntNode.makeIOs())
  val beu = InModuleBody(beuIntNode.makeIOs())

  // reset nodes
  val core_rst_node = BundleBridgeSource(() => Reset())
  core_with_l2.tile.core_reset_sink := core_rst_node

  class XSNoCTopImp(wrapper: XSNoCTop) extends LazyRawModuleImp(wrapper) {
    soc.XSTopPrefix.foreach { prefix =>
      val mod = this.toNamed
      annotate(new ChiselAnnotation {
        def toFirrtl = NestedPrefixModulesAnnotation(mod, prefix, true)
      })
    }
    FileRegisters.add("dts", dts)
    FileRegisters.add("graphml", graphML)
    FileRegisters.add("json", json)
    FileRegisters.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())

    val clock = IO(Input(Clock()))
    val reset = IO(Input(AsyncReset()))
    val noc_clock = EnableCHIAsyncBridge.map(_ => IO(Input(Clock())))
    val noc_reset = EnableCHIAsyncBridge.map(_ => IO(Input(AsyncReset())))
    val soc_clock = IO(Input(Clock()))
    val soc_reset = IO(Input(AsyncReset()))
    val io = IO(new Bundle {
      val hartId = Input(UInt(p(MaxHartIdBits).W))
      val riscv_halt = Output(Bool())
      val riscv_critical_error = Output(Bool())
      val hartIsInReset = Output(Bool())
      val riscv_rst_vec = Input(UInt(soc.PAddrBits.W))
      val chi = new PortIO
      val nodeID = Input(UInt(soc.NodeIDWidthList(issue).W))
      val clintTime = Input(ValidIO(UInt(64.W)))
    })
    println(" [XSNocTop] --------> WFIClockGate: " + WFIClockGate)

    // imsic axi4lite io
    val imsic_axi4lite = wrapper.u_imsic_bus_top.module.axi4lite.map(x => IO(chiselTypeOf(x)))
    // imsic tl io
    val imsic_m_tl = wrapper.u_imsic_bus_top.tl_m.map(x => IO(chiselTypeOf(x.getWrappedValue)))
    val imsic_s_tl = wrapper.u_imsic_bus_top.tl_s.map(x => IO(chiselTypeOf(x.getWrappedValue)))

    val noc_reset_sync = EnableCHIAsyncBridge.map(_ => withClockAndReset(noc_clock, noc_reset) { ResetGen() })
    val soc_reset_sync = withClockAndReset(soc_clock, soc_reset) { ResetGen() }

    // device clock and reset
    wrapper.u_imsic_bus_top.module.clock := soc_clock
    wrapper.u_imsic_bus_top.module.reset := soc_reset_sync

    // imsic axi4lite io connection
    wrapper.u_imsic_bus_top.module.axi4lite.foreach(_ <> imsic_axi4lite.get)

    // imsic tl io connection
    wrapper.u_imsic_bus_top.tl_m.foreach(_ <> imsic_m_tl.get)
    wrapper.u_imsic_bus_top.tl_s.foreach(_ <> imsic_s_tl.get)

    // input
    dontTouch(io)
    val pmuReset = Wire(Bool())
//    val wfiClockEn = WireDefault(true.B)
    val wfiClockEn = withClockAndReset(soc_clock, soc_reset) {RegInit(true.B)}
    val pmuClockEn = WireDefault(true.B)
    val resetTile = reset.asBool || pmuReset
    dontTouch(pmuClockEn)
    dontTouch(pmuReset)
    dontTouch(wfiClockEn)

    core_with_l2.module.clock := ClockGate(false.B, (wfiClockEn & pmuClockEn), clock)
//    core_with_l2.module.clock := ClockGate(false.B, wfiClockEn, clock)
    core_with_l2.module.reset := resetTile.asAsyncReset
    core_with_l2.module.noc_reset.foreach(_ := noc_reset.get)
    core_with_l2.module.soc_reset := soc_reset
    core_with_l2.module.io.hartId := io.hartId
    core_with_l2.module.io.nodeID.get := io.nodeID
    io.riscv_halt := core_with_l2.module.io.cpu_halt
    io.riscv_critical_error := core_with_l2.module.io.cpu_crtical_error
    io.hartIsInReset := core_with_l2.module.io.hartIsInReset
    core_with_l2.module.io.reset_vector := io.riscv_rst_vec

    /* Low power logic include:
     1. Interrupt source parse
     2. PPU-Core low power state transfer FSM
     3. WFI clock gating and wakeup FSM
     */

    val sIDLE :: sL2FLUSH :: sWAITWFI :: sPOFFREQ :: Nil = Enum(4)
    val state = withClockAndReset(soc_clock, resetTile.asAsyncReset) {RegInit(sIDLE)}
//    val powerOff = WireInit(false.B)


    //Interrupt sources
    val msip  = clint.head(0)
    val mtip  = clint.head(1)
    val meip  = plic.head(0)
    val seip  = plic.last(0)
    val nmi_31 = nmi.head(0)
    val nmi_43 = nmi.head(1)
    val msi_info_vld = wrapper.u_imsic_bus_top.module.o_msi_info_vld
    val intSrc = Cat(msip, mtip, meip, seip, nmi_31, nmi_43, msi_info_vld)
    val intAll = intSrc.orR
    val preIntSrc = withClockAndReset(soc_clock, soc_reset) {RegInit(0.U(7.W))}

    val counter = withClockAndReset(soc_clock, soc_reset) {RegInit(0.U(32.W))}
    counter := counter+1.U
//    val corePD = (counter > 702000.U) 
    val corePD = ((counter > 702000.U) && (counter < 702100.U)) |
                 ((counter > 703000.U) && (counter < 703100.U)) 

    dontTouch(state)
    dontTouch(intAll)
    dontTouch(counter)
    dontTouch(corePD)
   // FSM stransfer
    switch(state) {
      is(sIDLE) {
//        when(core_with_l2.module.io.corePWRDNEn) {
        when(corePD) {
        state := sL2FLUSH  
        }
      }
      is(sL2FLUSH) {
//        when(io.core_zzzwith_l2.module.io.l2FlushDone) {
//          when(core_with_l2.module.io.corePWRDNEn) {
          when(corePD) {
            state := sWAITWFI
          }.otherwise {
            state := sIDLE
          }
  //      }
      }
      is(sWAITWFI) {
//        when(core_with_l2.module.io.corePWRDNEn === false.B) {
        when (!corePD){
        state := sIDLE
        }
//        when(core_with_l2.module.io.corePWRDNEn && core_with_l2.module.io.isWFI) {
        when(corePD && core_with_l2.module.io.cpu_halt) {
          state := sPOFFREQ
        }
      }
      is(sPOFFREQ) {
//        powerOff := true.B
      }
    }
     //PMU-module
    if (EnablePMU) {
      val pmu = withClockAndReset(soc_clock, soc_reset_sync)(Module(new PMU))
      pmu.io.coreActive := ~(state === sPOFFREQ)
      pmu.io.coreWakeReq := intAll //TODO constraint to external interrupt only
      pmuClockEn := pmu.io.coreClken
      pmuReset := pmu.io.coreReset
    } else {
      pmuClockEn := true.B
      pmuReset := false.B
    }

    //Core in WFI -> gate clock -> interrupt/snoop -> recover clock
    val sNORMAL :: sGCLOCK :: sAWAKE :: Nil = Enum(3)
    val wfistate = withClockAndReset(soc_clock, soc_reset) {RegInit(sIDLE)}
   // FSM stransfer
    switch(state) {
      is(sNORMAL) {
        when(core_with_l2.module.io.cpu_halt) {
          wfistate := sGCLOCK
        }.elsewhen(intAll){
          wfistate := sAWAKE
        }}
      is(sGCLOCK){
        when(intAll){
          wfistate := sAWAKE
        }.elsewhen (io.chi.rx.snp.flitpend){
          wfistate := sNORMAL
        }
      }
      is(sAWAKE){
        when(!intAll || (preIntSrc =/= intSrc)) {
          wfistate := sNORMAL
        }
      }
    }
    // Save INT that wakeup core from WFI
    when(intAll) {
      preIntSrc := intSrc
    }
    //WFI gating 
    if (WFIClockGate) {
      when( (wfistate === sGCLOCK)){
        wfiClockEn := false.B
      }.otherwise {
        wfiClockEn := true.B
      }
    }
    else {
      wfiClockEn := true.B
    }

    

    EnableClintAsyncBridge match {
      case Some(param) =>
        withClockAndReset(soc_clock, soc_reset_sync) {
          val source = Module(new AsyncQueueSource(UInt(64.W), param))
          source.io.enq.valid := io.clintTime.valid
          source.io.enq.bits := io.clintTime.bits
          core_with_l2.module.io.clintTime <> source.io.async
        }
      case None =>
        core_with_l2.module.io.clintTime <> io.clintTime
    }

    EnableCHIAsyncBridge match {
      case Some(param) =>
        withClockAndReset(noc_clock.get, noc_reset_sync.get) {
          val sink = Module(new CHIAsyncBridgeSink(param))
          sink.io.async <> core_with_l2.module.io.chi
          io.chi <> sink.io.deq
        }
      case None =>
        io.chi <> core_with_l2.module.io.chi
    }

    core_with_l2.module.io.msiInfo.valid := wrapper.u_imsic_bus_top.module.o_msi_info_vld
    core_with_l2.module.io.msiInfo.bits.info := wrapper.u_imsic_bus_top.module.o_msi_info
    // tie off core soft reset
    core_rst_node.out.head._1 := false.B.asAsyncReset

    core_with_l2.module.io.debugTopDown.l3MissMatch := false.B
  }

  lazy val module = new XSNoCTopImp(this)
}


