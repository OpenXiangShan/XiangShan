// SPDX-License-Identifier: Apache-2.0
// See LICENSE.txt for license details.

package pbus

import _root_.circt.stage._
import aia.AXI4IMSIC
import aia.IMSICParameKey
import aia.IMSICParameters
import chisel3._
import chisel3.IO
import chisel3.experimental.ChiselAnnotation
import chisel3.experimental.annotate
import chisel3.experimental.dataview._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import device.EnableJtag
import device.SYSCNT
import device.SYSCNTConsts
import device.SYSCNTParams
import device.standalone.StandAloneDebugModule
import device.standalone.StandAloneSYSCNT
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.debug.APB
import freechips.rocketchip.devices.debug.CJTAG
import freechips.rocketchip.devices.debug.DebugAttachParams
import freechips.rocketchip.devices.debug.DebugExportProtocol
import freechips.rocketchip.devices.debug.DebugIO
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.devices.debug.DebugModuleParams
import freechips.rocketchip.devices.debug.DMI
import freechips.rocketchip.devices.debug.ExportDebug
import freechips.rocketchip.devices.debug.JTAG
import freechips.rocketchip.devices.debug.JtagDTMKey
import freechips.rocketchip.devices.debug.ResetCtrlIO
import freechips.rocketchip.diplomacy
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.ValName
import freechips.rocketchip.interrupts.IntSinkParameters
import freechips.rocketchip.subsystem.CBUS
import freechips.rocketchip.subsystem.FBUS
import freechips.rocketchip.subsystem.TLBusWrapperLocation
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.HeterogeneousBag
//import org.chipsalliance.cde.config.{Config, Parameters}
import org.chipsalliance.cde.config._
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import system.CVMParameters
import system.CVMParamsKey
import system.HasSoCParameter
import system.SoCParameters
import system.SoCParamsKey
import top.ArgParser
import top.TopMain.args
import utils.VerilogAXI4Record
import xiangshan.DebugOptions
import xiangshan.DebugOptionsKey
import xiangshan.DFTOptions
import xiangshan.DFTOptionsKey
import xiangshan.PMParameKey
import xiangshan.PMParameters
import xiangshan.XSCoreParameters
import xiangshan.XSTileKey


/**
 * Configurable parameters for the Pbus2 interconnect.
 *
 * @param numInputs       Number of input AXI4 ports. Default is 10.
 * @param idBits     ID width for each input port. Default is 4.
 * @param inputDataWidths A sequence of data widths for each input port.
 * @param numOutputs      Number of output AXI4 ports. The topology is fixed to 3.
 * @param outputAddrMap   A sequence of AddressSet for each output port.
 * @param MSIOutDataWidth Data width for each output port. Default is 64.
 */
case class AplicParams(
    CFG_ADDR_WIDTH: Int = 40,
    CFG_DATA_WIDTH: Int = 64,
    CFG_ID_WIDTH:   Int = 16,
    APLICAddrMap:   AddressSet = AddressSet(0x31100000L, 0x7fff),
    MSI_DATA_WIDTH: Int = 32,
    NumIntSrcs:     Int = 512
)
case class PeriParams(
    slaveDataBytes: Int = 8,
    timedataBytes:  Int = 8,
    addrWidth:      Int = 32
)

case class Pbus2Params(
    NumHarts:       Int = 2, // number of cpus +1(aplic)+1(pcie msi)
    idBits:         Int = 8,
    cpuAddrWidth:   Int = 32,
    cpuDataWidth:   Int = 64,
    dmHasBusMaster: Boolean = true,
    SYSCNTAddrMap: AddressSet = AddressSet(0x38040000L, 0x10000 - 1), // SYSCNTConsts.size - 1), 0x10000
    DebugAddrMap:  AddressSet = AddressSet(0x00010000L, 0x1000 - 1),  // 4KB
    dmsize:        Int = 0x1000,
    DieIDWidth:    Int = 3,
    CrsDataWidth:  Int = 128,
    CrsAddrWidth:  Int = 48,
    IMSICParams: aia.IMSICParams = aia.IMSICParams(
      imsicIntSrcWidth = 9,
      mAddr = 0x3a000000,
      sgAddr = 0x3b000000,
      geilen = 7,
      vgeinWidth = 6,
      iselectWidth = 12,
      EnableImsicAsyncBridge = true,
      HasTEEIMSIC = false
    ),
    aplicParams: AplicParams,
    MSIOutDataWidth: Int = 32,
    periParams:      PeriParams
) {
  lazy val NumInputs  = NumHarts + 0
  lazy val NumIntSrcs = 1 << IMSICParams.imsicIntSrcWidth
}

/**
 * A configurable hierarchical AXI4 bus interconnect with heterogeneous input widths.
 */
class Cbus(params: Pbus2Params)(implicit p: Parameters) extends LazyModule {
  // cpu master: cpus--> cpu_xbarNto1-->cpu2imsic_s/cpu2dm_s
//  val cpus_l0 = Seq.fill(params.NumHarts / 1)(AXI4Xbar())
  val cpus_l0 = (0 until params.NumHarts / 1).map {i =>
    val xbarNode = LazyModule(new AXI4Xbar())
    xbarNode.suggestName(s"cpus_l0_$i")
    xbarNode.node
  }
  val cpus_l1 = (0 until params.NumHarts / 2).map {i =>
    val xbarNode = LazyModule(new AXI4Xbar())
    xbarNode.suggestName(s"cpus_l1_$i")
    xbarNode.node
  }
  val cpus_l2 = (0 until params.NumHarts / 4).map {i =>
    val xbarNode = LazyModule(new AXI4Xbar())
    xbarNode.suggestName(s"cpus_l2_$i")
    xbarNode.node
  }
  val cpus_l3 = (0 until params.NumHarts / 8).map {i =>
    val xbarNode = LazyModule(new AXI4Xbar())
    xbarNode.suggestName(s"cpus_l3_$i")
    xbarNode.node
  }
  val cpus_l4LM = LazyModule(new AXI4Xbar())
  val cpus_l4 = cpus_l4LM.suggestName("cpus_l4")

//  val cpus_l1 = Seq.fill(params.NumHarts / 2)(AXI4Xbar())
//  val cpus_l2 = Seq.fill(params.NumHarts / 4)(AXI4Xbar())
//  val cpus_l3 = Seq.fill(params.NumHarts / 8)(AXI4Xbar())
//  val cpus_l4 = AXI4Xbar()
  // one xbar2to1 every two cpu modes
  for (i <- 0 until params.NumHarts) {
    cpus_l1(i / 2) :=* cpus_l0(i)
  }
  for (i <- 0 until params.NumHarts / 2) {
    cpus_l2(i / 2) := AXI4Buffer() :=* cpus_l1(i)
  }
  for (i <- 0 until params.NumHarts / 4) {
    cpus_l3(i / 2) :=* cpus_l2(i)
  }
  for (i <- 0 until params.NumHarts / 8) {
    cpus_l4 := AXI4Buffer() :=* cpus_l3(i)
  }
  lazy val module = new Imp
  class Imp extends LazyModuleImp(this)
}
class imsicPbusTop(params: Pbus2Params)(implicit p: Parameters) extends LazyModule {
  val Cbus = LazyModule(new Cbus(params))
  val hni_s_xbar = AXI4Xbar()
  val pcie_xbar1to2 = AXI4Xbar()
  val pbus_xbar = AXI4Xbar()
  val aplic_mNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "master-node",
      maxFlight = Some(0),
      id = IdRange(0, 1 << params.idBits)
    ))
  )))

  val regSize = (0x1000, 0x8000)
  val sNodes = Seq.tabulate(params.NumHarts)(n => {
    val m_mode = (params.IMSICParams.mAddr + n * regSize._1, regSize._1 - 1)
    val s_mode = (params.IMSICParams.sgAddr + n * 0x10000, regSize._2 - 1)
//    val m_mode = (params.IMSICParams.mAddr + n * 0x1000, 0x1000 - 1)
//    val s_mode = (params.IMSICParams.sgAddr + n * 0x1000 * (params.IMSICParams.geilen +1), 0x1000 * (params.IMSICParams.geilen +1) - 1)
    println(f"IMSICXbar: #${n}%-2d    M-mode [0x${m_mode._1}%x, 0x${m_mode._2}%x]")
    println(f"IMSICXbar:    S/VS-mode [0x${s_mode._1}%x, 0x${ s_mode._2}%x]")
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      Seq(AXI4SlaveParameters(
        address = Seq(AddressSet(m_mode._1, m_mode._2),
        AddressSet(s_mode._1, s_mode._2)),
        supportsWrite = TransferSizes(1, params.MSIOutDataWidth/8),
        supportsRead = TransferSizes(1, params.MSIOutDataWidth/8)
      )),
      beatBytes = 4)))
  })
  println("IMSICXbar: end sNodes define")



  pcie_xbar1to2 := aplic_mNode
  pcie_xbar1to2 := AXI4Buffer() := hni_s_xbar
  pbus_xbar := Cbus.cpus_l4
  pbus_xbar := pcie_xbar1to2
  // start to decoder for imsic below
  val imsic_l1 = Seq.fill(params.NumHarts / 8 ) (AXI4Xbar())
  val imsic_l2 = Seq.fill(params.NumHarts / 4 ) (AXI4Xbar())
  val imsic_l3 = Seq.fill(params.NumHarts / 2 ) (AXI4Xbar())
  val imsic_l4 = Seq.fill(params.NumHarts / 1 ) (AXI4Xbar())
  // one xbar1to2 every two cpu xbar
  for (i <- 0 until params.NumHarts/8) {
    imsic_l1(i) :*= AXI4Buffer() := pbus_xbar
  }
  for (i <- 0 until params.NumHarts/4) {
    imsic_l2(i) :*= imsic_l1(i/2)
  }
  for (i <- 0 until params.NumHarts/2) {
    imsic_l3(i) :*= AXI4Buffer() :=  imsic_l2(i/2)
  }
  for (i <- 0 until params.NumHarts/1) {
    imsic_l4(i) :*= imsic_l3(i/2)
  }
  for (i <- 0 until params.NumHarts) {
    sNodes(i) := AXI4Buffer() := imsic_l4(i)
  }

  // --- Module Implementation ---
  lazy val module = new Imp
  class Imp extends LazyModuleImp(this) {
    // slave access from hni
    val s_aplic = IO(Flipped(new AXI4Bundle(aplic_mNode.out.head._2.bundle))) // mNode.head.out for vector TDO
    // master to imsic
    val m = IO(Vec(
      params.NumHarts,new VerilogAXI4Record(sNodes.head.in.head._2.bundle)))
    // connect io
    aplic_mNode.out.head._1 <> s_aplic
    for (i <- 0 until params.NumHarts) {
      m(i).viewAs[AXI4Bundle] <> sNodes(i).in.head._1
      sNodes(i).in.head._1.ar.ready := true.B
      sNodes(i).in.head._1.r.bits.data := 0.U
      m(i).viewAs[AXI4Bundle].ar.bits.addr := 0.U
      m(i).viewAs[AXI4Bundle].ar.bits.id   := 0.U
      m(i).viewAs[AXI4Bundle].ar.bits.prot := 0.U
      m(i).viewAs[AXI4Bundle].ar.bits.size := 2.U
      m(i).viewAs[AXI4Bundle].ar.bits.len := 0.U
      m(i).viewAs[AXI4Bundle].ar.bits.burst := 0.U
      m(i).viewAs[AXI4Bundle].ar.bits.lock := 0.U
      m(i).viewAs[AXI4Bundle].ar.bits.cache := 0.U
      m(i).viewAs[AXI4Bundle].ar.bits.qos := 0.U
      m(i).viewAs[AXI4Bundle].ar.valid := false.B
      m(i).viewAs[AXI4Bundle].r.ready := true.B

    }
  }
}
class dmPbusTop(params: Pbus2Params)(implicit p: Parameters) extends LazyModule {
  // debugModule instance
  println("=== enter dmPbusTop class ====")
  val dm = LazyModule(new StandAloneDebugModule(
    useTL = false,
    baseAddress = params.DebugAddrMap.base,
    addrWidth = params.cpuAddrWidth,
    dataWidth = params.cpuDataWidth,
    hartNum = params.NumHarts
  ))
  // dm master: cpus--> cpu_xbarNto1-->cpu2dm_s
  val Cbus = LazyModule(new Cbus(params))
  val cpu2dm_s =
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(params.DebugAddrMap),
        supportsWrite = TransferSizes(1, params.cpuAddrWidth / 8),
        supportsRead = TransferSizes(1, params.cpuAddrWidth / 8)
      )),
      beatBytes = params.cpuAddrWidth / 8
    )))
  cpu2dm_s := Cbus.cpus_l4
  // define dm_s_self
  val dm_sNode =
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(params.DebugAddrMap),
        supportsWrite = TransferSizes(1, params.cpuAddrWidth / 8),
        supportsRead = TransferSizes(1, params.cpuAddrWidth / 8)
      )),
      beatBytes = params.cpuAddrWidth / 8
    )))
  // define dm_self channel dm_self_mNode <> cpu2dm_s & is_self_id
  // dm_self_mNode,dm_crs_mNode --> dmxbar2to1 --->debugModule
  val dm_self_mNode =
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "dm-self-Mnode",
        id = IdRange(0, 1 << params.idBits)
      ))
    )))
  val dm_mNode_crs =
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "dm-mnode-crs",
        id = IdRange(0, 1 << params.idBits)
      ))
    )))
  val dmxbar2to1 = AXI4Xbar()
  dmxbar2to1 := dm_self_mNode
  dmxbar2to1 := AXI4Buffer() := dm_mNode_crs
  dm_sNode := dmxbar2to1
  println("=== exit dmPbusTop class last ====")
  class Imp(outer: dmPbusTop) extends LazyRawModuleImp(outer) {
    println("==== enter uncoreTop Imp ... ==")
    val dm_crs_s = IO(Flipped(new AXI4Bundle(AXI4BundleParameters(
      addrBits = params.CrsAddrWidth,dataBits=params.CrsDataWidth,idBits = params.idBits
    ))))// cross-die slave ports for debug
    val dm_crs_m = IO(new AXI4Bundle(AXI4BundleParameters(
      addrBits = params.CrsAddrWidth,dataBits=params.CrsDataWidth,idBits = params.idBits
    )))
    // instance debugModule sba port
    val dm_m = Option.when(params.dmHasBusMaster)(IO(new VerilogAXI4Record(dm.axi4masternode.get.params)))
    val dmio = IO(new dm.debugModule.DebugModuleIO)
    val dmint = IO(Output(UInt(params.NumHarts.W)))
    val req_id = IO(Input(UInt(params.DieIDWidth.W))) // die id number for request die
    val self_id = IO(Input(UInt(params.DieIDWidth.W))) // die id number for current die
    val isselfid = req_id === self_id
    dm_mNode_crs.out.head._1 <> dm_crs_s
    println("==== test00..==")
    dm_crs_m.aw <> cpu2dm_s.in.head._1.aw
    dm_crs_m.w <> cpu2dm_s.in.head._1.w
    dm_crs_m.b <> cpu2dm_s.in.head._1.b
    dm_crs_m.ar <> cpu2dm_s.in.head._1.ar
    dm_crs_m.r <> cpu2dm_s.in.head._1.r
    dm_crs_m.aw.valid := !isselfid & cpu2dm_s.in.head._1.aw.valid
    dm_crs_m.w.valid := !isselfid & cpu2dm_s.in.head._1.w.valid
    dm_crs_m.ar.valid := !isselfid & cpu2dm_s.in.head._1.ar.valid
    dm_m.foreach(_ <> dm.axi4masternode.get)
    dm.axi4node.foreach(_.getWrappedValue.viewAs[AXI4Bundle] <> dm_sNode.in.head._1)
    dm_self_mNode.out.head._1 <> cpu2dm_s.in.head._1
    dm_self_mNode.out.head._1.aw.valid := isselfid & cpu2dm_s.in.head._1.aw.valid
    dm_self_mNode.out.head._1.w.valid  := isselfid & cpu2dm_s.in.head._1.w.valid
    dm_self_mNode.out.head._1.ar.valid := isselfid & cpu2dm_s.in.head._1.ar.valid
//    for (i <- 0 until params.NumHarts) {
//      cpu_mNodes(i).out.head._1 <> cpu_s(i).viewAs[AXI4Bundle]
//    }
    dmio <> dm.module.io
    val dmintSrc = dm.int.getWrappedValue.asInstanceOf[HeterogeneousBag[Vec[Bool]]]
    dmint := dmintSrc.head.asUInt
    println("==== dmPbusTop Imp end ..==")
  }
  println("==== dmPbusTop before override define ..==")
  override lazy val module = new Imp(this)
  println("==== dmPbusTop after override define ..==")
}
class uncoreTop(params: Pbus2Params)(implicit p: Parameters) extends LazyModule {

  // cpu axi ports -> xbar1to2 -> imsic slave and dm slave
  println("====start: enter uncoreTop ..==")
  val cpu_mNodes = Seq.fill(params.NumHarts) {
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "cpu-Mnode",
        maxFlight = Some(0),
        id = IdRange(0, 1 << params.idBits)
      ))
    )))
  }
  val cpu_xbar1to2 = Seq.fill(params.NumHarts)(AXI4Xbar())
  println("====uncoreTop: before imsicTop instance ..==")
  // instance modules
  val imsicTop = LazyModule(new imsicPbusTop(params))
  val hni_mNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "master-node",
      maxFlight = Some(0),
      id = IdRange(0, 1 << params.idBits)
    ))
  )))
  imsicTop.hni_s_xbar := hni_mNode
  println("====uncoreTop: after imsicTop instance ..==")
  println("====uncoreTop: before dmTop instance ..==")
  val dmTop = LazyModule(new dmPbusTop(params))
  println("====uncoreTop: after dmTop instance ..==")
  println("====uncoreTop: before syscnt instance ..==")
  val syscnt = LazyModule(new StandAloneSYSCNT(
    useTL = false,
    baseAddress = params.SYSCNTAddrMap.base,
    addrWidth = params.periParams.addrWidth,
    dataWidth = params.periParams.timedataBytes * 8,
    hartNum = params.NumHarts
  ))
  println("====uncoreTop: after syscnt instance ..==")
  for (i <- 0 until params.NumHarts) {
    cpu_xbar1to2(i) := AXI4Buffer() := cpu_mNodes(i)
    imsicTop.Cbus.cpus_l0(i) := cpu_xbar1to2(i)
    dmTop.Cbus.cpus_l0(i) := cpu_xbar1to2(i)
  }
  val peri_xbar = AXI4Xbar()
  val peri_mNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "master-node",
      maxFlight = Some(0),
      id = IdRange(0, 1 << params.idBits)
    ))
  )))
  // peri snode <> aplic cfg
  // aplic cfg <> slaveNode <> xbar <> perixbar
  val peri_sNode =
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(params.aplicParams.APLICAddrMap),
        supportsWrite = TransferSizes(1, params.aplicParams.CFG_DATA_WIDTH / 8),
        supportsRead = TransferSizes(1, params.aplicParams.CFG_DATA_WIDTH / 8)
      )),
      beatBytes = params.aplicParams.CFG_DATA_WIDTH / 8
    )))
  peri_xbar  := peri_mNode
  peri_sNode := peri_xbar
  val peri_s1Node =
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(params.SYSCNTAddrMap),
        supportsWrite = TransferSizes(1, params.periParams.timedataBytes),
        supportsRead = TransferSizes(1, params.periParams.timedataBytes)
      )),
      beatBytes = params.periParams.timedataBytes
    )))
  peri_s1Node := peri_xbar
  // define debugModule sba node
  println("==== uncoreTop last..==")
  class Imp(outer: uncoreTop) extends LazyRawModuleImp(outer) {
    // 在模块实现类中添加前缀注解
    println("==== start: enter uncoreTop Imp..==")
    val target = this.toNamed
    annotate(new ChiselAnnotation {
      def toFirrtl = NestedPrefixModulesAnnotation(
        target = target, // 目标模块为当前 uncoreTop 的实现类
        prefix = "uncore_",
        inclusive = true // 递归为所有子模块添加前缀
      )
    })
    val i_dft_icg_scan_en    = IO(Input(Bool()))
    val i_aplic_wire_int_vld = IO(Input(UInt(params.NumIntSrcs.W)))
    val rtc_clock            = IO(Input(Clock()))
    val rtc_reset            = IO(Input(AsyncReset()))
    val clock                = IO(Input(Clock()))
    val reset                = IO(Input(AsyncReset()))
    val time                 = IO(Output(ValidIO(UInt(64.W))))
    val peri_s = IO(Flipped(new AXI4Bundle(peri_mNode.out.head._2.bundle))) // peri slave ports: aplic and clint
    val hni_s = IO(Flipped(new VerilogAXI4Record(hni_mNode.out.head._2.bundle)))
    val msi_m  = IO(imsicTop.module.m.cloneType) // to imsic master ports
    val cpu_s =
      IO(Vec(params.NumHarts, Flipped(new VerilogAXI4Record(cpu_mNodes.head.out.head._2.bundle)))) // cpu access ports
    val dm_crs_s = IO(Flipped(new AXI4Bundle(dmTop.module.dm_crs_s.params))) // cross-die slave ports for debug
    val dm_crs_m = IO(dmTop.module.dm_crs_m.cloneType)
    // instance debugModule sba port
    val dm_m    = Option.when(params.dmHasBusMaster)(IO(dmTop.module.dm_m.get.cloneType))
    val dmio    = IO(dmTop.module.dmio.cloneType)
    val dmint   = IO(dmTop.module.dmint.cloneType)
    val req_id = IO(Input(UInt(params.DieIDWidth.W))) // die id number for request die
    val self_id = IO(Input(UInt(params.DieIDWidth.W))) // die id number for current die
    childClock := clock
    childReset := reset
    // instance aplic
    withClockAndReset(clock, reset) {
      val aplic_top = Module(new aplic_top(params.aplicParams))
      // aplic
      aplic_top.i_aplic_wire_int_vld := i_aplic_wire_int_vld
      aplic_top.i_dft_icg_scan_en    := i_dft_icg_scan_en
      aplic_top.aplic_s <> peri_sNode.in.head._1
      imsicTop.module.s_aplic <> aplic_top.aplic_m
    }
    // connect io
    hni_mNode.out.head._1 <> hni_s.viewAs[AXI4Bundle]
    peri_mNode.out.head._1 <> peri_s // uncore peri cfg slave io
    cpu_mNodes.zip(cpu_s).foreach { case (node, io) => node.out.head._1 <> io.viewAs[AXI4Bundle] }
    // bypass the read channel
    hni_mNode.out.head._1.ar.bits.addr := 0.U
    hni_mNode.out.head._1.ar.bits.id   := 0.U
    hni_mNode.out.head._1.ar.bits.prot := 0.U
    hni_mNode.out.head._1.ar.bits.size := 2.U
    hni_mNode.out.head._1.ar.bits.len := 0.U
    hni_mNode.out.head._1.ar.bits.burst := 0.U
    hni_mNode.out.head._1.ar.bits.lock := 0.U
    hni_mNode.out.head._1.ar.bits.cache := 0.U
    hni_mNode.out.head._1.ar.bits.qos := 0.U
    hni_mNode.out.head._1.ar.valid := false.B
    hni_mNode.out.head._1.r.ready := true.B
    // connection about cross-die access ports for debug
    println("==== 12.17start to connect debugModule..==")
    syscnt.axi4node.foreach(_.getWrappedValue.viewAs[AXI4Bundle] <> peri_s1Node.in.head._1)
    // cpu2msi_sNodes -> imsicTop
    msi_m <> imsicTop.module.m
    dm_crs_s <> dmTop.module.dm_crs_s
    dm_crs_m <> dmTop.module.dm_crs_m
    dmTop.module.dm_m.foreach(_ <> dm_m.get)
    dmio <> dmTop.module.dmio
    dmint <> dmTop.module.dmint
    dmTop.module.req_id := req_id
    dmTop.module.self_id := self_id
  // syscnt connect
    syscnt.module.rtc_clock       := rtc_clock
    syscnt.module.rtc_reset       := rtc_reset
    syscnt.module.clock           := clock
    syscnt.module.reset           := reset
    syscnt.module.io.stop_en      := false.B
    syscnt.module.io.update_en    := false.B
    syscnt.module.io.update_value := false.B
    time                          := syscnt.module.io.time
    println("==== start: exit  uncoreTop Imp, the last .....==")
  }
  println("==== uncoreTop before override ==")
  override lazy val module = new Imp(this)
  println("==== uncoreTop after override ==")
}

/**
 * Main object to generate SystemVerilog for the IMSICPbus module.
 */
object PbusGen extends App {
  // Example configuration with mixed input widths
  val aplicparams = AplicParams(
    CFG_ADDR_WIDTH = 40,
    CFG_DATA_WIDTH = 64,
    CFG_ID_WIDTH = 8,
    APLICAddrMap = AddressSet(0x31100000L, 0x7fff),
    MSI_DATA_WIDTH = 32,
    NumIntSrcs = 512
  )
  val periParams = PeriParams(
    slaveDataBytes = 8,
    timedataBytes = 8
  )
  val dmParams = DebugModuleParams(
    baseAddress = 0x38020000L,
    // nDMIAddrSize  : Int = 7,
    // nProgramBufferWords: Int = 16,
    nAbstractDataWords = 2,
    nScratch = 2,
    hasBusMaster = true,
    // clockGate : Boolean = true,
    maxSupportedSBAccess = 64,
    // supportQuickAccess : Boolean = false,
    // supportHartArray   : Boolean = true,
    // nHaltGroups        : Int = 1,
    // nExtTriggers       : Int = 0,
    hasHartResets = true,
    // hasImplicitEbreak  = false,
    // hasAuthentication  = false,
    crossingHasSafeReset = false
  )
  val dmAtParams = DebugAttachParams(
    protocols = Set(JTAG)
  )
  val params = Pbus2Params(aplicParams = aplicparams, periParams = periParams, NumHarts=16)
  implicit val p: Parameters = Parameters.empty.alterPartial {
    case SoCParamsKey   => SoCParameters()
    case DebugModuleKey => Some(dmParams)
    case ExportDebug    => dmAtParams
    case MaxHartIdBits  => log2Up(params.NumHarts) max 6
  }

  val pbusM = LazyModule(new uncoreTop(params)(p))

  println("Generating the Pbus SystemVerilog...")
  val path = """./build/rtl/"""
  (new ChiselStage).execute(
    Array(
      "--target-dir",
      path,
      "--split-verilog"
    ),
    Seq(
      ChiselGeneratorAnnotation(() => pbusM.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--disable-annotation-unknown"),
      FirtoolOption(
        "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
      ),
      _root_.circt.stage.CIRCTTargetAnnotation(_root_.circt.stage.CIRCTTarget.SystemVerilog)
    )
  )
//println("Generating the Pbus SystemVerilog...")
//(new ChiselStage).execute(
//  args,
//  Seq(chisel3.stage.ChiselGeneratorAnnotation(() => LazyModule(new uncoreTop(params)).module))
//)
}
