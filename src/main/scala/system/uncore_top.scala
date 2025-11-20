// SPDX-License-Identifier: Apache-2.0
// See LICENSE.txt for license details.

package pbus

//import _root_.circt.stage.ChiselStage
import chisel3._
import device.standalone.StandAloneDebugModule
import system.{HasSoCParameter, SoCParameters, SoCParamsKey}
import device.{SYSCNT, SYSCNTParams}
import chisel3.experimental.{ChiselAnnotation, annotate}
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import device.standalone.StandAloneSYSCNT
import chisel3.util._
import freechips.rocketchip.diplomacy
import device.SYSCNTConsts
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.ValName
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.HeterogeneousBag
//import org.chipsalliance.cde.config.{Config, Parameters}
import org.chipsalliance.cde.config._
import utils.VerilogAXI4Record
import _root_.circt.stage._
import aia.{AXI4IMSIC, IMSICParameKey, IMSICParameters}
import chisel3.stage.ChiselGeneratorAnnotation
import freechips.rocketchip.devices.debug.DebugModuleKey


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
    CFG_ID_WIDTH: Int = 16,
    APLICAddrMap: AddressSet = AddressSet(0x31100000L, 0x7fff),
    MSI_DATA_WIDTH: Int = 32,
    NumIntSrcs: Int = 512
    )
case class PeriParams(
    slaveDataBytes: Int = 8,
    timedataBytes: Int = 8,
    addrWidth: Int = 32
                     )
case class Pbus2Params(
    NumHarts:    Int = 1, // number of cpus +1(aplic)+1(pcie msi)
    idBits: Int = 2,
    cpuAddrWidth: Int = 32,
    cpuDataWidth: Int = 64,
    dmHasBusMaster: Boolean = true,
//  inputDataWidths: Seq[Int] = Seq.fill(10)(64),
//  numOutputs: Int = 3, // This topology is fixed to 3 outputs
//    APLICcfgAddr:  AddressSet = AddressSet(0x31100000L, 0x7fff),      // 32KB
    SYSCNTAddrMap: AddressSet = AddressSet(0x38040000L, 0x10000 - 1), // SYSCNTConsts.size - 1), 0x10000
    DebugAddrMap: AddressSet = AddressSet(0x38020000L, 0x1000 - 1), // 4KB
    IMSICAddrMap: Seq[AddressSet] = Seq(
      AddressSet(0x3a000000L, 0xffffff),
      AddressSet(0x3b000000L, 0xffffff)
    ), // 4KB
    IMSICParams: aia.IMSICParams = aia.IMSICParams(
      imsicIntSrcWidth = 9,
      mAddr = 0x3a800000,
      sgAddr = 0x3b000000,
      geilen = 5,
      vgeinWidth = 6,
      iselectWidth = 12,
      EnableImsicAsyncBridge = true,
      HasTEEIMSIC = false
    ),
    aplicParams: AplicParams,
//  outputAddrMap: Seq[AddressSet] = Seq(
//    AddressSet(0x00000000L, 0x3fffffffL), // 1GB for Slave 0
//    AddressSet(0x40000000L, 0x7fffffffL), // 1GB for Slave 1
//    AddressSet(0x80000000L, 0xffffffffL)  // 2GB for Slave 2
//  ),
    MSIOutDataWidth: Int = 32,
    periParams: PeriParams
) {
  lazy val NumInputs = NumHarts + 0
  lazy val NumIntSrcs = 1 << IMSICParams.imsicIntSrcWidth
//  require(NumInputs == 10, "ThishartNum imsic_pbus topology is fixed to 10 inputs.")
//  require(inputDataWidths.length == NumInputs, s"inputDataWidths length must match NumInputs ($numInputs).")
//  require(numOutputs == 3, "This imsic_pbus topology is fixed to 3 outputs.")
//  require(outputAddrMap.length == numOutputs, s"outputAddrMap length must match numOutputs ($numOutputs).")
}

/**
 * A configurable hierarchical AXI4 bus interconnect with heterogeneous input widths.
 */
//class IMSICPbus(params: Pbus2Params)(implicit p: Parameters) extends LazyModule {
//
//  val internalDataWidth = 64 // All crossbars will operate at this width
//
//  // --- Diplomacy Nodes ---
//
//  // Input nodes from configured masters
////  val inNodes = Seq.fill(params.NumInputs) {
////    AXI4MasterNode(Seq(AXI4MasterPortParameters(
////      masters = Seq(AXI4MasterParameters(
////        name = "master-node",
////        id = IdRange(0, 1 << params.idBits)
////      ))
////    )))
////  }
//
//  // Create a sequence of master nodes that are adapted to the internal bus width
////  val adaptedMasters = masters.zip(params.inputDataWidths).map { case (masterNode, width) =>
////    if (width == internalDataWidth) {
////      masterNode // No conversion needed
////    } else {
////      val to_tl = LazyModule(new AXI4ToTL())
////      val width_widget = LazyModule(new TLWidthWidget(internalDataWidth / 8))
////      val from_axi = LazyModule(new TLToAXI4)
////
////      masterNode :*= to_tl.node
////      to_tl.node :*= width_widget.node
////      width_widget.node :*= from_axi.node
////
////      from_axi.node
////    }
////  }
//
//  // Output nodes to configured slaves
////  val slaves = for (i <- 0 until params.numOutputs) yield {
////  val inNodes = Seq.fill(params.NumInputs) {
////    AXI4MasterNode(Seq(AXI4MasterPortParameters(
////      masters = Seq(AXI4MasterParameters(
////        name = "master-node",
////        id = IdRange(0, 1 << params.idBits)
////      ))
////    )))
////  }
//  val outNodes = {
//    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
//      slaves = Seq(AXI4SlaveParameters(
//        address = params.IMSICAddrMap,
//        supportsWrite = TransferSizes(1, params.MSIOutDataWidth/8), // TDO
//        supportsRead = TransferSizes(1, params.MSIOutDataWidth/8) // TDO
//      )),
//      beatBytes = params.MSIOutDataWidth / 8
//    )))
//  }
//  val pbus_xbar = AXI4Xbar()
////  for (i <- 0 until params.NumInputs) {
////    IMSICPbus :*= inNodes(i)
////  }
//  outNodes :*= pbus_xbar
////  val pbus_s = inNodes.zipWithIndex.map { case (node, i) =>
////    InModuleBody {
////      node.makeIOs()(ValName(s"s_$i"))
////    }
////  }
////  val pbus_m = InModuleBody(outNodes.makeIOs())
//  lazy val module = new Impl
//  class Impl extends LazyModuleImp(this)
//}

// --- Internal Crossbars ---
//  val xbar4to1_a = AXI4Xbar()
//  val xbar4to1_b = AXI4Xbar()
//  val xbar4to3 = AXI4Xbar()
//
//  // --- Diplomacy Graph Connections ---
//
//  for (i <- 0 until 4) { adaptedMasters(i) :*= xbar4to1_a }
//  for (i <- 4 until 8) { adaptedMasters(i) :*= xbar4to1_b }
//
//  xbar4to1_a :*= xbar4to3
//  xbar4to1_b :*= xbar4to3
//  adaptedMasters(8) :*= xbar4to3
//  adaptedMasters(9) :*= xbar4to3
//
//  for (slaveNode <- slaves) { xbar4to3 :*= slaveNode }

class imsicPbusTop(params: Pbus2Params)(implicit p: Parameters) extends LazyModule{
//  with HasSoCParameter{
//  val IMSICPbus = LazyModule(new IMSICPbus(params))
  val pbus_xbar = AXI4Xbar()
  val cpu_mNodes = Seq.fill(params.NumHarts) {
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "cpu-Mnode",
        id = IdRange(0, 1 << params.idBits)
      ))
    )))
  }
  val aplic_mNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "master-node",
      id = IdRange(0, 1 << params.idBits)
    ))
  )))
  val sNode = {
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = params.IMSICAddrMap,
        supportsWrite = TransferSizes(1, params.MSIOutDataWidth / 8),
        supportsRead = TransferSizes(1, params.MSIOutDataWidth / 8)
      )),
      beatBytes = params.MSIOutDataWidth / 8
    )))
  }
  for (i <- 0 until params.NumHarts) {
    pbus_xbar :*= cpu_mNodes(i)
  }
  pbus_xbar := aplic_mNode
  sNode := pbus_xbar


//  aplic_top.i_axi4 := s_peri
//  val s_periIO = InModuleBody {
//    s_peri.makeIOs()(ValName("s_peri"))  // 显式生成 IO 并命名
//  }
//  val m_msiIO = InModuleBody {
//    m_msi.makeIOs()(ValName("m_msi"))  // 显式生成 IO 并命名
//  }
//  InModuleBody(msi_m.makeIOs()(ValName("msi_m")))
//  InModuleBody(aplic_s.makeIOs()(ValName("aplic_s")))
//  IMSICPbus.pbus_xbar := aplic_top.o_axi4
  // connect module
//  val s_axi = InModuleBody(IMSICPbus.makeIOs())
//  val xbar_1 = AXI4Xbar()
//  xbar_1 := aplic_top.o_axi4
//    val s_axi = InModuleBody(IMSICPbus.makeIOs())
//  IMSICPbus.inNodes.zipWithIndex.filter(_._2 != 1).map { case (node, i) =>
////    if(i==1){
////      node :*= xbar_1
////    }
////    else {
//      InModuleBody {
//        node.makeIOs()(ValName(s"s_$i"))
////      }
//    }
//  }
//  InModuleBody(aplic_top.i_axi4.makeIOs())
//  InModuleBody(IMSICPbus.outNodes.makeIOs()(ValName(s"msi_m")))

  // --- Module Implementation ---
  lazy val module = new Imp
  class Imp extends LazyModuleImp(this)  {
    // slave access from cpu
    val s_aplic = IO(Flipped(new AXI4Bundle(aplic_mNode.out.head._2.bundle))) // mNode.head.out for vector TDO
    val s_cpu = IO(Vec(params.NumHarts, Flipped(new AXI4Bundle(cpu_mNodes.head.out.head._2.bundle)))) // mNode.head.out for vector TDO
    // master to imsic
    val m = IO(new AXI4Bundle(sNode.in.head._2.bundle))
    // connect io
    aplic_mNode.out.head._1 <> s_aplic
    m <> sNode.in.head._1
    (cpu_mNodes.zip(s_cpu)).foreach { case (node, io) => node.out.head._1 <> io }
    // Define IO ports using a HeterogeneousBag for varied input widths
//    val in_bundles = params.inputDataWidths.map { w =>
//      AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = w, idBits = params.idBits))
//    }
//    val in = IO(Flipped(HeterogeneousBag(in_bundles)))
//    val out = IO(Vec(params.numOutputs, AXI4Bundle(slaves.head.in.head._2.bundle)))
//
//    // Connect IO to the diplomacy nodes
//    (masters.zip(in)).foreach { case (node, io) => node.out.head._1 <> io }
//    (slaves.zip(out)).foreach { case (node, io) => io <> node.in.head._1 }
  }
}
class uncoreTop(params: Pbus2Params)(implicit p: Parameters) extends LazyModule {

 // Define the address map for the two output slave devices.
// val slaveAddress = Seq(
//     AddressSet(0x38000000, 0xFFF), // 2GB for 64-bit device
//     AddressSet(0x38020000, 0xFFF)  // 2GB for 32-bit device
// )

 // --- Diplomacy Nodes ---
 // Input nodes
// val msiMnodes = Seq.fill(params.NumInputs) {
//   AXI4MasterNode(Seq(AXI4MasterPortParameters(
//     masters = Seq(AXI4MasterParameters(
//       name = "master-node",
//       id = IdRange(0, 1 << params.idBits)
//     ))
//   )))
// }

  // Output nodes
//  val msiSNodes = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
//    slaves = Seq(AXI4SlaveParameters(
//      address = Seq(slaveAddress(0)),
//      supportsWrite = TransferSizes(1, params.MSIOutDataWidth / 8),
//      supportsRead = TransferSizes(1, params.MSIOutDataWidth / 8)
//    )),
  //    beatBytes = params.MSIOutDataWidth / 8
  //  )))
  // instance imsic_pbus_top module
  // instance aplic imsic syscnt module
  // cpu axi ports -> xbar1to2 -> imsic slave and dm slave
  val cpu_mNodes = Seq.fill(params.NumHarts) {
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "cpu-Mnode",
        id = IdRange(0, 1 << params.idBits)
      ))
    )))
  }
  val msi_sNodes = Seq.fill(params.NumHarts) {
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = params.IMSICAddrMap,
        supportsWrite = TransferSizes(1, params.cpuDataWidth / 8),
        supportsRead = TransferSizes(1, params.cpuDataWidth / 8)
      )),
      beatBytes = params.cpuDataWidth / 8
    )))
  }
  val dm_sNodes = Seq.fill(params.NumHarts) {
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(params.DebugAddrMap),
        supportsWrite = TransferSizes(1, params.cpuDataWidth / 8),
        supportsRead = TransferSizes(1, params.cpuDataWidth / 8)
      )),
      beatBytes = params.cpuDataWidth / 8
    )))
  }
  val cpu_xbar1to2 = Seq.fill(params.NumHarts) {AXI4Xbar()}
  for (i <- 0 until params.NumHarts) {
    cpu_xbar1to2(i) := cpu_mNodes(i)
    msi_sNodes(i) := cpu_xbar1to2(i)
    dm_sNodes(i) := cpu_xbar1to2(i)
  }
  // instance modules
  private val clintParam = SYSCNTParams(params.SYSCNTAddrMap.base)
  val aplic_top = LazyModule(new aplic_top(params.aplicParams))
  val imsic_pbus_top = LazyModule(new imsicPbusTop(params))
  val syscnt = LazyModule(new StandAloneSYSCNT(
    useTL = false,
    baseAddress = params.SYSCNTAddrMap.base,
    addrWidth = params.periParams.addrWidth,
    dataWidth = params.periParams.timedataBytes * 8,
    hartNum = params.NumHarts
  ))
  val peri_xbar = AXI4Xbar()
  val peri_mNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "master-node",
      id = IdRange(0, 1 << params.idBits)
    ))
  )))
  // peri snode <> aplic cfg
  // aplic cfg <> slaveNode <> xbar <> perixbar
  val peri_sNode = {
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(params.aplicParams.APLICAddrMap),
        supportsWrite = TransferSizes(1, params.aplicParams.CFG_DATA_WIDTH / 8),
        supportsRead = TransferSizes(1, params.aplicParams.CFG_DATA_WIDTH / 8)
      )),
      beatBytes = params.aplicParams.CFG_DATA_WIDTH / 8
    )))
  }
  peri_xbar := peri_mNode
  peri_sNode := peri_xbar
  val peri_s1Node = {
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(params.SYSCNTAddrMap),
        supportsWrite = TransferSizes(1, params.periParams.timedataBytes),
        supportsRead = TransferSizes(1, params.periParams.timedataBytes)
      )),
      beatBytes = params.periParams.timedataBytes
    )))
  }
  peri_s1Node := peri_xbar
  // debugModule integration
  val dm = LazyModule(new StandAloneDebugModule(
    useTL = false,
    baseAddress = params.DebugAddrMap.base,
    addrWidth = params.cpuAddrWidth,
    dataWidth = params.cpuDataWidth,
    hartNum  = params.NumHarts
  )(new Config((site, here, up) => {
    case SoCParamsKey => SoCParameters()
  })))

  val dmxbar = AXI4Xbar()
  val dm_mNodes = Seq.fill(params.NumHarts) {
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "dm-Mnode",
        id = IdRange(0, 1 << params.idBits)
      ))
    )))
  }
  val dm_sNode = {
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(params.DebugAddrMap),
        supportsWrite = TransferSizes(1, params.cpuAddrWidth / 8),
        supportsRead = TransferSizes(1, params.cpuAddrWidth / 8)
      )),
      beatBytes = params.cpuAddrWidth/8
    )))
  }
  for (i <- 0 until params.NumHarts) {
    dmxbar :*= dm_mNodes(i)
  }
  dm_sNode := dmxbar
  // define debugModule sba node
  class Imp(outer: uncoreTop) extends LazyRawModuleImp(outer)  {
    // 在模块实现类中添加前缀注解
    val target = this.toNamed
    annotate(new ChiselAnnotation {
      def toFirrtl = NestedPrefixModulesAnnotation(
        target = target,  // 目标模块为当前 uncoreTop 的实现类
        prefix = "uncore_",
        inclusive = true        // 递归为所有子模块添加前缀
      )
    })
    val i_dft_icg_scan_en    = IO(Input(Bool()))
    val i_aplic_wire_int_vld = IO(Input(UInt(params.NumIntSrcs.W)))
    val rtc_clock = IO(Input(Clock()))
    val rtc_reset = IO(Input(AsyncReset()))
    val clock = IO(Input(Clock()))
    val reset = IO(Input(AsyncReset()))
    val time      = IO(Output(ValidIO(UInt(64.W))))
    val peri_s = IO(Flipped(new AXI4Bundle(peri_mNode.out.head._2.bundle))) // peri slave ports: aplic and clint
    val msi_m = IO(new AXI4Bundle(imsic_pbus_top.sNode.in.head._2.bundle)) // to imsic master ports
    val cpu_s = IO(Vec(params.NumHarts, Flipped(new AXI4Bundle(cpu_mNodes.head.out.head._2.bundle)))) // cpu access ports
    // instance debugModule sba port
    val dm_m = Option.when(params.dmHasBusMaster)(IO(new VerilogAXI4Record(dm.axi4masternode.get.params)))
//    val dm_m = IO(new VerilogAXI4Record(dm.axi4masternode.get.params))
    val dmio = IO(new dm.debugModule.DebugModuleIO)
    // dm sba ports
//    dontTouch(peri_s)
//    dontTouch(msi_m)
    // connect io
    peri_mNode.out.head._1 <> peri_s // uncore peri cfg slave io
    imsic_pbus_top.module.s_aplic <> aplic_top.module.aplic_m
    (cpu_mNodes.zip(cpu_s)).foreach { case (node, io) => node.out.head._1 <> io }
    dm.axi4node.foreach(_ <> dm_sNode.in.head._1)
    dm_m.foreach(_ <> dm.axi4masternode.get)
//        dm_m <> dm.axi4masternode.get
    syscnt.axi4node.foreach(_ <> peri_s1Node.in.head._1)
    //msi_sNodes -> imsicPbusTop
    imsic_pbus_top.module.s_cpu.zip(msi_sNodes).foreach { case (io, node) => node.in.head._1 <> io }
    msi_m <> imsic_pbus_top.module.m
    for (i <- 0 until params.NumHarts) {
      dm_mNodes(i).out.head._1 <> dm_sNodes(i).in.head._1
    }
    dmio <> dm.module.io
    // aplic
    aplic_top.module.i_aplic_wire_int_vld := i_aplic_wire_int_vld
    aplic_top.module.i_dft_icg_scan_en := i_dft_icg_scan_en
    aplic_top.module.aplic_s <> peri_sNode.in.head._1
    // syscnt connect
    syscnt.module.rtc_clock := rtc_clock
    syscnt.module.rtc_reset := rtc_reset
    syscnt.module.clock := clock
    syscnt.module.reset := reset
    syscnt.module.io.stop_en := false.B
    syscnt.module.io.update_en := false.B
    syscnt.module.io.update_value := false.B
    time := syscnt.module.io.time
  }
  lazy val module = new Imp(this)
}
/**
 * Main object to generate SystemVerilog for the IMSICPbus module.
 */

//import org.chipsalliance.cde.config.{Config, Parameters}

// 定义具体参数结构
//case class SoCParameters(numCores: Int, addrWidth: Int)
//
//// 定义配置键
//case object SoCParamsKey extends Field[SoCParameters]

// 定义配置类，为 SoCParamsKey 赋值
//class DefaultConfig extends Config(
//  (site, here, up) => Map(
//    SoCParamsKey -> SoCParameters(
//      numCores = 4,
//      addrWidth = 32
//    )
//  )
//)
object PbusGen extends App {
  // Example configuration with mixed input widths
  val aplicparams = AplicParams(
    CFG_ADDR_WIDTH = 40,
    CFG_DATA_WIDTH = 64,
    CFG_ID_WIDTH = 16,
    APLICAddrMap = AddressSet(0x31100000L, 0x7fff),
    MSI_DATA_WIDTH = 32,
    NumIntSrcs = 512
  )
  val periParams = PeriParams(
    slaveDataBytes = 8,
    timedataBytes = 8
  )

  val params = Pbus2Params(aplicParams=aplicparams,periParams=periParams)
  implicit val p: Parameters = Parameters.empty.alterPartial({
    case SoCParamsKey => SoCParameters()
  }) //Parameters.empty

  val pbusM = LazyModule(new uncoreTop(params))

  println("Generating the Pbus SystemVerilog...")
    val path = """./build/rtl/"""
    (new ChiselStage).execute(
      Array(
        "--target-dir", path,
        "--split-verilog"
      ),
      Seq(
        ChiselGeneratorAnnotation(() => pbusM.module),
        FirtoolOption("--disable-all-randomization"),
        FirtoolOption("--disable-annotation-unknown"),
        FirtoolOption("--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"),
        _root_.circt.stage.CIRCTTargetAnnotation(_root_.circt.stage.CIRCTTarget.SystemVerilog)
      )
    )
//println("Generating the Pbus SystemVerilog...")
//(new ChiselStage).execute(
//  args,
//  Seq(chisel3.stage.ChiselGeneratorAnnotation(() => LazyModule(new uncoreTop(params)).module))
//)
}
