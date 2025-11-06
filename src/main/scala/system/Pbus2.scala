// SPDX-License-Identifier: Apache-2.0
// See LICENSE.txt for license details.

package pbus

//import _root_.circt.stage.ChiselStage
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import device.SYSCNTConsts
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.ValName
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.HeterogeneousBag
import org.chipsalliance.cde.config.Parameters
import utils.VerilogAXI4Record
import _root_.circt.stage._
import chiseltest.simulator.VerilatorFlags
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import chiseltest._
import chiseltest.ChiselScalatestTester
import chiseltest.VerilatorBackendAnnotation


import chisel3.stage.ChiselGeneratorAnnotation


/**
 * Configurable parameters for the Pbus2 interconnect.
 *
 * @param numInputs       Number of input AXI4 ports. Default is 10.
 * @param inputIdBits     ID width for each input port. Default is 4.
 * @param inputDataWidths A sequence of data widths for each input port.
 * @param numOutputs      Number of output AXI4 ports. The topology is fixed to 3.
 * @param outputAddrMap   A sequence of AddressSet for each output port.
 * @param outputDataWidth Data width for each output port. Default is 64.
 */
case class Pbus2Params(
    numHarts:    Int = 1, // number of cpus +1(aplic)+1(pcie msi)
    inputIdBits: Int = 16,
//  inputDataWidths: Seq[Int] = Seq.fill(10)(64),
//  numOutputs: Int = 3, // This topology is fixed to 3 outputs
    APLICcfgAddr:  AddressSet = AddressSet(0x31100000L, 0x7fff),      // 32KB
    SYSCNTcfgAddr: AddressSet = AddressSet(0x38040000L, 0x1000 - 1), // SYSCNTConsts.size - 1), // 0x1000
    DebugMAddr:    AddressSet = AddressSet(0x38020000L, 0x1000 - 1), // 4KB
    IMSICTotalAddr: Seq[AddressSet] = Seq(
      AddressSet(0x3a000000L, 0xffffff),
      AddressSet(0x3b000000L, 0xffffff)
    ), // 4KB
    IMSICParams: aia.IMSICParams = aia.IMSICParams(
      imsicIntSrcWidth = 8,
      mAddr = 0x3a800000,
      sgAddr = 0x3b000000,
      geilen = 5,
      vgeinWidth = 6,
      iselectWidth = 12,
      EnableImsicAsyncBridge = true,
      HasTEEIMSIC = false
    ),
//  outputAddrMap: Seq[AddressSet] = Seq(
//    AddressSet(0x00000000L, 0x3fffffffL), // 1GB for Slave 0
//    AddressSet(0x40000000L, 0x7fffffffL), // 1GB for Slave 1
//    AddressSet(0x80000000L, 0xffffffffL)  // 2GB for Slave 2
//  ),
    outputDataWidth: Int = 32
) {
  lazy val numInputs = numHarts + 0
//  require(numInputs == 10, "This imsic_pbus topology is fixed to 10 inputs.")
//  require(inputDataWidths.length == numInputs, s"inputDataWidths length must match numInputs ($numInputs).")
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
////  val inNodes = Seq.fill(params.numInputs) {
////    AXI4MasterNode(Seq(AXI4MasterPortParameters(
////      masters = Seq(AXI4MasterParameters(
////        name = "master-node",
////        id = IdRange(0, 1 << params.inputIdBits)
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
////  val inNodes = Seq.fill(params.numInputs) {
////    AXI4MasterNode(Seq(AXI4MasterPortParameters(
////      masters = Seq(AXI4MasterParameters(
////        name = "master-node",
////        id = IdRange(0, 1 << params.inputIdBits)
////      ))
////    )))
////  }
//  val outNodes = {
//    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
//      slaves = Seq(AXI4SlaveParameters(
//        address = params.IMSICTotalAddr,
//        supportsWrite = TransferSizes(1, params.outputDataWidth/8), // TDO
//        supportsRead = TransferSizes(1, params.outputDataWidth/8) // TDO
//      )),
//      beatBytes = params.outputDataWidth / 8
//    )))
//  }
//  val pbus_xbar = AXI4Xbar()
////  for (i <- 0 until params.numInputs) {
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

class uncoreTop(params: Pbus2Params)(implicit p: Parameters) extends LazyModule {
//  val IMSICPbus = LazyModule(new IMSICPbus(params))
  // instance aplic module
  val aplic_top = LazyModule(new aplic_top(AplicParams(AplicRange = params.APLICcfgAddr)))
  val pbus_xbar = AXI4Xbar()
  pbus_xbar := aplic_top.o_axi4
  val msi_m =
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = params.IMSICTotalAddr,
        supportsWrite = TransferSizes(1, params.outputDataWidth / 8),
        supportsRead = TransferSizes(1, params.outputDataWidth / 8)
      )),
      beatBytes = params.outputDataWidth / 8
    )))
  msi_m :*= pbus_xbar
  val aplic_s = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "master-node",
      id = IdRange(0, 1 << params.inputIdBits)
    ))
  )))

  aplic_top.i_axi4 := aplic_s
  InModuleBody(msi_m.makeIOs()(ValName("msi_m")))
  InModuleBody(aplic_s.makeIOs()(ValName("aplic_s")))
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
  lazy val module = new LazyModuleImp(this) {
    val i_dft_icg_scan_en    = IO(Input(Bool()))
    val i_aplic_wire_int_vld = IO(Input(UInt(512.W)))
    // connect io
    aplic_top.module.i_aplic_wire_int_vld := i_aplic_wire_int_vld
    aplic_top.module.i_dft_icg_scan_en    := i_dft_icg_scan_en
//    IMSICPbus.inNodes(1).in.head._1.aw <> aplic_top.o_axi4.out.head._1.aw
//    IMSICPbus.inNodes(1).in.head._1.w <> aplic_top.o_axi4.out.head._1.w
//    IMSICPbus.inNodes(1).in.head._1.b <> aplic_top.o_axi4.out.head._1.b
    // Define IO ports using a HeterogeneousBag for varied input widths
//    val in_bundles = params.inputDataWidths.map { w =>
//      AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = w, idBits = params.inputIdBits))
//    }
//    val in = IO(Flipped(HeterogeneousBag(in_bundles)))
//    val out = IO(Vec(params.numOutputs, AXI4Bundle(slaves.head.in.head._2.bundle)))
//
//    // Connect IO to the diplomacy nodes
//    (masters.zip(in)).foreach { case (node, io) => node.out.head._1 <> io }
//    (slaves.zip(out)).foreach { case (node, io) => io <> node.in.head._1 }
  }
}

/**
 * Main object to generate SystemVerilog for the IMSICPbus module.
 */
object PbusGen extends App {
  // Example configuration with mixed input widths
  val params = Pbus2Params()
  implicit val p: Parameters = Parameters.empty

  val pbusM = LazyModule(new uncoreTop(params)(p))
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
