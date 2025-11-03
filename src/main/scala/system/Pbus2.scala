// SPDX-License-Identifier: Apache-2.0
// See LICENSE.txt for license details.

package system

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import _root_.circt.stage.ChiselStage
import chisel3.experimental.BundleLiterals._
import freechips.rocketchip.util.HeterogeneousBag

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
  numInputs: Int = 10,
  inputIdBits: Int = 4,
  inputDataWidths: Seq[Int] = Seq.fill(10)(64),
  numOutputs: Int = 3, // This topology is fixed to 3 outputs
  outputAddrMap: Seq[AddressSet] = Seq(
    AddressSet(0x00000000L, 0x3fffffffL), // 1GB for Slave 0
    AddressSet(0x40000000L, 0x7fffffffL), // 1GB for Slave 1
    AddressSet(0x80000000L, 0xffffffffL)  // 2GB for Slave 2
  ),
  outputDataWidth: Int = 64
) {
  require(numInputs == 10, "This Pbus2 topology is fixed to 10 inputs.")
  require(inputDataWidths.length == numInputs, s"inputDataWidths length must match numInputs ($numInputs).")
  require(numOutputs == 3, "This Pbus2 topology is fixed to 3 outputs.")
  require(outputAddrMap.length == numOutputs, s"outputAddrMap length must match numOutputs ($numOutputs).")
}

/**
 * A configurable hierarchical AXI4 bus interconnect with heterogeneous input widths.
 */
class Pbus2(params: Pbus2Params)(implicit p: Parameters) extends LazyModule {

  val internalDataWidth = 64 // All crossbars will operate at this width

  // --- Diplomacy Nodes ---

  // Input nodes from configured masters
  val masters = Seq.fill(params.numInputs) {
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "master-node",
        id = IdRange(0, 1 << params.inputIdBits)
      ))
    )))
  }

  // Create a sequence of master nodes that are adapted to the internal bus width
//  val adaptedMasters = masters.zip(params.inputDataWidths).map { case (masterNode, width) =>
//    if (width == internalDataWidth) {
//      masterNode // No conversion needed
//    } else {
//      val to_tl = LazyModule(new AXI4ToTL())
//      val width_widget = LazyModule(new TLWidthWidget(internalDataWidth / 8))
//      val from_axi = LazyModule(new TLToAXI4)
//
//      masterNode :*= to_tl.node
//      to_tl.node :*= width_widget.node
//      width_widget.node :*= from_axi.node
//
//      from_axi.node
//    }
//  }

  // Output nodes to configured slaves
  val slaves = for (i <- 0 until params.numOutputs) yield {
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(params.outputAddrMap(i)),
        supportsWrite = TransferSizes(1, 256),
        supportsRead = TransferSizes(1, 256)
      )),
      beatBytes = params.outputDataWidth / 8
    )))
  }

  // --- Internal Crossbars ---

  val xbar4to1_a = AXI4Xbar()
  val xbar4to1_b = AXI4Xbar()
  val xbar4to3 = AXI4Xbar()

  // --- Diplomacy Graph Connections ---

  for (i <- 0 until 4) { adaptedMasters(i) :*= xbar4to1_a }
  for (i <- 4 until 8) { adaptedMasters(i) :*= xbar4to1_b }

  xbar4to1_a :*= xbar4to3
  xbar4to1_b :*= xbar4to3
  adaptedMasters(8) :*= xbar4to3
  adaptedMasters(9) :*= xbar4to3

  for (slaveNode <- slaves) { xbar4to3 :*= slaveNode }

  // --- Module Implementation ---
  lazy val module = new LazyModuleImp(this) {
    // Define IO ports using a HeterogeneousBag for varied input widths
    val in_bundles = params.inputDataWidths.map { w =>
      AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = w, idBits = params.inputIdBits))
    }
    val in = IO(Flipped(HeterogeneousBag(in_bundles)))
    val out = IO(Vec(params.numOutputs, AXI4Bundle(slaves.head.in.head._2.bundle)))

    // Connect IO to the diplomacy nodes
    (masters.zip(in)).foreach { case (node, io) => node.out.head._1 <> io }
    (slaves.zip(out)).foreach { case (node, io) => io <> node.in.head._1 }
  }
}

/**
 * Main object to generate SystemVerilog for the Pbus2 module.
 */
object Pbus2Gen extends App {
  // Example configuration with mixed input widths
  val params = Pbus2Params(
    inputDataWidths = Seq(32, 32, 64, 64, 128, 128, 64, 64, 32, 64)
  )
  implicit val p: Parameters = Parameters.empty

  println("Generating the Pbus2 SystemVerilog...")
  (new ChiselStage).execute(
    args,
    Seq(chisel3.stage.ChiselGeneratorAnnotation(() => LazyModule(new Pbus2(params)).module))
  )
}