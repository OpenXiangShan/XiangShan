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

package uncore

import chisel3._
import chisel3.experimental.noPrefix
import utility._
import org.chipsalliance.cde.config._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._

import difftest.common.DifftestWiring
import difftest.util.Profile

class pbus extends LazyModule

// code for calculating program 9master input, 2slave output
  //define parameters
  val imsicRange = AddressSet(0x3a000000, 0x1FFFFFF)
  val dmRange = AddressSet(0x38020000, 0xFFF)
  val slaveDataBytes = 16
  val NumMasterNodes = 2

  //define privatebus name
  val PrivatebusXbar = AXI4Xbar()
  val busMasterNodes = Array.fill(NumMasterNodes){AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters("axi4", IdRange(0, 16)))
  )))}
  for(port <- busMasterNodes){
    PrivatebusXbar := AXI4Buffer() := port
  }

// define slave0 imsic
val imsicSlaveNode = AXI4SlaveNode(Seq(
  AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address = Seq(imsicRange),
      regionType = RegionType.UNCACHED,
      executable = true,
      supportsWrite = TransferSizes(1, slaveDataBytes),
      supportsRead = TransferSizes(1, slaveDataBytes),
      interleavedId = Some(0)
    )),
    beatBytes = 8)
))
  // define slave1 debugModule
  val dmSlaveNode = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(dmRange),
        regionType = RegionType.UNCACHED,
        executable = true,
        supportsWrite = TransferSizes(1, slaveDataBytes),
        supportsRead = TransferSizes(1, slaveDataBytes),
        interleavedId = Some(0)
      )),
      beatBytes = 8)
  ))
  imsicSlaveNode := AXI4Buffer() := PrivatebusXbar
  dmSlaveNode := AXI4Buffer() := PrivatebusXbar
// define io ports: io_s is master ports, io_m_ is slave ports
  val pbus_s = busMasterNodes.zipWithIndex.map { case (node, i) =>
    InModuleBody {
      node.makeIOs()(ValName(s"axi_s_$i"))
    }
  }
  val pbus_m_dm = InModuleBody {imsicSlaveNode.makeIOs()}
  val pbus_m_imsic = InModuleBody {dmSlaveNode.makeIOs()}
}

class XSNoCTop()(implicit p: Parameters) extends BaseXSSoc
  with HasXSTile
  with HasSeperatedBusOpt
  with HasIMSIC
  with HasTraceIO
{
  override lazy val desiredName: String = "XSTop"

  class XSNoCTopImp(wrapper: XSNoCTop) extends BaseXSSocImp(wrapper)
    with HasAsyncClockImp
    with HasXSTileCHIImp[XSNoCTop]
    with HasSeperatedBusImpOpt[XSNoCTop]
    with HasCoreLowPowerImp[XSNoCTop]
    with HasClintTimeImp[XSNoCTop]
    with HasIMSICImp[XSNoCTop]
    with HasDTSImp[XSNoCTop]
  {
    /* CPU Low Power State */
    val cpuGatedClock = noPrefix { buildLowPower(clock, cpuReset_sync) }
    core_with_l2.module.clock := cpuGatedClock
    core_with_l2.module.reset := cpuReset.asAsyncReset
  }

  lazy val module = new XSNoCTopImp(this)
}

class XSNoCDiffTop(implicit p: Parameters) extends XSNoCTop
{
  class XSNoCDiffTopImp(wrapper: XSNoCTop) extends XSNoCTopImp(wrapper) {
    // TODO:
    // XSDiffTop is only part of DUT, we can not instantiate difftest here.
    // Temporarily we collect Performance counters for each DiffTop, need control signals passed from Difftest
    val timer = IO(Input(UInt(64.W)))
    val logEnable = IO(Input(Bool()))
    val clean = IO(Input(Bool()))
    val dump = IO(Input(Bool()))

    withClockAndReset(clock, cpuReset_sync) {
      XSLog.collect(timer, logEnable, clean, dump)
    }
    DifftestWiring.createAndConnectExtraIOs()
    Profile.generateJson("XiangShan")
    XSNoCDiffTopChecker()
  }

  override lazy val module = new XSNoCDiffTopImp(this)
}

// TODO:
// Currently we use two-step XiangShan-Difftest, generating XS(with Diff Interface only) and Difftest seperately
// To avoid potential interface problem between XS and Diff, we add Checker and CI(dual-core)
// We will try one-step XS-Diff later
object XSNoCDiffTopChecker {
  def apply(): Unit = {
    val verilog =
      """
        |`define CONFIG_XSCORE_NR 2
        |`include "gateway_interface.svh"
        |module XSDiffTopChecker(
        | input                                 cpu_clk,
        | input                                 cpu_rstn,
        | input                                 sys_clk,
        | input                                 sys_rstn
        |);
        |wire [63:0] timer;
        |wire logEnable;
        |wire clean;
        |wire dump;
        |// FIXME: use siganls from Difftest rather than default value
        |assign timer = 64'b0;
        |assign logEnable = 1'b0;
        |assign clean = 1'b0;
        |assign dump = 1'b0;
        |gateway_if gateway_if_i();
        |core_if core_if_o[`CONFIG_XSCORE_NR]();
        |generate
        |    genvar i;
        |    for (i = 0; i < `CONFIG_XSCORE_NR; i = i+1)
        |    begin: u_CPU_TOP
        |    // FIXME: add missing ports
        |    XSTop u_XSTop (
        |        .clock                   (cpu_clk),
        |        .noc_clock               (sys_clk),
        |        .soc_clock               (sys_clk),
        |        .io_hartId               (6'h0 + i),
        |        .timer                   (timer),
        |        .logEnable               (logEnable),
        |        .clean                   (clean),
        |        .dump                    (dump),
        |        .gateway_out             (core_if_o[i])
        |    );
        |    end
        |endgenerate
        |    CoreToGateway u_CoreToGateway(
        |    .gateway_out (gateway_if_i.out),
        |    .core_in (core_if_o)
        |    );
        |    GatewayEndpoint u_GatewayEndpoint(
        |    .clock (sys_clk),
        |    .reset (sys_rstn),
        |    .gateway_in (gateway_if_i.in),
        |    .step ()
        |    );
        |
        |endmodule
      """.stripMargin
    FileRegisters.writeOutputFile("./build", "XSDiffTopChecker.sv", verilog)
  }
}
