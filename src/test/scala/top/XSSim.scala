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

package top

import chisel3._
import device._
import difftest.{DifftestModule, DifftestTopIO, HasDiffTestInterfaces, UARTIO}
import freechips.rocketchip.amba.axi4.{AXI4IdIndexer, AXI4Xbar}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config
import system._
import utils._
import xiangshan._

class XSSim()(implicit p: config.Parameters) extends LazyModule with HasXSParameter {
  // address space[0G - 1024G)
  val fullRange = AddressSet(0x0L, 0xffffffffffL)
  // MMIO address space[0G - 2G)
  val mmioRange = AddressSet(base = 0x0000000000L, mask = 0x007fffffffL)
  // DRAM address range[2G - 1024G)
  val dramRange = fullRange.subtract(mmioRange)

  val soc = LazyModule(new XSSoc())

  // 4x1 crossbar
  val xbar = AXI4Xbar()
  soc.mem.map{mem => xbar := mem}

  // AXIRam
  // -----------------------------------
  val axiMem = LazyModule(new AXI4RAM(
    dramRange,
    memByte = 64L * 1024 * 1024 * 1024,
    useBlackBox = true,
    beatBytes = L3BusWidth / 8
  )).node
  axiMem := xbar

  // AXI DMA
  // -----------------------------------
  val burst = LazyModule(new AXI4BurstMaster(
    startAddr = 0x80000000L,
    nOp = 0,
    beatBytes = L3BusWidth / 8))
  soc.dma := AXI4IdIndexer(16) := burst.node

  // AXI MMIO
  // -----------------------------------
  val axiMMIO = LazyModule(new SimMMIO())
  axiMMIO.axiBus := soc.extDev

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with HasDiffTestInterfaces {
    val NumCores = top.Parameters.get.socParameters.NumCores
    soc.module.io.extIntrs := 0.U

    val uart = IO(new UARTIO)
    uart <> axiMMIO.module.io.uart

    // Check and display all source and sink connections
    ExcitingUtils.fixConnections()
    ExcitingUtils.checkAndDisplay()

    override def cpuName: Option[String] = Some("XiangShan")

    override def connectTopIOs(difftest: DifftestTopIO): Unit = {
      difftest.uart <> uart
    }
  }
}

object TestMain extends App {
  val axiSim = args.contains("--with-dramsim3")

  // set soc parameters
  val socArgs = args.filterNot(_ == "--with-dramsim3")
  Parameters.set(
    (socArgs.contains("--fpga-platform"), socArgs.contains("--dual-core"), socArgs.contains("--disable-log")) match {
      case (true,  false, _)     => Parameters()
      case (true,   true, _)     => Parameters.dualCoreParameters
      case (false,  true,  true) => Parameters.simDualCoreParameters
      case (false, false,  true) => Parameters.simParameters
      case (false,  true, false) => Parameters.debugDualCoreParameters
      case (false, false, false) => Parameters.debugParameters
    }
  )

  val otherArgs = socArgs.filterNot(_ == "--disable-log").filterNot(_ == "--fpga-platform").filterNot(_ == "--dual-core")

  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(otherArgs)

  Generator.execute(
    firrtlOpts,
    DifftestModule.top(LazyModule(new XSSim()(config)).module),
    firtoolOpts
  )
}
