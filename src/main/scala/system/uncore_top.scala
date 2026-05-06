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
import freechips.rocketchip.amba.axi4.{AXI4Deinterleaver, _}
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
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError}
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
import utility.ResetGen
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
    APLICAddrMap:   AddressSet = AddressSet(0x1E020000L, 0x7fff),
    MSI_DATA_WIDTH: Int = 32,
    NumIntSrcs:     Int = 512
)
case class PeriParams(
    slaveDataBytes: Int = 8,
    timedataBytes:  Int = 8,
    addrWidth:      Int = 32
)

object Pbus2Params {
  private val imsicMModeSize = 0x1000L
  private val imsicSModeStride = 0x10000L
  private val imsicSModeSize = 0x8000L

  def defaultLocalImsicAddrMap(numHarts: Int, imsicParams: aia.IMSICParams): Seq[AddressSet] = {
    (0 until numHarts).flatMap { hartId =>
      Seq(
        AddressSet(imsicParams.mAddr + hartId * imsicMModeSize, imsicMModeSize - 1),
        AddressSet(imsicParams.sgAddr + hartId * imsicSModeStride, imsicSModeSize - 1)
      )
    }
  }

  def defaultCrsImsicAddrMap(numDies: Int): Seq[AddressSet] = {
    (1 to numDies).flatMap { dieId =>
      Seq(
        AddressSet((BigInt(dieId) << 44) + 0x1C000000L, 0xFFFF), // bit[11:0] 4KB,bit[15:12] for 16 harts each die
        AddressSet((BigInt(dieId) << 44) + 0x1D000000L, 0xFFFFF) // bit[15:12] for s+7vs interrupt file,bit[19:16] for 16 harts each die
      )
    }
  }

  def defaultCrsDmAddrMap(numDies: Int): Seq[AddressSet] = {
    (0 to numDies).map { dieId =>
      AddressSet((BigInt(dieId) << 44) + 0x1B000000L, 0xFFF)
    }
  }
}

case class Pbus2Params(
    NumHarts:       Int = 2, // number of cpus +1(aplic)+1(pcie msi)
    NumDies:        Int = 4,
    CPUidBits:      Int = 3,
    APLICidBits:    Int = 0,
    NOCidBits:    Int = 11,
    cpuAddrWidth:   Int = 32,
    cpuDataWidth:   Int = 64,
    dmDataWidth:   Int = 64,
    nocDataWidth:   Int = 256,
    dmHasBusMaster: Boolean = true,
    SYSCNTAddrMap: AddressSet = AddressSet(0x1E000000L, 0x10000 - 1), // SYSCNTConsts.size - 1), 0x10000
    DebugAddrMap:  AddressSet = AddressSet(0x1B000000L, 0x1000 - 1),  // 4KB
    dmsize:        Int = 0x1000,
    DieIDWidth:    Int = 3,
    // bit[47:44] >0, 0x1C00_0000~0x1DFF_FFFF, access to imsic for crossdie
    crsimsicAddrMapOverride: Seq[AddressSet] = Nil,
    // bit[47:44] >0,0x1B000000~0x1B000FFF,for cross die debug
    crsdmAddrMapOverride: Seq[AddressSet] = Nil,
    IMSICParams: aia.IMSICParams = aia.IMSICParams(
      imsicIntSrcWidth = 9,
      mAddr = 0x1C000000,
      sgAddr = 0x1D000000,
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
  val localImsicAddrMap: Seq[AddressSet] = Pbus2Params.defaultLocalImsicAddrMap(NumHarts, IMSICParams)
  val crsimsicAddrMap: Seq[AddressSet] =
    if (crsimsicAddrMapOverride.nonEmpty) crsimsicAddrMapOverride else Pbus2Params.defaultCrsImsicAddrMap(NumDies)
  val crsdmAddrMap: Seq[AddressSet] =
    if (crsdmAddrMapOverride.nonEmpty) crsdmAddrMapOverride else Pbus2Params.defaultCrsDmAddrMap(NumDies)
  lazy val NumInputs  = NumHarts + 0
  lazy val NumIntSrcs = 1 << IMSICParams.imsicIntSrcWidth
}

object AXIDataBridge {
  def errorAddrMapFromLegal(legalAddrMap: Seq[AddressSet]): Seq[AddressSet] = {
    require(legalAddrMap.nonEmpty, "AXIDataBridge requires at least one legal address set")
    val mergedLegalAddrMap = AddressSet.unify(legalAddrMap) // merge some addrsets into smaller number of addrsets, and also check if they are valid(addr+mask)
    require(mergedLegalAddrMap.forall(_.finite), "AXIDataBridge legal address sets must be finite")
    val addrBits = mergedLegalAddrMap.map(_.max.bitLength).max max 1
    val universe = AddressSet(0, (BigInt(1) << addrBits) - 1)
    val errorAddrMap = mergedLegalAddrMap.foldLeft(Seq(universe)) { case (remaining, legal) =>
      remaining.flatMap(_.subtract(legal))
    }
    errorAddrMap.distinct.sorted // remove duplicates and sort by address
  }
}

/**
 * A configurable hierarchical AXI4 bus interconnect with heterogeneous input widths.
 */
class AXIDataBridge(SrcDataWidth: Int, DestDataWidth: Int, errorAddrMap: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  println("=====AXIDataBridge: start define=====")
  private val indexedIdBits = 6
  private val maxInFlightSources = 1 << indexedIdBits
  val axi_xbar_i = AXI4Xbar()
  val axi_xbar_o = AXI4Xbar()
  //  private val tmp_xbar = TLXbar()
  val error_xbar = TLXbar()
  require(errorAddrMap.nonEmpty, "AXIDataBridge requires a non-empty error address map")
  private val bridgeMaxTransfer = scala.math.min(4096, (DestDataWidth / 8) * (1 << AXI4Parameters.lenBits))
  private val errorMinAlignment = errorAddrMap.map(_.alignment).min
  private val errorMaxTransfer =
    if (errorMinAlignment > bridgeMaxTransfer) bridgeMaxTransfer else errorMinAlignment.toInt
  val error = LazyModule(new TLError(
    params = DevNullParams(
      address = errorAddrMap,
      maxAtomic = 1,
      maxTransfer = errorMaxTransfer),
    beatBytes = DestDataWidth/8
  ))
  error.node := error_xbar

  axi_xbar_o :=
//    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4IdIndexer(indexedIdBits) :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(DestDataWidth/8) :=
    TLToAXI4(wcorrupt = false) :=
    TLSourceShrinker(maxInFlightSources) :=
    TLFragmenter(DestDataWidth/8, errorMaxTransfer, holdFirstDeny = true) :=
    error_xbar :=
    TLBuffer.chainNode(2) :=
    TLFIFOFixer() :=
    TLWidthWidget(SrcDataWidth/8) :=
    AXI4ToTL(wcorrupt = false) :=
    AXI4UserYanker(Some(1)) :=
    AXI4Fragmenter() :=
    AXI4IdIndexer(indexedIdBits) :=
    AXI4Buffer() :=
    axi_xbar_i
  lazy val module = new Imp
  class Imp extends LazyModuleImp(this)
}
//  axi_xbar_o :=
//  AXI4Buffer() :=
//  AXI4Buffer() :=
//  AXI4Buffer() :=
//  AXI4IdIndexer(CPUidBits =) :=
//  AXI4UserYanker() :=
//  AXI4Deinterleaver(L3BlockSize) :=
//  TLToAXI4() :=
//  TLSourceShrinker(64) :=
//  TLWidthWidget(L3OuterBusWidth / 8) :=
//  TLBuffer.chainNode(2) :=
//  mem_xbar

// Reset utility functions
object ResetUtils {
  def propagateReset(inReset: Reset): AsyncReset = withReset(inReset) {
    ResetGen(SYNC_NUM = 1)
  }

  def mergeResets(routeResets: Seq[Reset]): AsyncReset = {
    require(routeResets.nonEmpty, "Cbus route reset merge requires at least one input")
    routeResets.map(_.asBool).reduce(_ || _).asAsyncReset
  }

  def stageResetOut(mod: Module, inReset: Reset): AsyncReset = {
    mod.reset := inReset
    ResetUtils.propagateReset(mod.reset)
  }
}

final case class BufferedAXI4Connection(buffer: AXI4Buffer) {
  def module = buffer.module

  def sinkReset(sourceReset: Reset): AsyncReset = {
    buffer.module.reset := sourceReset
    ResetUtils.propagateReset(buffer.module.reset)
  }
}

class Cbus(params: Pbus2Params)(implicit p: Parameters) extends LazyModule {
  private def newXbar(name: String): AXI4Xbar = {
    val xbar = LazyModule(new AXI4Xbar())
    xbar.suggestName(name)
    xbar
  }

  private def newBuffer(name: String): AXI4Buffer = {
    val buffer = LazyModule(new AXI4Buffer())
    buffer.suggestName(name)
    buffer
  }

  private def connectThroughBuffer(sink: AXI4Node, source: AXI4Node, name: String, star: Boolean = false): BufferedAXI4Connection = {
    val buffer = newBuffer(name)
    if (star) sink :=* buffer.node else sink := buffer.node
    buffer.node := source
    BufferedAXI4Connection(buffer)
  }

  // cpu master: cpus--> cpu_xbarNto1-->cpu2imsic_s/cpu2dm_s
  val cpuLMs = (0 until params.NumHarts / 1).map { i =>
    newXbar(s"cpu_s_$i")
  }
  val cpus = cpuLMs.map(_.node)
  println("Cbus: start enter Cbus define")
  val NumCX = 11
  val NumCX_l1 = 4
  val xbar2to1LMs = (0 until NumCX).map { i =>
    newXbar(s"cx2to1_$i")
  }
  val xbar2to1 = xbar2to1LMs.map(_.node)
  val l1xbar2to1LMs = (0 until NumCX_l1).map { i =>
    newXbar(s"L1cx2to1_$i")
  }
  val l1xbar2to1 = l1xbar2to1LMs.map(_.node)

  val cpum_LM = newXbar("cpu_m")
  val cpum = cpum_LM.node
  println("Cbus: test 00 ===")
  val cpu0ToCx0Bufs = (0 until 2).map { i =>
    connectThroughBuffer(xbar2to1(0), cpus(i), s"cbus_cpu${i}_to_cx0_buf", star = true)
  }
  val cpu2ToCx1Buf = connectThroughBuffer(xbar2to1(1), cpus(2), "cbus_cpu2_to_cx1_buf")
  val cx0ToCx1Buf = connectThroughBuffer(xbar2to1(1), xbar2to1(0), "cbus_cx0_to_cx1_buf")
  val cpu3ToCx2Buf = connectThroughBuffer(xbar2to1(2), cpus(3), "cbus_cpu3_to_cx2_buf")
  val cx1ToCx2Buf = connectThroughBuffer(xbar2to1(2), xbar2to1(1), "cbus_cx1_to_cx2_buf")
  val cpu4ToCx3Bufs = (4 until 6).map { i =>
    connectThroughBuffer(xbar2to1(3), cpus(i), s"cbus_cpu${i}_to_cx3_buf", star = true)
  }
  val cpu7ToCx5Bufs = (4 until 6).map { i =>
    val cpuIdx = i + 3
    connectThroughBuffer(xbar2to1(5), cpus(cpuIdx), s"cbus_cpu${cpuIdx}_to_cx5_buf", star = true)
  }
  val cpu10ToCx7Bufs = (4 until 6).map { i =>
    val cpuIdx = i + 3 * 2
    connectThroughBuffer(xbar2to1(7), cpus(cpuIdx), s"cbus_cpu${cpuIdx}_to_cx7_buf", star = true)
  }
  val cpu13ToCx9Bufs = (4 until 6).map { i =>
    val cpuIdx = i + 3 * 3
    connectThroughBuffer(xbar2to1(9), cpus(cpuIdx), s"cbus_cpu${cpuIdx}_to_cx9_buf", star = true)
  }
  println("Cbus: test 01 ===")
//  xbar2to10->xbar2to11->xbar2to12   xbar2to13->xbar2to14,xbar2to15->xbar2to16,xbar2to17->xbar2to18, xbar2to19->xbar2to110

  val cx3ToCx4Buf = connectThroughBuffer(xbar2to1(4), xbar2to1(3), "cbus_cx3_to_cx4_buf")
  val cpu6ToCx4Buf = connectThroughBuffer(xbar2to1(4), cpus(6), "cbus_cpu6_to_cx4_buf")
  val cx5ToCx6Buf = connectThroughBuffer(xbar2to1(6), xbar2to1(5), "cbus_cx5_to_cx6_buf")
  val cpu9ToCx6Buf = connectThroughBuffer(xbar2to1(6), cpus(9), "cbus_cpu9_to_cx6_buf")
  val cx7ToCx8Buf = connectThroughBuffer(xbar2to1(8), xbar2to1(7), "cbus_cx7_to_cx8_buf")
  val cpu12ToCx8Buf = connectThroughBuffer(xbar2to1(8), cpus(12), "cbus_cpu12_to_cx8_buf")
  val cx9ToCx10Buf = connectThroughBuffer(xbar2to1(10), xbar2to1(9), "cbus_cx9_to_cx10_buf", star = true)
  val cpu15ToCx10Buf = connectThroughBuffer(xbar2to1(10), cpus(15), "cbus_cpu15_to_cx10_buf", star = true)
  println("Cbus: test 02 ===")

  val cx10ToL1x0Buf = connectThroughBuffer(l1xbar2to1(0), xbar2to1(10), "cbus_cx10_to_l1x0_buf")
  val l1x2ToL1x3Buf = connectThroughBuffer(l1xbar2to1(3), l1xbar2to1(2), "cbus_l1x2_to_l1x3_buf", star = true)
  val l1x1ToL1x2Buf = connectThroughBuffer(l1xbar2to1(2), l1xbar2to1(1), "cbus_l1x1_to_l1x2_buf")
  val l1x0ToL1x1Buf = connectThroughBuffer(l1xbar2to1(1), l1xbar2to1(0), "cbus_l1x0_to_l1x1_buf", star = true)
  val cxToL1Bufs = (0 until NumCX_l1).map { i =>
    connectThroughBuffer(l1xbar2to1(i), xbar2to1(8 - i * 2), s"cbus_cx${8 - i * 2}_to_l1x${i}_buf", star = true)
  }
  println("Cbus: test 03 ===")
  val l1x3ToCpumBuf = connectThroughBuffer(cpum, l1xbar2to1(NumCX_l1 - 1), "cbus_l1x3_to_cpum_buf")
  lazy val module = new Imp
  class Imp extends LazyModuleImp(this) {
    val cpumResetOut = IO(Output(AsyncReset()))

    // Propagate reset along the same fan-in tree as the AXI path.
    // val cpuRouteResets = cpuLMs.map(lm => ResetUtils.stageResetOut(lm.module, reset))

    // buffer:=cpu(i), for each cpu, this is bundle, and to xbar for timing.
    val cpu0ToCx0RouteReset = cpu0ToCx0Bufs(0).sinkReset(reset)
    cpu0ToCx0Bufs.drop(1).foreach(_.module.reset := reset)
    val cx0RouteReset = ResetUtils.stageResetOut(xbar2to1LMs(0).module, cpu0ToCx0RouteReset)

    val cpu2ToCx1RouteReset = cpu2ToCx1Buf.sinkReset(reset)
    val cx0ToCx1RouteReset = cx0ToCx1Buf.sinkReset(cx0RouteReset)
    val cx1RouteReset = ResetUtils.stageResetOut(xbar2to1LMs(1).module, ResetUtils.mergeResets(Seq(cpu2ToCx1RouteReset, cx0ToCx1RouteReset)))
    // Retain: local CPU ingress and upstream staged ingress are two real fan-in reset sources.

    val cpu3ToCx2RouteReset = cpu3ToCx2Buf.sinkReset(reset)
    val cx1ToCx2RouteReset = cx1ToCx2Buf.sinkReset(cx1RouteReset)
    val cx2RouteReset = ResetUtils.stageResetOut(xbar2to1LMs(2).module, ResetUtils.mergeResets(Seq(cpu3ToCx2RouteReset, cx1ToCx2RouteReset)))
    // Retain: local CPU ingress and upstream staged ingress are two real fan-in reset sources.

    val cpu4ToCx3RouteReset = cpu4ToCx3Bufs.head.sinkReset(reset)
    cpu4ToCx3Bufs.tail.foreach(_.module.reset := reset)
    val cx3RouteReset = ResetUtils.stageResetOut(xbar2to1LMs(3).module, cpu4ToCx3RouteReset)

    val cpu7ToCx5RouteReset = cpu7ToCx5Bufs.head.sinkReset(reset)
    cpu7ToCx5Bufs.tail.foreach(_.module.reset := reset)
    val cx5RouteReset = ResetUtils.stageResetOut(xbar2to1LMs(5).module, cpu7ToCx5RouteReset)

    val cpu10ToCx7RouteReset = cpu10ToCx7Bufs.head.sinkReset(reset)
    cpu10ToCx7Bufs.tail.foreach(_.module.reset := reset)
    val cx7RouteReset = ResetUtils.stageResetOut(xbar2to1LMs(7).module, cpu10ToCx7RouteReset)

    val cpu13ToCx9RouteReset = cpu13ToCx9Bufs.head.sinkReset(reset)
    cpu13ToCx9Bufs.tail.foreach(_.module.reset := reset)
    val cx9RouteReset = ResetUtils.stageResetOut(xbar2to1LMs(9).module, cpu13ToCx9RouteReset)

    val cx3ToCx4RouteReset = cx3ToCx4Buf.sinkReset(cx3RouteReset)
    val cpu6ToCx4RouteReset = cpu6ToCx4Buf.sinkReset(reset)
    val cx4RouteReset = ResetUtils.stageResetOut(xbar2to1LMs(4).module, ResetUtils.mergeResets(Seq(cx3ToCx4RouteReset, cpu6ToCx4RouteReset)))
    // Retain: local CPU ingress and upstream staged ingress are two real fan-in reset sources.

    val cx5ToCx6RouteReset = cx5ToCx6Buf.sinkReset(cx5RouteReset)
    val cpu9ToCx6RouteReset = cpu9ToCx6Buf.sinkReset(reset)
    val cx6RouteReset = ResetUtils.stageResetOut(xbar2to1LMs(6).module, ResetUtils.mergeResets(Seq(cx5ToCx6RouteReset, cpu9ToCx6RouteReset)))
    // Retain: local CPU ingress and upstream staged ingress are two real fan-in reset sources.

    val cx7ToCx8RouteReset = cx7ToCx8Buf.sinkReset(cx7RouteReset)
    val cpu12ToCx8RouteReset = cpu12ToCx8Buf.sinkReset(reset)
    val cx8RouteReset = ResetUtils.stageResetOut(xbar2to1LMs(8).module, ResetUtils.mergeResets(Seq(cx7ToCx8RouteReset, cpu12ToCx8RouteReset)))
    // Retain: local CPU ingress and upstream staged ingress are two real fan-in reset sources.

    val cx9ToCx10RouteReset = cx9ToCx10Buf.sinkReset(cx9RouteReset)
    val cpu15ToCx10RouteReset = cpu15ToCx10Buf.sinkReset(reset)
    val cx10RouteReset = ResetUtils.stageResetOut(xbar2to1LMs(10).module, ResetUtils.mergeResets(Seq(cx9ToCx10RouteReset, cpu15ToCx10RouteReset)))
    // Retain: local CPU ingress and upstream staged ingress are two real fan-in reset sources.

    val cx10ToL1x0RouteReset = cx10ToL1x0Buf.sinkReset(cx10RouteReset)
    val cx8ToL1x0RouteReset = cxToL1Bufs(0).sinkReset(cx8RouteReset)
    val l1x0RouteReset = ResetUtils.stageResetOut(l1xbar2to1LMs(0).module, ResetUtils.mergeResets(Seq(cx10ToL1x0RouteReset, cx8ToL1x0RouteReset)))
    // Retain: this is a subtree fan-in barrier, so both child branches must release before L1x0.

    val l1x0ToL1x1RouteReset = l1x0ToL1x1Buf.sinkReset(l1x0RouteReset)
    val cx6ToL1x1RouteReset = cxToL1Bufs(1).sinkReset(cx6RouteReset)
    val l1x1RouteReset = ResetUtils.stageResetOut(l1xbar2to1LMs(1).module, ResetUtils.mergeResets(Seq(l1x0ToL1x1RouteReset, cx6ToL1x1RouteReset)))
    // Retain: this is a subtree fan-in barrier, so both child branches must release before L1x1.

    val l1x1ToL1x2RouteReset = l1x1ToL1x2Buf.sinkReset(l1x1RouteReset)
    val cx4ToL1x2RouteReset = cxToL1Bufs(2).sinkReset(cx4RouteReset)
    val l1x2RouteReset = ResetUtils.stageResetOut(l1xbar2to1LMs(2).module, ResetUtils.mergeResets(Seq(l1x1ToL1x2RouteReset, cx4ToL1x2RouteReset)))
    // Retain: this is a subtree fan-in barrier, so both child branches must release before L1x2.

    val l1x2ToL1x3RouteReset = l1x2ToL1x3Buf.sinkReset(l1x2RouteReset)
    val cx2ToL1x3RouteReset = cxToL1Bufs(3).sinkReset(cx2RouteReset)
    val l1x3RouteReset = ResetUtils.stageResetOut(l1xbar2to1LMs(3).module, ResetUtils.mergeResets(Seq(l1x2ToL1x3RouteReset, cx2ToL1x3RouteReset)))
    // Retain: this is a subtree fan-in barrier, so both child branches must release before L1x3.

    val l1x3ToCpumRouteReset = l1x3ToCpumBuf.sinkReset(l1x3RouteReset)
    cpumResetOut := ResetUtils.stageResetOut(cpum_LM.module, l1x3ToCpumRouteReset)
  }
}

class UncoreDebugModuleIO(val localHartCount: Int)(implicit val p: Parameters) extends Bundle {
  val resetCtrl = new ResetCtrlIO(localHartCount)(p)
  val debugIO = new DebugIO()(p)
  val clock = Input(Clock())
  val reset = Input(Reset())
}

class dm_axi2w(
  bundleParams: AXI4BundleParameters,
  localHartCount: Int,
  totalHartCount: Int,
  numDies: Int,
  dieIdWidth: Int
) extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AXI4Bundle(bundleParams))
    val dmint = Output(UInt(localHartCount.W))
    val hartResetReq = Output(UInt(localHartCount.W))
    val hartIsInReset = Output(UInt(totalHartCount.W))
  })
  private val dieIdFieldWidth = 4
  private val writePayloadWidth = 2 * localHartCount + totalHartCount + dieIdFieldWidth
  private val readPayloadWidth = 2 * localHartCount + totalHartCount
  require(localHartCount > 0, s"dm_axi2w expects localHartCount > 0, got $localHartCount")
  require(totalHartCount > 0, s"dm_axi2w expects totalHartCount > 0, got $totalHartCount")
  require(numDies > 0, s"dm_axi2w expects numDies > 0, got $numDies")
  require(totalHartCount == localHartCount * numDies,
    s"dm_axi2w expects totalHartCount == localHartCount * numDies, got $totalHartCount, $localHartCount and $numDies")
  require(dieIdWidth > 0, s"dm_axi2w expects dieIdWidth > 0, got $dieIdWidth")
  require(io.axi.w.bits.data.getWidth >= writePayloadWidth,
    s"dm_axi2w expects write payload width <= ${io.axi.w.bits.data.getWidth}, got $writePayloadWidth")
  require(io.axi.r.bits.data.getWidth >= readPayloadWidth,
    s"dm_axi2w expects read payload width <= ${io.axi.r.bits.data.getWidth}, got $readPayloadWidth")

  val dmintShadow = RegInit(0.U(localHartCount.W))
  val hartResetReqShadow = RegInit(0.U(localHartCount.W))
  val hartIsInResetShadow = RegInit(0.U(totalHartCount.W))

  val awQueue = Module(new Queue(chiselTypeOf(io.axi.aw.bits), 1, pipe = true, flow = true))
  val wQueue = Module(new Queue(chiselTypeOf(io.axi.w.bits), 1, pipe = true, flow = true))
  awQueue.io.enq <> io.axi.aw
  wQueue.io.enq <> io.axi.w

  val bBits = Wire(chiselTypeOf(io.axi.b.bits))
  bBits := 0.U.asTypeOf(bBits)
  val bValid = RegInit(false.B)
  val bResp = RegInit(AXI4Parameters.RESP_OKAY)
  val bId = RegInit(0.U(io.axi.b.bits.id.getWidth.W))
  bBits.id := bId
  bBits.resp := bResp
  io.axi.b.bits := bBits
  io.axi.b.valid := bValid
  when (io.axi.b.fire) {
    bValid := false.B
  }

  val rBits = Wire(chiselTypeOf(io.axi.r.bits))
  rBits := 0.U.asTypeOf(rBits)
  val rValid = RegInit(false.B)
  val rResp = RegInit(AXI4Parameters.RESP_DECERR)
  val rId = RegInit(0.U(io.axi.r.bits.id.getWidth.W))
  val rData = RegInit(0.U(io.axi.r.bits.data.getWidth.W))
  rBits.id := rId
  rBits.data := rData
  rBits.resp := rResp
  rBits.last := true.B
  io.axi.r.bits := rBits
  io.axi.r.valid := rValid
  io.axi.ar.ready := !rValid
  when (io.axi.ar.fire) {
    val readOffset = io.axi.ar.bits.addr(7, 0)
    val readPayload = Cat(hartIsInResetShadow, hartResetReqShadow, dmintShadow)
    rValid := true.B
    rId := io.axi.ar.bits.id
    when (readOffset === 0.U) {
      rResp := AXI4Parameters.RESP_OKAY
      if (io.axi.r.bits.data.getWidth > readPayloadWidth) {
        rData := Cat(0.U((io.axi.r.bits.data.getWidth - readPayloadWidth).W), readPayload)
      } else {
        rData := readPayload
      }
    }.otherwise {
      rResp := AXI4Parameters.RESP_DECERR
      rData := 0.U
    }
  }
  when (io.axi.r.fire) {
    rValid := false.B
  }

  val consumeWrite = !bValid && awQueue.io.deq.valid && wQueue.io.deq.valid
  awQueue.io.deq.ready := consumeWrite
  wQueue.io.deq.ready := consumeWrite
  when (consumeWrite) {
    val writeOffset = awQueue.io.deq.bits.addr(7, 0)
    val dmintData = wQueue.io.deq.bits.data(localHartCount - 1, 0)
    val hartResetReqData = wQueue.io.deq.bits.data(2 * localHartCount - 1, localHartCount)
    val hartIsInResetData = wQueue.io.deq.bits.data(2 * localHartCount + totalHartCount - 1, 2 * localHartCount)
    bValid := true.B
    bId := awQueue.io.deq.bits.id
    bResp := AXI4Parameters.RESP_OKAY
    when (writeOffset === 0.U) {
      dmintShadow := dmintData
      hartResetReqShadow := hartResetReqData
      hartIsInResetShadow := hartIsInResetData
    }.otherwise {
      bResp := AXI4Parameters.RESP_DECERR
    }
  }

  io.dmint := dmintShadow
  io.hartResetReq := hartResetReqShadow
  io.hartIsInReset := hartIsInResetShadow
}

class dm_w2axi(
  bundleParams: AXI4BundleParameters,
  localHartCount: Int,
  totalHartCount: Int,
  numDies: Int,
  dieIdWidth: Int,
  nocDataWidth: Int,
  baseAddr: Long
) extends Module {
  val io = IO(new Bundle {
    val axi = new AXI4Bundle(bundleParams)
    val dmint = Input(UInt(totalHartCount.W))
    val hartResetReq = Input(UInt(totalHartCount.W))
    val hartIsInReset = Input(UInt(localHartCount.W))
    val reqId = Input(UInt(dieIdWidth.W))
    val selfId = Input(UInt(dieIdWidth.W))
  })
  private val dieIdFieldWidth = 4
  private val packedPayloadWidth = 2 * localHartCount + totalHartCount
  private val txPayloadWidth = nocDataWidth
  require(localHartCount > 0, s"dm_w2axi expects localHartCount > 0, got $localHartCount")
  require(totalHartCount == localHartCount * numDies,
    s"dm_w2axi expects totalHartCount == localHartCount * numDies, got $totalHartCount, $localHartCount and $numDies")
  require(numDies > 0, s"dm_w2axi expects numDies > 0, got $numDies")
  require(dieIdWidth > 0, s"dm_w2axi expects dieIdWidth > 0, got $dieIdWidth")
  require(numDies <= ((1 << dieIdWidth) - 1),
    s"dm_w2axi expects dieIdWidth to encode die ids 1..$numDies, got dieIdWidth=$dieIdWidth")
  require(nocDataWidth >= packedPayloadWidth + dieIdFieldWidth,
    s"dm_w2axi expects nocDataWidth >= ${packedPayloadWidth + dieIdFieldWidth}, got $nocDataWidth")

  val dmIntAddr = baseAddr.U(io.axi.aw.bits.addr.getWidth.W)
  val dmintByDie = Wire(Vec(numDies, UInt(localHartCount.W)))
  val hartResetReqByDie = Wire(Vec(numDies, UInt(localHartCount.W)))
  for (dieIdx <- 0 until numDies) {
    dmintByDie(dieIdx) := io.dmint((dieIdx + 1) * localHartCount - 1, dieIdx * localHartCount)
    hartResetReqByDie(dieIdx) := io.hartResetReq((dieIdx + 1) * localHartCount - 1, dieIdx * localHartCount)
  }

  val dmintSent = RegInit(VecInit(Seq.fill(numDies)(0.U(localHartCount.W))))
  val hartResetReqSent = RegInit(VecInit(Seq.fill(numDies)(0.U(localHartCount.W))))
  val hartIsInResetSent = RegInit(VecInit(Seq.fill(numDies)(0.U(localHartCount.W))))

  val dmintDirtyByDie = VecInit.tabulate(numDies) { dieIdx =>
    dmintByDie(dieIdx) =/= dmintSent(dieIdx)
  }
  val hartResetReqDirtyByDie = VecInit.tabulate(numDies) { dieIdx =>
    hartResetReqByDie(dieIdx) =/= hartResetReqSent(dieIdx)
  }
  val hartIsInResetDirtyByDie = VecInit.tabulate(numDies) { dieIdx =>
    io.reqId === (dieIdx + 1).U && io.hartIsInReset =/= hartIsInResetSent(dieIdx)
  }

  val txReqValid = dmintDirtyByDie.asUInt.orR || hartResetReqDirtyByDie.asUInt.orR || hartIsInResetDirtyByDie.asUInt.orR
  val txSendDmint = dmintDirtyByDie.asUInt.orR
  val txSendHartResetReq = !txSendDmint && hartResetReqDirtyByDie.asUInt.orR
  val txSendHartIsInReset = !txSendDmint && !txSendHartResetReq && hartIsInResetDirtyByDie.asUInt.orR
  val txGroupMask = Mux1H(Seq(
    txSendDmint -> dmintDirtyByDie.asUInt,
    txSendHartResetReq -> hartResetReqDirtyByDie.asUInt,
    txSendHartIsInReset -> hartIsInResetDirtyByDie.asUInt,
    !txReqValid -> 0.U(numDies.W)
  ))
  val txGroupIdx = PriorityEncoder(txGroupMask)
  val txDieId = Wire(UInt(dieIdFieldWidth.W))
  txDieId := (txGroupIdx.pad(dieIdFieldWidth) +& 1.U(dieIdFieldWidth.W))(dieIdFieldWidth - 1, 0)
  dontTouch(txDieId)
  val txAddrDieId = txDieId(dieIdWidth - 1, 0)
  val txDmintNow = dmintByDie(txGroupIdx)
  val txHartResetReqNow = hartResetReqByDie(txGroupIdx)
  val txHartIsInResetNow = io.hartIsInReset
  val txAddrNow = (txAddrDieId << 44) | dmIntAddr
  val txDieIdField = txDieId
  val txHartIsInResetExpandedNow = WireDefault(0.U(totalHartCount.W))
  when (io.selfId >= 1.U && io.selfId <= numDies.U) {
    val hartIsInResetShift = (io.selfId - 1.U) * localHartCount.U
    txHartIsInResetExpandedNow := ((txHartIsInResetNow.pad(totalHartCount) << hartIsInResetShift)(totalHartCount - 1, 0))
  }

  val txBusy = RegInit(false.B)
  val txAwDone = RegInit(false.B)
  val txWDone = RegInit(false.B)
  val txAddrReg = Reg(UInt(io.axi.aw.bits.addr.getWidth.W))
  val txDmintReg = Reg(UInt(localHartCount.W))
  val txHartResetReqReg = Reg(UInt(localHartCount.W))
  val txHartIsInResetReg = Reg(UInt(localHartCount.W))
  val txGroupIdxReg = Reg(UInt(log2Ceil(numDies).W))
  val awBitsReg = RegInit(0.U.asTypeOf(chiselTypeOf(io.axi.aw.bits)))
  val awValidReg = RegInit(false.B)
  val wBitsReg = RegInit(0.U.asTypeOf(chiselTypeOf(io.axi.w.bits)))
  val wValidReg = RegInit(false.B)
  val bReadyReg = RegInit(false.B)

  val txPayloadPaddingWidth = nocDataWidth - packedPayloadWidth - dieIdFieldWidth
  val txPayload = Cat(
    txDieIdField,
    0.U(txPayloadPaddingWidth.W),
    txHartIsInResetExpandedNow,
    txHartResetReqNow,
    txDmintNow
  )
  val awBitsBundle = WireInit(0.U.asTypeOf(chiselTypeOf(io.axi.aw.bits)))
  awBitsBundle.id := 0.U
  awBitsBundle.addr := txAddrNow
  awBitsBundle.len := 0.U
  awBitsBundle.size := log2Ceil(nocDataWidth / 8).U
  awBitsBundle.burst := AXI4Parameters.BURST_INCR
  awBitsBundle.lock := 0.U
  awBitsBundle.cache := 0.U
  awBitsBundle.prot := 0.U
  awBitsBundle.qos := 0.U
  val wBitsBundle = WireInit(0.U.asTypeOf(chiselTypeOf(io.axi.w.bits)))
  wBitsBundle.data := txPayload
  wBitsBundle.strb := Fill(nocDataWidth / 8, 1.U(1.W))
  wBitsBundle.last := true.B

  when (!txBusy && txReqValid) {
    txBusy := true.B
    txAwDone := false.B
    txWDone := false.B
    txAddrReg := txAddrNow
    txDmintReg := txDmintNow
    txHartResetReqReg := txHartResetReqNow
    txHartIsInResetReg := txHartIsInResetNow
    txGroupIdxReg := txGroupIdx
    awBitsReg := awBitsBundle
    awValidReg := true.B
    wBitsReg := wBitsBundle
    wValidReg := true.B
    bReadyReg := false.B
  }

  val awFire = io.axi.aw.fire
  val wFire = io.axi.w.fire
  val txReqComplete = (awFire && (txWDone || wFire)) || (wFire && (txAwDone || awFire))

  io.axi.aw.bits := awBitsReg
  io.axi.aw.valid := awValidReg
  when (awFire) {
    txAwDone := true.B
    awValidReg := false.B
  }

  io.axi.w.bits := wBitsReg
  io.axi.w.valid := wValidReg
  when (wFire) {
    txWDone := true.B
    wValidReg := false.B
  }

  io.axi.ar.valid := false.B
  io.axi.ar.bits := 0.U.asTypeOf(io.axi.ar.bits)
  io.axi.r.ready := true.B
  io.axi.b.ready := bReadyReg
  when (txReqComplete) {
    bReadyReg := true.B
  }
  when (io.axi.b.fire) {
    txBusy := false.B
    bReadyReg := false.B
    dmintSent(txGroupIdxReg) := txDmintReg
    hartResetReqSent(txGroupIdxReg) := txHartResetReqReg
    hartIsInResetSent(txGroupIdxReg) := txHartIsInResetReg
  }
}

class imsicPbusTop(params: Pbus2Params)(implicit p: Parameters) extends LazyModule {
  private def newXbar(name: String): AXI4Xbar = {
    val xbar = LazyModule(new AXI4Xbar())
    xbar.suggestName(name)
    xbar
  }

  private def newBuffer(name: String): AXI4Buffer = {
    val buffer = LazyModule(new AXI4Buffer())
    buffer.suggestName(name)
    buffer
  }

  private def connectThroughBuffer(sink: AXI4Node, source: AXI4Node, name: String, star: Boolean = false): BufferedAXI4Connection = {
    val buffer = newBuffer(name)
    if (star) sink :=* buffer.node else sink := buffer.node
    buffer.node := source
    BufferedAXI4Connection(buffer)
  }

  val Cbus = LazyModule(new Cbus(params))
  val hni_s_xbarLM = newXbar("hni_s_xbar")
  val hni_s_xbar = hni_s_xbarLM.node
  val pcie_xbar1to2LM = newXbar("imsic_pcie_xbar1to2")
  val pcie_xbar1to2 = pcie_xbar1to2LM.node
  val pbus_xbarLM = newXbar("imsic_pbus_xbar")
  val pbus_xbar = pbus_xbarLM.node
  val aplic_mNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "master-node",
      maxFlight = Some(16),
      id = IdRange(0, 1)
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
      beatBytes = params.MSIOutDataWidth/8)))
  })
  println("IMSICXbar: end sNodes define")
  // cross-die MSI slave Node,bit[47:44]!=0，data width is 256bit
  val crsdie_msi_sN =
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = params.crsimsicAddrMap,
        supportsWrite = TransferSizes(1, params.nocDataWidth / 8),
        supportsRead = TransferSizes(1, params.nocDataWidth / 8)
      )),
      beatBytes = params.nocDataWidth / 8
    )))

  // instance data width switch bridge for s_noc2msi (256bit -> 64bit)
  val u_hnis_DataBridge = LazyModule(new AXIDataBridge(SrcDataWidth = params.nocDataWidth,
    DestDataWidth = params.MSIOutDataWidth,
    errorAddrMap = AXIDataBridge.errorAddrMapFromLegal(params.localImsicAddrMap ++ params.crsimsicAddrMap)))
  u_hnis_DataBridge.axi_xbar_i := hni_s_xbar
  pcie_xbar1to2 := aplic_mNode
  val hniBridgeToPcieBuf = connectThroughBuffer(pcie_xbar1to2, u_hnis_DataBridge.axi_xbar_o, "imsic_hni_bridge_to_pcie_buf")
  // instance data width switch bridge
  val u_cpus_DataBridge = LazyModule(new AXIDataBridge(SrcDataWidth = params.cpuDataWidth,
    DestDataWidth = params.MSIOutDataWidth,
    errorAddrMap = AXIDataBridge.errorAddrMapFromLegal(params.localImsicAddrMap ++ params.crsimsicAddrMap :+ params.DebugAddrMap)))
  u_cpus_DataBridge.axi_xbar_i := Cbus.cpum
  pbus_xbar := u_cpus_DataBridge.axi_xbar_o
  val pcieToPbusBuf = connectThroughBuffer(pbus_xbar, pcie_xbar1to2, "imsic_pcie_to_pbus_buf")
  // start to decoder for imsic below
  // imsic for cross-die
  // instance data width switch bridge from 32bit to 256bit
  val u_crsdie_DataBridge = LazyModule(new AXIDataBridge(SrcDataWidth = params.MSIOutDataWidth,
    DestDataWidth = params.nocDataWidth,
    errorAddrMap = Seq(AddressSet(0x800000000000L, 0xfffffffffffL))))
  u_crsdie_DataBridge.axi_xbar_i := pbus_xbar // 32bit
  crsdie_msi_sN := u_crsdie_DataBridge.axi_xbar_o // 256bit
  // imsic inside die
  val imsic_l4LMs = Seq.tabulate(params.NumHarts) { i =>
    newXbar(s"imsic_l4_$i")
  }
  val imsic_l4 = imsic_l4LMs.map(_.node)
  val NumCX = 11
  val NumCX_l1 = 4
  // inveter bus from xbar to harts
  val xbar1to2LMs = (0 until NumCX).map { i =>
    newXbar(s"cx1to2_$i")
  }
  val xbar1to2 = xbar1to2LMs.map(_.node)
  val l1xbar1to2LMs = (0 until NumCX_l1).map { i =>
    newXbar(s"L1cx1to2_$i")
  }
  val l1xbar1to2 = l1xbar1to2LMs.map(_.node)

  // l0 <- l1 <- l2 <- l3
  l1xbar1to2(3) := pbus_xbar
  val l1x3ToL1x2Buf = connectThroughBuffer(l1xbar1to2(2), l1xbar1to2(3), "imsic_l1x3_to_l1x2_buf")
  val l1x2ToL1x1Buf = connectThroughBuffer(l1xbar1to2(1), l1xbar1to2(2), "imsic_l1x2_to_l1x1_buf")
  val l1x1ToL1x0Buf = connectThroughBuffer(l1xbar1to2(0), l1xbar1to2(1), "imsic_l1x1_to_l1x0_buf")
  // icx4 <- icxl1_0, icx6 <- icxl1_1, icx8 <- icxl1_2, icx10 <- icxl1_3
  for (i <- 0 until NumCX_l1) {
    xbar1to2(8 - i * 2) := l1xbar1to2(i)
  }
  xbar1to2(10) := l1xbar1to2(0)
  // icx xbar 1to2 design
  for (i <- 0 until 2) {
    imsic_l4(i) :*= xbar1to2(0)
  }
  val xbar1ToImsic2Buf = connectThroughBuffer(imsic_l4(2), xbar1to2(1), "imsic_xbar1_to_l4_2_buf")
  val xbar1ToXbar0Buf = connectThroughBuffer(xbar1to2(0), xbar1to2(1), "imsic_xbar1_to_xbar0_buf")
  val xbar2ToImsic3Buf = connectThroughBuffer(imsic_l4(3), xbar1to2(2), "imsic_xbar2_to_l4_3_buf")
  val xbar2ToXbar1Buf = connectThroughBuffer(xbar1to2(1), xbar1to2(2), "imsic_xbar2_to_xbar1_buf")
  val xbar3ToImsicBufs = (4 until 6).map { i =>
    connectThroughBuffer(imsic_l4(i), xbar1to2(3), s"imsic_xbar3_to_l4_${i}_buf", star = true)
  }
  val xbar5ToImsicBufs = (4 until 6).map { i =>
    val imsicIdx = i + 3
    connectThroughBuffer(imsic_l4(imsicIdx), xbar1to2(5), s"imsic_xbar5_to_l4_${imsicIdx}_buf", star = true)
  }
  val xbar7ToImsicBufs = (4 until 6).map { i =>
    val imsicIdx = i + 3 * 2
    connectThroughBuffer(imsic_l4(imsicIdx), xbar1to2(7), s"imsic_xbar7_to_l4_${imsicIdx}_buf", star = true)
  }
  val xbar9ToImsicBufs = (4 until 6).map { i =>
    val imsicIdx = i + 3 * 3
    connectThroughBuffer(imsic_l4(imsicIdx), xbar1to2(9), s"imsic_xbar9_to_l4_${imsicIdx}_buf", star = true)
  }
  //  icx0->cx1->cx2   cx3->cx4,cx5->cx6,cx7->cx8, cx9->cx10
  val xbar4ToXbar3Buf = connectThroughBuffer(xbar1to2(3), xbar1to2(4), "imsic_xbar4_to_xbar3_buf")
  val xbar4ToImsic6Buf = connectThroughBuffer(imsic_l4(6), xbar1to2(4), "imsic_xbar4_to_l4_6_buf")
  val xbar6ToXbar5Buf = connectThroughBuffer(xbar1to2(5), xbar1to2(6), "imsic_xbar6_to_xbar5_buf")
  val xbar6ToImsic9Buf = connectThroughBuffer(imsic_l4(9), xbar1to2(6), "imsic_xbar6_to_l4_9_buf")
  val xbar8ToXbar7Buf = connectThroughBuffer(xbar1to2(7), xbar1to2(8), "imsic_xbar8_to_xbar7_buf")
  val xbar8ToImsic12Buf = connectThroughBuffer(imsic_l4(12), xbar1to2(8), "imsic_xbar8_to_l4_12_buf")
  val xbar10ToXbar9Buf = connectThroughBuffer(xbar1to2(9), xbar1to2(10), "imsic_xbar10_to_xbar9_buf")
  val xbar10ToImsic15Buf = connectThroughBuffer(imsic_l4(15), xbar1to2(10), "imsic_xbar10_to_l4_15_buf")
  val imsicL4ToSNodeBufs = (0 until params.NumHarts).map { i =>
    val buffer = newBuffer(s"imsic_l4_${i}_to_snode_buf")
    sNodes(i) := buffer.node
    buffer.node := imsic_l4(i)
    buffer
  }

  // --- Module Implementation ---
  lazy val module = new Imp
  class Imp extends LazyModuleImp(this) {
    // slave access from hni
    val s_aplic = IO(Flipped(new AXI4Bundle(aplic_mNode.out.head._2.bundle))) // mNode.head.out for vector TDO
    // master to imsic
    val m = IO(Vec(
      params.NumHarts,new VerilogAXI4Record(sNodes.head.in.head._2.bundle)))
    // val m_reset = IO(Vec(params.NumHarts, Output(AsyncReset())))
    // cross-die imsic
    val m_msi2noc = IO(new VerilogAXI4Record(crsdie_msi_sN.in.head._2.bundle))

    Seq(
      hni_s_xbarLM.module,
      u_hnis_DataBridge.module,
      hniBridgeToPcieBuf.module,
      pcie_xbar1to2LM.module,
      pcieToPbusBuf.module
    ).foreach { mod =>
      mod.reset := reset
    }
    val cpuBridgeRouteReset = ResetUtils.stageResetOut(u_cpus_DataBridge.module, Cbus.module.cpumResetOut)
    val pbusRouteReset = ResetUtils.stageResetOut(pbus_xbarLM.module, cpuBridgeRouteReset)
    u_crsdie_DataBridge.module.reset := pbusRouteReset

    val l1x3RouteReset = ResetUtils.stageResetOut(l1xbar1to2LMs(3).module, pbusRouteReset)
    val l1x3ToL1x2RouteReset = l1x3ToL1x2Buf.sinkReset(l1x3RouteReset)
    val l1x2RouteReset = ResetUtils.stageResetOut(l1xbar1to2LMs(2).module, l1x3ToL1x2RouteReset)
    val l1x2ToL1x1RouteReset = l1x2ToL1x1Buf.sinkReset(l1x2RouteReset)
    val l1x1RouteReset = ResetUtils.stageResetOut(l1xbar1to2LMs(1).module, l1x2ToL1x1RouteReset)
    val l1x1ToL1x0RouteReset = l1x1ToL1x0Buf.sinkReset(l1x1RouteReset)
    val l1x0RouteReset = ResetUtils.stageResetOut(l1xbar1to2LMs(0).module, l1x1ToL1x0RouteReset)

    val xbar10RouteReset = ResetUtils.stageResetOut(xbar1to2LMs(10).module, l1x0RouteReset)
    val xbar8RouteReset = ResetUtils.stageResetOut(xbar1to2LMs(8).module, l1x0RouteReset)
    val xbar6RouteReset = ResetUtils.stageResetOut(xbar1to2LMs(6).module, l1x1RouteReset)
    val xbar4RouteReset = ResetUtils.stageResetOut(xbar1to2LMs(4).module, l1x2RouteReset)
    val xbar2RouteReset = ResetUtils.stageResetOut(xbar1to2LMs(2).module, l1x3RouteReset)

    val xbar10ToXbar9RouteReset = xbar10ToXbar9Buf.sinkReset(xbar10RouteReset)
    val xbar9RouteReset = ResetUtils.stageResetOut(xbar1to2LMs(9).module, xbar10ToXbar9RouteReset)
    val xbar10ToImsic15RouteReset = xbar10ToImsic15Buf.sinkReset(xbar10RouteReset)
    val imsic15RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(15).module, xbar10ToImsic15RouteReset)
    val xbar9ToImsicRouteResets = xbar9ToImsicBufs.map(buf => buf.sinkReset(xbar9RouteReset))
    val imsic13RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(13).module, xbar9ToImsicRouteResets(0))
    val imsic14RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(14).module, xbar9ToImsicRouteResets(1))

    val xbar8ToXbar7RouteReset = xbar8ToXbar7Buf.sinkReset(xbar8RouteReset)
    val xbar7RouteReset = ResetUtils.stageResetOut(xbar1to2LMs(7).module, xbar8ToXbar7RouteReset)
    val xbar8ToImsic12RouteReset = xbar8ToImsic12Buf.sinkReset(xbar8RouteReset)
    val imsic12RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(12).module, xbar8ToImsic12RouteReset)
    val xbar7ToImsicRouteResets = xbar7ToImsicBufs.map(buf => buf.sinkReset(xbar7RouteReset))
    val imsic10RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(10).module, xbar7ToImsicRouteResets(0))
    val imsic11RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(11).module, xbar7ToImsicRouteResets(1))

    val xbar6ToXbar5RouteReset = xbar6ToXbar5Buf.sinkReset(xbar6RouteReset)
    val xbar5RouteReset = ResetUtils.stageResetOut(xbar1to2LMs(5).module, xbar6ToXbar5RouteReset)
    val xbar6ToImsic9RouteReset = xbar6ToImsic9Buf.sinkReset(xbar6RouteReset)
    val imsic9RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(9).module, xbar6ToImsic9RouteReset)
    val xbar5ToImsicRouteResets = xbar5ToImsicBufs.map(buf => buf.sinkReset(xbar5RouteReset))
    val imsic7RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(7).module, xbar5ToImsicRouteResets(0))
    val imsic8RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(8).module, xbar5ToImsicRouteResets(1))

    val xbar4ToXbar3RouteReset = xbar4ToXbar3Buf.sinkReset(xbar4RouteReset)
    val xbar3RouteReset = ResetUtils.stageResetOut(xbar1to2LMs(3).module, xbar4ToXbar3RouteReset)
    val xbar4ToImsic6RouteReset = xbar4ToImsic6Buf.sinkReset(xbar4RouteReset)
    val imsic6RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(6).module, xbar4ToImsic6RouteReset)
    val xbar3ToImsicRouteResets = xbar3ToImsicBufs.map(buf => buf.sinkReset(xbar3RouteReset))
    val imsic4RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(4).module, xbar3ToImsicRouteResets(0))
    val imsic5RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(5).module, xbar3ToImsicRouteResets(1))

    val xbar2ToImsic3RouteReset = xbar2ToImsic3Buf.sinkReset(xbar2RouteReset)
    val imsic3RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(3).module, xbar2ToImsic3RouteReset)
    val xbar2ToXbar1RouteReset = xbar2ToXbar1Buf.sinkReset(xbar2RouteReset)
    val xbar1RouteReset = ResetUtils.stageResetOut(xbar1to2LMs(1).module, xbar2ToXbar1RouteReset)

    val xbar1ToImsic2RouteReset = xbar1ToImsic2Buf.sinkReset(xbar1RouteReset)
    val imsic2RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(2).module, xbar1ToImsic2RouteReset)
    val xbar1ToXbar0RouteReset = xbar1ToXbar0Buf.sinkReset(xbar1RouteReset)
    val xbar0RouteReset = ResetUtils.stageResetOut(xbar1to2LMs(0).module, xbar1ToXbar0RouteReset)
    val imsic0RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(0).module, xbar0RouteReset)
    val imsic1RouteReset = ResetUtils.stageResetOut(imsic_l4LMs(1).module, xbar0RouteReset)

    val imsicRouteResets = Seq(
      imsic0RouteReset,
      imsic1RouteReset,
      imsic2RouteReset,
      imsic3RouteReset,
      imsic4RouteReset,
      imsic5RouteReset,
      imsic6RouteReset,
      imsic7RouteReset,
      imsic8RouteReset,
      imsic9RouteReset,
      imsic10RouteReset,
      imsic11RouteReset,
      imsic12RouteReset,
      imsic13RouteReset,
      imsic14RouteReset,
      imsic15RouteReset
    )
    val imsicToSNodeRouteResets = imsicL4ToSNodeBufs.zip(imsicRouteResets).map { case (buf, inReset) =>
      ResetUtils.stageResetOut(buf.module, inReset)
    }

    // connect io
    aplic_mNode.out.head._1 <> s_aplic
    m_msi2noc.viewAs[AXI4Bundle] <> crsdie_msi_sN.in.head._1
    // m_reset.zip(imsicToSNodeRouteResets).foreach { case (outReset, routeReset) =>
    //   outReset := routeReset
    // }
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
  private val totalHartCount = params.NumDies * params.NumHarts
  private val dmWcrsBaseAddr = 0x1B100000L
  private val dmWcrsAddrMap = (0 to params.NumDies).map { dieId =>
    AddressSet((BigInt(dieId) << 44) + dmWcrsBaseAddr, (params.nocDataWidth / 8) - 1)
  }
  // debugModule instance
  println("=== enter dmPbusTop class ====")
  val dm = LazyModule(new StandAloneDebugModule(
    useTL = false,
    baseAddress = params.DebugAddrMap.base,
    addrWidth = params.cpuAddrWidth,
    dataWidth = params.dmDataWidth,
    hartNum = params.NumDies * params.NumHarts // support all dies access to debugModule.
  ))
  // dm master: cpus--> cpu_xbarNto1-->Cbus.cpum
  val Cbus = LazyModule(new Cbus(params))
  // define node from noc(access to debug of current die from other die)
  val dm_fcrs_mNode =
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "dm-mnode-crs",
        maxFlight = Some(1),
        aligned = true,
        id = IdRange(0, 1 << params.NOCidBits)
      ))
    )))
  val dm_fcrs_xbar = AXI4Xbar()
  val dm_wcrs_sNode =
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = dmWcrsAddrMap,
        supportsWrite = TransferSizes(1, params.nocDataWidth / 8),
        supportsRead = TransferSizes(1, params.nocDataWidth / 8)
      )),
      beatBytes = params.nocDataWidth / 8
    )))
  // data width switch bridge for cross-die debug, 256bit->64bit
  val u_dm_DataBridge = LazyModule(new AXIDataBridge(SrcDataWidth = params.nocDataWidth, DestDataWidth = params.dmDataWidth,
    errorAddrMap = AXIDataBridge.errorAddrMapFromLegal(Seq(params.DebugAddrMap) ++ dmWcrsAddrMap))) // dm cfg data must be 64bit
  u_dm_DataBridge.axi_xbar_i := dm_fcrs_xbar // 64bit
  dm_wcrs_sNode := dm_fcrs_xbar
  dm_fcrs_xbar := dm_fcrs_mNode
  val dmxbar2to1LM = LazyModule(new AXI4Xbar())
  dmxbar2to1LM.suggestName("dmxbar2to1")
  val dmxbar2to1 = dmxbar2to1LM.node
  // Cbus.cpum,dm_fcrs_mNode --> dmxbar2to1 --->(sefid==reqid) & dm_sNode =>debugModule
  dmxbar2to1 := Cbus.cpum // dm_self_mNode
  dmxbar2to1 := AXI4Buffer() := u_dm_DataBridge.axi_xbar_o
  // define slaveNode, addr space is for debug
  val dm_sNode =
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(params.DebugAddrMap),
        supportsWrite = TransferSizes(1, params.dmDataWidth / 8),
        supportsRead = TransferSizes(1, params.dmDataWidth / 8)
      )),
      beatBytes = params.dmDataWidth / 8
    )))
  dm_sNode := dmxbar2to1
  // current die -> other die
  val dm_tcrs_sNode =
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = params.crsdmAddrMap,
        supportsWrite = TransferSizes(1, params.nocDataWidth / 8),
        supportsRead = TransferSizes(1, params.nocDataWidth / 8)
      )),
      beatBytes = params.nocDataWidth / 8
    )))
  // data width switch bridge for cross-die debug from current die, 64bit->256bit
  val u_dm_mDataBridge = LazyModule(new AXIDataBridge(SrcDataWidth = params.dmDataWidth, DestDataWidth = params.nocDataWidth,
    errorAddrMap = AXIDataBridge.errorAddrMapFromLegal(params.crsdmAddrMap ++ dmWcrsAddrMap))) // dm cfg data must be 64bit
  // master Node for accessing to debugModule of other dies, whose addr[47:44] is reqid,based on sNodes
  val dm_tcrs_mNode =
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "dm-buf-master",
        maxFlight = Some(1), // 必须指定
        aligned = true,
        id = IdRange(0, 1 << params.NOCidBits)  // 需要适当的 ID 范围
      ))
    )))
  u_dm_mDataBridge.axi_xbar_i := dm_tcrs_mNode
  val dm_wcrs_mNode =
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "dm-wcrs-master",
        maxFlight = Some(1),
        aligned = true,
        id = IdRange(0, 1) // id is fix
      ))
    )))
  val dm_tcrs_xbar = AXI4Xbar()
  dm_tcrs_sNode := AXI4Buffer() := dm_tcrs_xbar
  dm_tcrs_xbar := u_dm_mDataBridge.axi_xbar_o
  dm_tcrs_xbar := dm_wcrs_mNode
  // data width switch from 64bit to 256bit for access to cross-die debugModule
  println("=== exit dmPbusTop class last ====")
  class Imp(outer: dmPbusTop) extends LazyModuleImp(outer) {
    println("==== enter uncoreTop Imp ... ==")
    val s_noc2dm = IO(Flipped(new AXI4Bundle(dm_fcrs_mNode.out.head._2.bundle)))// cross-die slave ports for debug
    val m_dm2noc = IO(new VerilogAXI4Record(dm_tcrs_sNode.in.head._2.bundle))
    // instance debugModule sba port
    val m_sba2noc = Option.when(params.dmHasBusMaster)(IO(new VerilogAXI4Record(dm.axi4masternode.get.params)))
    val dmio = IO(new UncoreDebugModuleIO(params.NumHarts))
    val dmint = IO(Output(UInt(params.NumHarts.W)))
    val req_id = IO(Input(UInt(params.DieIDWidth.W))) // die id number for request die
    val self_id = IO(Input(UInt(params.DieIDWidth.W))) // die id number for current die
    val isselfid = req_id === self_id
    val dmWcrsIn = dm_wcrs_sNode.in.head._1
    val dmWcrsOut = dm_wcrs_mNode.out.head._1
    val u_dm_axi2w = Module(new dm_axi2w(
      bundleParams = dm_wcrs_sNode.in.head._2.bundle,
      localHartCount = params.NumHarts,
      totalHartCount = totalHartCount,
      numDies = params.NumDies,
      dieIdWidth = params.DieIDWidth
    ))
    val u_dm_w2axi = Module(new dm_w2axi(
      bundleParams = dm_wcrs_mNode.out.head._2.bundle,
      localHartCount = params.NumHarts,
      totalHartCount = totalHartCount,
      numDies = params.NumDies,
      dieIdWidth = params.DieIDWidth,
      nocDataWidth = params.nocDataWidth,
      baseAddr = dmWcrsBaseAddr
    ))
    dmxbar2to1LM.module.reset := (Cbus.module.cpumResetOut.asBool || reset.asBool).asAsyncReset

    dm_fcrs_mNode.out.head._1 <> s_noc2dm
    dm_tcrs_sNode.in.head._1 <> m_dm2noc.viewAs[AXI4Bundle]
    m_sba2noc.foreach(_ <> dm.axi4masternode.get)
    dm.axi4node.foreach(_.getWrappedValue.viewAs[AXI4Bundle] <> dm_sNode.in.head._1)
    dm.module.io.debugIO <> dmio.debugIO
    dm.module.io.clock := dmio.clock
    dm.module.io.reset := dmio.reset

    val dmintSrc = dm.int.getWrappedValue.asInstanceOf[HeterogeneousBag[Vec[Bool]]]
    require(dmintSrc.length == totalHartCount,
      s"dm.int port count ${dmintSrc.length} does not match total hart count $totalHartCount")
    require(dmintSrc.forall(_.length == 1),
      s"dm.int entries must each contain exactly one interrupt bit")
    val dmintGlobal = Cat(dmintSrc.reverse.map(_.head.asUInt).toSeq)
    val hartResetReqLocal = dm.module.io.resetCtrl.hartResetReq.map(_.asUInt).getOrElse(0.U(totalHartCount.W))
    val hartIsInResetExternal = dmio.resetCtrl.hartIsInReset.asUInt

    u_dm_axi2w.io.axi <> dmWcrsIn
    u_dm_w2axi.io.axi <> dmWcrsOut
    u_dm_w2axi.io.reqId := req_id
    u_dm_w2axi.io.selfId := self_id
    // Debug sideband信号分段OR逻辑
    // For each die, select the 16-bit segment of StandAloneDebugModule output corresponding to self_id, OR with dm_axi2w output, and output as dmint/hartResetReq
    // 分段为 Vec，按 self_id 选段
    // 只将本die段的standaloneDebugModule输出与dm_axi2w输出做OR，且本die段不写入dm_w2axi
    val dmintVec = Wire(Vec(params.NumDies, UInt(params.NumHarts.W)))
    val hartResetReqVec = Wire(Vec(params.NumDies, UInt(params.NumHarts.W)))
    for (i <- 0 until params.NumDies) {
      dmintVec(i) := dmintGlobal((i+1)*params.NumHarts-1, i*params.NumHarts)
      hartResetReqVec(i) := hartResetReqLocal((i+1)*params.NumHarts-1, i*params.NumHarts)
    }
    val selfIdx = (self_id - 1.U)(params.DieIDWidth-1, 0)
    val dmintLocal = dmintVec(selfIdx)
    val hartResetReqLocalSeg = hartResetReqVec(selfIdx)
    // 只将本die段的standaloneDebugModule输出与dm_axi2w输出做OR
    val dmintOr = dmintLocal | u_dm_axi2w.io.dmint
    val hartResetReqOr = hartResetReqLocalSeg | u_dm_axi2w.io.hartResetReq
    dmint := dmintOr
    dmio.resetCtrl.hartResetReq.foreach { req =>
      req := VecInit(hartResetReqOr.asBools)
    }
    // dm_w2axi 只输出跨die部分（本die段清零），用mask避免组合环路
    // 用移位和常量生成mask，彻底消除组合环路
    val base = selfIdx * params.NumHarts.U
    val localMask = ((BigInt(1) << params.NumHarts) - 1).U(totalHartCount.W) << base
    val dmintGlobalNoLocal = dmintGlobal & ~localMask
    val hartResetReqNoLocal = hartResetReqLocal & ~localMask
    val hartIsInResetNoLocal = Mux(isselfid, 0.U, hartIsInResetExternal)
   
    u_dm_w2axi.io.dmint := dmintGlobalNoLocal
    u_dm_w2axi.io.hartResetReq := hartResetReqNoLocal
    u_dm_w2axi.io.hartIsInReset := hartIsInResetNoLocal

    // val dmintLocal = dmintGlobal(base + params.NumHarts.U - 1, base)
    // val hartResetReqLocalSeg = hartResetReqLocal(base + params.NumHarts.U - 1, base)
    val hartIsInResetLocal = Mux(isselfid, hartIsInResetExternal, 0.U)
    // val dmintOr = dmintLocal | u_dm_axi2w.io.dmint
    // val hartResetReqOr = hartResetReqLocalSeg | u_dm_axi2w.io.hartResetReq
    val hartIsInResetOr = hartIsInResetLocal | u_dm_axi2w.io.hartIsInReset
    // dmint := dmintOr
    // dmio.resetCtrl.hartResetReq.foreach { req =>
    //   req := VecInit(hartResetReqOr.asBools)
    // }
    dm.module.io.resetCtrl.hartIsInReset := VecInit(hartIsInResetOr.asBools)
    // self node control
    dm.axi4node.foreach(_.getWrappedValue.viewAs[AXI4Bundle].aw.valid := isselfid & dm_sNode.in.head._1.aw.valid)
    dm.axi4node.foreach(_.getWrappedValue.viewAs[AXI4Bundle].w.valid := isselfid & dm_sNode.in.head._1.w.valid)
    dm.axi4node.foreach(_.getWrappedValue.viewAs[AXI4Bundle].ar.valid := isselfid & dm_sNode.in.head._1.ar.valid)
    dm_tcrs_mNode.out.head._1 <> dm_sNode.in.head._1
    dm_tcrs_mNode.out.head._1.aw.valid := !isselfid & dm_sNode.in.head._1.aw.valid
    dm_tcrs_mNode.out.head._1.aw.bits.addr := (req_id << 44) + dm_sNode.in.head._1.aw.bits.addr //bit[47:44] is req_id
    dm_tcrs_mNode.out.head._1.w.valid := !isselfid & dm_sNode.in.head._1.w.valid
    dm_tcrs_mNode.out.head._1.ar.valid := !isselfid & dm_sNode.in.head._1.ar.valid
    dm_tcrs_mNode.out.head._1.ar.bits.addr := (req_id << 44) + dm_sNode.in.head._1.ar.bits.addr
  }
  override lazy val module = new Imp(this)
}
class uncoreTop(params: Pbus2Params)(implicit p: Parameters) extends LazyModule {
  // cpu axi ports -> xbar1to2 -> imsic slave and dm slave
  println("====start: enter uncoreTop ..==")
  val cpu_mNodes = Seq.fill(params.NumHarts) {
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "cpu-Mnode",
        maxFlight = Some(1),
        aligned = true,
        id = IdRange(0, 1 << params.CPUidBits)
      ))
    )))
  }
  val cpu_xbar1to2 = Seq.fill(params.NumHarts)(AXI4Xbar())
  // instance modules
  val imsicTop = LazyModule(new imsicPbusTop(params))
  val hni_mNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "master-node",
      maxFlight = Some(1),
      aligned = true,
      id = IdRange(0, 1 << params.NOCidBits)
    ))
  )))
  imsicTop.hni_s_xbar := hni_mNode
  val dmTop = LazyModule(new dmPbusTop(params))
  val syscnt = LazyModule(new StandAloneSYSCNT(
    useTL = false,
    baseAddress = params.SYSCNTAddrMap.base,
    addrWidth = params.periParams.addrWidth,
    dataWidth = params.periParams.timedataBytes * 8,
    hartNum = params.NumHarts
  ))
  for (i <- 0 until params.NumHarts) {
    cpu_xbar1to2(i) := AXI4Buffer() := cpu_mNodes(i)
    imsicTop.Cbus.cpus(i) := cpu_xbar1to2(i)
    dmTop.Cbus.cpus(i) := cpu_xbar1to2(i)
  }
  val peri_xbar = AXI4Xbar()
  val peri_mNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "master-node",
      maxFlight = Some(1),
      aligned = true,
      id = IdRange(0, 1 << params.NOCidBits)
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
  // instance data width switch bridge for s_noc2cfg (256bit -> 64bit)
  val u_peri_DataBridge = LazyModule(new AXIDataBridge(SrcDataWidth = params.nocDataWidth, DestDataWidth = params.aplicParams.CFG_DATA_WIDTH,
    errorAddrMap = AXIDataBridge.errorAddrMapFromLegal(Seq(params.aplicParams.APLICAddrMap, params.SYSCNTAddrMap))))
  u_peri_DataBridge.axi_xbar_i := peri_mNode
  peri_xbar  := u_peri_DataBridge.axi_xbar_o
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
    val s_noc2cfg = IO(Flipped(new VerilogAXI4Record(peri_mNode.out.head._2.bundle))) // peri slave ports: aplic and clint
    val s_noc2msi = IO(Flipped(new VerilogAXI4Record(hni_mNode.out.head._2.bundle)))
    val m_uncore2cpu  = IO(imsicTop.module.m.cloneType) // to imsic master ports
    val m_msi2noc  = IO(imsicTop.module.m_msi2noc.cloneType) // to imsic master ports
    val s_cpu2uncore =
      IO(Vec(params.NumHarts, Flipped(new VerilogAXI4Record(cpu_mNodes.head.out.head._2.bundle)))) // cpu access ports
    val s_noc2dm = IO(Flipped(new VerilogAXI4Record(dmTop.dm_fcrs_mNode.out.head._2.bundle))) // cross-die slave ports for debug
    val m_dm2noc = IO(dmTop.module.m_dm2noc.cloneType)
    // instance debugModule sba port
    val m_sba2noc    = Option.when(params.dmHasBusMaster)(IO(dmTop.module.m_sba2noc.get.cloneType))
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
    val noc2msi = s_noc2msi.viewAs[AXI4Bundle]
    val noc2msiSelfId = self_id.pad(4)
    def routeNoc2msiAddr(addr: UInt, isLocal: Bool): UInt = {
      Mux(isLocal, Cat(0.U(4.W), addr(43, 0)), addr)
    }
    val noc2msiAwIsLocal = noc2msi.aw.bits.addr(47, 44) === noc2msiSelfId
    val noc2msiAwAddr = routeNoc2msiAddr(noc2msi.aw.bits.addr, noc2msiAwIsLocal)

    hni_mNode.out.head._1 <> noc2msi
    hni_mNode.out.head._1.aw.bits.addr := noc2msiAwAddr
    peri_mNode.out.head._1 <> s_noc2cfg.viewAs[AXI4Bundle] // uncore peri cfg slave io
    cpu_mNodes.zip(s_cpu2uncore).foreach { case (node, io) => node.out.head._1 <> io.viewAs[AXI4Bundle] }
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
    syscnt.axi4node.foreach(_.getWrappedValue.viewAs[AXI4Bundle] <> peri_s1Node.in.head._1)
    // cpu2msi_sNodes -> imsicTop
    m_uncore2cpu <> imsicTop.module.m
    // cross-die msi connect io
    m_msi2noc <> imsicTop.module.m_msi2noc
    s_noc2dm.viewAs[AXI4Bundle] <> dmTop.module.s_noc2dm
    m_dm2noc <> dmTop.module.m_dm2noc
    imsicTop.module.clock := clock
    imsicTop.module.reset := reset
    dmTop.module.clock := clock
    dmTop.module.reset := reset
    dmTop.module.m_sba2noc.foreach(_ <> m_sba2noc.get)
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
    CFG_ID_WIDTH = 11,
    APLICAddrMap = AddressSet(0x1E020000L, 0x7fff),
    MSI_DATA_WIDTH = 32,
    NumIntSrcs = 512
  )
  val periParams = PeriParams(
    slaveDataBytes = 8,
    timedataBytes = 8
  )
  val dmParams = DebugModuleParams(
    baseAddress = 0x1B000000L,
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

// 计算本die段mask
// val totalHartCount = params.NumDies * params.NumHarts
// val selfIdx = (self_id - 1.U)(log2Ceil(params.NumDies)-1, 0)
// val base = selfIdx * params.NumHarts.U
// val localMask = ((BigInt(1) << params.NumHarts) - 1).U(totalHartCount.W) << base

// // 只对跨die部分送入dm_w2axi
// val dmintGlobalNoLocal = dmintGlobal & ~localMask
// val hartResetReqNoLocal = hartResetReqLocal & ~localMask
// val hartIsInResetNoLocal = hartIsInResetExternal & ~localMask

// u_dm_w2axi.io.dmint := dmintGlobalNoLocal
// u_dm_w2axi.io.hartResetReq := hartResetReqNoLocal
// u_dm_w2axi.io.hartIsInReset := hartIsInResetNoLocal



