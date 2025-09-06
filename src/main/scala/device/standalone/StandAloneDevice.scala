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

package device.standalone

import chisel3._
import chisel3.util._
import chisel3.experimental.{annotate, ChiselAnnotation}
import chisel3.experimental.dataview._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import top.Generator
import system.SoCParamsKey
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import scala.annotation.tailrec
import xiangshan.XSTileKey
import utils.VerilogAXI4Record

trait HasMasterInterface { this: StandAloneDevice =>

  def masterAddrWidth: Int

  protected val masternode = TLIdentityNode()
  // tilelink master io
  private val tlmaster = Option.when(useTL)(TLManagerNode(Seq(
    TLSlavePortParameters.v1(
      managers = Seq(
        TLSlaveParameters.v1(
          address = Seq(AddressSet(0, (BigInt(1) << masterAddrWidth) - 1)),
          regionType = RegionType.UNCACHED,
          supportsGet = TransferSizes(1, p(SoCParamsKey).L3BlockSize),
          supportsPutPartial = TransferSizes(1, p(SoCParamsKey).L3BlockSize),
          supportsPutFull = TransferSizes(1, p(SoCParamsKey).L3BlockSize),
          fifoId = Some(0)
        )
      ),
      beatBytes = p(SoCParamsKey).L3OuterBusWidth / 8
    )
  )))
  tlmaster.foreach(_ := masternode)
  val tlmasternode = tlmaster.map(tlmaster => InModuleBody(tlmaster.makeIOs()))

  // axi4 master io
  private val axi4master = Option.when(!useTL)(AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = Seq(AddressSet(0, (BigInt(1) << masterAddrWidth) - 1)),
          regionType = RegionType.UNCACHED,
          supportsRead = TransferSizes(1, p(SoCParamsKey).L3BlockSize),
          supportsWrite = TransferSizes(1, p(SoCParamsKey).L3BlockSize),
          interleavedId = Some(0)
        )
      ),
      beatBytes = p(SoCParamsKey).L3OuterBusWidth / 8
    )
  )))
  axi4master.foreach(
    _ :=
      AXI4Buffer() :=
      AXI4Buffer() :=
      AXI4Buffer() :=
      AXI4IdIndexer(1) :=
      AXI4UserYanker() :=
      AXI4Deinterleaver(p(SoCParamsKey).L3BlockSize) :=
      TLToAXI4() :=
      TLSourceShrinker(64) :=
      TLWidthWidget(p(SoCParamsKey).L3OuterBusWidth / 8) :=
      TLBuffer.chainNode(2) :=
      masternode
  )
  val axi4masternode = axi4master.map(axi4master => InModuleBody {
    val axi4masternode = chisel3.IO(new VerilogAXI4Record(axi4master.in.head._1.params))
    axi4masternode.viewAs[AXI4Bundle] <> axi4master.in.head._1
    axi4masternode
  })
}

abstract class StandAloneDevice (
  val useTL: Boolean = false,
  val baseAddress: BigInt,
  val addrWidth: Int,
  val dataWidth: Int,
  val hartNum: Int
)(implicit p: Parameters) extends LazyModule {

  def addressSet: AddressSet

  private val dummy = LazyModule(new TLError(
    params = DevNullParams(
      address = AddressSet(0, (BigInt(1) << addrWidth) - 1).subtract(addressSet),
      maxAtomic = 8,
      maxTransfer = 64
    ),
    beatBytes = dataWidth / 8
  ))
  protected val xbar = TLXbar()
  dummy.node := xbar

  // tilelink io
  private val tl = Option.when(useTL)(TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1("tl", IdRange(0, 1)))
  ))))
  tl.foreach(xbar := _)
  val tlnode = tl.map(tl => InModuleBody(tl.makeIOs()))

  // axi4 io
  private val axi4 = Option.when(!useTL)(AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters("axi4", IdRange(0, 1)))
  ))))
  axi4.foreach(
    xbar :=
      TLFIFOFixer() :=
      AXI4ToTL() :=
      AXI4UserYanker(Some(1)) :=
      AXI4Fragmenter() :=
      AXI4Buffer() :=
      AXI4Buffer() :=
      AXI4IdIndexer(1) :=
      _
  )
  val axi4node = axi4.map(axi4 => InModuleBody {
    val axi4node = chisel3.IO(Flipped(new VerilogAXI4Record(axi4.out.head._1.params)))
    axi4node.viewAs[AXI4Bundle] <> axi4.out.head._1
    axi4node
  })

  lazy val module: LazyModuleImpLike = new StandAloneDeviceImp(this)

}

class StandAloneDeviceImp(outer: StandAloneDevice)(implicit p: Parameters) extends LazyModuleImp(outer) with RequireAsyncReset {
  p(SoCParamsKey).XSTopPrefix.foreach { prefix =>
    val mod = this.toNamed
    annotate(new ChiselAnnotation {
      def toFirrtl = NestedPrefixModulesAnnotation(mod, prefix, true)
    })
  }
}

class StandAloneDeviceRawImp(outer: StandAloneDevice)(implicit p: Parameters) extends LazyRawModuleImp(outer) {
  p(SoCParamsKey).XSTopPrefix.foreach { prefix =>
    val mod = this.toNamed
    annotate(new ChiselAnnotation {
      def toFirrtl = NestedPrefixModulesAnnotation(mod, prefix, true)
    })
  }
}

object ArgParser {
  def parse(args: Array[String], p: Parameters): (StandAloneDevice, Array[String]) = {
    var firrtlOpts = Array[String]()
    var module: String = ""
    var useTL: Boolean = false
    var baseAddress: BigInt = -1
    var addrWidth: Int = -1
    var dataWidth: Int = 64
    @tailrec
    def nextOption(list: List[String]): Unit = {
      list match {
        case Nil =>
        case "--standalone-device" :: value :: tail =>
          module = value
          nextOption(tail)
        case "--use-tl" :: tail =>
          useTL = true
          nextOption(tail)
        case "--use-axi4" :: tail =>
          useTL = false
          nextOption(tail)
        case "--device-base-addr" :: value :: tail =>
          baseAddress = value match {
            case s"0x$hex" => BigInt(hex, 16)
            case s"0X$hex" => BigInt(hex, 16)
            case _: String => BigInt(value)
          }
          nextOption(tail)
        case "--device-addr-width" :: value :: tail =>
          addrWidth = value.toInt
          nextOption(tail)
        case "--device-data-width" :: value :: tail =>
          dataWidth = value.toInt
          nextOption(tail)
        case option :: tail =>
          // unknown option, maybe a firrtl option, skip
          firrtlOpts :+= option
          nextOption(tail)
      }
    }
    nextOption(args.toList)
    require(baseAddress >= 0, "baseAddress not specified correctly")
    require(addrWidth >= 0, "addrWidth not specified correctly")
    require(dataWidth >= 0, "dataWidth not specified correctly")
    val device: StandAloneDevice = module match {
      case "StandAloneSYSCNT" =>
        DisableMonitors(p => LazyModule(new StandAloneSYSCNT(
          useTL, baseAddress, addrWidth, dataWidth, p(XSTileKey).size
        )(p)))(p)
      case "StandAlonePLIC" =>
        DisableMonitors(p => LazyModule(new StandAlonePLIC(
          useTL, baseAddress, addrWidth, dataWidth, p(XSTileKey).size
        )(p)))(p)
      case "StandAloneDebugModule" =>
        DisableMonitors(p => LazyModule(new StandAloneDebugModule(
          useTL, baseAddress, addrWidth, dataWidth, p(XSTileKey).size
        )(p)))(p.alter((site, here, up) => {
          case DebugModuleKey => up(DebugModuleKey).map(_.copy(baseAddress = baseAddress))
        }))
      case _: String => throw new IllegalArgumentException(s"$module not found")
    }
    (device, firrtlOpts)
  }
}

object Main extends App {
  val (config, secondaryOpts, firtoolOpts) = top.ArgParser.parse(args)
  val (device, firrtlOpts) = ArgParser.parse(secondaryOpts, config)

  Generator.execute(firrtlOpts, device.module, firtoolOpts)
}
