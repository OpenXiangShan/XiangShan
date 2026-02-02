/***************************************************************************************
* Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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

import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._

import aia.IMSICParams
import org.chipsalliance.cde.config.Parameters
import system.{CVMParameters, CVMParamsKey, SoCParamsKey}
import xiangshan.backend.fu.{MemoryRange, PMAConfigEntry}
import xiangshan.{DFTOptionsKey, XSTileKey}
import freechips.rocketchip.devices.debug.{DebugAttachParams, ExportDebug}
import freechips.rocketchip.devices.debug.{DMI, JTAG, CJTAG, APB}
import freechips.rocketchip.devices.debug.{DebugModuleKey, DebugModuleParams}
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.util.AsyncQueueParams
import device.IMSICBusType

case class YamlConfig(
  Config: Option[String],
  PmemRanges: Option[List[MemoryRange]],
  PMAConfigs: Option[List[PMAConfigEntry]],
  EnableCHIAsyncBridge: Option[Boolean],
  L2CacheConfig: Option[L2CacheConfig],
  L3CacheConfig: Option[L3CacheConfig],
  HartIDBits: Option[Int],
  DebugAttachProtocals: Option[List[String]],
  DebugModuleParams: Option[DebugModuleParams],
  WFIResume: Option[Boolean],
  BEURange: Option[AddressSet],
  EnableDCacheCtrl: Option[Boolean],
  DCacheCtrlRange: Option[AddressSet],
  EnableICacheCtrl: Option[Boolean],
  ICacheCtrlRange: Option[AddressSet],
  SeperateDM: Option[Boolean],
  SeperateBus: Option[String],
  SeperateBusRanges: Option[List[AddressSet]],
  UsePrivateClint: Option[Boolean],
  TIMERRange: Option[AddressSet],
  IMSICBusType: Option[String],
  IMSICParams: Option[IMSICParams],
  CHIIssue: Option[String],
  WFIClockGate: Option[Boolean],
  EnablePowerDown: Option[Boolean],
  XSTopPrefix: Option[String],
  EnableDFX: Option[Boolean],
  EnableSramCtl: Option[Boolean],
  EnableCHINS: Option[Boolean],
  CHIAddrWidth: Option[Int],
  CVMParams: Option[CVMParameters],
  EnableBitmapCheck: Option[Boolean],
  EnableBitmapCheckDefault: Option[Boolean],
)

object YamlParser {
  implicit val customParserConfig: Configuration = Configuration.default.withDefaults
  def parseYaml(config: Parameters, yamlFile: String): Parameters = {
    val yaml = scala.io.Source.fromFile(yamlFile).mkString
    val json = io.circe.yaml.parser.parse(yaml) match {
      case Left(value) => throw value
      case Right(value) => value
    }
    val yamlConfig = json.as[YamlConfig] match {
      case Left(value) => throw value
      case Right(value) => value
    }
    var newConfig = config
    yamlConfig.Config.foreach { config =>
      newConfig = ArgParser.getConfigByName(config)
    }
    yamlConfig.PmemRanges.foreach { ranges =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(PmemRanges = ranges)
      })
    }
    yamlConfig.PMAConfigs.foreach { pmaConfigs =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(PMAConfigs = pmaConfigs)
      })
    }
    yamlConfig.EnableCHIAsyncBridge.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(
          EnableCHIAsyncBridge = Option.when(enable)(AsyncQueueParams(depth = 4, sync = 3, safe = true))
        )
      })
    }
    yamlConfig.L2CacheConfig.foreach(l2 => newConfig = newConfig.alter(l2))
    yamlConfig.L3CacheConfig.foreach(l3 => newConfig = newConfig.alter(l3))
    yamlConfig.DebugAttachProtocals.foreach { protocols =>
      newConfig = newConfig.alter((site, here, up) => {
        case ExportDebug => DebugAttachParams(protocols = protocols.map {
          case "DMI" => DMI
          case "JTAG" => JTAG
          case "CJTAG" => CJTAG
          case "APB" => APB
        }.toSet)
      })
    }
    yamlConfig.HartIDBits.foreach { bits =>
      newConfig = newConfig.alter((site, here, up) => {
        case MaxHartIdBits => bits
      })
    }
    yamlConfig.DebugModuleParams.foreach { params =>
      newConfig = newConfig.alter((site, here, up) => {
        case DebugModuleKey => Some(params)
      })
    }
    yamlConfig.WFIResume.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case XSTileKey => up(XSTileKey).map(_.copy(wfiResume = enable))
      })
    }
    yamlConfig.BEURange.foreach { range =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(BEURange = range)
      })
    }
    yamlConfig.EnableDCacheCtrl.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(EnableDCacheCtrl = enable)
      })
    }
    yamlConfig.DCacheCtrlRange.foreach { range =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(DCacheCtrlRange = range)
      })  
    }
    yamlConfig.EnableICacheCtrl.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(EnableICacheCtrl = enable)
      })
    }
    yamlConfig.ICacheCtrlRange.foreach { range =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(ICacheCtrlRange = range)
      })  
    }
    yamlConfig.SeperateDM.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(SeperateDM = enable)
      })
    }
    yamlConfig.SeperateBus.foreach { busType =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(SeperateBus = top.SeperatedBusType.withName(busType))
      })
    }
    yamlConfig.SeperateBusRanges.foreach { ranges =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(SeperateBusRanges = ranges)
      })
    }
    yamlConfig.UsePrivateClint.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(UsePrivateClint = enable)
      })
    }
    yamlConfig.TIMERRange.foreach { ranges =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(TIMERRange = ranges)
      })
    }
    yamlConfig.IMSICBusType.foreach { busType =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(IMSICBusType = device.IMSICBusType.withName(busType))
      })
    }
    yamlConfig.IMSICParams.foreach { params =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(IMSICParams = params)
      })
    }
    yamlConfig.CHIIssue.foreach { issue =>
      newConfig = newConfig.alter((site, here, up) => {
        case coupledL2.tl2chi.CHIIssue => issue
      })
    }
    yamlConfig.WFIClockGate.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(WFIClockGate = enable)
      })
    }
    yamlConfig.EnablePowerDown.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(EnablePowerDown = enable)
      })
    }
    yamlConfig.XSTopPrefix.foreach { prefix =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(XSTopPrefix = Option.when(prefix.nonEmpty)(prefix))
      })
    }
    yamlConfig.EnableDFX.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case DFTOptionsKey => up(DFTOptionsKey).copy(EnableMbist = enable)
      })
    }
    yamlConfig.EnableSramCtl.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case DFTOptionsKey => up(DFTOptionsKey).copy(EnableSramCtl = enable)
      })
    }
    yamlConfig.EnableCHINS.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case coupledL2.tl2chi.NonSecureKey => enable
      })
    }
    yamlConfig.CHIAddrWidth.foreach { width =>
      newConfig = newConfig.alter((site, here, up) => {
        case coupledL2.tl2chi.CHIAddrWidthKey => width
      })
    }
    yamlConfig.CVMParams.foreach { cvmParams =>
      newConfig = newConfig.alter((site, here, up) => {
        case CVMParamsKey => cvmParams
      })
    }
    yamlConfig.EnableBitmapCheck.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case XSTileKey => up(XSTileKey).map(_.copy(HasBitmapCheck = enable))
      })
    }
    yamlConfig.EnableBitmapCheckDefault.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case XSTileKey => up(XSTileKey).map(_.copy(HasBitmapCheckDefault = enable))
      })
    }
    newConfig
  }
}
