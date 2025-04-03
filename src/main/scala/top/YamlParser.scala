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

import org.chipsalliance.cde.config.Parameters
import system.SoCParamsKey
import xiangshan.backend.fu.{MemoryRange, PMAConfigEntry}
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.util.AsyncQueueParams
import freechips.rocketchip.diplomacy.AddressSet

case class YamlConfig(
  PmemRanges: Option[List[MemoryRange]],
  PMAConfigs: Option[List[PMAConfigEntry]],
  EnableCHIAsyncBridge: Option[Boolean],
  L2CacheConfig: Option[L2CacheConfig],
  L3CacheConfig: Option[L3CacheConfig],
  DebugModuleBaseAddr: Option[BigInt],
  SeperateDM: Option[Boolean],
  SeperateTLBus: Option[Boolean],
  SeperateTLBusRanges: Option[List[AddressSet]]
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
          EnableCHIAsyncBridge = Option.when(enable)(AsyncQueueParams(depth = 16, sync = 3, safe = false))
        )
      })
    }
    yamlConfig.L2CacheConfig.foreach(l2 => newConfig = newConfig.alter(l2))
    yamlConfig.L3CacheConfig.foreach(l3 => newConfig = newConfig.alter(l3))
    yamlConfig.DebugModuleBaseAddr.foreach { addr =>
      newConfig = newConfig.alter((site, here, up) => {
        case DebugModuleKey => up(DebugModuleKey).map(_.copy(baseAddress = addr))
      })
    }
    yamlConfig.SeperateDM.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(SeperateDM = enable)
      })
    }
    yamlConfig.SeperateTLBus.foreach { enable =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(SeperateTLBus = enable)
      })
    }
    yamlConfig.SeperateTLBusRanges.foreach { ranges =>
      newConfig = newConfig.alter((site, here, up) => {
        case SoCParamsKey => up(SoCParamsKey).copy(SeperateTLBusRanges = ranges)
      })
    }
    newConfig
  }
}
