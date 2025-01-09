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

import org.chipsalliance.cde.config.{Config, Parameters}

import scala.jdk.CollectionConverters._
import scala.annotation.tailrec
import java.util.{List => JList, Map => JMap}
import system.SoCParamsKey
import xiangshan.backend.fu.PMAConfigEntry
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.util.AsyncQueueParams

object YamlParser {
  private def getSizeInKB(a: String): Int = a match {
    case s"${k}KB" => k.strip().toInt
    case s"${m}MB" => (m.strip().toDouble * 1024).toInt
  }
  def parseYaml(config: Parameters, yamlFile: String): Parameters = {
    val file = new java.io.File(yamlFile)
    val inputStream = new java.io.FileInputStream(file)
    val yaml = new org.yaml.snakeyaml.Yaml()
    val yamlSeq = yaml.load(inputStream).asInstanceOf[JMap[String, Any]].asScala.toSeq
    @tailrec
    def nextConfig(config: Parameters, yamlSeq: Seq[(String, Any)]): Parameters = {
      def toBigInt(a: Any): BigInt = a match {
        case i: Int => BigInt(i)
        case l: Long => BigInt(l)
        case s"PmemRanges($i).lower" => config(SoCParamsKey).PmemRanges(i.toInt)._1
        case s"PmemRanges($i).upper" => config(SoCParamsKey).PmemRanges(i.toInt)._2
      }
      yamlSeq match {
        case Nil => config
        case ("PmemRanges", pmemRanges) :: tail =>
          val param = pmemRanges.asInstanceOf[JList[JMap[String, Any]]].asScala.map { range =>
            val Seq(lower, upper) = Seq("lower", "upper").map(key => toBigInt(range.asScala(key)))
            (lower, upper)
          }.toSeq
          nextConfig(config.alter((site, here, up) => {
            case SoCParamsKey => up(SoCParamsKey).copy(PmemRanges = param)
          }), tail)
        case ("PMAConfigs", pmaConfigs) :: tail =>
          val param = pmaConfigs.asInstanceOf[JList[JMap[String, Any]]].asScala.map { pmaConfig =>
            require(pmaConfig.containsKey("base_addr"), "base_addr is required in every PMAConfigs")
            val conf = pmaConfig.asScala
            val base_addr = toBigInt(conf.withDefaultValue(0)("base_addr"))
            val range = toBigInt(conf.withDefaultValue(0)("range"))
            val a = conf.withDefaultValue(0)("a").asInstanceOf[Int]
            require(a == 0 || a == 1 || a == 3, "a should be 0, 1 or 3")
            val Seq(l, c, atomic, x, w, r) = Seq("l", "c", "atomic", "x", "w", "r").map { key =>
              conf.withDefaultValue(false)(key).asInstanceOf[Boolean]
            }
            PMAConfigEntry(base_addr, range, l, c, atomic, a, x, w, r)
          }.toSeq
          nextConfig(config.alter((site, here, up) => {
            case SoCParamsKey => up(SoCParamsKey).copy(PMAConfigs = param)
          }), tail)
        case ("CHIAsyncBridge", chiAsyncBridge) :: tail =>
          val param = chiAsyncBridge.asInstanceOf[JMap[String, Any]].asScala
          require(param.contains("depth"), "depth is required in CHIAsyncBridge")
          val depth = param("depth").asInstanceOf[Int]
          val enableCHIAsyncBridge = if (depth != 0) {
            val sync = param.withDefaultValue(3)("sync").asInstanceOf[Int]
            val safe = param.withDefaultValue(false)("safe").asInstanceOf[Boolean]
            Some(AsyncQueueParams(depth, sync, safe))
          } else None
          nextConfig(config.alter((site, here, up) => {
            case SoCParamsKey => up(SoCParamsKey).copy(EnableCHIAsyncBridge = enableCHIAsyncBridge)
          }), tail)
        case ("L2CacheConfig", l2CacheConfig) :: tail =>
          val param = l2CacheConfig.asInstanceOf[JMap[String, Any]].asScala
          val n = getSizeInKB(param("size").asInstanceOf[String])
          val ways = param.withDefaultValue(8)("ways").asInstanceOf[Int]
          val inclusive = param.withDefaultValue(true)("inclusive").asInstanceOf[Boolean]
          val banks = param.withDefaultValue(1)("banks").asInstanceOf[Int]
          val tp = param.withDefaultValue(true)("tp").asInstanceOf[Boolean]
          nextConfig(config.alter(new WithNKBL2(n, ways, inclusive, banks, tp)), tail)
        case ("L3CacheConfig", l3CacheConfig) :: tail =>
          val param = l3CacheConfig.asInstanceOf[JMap[String, Any]].asScala
          val n = getSizeInKB(param("size").asInstanceOf[String])
          val ways = param.withDefaultValue(8)("ways").asInstanceOf[Int]
          val inclusive = param.withDefaultValue(true)("inclusive").asInstanceOf[Boolean]
          val banks = param.withDefaultValue(1)("banks").asInstanceOf[Int]
          nextConfig(config.alter(new WithNKBL3(n, ways, inclusive, banks)), tail)
        case ("DebugModuleBaseAddr", debugModuleBaseAddr) :: tail =>
          val param = toBigInt(debugModuleBaseAddr)
          nextConfig(config.alter((site, here, up) => {
            case DebugModuleKey => up(DebugModuleKey).map(_.copy(baseAddress = param))
          }), tail)
        case (s, _) :: tail =>
          println(s"Warning: $s is not supported in Yaml Config")
          nextConfig(config, tail)
      }
    }
    nextConfig(config, yamlSeq)
  }
}
