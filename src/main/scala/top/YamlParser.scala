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

package top

import org.chipsalliance.cde.config.{Config, Parameters}

import org.yaml.snakeyaml.Yaml
import scala.jdk.CollectionConverters._
import scala.annotation.tailrec
import system.SoCParamsKey
import xiangshan.backend.fu.PMAConfigEntry

object YamlParser {
  def parseYaml(config: Parameters, yamlFile: String): Parameters = {
    val file = new java.io.File(yamlFile)
    val inputStream = new java.io.FileInputStream(file)
    val yamlSeq = (new Yaml()).load(inputStream).asInstanceOf[java.util.Map[String, Any]].asScala.toSeq
    @tailrec
    def nextConfig(config: Parameters, yamlSeq: Seq[(String, Any)]): Parameters = {
      yamlSeq match {
        case Nil => config
        case ("PmemRanges", pmemRanges) :: tail =>
          val param = pmemRanges.asInstanceOf[java.util.List[java.util.Map[String, Any]]].asScala.map { range =>
            val Seq(lower, upper) = Seq("lower", "upper").map { key =>
              range.asScala(key) match {
                case i: Int => BigInt(i)
                case l: Long => BigInt(l)
              }
            }
            (lower, upper)
          }.toSeq
          nextConfig(config.alter((site, here, up) => {
            case SoCParamsKey => up(SoCParamsKey).copy(PmemRanges = param)
          }), tail)
        case ("PMAConfigs", pmaConfigs) :: tail =>
          val param = pmaConfigs.asInstanceOf[java.util.List[java.util.Map[String, Any]]].asScala.map { pmaConfig =>
            require(pmaConfig.containsKey("base_addr"), "base_addr is required in every PMAConfigs")
            val conf = pmaConfig.asScala.withDefaultValue(0)
            val base_addr = conf("base_addr") match {
              case i: Int => BigInt(i)
              case l: Long => BigInt(l)
              case s"PmemRanges($i).lower" => config(SoCParamsKey).PmemRanges(i.toInt)._1
              case s"PmemRanges($i).upper" => config(SoCParamsKey).PmemRanges(i.toInt)._2
            }
            val range = conf("range") match {
              case i: Int => BigInt(i)
              case l: Long => BigInt(l)
            }
            val a = conf("a") match {
              case i: Int => i.toInt
              case l: Long => l.toInt
            }
            require(a == 0 || a == 1 || a == 3, "a should be 0, 1 or 3")
            val Seq(l, c, atomic, x, w, r) = Seq("l", "c", "atomic", "x", "w", "r").map { key =>
              conf(key) match {
                case i: Int => i != 0
                case i: Long => i != 0
                case b: Boolean => b
              }
            }
            PMAConfigEntry(base_addr, range, l, c, atomic, a, x, w, r)
          }.toSeq
          nextConfig(config.alter((site, here, up) => {
            case SoCParamsKey => up(SoCParamsKey).copy(PMAConfigs = param)
          }), tail)
        case (s, _) :: tail =>
          println(s"Warning: $s is not supported in Yaml Config")
          nextConfig(config, tail)
      }
    }
    nextConfig(config, yamlSeq)
  }
}
