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

import org.chipsalliance.cde.config.{Config, Parameters}
import system.SoCParamsKey
import xiangshan.{DebugOptionsKey, XSTileKey}
import freechips.rocketchip.tile.MaxHartIdBits
import difftest.DifftestModule

import scala.annotation.tailrec
import scala.sys.exit
import chisel3.util.log2Up

object ArgParser {
  // TODO: add more explainations
  val usage =
    """
      |XiangShan Options
      |--xs-help                  print this help message
      |--config <ConfigClassName>
      |--num-cores <Int>
      |--with-dramsim3
      |--fpga-platform
      |--enable-difftest
      |--enable-log
      |--with-chiseldb
      |--with-rollingdb
      |--disable-perf
      |--disable-alwaysdb
      |""".stripMargin

  def getConfigByName(confString: String): Parameters = {
    var prefix = "top." // default package is 'top'
    if(confString.contains('.')){ // already a full name
      prefix = ""
    }
    val c = Class.forName(prefix + confString).getConstructor(Integer.TYPE)
    c.newInstance(1.asInstanceOf[Object]).asInstanceOf[Parameters]
  }
  def parse(args: Array[String]): (Parameters, Array[String], Array[String]) = {
    val default = new DefaultConfig(1)
    var firrtlOpts = Array[String]()
    var firtoolOpts = Array[String]()
    @tailrec
    def nextOption(config: Parameters, list: List[String]): Parameters = {
      list match {
        case Nil => config
        case "--xs-help" :: tail =>
          println(usage)
          if(tail == Nil) exit(0)
          nextOption(config, tail)
        case "--config" :: confString :: tail =>
          nextOption(getConfigByName(confString), tail)
        case "--num-cores" :: value :: tail =>
          nextOption(config.alter((site, here, up) => {
            case XSTileKey => (0 until value.toInt) map { i =>
              up(XSTileKey).head.copy(HartId = i)
            }
            case MaxHartIdBits =>
              log2Up(value.toInt) max up(MaxHartIdBits)
          }), tail)
        case "--with-dramsim3" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(UseDRAMSim = true)
          }), tail)
        case "--with-chiseldb" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnableChiselDB = true)
          }), tail)
        case "--with-rollingdb" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnableRollingDB = true)
          }), tail)
        case "--with-constantin" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnableConstantin = true)
          }), tail)
        case "--fpga-platform" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(FPGAPlatform = true)
          }), tail)
        case "--enable-difftest" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnableDifftest = true)
          }), tail)
        case "--disable-always-basic-diff" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(AlwaysBasicDiff = false)
          }), tail)
        case "--enable-log" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnableDebug = true)
          }), tail)
        case "--disable-perf" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnablePerfDebug = false)
          }), tail)
        case "--disable-alwaysdb" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(AlwaysBasicDB = false)
          }), tail)
        case "--xstop-prefix" :: value :: tail if chisel3.BuildInfo.version != "3.6.0" =>
          nextOption(config.alter((site, here, up) => {
            case SoCParamsKey => up(SoCParamsKey).copy(XSTopPrefix = Some(value))
          }), tail)
        case "--firtool-opt" :: option :: tail =>
          firtoolOpts ++= option.split(" ").filter(_.nonEmpty)
          nextOption(config, tail)
        case option :: tail =>
          // unknown option, maybe a firrtl option, skip
          firrtlOpts :+= option
          nextOption(config, tail)
      }
    }
    val newArgs = DifftestModule.parseArgs(args)
    var config = nextOption(default, newArgs.toList)
    (config, firrtlOpts, firtoolOpts)
  }
}
