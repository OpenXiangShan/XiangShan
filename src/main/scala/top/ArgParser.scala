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

import scala.annotation.tailrec
import scala.sys.exit

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
      |--mfc
      |""".stripMargin

  def getConfigByName(confString: String): Parameters = {
    var prefix = "top." // default package is 'top'
    if(confString.contains('.')){ // already a full name
      prefix = ""
    }
    val c = Class.forName(prefix + confString).getConstructor(Integer.TYPE)
    c.newInstance(1.asInstanceOf[Object]).asInstanceOf[Parameters]
  }
  def parse(args: Array[String]): (Parameters, Array[String], FirrtlCompiler, Array[String]) = {
    val default = new DefaultConfig(1)
    var firrtlOpts = Array[String]()
    var firrtlCompiler: FirrtlCompiler = SFC
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
            case XSTileKey => (0 until value.toInt) map{ i =>
              up(XSTileKey).head.copy(HartId = i)
            }
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
        case "--enable-log" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnableDebug = true)
          }), tail)
        case "--disable-perf" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnablePerfDebug = false)
          }), tail)
        case "--mfc" :: tail =>
          firrtlCompiler = MFC
          nextOption(config, tail)
        case "--firtool-opt" :: option :: tail =>
          firtoolOpts :+= option
          nextOption(config, tail)
        case option :: tail =>
          // unknown option, maybe a firrtl option, skip
          firrtlOpts :+= option
          nextOption(config, tail)
      }
    }
    var config = nextOption(default, args.toList)
    (config, firrtlOpts, firrtlCompiler, firtoolOpts)
  }
}
