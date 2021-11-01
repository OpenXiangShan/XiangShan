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

import chipsalliance.rocketchip.config.{Config, Parameters}
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
      |--dual-core                same as '--num-cores 2'
      |--with-dramsim3
      |--disable-log
      |--disable-perf
      |""".stripMargin

  def getConfigByName(confString: String): Parameters = {
    var prefix = "top." // default package is 'top'
    if(confString.contains('.')){ // already a full name
      prefix = ""
    }
    val c = Class.forName(prefix + confString).getConstructor(Integer.TYPE)
    c.newInstance(1.asInstanceOf[Object]).asInstanceOf[Parameters]
  }
  def parse(args: Array[String], fpga: Boolean = true): (Parameters, Array[String]) = {
    val default = new DefaultConfig(1)
    var firrtlOpts = Array[String]()
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
        case "--dual-core" :: tail =>
          nextOption(config, "--num-cores" :: "2" :: tail)
        case "--with-dramsim3" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(UseDRAMSim = true)
          }), tail)
        case "--disable-log" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnableDebug = false)
          }), tail)
        case "--disable-perf" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnablePerfDebug = false)
          }), tail)
        case option :: tail =>
          // unknown option, maybe a firrtl option, skip
          firrtlOpts :+= option
          nextOption(config, tail)
      }
    }
    var config = nextOption(default, args.toList)
    if(!fpga){
      config = config.alter((site, here, up) => {
        case DebugOptionsKey => up(DebugOptionsKey).copy(FPGAPlatform = false)
      })
    }
    (config, firrtlOpts)
  }
}
