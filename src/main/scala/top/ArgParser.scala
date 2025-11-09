/***************************************************************************************
* Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
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

import difftest.DifftestModule
import freechips.rocketchip.tile.XLen
import org.chipsalliance.cde.config.{Config, Parameters}
import utility._

import scala.annotation.tailrec

object ArgParser {
  def parse(args: Array[String]): (Parameters, Array[String], Array[String]) = {
    val default = new Config((_, _, _) => {
      case XLen => 64
      case LogUtilsOptionsKey => LogUtilsOptions(
        enableDebug = false,
        enablePerf = false,
        fpgaPlatform = false,
        enableXMR = true,
      )
      case PerfCounterOptionsKey => PerfCounterOptions(
        enablePerfPrint = false,
        enablePerfDB = false,
        perfLevel = XSPerfLevel.NORMAL,
        perfDBHartID = 0
      )
    })
    var firrtlOpts = Array[String]()
    var firtoolOpts = Array[String]()
    @tailrec
    def nextOption(config: Parameters, list: List[String]): Parameters = {
      list match {
        case Nil => config
        case "--firtool-opt" :: option :: tail =>
          firtoolOpts ++= option.split(" ").filter(_.nonEmpty)
          nextOption(config, tail)
        case option :: tail =>
          // unknown option, maybe a firrtl option, skip
          firrtlOpts :+= option
          nextOption(config, tail)
      }
    }
    val (newArgs, firtoolOptions) = DifftestModule.parseArgs(args)
    val config = nextOption(default, newArgs.toList)
    (config, firrtlOpts, firtoolOpts ++ firtoolOptions.map(_.option))
  }
}
