/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import utility._

object ArgParser {
  // TODO: add more explainations
  val usage =
    """
      |XiangShan Options
      |--xs-help                  print this help message
      |--version                  print version info
      |--config <ConfigClassName>
      |--num-cores <Int>
      |--hartidbits <Int>
      |--with-dramsim3
      |--fpga-platform
      |--reset-gen
      |--enable-difftest
      |--enable-log
      |--with-chiseldb
      |--with-rollingdb
      |--disable-perf
      |--disable-alwaysdb
      |--enable-dfx
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
        case "--version" :: tail =>
          println(os.read(os.resource / "publishVersion"))
          if(tail == Nil) exit(0)
          nextOption(config, tail)
        case "--config" :: confString :: tail =>
          nextOption(getConfigByName(confString), tail)
        case "--issue" :: issueString :: tail =>
          nextOption(config.alter((site, here, up) => {
            case coupledL2.tl2chi.CHIIssue => issueString
          }), tail)
        case "--num-cores" :: value :: tail =>
          nextOption(config.alter((site, here, up) => {
            case XSTileKey => (0 until value.toInt) map { i =>
              up(XSTileKey).head.copy(HartId = i)
            }
            case MaxHartIdBits =>
              log2Up(value.toInt) max up(MaxHartIdBits)
          }), tail)
        case "--hartidbits" :: hartidbits :: tail =>
          nextOption(config.alter((site, here, up) => {
            case MaxHartIdBits => hartidbits.toInt
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
        case "--reset-gen" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(ResetGen = true)
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
        case "--perf-level" :: value :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(PerfLevel = value)
          }), tail)
        case "--disable-alwaysdb" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(AlwaysBasicDB = false)
          }), tail)
        case "--xstop-prefix" :: value :: tail =>
          nextOption(config.alter((site, here, up) => {
            case SoCParamsKey => up(SoCParamsKey).copy(XSTopPrefix = Some(value))
          }), tail)
        case "--imsic-use-tl" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case SoCParamsKey => up(SoCParamsKey).copy(IMSICUseTL = true)
          }), tail)
        case "--firtool-opt" :: option :: tail =>
          firtoolOpts ++= option.split(" ").filter(_.nonEmpty)
          nextOption(config, tail)
        case "--l2-cache-size" :: value :: tail =>
          nextOption(config.alter((site, here, up) => {
            case XSTileKey =>
              val tileParams = up(XSTileKey)
              val banks = tileParams.map(_.L2NBanks)
              val ways = tileParams.map(_.L2CacheParamsOpt.map(_.ways))
              val l2sets = banks zip ways map { case (banks, ways) =>
                ways.map(value.toInt * 1024 / banks / _ / 64)
              }
              val newL2Params = tileParams zip l2sets map { case (tileParam, l2sets) =>
                tileParam.L2CacheParamsOpt.map(_.copy(
                  sets = l2sets.get
                ))
              }
              tileParams zip newL2Params map { case (tileParam, newL2Param) =>
                tileParam.copy(L2CacheParamsOpt = newL2Param)
              }
          }), tail)
        case "--l3-cache-size" :: value :: tail =>
          nextOption(config.alter((site, here, up) => {
            case SoCParamsKey =>
              val socParam = up(SoCParamsKey)
              val banks = socParam.L3NBanks
              val l3Ways = socParam.L3CacheParamsOpt.map(_.ways)
              val l3Sets = l3Ways.map(value.toInt * 1024 / banks / _ / 64)
              val openLLCWays = socParam.OpenLLCParamsOpt.map(_.ways)
              val openLLCSets = openLLCWays.map(value.toInt * 1024 / banks / _ / 64)
              val newL3Param = socParam.L3CacheParamsOpt.map(_.copy(
                sets = l3Sets.get
              ))
              val openLLCParam = socParam.OpenLLCParamsOpt.map(_.copy(
                sets = openLLCSets.get
              ))
              socParam.copy(
                L3CacheParamsOpt = newL3Param,
                OpenLLCParamsOpt = openLLCParam
              )
          }), tail)
        case "--dfx" :: value :: tail =>
          nextOption(config.alter((site, here, up) => {
            case XSTileKey => up(XSTileKey).map(_.copy(hasMbist = value.toBoolean))
          }), tail)
        case "--sram-with-ctl" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case XSTileKey => up(XSTileKey).map(_.copy(hasSramCtl = true))
          }), tail)
        case "--seperate-tl-bus" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case SoCParamsKey => up(SoCParamsKey).copy(SeperateTLBus = true)
          }), tail)
        case "--seperate-dm" :: tail =>
          nextOption(config.alter((site, here, up) => {
            case SoCParamsKey => up(SoCParamsKey).copy(SeperateDM = true)
          }), tail)
        case "--yaml-config" :: yamlFile :: tail =>
          nextOption(YamlParser.parseYaml(config, yamlFile), tail)
        case option :: tail =>
          // unknown option, maybe a firrtl option, skip
          firrtlOpts :+= option
          nextOption(config, tail)
      }
    }
    val newArgs = DifftestModule.parseArgs(args)
    val config = nextOption(default, newArgs.toList).alter((site, here, up) => {
      case LogUtilsOptionsKey => LogUtilsOptions(
        here(DebugOptionsKey).EnableDebug,
        here(DebugOptionsKey).EnablePerfDebug,
        here(DebugOptionsKey).FPGAPlatform
      )
      case PerfCounterOptionsKey => PerfCounterOptions(
        here(DebugOptionsKey).EnablePerfDebug && !here(DebugOptionsKey).FPGAPlatform,
        here(DebugOptionsKey).EnableRollingDB && !here(DebugOptionsKey).FPGAPlatform,
        XSPerfLevel.withName(here(DebugOptionsKey).PerfLevel),
        0
      )
    })
    (config, firrtlOpts, firtoolOpts)
  }
}
