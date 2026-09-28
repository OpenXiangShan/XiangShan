/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package top

import chisel3._
import freechips.rocketchip.diplomacy.DisableMonitors
import org.chipsalliance.cde.config.Parameters
import system.SoCParamsKey
import xiangshan.{DebugOptionsKey, XSTileKey}
import xs.utils.debug.{HardwareAssertionKey, HwaParams}
import xs.utils.perf.{DebugOptions => ZJDebugOptions, DebugOptionsKey => ZJDebugOptionsKey}
import xs.utils.perf.{LogUtilsOptions => ZJLogUtilsOptions, LogUtilsOptionsKey => ZJLogUtilsOptionsKey}
import xs.utils.perf.{PerfCounterOptions => ZJPerfCounterOptions, PerfCounterOptionsKey => ZJPerfCounterOptionsKey, XSPerfLevel => ZJXSPerfLevel}
import xscache.chi.{CHIDataCheckKey, CHIPoisonKey, DecoupledPortIO}
import zhujiang.{HasCHIToZhuJiangBridge, Zhujiang, ZJParametersKey}
import zhujiang.perf.{XiangShanUtilityPerfBackend, ZJPerfBackendKey}

// 独立 ZhuJiang（RTL）回放顶层：与 XSTop 内 zhujiang_opt 完全同配置（同一
// ZhuJiangNoCTopology + 同一 zhujiangParams 覆盖链），外加 connectCHIToZhuJiang
// 的 SocketDevSide + flit remap（该逻辑在 Zhujiang 模块之外、WolvicZjBB 边界之内，
// wolvicmod 模型自带这部分）。端口即 proj-xiangshan-l3 全程 trace 的三边界：
//   io_decoupledCHI_*  ↔ l2.* 列（tile 侧 CHI 缝）
//   m_axi_mem_0_*      ↔ mem.* 列（Zhujiang 自身 ExtAxi 口）
//   m_axi_main_*       ↔ cfg.* 列
// 用于 wolvicmod 模型与 RTL 的孤立同激励对拍与性能对比（不含 SoC/边界噪声）。
class ZhujiangReplayTop(implicit p: Parameters) extends RawModule with HasCHIToZhuJiangBridge {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(AsyncReset()))

  private val soc       = p(SoCParamsKey)
  private val debugOpts = p(DebugOptionsKey)
  private val numCores  = p(XSTileKey).size

  // 与 XSTop 对 core_with_l2 的 ZhuJiang 模式 alter 一致：关 DataCheck/Poison
  private val chiParams = p.alter((site, here, up) => {
    case CHIDataCheckKey => "none"
    case CHIPoisonKey    => false
  })
  val io_decoupledCHI = IO(Flipped(new DecoupledPortIO()(chiParams)))

  // 与 Top.scala XSTopImp 完全相同的 zhujiangParams 覆盖链
  private val zhujiangParams = p.alterPartial {
    case ZJParametersKey => ZhuJiangNoCTopology(numCores, soc.ZhuJiangParams, soc.L3OuterBusWidth)
    case ZJPerfBackendKey => XiangShanUtilityPerfBackend
    case HardwareAssertionKey => HwaParams(enable = false)
    case ZJLogUtilsOptionsKey => ZJLogUtilsOptions(
      enableDebug = false,
      enablePerf = debugOpts.EnablePerfDebug,
      fpgaPlatform = debugOpts.FPGAPlatform
    )
    case ZJPerfCounterOptionsKey => ZJPerfCounterOptions(
      enablePerfPrint = debugOpts.EnablePerfDebug && !debugOpts.FPGAPlatform,
      enablePerfDB = debugOpts.EnableRollingDB && !debugOpts.FPGAPlatform,
      perfLevel = ZJXSPerfLevel.withName(debugOpts.PerfLevel),
      perfDBHartID = 0
    )
    case ZJDebugOptionsKey => ZJDebugOptions(
      FPGAPlatform = debugOpts.FPGAPlatform,
      EnableDifftest = false,
      AlwaysBasicDiff = false,
      EnableDebug = false,
      EnablePerfDebug = debugOpts.EnablePerfDebug,
      UseDRAMSim = false,
      EnableTopDown = false,
      EnableChiselDB = false,
      AlwaysBasicDB = false,
      EnableRollingDB = debugOpts.EnableRollingDB,
      EnableHWMoniter = false
    )
  }

  // 环站复位传播/就绪指示，供回放 harness 对齐 trace row 0
  val on_reset = IO(Output(Bool()))

  // xs.utils 的 LogPerfHelper 经 XMR 引用 SimTop 作用域的四个 difftest 信号
  // （timer/log_enable/perfCtrl_clean/perfCtrl_dump）。独立回放没有 SimTop，
  // 在本顶层提供同名常量（perf/log 已关，值无功能影响），verilate 时以
  // +define+SIM_TOP_MODULE_NAME=ZhujiangReplayTop 指向本顶层。
  val difftest_timer = WireDefault(0.U(64.W))
  val difftest_log_enable = WireDefault(false.B)
  val difftest_perfCtrl_clean = WireDefault(false.B)
  val difftest_perfCtrl_dump = WireDefault(false.B)
  dontTouch(difftest_timer)
  dontTouch(difftest_log_enable)
  dontTouch(difftest_perfCtrl_clean)
  dontTouch(difftest_perfCtrl_dump)

  withClockAndReset(clock, reset) {
    val zj = Module(new Zhujiang()(zhujiangParams))

    val ccNodes = zhujiangParams(ZJParametersKey).island.filter(_.nodeType == xijiang.NodeType.CC)
    require(ccNodes.size >= numCores,
      s"ZhuJiang exposes ${ccNodes.size} CC nodes, but $numCores cores are requested")
    connectCHIToZhuJiang(io_decoupledCHI, zj.ccnIO(0), ccNodes(0), zhujiangParams)

    // Zhujiang 自身 ExtAxi 口前递为顶层端口（名对齐 trace 前缀）
    val m_axi_mem_0 = IO(chiselTypeOf(zj.ddrIO.head))
    m_axi_mem_0.suggestName("m_axi_mem_0")
    m_axi_mem_0 <> zj.ddrIO.head
    zj.ddrIO.drop(1).foreach(_ := DontCare)

    val m_axi_main = IO(chiselTypeOf(zj.cfgIO.head))
    m_axi_main.suggestName("m_axi_main")
    m_axi_main <> zj.cfgIO.head
    zj.cfgIO.drop(1).foreach(_ := DontCare)

    zj.dmaIO.foreach(_ := DontCare)
    zj.hwaIO.foreach(_ := DontCare)
    zj.io.ci := 0.U
    zj.io.dft := DontCare
    zj.io.ramctl := DontCare
    on_reset := zj.io.onReset
  }
}

object ZhujiangReplayTopMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)
  Generator.execute(
    firrtlOpts,
    DisableMonitors(p => new ZhujiangReplayTop()(p))(config),
    firtoolOpts
  )
}
