// WolvicZjBB：wolvicmod ZhuJiang L3 模型（WolvicZjTop）的 BlackBox 边界。
//
// 由 --wolvic-zj 开启（需 LLC=ZhuJiang）：XSTop 中以本模块整体替换
// Zhujiang 实例与 connectCHIToZhuJiang 在 l_soc 侧例化的 SocketDevSide +
// flit remap——即模型的全部边界：
//   rn   = 各 tile L2 的 xscache DecoupledPortIO（纯 valid/ready 六通道）
//   ddrc = memAXI（zhujiangMemMaster，S 节点）
//   peri = cfgAXI（zhujiangCfgMasters，HI 节点）
// 对应的 SV 实现（DPI-C 薄壳，调用 wolvicmod 静态库）经 verilator libdir
// （RTL_INCLUDE）注入，不在本仓库。
package top

import chisel3._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.{Field, Parameters}
import utils.VerilogAXI4Record
import xscache.chi.DecoupledPortIO

case object UseWolvicZjKey extends Field[Boolean](false)

class WolvicZjBB(
  numCores: Int,
  ddrcParams: AXI4BundleParameters,
  periParams: Seq[AXI4BundleParameters]
)(implicit p: Parameters) extends BlackBox {
  // Vec 要求各口参数一致；kunminghu-v3 单 HI 节点，天然满足
  require(periParams.nonEmpty && periParams.distinct.size == 1,
    "WolvicZjBB: peri ports must share identical AXI4 parameters")
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val rn = Vec(numCores, Flipped(new DecoupledPortIO))
    val ddrc = new VerilogAXI4Record(ddrcParams)
    val peri = Vec(periParams.size, new VerilogAXI4Record(periParams.head))
  })
  override val desiredName = "WolvicZjBB"
}
