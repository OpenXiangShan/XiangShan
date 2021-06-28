package top

import bus.tilelink.TLChannel
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLBundle, TLBundleBase, TLEdge}
import chisel3._
import chisel3.util._
import utils.{XSPerfAccumulate, XSPerfPrint}

class BusPerfMonitor(enable: Boolean)(implicit p: Parameters) extends LazyModule {

  val node = TLAdapterNode()

  lazy val module = if(enable) {
    new BusPerfMonitorImp(this)
  } else new BaseBusPerfMonitorImp(this)

}

class BaseBusPerfMonitorImp(outer: BusPerfMonitor)
  extends LazyModuleImp(outer)
{
  outer.node.in.zip(outer.node.out).foreach{
    case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
  }
}

class BusPerfMonitorImp(outer: BusPerfMonitor)
  extends BaseBusPerfMonitorImp(outer)
{
  def CHANNEL_CONFLICT[T <: Data](in: Seq[(DecoupledIO[T], String)], channelName: String): Unit = {
    val (chns, names) = in.unzip
    val valids = Cat(chns.map(_.valid))
    val conflict = PopCount(valids) > 1.U
    when(conflict){
      XSPerfPrint(p"CHN_CONFLICT: [$channelName] [${names.mkString(" ")}] ${Binary(valids)}\n")
    }
    XSPerfAccumulate(s"TOTAL_CHN_CONFLICT: [$channelName]", conflict)
  }

  CHANNEL_CONFLICT(outer.node.in.map(in => (in._1.a, in._2.master.masters.head.name)), "A")
  CHANNEL_CONFLICT(outer.node.in.map(in => (in._1.c, in._2.master.masters.head.name)), "C")
  CHANNEL_CONFLICT(outer.node.in.map(in => (in._1.e, in._2.master.masters.head.name)), "E")

  for((in, edgeIn) <- outer.node.in){
    val client = edgeIn.client.clients.head.name
    when(in.a.fire()){
      XSPerfPrint(p"CHN_STAT_A: [$client] op: ${in.a.bits.opcode} param: ${in.a.bits.param}\n")
    }
    when(in.d.fire()){
      XSPerfPrint(p"CHN_STAT_D: [$client] op: ${in.d.bits.opcode} param: ${in.d.bits.param}\n")
    }
    if(in.params.hasBCE){
      when(in.b.fire()){
        XSPerfPrint(p"CHN_STAT_B: [$client] op: ${in.b.bits.opcode} param: ${in.b.bits.param}\n")
      }
      when(in.c.fire()){
        XSPerfPrint(p"CHN_STAT_C: [$client] op: ${in.c.bits.opcode} param: ${in.c.bits.param}\n")
      }
      when(in.e.fire()){
        XSPerfPrint(p"CHN_STAT_E: [$client]\n")
      }
    }
  }
}

object BusPerfMonitor {
  def apply(enable: Boolean = false)(implicit p: Parameters): TLAdapterNode = {
    val busPMU = LazyModule(new BusPerfMonitor(enable))
    busPMU.node
  }
}
