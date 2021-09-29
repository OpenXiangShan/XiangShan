package top

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
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

  def PERF_CHN[T <: TLChannel](clientName: String, chn: DecoupledIO[T]) = {

    XSPerfAccumulate(s"$clientName ${chn.bits.channelName} fire", chn.fire())
    XSPerfAccumulate(s"$clientName ${chn.bits.channelName} stall", chn.valid && !chn.ready)

    val ops = chn.bits match {
      case _: TLBundleA => TLMessages.a.map(_._1)
      case _: TLBundleB => TLMessages.b.map(_._1)
      case _: TLBundleC => TLMessages.c.map(_._1)
      case _: TLBundleD => TLMessages.d.map(_._1)
      case _: TLBundleE => Nil
    }
    for((op, i) <- ops.zipWithIndex){
      chn.bits match {
        case a: TLBundleA =>
          XSPerfAccumulate(s"$clientName ${chn.bits.channelName} $op fire",
            i.U === a.opcode && chn.fire()
          )
          XSPerfAccumulate(s"$clientName ${chn.bits.channelName} $op stall",
            i.U === a.opcode && chn.valid && !chn.ready
          )
        case b: TLBundleB =>
          XSPerfAccumulate(s"$clientName ${chn.bits.channelName} $op fire",
            i.U === b.opcode && chn.fire()
          )
          XSPerfAccumulate(s"$clientName ${chn.bits.channelName} $op stall",
            i.U === b.opcode && chn.valid && !chn.ready
          )
        case c: TLBundleC =>
          XSPerfAccumulate(s"$clientName ${chn.bits.channelName} $op fire",
            i.U === c.opcode && chn.fire()
          )
          XSPerfAccumulate(s"$clientName ${chn.bits.channelName} $op stall",
            i.U === c.opcode && chn.valid && !chn.ready
          )
        case d: TLBundleD =>
          XSPerfAccumulate(s"$clientName ${chn.bits.channelName} $op fire",
            i.U === d.opcode && chn.fire()
          )
          XSPerfAccumulate(s"$clientName ${chn.bits.channelName} $op stall",
            i.U === d.opcode && chn.valid && !chn.ready
          )
      }
    }
  }

  for(((in, edgeIn), i) <- outer.node.in.zipWithIndex) {
    val clientName = s"${edgeIn.master.masters.head.name}_bank_$i"
    PERF_CHN(clientName, in.a)
    PERF_CHN(clientName, in.d)
    if(in.params.hasBCE){
      PERF_CHN(clientName, in.b)
      PERF_CHN(clientName, in.c)
      PERF_CHN(clientName, in.e)
    }
  }
}

object BusPerfMonitor {
  def apply(enable: Boolean = false)(implicit p: Parameters): TLAdapterNode = {
    val busPMU = LazyModule(new BusPerfMonitor(enable))
    busPMU.node
  }
}
