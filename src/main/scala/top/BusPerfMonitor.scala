package top

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AdapterNode, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import chisel3._
import chisel3.util._
import utils.{XSPerfAccumulate, XSPerfPrint}

class BusPerfMonitor()(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode()
  lazy val module = new BusPerfMonitorImp(this)
}

class BusPerfMonitorImp(outer: BusPerfMonitor)
  extends LazyModuleImp(outer)
{

  outer.node.in.zip(outer.node.out).foreach{
    case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
  }

  def PERF_CHN[T <: TLChannel](clientName: String, chn: DecoupledIO[T]) = {

    val channelName = chn.bits.channelName.replaceAll(" ", "_").replaceAll("'", "")
    XSPerfAccumulate(s"${clientName}_${channelName}_fire", chn.fire())
    XSPerfAccumulate(s"${clientName}_${channelName}_stall", chn.valid && !chn.ready)

    val ops = chn.bits match {
      case _: TLBundleA => TLMessages.a.map(_._1)
      case _: TLBundleB => TLMessages.b.map(_._1)
      case _: TLBundleC => TLMessages.c.map(_._1)
      case _: TLBundleD => TLMessages.d.map(_._1)
      case _: TLBundleE => Nil
    }

    for((op_raw, i) <- ops.zipWithIndex){
      val op = s"${op_raw}".replaceAll(" ", "_")
      chn.bits match {
        case a: TLBundleA =>
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_fire",
            i.U === a.opcode && chn.fire()
          )
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_stall",
            i.U === a.opcode && chn.valid && !chn.ready
          )
        case b: TLBundleB =>
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_fire",
            i.U === b.opcode && chn.fire()
          )
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_stall",
            i.U === b.opcode && chn.valid && !chn.ready
          )
        case c: TLBundleC =>
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_fire",
            i.U === c.opcode && chn.fire()
          )
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_stall",
            i.U === c.opcode && chn.valid && !chn.ready
          )
        case d: TLBundleD =>
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_fire",
            i.U === d.opcode && chn.fire()
          )
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_stall",
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
  def apply(enable: Boolean = false)(implicit p: Parameters) = {
    if(enable){
      val busPMU = LazyModule(new BusPerfMonitor())
      busPMU.node
    } else {
      TLTempNode()
    }
  }
}
