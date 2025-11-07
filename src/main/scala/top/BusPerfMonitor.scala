package top

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{AdapterNode, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import chisel3._
import chisel3.util._
import utility.{XSPerfAccumulate, XSPerfPrint}

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

  def perfAccumulate(
    clientName: String,
    chnName: String,
    condFire: Bool,
    condStall: Bool,
    op: Option[String] = None,
  ): Unit = {
    val chn = chnName.replace("'", "").replace(" ", "_")
    val base = op match {
      case Some(o) => s"${clientName}_${chn}_${o.replace(" ", "_")}"
      case None    => s"${clientName}_${chn}"
    }
    XSPerfAccumulate(s"${base}_fire", condFire)
    XSPerfAccumulate(s"${base}_stall", condStall)
  }

  def PERF_CHN[T <: TLChannel](clientName: String, chn: DecoupledIO[T]) = {

    perfAccumulate(clientName, chn.bits.channelName, chn.fire, chn.valid && !chn.ready)

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
          perfAccumulate(
            clientName,
            chn.bits.channelName,
            i.U === a.opcode && chn.fire,
            i.U === a.opcode && chn.valid && !chn.ready,
            Some(op)
          )
        case b: TLBundleB =>
          perfAccumulate(
            clientName,
            chn.bits.channelName,
            i.U === b.opcode && chn.fire,
            i.U === b.opcode && chn.valid && !chn.ready,
            Some(op)
          )
        case c: TLBundleC =>
          perfAccumulate(
            clientName,
            chn.bits.channelName,
            i.U === c.opcode && chn.fire,
            i.U === c.opcode && chn.valid && !chn.ready,
            Some(op)
          )
        case d: TLBundleD =>
          perfAccumulate(
            clientName,
            chn.bits.channelName,
            i.U === d.opcode && chn.fire,
            i.U === d.opcode && chn.valid && !chn.ready,
            Some(op)
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
