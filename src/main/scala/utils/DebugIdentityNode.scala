package utils

import chisel3._
import chipsalliance.rocketchip.config.Parameters
import chisel3.util.DecoupledIO
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBundle, TLClientNode, TLIdentityNode, TLMasterParameters, TLMasterPortParameters}
import xiangshan.HasXSLog

class DebugIdentityNode()(implicit p: Parameters) extends LazyModule  {

  val node = TLIdentityNode()

  val n = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(
      TLMasterParameters.v1("debug node")
    )
  )))

  lazy val module = new LazyModuleImp(this) with HasXSLog with HasTLDump{
    val (out, _) = node.out(0)
    val (in, _) = node.in(0)

    def debug(t: TLBundle, valid: Boolean = false): Unit ={
      def fire[T <: Data](x: DecoupledIO[T]) = if(valid) x.valid else x.fire()
      val channels = Seq(t.a, t.b, t.c, t.d, t.e)
      channels.foreach(c =>
        when(fire(c)){
          XSDebug(" ")
          c.bits.dump
        }
      )
    }
    debug(in, true)
  }
}

object DebugIdentityNode {
  def apply()(implicit p: Parameters): TLIdentityNode = {
    val identityNode = LazyModule(new DebugIdentityNode())
    identityNode.node
  }
}