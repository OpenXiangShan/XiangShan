package utils

import chisel3._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLClientNode, TLIdentityNode, TLMasterParameters, TLMasterPortParameters}
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
    when(in.a.fire()){
      XSDebug(" ")
      in.a.bits.dump
    }
    when(in.d.fire()){
      XSDebug(" ")
      in.d.bits.dump
    }
  }
}

object DebugIdentityNode {
  def apply()(implicit p: Parameters): TLIdentityNode = {
    val identityNode = LazyModule(new DebugIdentityNode())
    identityNode.node
  }
}