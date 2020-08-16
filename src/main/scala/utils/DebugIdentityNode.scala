package utils

import chisel3._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLClientNode, TLIdentityNode, TLMasterParameters, TLMasterPortParameters}

class DebugIdentityNode()(implicit p: Parameters) extends LazyModule {

  val node = TLIdentityNode()

  val n = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(
      TLMasterParameters.v1("debug node")
    )
  )))

  lazy val module = new LazyModuleImp(this){
    val (out, _) = node.out(0)
    val (in, _) = node.in(0)
    val timer = GTimer()
    when(in.a.fire()){
      printf(p"[$timer][A] addr: ${Hexadecimal(in.a.bits.address)} " +
        p"opcode: ${in.a.bits.opcode} data: ${Hexadecimal(in.a.bits.data)} size: ${in.a.bits.size} source: ${in.a.bits.source}\n"
      )
    }
    when(in.d.fire()){
      printf(p"[$timer][D] opcode: ${in.d.bits.opcode} data: ${Hexadecimal(in.d.bits.data)} size:${in.d.bits.size} source: ${in.d.bits.source}\n")
    }
  }
}

object DebugIdentityNode {
  def apply()(implicit p: Parameters): TLIdentityNode = {
    val identityNode = LazyModule(new DebugIdentityNode())
    identityNode.node
  }
}