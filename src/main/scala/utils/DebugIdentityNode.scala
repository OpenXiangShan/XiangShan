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
    when(in.a.fire()){
      printf(p"[A] addr: ${Hexadecimal(in.a.bits.address)} " +
        p"opcode: ${in.a.bits.opcode} data: ${Hexadecimal(in.a.bits.data)}\n"
      )
    }
    when(in.d.fire()){
      printf(p"[D] opcode: ${in.d.bits.opcode} data: ${Hexadecimal(in.d.bits.data)}\n")
    }
  }
}
