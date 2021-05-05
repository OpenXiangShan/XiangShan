package utils

import chisel3._
import chipsalliance.rocketchip.config.Parameters
import chisel3.util.DecoupledIO
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBundle, TLClientNode, TLIdentityNode, TLMasterParameters, TLMasterPortParameters}

class DataDontCareNode(a: Boolean = false, b: Boolean = false, c: Boolean = false, d: Boolean = false)(implicit p: Parameters) extends LazyModule  {

  val node = TLIdentityNode()

  val n = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(
      TLMasterParameters.v1("DataDontCareNode")
    )
  )))

  lazy val module = new LazyModuleImp(this) with HasTLDump{
    val (out, _) = node.out(0)
    val (in, _) = node.in(0)

    if (a) {
      out.a.bits.data := DontCare
    }
    if (b) {
      in.b.bits.data := DontCare
    }
    if (c) {
      out.c.bits.data := DontCare
    }
    if (d) {
      in.d.bits.data := DontCare
    }
  }
}

object DataDontCareNode {
  def apply(a: Boolean = false, b: Boolean = false, c: Boolean = false, d: Boolean = false)(implicit p: Parameters): TLIdentityNode = {
    val dataDontCareNode = LazyModule(new DataDontCareNode(a, b, c, d))
    dataDontCareNode.node
  }
}
