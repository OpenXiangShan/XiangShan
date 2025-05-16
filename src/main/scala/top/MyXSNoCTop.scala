package my

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

import xiangshan._
import top._

class MyXSTileWrap()(implicit p: Parameters) extends XSTileWrap
{
  override lazy val desiredName: String = "MyXSTile"
}

class MyXSNoCTop()(implicit p: Parameters) extends XSNoCTop
{
  override protected def buildCoreWithL2(params: Parameters): XSTileWrap = {
    buildLazyModuleWithName("core_with_l2")(
      (ps: Parameters) => new MyXSTileWrap()(ps)
    )(params)
  }
}
