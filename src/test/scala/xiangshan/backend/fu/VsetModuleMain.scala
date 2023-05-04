package xiangshan.backend.fu

import chisel3._
import chiseltest.{ChiselScalatestTester, _}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import top.DefaultConfig
import xiangshan.backend.fu.vector.Bundles.{VLmul, VSew}
import xiangshan.{VSETOpType, XSCoreParameters, XSCoreParamsKey, XSTileKey}

class VsetModuleMain extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val defaultConfig = (new DefaultConfig).alterPartial({
    case XSCoreParamsKey => XSCoreParameters()
  })

  def F = false.B

  def T = true.B

  private val (ill, vta, vma, vsew, vlmul) = (F, F, F, VSew.e8, VLmul.m8)
  private val avl = 30.U

  println("Test started!")

  behavior of "VsetModule"
  it should "run" in {
    test(new VsetModule()(defaultConfig)) { m: VsetModule =>
      m.io.in.func.poke(VSETOpType.uvsetvcfg_xi)
      m.io.in.avl.poke(avl)
      m.io.in.vtype.illegal.poke(ill)
      m.io.in.vtype.vta.poke(vta)
      m.io.in.vtype.vma.poke(vma)
      m.io.in.vtype.vsew.poke(vsew)
      m.io.in.vtype.vlmul.poke(vlmul)
      m.io.in.oldVl.poke(0)
      m.io.out.vconfig.vtype.illegal.expect(false.B)
      m.io.out.vconfig.vtype.vta.expect(vta)
      m.io.out.vconfig.vtype.vma.expect(vma)
      m.io.out.vconfig.vtype.vsew.expect(vsew)
      m.io.out.vconfig.vtype.vlmul.expect(vlmul)
      m.io.out.vconfig.vl.expect((32).U)

      println(s"log2Vlmax : ${m.io.testOut.log2Vlmax.peek().litValue}")
      println(s"vlmax : ${m.io.testOut.vlmax.peek().litValue} ")
      println(s"vta: ${m.io.out.vconfig.vtype.vta.peek().litToBoolean} ")
      println(s"vma: ${m.io.out.vconfig.vtype.vma.peek().litToBoolean} ")
      println(s"vsew: ${m.io.out.vconfig.vtype.vsew.peek().litValue} ")
      println(s"vlmul: ${m.io.out.vconfig.vtype.vlmul.peek().litValue} ")
    }
  }
}
