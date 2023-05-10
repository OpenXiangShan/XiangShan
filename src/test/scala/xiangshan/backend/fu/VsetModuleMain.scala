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

  private val ills = Seq(F, T)
  private val vtas = Seq(F, T)
  private val vmas = Seq(F, T)
  private val vsews = Seq(VSew.e8, VSew.e16, VSew.e32, VSew.e64)
  private val vlmuls = Seq(VLmul.m8, VLmul.m4, VLmul.m2, VLmul.m1, VLmul.mf2, VLmul.mf4, VLmul.mf8)
  private val avls = Seq(0.U, 1.U, 0x100.U)
  private val funcs = Seq(VSETOpType.uvsetvcfg_xi,
                          VSETOpType.uvsetrd_xi,
                          VSETOpType.uvsetvcfg_vlmax_i,
                          VSETOpType.uvsetrd_vlmax_i,
                          VSETOpType.uvsetvcfg_keep_v,
                          VSETOpType.uvsetvcfg_xx,
                          VSETOpType.uvsetrd_xx,
                          VSETOpType.uvsetvcfg_vlmax_x,
                          VSETOpType.uvsetrd_vlmax_x,
                          VSETOpType.uvsetvcfg_vv,
                          VSETOpType.uvsetvcfg_ii,
                          VSETOpType.uvsetrd_ii)
  private val oldVls = Seq(0x83.U)

  val inputs = for {
    ill <- ills
    vta <- vtas
    vma <- vmas
    vsew <- vsews
    vlmul <- vlmuls
    func <- funcs
    avl <- avls
    oldVl <- oldVls
  }
  yield {
    (ill, vta, vma, vsew, vlmul, func, avl, oldVl)
  }


  println("Test started!")

  behavior of "VsetModule"
  it should "run" in {
    test(new VsetTop()(defaultConfig)) { m: VsetTop =>
      for((ill, vta, vma, vsew, vlmul, func, avl, oldVl) <- inputs) {
        val (testVl, testVill, testVta, testVma, testVsew, testVlmul) = VsetRef.test(avlPre = avl.litValue.toInt,
                                                                                     vsew = vsew.litValue.toInt,
                                                                                     vlmul = vlmul.litValue.toInt,
                                                                                     vta = vta.litToBoolean,
                                                                                     vma = vma.litToBoolean,
                                                                                     villPre = ill.litToBoolean,
                                                                                     func = func.litValue.toInt,
                                                                                     oldVL = oldVl.litValue.toInt)

        m.io.in.func.poke(func)
        m.io.in.avl.poke(avl)
        m.io.in.vtype.illegal.poke(ill)
        m.io.in.vtype.vta.poke(vta)
        m.io.in.vtype.vma.poke(vma)
        m.io.in.vtype.vsew.poke(vsew)
        m.io.in.vtype.vlmul.poke(vlmul)
        m.io.in.oldVl.poke(oldVl)

        val message = " ill: " + ill.litToBoolean +
                      " vta: " + vta.litToBoolean +
                      " vma: " + vma.litToBoolean +
                      " vsew: " + vsew.litValue.toInt +
                      " vlmul: " + vlmul.litValue.toInt +
                      " func: " + func.litValue.toInt.toBinaryString +
                      " avl: " + avl.litValue.toInt

        m.io.out.vconfig.vtype.illegal.expect(testVill.B, message)
        m.io.out.vconfig.vtype.vta.expect(testVta, message)
        m.io.out.vconfig.vtype.vma.expect(testVma, message)
        m.io.out.vconfig.vtype.vsew.expect(testVsew, message)
        m.io.out.vconfig.vtype.vlmul.expect(testVlmul, message)
        m.io.out.vconfig.vl.expect(testVl.U, message)

        println(s"illegal: ${m.io.out.vconfig.vtype.illegal.peek().litValue}")
        println(s"vta: ${m.io.out.vconfig.vtype.vta.peek().litToBoolean} ")
        println(s"vma: ${m.io.out.vconfig.vtype.vma.peek().litToBoolean} ")
        println(s"vsew: ${m.io.out.vconfig.vtype.vsew.peek().litValue} ")
        println(s"vlmul: ${m.io.out.vconfig.vtype.vlmul.peek().litValue} ")
        println(s"vl: ${m.io.out.vconfig.vl.peek().litValue} ")
        println("-------------")
        println(s"fuoptype: ${m.io.debug.fuOpType0.peek().litValue.toInt.toBinaryString} ")
        println(s"src0: ${m.io.debug.src0(0).peek().litValue.toInt.toBinaryString} ")
        println(s"src1: ${m.io.debug.src0(1).peek().litValue.toInt.toBinaryString} ")
        println("-------------")
        println(s"fuoptype: ${m.io.debug.fuOpType1.peek().litValue.toInt.toBinaryString} ")
        println(s"src0: ${m.io.debug.src1(0).peek().litValue.toInt.toBinaryString} ")
        println(s"src1: ${m.io.debug.src1(1).peek().litValue.toInt.toBinaryString} ")
        println("-------------")
        println(s"fuoptype: ${m.io.debug.fuOpType2.peek().litValue.toInt.toBinaryString} ")
        println(s"src0: ${m.io.debug.src2(0).peek().litValue.toInt.toBinaryString} ")
        println(s"src1: ${m.io.debug.src2(1).peek().litValue.toInt.toBinaryString} ")
      }
    }
  }
}
