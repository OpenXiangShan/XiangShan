package xiangshan.backend.fu.vector

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import top.DefaultConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.{XSCoreParameters, XSCoreParamsKey}

class ByteMaskTailGenTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  println("Generating the ByteMaskTailGen hardware")

  val defaultConfig = (new DefaultConfig).alterPartial({
    case XSCoreParamsKey => XSCoreParameters()
  })

  emitVerilog(new ByteMaskTailGen(128)(defaultConfig), Array("--target-dir", "build/ByteMaskTailGen"))

  println("test start")

  behavior of "ByteMaskTailGen"
  it should "run" in {
    test(new ByteMaskTailGen(128)(defaultConfig)).withAnnotations(Seq(VerilatorBackendAnnotation)) {
      m: ByteMaskTailGen =>
        m.io.in.begin.poke(2.U)
        m.io.in.end.poke(7.U)
        m.io.in.vma.poke(false.B)
        m.io.in.vta.poke(false.B)
        m.io.in.vsew.poke(VSew.e8)
        m.io.in.maskUsed.poke("b0101_0101_0101_0101".U)

        println("startBytes: " + m.io.debugOnly.startBytes.peek().litValue.toInt)
        println("vlBytes: " + m.io.debugOnly.vlBytes.peek().litValue.toInt)
        println("prestartEn: " + m.io.debugOnly.prestartEn.peek().litValue.toString(2))
        println("activeEn: " + m.io.debugOnly.activeEn.peek().litValue.toString(2))
        println("tailEn: " + m.io.debugOnly.tailEn.peek().litValue.toString(2))
        println("maskEn: " + m.io.debugOnly.maskEn.peek().litValue.toString(2))
        println("keepEn: " + m.io.out.keepEn.peek().litValue.toString(2))
        println("agnosticEn: " + m.io.out.agnosticEn.peek().litValue.toString(2))
    }
    println("test done")
  }
}
