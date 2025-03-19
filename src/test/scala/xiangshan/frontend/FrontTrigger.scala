package xiangshan.frontend

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey}
import xiangshan.{DebugOptionsKey, XSCoreParameters, XSCoreParamsKey}


class FrontendTriggerTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  val defaultConfig = (new DefaultConfig).alterPartial {
    case XSCoreParamsKey => XSCoreParameters()
  }.alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(
      here(DebugOptionsKey).EnableDebug,
      here(DebugOptionsKey).EnablePerfDebug,
      here(DebugOptionsKey).FPGAPlatform
    )
  })

  println("test start")

  behavior of "FrontendTrigger"
  it should "run" in {
    test(new FrontendTrigger()(defaultConfig)).withAnnotations(Seq(VerilatorBackendAnnotation)) {
      m: FrontendTrigger =>
        m.io.frontendTrigger.debugMode.poke(false.B)
        m.io.frontendTrigger.tEnableVec.map(_.poke(false.B))
        m.io.frontendTrigger.tEnableVec(1).poke(true.B)
        m.io.frontendTrigger.triggerCanRaiseBpExp.poke(true.B)
        m.io.frontendTrigger.tUpdate.valid.poke(true.B)
        m.io.frontendTrigger.tUpdate.bits.addr.poke(1.U)
        m.io.frontendTrigger.tUpdate.bits.tdata.matchType.poke(3.U)
        m.io.frontendTrigger.tUpdate.bits.tdata.tdata2.poke("h1234_567f".U)
        m.io.pc.zipWithIndex.map { case (pc, i) =>
          pc.poke((0x1234_5676 + 2 * i).U)
        }
        for (_ <- 0 until 4) { m.clock.step() }
        m.io.triggered.zipWithIndex.map { case (action, i) =>
          println(s"out.triggerAction${i}：" + action.peek().litValue.toString(16) + "\n")
        }
    }
    println("test done")
  }
}
