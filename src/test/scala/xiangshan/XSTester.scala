package xiangshan

import chisel3._
import chipsalliance.rocketchip.config.Config
import chiseltest._
import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import chiseltest.simulator.{VerilatorCFlags, VerilatorFlags}
import firrtl.AnnotationSeq
import firrtl.stage.RunFirrtlTransformAnnotation
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

import top.{ArgParser, DefaultConfig}

abstract class XSTester extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {
  behavior of "XiangShan Module"
  val defaultConfig = (new DefaultConfig)
  implicit val config = defaultConfig.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy(
      // Example of how to change params
      IssQueSize = 12
    )
  })
}

trait HasTestAnnos {
  var testAnnos: AnnotationSeq = Seq()
}

trait DumpVCD { this: HasTestAnnos =>
  testAnnos = testAnnos :+ WriteVcdAnnotation
}

trait UseVerilatorBackend { this: HasTestAnnos =>
  testAnnos = testAnnos ++ Seq(VerilatorBackendAnnotation)
}