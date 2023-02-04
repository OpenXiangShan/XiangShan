package xiangshan

import chisel3._
import chisel3.stage._
import chiseltest._
import chiseltest.ChiselScalatestTester
import chiseltest.VerilatorBackendAnnotation
import chiseltest.simulator.{VerilatorFlags, VerilatorCFlags}
import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import firrtl.stage.RunFirrtlTransformAnnotation
import xstransforms.PrintModuleName

import firrtl.options.TargetDirAnnotation

import top.ArgParser
import xiangshan.backend.decode.DecodeUnit

object DecodeMain extends App with HasRocketChipStageUtils {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    // //val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(config)
    // If Complex Params are needed, wrap it with a Top Module to do dirty works,
    // and use "chisel3.aop.Select.collectDeep[ModuleWanted](WrapperModule){case a: ModuleWanted => a}.head.Params"
    val defaultConfig = config.alterPartial({
      // Get XSCoreParams and pass it to the "small module"
      case XSCoreParamsKey => config(XSTileKey).head.copy(
        // Example of how to change params
        IssQueSize = 12
      )
    })
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => new DecodeUnit()(defaultConfig)
    )))
//    // Generate files when compiling. Used by ChiselDB.
//    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
//      writeOutputFile("./build", s"DecodeUnit.${extension}", contents())
//    }
  }
}

class DecodeUnitTest extends XSTester {
  behavior of "DecodeUnit"
  it should "pass" in {
    test(new DecodeUnit()(config)).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      WriteVcdAnnotation,
      TargetDirAnnotation("./build"),
      RunFirrtlTransformAnnotation(new PrintModuleName)
    )){ dut =>
      dut.clock.step(10)
    }
  }
}