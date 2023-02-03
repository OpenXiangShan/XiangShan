package xiangshan

import top.{ArgParser, Generator}
import xiangshan.backend.decode.DecodeUnit
import chisel3._
import chisel3.stage._
import chisel3.util._
//import xiangshan._
import utils._
import utility._
import system._
import device._
import chisel3.stage.ChiselGeneratorAnnotation
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.jtag.JTAGIO
import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils, UIntToOH1}

object DecodeMain extends App with HasRocketChipStageUtils {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    // //val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(config)
    // If Complex Params are needed, wrap it with a Top Module to do dirty works,
    // and use "chisel3.aop.Select.collectDeep[ModuleWanted](WrapperModule){case a: ModuleWanted => a}.head.Params"
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => new DecodeUnit()(config.alterPartial({
        // Get XSCoreParams and pass it to the "small module"
        case XSCoreParamsKey => config(XSTileKey).head.copy(
          // Example of how to change params
          IssQueSize = 12
        )
      }))
    )))
//    // Generate files when compiling. Used by ChiselDB.
//    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
//      writeOutputFile("./build", s"DecodeUnit.${extension}", contents())
//    }
  }
}
