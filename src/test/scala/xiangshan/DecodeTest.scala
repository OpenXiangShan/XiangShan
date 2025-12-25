package xiangshan

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.simulator.scalatest.ChiselSim
import chisel3.simulator.HasSimulator
import svsim.CommonCompilationSettings
import svsim.CommonCompilationSettings.VerilogPreprocessorDefine
import svsim.verilator.Backend.CompilationSettings.{TraceKind, TraceStyle}
import top.ArgParser
import xiangshan.backend.decode.DecodeUnit
import xiangshan.backend.regfile.IntPregParams
import circt.stage.ChiselStage

object DecodeMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)
  // //val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(config)
  // If Complex Params are needed, wrap it with a Top Module to do dirty works,
  // and use "chisel3.aop.Select.collectDeep[ModuleWanted](WrapperModule){case a: ModuleWanted => a}.head.Params"
  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head.copy(
      // Example of how to change params
      intPreg = IntPregParams(
          numEntries = 64,
          numBank = 4,
          numRead = Some(14),
          numWrite = Some(8),
        ),
    )
  })
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => new DecodeUnit()(defaultConfig)
  )))
//  // Generate files when compiling. Used by ChiselDB.
//  FileRegisters.write("./build")
}

class DecodeUnitTest extends XSTester {
  behavior of "DecodeUnit"
  it should "pass" in {
    implicit val sim = XSTester.verilatorWithVcd
    simulate(new DecodeUnit()(config)) { dut =>
      enableWaves()
      dut.clock.step(10)
    }
  }
}
