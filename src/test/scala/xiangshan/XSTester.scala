package xiangshan

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.simulator.HasSimulator
import svsim.CommonCompilationSettings
import svsim.CommonCompilationSettings.VerilogPreprocessorDefine
import svsim.verilator.Backend.CompilationSettings.{TraceKind, TraceStyle}
import org.scalatest.flatspec._
import org.scalatest.matchers.should._
import top.{ArgParser, DefaultConfig}
import xiangshan.backend.regfile.IntPregParams

abstract class XSTester extends AnyFlatSpec with ChiselSim with Matchers {
  behavior of "XiangShan Module"
  val defaultConfig = (new DefaultConfig)
  implicit val config: org.chipsalliance.cde.config.Parameters = defaultConfig.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy(
      // Example of how to change params
      intPreg = IntPregParams(
        numEntries = 64,
        numBank = 4,
        numRead = Some(14),
        numWrite = Some(8),
      ),
    )
  })
}

object XSTester {
  def verilatorWithVcd: HasSimulator = HasSimulator.simulators.verilator(
      compilationSettings = CommonCompilationSettings(
        verilogPreprocessorDefines = Seq(
          VerilogPreprocessorDefine("RANDOMIZE_REG_INIT"),
          VerilogPreprocessorDefine("RANDOMIZE_MEM_INIT"),
          VerilogPreprocessorDefine("RANDOMIZE_GARBAGE_ASSIGN")
        )
      ),
      verilatorSettings = svsim.verilator.Backend.CompilationSettings.default
        .withTraceStyle(Some(TraceStyle(TraceKind.Vcd)))
        .withOutputSplit(Some(30000))
        .withOutputSplitCFuncs(Some(30000))
        .withDisabledWarnings(Seq("STMTDLY", "WIDTH"))
    )
}
