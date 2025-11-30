package xiangshan.backend.vector.util

import chisel3.RawModule
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import firrtl.{AnnotationSeq, EmittedVerilogCircuitAnnotation}

object Verilog {
  def emitVerilog(gen: => RawModule, args: Array[String] = Array.empty): String = {
    val annotations = Array(
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none",
      "--default-layer-specialization=enable",
    )
      .map(FirtoolOption.apply).toSeq

    (new ChiselStage)
      .execute(
        Array("--target", "systemverilog") ++ args,
        ChiselGeneratorAnnotation(() => gen) +: annotations
      )
      .collectFirst {
        case EmittedVerilogCircuitAnnotation(a) => a
      }
      .get
      .value
  }
}
