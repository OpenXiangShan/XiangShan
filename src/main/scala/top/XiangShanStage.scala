package top

import chisel3.stage.ChiselCli
import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, Shell, ShellOption}
import firrtl.stage.{FirrtlCli, RunFirrtlTransformAnnotation}
import xstransforms.ShowPrintTransform

case class DisablePrintfAnnotation(m: String) extends NoTargetAnnotation

object DisablePrintfAnnotation extends HasShellOptions{

  val options = Seq(
    new ShellOption[String](
      longOption = "disable-module-print",
      toAnnotationSeq = s => Seq(DisablePrintfAnnotation(s)),
      helpText =
        "The verilog 'printf' in the <module> and it's submodules will be removed\n",
      shortOption = Some("dm"),
      helpValueName = Some("<module>")
    )
  )

}

case class DisableAllPrintAnnotation() extends NoTargetAnnotation

object DisableAllPrintAnnotation extends HasShellOptions {
  val options = Seq(
    new ShellOption[Unit](
      longOption = "disable-log",
      toAnnotationSeq = _ => Seq(DisableAllPrintAnnotation()),
      helpText =
        "All the verilog 'printf' will be removed\n",
      shortOption = Some("dall")
    )
  )
}

trait XiangShanCli { this: Shell =>
  parser.note("XiangShan Options")
  DisablePrintfAnnotation.addOptions(parser)
  DisableAllPrintAnnotation.addOptions(parser)
}

class XiangShanStage extends chisel3.stage.ChiselStage {
  override val shell: Shell = new Shell("xiangshan")
    with XiangShanCli
    with ChiselCli
    with FirrtlCli
}

object XiangShanStage {
  def execute
  (
    args: Array[String],
    annotations: AnnotationSeq
  ): AnnotationSeq = {
    (new XiangShanStage).execute(
      args,
      annotations :+ RunFirrtlTransformAnnotation(new ShowPrintTransform)
    )
  }
}
