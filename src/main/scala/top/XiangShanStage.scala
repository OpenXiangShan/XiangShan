package top

import chisel3.stage.ChiselCli
import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, Shell, ShellOption}
import firrtl.stage.{FirrtlCli, RunFirrtlTransformAnnotation}
import freechips.rocketchip.transforms.naming.{OverrideDesiredNameAnnotation, RenameDesiredNames}
import xstransforms.ShowPrintTransform
import xstransforms.PrintModuleName

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

case class EnablePrintfAnnotation(m: String) extends NoTargetAnnotation

object EnablePrintfAnnotation extends HasShellOptions {
  val options = Seq(
    new ShellOption[String](
      longOption = "enable-module-print",
      toAnnotationSeq = s => Seq(EnablePrintfAnnotation(s)),
      helpText =
        "The verilog 'printf' except the <module> and it's submodules will be removed\n",
      shortOption = Some("em"),
      helpValueName = Some("<module>")
    )
  )

}

case class DisableAllPrintAnnotation() extends NoTargetAnnotation

object DisableAllPrintAnnotation extends HasShellOptions {
  val options = Seq(
    new ShellOption[Unit](
      longOption = "disable-all",
      toAnnotationSeq = _ => Seq(DisableAllPrintAnnotation()),
      helpText =
        "All the verilog 'printf' will be removed\n",
      shortOption = Some("dall")
    )
  )
}

case class RemoveAssertAnnotation() extends NoTargetAnnotation

object RemoveAssertAnnotation extends HasShellOptions{
  val options = Seq(
    new ShellOption[Unit](
      longOption = "remove-assert",
      toAnnotationSeq = _ => Seq(RemoveAssertAnnotation()),
      helpText = "All the 'assert' will be removed\n",
      shortOption = None
    )
  )
}

trait XiangShanCli { this: Shell =>
  parser.note("XiangShan Options")
  DisablePrintfAnnotation.addOptions(parser)
  EnablePrintfAnnotation.addOptions(parser)
  DisableAllPrintAnnotation.addOptions(parser)
  RemoveAssertAnnotation.addOptions(parser)
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
      annotations ++ Seq(
        RunFirrtlTransformAnnotation(new ShowPrintTransform),
        RunFirrtlTransformAnnotation(new PrintModuleName),
        RunFirrtlTransformAnnotation(new RenameDesiredNames)
      )
    )
  }
}
