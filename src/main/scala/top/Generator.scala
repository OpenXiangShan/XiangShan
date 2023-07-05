/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package top

import chisel3.RawModule
import chisel3.stage.{ChiselCli, ChiselGeneratorAnnotation}
import firrtl.options.Shell
import firrtl.stage.{FirrtlCli, RunFirrtlTransformAnnotation}
import freechips.rocketchip.transforms.naming.RenameDesiredNames
import xstransforms._

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

abstract class FirrtlCompiler
case object SFC extends FirrtlCompiler
case object MFC extends FirrtlCompiler

object Generator {

  def execute(args: Array[String], mod: => RawModule, fc: FirrtlCompiler, firtoolOpts: Array[String]) = {
    fc match {
      case MFC =>
        val sfcXsTransforms = Seq(
          DisablePrintfAnnotation,
          EnablePrintfAnnotation,
          DisableAllPrintAnnotation,
          RemoveAssertAnnotation
        )
        val sfcOptions = sfcXsTransforms.flatMap(_.options.map(_.longOption)) ++
          sfcXsTransforms.flatMap(_.options.flatMap(_.shortOption))
        val mfcArgs = args.filter(s => {
          val option_s = if(s.startsWith("--")){
            s.replace("--", "")
          } else if(s.startsWith("-")){
            s.replace("-", "")
          } else s
          val cond = sfcOptions.contains(option_s)
          if(cond){
            println(s"[Warnning] SFC Transform Option ${s} will be removed in MFC!")
          }
          !cond
        })
        (new circt.stage.ChiselStage).execute(mfcArgs, Seq(
          ChiselGeneratorAnnotation(mod _),
          circt.stage.CIRCTTargetAnnotation(circt.stage.CIRCTTarget.Verilog)
        ) ++ firtoolOpts.map(opt => circt.stage.FirtoolOption(opt)))
      case SFC =>
        (new XiangShanStage).execute(args, Seq(
          ChiselGeneratorAnnotation(mod _),
          RunFirrtlTransformAnnotation(new PrintControl),
          RunFirrtlTransformAnnotation(new PrintModuleName),
          RunFirrtlTransformAnnotation(new RenameDesiredNames)
        ))
      case _ =>
        assert(false, s"Unknown firrtl compiler: ${fc.getClass.getName}!")
    }
  }

}
