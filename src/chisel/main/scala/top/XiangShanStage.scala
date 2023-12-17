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

import chisel3.stage._
import firrtl.stage._
import firrtl.options.Shell
import xiangshan.transforms._
import firrtl.options.Stage
import firrtl.options.Phase
import circt.stage.CLI
import firrtl.AnnotationSeq
import firrtl.options.PhaseManager
import firrtl.options.Dependency
import circt.stage.ChiselStage


class XiangShanStage extends ChiselStage {

  override val shell = new Shell("xiangshan") with CLI with XiangShanCli

  trait XiangShanCli { this: Shell =>
    parser.note("XiangShan Options")
    DisablePrintfAnnotation.addOptions(parser)
    EnablePrintfAnnotation.addOptions(parser)
    DisableAllPrintAnnotation.addOptions(parser)
    RemoveAssertAnnotation.addOptions(parser)
  }

  override def run(annotations: AnnotationSeq): AnnotationSeq = {

    val pm = new PhaseManager(
      targets = Seq(
        Dependency[chisel3.stage.phases.Checks],
        Dependency[chisel3.stage.phases.AddImplicitOutputFile],
        Dependency[chisel3.stage.phases.AddImplicitOutputAnnotationFile],
        Dependency[chisel3.stage.phases.MaybeAspectPhase],
        Dependency[chisel3.stage.phases.AddSerializationAnnotations],
        Dependency[chisel3.stage.phases.Convert],
        Dependency[xiangshan.transforms.PrintModuleName],
        Dependency[xiangshan.transforms.PrintControl],
        Dependency[chisel3.stage.phases.MaybeInjectingPhase],
        Dependency[circt.stage.phases.AddImplicitOutputFile],
        Dependency[circt.stage.phases.Checks],
        Dependency[circt.stage.phases.CIRCT]
      ),
      currentState = Seq(
        Dependency[firrtl.stage.phases.AddDefaults],
        Dependency[firrtl.stage.phases.Checks]
      )
    )
    pm.transform(annotations)
  }

}
