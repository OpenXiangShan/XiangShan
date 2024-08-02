/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import xiangshan.transforms._
import circt.stage.CLI
import circt.stage.ChiselStage


class XiangShanStage extends ChiselStage {

  override val shell = new firrtl.options.Shell("xiangshan") with CLI with XiangShanCli {
    // These are added by firrtl.options.Shell (which we must extend because we are a Stage)
    override protected def includeLoggerOptions = false
  }

  trait XiangShanCli { this: firrtl.options.Shell =>
    parser.note("XiangShan Options")
    DisablePrintfAnnotation.addOptions(parser)
    EnablePrintfAnnotation.addOptions(parser)
    DisableAllPrintAnnotation.addOptions(parser)
    RemoveAssertAnnotation.addOptions(parser)
  }

  override def run(annotations: firrtl.AnnotationSeq): firrtl.AnnotationSeq = {

    val pm = new firrtl.options.PhaseManager(
      targets = Seq(
        firrtl.options.Dependency[chisel3.stage.phases.AddImplicitOutputFile],
        firrtl.options.Dependency[chisel3.stage.phases.AddImplicitOutputAnnotationFile],
        firrtl.options.Dependency[chisel3.stage.phases.MaybeAspectPhase],
        firrtl.options.Dependency[chisel3.stage.phases.AddSerializationAnnotations],
        firrtl.options.Dependency[chisel3.stage.phases.Convert],
        firrtl.options.Dependency[xiangshan.transforms.PrintModuleName],
        firrtl.options.Dependency[xiangshan.transforms.PrintControl],
        firrtl.options.Dependency[chisel3.stage.phases.AddDedupGroupAnnotations],
        firrtl.options.Dependency[chisel3.stage.phases.MaybeInjectingPhase],
        firrtl.options.Dependency[circt.stage.phases.AddImplicitOutputFile],
        firrtl.options.Dependency[circt.stage.phases.CIRCT]
      ),
      currentState = Seq(
        firrtl.options.Dependency[firrtl.stage.phases.AddDefaults],
        firrtl.options.Dependency[firrtl.stage.phases.Checks]
      )
    )
    pm.transform(annotations)
  }

}
