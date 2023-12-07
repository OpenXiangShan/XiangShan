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

import circt.stage._
import chisel3.stage.ChiselGeneratorAnnotation
import xiangshan.types.RunFirrtlTransformAnnotation
import xiangshan.transforms._

object Generator {
  def execute(args: Array[String], mod: => chisel3.RawModule, firtoolOpts: Array[String]) = {
    val annotations = chisel3.BuildInfo.version match {
      case "3.6.0" => Seq(
        RunFirrtlTransformAnnotation(new PrintControl),
        RunFirrtlTransformAnnotation(new PrintModuleName)
      )
      case _ => Seq(CIRCTTargetAnnotation(CIRCTTarget.Verilog)) ++ firtoolOpts.map(FirtoolOption.apply)
    }

    (new XiangShanStage).execute(args, ChiselGeneratorAnnotation(mod _) +: annotations)
  }
}
