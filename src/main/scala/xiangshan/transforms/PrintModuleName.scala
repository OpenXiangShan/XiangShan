/***************************************************************************************
* Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package chisel3.stage.phases.xiangshan

import chisel3._
import chisel3.stage.ChiselCircuitAnnotation
import chisel3.stage.phases.Elaborate
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase}

@scala.annotation.nowarn("msg=All APIs in package firrtl are deprecated")
class PrintModuleName extends Phase {

  import chisel3.internal.firrtl.xiangshan.ChiselCircuitHelpers._
  import chisel3.internal.firrtl.ir._

  override def prerequisites = Seq(Dependency[Elaborate])
  override def invalidates(a: Phase) = false

  def transform(annotations: AnnotationSeq): AnnotationSeq = {

    def onCommand(c: Command): Command = c match {
      case Printf(id, sourceInfo, filename, clock, pable) =>
        val (fmt, data) = pable.unpack
        val newPable = Printable.pack(utility.XSLog.replaceFIRStr(fmt), data:_*)
        Printf(id, sourceInfo, filename, clock, newPable)
      case other: Command => other.mapCommand(onCommand)
    }

    annotations.flatMap {
      case a: ChiselCircuitAnnotation =>
        Some(ChiselCircuitAnnotation(ElaboratedCircuit(
          a.elaboratedCircuit._circuit.mapComponent(c => c.mapCommand(onCommand)), Seq()
        )))
      case a => Some(a)
    }
  }
}
