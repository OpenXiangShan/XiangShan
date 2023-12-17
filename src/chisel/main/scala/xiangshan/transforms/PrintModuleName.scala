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

package xiangshan.transforms

import firrtl._
import firrtl.ir._
import utils.XSLog
import firrtl.options.Phase
import firrtl.stage.FirrtlCircuitAnnotation

class PrintModuleName extends Phase {

  override def invalidates(a: Phase) = false

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {

    import xiangshan.transforms.Helpers._

    val (Seq(circuitAnno: FirrtlCircuitAnnotation), otherAnnos) = annotations.partition {
      case _: FirrtlCircuitAnnotation => true
      case _ => false
    }
    val c = circuitAnno.circuit

    def onStmt(s: Statement): Statement = s match {
      case Print(info, StringLit(string), args, clk, en) =>
        Print(info, StringLit(string.replace(XSLog.MagicStr, "%m")), args, clk, en)
      case other: Statement =>
        other.mapStmt(onStmt)
    }

    FirrtlCircuitAnnotation(c.mapModule(m => m.mapStmt(onStmt))) +: otherAnnos
  }
}
