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

package xiangshan.transforms

class PrintModuleName extends firrtl.options.Phase {

  override def invalidates(a: firrtl.options.Phase) = false

  override def transform(annotations: firrtl.AnnotationSeq): firrtl.AnnotationSeq = {

    import xiangshan.transforms.Helpers._

    val (Seq(circuitAnno: firrtl.stage.FirrtlCircuitAnnotation), otherAnnos) = annotations.partition {
      case _: firrtl.stage.FirrtlCircuitAnnotation => true
      case _ => false
    }
    val c = circuitAnno.circuit

    def onStmt(s: firrtl.ir.Statement): firrtl.ir.Statement = s match {
      case firrtl.ir.Print(info, firrtl.ir.StringLit(string), args, clk, en) =>
        firrtl.ir.Print(info, firrtl.ir.StringLit(string.replace(utility.XSLog.MagicStr, "%m")), args, clk, en)
      case other: firrtl.ir.Statement =>
        other.mapStmt(onStmt)
    }

    firrtl.stage.FirrtlCircuitAnnotation(c.mapModule(m => m.mapStmt(onStmt))) +: otherAnnos
  }
}
