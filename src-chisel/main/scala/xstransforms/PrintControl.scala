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

package xstransforms

import firrtl._
import firrtl.ir._
import firrtl.options.Phase
import firrtl.stage.FirrtlCircuitAnnotation

import scala.collection.mutable

class PrintControl extends Phase {

  override def invalidates(a: Phase) = false

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {

    import xstransforms.Helpers._

    val disableList = annotations.collect {
      case DisablePrintfAnnotation(m) => m
    }
    val enableList = annotations.collect {
      case EnablePrintfAnnotation(m) => m
    }
    val disableAll = annotations.collectFirst {
      case DisableAllPrintAnnotation() => true
    }.nonEmpty
    val removeAssert = annotations.collectFirst{
      case RemoveAssertAnnotation() => true
    }.nonEmpty

    assert(!(enableList.nonEmpty && (disableAll || disableList.nonEmpty)))

    val (Seq(circuitAnno: FirrtlCircuitAnnotation), otherAnnos) = annotations.partition {
      case _: FirrtlCircuitAnnotation => true
      case _ => false
    }
    val c = circuitAnno.circuit

    val top = c.main
    val queue = new mutable.Queue[String]()
    val ancestors = new mutable.HashMap[String, mutable.LinkedHashSet[String]]()

    queue += top
    ancestors(top) = mutable.LinkedHashSet.empty

    while (queue.nonEmpty) {
      val curr = queue.dequeue()
      c.modules.find(m => m.name==curr).foreach(m => {
        def viewStmt(s: Statement): Statement = s match {
          case DefInstance(_, _, module, _) =>
            ancestors(module) = ancestors(curr) + m.name
            queue += module
            s
          case other =>
            other.mapStmt(viewStmt)
        }
        m.foreachStmt(viewStmt)
      })
    }

    def onModule(m: DefModule): DefModule = m match {
      case _: ExtModule => m
      case _: Module =>
        def inRange(seq: Seq[String]): Boolean = {
          seq.nonEmpty && (seq.contains(m.name) || seq.map(elm => {
            ancestors(m.name).contains(elm)
          }).reduce(_||_))
        }
        val enable = enableList.isEmpty || inRange(enableList)
        val disable = disableAll || inRange(disableList) || !enable
        def onStmt(s: Statement): Statement = s match {
          case _: Print if disable =>
            EmptyStmt
          case _: Stop if removeAssert => EmptyStmt
          case _: Verification if removeAssert => EmptyStmt
          case other => other.mapStmt(onStmt)
        }
        m.mapStmt(onStmt)
    }

    FirrtlCircuitAnnotation(c.mapModule(onModule)) +: otherAnnos
  }
}
