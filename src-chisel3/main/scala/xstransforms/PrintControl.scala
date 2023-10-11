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
import firrtl.annotations.NoTargetAnnotation
import firrtl.ir._
import firrtl.options.{HasShellOptions, ShellOption}

import scala.collection.mutable

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

class PrintControl extends Transform with DependencyAPIMigration {

  override def optionalPrerequisiteOf = firrtl.stage.Forms.MinimalHighForm
  override def invalidates(a: Transform) = true

  override protected def execute(state: CircuitState): CircuitState = {

    val disableList = state.annotations.collect {
      case DisablePrintfAnnotation(m) => m
    }
    val enableList = state.annotations.collect {
      case EnablePrintfAnnotation(m) => m
    }
    val disableAll = state.annotations.collectFirst {
      case DisableAllPrintAnnotation() => true
    }.nonEmpty
    val removeAssert = state.annotations.collectFirst{
      case RemoveAssertAnnotation() => true
    }.nonEmpty

    assert(!(enableList.nonEmpty && (disableAll || disableList.nonEmpty)))

    val c = state.circuit

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
    state.copy(circuit = c.mapModule(onModule))
  }
}
