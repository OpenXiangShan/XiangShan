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

case class DisablePrintfAnnotation(m: String) extends firrtl.annotations.NoTargetAnnotation
object DisablePrintfAnnotation extends firrtl.options.HasShellOptions{

  val options = Seq(
    new firrtl.options.ShellOption[String](
      longOption = "disable-module-print",
      toAnnotationSeq = s => Seq(DisablePrintfAnnotation(s)),
      helpText =
        "The verilog 'printf' in the <module> and it's submodules will be removed\n",
      shortOption = Some("dm"),
      helpValueName = Some("<module>")
    )
  )

}

case class EnablePrintfAnnotation(m: String) extends firrtl.annotations.NoTargetAnnotation
object EnablePrintfAnnotation extends firrtl.options.HasShellOptions {
  val options = Seq(
    new firrtl.options.ShellOption[String](
      longOption = "enable-module-print",
      toAnnotationSeq = s => Seq(EnablePrintfAnnotation(s)),
      helpText =
        "The verilog 'printf' except the <module> and it's submodules will be removed\n",
      shortOption = Some("em"),
      helpValueName = Some("<module>")
    )
  )

}

case class DisableAllPrintAnnotation() extends firrtl.annotations.NoTargetAnnotation
object DisableAllPrintAnnotation extends firrtl.options.HasShellOptions {
  val options = Seq(
    new firrtl.options.ShellOption[Unit](
      longOption = "disable-all",
      toAnnotationSeq = _ => Seq(DisableAllPrintAnnotation()),
      helpText =
        "All the verilog 'printf' will be removed\n",
      shortOption = Some("dall")
    )
  )
}

case class RemoveAssertAnnotation() extends firrtl.annotations.NoTargetAnnotation
object RemoveAssertAnnotation extends firrtl.options.HasShellOptions{
  val options = Seq(
    new firrtl.options.ShellOption[Unit](
      longOption = "remove-assert",
      toAnnotationSeq = _ => Seq(RemoveAssertAnnotation()),
      helpText = "All the 'assert' will be removed\n",
      shortOption = None
    )
  )
}

import scala.collection.mutable

class PrintControl extends firrtl.options.Phase {

  override def invalidates(a: firrtl.options.Phase) = false

  override def transform(annotations: firrtl.AnnotationSeq): firrtl.AnnotationSeq = {

    import xiangshan.transforms.Helpers._

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

    val (Seq(circuitAnno: firrtl.stage.FirrtlCircuitAnnotation), otherAnnos) = annotations.partition {
      case _: firrtl.stage.FirrtlCircuitAnnotation => true
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
        def viewStmt(s: firrtl.ir.Statement): firrtl.ir.Statement = s match {
          case firrtl.ir.DefInstance(_, _, module, _) =>
            ancestors(module) = ancestors(curr).union(Set(m.name))
            queue += module
            s
          case other =>
            other.mapStmt(viewStmt)
        }
        m.foreachStmt(viewStmt)
      })
    }

    def onModule(m: firrtl.ir.DefModule): firrtl.ir.DefModule = m match {
      case _: firrtl.ir.ExtModule => m
      case _: firrtl.ir.Module =>
        def inRange(seq: Seq[String]): Boolean = {
          seq.nonEmpty && (seq.contains(m.name) || seq.map(elm => {
            ancestors(m.name).contains(elm)
          }).reduce(_||_))
        }
        val enable = enableList.isEmpty || inRange(enableList)
        val disable = disableAll || inRange(disableList) || !enable
        def onStmt(s: firrtl.ir.Statement): firrtl.ir.Statement = s match {
          case _: firrtl.ir.Print if disable => firrtl.ir.EmptyStmt
          case _: firrtl.ir.Stop if removeAssert => firrtl.ir.EmptyStmt
          case _: firrtl.ir.Verification if removeAssert => firrtl.ir.EmptyStmt
          case other => other.mapStmt(onStmt)
        }
        m.mapStmt(onStmt)
    }

    firrtl.stage.FirrtlCircuitAnnotation(c.mapModule(onModule)) +: otherAnnos
  }
}
