package xstransforms

import firrtl._
import firrtl.ir._
import top.{DisableAllPrintAnnotation, DisablePrintfAnnotation}

import scala.collection.mutable

class ShowPrintTransform extends Transform with DependencyAPIMigration {

  override protected def execute(state: CircuitState): CircuitState = {
    val c = state.circuit

    val blackList = state.annotations.collect {
      case DisablePrintfAnnotation(m) => m
    }
    val disableAll = state.annotations.collectFirst {
      case DisableAllPrintAnnotation() => true
    }.nonEmpty

    val top = c.main
    val queue = new mutable.Queue[String]()
    val ancestors = new mutable.HashMap[String, mutable.LinkedHashSet[String]]()

    queue += top
    ancestors(top) = mutable.LinkedHashSet.empty

    def findSubModules(m: DefModule): Unit = {
      def viewStmt(s: Statement): Statement = s match {
        case DefInstance(_, name, module) =>
          ancestors(module) = ancestors(m.name) + m.name
          queue += module
          s
        case other =>
          other.mapStmt(viewStmt)
      }
      m.foreachStmt(viewStmt)
    }

    while (queue.nonEmpty) {
      val curr = queue.dequeue()
      c.modules.find(m => m.name==curr).map(findSubModules)
    }

    def processModule(m: DefModule): DefModule = {
      def disableModulePrint = {
        def disableStmtPrint(s: Statement): Statement = s match {
          case _: Print =>
            EmptyStmt
          case other =>
            other.mapStmt(disableStmtPrint)
        }
        m.mapStmt(disableStmtPrint)
      }
      if(
        disableAll ||
        blackList.nonEmpty &&
          (
            blackList.contains(m.name) ||
              blackList.map(b => ancestors(m.name).contains(b)).reduce(_||_)
          )
      ){
        disableModulePrint
      } else {
        m
      }
    }

    state.copy(c.mapModule(processModule))
  }
}