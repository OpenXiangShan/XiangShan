package xstransforms

import firrtl._
import firrtl.ir._
import top._

import scala.collection.mutable

class ShowPrintTransform extends Transform with DependencyAPIMigration {

  override def optionalPrerequisiteOf = firrtl.stage.Forms.MinimalHighForm
  override def invalidates(a: Transform) = true

  override protected def execute(state: CircuitState): CircuitState = {
    val c = state.circuit

    val blackList = state.annotations.collect {
      case DisablePrintfAnnotation(m) => m
    }
    val whiteList = state.annotations.collect {
      case EnablePrintfAnnotation(m) => m
    }
    val disableAll = state.annotations.collectFirst {
      case DisableAllPrintAnnotation() => true
    }.nonEmpty

    val removeAssert = state.annotations.collectFirst{
      case RemoveAssertAnnotation() => true
    }.nonEmpty

    assert(
      !(whiteList.nonEmpty && (disableAll || blackList.nonEmpty)),
      "'white list' can't be used with 'disable all' or 'black list'!"
    )

    val top = c.main
    val queue = new mutable.Queue[String]()
    val ancestors = new mutable.HashMap[String, mutable.LinkedHashSet[String]]()

    queue += top
    ancestors(top) = mutable.LinkedHashSet.empty

    def findSubModules(m: DefModule): Unit = {
      def viewStmt(s: Statement): Statement = s match {
        case DefInstance(_, name, module, _) =>
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
      def disableModulePrint(mod: DefModule) = {
        def disableStmtPrint(s: Statement): Statement = s match {
          case _: Print =>
            EmptyStmt
          case other =>
            other.mapStmt(disableStmtPrint)
        }
        mod.mapStmt(disableStmtPrint)
      }
      def removeModuleAssert(mod: DefModule)= {
        def removeStmtAssert(s: Statement): Statement = s match {
          case _: Stop =>
            EmptyStmt
          case other =>
            other.mapStmt(removeStmtAssert)
        }
        mod.mapStmt(removeStmtAssert)
      }

      val isInBlackList = blackList.nonEmpty && (
        blackList.contains(m.name) || blackList.map( b => ancestors(m.name).contains(b)).reduce(_||_)
      )
      val isInWhiteList = whiteList.isEmpty || (
        whiteList.nonEmpty && (whiteList.contains(m.name) || whiteList.map( x => ancestors(m.name).contains(x)).reduce(_||_))
      ) 
      val tmpMod = if(disableAll || isInBlackList || !isInWhiteList){
        disableModulePrint(m)
      } else {
        m
      }
      if(removeAssert) removeModuleAssert(tmpMod) else tmpMod
    }

    state.copy(c.mapModule(processModule))
  }
}
