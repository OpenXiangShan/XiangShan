package xstransforms

import firrtl._
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.passes.wiring.WiringTransform
import firrtl.stage.TransformManager.TransformDependency
import utils.XSLog

class PrintModuleName extends Transform with DependencyAPIMigration {

  // avoid print's check
  override def prerequisites = firrtl.stage.Forms.Checks
  override def invalidates(a: Transform) = false
  override def optionalPrerequisites = Seq(Dependency[WiringTransform])

  override protected def execute(state: CircuitState): CircuitState = {

    val c = state.circuit

    def onStmt(s: Statement): Statement = s match {
      case Print(info, StringLit(string), args, clk, en) => 
        Print(info, StringLit(string.replace(XSLog.MagicStr, "%m")), args, clk, en)
      case other: Statement =>
        other.mapStmt(onStmt)
    }

    state.copy(c.mapModule(m => m.mapStmt(onStmt)))

  }

}
