package xstransforms

import firrtl._
import firrtl.ir._

class ShowPrintTransform extends Transform {

  override def inputForm: CircuitForm = ChirrtlForm

  override def outputForm: CircuitForm = ChirrtlForm

  override protected def execute(state: CircuitState): CircuitState = {
    val c = state.circuit
    val modules = scala.collection.mutable.ArrayBuffer[String]()

    def containsPrint(s: Statement): Boolean = s match {
      case p: Print => true
      case b: Block =>
        for (st <- b.stmts) if (containsPrint(st)) return true
        false
      case cond: Conditionally =>
        if (containsPrint(cond.conseq)) return true
        if (containsPrint(cond.alt)) return true
        false
      case _ => false
    }

    for (m <- c.modules) {
      m match {
        case chiselModule: Module =>
          if (containsPrint(chiselModule.body)) {
            var en = true
            var flag = true
            while (flag) {
              val str = scala.io.StdIn.readLine(s"Enable printf in [${m.name}]? Press Y(y)/N(n):")
              if (str.isEmpty || str.toLowerCase.contains("y")) {
                flag = false
              } else if (str.toLowerCase().contains("n")) {
                flag = false
                en = false
              }
            }
            modules += m.name
          }
        case _ => // do nothing
      }
    }

    def disableModulePrintf(m: Module): DefModule = {
      def disableStmtPrintf(s: Statement): Statement = s match {
        case print: Print =>
          EmptyStmt
        case other =>
          other.mapStmt(disableStmtPrintf)
      }
      m.mapStmt(disableStmtPrintf)
    }

    def processModule(m: DefModule): DefModule = m match {
      case chiselModule: Module =>
        if (modules.contains(chiselModule.name)) chiselModule
        else disableModulePrintf(chiselModule)
      case otherModule =>
        otherModule
    }

    state.copy(c.mapModule(processModule))
  }
}