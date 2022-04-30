package xstransforms

import firrtl._
import firrtl.annotations.{ModuleTarget, NoTargetAnnotation}
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency
import firrtl.passes.memlib.{InferReadWrite, ReplSeqMem, GenVerilogMemBehaviorModelAnno}

case class ModulePrefixAnnotation(prefix: String) extends NoTargetAnnotation

class AddModulePrefix extends Transform with DependencyAPIMigration {

  override def prerequisites: Seq[TransformDependency] = Seq(
    Dependency[InferReadWrite],
    Dependency[ReplSeqMem]
  ) ++ Forms.LowForm
  override def optionalPrerequisites:  Seq[TransformDependency] = Forms.LowFormOptimized
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Forms.LowEmitters
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: CircuitState): CircuitState = {
    val c = state.circuit

    val prefixOpt = state.annotations.collectFirst {
      case ModulePrefixAnnotation(p) => p
    }

    if (prefixOpt.isEmpty){ return state }

    val prefix = prefixOpt.get

    def rename(old: String): String = prefix + old

    val renameMap = RenameMap()

    def onStmt(s: Statement): Statement = s match {
      case DefInstance(info, name, module, tpe) =>
        DefInstance(info, name, rename(module), tpe)
      case other =>
        other.mapStmt(onStmt)
    }

    def onModule(m: DefModule): DefModule = {
      val newName = rename(m.name)
      renameMap.record(
        ModuleTarget(c.main, m.name), ModuleTarget(c.main, newName)
      )
      m match {
        case mod@Module(info, name, ports, body) =>
          mod.copy(name = newName).mapStmt(onStmt)
        case extMod@ExtModule(info, name, ports, defname, params) =>
          extMod.copy(name = newName, defname = newName)
      }
    }

    val newCircuit = c.mapModule(onModule)
    state.copy(
      circuit = newCircuit.copy(main = rename(c.main)),
      renames = Some(renameMap)
    )
  }
}
