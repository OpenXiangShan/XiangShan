// Hacked from CIRCT source code. Look like some SiFive internal annotations.
package sifive.enterprise.firrtl

import firrtl.annotations.{ModuleTarget, SingleTargetAnnotation}

case class NestedPrefixModulesAnnotation(
  target: ModuleTarget,
  prefix: String,
  inclusive: Boolean = false,
) extends SingleTargetAnnotation[ModuleTarget] {
  def duplicate(n: ModuleTarget): NestedPrefixModulesAnnotation = this.copy(n)
}
