// Hacked from CIRCT source code. Look like some SiFive internal annotations.
package sifive.enterprise.firrtl

case class NestedPrefixModulesAnnotation(
  target: firrtl.annotations.ModuleTarget,
  prefix: String,
  inclusive: Boolean = false,
) extends firrtl.annotations.SingleTargetAnnotation[firrtl.annotations.ModuleTarget] {
  def duplicate(n: firrtl.annotations.ModuleTarget): NestedPrefixModulesAnnotation = this.copy(n)
}
