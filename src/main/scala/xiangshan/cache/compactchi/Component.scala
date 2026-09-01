package oceanus.compactchi

object CCHIComponentType extends Enumeration {
  protected case class ComponentTypeValue(name: String, canonicalName: String, coherent: Boolean, coherentUnique: Boolean) extends super.Val {
    def isCoherent = coherent
    def takeUnique = coherent && coherentUnique
  }
  type Value = ComponentTypeValue

  val FullyCoherent       = ComponentTypeValue("Fully coherent"         , "Type 1", true , true ) // Type 1
  val ReadOnlyCoherent    = ComponentTypeValue("Read-only coherent"     , "Type 2", true , false) // Type 2
  val Noncoherent         = ComponentTypeValue("Non-coherent"           , "Type 3", false, false) // Type 3
  val ReadOnlyNoncoherent = ComponentTypeValue("Read-only Non-coherent" , "Type 4", false, false) // Type 4
  val Prefetcher          = ComponentTypeValue("Prefetcher"             , "Type 5", false, false) // Type 5
}

class CCHIComponent(val componentType: CCHIComponentType.Value, val componentName: String)
