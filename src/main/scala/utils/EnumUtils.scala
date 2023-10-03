package utils

import MathUtils.IntToOH

object EnumUtils {
  class OHEnumeration() extends Enumeration {
    protected class OHVal(i: Int, name: String) extends super.Val(i, name) {
      def ohid: BigInt = IntToOH(id)
    }
  }
}
