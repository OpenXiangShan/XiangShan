package utils

import chisel3._
import MathUtils.IntToOH

object EnumUtils {
  class OHEnumeration() extends Enumeration {
    protected class OHVal(i: Int, name: String) extends super.Val(i, name) {
      def ohid: BigInt = IntToOH(id)
      def getName: String = name
    }
  }

  class ChiselOHEnum extends OHEnumeration {
    class OHType(i: Int, name: String) extends super.OHVal(i: Int, name: String)

    def OHType(i: Int, name: String): OHType = new OHType(i, name)

    implicit class fromOHValToLiteral(x: OHType) {
      def U: UInt = x.ohid.U
      def U(width: Width): UInt = x.ohid.U(width)
    }
    implicit def valueToOHType(x: Value): OHType = x.asInstanceOf[OHType]

    protected var initVal = 0

    protected def addType(name: String): OHType = {
      val ohval = OHType(initVal, name)
      initVal += 1
      ohval
    }

    object IsOneOf {
      def apply(ohtype: UInt, opt0: OHType, opts: OHType*): Bool = {
        apply(ohtype, opt0 +: opts)
      }

      def apply(ohtype: UInt, opts: Seq[OHType]): Bool = {
        opts.map(x => ohtype(x.id)).fold(false.B)(_ || _)
      }

      def apply(ohtype: OHType, opt0: OHType, opts: OHType*): Boolean = {
        apply(ohtype, opt0 +: opts)
      }

      def apply(ohtype: OHType, opts: Seq[OHType]): Boolean = {
        opts.map(x => x == ohtype).fold(false)(_ || _)
      }
    }
  }
}
