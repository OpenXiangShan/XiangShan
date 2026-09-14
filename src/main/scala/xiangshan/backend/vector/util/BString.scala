package xiangshan.backend.vector.util

import chisel3._
import chisel3.util.BitPat

object BString {
  implicit class BinaryStringHelper(private val sc: StringContext) extends AnyVal {
    def b(args: Any*): UInt = {
      val str: String = StringContext.standardInterpolator(x => x, args, sc.parts)
        .filterNot(x => x == ' ' || x == '_')
      require(str.forall(c => c == '0' || c == '1'))
      ("b" + str).U(str.length.W)
    }

    /**
     * Create fixed width UInt by binary string
     */
    def ub(args: Any*): UInt = {
      val str: String = StringContext.standardInterpolator(x => x, args, sc.parts)
        .filterNot(x => x == ' ' || x == '_')
      require(str.forall(c => c == '0' || c == '1'))
      ("b" + str).U(str.length.W)
    }

    /**
     * Create fixed width BitPat by binary string
     */
    def bb(args: Any*): BitPat = {
      val str: String = StringContext.standardInterpolator(x => x, args, sc.parts)
        .filterNot(x => x == ' ' || x == '_')
      require(str.forall(c => c == '0' || c == '1' || c == '?'))
      BitPat("b" + str)
    }
  }
}
