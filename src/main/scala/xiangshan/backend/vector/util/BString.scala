package xiangshan.backend.vector.util

import chisel3._

object BString {
  implicit class BinaryStringHelper(private val sc: StringContext) extends AnyVal {
    def b(args: Any*): UInt = {
      val str: String = sc.standardInterpolator(x => x, args)
        .filterNot(x => x == ' ' || x == '_')
      require(str.forall(c => c == '0' || c == '1'))
      ("b" + str).U(str.length.W)
    }
  }
}
