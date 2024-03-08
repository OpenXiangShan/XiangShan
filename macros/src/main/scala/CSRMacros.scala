package xiangshan.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object CSRMacros {
  object CSRFieldsImpl {
    private def helper(str: String)(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], wfn: c.Tree): c.Tree = {
      import c.universe._

      val Literal(Constant(i_msb: Int)) = msb.tree
      val Literal(Constant(i_lsb: Int)) = lsb.tree
      val w = Literal(Constant(i_msb - i_lsb + 1))
      c.parse(s"CSRDefines.CSR${str}Field${w}Bits($i_msb, $i_lsb, $wfn)")
    }

    def CSRFieldWARLBitsRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], wfn: c.Tree): c.Tree = {
      this.helper("WARL")(c)(msb, lsb, wfn)
    }

    def CSRFieldWARLBitsBit(c: Context)(bit: c.Expr[Int], wfn: c.Tree): c.Tree = {
      this.helper("WARL")(c)(bit, bit, wfn)
    }

    def CSRFieldROBitsRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], rfn: c.Tree): c.Tree = {
      this.helper("RO")(c)(msb, lsb, rfn)
    }

    def CSRFieldROBitsBit(c: Context)(bit: c.Expr[Int], rfn: c.Tree): c.Tree = {
      this.helper("RO")(c)(bit, bit, rfn)
    }

    private def refHelper(str: String)(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], ref: c.Tree,  rfn: c.Tree): c.Tree = {
      import c.universe._

      val Literal(Constant(i_msb: Int)) = msb.tree
      val Literal(Constant(i_lsb: Int)) = lsb.tree
      val w = Literal(Constant(i_msb - i_lsb + 1))
      c.parse(s"CSRDefines.CSR${str}Field${w}Bits($i_msb, $i_lsb, $ref, $rfn)")
    }

    def CSRFieldRefBitsRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], ref: c.Tree, rfn: c.Tree): c.Tree = {
      this.refHelper("Ref")(c)(msb, lsb, ref, rfn)
    }

    def CSRFieldRefBitsBit(c: Context)(bit: c.Expr[Int], ref: c.Tree, rfn: c.Tree): c.Tree = {
      this.refHelper("Ref")(c)(bit, bit, ref, rfn)
    }

    def CSRFieldRefROBitsRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], ref: c.Tree, rfn: c.Tree): c.Tree = {
      this.refHelper("RefRO")(c)(msb, lsb, ref, rfn)
    }

    def CSRFieldRefROBitsBit(c: Context)(bit: c.Expr[Int], ref: c.Tree, rfn: c.Tree): c.Tree = {
      this.refHelper("RefRO")(c)(bit, bit, ref, rfn)
    }

  }
}


