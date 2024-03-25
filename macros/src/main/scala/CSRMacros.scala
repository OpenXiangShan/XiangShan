package xiangshan.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object CSRMacros {
  object CSRFieldsImpl {
    private def calcuWidth(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int]): Int = {
      import c.universe._

      val Literal(Constant(i_msb: Int)) = msb.tree
      val Literal(Constant(i_lsb: Int)) = lsb.tree
      i_msb - i_lsb + 1
    }

    def CSRROFieldRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], rfn: c.Tree): c.Tree = {
      import c.universe._

      val Literal(Constant(i_msb: Int)) = msb.tree
      val Literal(Constant(i_lsb: Int)) = lsb.tree
      c.parse(s"CSRDefines.CSRField${calcuWidth(c)(msb, lsb)}Bits.RO($i_msb, $i_lsb, $rfn)")
    }

    def CSRROFieldBit(c: Context)(bit: c.Expr[Int], rfn: c.Tree): c.Tree = {
      CSRROFieldRange(c)(bit, bit, rfn)
    }

    def CSRROFieldRangeNoFn(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int]): c.Tree = {
      CSRROFieldRange(c)(msb, lsb, null)
    }

    def CSRROFieldBitNoFn(c: Context)(bit: c.Expr[Int]): c.Tree = {
      CSRROFieldRange(c)(bit, bit, null)
    }

    def CSRWARLFieldRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], fn: c.Tree): c.Tree = {
      import c.universe._

      val Literal(Constant(i_msb: Int)) = msb.tree
      val Literal(Constant(i_lsb: Int)) = lsb.tree
      c.parse(s"CSRDefines.CSRField${calcuWidth(c)(msb, lsb)}Bits.WARL($i_msb, $i_lsb, $fn)")
    }

    def CSRWARLFieldBit(c: Context)(bit: c.Expr[Int], fn: c.Tree): c.Tree = {
      CSRWARLFieldRange(c)(bit, bit, fn)
    }

    def CSRRWFieldRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int]): c.Tree = {
      import c.universe._

      val Literal(Constant(i_msb: Int)) = msb.tree
      val Literal(Constant(i_lsb: Int)) = lsb.tree
      c.parse(s"CSRDefines.CSRField${calcuWidth(c)(msb, lsb)}Bits.RW($i_msb, $i_lsb)")
    }

    def CSRRWFieldBit(c: Context)(bit: c.Expr[Int]): c.Tree = {
      CSRRWFieldRange(c)(bit, bit)
    }

    def CSRWLRLFieldRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], fn: c.Tree): c.Tree = {
      import c.universe._

      val Literal(Constant(i_msb: Int)) = msb.tree
      val Literal(Constant(i_lsb: Int)) = lsb.tree
      c.parse(s"CSRDefines.CSRField${calcuWidth(c)(msb, lsb)}Bits.WARL($i_msb, $i_lsb, $fn)")
    }

    def CSRWLRLFieldBit(c: Context)(bit: c.Expr[Int], fn: c.Tree): c.Tree = {
      CSRWLRLFieldRange(c)(bit, bit, fn)
    }

    def CSRRefWARLFieldRange(c: Context)(ref: c.Tree, msb: c.Expr[Int], lsb: c.Expr[Int], wfn: c.Tree): c.Tree = {
      import c.universe._

      val Literal(Constant(i_msb: Int)) = msb.tree
      val Literal(Constant(i_lsb: Int)) = lsb.tree
      c.parse(s"CSRDefines.CSRField${calcuWidth(c)(msb, lsb)}Bits.RefWARL($ref, $i_msb, $i_lsb, $wfn)")
    }

    def CSRRefWARLFieldBit(c: Context)(ref: c.Tree, bit: c.Expr[Int], wfn: c.Tree): c.Tree = {
      CSRRefWARLFieldRange(c)(ref, bit, bit, wfn)
    }
  }
}


