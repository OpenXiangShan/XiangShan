package xiangshan.macros

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object CSRMacros {
  object CSRFieldsImpl {
    private def calcuWidth(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int]): Int = {
      import c.universe._

      val i_msb = c.eval(msb)
      val i_lsb = c.eval(lsb)

      i_msb - i_lsb + 1
    }

    @compileTimeOnly("")
    def CSRROFieldRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], rfn: c.Tree): c.Tree = {
      c.parse(s"CSRDefines.CSRField${calcuWidth(c)(msb, lsb)}Bits.RO(${c.eval(msb)}, ${c.eval(lsb)}, $rfn)")
    }

    @compileTimeOnly("")
    def CSRROFieldBit(c: Context)(bit: c.Expr[Int], rfn: c.Tree): c.Tree = {
      CSRROFieldRange(c)(bit, bit, rfn)
    }

    @compileTimeOnly("")
    def CSRROFieldRangeNoFn(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int]): c.Tree = {
      CSRROFieldRange(c)(msb, lsb, null)
    }

    @compileTimeOnly("")
    def CSRROFieldBitNoFn(c: Context)(bit: c.Expr[Int]): c.Tree = {
      CSRROFieldRange(c)(bit, bit, null)
    }

    @compileTimeOnly("")
    def CSRWARLFieldRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], fn: c.Tree): c.Tree = {
      c.parse(s"CSRDefines.CSRField${calcuWidth(c)(msb, lsb)}Bits.WARL(${c.eval(msb)}, ${c.eval(lsb)}, $fn)")
    }

    @compileTimeOnly("")
    def CSRWARLFieldBit(c: Context)(bit: c.Expr[Int], fn: c.Tree): c.Tree = {
      CSRWARLFieldRange(c)(bit, bit, fn)
    }

    @compileTimeOnly("")
    def CSRRWFieldRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int]): c.Tree = {
      c.parse(s"CSRDefines.CSRField${calcuWidth(c)(msb, lsb)}Bits.RW(" +
        s"${c.eval(msb)}, " +
        s"${c.eval(lsb)}" +
        s")"
      )
    }

    @compileTimeOnly("")
    def CSRRWFieldBit(c: Context)(bit: c.Expr[Int]): c.Tree = {
      CSRRWFieldRange(c)(bit, bit)
    }

    @compileTimeOnly("")
    def CSRRWFieldRangeWithReset(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], resetVal: c.Tree): c.Tree = {
      c.parse(s"CSRDefines.CSRField${calcuWidth(c)(msb, lsb)}Bits.RW(" +
        s"${c.eval(msb)}, " +
        s"${c.eval(lsb)}, " +
        s"${resetVal}" +
        s")"
      )
    }

    @compileTimeOnly("")
    def CSRRWFieldBitWithReset(c: Context)(bit: c.Expr[Int], resetVal: c.Tree): c.Tree = {
      CSRRWFieldRangeWithReset(c)(bit, bit, resetVal)
    }

    @compileTimeOnly("")
    def CSRWLRLFieldRange(c: Context)(msb: c.Expr[Int], lsb: c.Expr[Int], fn: c.Tree): c.Tree = {
      c.parse(s"CSRDefines.CSRField${calcuWidth(c)(msb, lsb)}Bits.WARL(${c.eval(msb)}, ${c.eval(lsb)}, $fn)")
    }

    @compileTimeOnly("")
    def CSRWLRLFieldBit(c: Context)(bit: c.Expr[Int], fn: c.Tree): c.Tree = {
      CSRWLRLFieldRange(c)(bit, bit, fn)
    }

    @compileTimeOnly("")
    def CSRRefWARLFieldRange(c: Context)(ref: c.Tree, msb: c.Expr[Int], lsb: c.Expr[Int], wfn: c.Tree): c.Tree = {
      c.parse(s"CSRDefines.CSRField${calcuWidth(c)(msb, lsb)}Bits.RefWARL($ref, ${c.eval(msb)}, ${c.eval(lsb)}, $wfn)")
    }

    @compileTimeOnly("")
    def CSRRefWARLFieldBit(c: Context)(ref: c.Tree, bit: c.Expr[Int], wfn: c.Tree): c.Tree = {
      CSRRefWARLFieldRange(c)(ref, bit, bit, wfn)
    }
  }
}


