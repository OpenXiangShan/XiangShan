package xiangshan.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object InstanceNameMacro {
  def getVariableName[T](x: T): String = macro getVariableNameImpl[T]

  def getVariableNameImpl[T: c.WeakTypeTag](c: blackbox.Context)(x: c.Tree): c.Tree = {
    import c.universe._

    val name = this.extractName(c)(x)
    q"$name"
  }

  def getVariableNameSeq[T](xs: T*): Seq[String] = macro getVariableNameSeqImpl[T]

  def getVariableNameSeqImpl[T: c.WeakTypeTag](c: blackbox.Context)(xs: c.Expr[T]*): c.Tree = {
    import c.universe._
    c.echo(c.enclosingPosition, s"call getVariableNameSeq times = ${times}")
    times += 1
    val names = xs.map { x =>
      val name = extractName(c)(x.tree)
      name
    }
    q"_root_.scala.collection.immutable.Seq(..$names)"
  }

  def withName[T](x: T): (T, String) = macro withNameImpl[T]

  def withNameImpl[T: c.WeakTypeTag](c: blackbox.Context)(x: c.Tree): c.Tree = {
    import c.universe._

    val name = this.extractName(c)(x)
    q"($x, $name)"
  }

  def withNameSeq[T](xs: T*): Seq[(T, String)] = macro withNameSeqImpl[T]

  def withNameSeqImpl[T: c.WeakTypeTag](c: blackbox.Context)(xs: c.Expr[T]*): c.Tree = {
    import c.universe._

    val tuples: Seq[Tree] = xs.map { expr =>
      val name = extractName(c)(expr.tree)
      q"($expr, $name)"
    }

    q"Seq(..$tuples)"
  }

  private def extractName(c: blackbox.Context)(tree: c.Tree): String = {
    import c.universe._
    tree match {
      case Ident(TermName(name)) =>
        name
      case Select(qualifier, TermName(name)) =>
        name
      case _ =>
        c.abort(c.enclosingPosition, s"getVariableNames can only be used with simple variables or object fields, but get ${tree}")
    }
  }

  var times = 0
}