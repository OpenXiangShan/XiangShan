package xiangshan.backend.vector.util

import chisel3.util.BitPat

import scala.language.implicitConversions

object ScalaTypeExt {
  class StringExt(val str: String) {
    def endsWithThese(suffixes: Iterable[String]): Boolean = {
      suffixes.exists(suffix => str.endsWith(suffix))
    }

    def endsWithThese(suffixes: String*): Boolean = {
      this.endsWithThese(suffixes)
    }

    def startsWithThese(prefixes: Iterable[String]): Boolean = {
      prefixes.exists(prefix => str.startsWith(prefix))
    }

    def startsWithThese(prefixes: String*): Boolean = {
      this.startsWithThese(prefixes)
    }
  }

  implicit def StringToExt(str: String): StringExt = new StringExt(str)

  class BooleanExt(val boolean: Boolean) {
    def toBitPat: BitPat = if (this.boolean) BitPat.Y() else BitPat.N()
  }

  implicit def BooleanToExt(boolean: Boolean): BooleanExt = new BooleanExt(boolean)

  class SeqExt[T](val seq: Seq[T]) {
    def product[T2](that: Seq[T2]): Seq[Seq[Any]] = {
      if (this.seq.isEmpty)
        return that.map(x => Seq(x))
      for {
        a <- this.seq
        b: T2 <- that
      } yield {
        Seq(a, b)
      }
    }
  }

  implicit def SeqToExt[T](seq: Seq[T]): SeqExt[T] = new SeqExt[T](seq)
}
