package xiangshan.backend

import scala.language.implicitConversions

/** Normalize general Scala collections to the sequence types expected by backend builders. */
object implicitCast {
  implicit def IndexedSeqCast[T](seq: scala.collection.IndexedSeq[T]): IndexedSeq[T] = {
    seq.toIndexedSeq
  }

  implicit def SeqCast[T](seq: scala.collection.Seq[T]): Seq[T] = {
    seq.toSeq
  }
}
