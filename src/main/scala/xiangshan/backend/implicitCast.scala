package xiangshan.backend

object implicitCast {
  implicit def IndexedSeqCast[T](seq: scala.collection.IndexedSeq[T]): IndexedSeq[T] = {
    seq.toIndexedSeq
  }

  implicit def SeqCast[T](seq: scala.collection.Seq[T]): Seq[T] = {
    seq.toSeq
  }
}
