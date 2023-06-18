package utils

import scala.collection.mutable

object SeqUtils {
  /**
    * @todo remove it when when xiangshan is updated to 2.13.11
    */
  def distinctBy[A, B](seqLike: Seq[B])(f: B => A): Seq[B] = {
    val seen = new mutable.HashSet[A]()
    var res = Seq[B]()
    val it = seqLike.iterator
    while (it.hasNext) {
      val next = it.next
      if (seen.add(f(next))) {
        res :+= next
      }
    }
    res
  }
}
