package utils

import chisel3._
import chisel3.util._

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

  type Seq2[+T] = Seq[Seq[T]]
  type Seq3[+T] = Seq2[Seq[T]]
  type MixedVec2[T <: Data] = MixedVec[MixedVec[T]]
  type MixedVec3[T <: Data] = MixedVec2[MixedVec[T]]

  def mapToMixedVec[T, A <: Data](in: Seq[T], f: T => A): MixedVec[A] = {
    MixedVec(in.map(f))
  }

  def mapToMixedVec2[T, A <: Data](in: Seq2[T], f: T => A): MixedVec2[A] = {
    MixedVec(in.map(x => mapToMixedVec(x, f)))
  }

  def mapToMixedVec3[T, A <: Data](in: Seq3[T], f: T => A): MixedVec3[A] = {
    MixedVec(in.map(x => mapToMixedVec2(x, f)))
  }
}
