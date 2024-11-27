package utils

import chisel3._
import chisel3.util._
object SeqUtils {
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
