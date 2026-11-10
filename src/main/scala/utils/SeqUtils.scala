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

  /** Computes the prefix OR (cumulative logical OR) of a sequence of Bool signals.
    *
    * Example:
    *   prefixOr([a, b, c, d]) -> [a, a||b, a||b||c, a||b||c||d]
    *
    * The implementation uses a divide-and-conquer recursive approach.
    *
    * @param in input sequence of Bool
    * @return   output sequence where each element is the OR of all previous inputs up to that index
    */
  // Keep Bool-specific because prefixOr relies on logical OR semantics.
  def prefixOr(in: Seq[Bool]): Seq[Bool] = {
    val n = in.length
    if (n <= 1) {
      in
    } else {
      val half    = n / 2
      val leftIn  = in.take(half)
      val rightIn = in.drop(half)

      // Recursively compute prefix OR on left and right halves
      val leftPrefix  = prefixOr(leftIn)
      val rightPrefix = prefixOr(rightIn)

      // Overall OR value of the left half (i.e., the last element of leftPrefix)
      val leftTotalOr = leftPrefix.last

      // Every element in the right half must be ORed with leftTotalOr
      val rightPrefixAdjusted = rightPrefix.map(_ || leftTotalOr)

      // Concatenate the results
      leftPrefix ++ rightPrefixAdjusted
    }
  }
}
