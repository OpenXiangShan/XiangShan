/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

// See LICENSE.Berkeley for license details.

package utils

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

abstract class Decoding
{
  def uncorrected: UInt
  def corrected: UInt
  def correctable: Bool
  def uncorrectable: Bool // If true, correctable should be ignored
  def error = correctable || uncorrectable
}

abstract class Code
{
  def canDetect: Boolean
  def canCorrect: Boolean

  def width(w0: Int): Int

  /** Encode x to a codeword suitable for decode.
   *  If poison is true, the decoded value will report uncorrectable
   *  error despite uncorrected == corrected == x.
   */
  def encode(x: UInt, poison: Bool = false.B): UInt
  def decode(x: UInt): Decoding

  /** Copy the bits in x to the right bit positions in an encoded word,
   *  so that x === decode(swizzle(x)).uncorrected; but don't generate
   *  the other code bits, so decode(swizzle(x)).error might be true.
   *  For codes for which this operation is not trivial, throw an
   *  UnsupportedOperationException.  */
  def swizzle(x: UInt): UInt
}

class IdentityCode extends Code
{
  def canDetect = false
  def canCorrect = false

  def width(w0: Int) = w0
  def encode(x: UInt, poison: Bool = false.B) = {
    require (poison.isLit && poison.litValue == 0, "IdentityCode can not be poisoned")
    x
  }
  def swizzle(x: UInt) = x
  def decode(y: UInt) = new Decoding {
    def uncorrected = y
    def corrected = y
    def correctable = false.B
    def uncorrectable = false.B
  }
}

class ParityCode extends Code
{
  def canDetect = true
  def canCorrect = false

  def width(w0: Int) = w0+1
  def encode(x: UInt, poison: Bool = false.B) = Cat(x.xorR ^ poison, x)
  def swizzle(x: UInt) = Cat(false.B, x)
  def decode(y: UInt) = new Decoding {
    val uncorrected = y(y.getWidth-2,0)
    val corrected = uncorrected
    val correctable = false.B
    val uncorrectable = y.xorR
  }
}

class SECCode extends Code
{
  def canDetect = true
  def canCorrect = true

  // SEC codes may or may not be poisonous depending on the length
  // If the code is perfect, every non-codeword is correctable
  def poisonous(n: Int) = !isPow2(n+1)

  def width(k: Int) = {
    val m = log2Floor(k) + 1
    k + m + (if((1 << m) < m+k+1) 1 else 0)
  }
  def swizzle(x: UInt) = {
    val k = x.getWidth
    val n = width(k)
    Cat(0.U((n-k).W), x)
  }

  // An (n=16, k=11) Hamming code is naturally encoded as:
  //   PPxPxxxPxxxxxxxP where P are parity bits and x are data
  //   Indexes typically start at 1, because then the P are on powers of two
  // In systematic coding, you put all the data in the front:
  //   xxxxxxxxxxxPPPPP
  //   Indexes typically start at 0, because Computer Science
  // For sanity when reading SRAMs, you want systematic form.

  private def impl(n: Int, k: Int) = {
    require (n >= 3 && k >= 1 && !isPow2(n))
    val hamm2sys = IndexedSeq.tabulate(n+1) { i =>
      if (i == 0) {
        n /* undefined */
      } else if (isPow2(i)) {
        k + log2Ceil(i)
      } else {
        i - 1 - log2Ceil(i)
      }
    }
    val sys2hamm = hamm2sys.zipWithIndex.sortBy(_._1).map(_._2).toIndexedSeq
    def syndrome(j: Int) = {
      val bit = 1 << j
      ("b" + Seq.tabulate(n) { i =>
        if ((sys2hamm(i) & bit) != 0) "1" else "0"
      }.reverse.mkString).U
    }
    (hamm2sys, sys2hamm, syndrome _)
  }

  def encode(x: UInt, poison: Bool = false.B) = {
    val k = x.getWidth
    val n = width(k)
    val (_, _, syndrome) = impl(n, k)

    require ((poison.isLit && poison.litValue == 0) || poisonous(n), s"SEC code of length ${n} cannot be poisoned")

    /* By setting the entire syndrome on poison, the corrected bit falls off the end of the code */
    val syndromeUInt = VecInit.tabulate(n-k) { j => (syndrome(j)(k-1, 0) & x).xorR ^ poison }.asUInt
    Cat(syndromeUInt, x)
  }

  def decode(y: UInt) = new Decoding {
    val n = y.getWidth
    val k = n - log2Ceil(n)
    val (_, sys2hamm, syndrome) = impl(n, k)

    val syndromeUInt = VecInit.tabulate(n-k) { j => (syndrome(j) & y).xorR }.asUInt

    val hammBadBitOH = UIntToOH(syndromeUInt, n+1)
    val sysBadBitOH = VecInit.tabulate(k) { i => hammBadBitOH(sys2hamm(i)) }.asUInt

    val uncorrected = y(k-1, 0)
    val corrected = uncorrected ^ sysBadBitOH
    val correctable = syndromeUInt.orR
    val uncorrectable = if (poisonous(n)) { syndromeUInt > n.U } else { false.B }
  }
}

class SECDEDCode extends Code
{
  def canDetect = true
  def canCorrect = true

  private val sec = new SECCode
  private val par = new ParityCode

  def width(k: Int) = sec.width(k)+1
  def encode(x: UInt, poison: Bool = false.B) = {
    // toggling two bits ensures the error is uncorrectable
    // to ensure corrected == uncorrected, we pick one redundant
    // bit from SEC (the highest); correcting it does not affect
    // corrected == uncorrected. the second toggled bit is the
    // parity bit, which also does not appear in the decoding
    val toggle_lo = Cat(poison.asUInt, poison.asUInt)
    val toggle_hi = toggle_lo << (sec.width(x.getWidth)-1)
    par.encode(sec.encode(x)) ^ toggle_hi
  }
  def swizzle(x: UInt) = par.swizzle(sec.swizzle(x))
  def decode(x: UInt) = new Decoding {
    val secdec = sec.decode(x(x.getWidth-2,0))
    val pardec = par.decode(x)

    val uncorrected = secdec.uncorrected
    val corrected = secdec.corrected
    val correctable = pardec.uncorrectable
    val uncorrectable = !pardec.uncorrectable && secdec.correctable
  }
}

object ErrGen
{
  // generate a 1-bit error with approximate probability 2^-f
  def apply(width: Int, f: Int): UInt = {
    require(width > 0 && f >= 0 && log2Up(width) + f <= 16)
    UIntToOH(LFSR(16)(log2Up(width)+f-1,0))(width-1,0)
  }
  def apply(x: UInt, f: Int): UInt = x ^ apply(x.getWidth, f)
}

trait CanHaveErrors extends Bundle {
  val correctable: Option[ValidIO[UInt]]
  val uncorrectable: Option[ValidIO[UInt]]
}

case class ECCParams(
  bytes: Int = 1,
  code: Code = new IdentityCode,
  notifyErrors: Boolean = false
)

object Code {
  def fromString(s: Option[String]): Code = fromString(s.getOrElse("none"))
  def fromString(s: String): Code = s.toLowerCase match {
    case "none" => new IdentityCode
    case "identity" => new IdentityCode
    case "parity" => new ParityCode
    case "sec" => new SECCode
    case "secded" => new SECDEDCode
    case _ => throw new IllegalArgumentException("Unknown ECC type")
  }
}
