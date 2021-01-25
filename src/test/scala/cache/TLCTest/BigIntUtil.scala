package cache.TLCTest

import scala.collection.mutable.ListBuffer

trait BigIntExtract {
  val prefix: Array[Byte] = Array(0.toByte)

  def extract256Bit(n: BigInt, index: Int): BigInt = {
    val mask256 = BigInt(prefix ++ Array.fill(32)(0xff.toByte))
    (n >> (index * 256)) & mask256
  }

  def replaceNBytes(n: BigInt, in: BigInt, start: Int, len: Int): BigInt = {
    val inArray = in.toByteArray.dropWhile(_ == 0)
    val nArray = n.toByteArray.dropWhile(_ == 0)
    require(inArray.size <= len, s"given insert value:$in, inArray: ${inArray.mkString("Array(", ", ", ")")} longer than len: $len")
    if (nArray.size <= start) {
      BigInt(prefix ++ inArray ++ Array.fill(start - nArray.size)(0.toByte) ++ nArray)
    }
    else {
      BigInt(prefix ++ nArray.dropRight(start + len) ++ Array.fill(len - inArray.size)(0.toByte) ++ inArray ++ nArray.takeRight(start))
    }
  }

  def extractByte(n: BigInt, start: Int, len: Int): BigInt = {
    val mask = BigInt(prefix ++ Array.fill(len)(0xff.toByte))
    (n >> (start * 8)) & mask
  }

  def writeMaskedData(old: BigInt, in: BigInt, mask: BigInt): BigInt = {
    val mask8_buffer = ListBuffer[Byte]()
    var tmp_mask = mask
    while (tmp_mask != 0) {
      mask8_buffer.prepend(
        if ((tmp_mask & 1) == 1) {
          0xff.toByte
        }
        else {
          0.toByte
        }
      )
      tmp_mask >>= 1
    }
    val tmp = mask8_buffer.toArray
    val mask8 = BigInt(prefix ++ tmp)
    ((old | mask8) ^ mask8) | (in & mask8)
  }

  def cleanMask(old: BigInt, off: BigInt): BigInt = {
    (old | off) ^ off
  }
}
