package utils

object MathUtils {
  def IntToOH(n: Int): BigInt = {
    BigInt(1) << n
  }

  object BigIntGenMask {
    // generate w/r mask
    def apply(high: Int, low: Int): BigInt = {
      require(high > low)
      val maskLen = high - low + 1
      ((BigInt(1) << maskLen) - 1) << low
    }

    def apply(pos: Int): BigInt = {
      BigInt(1) << pos
    }
  }

  object BigIntNot {
    def apply(bigInt: BigInt): BigInt = {
      var res = bigInt
      val len = bigInt.bitLength
      for (i <- 0 until len) {
        res = res.flipBit(i)
      }
      res
    }
  }
}
