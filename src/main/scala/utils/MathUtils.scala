package utils

object MathUtils {
  def IntToOH(n: Int): BigInt = {
    BigInt(1) << n
  }

  /** function to generate w/r mask
   */
  object BigIntGenMask {
    /** generate a continuous mask period
     * @param high the highest bit of the mask
     * @param low the lowest bit of the mask
     * @return the mask period, can be "or"ed together to form a large mask
     */
    def maskPeriodGen(high: Int, low: Int): BigInt = {
      require(high > low)
      val maskLen = high - low + 1
      ((BigInt(1) << maskLen) - 1) << low
    }

    /** generate w/r mask for a single bit
     */
    def maskBitGen(pos: Int): BigInt = {
      BigInt(1) << pos
    }

    /** Apply function of BigIntGen. Generate a mask for a Seq of input.
     * The Seq contains 2 kinds of input: (high: Int, low: Int) and (pos: Int).
     * The former generates a mask period, the latter generates a mask bit.
     * @param inputs a Seq of input
     * @param rev whether needs to get the not of this mask
     * @param len if rev is true, then we needs to specify the total length of the BigInt
     * @return a Bigint mask generated for the input
     */
    def apply(inputs: Seq[Any], rev: Boolean, len: Int): BigInt = {
      val maskTmp = inputs.map {
        case (high: Int, low: Int) => maskPeriodGen(high, low)
        case pos: Int => maskBitGen(pos)
        case _ => BigInt(0)
      }.foldLeft(BigInt(0))(_ | _)
      if (rev) {
        BigIntNot(maskTmp, len)
      } else {
        maskTmp
      }
    }

    def apply(inputs: Seq[Any]): BigInt = {
      inputs.map {
        case (high: Int, low: Int) => maskPeriodGen(high, low)
        case pos: Int => maskBitGen(pos)
        case _ => BigInt(0)
      }.foldLeft(BigInt(0))(_ | _)
    }
  }

  object BigIntNot {
    def apply(bigInt: BigInt, len: Int): BigInt = {
      var res = bigInt
      for (i <- 0 until len) {
        res = res.flipBit(i)
      }
      res
    }

    def apply(bigInt: BigInt): BigInt = apply(bigInt, bigInt.bitLength)
  }
}
