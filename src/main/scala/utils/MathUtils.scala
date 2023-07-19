package utils

object MathUtils {
  def IntToOH(n: Int): BigInt = {
    BigInt(1) << n
  }
}
