package xiangshan.backend.fu.vector.utils

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import xiangshan.backend.fu.vector.Utils.{NOnes, NZeros}

/**
 * Convert an UInt to continuous 1s at low bits
 * e.g. 3.U(3.W) -> 0b0000111.U(7.W), 5.U(3.W) -> 0b0011111.U(7.W)
 * @param uintWidth width of input UInt
 */
class UIntToContLow1s(uintWidth: Int) extends Module {
  private val outWidth = (1 << uintWidth) - 1 // 2^n - 1

  val io = IO(new Bundle {
    val dataIn = Input(UInt(uintWidth.W))
    val dataOut = Output(UInt(outWidth.W))
  })

  io.dataOut := helper(io.dataIn)

  /**
   * A helper function to convert UInt to continuous low 1s recursively
   * @param data input UInt with width n
   * @return continuous low 1s with width 2^uint.n^- 1
   */
  private def helper(data: UInt): UInt = data.getWidth match {
    case 1 => Mux(data(0), 1.U(1.W), 0.U(1.W)) // 1-bit 1 or 0
    case w => Mux(
      data(w - 1),
      Cat(helper(data(w - 2, 0)), NOnes(1 << (w - 1))), // if the highest bit is 1, then append 1s to the right
      Cat(NZeros(1 << (w - 1)), helper(data(w - 2, 0))) // if the highest bit is 0, then append 0s to the left
    )
  }
}

/**
 * Convert an UInt to continuous 1s at low bits
 * e.g. 3.U(3.W) -> 0b0000111.U(7.W), 5.U(3.W) -> 0b0011111.U(7.W)
 */
object UIntToContLow1s {
  /**
   * Convert an UInt to continuous 1s at low bits
   * e.g. 3.U(3.W) -> 0b0000111.U(7.W), 5.U(3.W) -> 0b0011111.U(7.W)
   * @param uint input UInt
   * @return continuous low 1s with width 2^[[uint]].getWidth^- 1
   */
  def apply(uint: UInt): UInt = apply(uint, uint.getWidth)

  /**
   * Convert an UInt to continuous 1s at low bits
   * e.g. 3.U(3.W) -> 0b0000111.U(7.W), 5.U(3.W) -> 0b0011111.U(7.W)
   * @param uint input UInt
   * @param width width of output UInt
   * @return continuous low 1s with width [[width]]
   */
  def apply(uint: UInt, width: Int): UInt = {
    val uintToContTail1sMod = Module(new UIntToContLow1s(uint.getWidth)).suggestName(s"uintToContTail1sMod${width}Bits")
    uintToContTail1sMod.io.dataIn := uint
    val dataOutWidth = uintToContTail1sMod.io.dataOut.getWidth
    if (width <= dataOutWidth) {
      uintToContTail1sMod.io.dataOut(width - 1, 0)
    } else {
      Cat(0.U((width - dataOutWidth).W), uintToContTail1sMod.io.dataOut)
    }
  }
}

/**
 * Convert an UInt to continuous 1s at high bits
 * e.g. 3.U(3.W) -> 0b1110000.U(7.W), 5.U(3.W) -> 0b1111100.U(7.W)
 */
object UIntToContHigh1s {
  /**
   * Convert an UInt to continuous 1s at high bits
   * e.g. 3.U(3.W) -> 0b1110000.U(7.W), 5.U(3.W) -> 0b1111100.U(7.W)
   * @param uint input UInt
   * @return continuous high 1s with width 2^[[uint]].getWidth^- 1
   */
  def apply(uint: UInt): UInt = Reverse(UIntToContLow1s(uint))

  /**
   * Convert an UInt to continuous 1s at high bits
   * e.g. 3.U(3.W) -> 0b1110000.U(7.W), 5.U(3.W) -> 0b1111100.U(7.W)
   * @param uint input UInt
   * @param width width of output UInt
   * @return continuous high 1s with width [[width]]
   */
  def apply(uint: UInt, width: Int): UInt = Reverse(UIntToContLow1s(uint, width))
}

/*
class UIntToContLow1sTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  private val uintWidth = 3
  private val outWidth = (1 << uintWidth) - 1

  private val testSeq = Seq(
    0.U  -> "b0000000".U,
    1.U  -> "b0000001".U,
    2.U  -> "b0000011".U,
    3.U  -> "b0000111".U,
    4.U  -> "b0001111".U,
    5.U  -> "b0011111".U,
    6.U  -> "b0111111".U,
    7.U  -> "b1111111".U,
  )

  behavior of "UIntToContLow1s"
  it should "run" in {
    test(new UIntToContLow1s(uintWidth)) {
      m: UIntToContLow1s =>
        for (((uint, res), i) <- testSeq.zipWithIndex) {
          m.io.dataIn.poke(uint)

          val message = "i: " + i +
                        " uint: " + uint.litValue.toInt.toBinaryString +
                        " right result: " + res.litValue.toInt.toBinaryString

          m.io.dataOut.expect(res, message)
        }
    }
  }
  println("test done")
}
*/
