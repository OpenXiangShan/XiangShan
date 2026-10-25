package xiangshan.backend.fu.vector.utils

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import xiangshan.backend.fu.vector.Utils.{NOnes, NZeros}

/** Recursive combinational thermometer mask: bit i is one when i >= dataIn.
  * The full output has 2^uintWidth-1 bits, so the maximum input clears every bit.
  */
class UIntToContLow0s(uintWidth: Int) extends Module {
  private val outWidth = (1 << uintWidth) - 1 // 2^n - 1

  val io = IO(new Bundle {
    val dataIn = Input(UInt(uintWidth.W))
    val dataOut = Output(UInt(outWidth.W))
  })

  io.dataOut := helper(io.dataIn)

  private def helper(data: UInt): UInt = data.getWidth match {
    case 1 => Mux(data(0), 0.U(1.W), 1.U(1.W))
    case w => Mux(
      data(w - 1),
      Cat(helper(data(w - 2, 0)), NZeros(1 << (w - 1))),
      Cat(NOnes(1 << (w - 1)), helper(data(w - 2, 0)))
    )
  }
}

object UIntToContLow0s {
  /** Default helper width is the input bit width, not the class's full mask width. */
  def apply(uint: UInt): UInt = apply(uint, uint.getWidth)

  /** Truncate the low mask bits or zero-extend to the explicitly requested width. */
  def apply(uint: UInt, width: Int): UInt = {
    val uintToContTail0sMod = Module(new UIntToContLow0s(uint.getWidth)).suggestName(s"uintToContTail0sMod${width}Bits")
    uintToContTail0sMod.io.dataIn := uint
    val dataOutWidth = uintToContTail0sMod.io.dataOut.getWidth
    if (width <= dataOutWidth) {
      uintToContTail0sMod.io.dataOut(width - 1, 0)
    } else {
      Cat(0.U((width - dataOutWidth).W), uintToContTail0sMod.io.dataOut)
    }
  }
}

object UIntToContHigh0s {
  def apply(uint: UInt): UInt = Reverse(UIntToContLow0s(uint))
  def apply(uint: UInt, width: Int): UInt = Reverse(UIntToContLow0s(uint, width))
}

class UIntToContLow0sTest extends AnyFlatSpec with Matchers with ChiselSim {

  private val uintWidth = 3
  private val outWidth = (1 << uintWidth) - 1

  private val testSeq = Seq(
    0.U  -> "b1111111".U,
    1.U  -> "b1111110".U,
    2.U  -> "b1111100".U,
    3.U  -> "b1111000".U,
    4.U  -> "b1110000".U,
    5.U  -> "b1100000".U,
    6.U  -> "b1000000".U,
    7.U  -> "b0000000".U,
  )

  behavior of "UIntToContLow0s"
  it should "run" in {
    simulate(new UIntToContLow0s(uintWidth)) {
      m: UIntToContLow0s =>
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
