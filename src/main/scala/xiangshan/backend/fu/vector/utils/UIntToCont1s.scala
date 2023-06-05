package xiangshan.backend.fu.vector.utils

import chisel3._
import chisel3.util._
import chiseltest._
import firrtl.FirrtlProtos.Firrtl.Width
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import xiangshan.backend.fu.vector.Utils.{NOnes, NZeros}

class UIntToContLow1s(uintWidth: Int) extends Module {
  private val outWidth = (1 << uintWidth) - 1 // 2^n - 1

  val io = IO(new Bundle {
    val dataIn = Input(UInt(uintWidth.W))
    val dataOut = Output(UInt(outWidth.W))
  })

  io.dataOut := helper(io.dataIn)

  private def helper(data: UInt): UInt = data.getWidth match {
    case 1 => Mux(data(0), 1.U(1.W), 0.U(1.W))
    case w => Mux(
      data(w - 1),
      Cat(helper(data(w - 2, 0)), NOnes(1 << (w - 1))),
      Cat(NZeros(1 << (w - 1)), helper(data(w - 2, 0)))
    )
  }
}

object UIntToContLow1s {
  def apply(uint: UInt): UInt = apply(uint, uint.getWidth)

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

object UIntToContHigh1s {
  def apply(uint: UInt): UInt = Reverse(UIntToContLow1s(uint))
  def apply(uint: UInt, width: Int): UInt = Reverse(UIntToContLow1s(uint, width))
}

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