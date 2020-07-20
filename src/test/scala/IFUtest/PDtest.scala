package IFUtest

import chisel3._
import chiseltest._
import org.scalatest._
import xiangshan.frontend.PDecode

class PDtest extends FlatSpec with ChiselScalatestTester with Matchers {
  val cacheLine = ("b" +
    "0010000000000001" +  //rvc jal
    "1111000110111001" +  //rvc branch
    "00100000001000001000000111100111" +  //RET
    "00110010110101000001110111100011" +  //Branch bne
    "00010011010010110000001011100111" +  //JALR_CALL
    "00001101001010010111000011101111" +  //JAL_CALL
    "00001011011100000000011101100111" +  //JARL
    "00000110100110110101010101101111" +  //JAR
    "00000110101101000011100000101111").U //Notbr
  behavior of "PD Test"

  it should "test PDecode" in {
    test(new PDecode) { c =>
      println(s"\n--------------------cycle 1------------------\n")
      c.io.in.valid.poke(true.B)
      c.io.in.bits.cacheLine.poke(cacheLine)
      c.clock.step()
      println(s"\n--------------------cycle 2------------------\n")
      c.io.in.valid.poke(true.B)
      c.io.in.bits.cacheLine.poke(cacheLine)
      c.clock.step()
      println(s"\n--------------------cycle 3------------------\n")
    }
  }
}
