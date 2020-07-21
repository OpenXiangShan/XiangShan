package IFUtest

import chisel3._
import chiseltest._
import org.scalatest._
import xiangshan.frontend.PDecode

class PDtest extends FlatSpec with ChiselScalatestTester with Matchers {
  val cacheLine = ("b" +
    "100_1_00001_00000_10" +  //rvc jalr
    "100_0_00001_00000_10" +  //rvc jr
    "101_00000000000_01" +  //rvc j
    "001_00000000000_01" +  //rvc jal
    "111_000_001_00000_01" +  //RVC bnez
    "110_000_001_00000_01" +  //RVC beqz
    "0000000_00000_00001_000_10000_1100111" +  //RET
    "0000000_00000_00101_000_10000_1100111" +  //RET
    "0000000_00000_00000_000_00101_1100111" +  //JALR_CALL
    "0000000_00000_00000_000_00001_1100111" +  //JALR_CALL
    "0000000_00000_00000_000_00101_1101111" +  //JAL_CALL
    "0000000_00000_00000_000_00001_1101111" +  //JAL_CALL
    "0000000_00010_00011_000_10000_1100111" +  //JARL
    "0000000_00000_00000_000_10000_1101111" +  //JAR
    "0000000_00010_00001_101_00000_1100011" +  //Branch bge
    "0000000_00010_00001_100_00000_1100011" +  //Branch blt
    "0000000_00010_00001_001_00000_1100011" +  //Branch bne
    "0000000_00010_00001_000_00000_1100011" +  //Branch beq
    "0000000_00010_00001_000_00001_0000011").U //Notbr
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
