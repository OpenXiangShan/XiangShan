package xiangshan.frontend

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class PDtest extends AnyFlatSpec with ChiselScalatestTester with Matchers{
  val cacheLine2 = ("b" +
    "100_1_00001_00000_10" +  //rvc jalr
    "100_0_00001_00000_10" +  //rvc jr
    "101_00000000000_01" +  //rvc j
    "001_00000000000_01" +  //rvc jal
    "111_000_001_00000_01" +  //RVC bnez

    "0000000_00000_00001_000_10000_1100111" +  //RET
    "0000000_00000_00101_000_10000_1100111" +  //RET
    "0000000_00000_00000_000_00101_1100111" +  //JALR_CALL
    "0000000_00000_00000_000_00001_1100111" +  //JALR_CALL
    "0000000_00000_00000_000_00101_1101111" +  //JAL_CALL
    "0000000_00000_0000").U   //JAL_CALL_height-half

  val cacheLine1 = ("b" +
    "0_000_00001_1101111" +  //JAL_CALL_low-half
    "110_000_001_00000_01" +  //RVC beqz
    "0000000_00010_00011_000_10000_1100111" +  //JARL
    "0000000_00000_00000_000_10000_1101111" +  //JAR
    "0000000_00010_00001_101_00000_1100011" +  //Branch bge
    "0000000_00010_00001_100_00000_1100011" +  //Branch blt
    "0000000_00010_00001_001_00000_1100011" +  //Branch bne
    "0000000_00010_00001_000_00000_1100011" +  //Branch beq
    "0000000_00010_00001_000_00001_0000011").U //Notbr

  it should "test PDecode" in {
    test(new PreDecode) { c =>
      println(s"\n--------------------cycle 1------------------\n")
      c.io.in.pc.poke(0.U)
      c.io.in.mask.poke("b1111_1111_1111_0000".U)
      c.io.in.data.poke(cacheLine1)
      c.clock.step()
      println(s"\n--------------------cycle 2------------------\n")
      c.io.in.pc.poke((1<<5).U)
      c.io.in.mask.poke("b1111_1111_1111_1111".U)
      c.io.in.data.poke(cacheLine2)
      c.clock.step()
      println(s"\n--------------------cycle 3------------------\n")
      c.io.in.pc.poke((2<<5).U)
      c.io.in.mask.poke("b1111_1111_1111_1111".U)
      c.io.in.data.poke(cacheLine1)
      c.clock.step()
      println(s"\n--------------------cycle 4------------------\n")
      c.io.in.pc.poke((3<<5).U)
      c.io.in.mask.poke("b1111_1111_1111_1111".U)
      c.io.in.data.poke(cacheLine2)
      c.clock.step()
    }
  }
}
