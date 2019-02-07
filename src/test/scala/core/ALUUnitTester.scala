package core

import chisel3.iotesters.PeekPokeTester
import top.ALUModule

class ALUUnitTester(alu: ALUModule) extends PeekPokeTester(alu) {
  def asUnsigned(a: Long) : Long = if (a < 0) a + 0x100000000L else a

  def scalaAlu(a: Long, b: Long, func: Int): Long = {
    val res = func match {
      case 0 => a + b
      case 1 => a.toInt << (b & 0x1f)
      case 2 => (a < b).toLong
      case 3 => (asUnsigned(a) < asUnsigned(b)).toLong
      case 4 => a ^ b
      case 5 => asUnsigned(a) >> (b & 0x1f)
      case 6 => a | b
      case 7 => a & b
      case 8 => a - b
      case 13 => a.toInt >>> (b & 0x1f)
    }
    val res2 = asUnsigned(res.toInt)
    res2
  }

  val input = List(0, 1, 2, 0x7ffffffe, 0x7fffffff, 0x80000000, 0x80000001, 0xfffffffe, 0xffffffff)
  for (f <- (0 to 8).toList :+ 13) {
    for (a <- input) {
      for (b <- input) {
        poke(alu.io.src1, a)
        poke(alu.io.src2, b)
        poke(alu.io.func, f)
        val ref = scalaAlu(a, b, f)
        val dut = peek(alu.io.out)
        if (dut != ref) {
          println(s"a = $a, op = $f, b = $b, right = $ref, wrong = $dut")
        }
        expect(alu.io.out, ref)
      }
    }
  }
}

