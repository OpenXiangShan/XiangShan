package core

import chisel3.iotesters.PeekPokeTester

class NOOPTester(noop: NOOP) extends PeekPokeTester(noop) {
  val mem = List(
    0x07b08093,   // addi x1,x1,123
    0xf8508093,   // addi x1,x1,-123
    0x0000806b,   // trap x1
    0, 0, 0, 0
  )

  var pc = 0
  var trap = 0
  var instr = 0
  do {
    pc = peek(noop.io.imem.out.bits.addr).toInt
    assert((pc & 0x3) == 0)
    instr = mem(pc >> 2)
    poke(noop.io.imem.in.rdata, instr)

    step(1)

    trap = peek(noop.io.trap).toInt
  } while (trap == 3)

  trap match {
    case 0 => println(s"\33[1;32mHIT GOOD TRAP\33[0m at pc = $pc") 
    case 1 => println(s"\33[1;31mHIT BAD TRAP\33[0m at pc = $pc")
    case 2 => println(s"\33[1;31mINVALID OPCODE\33[0m at pc = $pc, instr = $instr")
  }
}
