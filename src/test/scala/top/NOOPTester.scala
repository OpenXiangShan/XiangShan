package top

import chisel3.iotesters.PeekPokeTester
import chisel3.iotesters
import chisel3.iotesters.Driver

import noop._

class NOOPTester(noop: NOOP, imgPath: String) extends PeekPokeTester(noop)
  with HasResetVector {

  var pc = 0
  var trap = 0
  var instr = 0
  var oldTime = UpTime()

  val mem = new SimMem
  mem.init(imgPath, resetVector)

  do {
    pc = peek(noop.io.imem.out.bits.addr).toInt
    instr = mem.read(pc, peek(noop.io.imem.out.bits.size).toInt)
    poke(noop.io.imem.in.rdata, instr)

    val valid = peek(noop.io.dmem.out.valid)
    if (valid == 1) {
      val dmemAddr = peek(noop.io.dmem.out.bits.addr).toInt
      val size = peek(noop.io.dmem.out.bits.size).toInt
      poke(noop.io.dmem.in.rdata, mem.read(dmemAddr, size))

      val wen = peek(noop.io.dmem.out.bits.wen)
      if (wen == 1) mem.write(dmemAddr, size, peek(noop.io.dmem.out.bits.wdata).toInt)
    }

    step(1)

    trap = peek(noop.io.trap).toInt

    val newTime = UpTime()
    if (newTime - oldTime > 100) {
      val exit = NOOPDevice.call.poll_event()
      if (trap == 3 && exit == 1) trap = 4
      oldTime = newTime
    }

  } while (trap == 3)

  trap match {
    case 0 => println(f"\33[1;32mHIT GOOD TRAP\33[0m at pc = 0x$pc%08x")
    case 1 => println(f"\33[1;31mHIT BAD TRAP\33[0m at pc = 0x$pc%08x")
    case 2 => println(f"\33[1;31mINVALID OPCODE\33[0m at pc = 0x$pc%08x, instr = 0x$instr%08x")
    case 4 => println(f"\33[1;34mABORT\33[0m at pc = 0x$pc%08x")
  }

  expect(noop.io.trap, 0)
}

object TestMain extends App {
  var imgPath = ""
  var newArgs: Array[String] = Array()
  args.sliding(2, 2).toList.collect {
    case Array("--image", argImg: String) => imgPath = argImg
    case Array(a: String, b: String) => newArgs = newArgs :+ a :+ b
  }

  iotesters.Driver.execute(newArgs, () => new NOOP) {
    c => new NOOPTester(c, imgPath)
  }
}
