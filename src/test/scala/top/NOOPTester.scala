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
    // CPU
    pc = peek(noop.io.imem.a.bits.addr).toInt
    instr = mem.read(pc, 2)
    poke(noop.io.imem.r.bits.data, instr)

    if (peek(noop.io.dmem.a.valid) == 1) {
      val addr = peek(noop.io.dmem.a.bits.addr).toInt
      val size = peek(noop.io.dmem.a.bits.size).toInt
      val wen = peek(noop.io.dmem.w.valid)

      if (wen == 1) {
        val wdata = peek(noop.io.dmem.w.bits.data).toInt
        val wmask = peek(noop.io.dmem.w.bits.mask).toInt
        mem.write(addr, size, wdata, wmask)
      }
      else {
        poke(noop.io.dmem.r.bits.data, mem.read(addr, size))
      }
    }

    // GPU
    if (noop.HasGPU) {
      poke(noop.io.gpuStart, mem.read(0x4108, 0))
      if (peek(noop.io.gmem.a.valid) == 1) {
        val addr = peek(noop.io.gmem.a.bits.addr).toInt
        val size = peek(noop.io.gmem.a.bits.size).toInt
        val wen = peek(noop.io.gmem.w.valid)
        if (wen == 1) {
          if (size > 2) mem.writeBig(addr, size, peek(noop.io.gmem.w.bits.data))
          else mem.write(addr, size, peek(noop.io.gmem.w.bits.data).toInt, 0xf)
        }
        else {
          poke(noop.io.gmem.r.bits.data,
            if (size > 2) mem.readBig(addr, size) else BigInt(mem.read(addr, size))
          )
        }
      }
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
