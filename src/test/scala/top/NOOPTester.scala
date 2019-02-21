package top

import chisel3.iotesters.PeekPokeTester
import chisel3.iotesters
import chisel3.iotesters.Driver

import noop._

class NOOPTester(noop: NOOPSimTop) extends PeekPokeTester(noop) {
  val vmem: Array[Int] = Array.fill(0x80000 / 4)(0)

  def handleMMIO(trapEncode: BigInt) = {
    val mmioValid = ((trapEncode >> 70) & 0x1).toInt
    if (mmioValid == 1) {
      val mmioCmd = ((trapEncode >> 71) & 0x7).toInt
      val mmioWrite = mmioCmd >> 2
      if (mmioWrite == 0) {
        val mmioRdata = mmioCmd match {
          case 0 => UpTime()
          case 1 => NOOPDevice.call.read_key()
          case 2 => NOOPDevice.call.screen_size()
        }
        poke(noop.io.mmioRdata, mmioRdata)
      }
      else {
        mmioCmd match {
          case 4 =>
            println(s"sync vga at ${UpTime()}")
            NOOPDevice.call.update_screen(vmem)
          case 5 =>
            val addr = ((trapEncode >> 34) & 0xffffffff).toInt
            val wdata = ((trapEncode >> 2) & 0xffffffff).toInt
            val wmask = ((trapEncode >> 66) & 0xf).toInt
            val wmaskExpand = wmask match {
              case 0x1 => 0x000000ff
              case 0x2 => 0x0000ff00
              case 0x4 => 0x00ff0000
              case 0x8 => 0xff000000
              case 0x3 => 0x0000ffff
              case 0xc => 0xffff0000
              case 0xf => 0xffffffff
              case _ => assert(false, f"Bad wmask = 0x$wmask%x"); 0
            }
            val idx = (addr - 0x40000) >> 2
            vmem(idx) = (wdata & wmaskExpand) | (vmem(idx) & ~wmaskExpand)
          case 6 => // putc()
        }
      }
    }
  }

  var oldTime = UpTime()
  def pollEvent(): Int = {
    val newTime = UpTime()
    if (newTime - oldTime > 100) {
      oldTime = newTime
      NOOPDevice.call.poll_event()
    }
    else 0
  }


  var trap = 0
  NOOPDevice.call.init_sdl()

  do {
    step(1)

    val trapEncode = peek(noop.io.trap)
    handleMMIO(trapEncode)
    trap = (trapEncode & 0x3).toInt
    if (trap == 3 && pollEvent() == 1) trap = 4

  } while (trap == 3)

  val pc = peek(noop.io.trapInfo.pc).toInt
  val instr = peek(noop.io.trapInfo.instr).toInt
  trap match {
    case 0 => println(f"\33[1;32mHIT GOOD TRAP\33[0m at pc = 0x$pc%08x")
    case 1 => println(f"\33[1;31mHIT BAD TRAP\33[0m at pc = 0x$pc%08x")
    case 2 => println(f"\33[1;31mINVALID OPCODE\33[0m at pc = 0x$pc%08x, instr = 0x$instr%08x")
    case 4 => println(f"\33[1;34mABORT\33[0m at pc = 0x$pc%08x")
  }

  val instrCnt = peek(noop.io.instrCnt).toInt
  val cycleCnt = peek(noop.io.cycleCnt).toInt
  println(s"instrCnt = $instrCnt, cycleCnt = $cycleCnt, IPC = ${instrCnt.toFloat / cycleCnt.toFloat}")
  //expect(noop.io.trap, 0)
}

object TestMain extends App {
  var imgPath = ""
  var newArgs: Array[String] = Array()
  args.sliding(2, 2).toList.collect {
    case Array("--image", argImg: String) => imgPath = argImg
    case Array(a: String, b: String) => newArgs = newArgs :+ a :+ b
  }

  iotesters.Driver.execute(newArgs, () => new NOOPSimTop(memInitFile = imgPath)) {
    c => new NOOPTester(c)
  }
}
