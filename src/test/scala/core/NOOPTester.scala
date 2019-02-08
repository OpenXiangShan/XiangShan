package core

import chisel3.iotesters.PeekPokeTester

import java.nio.{IntBuffer, ByteOrder}
import java.io.FileInputStream
import java.nio.channels.FileChannel

class SimMem {
  private val memSize = 128 * 1024 * 1024
  private var mem: Array[Int] = Array()
  def init(imgPath: String, resetVector: Int) = {
    if (imgPath == "") {
      mem = Array.fill(resetVector / 4)(0) ++ Array(
        0x07b08093,   // addi x1,x1,123
        0xf8508093,   // addi x1,x1,-123
        0x0000806b,   // trap x1
        0, 0, 0, 0
      )
    }
    else {
      val fc = new FileInputStream(imgPath).getChannel()
      println(f"bin size = 0x${fc.size()}%08x")

      mem = Array.fill(memSize / 4)(0)
      fc.map(FileChannel.MapMode.READ_ONLY, 0, fc.size()).order(ByteOrder.LITTLE_ENDIAN)
        .asIntBuffer().get(mem, resetVector / 4, fc.size().toInt / 4)
    }
  }

  def getDataMask(sizeEncode: Int): Int = {
    sizeEncode match {
      case 0 => 0xff
      case 1 => 0xffff
      case 2 => 0xffffffff
      case _ => 0xffffffff
    }
  }

  def checkAddrAlign(addr: Int, sizeEncode: Int) = {
    val addrMask = sizeEncode match {
      case 0 => 0
      case 1 => 0x1
      case 2 => 0x3
      case _ => 0xffffffff
    }

    assert((addr & addrMask) == 0)
  }

  def read(addr: Int, sizeEncode: Int): Int = {
    checkAddrAlign(addr, sizeEncode)
    val idx = addr >> 2
    val offset = addr & 0x3
    val data = mem(idx)
    val rdataAlign = data >> (offset * 8)
    //println(f"rdataAlign = 0x$rdataAlign%08x")
    rdataAlign
  }

  def write(addr: Int, sizeEncode: Int, wdata: Int) = {
    checkAddrAlign(addr, sizeEncode)
    val idx = addr >> 2
    val offset = addr & 0x3
    val data = mem(idx)
    val wdataAlign = wdata << (offset * 8)
    val dataMaskAlign = getDataMask(sizeEncode) << (offset * 8)
    val newData = (data & ~dataMaskAlign) | (wdataAlign & dataMaskAlign)
    if (addr == 0x43f8 && sizeEncode == 0) {
      // write to uart data
      print(f"${wdata & 0xff}%c")
    }
    else { mem(idx) = newData }
    //println(f"wdata = 0x$wdata%08x, realWdata = 0x$newData%08x")
  }
}

class NOOPTester(noop: NOOP, imgPath: String) extends PeekPokeTester(noop)
  with  HasResetVector {

  var pc = 0
  var trap = 0
  var instr = 0

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
  } while (trap == 3)

  trap match {
    case 0 => println(f"\33[1;32mHIT GOOD TRAP\33[0m at pc = 0x$pc%08x")
    case 1 => println(f"\33[1;31mHIT BAD TRAP\33[0m at pc = 0x$pc%08x")
    case 2 => println(f"\33[1;31mINVALID OPCODE\33[0m at pc = 0x$pc%08x, instr = 0x$instr%08x")
  }

  expect(noop.io.trap, 0)
}
