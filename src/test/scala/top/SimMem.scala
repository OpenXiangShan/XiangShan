package top

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

    NOOPDevice.call.init_sdl()
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

    // read RTC
    if (addr == 0x4048 && sizeEncode == 2) { UpTime() }
    // read key
    else if (addr == 0x4060 && sizeEncode == 2) { NOOPDevice.call.read_key() }
    // read screen size
    else if (addr == 0x4100 && sizeEncode == 2) { (400 << 16) | 300 }
    else { rdataAlign }
  }

  def write(addr: Int, sizeEncode: Int, wdata: Int) = {
    checkAddrAlign(addr, sizeEncode)
    val idx = addr >> 2
    val offset = addr & 0x3
    val data = mem(idx)
    val wdataAlign = wdata << (offset * 8)
    val dataMaskAlign = getDataMask(sizeEncode) << (offset * 8)
    val newData = (data & ~dataMaskAlign) | (wdataAlign & dataMaskAlign)

    // write to uart data
    if (addr == 0x43f8 && sizeEncode == 0) { print(f"${wdata & 0xff}%c") }
    else if (addr == 0x4104 && sizeEncode == 2) {
      // sync vga
      println(s"sync vga at ${UpTime()}")
      NOOPDevice.call.update_screen(mem)
    }
    else { mem(idx) = newData }
    //println(f"wdata = 0x$wdata%08x, realWdata = 0x$newData%08x")
  }
}
