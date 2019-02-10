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
      case _ => assert(false, f"Bad sizeEncode = $sizeEncode"); 0xffffffff
    }
  }

  def checkAddrAlign(addr: Int, sizeEncode: Int) = {
    val addrMask = sizeEncode match {
      case 0 => 0
      case 1 => 0x1
      case 2 => 0x3
      case 3 => 0x7
      case 4 => 0xf
      case 5 => 0x1f
      case _ => 0xffffffff
    }

    assert((addr & addrMask) == 0, f"addr = 0x$addr%08x, addrMask = 0x$addrMask%08x")
  }

  def read(addr: Int, sizeEncode: Int): Int = {
    checkAddrAlign(addr, sizeEncode)
    val idx = addr >> 2
    val offset = addr & 0x3
    val data = mem(idx)
    val rdataAlign = data >> (offset * 8)

    // read RTC
    if (addr == 0x4048 && sizeEncode == 2) { UpTime() }
    // read key
    else if (addr == 0x4060 && sizeEncode == 2) { NOOPDevice.call.read_key() }
    // read screen size
    else if (addr == 0x4100 && sizeEncode == 2) { (400 << 16) | 320 }
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
  }

  def readBig(addr: Int, sizeEncode: Int): BigInt = {
    checkAddrAlign(addr, sizeEncode)
    val idx = addr >> 2
    // 32 byte
    var data: BigInt = 0;
    sizeEncode match {
      case 3 =>
        data = (data << 32) | BigInt(mem(idx + 1))
        data = (data << 32) | BigInt(mem(idx + 0))
      case 5 =>
        data = (data << 32) | BigInt(mem(idx + 7))
        data = (data << 32) | BigInt(mem(idx + 6))
        data = (data << 32) | BigInt(mem(idx + 5))
        data = (data << 32) | BigInt(mem(idx + 4))
        data = (data << 32) | BigInt(mem(idx + 3))
        data = (data << 32) | BigInt(mem(idx + 2))
        data = (data << 32) | BigInt(mem(idx + 1))
        data = (data << 32) | BigInt(mem(idx + 0))
      case _ => assert(false, f"Bad sizeEncode = $sizeEncode")
    }
    data
  }

  def writeBig(addr: Int, sizeEncode: Int, wdata: BigInt) = {
    checkAddrAlign(addr, sizeEncode)
    val idx = addr >> 2
    assert(sizeEncode == 5, f"Bad sizeEncode = $sizeEncode")
    // 32 byte
    var data: BigInt = wdata;
    mem(idx + 0) = (data & 0xffffffff).toInt; data = data >> 32
    mem(idx + 1) = (data & 0xffffffff).toInt; data = data >> 32
    mem(idx + 2) = (data & 0xffffffff).toInt; data = data >> 32
    mem(idx + 3) = (data & 0xffffffff).toInt; data = data >> 32
    mem(idx + 4) = (data & 0xffffffff).toInt; data = data >> 32
    mem(idx + 5) = (data & 0xffffffff).toInt; data = data >> 32
    mem(idx + 6) = (data & 0xffffffff).toInt; data = data >> 32
    mem(idx + 7) = (data & 0xffffffff).toInt; data = data >> 32
  }
}
