package memory

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

class DistributedMem(memByte: Int, dualPort: Boolean, dataFile: String = "") extends Module {
  val io = IO(new Bundle {
    val rw = Flipped(new MemIO)
    val ro = Flipped(new MemIO)
  })

  val useTreadle = false

  val wordNum = memByte / 4
  val memAddrBits = log2Up(wordNum)
  def Index(addr: UInt): UInt = addr(memAddrBits + 2 - 1, 2)

  val rwIdx = Index(io.rw.a.bits.addr)
  val roIdx = Index(io.ro.a.bits.addr)
  val wen = io.rw.a.valid && io.rw.w.valid
  val wdataVec = VecInit.tabulate(4) { i => io.rw.w.bits.data(8 * (i + 1) - 1, 8 * i) }
  val wmask = VecInit.tabulate(4) { i => io.rw.w.bits.mask(i).toBool }

  val rwData = Wire(UInt(32.W))
  val roData = Wire(UInt(32.W))

  if (useTreadle) {
    val mem = Mem(memByte, UInt(8.W))
    if (dataFile != "")
      loadMemoryFromFile(mem, dataFile)
    def read(idx: UInt) = Cat(mem(idx + 3.U), mem(idx + 2.U), mem(idx + 1.U), mem(idx + 0.U))

    rwData := read(rwIdx << 2)
    roData := read(roIdx << 2)
    wmask.zipWithIndex.map { case(m, i) => {
      when (m && wen) {
        mem((rwIdx << 2) + i.U) := wdataVec(i)
      }
    }}
  }
  else {
    val mem = Mem(wordNum, Vec(4, UInt(8.W)))
    if (dataFile != "")
      loadMemoryFromFile(mem, dataFile)

    rwData := Cat(mem.read(rwIdx).reverse)
    roData := Cat(mem.read(roIdx).reverse)
    when (wen) { mem.write(rwIdx, wdataVec, wmask) }
  }

  io.rw.r.bits.data := rwData
  io.rw.r.valid := true.B
  if (dualPort) {
    io.ro.r.bits.data := roData
    io.ro.r.valid := true.B
  }
  else {
    io.ro := DontCare
  }
}
