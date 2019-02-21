package memory

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

class DistributedMem(memByte: Int, dualPort: Boolean, delayCycles: Int = 0, dataFile: String = "") extends Module {
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
  val wen = io.rw.isWrite()
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

  def readPort(p: MemIO, rdata: UInt) = {
    val s_idle :: s_reading :: Nil = Enum(2)
    val state = RegInit(s_idle)
    switch (state) {
      is (s_idle) {
        when (p.a.fire()) { state := Mux(p.r.fire(), s_idle, s_reading) }
      }
      is (s_reading) {
        when (p.r.fire()) { state := s_idle }
      }
    }

    p.a.ready := state === s_idle
    p.r.bits.data := rdata
    p.r.valid := (if (delayCycles == 0) p.a.fire() else Counter(state === s_reading, delayCycles)._2)
  }

  readPort(io.rw, rwData)
  if (dualPort) {
    readPort(io.ro, roData)
  }
  else {
    io.ro := DontCare
  }
}
