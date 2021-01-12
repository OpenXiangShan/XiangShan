package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, TlbRequestIO, MemoryOpConstants}
import xiangshan.backend.LSUOpType
import xiangshan.mem._
import xiangshan.backend.roq.RoqPtr

class LQDataEntry extends XSBundle {
  // val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
  val exception = UInt(16.W) // TODO: opt size
  val fwdMask = Vec(8, Bool())
}

// Data module define
// These data modules are like SyncDataModuleTemplate, but support cam-like ops
class paddrModule(numEntries: Int, numRead: Int, numWrite: Int, numMatch: Int) extends XSModule {
  val io = IO(new Bundle {
    val raddr = Vec(numRead,  Input(UInt(log2Up(numEntries).W)))
    val rdata = Vec(numRead,  Output(UInt((PAddrBits-3).W)))
    val wen   = Vec(numWrite, Input(Bool()))
    val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
    val wdata = Vec(numWrite, Input(UInt((PAddrBits-3).W)))
    val mdata = Vec(numMatch, Input(UInt((PAddrBits-3).W)))
    val mmask = Vec(numMatch, Output(Vec(numEntries, Bool())))
  })

  val data = Mem(numEntries, UInt((PAddrBits-3).W))

  // read ports
  for (i <- 0 until numRead) {
    io.rdata(i) := data(io.raddr(i))
  }

  // below is the write ports (with priorities)
  for (i <- 0 until numWrite) {
    when (io.wen(i)) {
      data(io.waddr(i)) := io.wdata(i)
    }
  }
  
  // content addressed match
  for (i <- 0 until numMatch) {
    io.mmask(i) := 0.U.asBools
    for (j <- 0 until numEntries) {
      when (io.mdata(i) === data(j)) {
        io.mmask(i)(j) := true.B
      }
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

class maskModule(numEntries: Int, numRead: Int, numWrite: Int, numMatch: Int) extends XSModule {
  val io = IO(new Bundle {
    val raddr = Vec(numRead,  Input(UInt(log2Up(numEntries).W)))
    val rdata = Vec(numRead,  Output(UInt(8.W)))
    val wen   = Vec(numWrite, Input(Bool()))
    val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
    val wdata = Vec(numWrite, Input(UInt(8.W)))
    val mdata = Vec(numMatch, Input(UInt(8.W)))
    val mmask = Vec(numMatch, Output(Vec(numEntries, Bool())))
  })

  val data = Mem(numEntries, UInt(8.W))

  // read ports
  for (i <- 0 until numRead) {
    io.rdata(i) := data(io.raddr(i))
  }

  // below is the write ports (with priorities)
  for (i <- 0 until numWrite) {
    when (io.wen(i)) {
      data(io.waddr(i)) := io.wdata(i)
    }
  }
  
  // content addressed match
  for (i <- 0 until numMatch) {
    io.mmask(i) := 0.U.asBools
    for (j <- 0 until numEntries) {
      when ((io.mdata(i) & data(j)).orR) {
        io.mmask(i)(j) := true.B
      }
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

class exceptionModule(numEntries: Int, numRead: Int, numWrite: Int) extends XSModule {
  val io = IO(new Bundle {
    // read
    val raddr = Vec(numRead,  Input(UInt(log2Up(numEntries).W)))
    val rdata = Vec(numRead,  Output(UInt(16.W)))
    // address indexed write
    val wen   = Vec(numWrite, Input(Bool()))
    val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
    val wdata = Vec(numWrite, Input(UInt(16.W)))
    // masked write
    val mwmask = Input(Vec(numEntries, Bool()))
    val mwdata = Input(UInt(16.W))
  })

  val data = Mem(numEntries, UInt(16.W))

  // read ports
  for (i <- 0 until numRead) {
    io.rdata(i) := data(io.raddr(i))
  }

  // below is the write ports (with priorities)
  for (i <- 0 until numWrite) {
    when (io.wen(i)) {
      data(io.waddr(i)) := io.wdata(i)
    }
  }
  
  // masked write
  for (j <- 0 until numEntries) {
    when (io.mwmask(j)) {
      data(j) := io.mwdata
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

class coredataModule(numEntries: Int, numRead: Int, numWrite: Int) extends XSModule {
  val io = IO(new Bundle {
    // data io
    // read
    val raddr = Vec(numRead,  Input(UInt(log2Up(numEntries).W)))
    val rdata = Vec(numRead,  Output(UInt(XLEN.W)))
    // address indexed write
    val wen   = Vec(numWrite, Input(Bool()))
    val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
    val wdata = Vec(numWrite, Input(UInt(XLEN.W)))
    // masked write
    val mwmask = Input(Vec(numEntries, Bool()))
    val mwdata = Input(UInt(XLEN.W))
    
    // fwdMask io
    val fwdMaskWdata = Vec(numWrite, Input(UInt(8.W)))
  })

  val data = Mem(numEntries, UInt(XLEN.W))
  val fwdMask = Mem(numEntries, UInt(8.W))

  // read ports
  for (i <- 0 until numRead) {
    io.rdata(i) := data(io.raddr(i))
  }

  // below is the write ports (with priorities)
  for (i <- 0 until numWrite) {
    when (io.wen(i)) {
      data(io.waddr(i)) := io.wdata(i)
    }
  }

  // masked write
  // refill missed load
  def mergeRefillData(refill: UInt, fwd: UInt, fwdMask: UInt): UInt = {
    val res = Wire(Vec(8, UInt(8.W)))
    (0 until 8).foreach(i => {
      res(i) := Mux(fwdMask(i), fwd(8 * (i + 1) - 1, 8 * i), refill(8 * (i + 1) - 1, 8 * i))
    })
    res.asUInt
  }

  for (j <- 0 until numEntries) {
    when (io.mwmask(j)) {
      data(j) := mergeRefillData(io.mwdata, data(j), fwdMask(j))
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

class LoadQueueData(size: Int, wbNumRead: Int, wbNumWrite: Int) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val wb = new Bundle() {
      val wen = Vec(wbNumWrite, Input(Bool()))
      val waddr = Input(Vec(wbNumWrite, UInt(log2Up(size).W)))
      val wdata = Input(Vec(wbNumWrite, new LQDataEntry))
      val raddr = Input(Vec(wbNumRead, UInt(log2Up(size).W)))
      val rdata = Output(Vec(wbNumRead, new LQDataEntry))
    }
    val uncache = new Bundle() {
      val wen = Input(Bool())
      val waddr = Input(UInt(log2Up(size).W))
      val wdata = Input(UInt(XLEN.W)) // only write back uncache data
      val raddr = Input(UInt(log2Up(size).W))
      val rdata = Output(new LQDataEntry)
    }
    val refill = new Bundle() {
      val valid = Input(Bool())
      val paddr = Input(UInt(PAddrBits.W))
      val data = Input(UInt((cfg.blockBytes * 8).W))
      val refillMask = Input(Vec(size, Bool()))
      val matchMask = Output(Vec(size, Bool()))
    }
    val violation = Vec(StorePipelineWidth, new Bundle() {
      val paddr = Input(UInt(PAddrBits.W))
      val mask = Input(UInt(8.W))
      val violationMask = Output(Vec(size, Bool()))
    })
    val debug = Output(Vec(size, new LQDataEntry))

    def wbWrite(channel: Int, waddr: UInt, wdata: LQDataEntry): Unit = {
      require(channel < wbNumWrite && wbNumWrite >= 0)
      // need extra "this.wb(channel).wen := true.B"
      this.wb.waddr(channel) := waddr
      this.wb.wdata(channel) := wdata
    }

    def uncacheWrite(waddr: UInt, wdata: UInt): Unit = {
      // need extra "this.uncache.wen := true.B"
      this.uncache.waddr := waddr
      this.uncache.wdata := wdata
    }

    // def refillWrite(ldIdx: Int): Unit = {
    // }
    // use "this.refill.wen(ldIdx) := true.B" instead
  })

  val data = Reg(Vec(size, new LQDataEntry))

  // read data
  (0 until wbNumRead).map(i => {
    io.wb.rdata(i) := data(io.wb.raddr(i))
  })

  io.uncache.rdata := data(io.uncache.raddr)

  // writeback to lq/sq
  (0 until wbNumWrite).map(i => {
    when(io.wb.wen(i)){
      data(io.wb.waddr(i)) := io.wb.wdata(i)
    }
  })

  when(io.uncache.wen){
    data(io.uncache.waddr).data := io.uncache.wdata
  }

  // refill missed load
  def mergeRefillData(refill: UInt, fwd: UInt, fwdMask: UInt): UInt = {
    val res = Wire(Vec(8, UInt(8.W)))
    (0 until 8).foreach(i => {
      res(i) := Mux(fwdMask(i), fwd(8 * (i + 1) - 1, 8 * i), refill(8 * (i + 1) - 1, 8 * i))
    })
    res.asUInt
  }

  // split dcache result into words
  val words = VecInit((0 until blockWords) map { i => io.refill.data(DataBits * (i + 1) - 1, DataBits * i)})

  // gen paddr match mask
  (0 until size).map(i => {
    io.refill.matchMask(i) := get_block_addr(data(i).paddr) === get_block_addr(io.refill.paddr)
  })

  // refill data according to matchMask, refillMask and refill.valid
  (0 until size).map(i => {
    when(io.refill.valid && io.refill.matchMask(i) && io.refill.refillMask(i)){
      val refillData = words(get_word(data(i).paddr))
      data(i).data := mergeRefillData(refillData, data(i).data.asUInt, data(i).fwdMask.asUInt)
      XSDebug("miss resp: pos %d addr %x data %x + %x(%b)\n", i.U, data(i).paddr, refillData, data(i).data.asUInt, data(i).fwdMask.asUInt)
    }
  })

  // mem access violation check, gen violationMask
  (0 until StorePipelineWidth).map(i => {
    io.violation(i).violationMask := VecInit((0 until size).map(j => {
      val addrMatch = io.violation(i).paddr(PAddrBits - 1, 3) === data(j).paddr(PAddrBits - 1, 3)
      val violationVec = (0 until 8).map(k => data(j).mask(k) && io.violation(i).mask(k))
      Cat(violationVec).orR() && addrMatch
    }))
  })
    
  // debug data read
  io.debug := data
}
