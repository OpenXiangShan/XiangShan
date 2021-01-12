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


class LoadQueueData(size: Int, numRead: Int, numWrite: Int) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val wb = Vec(numWrite, new Bundle() {
      val wen = Input(Bool())
      val index = Input(UInt(log2Up(size).W))
      val wdata = Input(new LQDataEntry)
    })
    val uncache = new Bundle() {
      val wen = Input(Bool())
      val index = Input(UInt(log2Up(size).W))
      val wdata = Input(UInt(XLEN.W))
    }
    val refill = new Bundle() {
      val wen = Input(Vec(size, Bool()))
      val data = Input(UInt((cfg.blockBytes * 8).W))
    }
    val rdata = Output(Vec(size, new LQDataEntry))

    // val debug = new Bundle() {
    //   val debug_data = Vec(LoadQueueSize, new LQDataEntry)
    // }

    def wbWrite(channel: Int, index: UInt, wdata: LQDataEntry): Unit = {
      require(channel < numWrite && numWrite >= 0)
      // need extra "this.wb(channel).wen := true.B"
      this.wb(channel).index := index
      this.wb(channel).wdata := wdata
    }

    def uncacheWrite(index: UInt, wdata: UInt): Unit = {
      // need extra "this.uncache.wen := true.B"
      this.uncache.index := index
      this.uncache.wdata := wdata
    }

    // def refillWrite(ldIdx: Int): Unit = {
    // }
    // use "this.refill.wen(ldIdx) := true.B" instead
  })

  io := DontCare

  val data = Reg(Vec(size, new LQDataEntry))

  // writeback to lq/sq
  (0 until 2).map(i => {
    when(io.wb(i).wen){
      data(io.wb(i).index) := io.wb(i).wdata
    }
  })

  when(io.uncache.wen){
    data(io.uncache.index).data := io.uncache.wdata
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


  (0 until size).map(i => {
    when(io.refill.wen(i) ){
      val refillData = words(get_word(data(i).paddr))
      data(i).data := mergeRefillData(refillData, data(i).data.asUInt, data(i).fwdMask.asUInt)
      XSDebug("miss resp: pos %d addr %x data %x + %x(%b)\n", i.U, data(i).paddr, refillData, data(i).data.asUInt, data(i).fwdMask.asUInt)
    }
  })

  // data read
  io.rdata := data
  // io.debug.debug_data := data
}
