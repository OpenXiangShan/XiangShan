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


class SQDataEntry extends XSBundle {
//   val vaddr = UInt(VAddrBits.W) // TODO: need opt
  val paddr = UInt(PAddrBits.W)
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
//   val exception = UInt(16.W) // TODO: opt size
}

class StoreQueueData(size: Int, numRead: Int, numWrite: Int, numForward: Int) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val raddr = Vec(numRead,  Input(UInt(log2Up(size).W)))
    val rdata = Vec(numRead,  Output(new SQDataEntry))
    val wen   = Vec(numWrite, Input(Bool()))
    val waddr = Vec(numWrite, Input(UInt(log2Up(size).W)))
    val wdata = Vec(numWrite, Input(new SQDataEntry))
    val debug = Vec(size, Output(new SQDataEntry))

    val needForward = Input(Vec(numForward, Vec(2, UInt(size.W))))
    val forward = Vec(numForward, Flipped(new LoadForwardQueryIO))

    def forwardQuery(numForward: Int, paddr: UInt, needForward1: Data, needForward2: Data): Unit = {
      this.needForward(numForward)(0) := needForward1
      this.needForward(numForward)(1) := needForward2
      this.forward(numForward).paddr := paddr
    }
  })

  io := DontCare

  val data = Reg(Vec(size, new SQDataEntry))

  // writeback to lq/sq
  (0 until numWrite).map(i => {
    when(io.wen(i)){
      data(io.waddr(i)) := io.wdata(i)
    }
  })

  // destorequeue read data
  (0 until numRead).map(i => {
      io.rdata(i) := data(RegNext(io.raddr(i)))
  })

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }

  // forwarding
  // Compare ringBufferTail (deqPtr) and forward.sqIdx, we have two cases:
  // (1) if they have the same flag, we need to check range(tail, sqIdx)
  // (2) if they have different flags, we need to check range(tail, LoadQueueSize) and range(0, sqIdx)
  // Forward1: Mux(same_flag, range(tail, sqIdx), range(tail, LoadQueueSize))
  // Forward2: Mux(same_flag, 0.U,                   range(0, sqIdx)    )
  // i.e. forward1 is the target entries with the same flag bits and forward2 otherwise

  // entry with larger index should have higher priority since it's data is younger

  (0 until numForward).map(i => {
    val forwardMask1 = WireInit(VecInit(Seq.fill(8)(false.B)))
    val forwardData1 = WireInit(VecInit(Seq.fill(8)(0.U(8.W))))
    val forwardMask2 = WireInit(VecInit(Seq.fill(8)(false.B)))
    val forwardData2 = WireInit(VecInit(Seq.fill(8)(0.U(8.W))))

    for (j <- 0 until size) {
      val needCheck = io.forward(i).paddr(PAddrBits - 1, 3) === data(j).paddr(PAddrBits - 1, 3)
      (0 until XLEN / 8).foreach(k => {
        when (needCheck && data(j).mask(k)) {
          when (io.needForward(i)(0)(j)) {
            forwardMask1(k) := true.B
            forwardData1(k) := data(j).data(8 * (k + 1) - 1, 8 * k)
          }
          when (io.needForward(i)(1)(j)) {
            forwardMask2(k) := true.B
            forwardData2(k) := data(j).data(8 * (k + 1) - 1, 8 * k)
          }
          XSDebug(io.needForward(i)(0)(j) || io.needForward(i)(1)(j),
            p"forwarding $k-th byte ${Hexadecimal(data(j).data(8 * (k + 1) - 1, 8 * k))} " +
            p"from ptr $j\n")
        }
      })
    }

    // parallel fwd logic
    val paddrMatch = Wire(Vec(size, Bool()))
    val matchResultVec = Wire(Vec(size * 2, new FwdEntry))

    def parallelFwd(xs: Seq[Data]): Data = {
      ParallelOperation(xs, (a: Data, b: Data) => {
        val l = a.asTypeOf(new FwdEntry)
        val r = b.asTypeOf(new FwdEntry)
        val res = Wire(new FwdEntry)
        (0 until 8).map(p => {
          res.mask(p) := l.mask(p) || r.mask(p)
          res.data(p) := Mux(r.mask(p), r.data(p), l.data(p))
        })
        res
      })
    }

    for (j <- 0 until size) {
      paddrMatch(j) := io.forward(i).paddr(PAddrBits - 1, 3) === data(j).paddr(PAddrBits - 1, 3)
    }

    for (j <- 0 until size) {
      val needCheck0 = RegNext(paddrMatch(j) && io.needForward(i)(0)(j))
      val needCheck1 = RegNext(paddrMatch(j) && io.needForward(i)(1)(j))
      (0 until XLEN / 8).foreach(k => {
        matchResultVec(j).mask(k) := needCheck0 && data(j).mask(k)
        matchResultVec(j).data(k) := data(j).data(8 * (k + 1) - 1, 8 * k)
        matchResultVec(size + j).mask(k) := needCheck1 && data(j).mask(k)
        matchResultVec(size + j).data(k) := data(j).data(8 * (k + 1) - 1, 8 * k)
      })
    }

    val parallelFwdResult = parallelFwd(matchResultVec).asTypeOf(new FwdEntry)

    io.forward(i).forwardMask := parallelFwdResult.mask
    io.forward(i).forwardData := parallelFwdResult.data

  })

  io.debug := data
}