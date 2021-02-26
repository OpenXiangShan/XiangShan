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


// Data module define
// These data modules are like SyncDataModuleTemplate, but support cam-like ops
class SQPaddrModule(numEntries: Int, numRead: Int, numWrite: Int, numForward: Int) extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle {
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, UInt((PAddrBits).W)))
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, UInt((PAddrBits).W)))
    val forwardMdata = Input(Vec(numForward, UInt((PAddrBits).W)))
    val forwardMmask = Output(Vec(numForward, Vec(numEntries, Bool())))
  })

  val data = Reg(Vec(numEntries, UInt((PAddrBits).W)))

  // read ports
  for (i <- 0 until numRead) {
    io.rdata(i) := data(RegNext(io.raddr(i)))
  }

  // below is the write ports (with priorities)
  for (i <- 0 until numWrite) {
    when (io.wen(i)) {
      data(io.waddr(i)) := io.wdata(i)
    }
  }
  
  // content addressed match
  for (i <- 0 until numForward) {
    for (j <- 0 until numEntries) {
      io.forwardMmask(i)(j) := io.forwardMdata(i)(PAddrBits-1, 3) === data(j)(PAddrBits-1, 3)
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

class SQData8Entry extends XSBundle {
  // val paddr = UInt(PAddrBits.W)
  val valid = Bool()
  val data = UInt((XLEN/8).W)
}

class SQData8Module(size: Int, numRead: Int, numWrite: Int, numForward: Int) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val raddr = Vec(numRead,  Input(UInt(log2Up(size).W)))
    val rdata = Vec(numRead,  Output(new SQData8Entry))
    val wen   = Vec(numWrite, Input(Bool()))
    val waddr = Vec(numWrite, Input(UInt(log2Up(size).W)))
    val wdata = Vec(numWrite, Input(new SQData8Entry))

    val needForward = Input(Vec(numForward, Vec(2, UInt(size.W))))
    val forwardValid = Vec(numForward, Output(Bool()))
    val forwardData = Vec(numForward, Output(UInt(8.W)))
  })

  io := DontCare

  val data = Reg(Vec(size, new SQData8Entry))

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
    // parallel fwd logic
    val matchResultVec = Wire(Vec(size * 2, new FwdEntry))

    def parallelFwd(xs: Seq[Data]): Data = {
      ParallelOperation(xs, (a: Data, b: Data) => {
        val l = a.asTypeOf(new FwdEntry)
        val r = b.asTypeOf(new FwdEntry)
        val res = Wire(new FwdEntry)
        res.valid := l.valid || r.valid
        res.data := Mux(r.valid, r.data, l.data)
        res
      })
    }

    // paddrMatch is now included in io.needForward
    // for (j <- 0 until size) {
    //   paddrMatch(j) := io.forward(i).paddr(PAddrBits - 1, 3) === data(j).paddr(PAddrBits - 1, 3)
    // }

    for (j <- 0 until size) {
      val needCheck0 = RegNext(io.needForward(i)(0)(j))
      val needCheck1 = RegNext(io.needForward(i)(1)(j))
      (0 until XLEN / 8).foreach(k => {
        matchResultVec(j).valid := needCheck0 && data(j).valid
        matchResultVec(j).data := data(j).data
        matchResultVec(size + j).valid := needCheck1 && data(j).valid
        matchResultVec(size + j).data := data(j).data
      })
    }

    val parallelFwdResult = parallelFwd(matchResultVec).asTypeOf(new FwdEntry)

    io.forwardValid(i) := parallelFwdResult.valid
    io.forwardData(i) := parallelFwdResult.data

  })
}

class SQDataEntry extends XSBundle {
  // val paddr = UInt(PAddrBits.W)
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
}

class StoreQueueData(size: Int, numRead: Int, numWrite: Int, numForward: Int) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val raddr = Vec(numRead,  Input(UInt(log2Up(size).W)))
    val rdata = Vec(numRead,  Output(new SQDataEntry))
    val wen   = Vec(numWrite, Input(Bool()))
    val waddr = Vec(numWrite, Input(UInt(log2Up(size).W)))
    val wdata = Vec(numWrite, Input(new SQDataEntry))

    val needForward = Input(Vec(numForward, Vec(2, UInt(size.W))))
    val forwardMask = Vec(numForward, Output(Vec(8, Bool())))
    val forwardData = Vec(numForward, Output(Vec(8, UInt(8.W))))
  })

  val data8 = Seq.fill(8)(Module(new SQData8Module(size, numRead, numWrite, numForward)))

  // writeback to lq/sq
  for (i <- 0 until numWrite) {
    // write to data8
    for (j <- 0 until 8) {
      data8(j).io.waddr(i) := io.waddr(i)
      data8(j).io.wdata(i).valid := io.wdata(i).mask(j)
      data8(j).io.wdata(i).data := io.wdata(i).data(8*(j+1)-1, 8*j)
      data8(j).io.wen(i) := io.wen(i)
    }
  }

  // destorequeue read data
  for (i <- 0 until numRead) {
    for (j <- 0 until 8) {
      data8(j).io.raddr(i) := io.raddr(i)
    }
    io.rdata(i).mask := VecInit((0 until 8).map(j => data8(j).io.rdata(i).valid)).asUInt
    io.rdata(i).data := VecInit((0 until 8).map(j => data8(j).io.rdata(i).data)).asUInt
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }

  (0 until numForward).map(i => {
    // parallel fwd logic
    for (j <- 0 until 8) {
      data8(j).io.needForward(i) <> io.needForward(i)
      io.forwardMask(i) := VecInit((0 until 8).map(j => data8(j).io.forwardValid(i)))
      io.forwardData(i) := VecInit((0 until 8).map(j => data8(j).io.forwardData(i)))
    }
  })
}