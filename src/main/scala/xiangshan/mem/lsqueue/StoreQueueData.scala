/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, MemoryOpConstants}
import xiangshan.mem._
import xiangshan.backend.roq.RoqPtr


// Data module define
// These data modules are like SyncDataModuleTemplate, but support cam-like ops
class SQAddrModule(dataWidth: Int, numEntries: Int, numRead: Int, numWrite: Int, numForward: Int)(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle {
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, UInt(dataWidth.W)))
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, UInt(dataWidth.W)))
    val forwardMdata = Input(Vec(numForward, UInt(dataWidth.W)))
    val forwardMmask = Output(Vec(numForward, Vec(numEntries, Bool())))
    val debug_data = Output(Vec(numEntries, UInt(dataWidth.W)))
  })

  val data = Reg(Vec(numEntries, UInt(dataWidth.W)))
  io.debug_data := data

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
      io.forwardMmask(i)(j) := io.forwardMdata(i)(dataWidth-1, 3) === data(j)(dataWidth-1, 3)
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

class SQData8Entry(implicit p: Parameters) extends XSBundle {
  // val paddr = UInt(PAddrBits.W)
  val valid = Bool()
  val data = UInt((XLEN/8).W)
}

class SQData8Module(numEntries: Int, numRead: Int, numWrite: Int, numForward: Int)(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val raddr = Vec(numRead,  Input(UInt(log2Up(numEntries).W)))
    val rdata = Vec(numRead,  Output(new SQData8Entry))
    val data = new Bundle() {
      val wen   = Vec(numWrite, Input(Bool()))
      val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
      val wdata = Vec(numWrite, Input(UInt((XLEN/8).W)))
    }
    val mask = new Bundle() {
      val wen   = Vec(numWrite, Input(Bool()))
      val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
      val wdata = Vec(numWrite, Input(Bool()))
    }

    val needForward = Input(Vec(numForward, Vec(2, UInt(numEntries.W))))
    val forwardValidFast = Vec(numForward, Output(Bool()))
    val forwardValid = Vec(numForward, Output(Bool()))
    val forwardData = Vec(numForward, Output(UInt(8.W)))
  })

  io := DontCare

  val data = Reg(Vec(numEntries, new SQData8Entry))

  // writeback to sq
  (0 until numWrite).map(i => {
    when(io.data.wen(i)){
      data(io.data.waddr(i)).data := io.data.wdata(i)
    }
  })
  (0 until numWrite).map(i => {
    when(io.mask.wen(i)){
      data(io.mask.waddr(i)).valid := io.mask.wdata(i)
    }
  })

  // destorequeue read data
  (0 until numRead).map(i => {
      io.rdata(i) := data(RegNext(io.raddr(i)))
  })

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.data.wen(i) && io.data.wen(j) && io.data.waddr(i) === io.data.waddr(j)))
    }
  }
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.mask.wen(i) && io.mask.wen(j) && io.mask.waddr(i) === io.mask.waddr(j)))
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
    val matchResultVec = Wire(Vec(numEntries * 2, new FwdEntry))

    def parallelFwd(xs: Seq[Data]): Data = {
      ParallelOperation(xs, (a: Data, b: Data) => {
        val l = a.asTypeOf(new FwdEntry)
        val r = b.asTypeOf(new FwdEntry)
        val res = Wire(new FwdEntry)
        res.validFast := l.validFast || r.validFast
        res.valid := l.valid || r.valid
        // res.valid := RegNext(res.validFast)
        res.data := Mux(r.valid, r.data, l.data)
        res
      })
    }

    for (j <- 0 until numEntries) {
      val needCheck0 = io.needForward(i)(0)(j)
      val needCheck1 = io.needForward(i)(1)(j)
      val needCheck0Reg = RegNext(needCheck0)
      val needCheck1Reg = RegNext(needCheck1)
      (0 until XLEN / 8).foreach(k => {
        matchResultVec(j).validFast := needCheck0 && data(j).valid
        matchResultVec(j).valid := needCheck0Reg && data(j).valid
        matchResultVec(j).data := data(j).data
        matchResultVec(numEntries + j).validFast := needCheck1 && data(j).valid
        matchResultVec(numEntries + j).valid := needCheck1Reg && data(j).valid
        matchResultVec(numEntries + j).data := data(j).data
      })
    }

    val parallelFwdResult = parallelFwd(matchResultVec).asTypeOf(new FwdEntry)
  
    // validFast is generated the same cycle with query
    io.forwardValidFast(i) := parallelFwdResult.validFast
    // valid is generated 1 cycle after query request
    io.forwardValid(i) := parallelFwdResult.valid
    // data is generated 1 cycle after query request
    io.forwardData(i) := parallelFwdResult.data

  })
}

class SQDataEntry(implicit p: Parameters) extends XSBundle {
  // val paddr = UInt(PAddrBits.W)
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
}

class SQDataModule(numEntries: Int, numRead: Int, numWrite: Int, numForward: Int)(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val raddr = Vec(numRead,  Input(UInt(log2Up(numEntries).W)))
    val rdata = Vec(numRead,  Output(new SQDataEntry))
    val data = new Bundle() {
      val wen   = Vec(numWrite, Input(Bool()))
      val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
      val wdata = Vec(numWrite, Input(UInt(XLEN.W)))
    }
    val mask = new Bundle() {
      val wen   = Vec(numWrite, Input(Bool()))
      val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
      val wdata = Vec(numWrite, Input(UInt(8.W)))
    }

    val needForward = Input(Vec(numForward, Vec(2, UInt(numEntries.W))))
    val forwardMaskFast = Vec(numForward, Output(Vec(8, Bool())))
    val forwardMask = Vec(numForward, Output(Vec(8, Bool())))
    val forwardData = Vec(numForward, Output(Vec(8, UInt(8.W))))
  })

  val data8 = Seq.fill(8)(Module(new SQData8Module(numEntries, numRead, numWrite, numForward)))

  // writeback to lq/sq
  for (i <- 0 until numWrite) {
    // write to data8
    for (j <- 0 until 8) {
      data8(j).io.mask.waddr(i) := io.mask.waddr(i)
      data8(j).io.mask.wdata(i) := io.mask.wdata(i)(j)
      data8(j).io.mask.wen(i)   := io.mask.wen(i)
      data8(j).io.data.waddr(i) := io.data.waddr(i)
      data8(j).io.data.wdata(i) := io.data.wdata(i)(8*(j+1)-1, 8*j)
      data8(j).io.data.wen(i)   := io.data.wen(i)
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
      assert(!(io.data.wen(i) && io.data.wen(j) && io.data.waddr(i) === io.data.waddr(j)))
    }
  }
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.mask.wen(i) && io.mask.wen(j) && io.mask.waddr(i) === io.mask.waddr(j)))
    }
  }

  (0 until numForward).map(i => {
    // parallel fwd logic
    for (j <- 0 until 8) {
      data8(j).io.needForward(i) <> io.needForward(i)
      io.forwardMaskFast(i) := VecInit((0 until 8).map(j => data8(j).io.forwardValidFast(i)))
      io.forwardMask(i) := VecInit((0 until 8).map(j => data8(j).io.forwardValid(i)))
      io.forwardData(i) := VecInit((0 until 8).map(j => data8(j).io.forwardData(i)))
    }
  })
}
