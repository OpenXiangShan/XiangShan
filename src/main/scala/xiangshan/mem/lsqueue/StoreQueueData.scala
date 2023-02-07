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
import utility._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, MemoryOpConstants}
import xiangshan.mem._
import xiangshan.backend.rob.RobPtr


// Data module define
// These data modules are like SyncDataModuleTemplate, but support cam-like ops
class SQAddrModule(dataWidth: Int, numEntries: Int, numRead: Int, numWrite: Int, numForward: Int)(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle {
    // sync read
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, UInt(dataWidth.W))) // rdata: store addr
    val rlineflag = Output(Vec(numRead, Bool())) // rdata: line op flag
    // write
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, UInt(dataWidth.W))) // wdata: store addr
    val wlineflag = Input(Vec(numWrite, Bool())) // wdata: line op flag
    // forward addr cam
    val forwardMdata = Input(Vec(numForward, UInt(dataWidth.W))) // addr
    val forwardMmask = Output(Vec(numForward, Vec(numEntries, Bool()))) // cam result mask
    // debug
    val debug_data = Output(Vec(numEntries, UInt(dataWidth.W)))
  })

  val data = Reg(Vec(numEntries, UInt(dataWidth.W)))
  val lineflag = Reg(Vec(numEntries, Bool())) // cache line match flag
  // if lineflag == true, this address points to a whole cacheline
  io.debug_data := data

  // read ports
  for (i <- 0 until numRead) {
    io.rdata(i) := data(RegNext(io.raddr(i)))
    io.rlineflag(i) := lineflag(RegNext(io.raddr(i)))
  }

  // below is the write ports (with priorities)
  for (i <- 0 until numWrite) {
    when (io.wen(i)) {
      data(io.waddr(i)) := io.wdata(i)
      lineflag(io.waddr(i)) := io.wlineflag(i)
    }
  }

  // content addressed match
  for (i <- 0 until numForward) {
    for (j <- 0 until numEntries) {
      // io.forwardMmask(i)(j) := io.forwardMdata(i)(dataWidth-1, 3) === data(j)(dataWidth-1, 3)
      val linehit = io.forwardMdata(i)(dataWidth-1, DCacheLineOffset) === data(j)(dataWidth-1, DCacheLineOffset)
      val hit128bit = io.forwardMdata(i)(DCacheLineOffset-1, DCacheVWordOffset) === data(j)(DCacheLineOffset-1, DCacheVWordOffset)
      //val hit64bit = io.forwardMdata(i)(DCacheLineOffset-1, DCacheWordOffset) === data(j)(DCacheLineOffset-1, DCacheWordOffset)
      //io.forwardMmask(i)(j) := linehit && ( (hit128bit & 128bitReq) || (hit64bit & ~128bitReq) || lineflag(j))
      io.forwardMmask(i)(j) := linehit && (hit128bit || lineflag(j))//TODO: Cause performance degradation

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
  val valid = Bool() // this byte is valid
  val data = UInt((XLEN/8).W)
}

class SQData8Module(numEntries: Int, numRead: Int, numWrite: Int, numForward: Int)(implicit p: Parameters) extends XSModule 
  with HasDCacheParameters 
  with HasCircularQueuePtrHelper 
{
  val io = IO(new Bundle() {
    // sync read port
    val raddr = Vec(numRead, Input(UInt(log2Up(numEntries).W)))
    val rdata = Vec(numRead, Output(new SQData8Entry))
    // data write port
    val data = new Bundle() {
      val wen   = Vec(numWrite, Input(Bool()))
      val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
      val wdata = Vec(numWrite, Input(UInt((XLEN/8).W)))
    }
    // mask (data valid) write port
    val mask = new Bundle() {
      val wen   = Vec(numWrite, Input(Bool()))
      val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
      val wdata = Vec(numWrite, Input(Bool()))
    }

    // st-ld forward addr cam result input, used to select forward data
    val needForward = Input(Vec(numForward, Vec(2, UInt(numEntries.W))))
    // forward result valid bit generated in current cycle
    val forwardValidFast = Vec(numForward, Output(Bool()))
    // forward result generated in the next cycle
    val forwardValid = Vec(numForward, Output(Bool())) // forwardValid = RegNext(forwardValidFast)
    val forwardData = Vec(numForward, Output(UInt(8.W)))
  })

  io := DontCare

  val data = Reg(Vec(numEntries, new SQData8Entry))

  require(isPow2(StoreQueueNWriteBanks))
  require(StoreQueueNWriteBanks > 1)
  def get_bank(in: UInt): UInt = in(log2Up(StoreQueueNWriteBanks) -1, 0)
  def get_bank_index(in: UInt): UInt = in >> log2Up(StoreQueueNWriteBanks)
  def get_vec_index(index: Int, bank: Int): Int = {
    (index << log2Up(StoreQueueNWriteBanks)) + bank
  }

  // writeback to sq
  // store queue data write takes 2 cycles
  // (0 until numWrite).map(i => {
  //   when(RegNext(io.data.wen(i))){
  //     data(RegNext(io.data.waddr(i))).data := RegNext(io.data.wdata(i))
  //   }
  // })
  (0 until numWrite).map(i => {
    (0 until StoreQueueNWriteBanks).map(bank => {
      val s0_wen = io.data.wen(i) && get_bank(io.data.waddr(i)) === bank.U
      val s1_wen = RegNext(s0_wen)
      val s1_wdata = RegEnable(io.data.wdata(i), s0_wen)
      val s1_waddr = RegEnable(get_bank_index(io.data.waddr(i)), s0_wen)
      val numRegsPerBank = StoreQueueSize / StoreQueueNWriteBanks
      (0 until numRegsPerBank).map(index => {
        when(s1_wen && s1_waddr === index.U){
          data(get_vec_index(index, bank)).data := s1_wdata
        }
      })
      s0_wen.suggestName("data_s0_wen_" + i +"_bank_" + bank)
      s1_wen.suggestName("data_s1_wen_" + i +"_bank_" + bank)
      s1_wdata.suggestName("data_s1_wdata_" + i +"_bank_" + bank)
      s1_waddr.suggestName("data_s1_waddr_" + i +"_bank_" + bank)
    })
  })

  // (0 until numWrite).map(i => {
  //   when(RegNext(io.mask.wen(i))){
  //     data(RegNext(io.mask.waddr(i))).valid := RegNext(io.mask.wdata(i))
  //   }
  // })
  (0 until numWrite).map(i => {
    (0 until StoreQueueNWriteBanks).map(bank => {
      val s0_wen = io.mask.wen(i) && get_bank(io.mask.waddr(i)) === bank.U
      val s1_wen = RegNext(s0_wen)
      val s1_wdata = RegEnable(io.mask.wdata(i), s0_wen)
      val s1_waddr = RegEnable(get_bank_index(io.mask.waddr(i)), s0_wen)
      val numRegsPerBank = StoreQueueSize / StoreQueueNWriteBanks
      (0 until numRegsPerBank).map(index => {
        when(s1_wen && s1_waddr === index.U){
          data(get_vec_index(index, bank)).valid := s1_wdata
        }
      })
      s0_wen.suggestName("mask_s0_wen_" + i +"_bank_" + bank)
      s1_wen.suggestName("mask_s1_wen_" + i +"_bank_" + bank)
      s1_wdata.suggestName("mask_s1_wdata_" + i +"_bank_" + bank)
      s1_waddr.suggestName("mask_s1_waddr_" + i +"_bank_" + bank)
    })
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
  val mask = UInt((VLEN/8).W)
  val data = UInt(VLEN.W)
}

// SQDataModule is a wrapper of SQData8Modules
class SQDataModule(numEntries: Int, numRead: Int, numWrite: Int, numForward: Int)(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    // sync read port
    val raddr = Vec(numRead,  Input(UInt(log2Up(numEntries).W)))
    val rdata = Vec(numRead,  Output(new SQDataEntry))
    // data write port
    val data = new Bundle() {
      val wen   = Vec(numWrite, Input(Bool()))
      val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
      val wdata = Vec(numWrite, Input(UInt(VLEN.W)))
    }
    // mask (data valid) write port
    val mask = new Bundle() {
      val wen   = Vec(numWrite, Input(Bool()))
      val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
      val wdata = Vec(numWrite, Input(UInt((VLEN/8).W)))
    }

    // st-ld forward addr cam result input, used to select forward data
    val needForward = Input(Vec(numForward, Vec(2, UInt(numEntries.W))))
    // forward result valid bit generated in current cycle
    val forwardMaskFast = Vec(numForward, Output(Vec((VLEN/8), Bool())))
    // forward result generated in the next cycle
    val forwardMask = Vec(numForward, Output(Vec((VLEN/8), Bool()))) // forwardMask = RegNext(forwardMaskFast)
    val forwardData = Vec(numForward, Output(Vec((VLEN/8), UInt(8.W))))
  })

  val data16 = Seq.fill(16)(Module(new SQData8Module(numEntries, numRead, numWrite, numForward)))

  // writeback to lq/sq
  for (i <- 0 until numWrite) {
    // write to data16
    for (j <- 0 until 16) {
      data16(j).io.mask.waddr(i) := io.mask.waddr(i)
      data16(j).io.mask.wdata(i) := io.mask.wdata(i)(j)
      data16(j).io.mask.wen(i)   := io.mask.wen(i)
      data16(j).io.data.waddr(i) := io.data.waddr(i)
      data16(j).io.data.wdata(i) := io.data.wdata(i)(8*(j+1)-1, 8*j)
      data16(j).io.data.wen(i)   := io.data.wen(i)
    }
  }

  // destorequeue read data
  for (i <- 0 until numRead) {
    for (j <- 0 until 16) {
      data16(j).io.raddr(i) := io.raddr(i)
    }
    io.rdata(i).mask := VecInit((0 until 16).map(j => data16(j).io.rdata(i).valid)).asUInt
    io.rdata(i).data := VecInit((0 until 16).map(j => data16(j).io.rdata(i).data)).asUInt
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
    for (j <- 0 until 16) {
      data16(j).io.needForward(i) <> io.needForward(i)
      io.forwardMaskFast(i) := VecInit((0 until 16).map(j => data16(j).io.forwardValidFast(i)))
      io.forwardMask(i) := VecInit((0 until 16).map(j => data16(j).io.forwardValid(i)))
      io.forwardData(i) := VecInit((0 until 16).map(j => data16(j).io.forwardData(i)))
    }
  })
}
