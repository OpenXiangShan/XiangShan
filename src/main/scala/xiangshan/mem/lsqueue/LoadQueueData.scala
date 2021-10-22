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
import xiangshan.backend.rob.RobPtr

class LQDataEntry(implicit p: Parameters) extends XSBundle {
  // val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
  val fwdMask = Vec(8, Bool())
}

// Data module define
// These data modules are like SyncDataModuleTemplate, but support cam-like ops
class LQPaddrModule(numEntries: Int, numRead: Int, numWrite: Int)(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle {
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, UInt((PAddrBits).W)))
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, UInt((PAddrBits).W)))
    val violationMdata = Input(Vec(StorePipelineWidth, UInt((PAddrBits).W)))
    val violationMmask = Output(Vec(StorePipelineWidth, Vec(numEntries, Bool())))
    val releaseMdata = Input(Vec(LoadPipelineWidth, UInt((PAddrBits).W)))
    val releaseMmask = Output(Vec(LoadPipelineWidth, Vec(numEntries, Bool())))
    val refillMdata = Input(UInt((PAddrBits).W))
    val refillMmask = Output(Vec(numEntries, Bool()))
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
  for (i <- 0 until StorePipelineWidth) {
    for (j <- 0 until numEntries) {
      io.violationMmask(i)(j) := io.violationMdata(i)(PAddrBits-1, DCacheIndexOffset) === data(j)(PAddrBits-1, DCacheIndexOffset)
    }
  }
  for (i <- 0 until LoadPipelineWidth) {
    for (j <- 0 until numEntries) {
      io.releaseMmask(i)(j) := io.releaseMdata(i)(PAddrBits-1, DCacheTagOffset) === data(j)(PAddrBits-1, DCacheTagOffset)
    }
  }

  for (j <- 0 until numEntries) {
    io.refillMmask(j) := get_refill_addr(io.refillMdata) === get_refill_addr(data(j))
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

class MaskModule(numEntries: Int, numRead: Int, numWrite: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, UInt(8.W)))
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, UInt(8.W)))
    val violationMdata = Input(Vec(StorePipelineWidth, UInt((PAddrBits).W)))
    val violationMmask = Output(Vec(StorePipelineWidth, Vec(numEntries, Bool())))
  })

  val data = Reg(Vec(numEntries, UInt(8.W)))

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
  for (i <- 0 until StorePipelineWidth) {
    for (j <- 0 until numEntries) {
      io.violationMmask(i)(j) := (io.violationMdata(i) & data(j)).orR
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

// class LQData8Module(numEntries: Int, numRead: Int, numWrite: Int) extends XSModule with HasDCacheParameters {
//   val io = IO(new Bundle {
//     // read
//     val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
//     val rdata = Output(Vec(numRead, UInt(8.W)))
//     // address indexed write
//     val wen   = Input(Vec(numWrite, Bool()))
//     val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
//     val wdata = Input(Vec(numWrite, UInt(8.W)))
//     // masked write
//     val mwmask = Input(Vec(blockWords, Vec(numEntries, Bool())))
//     val mwdata = Input(Vec(blockWords, UInt(8.W)))
//   })

//   val data = Reg(Vec(numEntries, UInt(8.W)))

//   // read ports
//   for (i <- 0 until numRead) {
//     io.rdata(i) := data(RegNext(io.raddr(i)))
//   }

//   // below is the write ports (with priorities)
//   for (i <- 0 until numWrite) {
//     when (io.wen(i)) {
//       data(io.waddr(i)) := io.wdata(i)
//     }
//   }

//   // masked write
//   for (j <- 0 until numEntries) {
//     val wen = VecInit((0 until blockWords).map(i => io.mwmask(i)(j))).asUInt.orR
//     when (wen) {
//       data(j) := VecInit((0 until blockWords).map(i => {
//         Mux(io.mwmask(i)(j), io.mwdata(i), 0.U)
//       })).reduce(_ | _)
//     }
//   }

//   // DataModuleTemplate should not be used when there're any write conflicts
//   for (i <- 0 until numWrite) {
//     for (j <- i+1 until numWrite) {
//       assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
//     }
//   }
// }

class CoredataModule(numEntries: Int, numRead: Int, numWrite: Int)(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle {
    // data io
    // read
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, UInt(XLEN.W)))
    // address indexed write
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, UInt(XLEN.W)))
    // masked write
    val mwmask = Input(Vec(numEntries, Bool()))
    val refillData = Input(UInt(l1BusDataWidth.W))

    // fwdMask io
    val fwdMaskWdata = Input(Vec(numWrite, UInt(8.W)))
    val fwdMaskWen = Input(Vec(numWrite, Bool()))
    // fwdMaskWaddr = waddr

    // paddr io
    // refillOffBits - wordOffBits bits in paddr need to be stored in CoredataModule for refilling
    val paddrWdata = Input(Vec(numWrite, UInt((PAddrBits).W)))
    val paddrWen = Input(Vec(numWrite, Bool()))
  })

  val data8 = Seq.fill(8)(Module(new MaskedSyncDataModuleTemplate(UInt(8.W), numEntries, numRead, numWrite, numMWrite = refillWords)))
  val fwdMask = Reg(Vec(numEntries, UInt(8.W)))
  val wordIndex = Reg(Vec(numEntries, UInt((refillOffBits - wordOffBits).W)))

  // read ports
  for (i <- 0 until numRead) {
    for (j <- 0 until 8) {
      data8(j).io.raddr(i) := io.raddr(i)
    }
    io.rdata(i) := VecInit((0 until 8).map(j => data8(j).io.rdata(i))).asUInt
  }

  // below is the write ports (with priorities)
  for (i <- 0 until numWrite) {
    // write to data8
    for (j <- 0 until 8) {
      data8(j).io.waddr(i) := io.waddr(i)
      data8(j).io.wdata(i) := io.wdata(i)(8*(j+1)-1, 8*j)
      data8(j).io.wen(i) := io.wen(i)
    }

    // write ctrl info
    when (io.fwdMaskWen(i)) {
      fwdMask(io.waddr(i)) := io.fwdMaskWdata(i)
    }
    when (io.paddrWen(i)) {
      wordIndex(io.waddr(i)) := get_word(io.paddrWdata(i))
    }
  }

  // write refilled data to data8

  // select refill data
  // split dcache result into words
  val words = VecInit((0 until refillWords) map { i => io.refillData(DataBits * (i + 1) - 1, DataBits * i)})
  // select refill data according to wordIndex (paddr)
  for (i <- 0 until 8) {
    for (j <- 0 until refillWords) {
      data8(i).io.mwdata(j) := words(j)(8*(i+1)-1, 8*i)
    }
  }

  // gen refill wmask
  for (j <- 0 until refillWords) {
    for (k <- 0 until numEntries) {
      val wordMatch = wordIndex(k) === j.U
      for (i <- 0 until 8) {
        data8(i).io.mwmask(j)(k) := wordMatch && io.mwmask(k) && !fwdMask(k)(i)
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

class LoadQueueData(size: Int, wbNumRead: Int, wbNumWrite: Int)(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
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
      val data = Input(UInt(l1BusDataWidth.W))
      val refillMask = Input(Vec(size, Bool()))
      val matchMask = Output(Vec(size, Bool()))
    }
    // st-ld violation query, word level cam
    val violation = Vec(StorePipelineWidth, new Bundle() {
      val paddr = Input(UInt(PAddrBits.W))
      val mask = Input(UInt(8.W))
      val violationMask = Output(Vec(size, Bool()))
    })
    // ld-ld violation query, cache line level cam
    val release_violation = Vec(LoadPipelineWidth, new Bundle() {
      val paddr = Input(UInt(PAddrBits.W))
      val match_mask = Output(Vec(size, Bool()))
      // if ld-ld violation does happened, we replay from the elder load
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

  // val data = Reg(Vec(size, new LQDataEntry))
  // data module
  val paddrModule = Module(new LQPaddrModule(size, numRead = LoadPipelineWidth+1, numWrite = LoadPipelineWidth))
  val maskModule = Module(new MaskModule(size, numRead = LoadPipelineWidth+1, numWrite = LoadPipelineWidth))
  val coredataModule = Module(new CoredataModule(size, numRead = LoadPipelineWidth+1, numWrite = LoadPipelineWidth+1))

  // read data
  // read port 0 -> wbNumRead-1
  (0 until wbNumRead).map(i => {
    paddrModule.io.raddr(i) := io.wb.raddr(i)
    maskModule.io.raddr(i) := io.wb.raddr(i)
    coredataModule.io.raddr(i) := io.wb.raddr(i)

    io.wb.rdata(i).paddr := paddrModule.io.rdata(i)
    io.wb.rdata(i).mask := maskModule.io.rdata(i)
    io.wb.rdata(i).data := coredataModule.io.rdata(i)
    io.wb.rdata(i).fwdMask := DontCare
  })

  // read port wbNumRead
  paddrModule.io.raddr(wbNumRead) := io.uncache.raddr
  maskModule.io.raddr(wbNumRead) := io.uncache.raddr
  coredataModule.io.raddr(wbNumRead) := io.uncache.raddr

  io.uncache.rdata.paddr := paddrModule.io.rdata(wbNumRead)
  io.uncache.rdata.mask := maskModule.io.rdata(wbNumRead)
  io.uncache.rdata.data := coredataModule.io.rdata(wbNumRead)
  io.uncache.rdata.fwdMask := DontCare

  // write data
  // write port 0 -> wbNumWrite-1
  (0 until wbNumWrite).map(i => {
    paddrModule.io.wen(i) := false.B
    maskModule.io.wen(i) := false.B
    coredataModule.io.wen(i) := false.B
    coredataModule.io.fwdMaskWen(i) := false.B
    coredataModule.io.paddrWen(i) := false.B

    paddrModule.io.waddr(i) := io.wb.waddr(i)
    maskModule.io.waddr(i) := io.wb.waddr(i)
    coredataModule.io.waddr(i) := io.wb.waddr(i)

    paddrModule.io.wdata(i) := io.wb.wdata(i).paddr
    maskModule.io.wdata(i) := io.wb.wdata(i).mask
    coredataModule.io.wdata(i) := io.wb.wdata(i).data
    coredataModule.io.fwdMaskWdata(i) := io.wb.wdata(i).fwdMask.asUInt
    coredataModule.io.paddrWdata(i) := io.wb.wdata(i).paddr

    when(io.wb.wen(i)){
      paddrModule.io.wen(i) := true.B
      maskModule.io.wen(i) := true.B
      coredataModule.io.wen(i) := true.B
      coredataModule.io.fwdMaskWen(i) := true.B
      coredataModule.io.paddrWen(i) := true.B
    }
  })

  // write port wbNumWrite
  // exceptionModule.io.wen(wbNumWrite) := false.B
  coredataModule.io.wen(wbNumWrite) := io.uncache.wen
  coredataModule.io.fwdMaskWen(wbNumWrite) := false.B
  coredataModule.io.paddrWen(wbNumWrite) := false.B

  coredataModule.io.waddr(wbNumWrite) := io.uncache.waddr

  coredataModule.io.fwdMaskWdata(wbNumWrite) := DontCare
  coredataModule.io.paddrWdata(wbNumWrite) := DontCare
  coredataModule.io.wdata(wbNumWrite) := io.uncache.wdata

  // st-ld mem access violation check, gen violationMask
  (0 until StorePipelineWidth).map(i => {
    paddrModule.io.violationMdata(i) := io.violation(i).paddr
    maskModule.io.violationMdata(i) := io.violation(i).mask
    io.violation(i).violationMask := (paddrModule.io.violationMmask(i).asUInt & maskModule.io.violationMmask(i).asUInt).asBools
  })

  // ld-ld mem access violation check, gen violationMask (cam match mask)
  (0 until LoadPipelineWidth).map(i => {
    paddrModule.io.releaseMdata(i) := io.release_violation(i).paddr
    io.release_violation(i).match_mask := paddrModule.io.releaseMmask(i)
  })

  // refill missed load
  def mergeRefillData(refill: UInt, fwd: UInt, fwdMask: UInt): UInt = {
    val res = Wire(Vec(8, UInt(8.W)))
    (0 until 8).foreach(i => {
      res(i) := Mux(fwdMask(i), fwd(8 * (i + 1) - 1, 8 * i), refill(8 * (i + 1) - 1, 8 * i))
    })
    res.asUInt
  }

  // gen paddr match mask
  paddrModule.io.refillMdata := io.refill.paddr
  (0 until size).map(i => {
    io.refill.matchMask := paddrModule.io.refillMmask
    // io.refill.matchMask(i) := get_block_addr(data(i).paddr) === get_block_addr(io.refill.paddr)
  })

  // refill data according to matchMask, refillMask and refill.valid
  coredataModule.io.refillData := io.refill.data
  (0 until size).map(i => {
    coredataModule.io.mwmask(i) := io.refill.valid && io.refill.matchMask(i) && io.refill.refillMask(i)
  })

  // debug data read
  io.debug := DontCare
}
