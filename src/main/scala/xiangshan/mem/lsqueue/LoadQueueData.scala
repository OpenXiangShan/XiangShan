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

class LQDataEntryWoPaddr(implicit p: Parameters) extends XSBundle {
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
  val fwdMask = Vec(8, Bool())
}

class LQDataEntry(implicit p: Parameters) extends LQDataEntryWoPaddr {
  val paddr = UInt(PAddrBits.W)
}

// Data module define
// These data modules are like SyncDataModuleTemplate, but support cam-like ops

// load queue paddr module
// 
// It supports 2 cam sources:
// * st-ld violation addr cam 
// * data release addr cam 
class LQPaddrModule(numEntries: Int, numRead: Int, numWrite: Int, numWBanks: Int)(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle {
    // normal read/write ports
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, UInt((PAddrBits).W)))
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, UInt((PAddrBits).W)))
    // violation cam: hit if addr is in the same word
    val violationMdata = Input(Vec(StorePipelineWidth, UInt((PAddrBits).W))) // addr
    val violationMmask = Output(Vec(StorePipelineWidth, Vec(numEntries, Bool()))) // cam result mask
    // release cam: hit if addr is in the same cacheline
    val releaseMdata = Input(Vec(LoadPipelineWidth, UInt((PAddrBits).W)))
    val releaseMmask = Output(Vec(LoadPipelineWidth, Vec(numEntries, Bool())))
  })

  require(isPow2(numWBanks))
  require(numWBanks >= 2)

  val numEntryPerBank = numEntries / numWBanks

  val data = Reg(Vec(numEntries, UInt((PAddrBits).W)))

  // read ports
  for (i <- 0 until numRead) {
    io.rdata(i) := data(RegNext(io.raddr(i)))
  }

  // write ports
  val waddr_dec = io.waddr.map(a => UIntToOH(a))
  def selectBankMask(in: UInt, bank: Int): UInt = {
    in((bank + 1) * numEntryPerBank - 1, bank * numEntryPerBank)
  }
  for (bank <- 0 until numWBanks) {
    // write ports
    // s0: write to bank level buffer
    val s0_bank_waddr_dec = waddr_dec.map(a => selectBankMask(a, bank))
    val s0_bank_write_en = io.wen.zip(s0_bank_waddr_dec).map(w => w._1 && w._2.orR)
    s0_bank_waddr_dec.zipWithIndex.map(a =>
      a._1.suggestName("s0_bank_waddr_dec" + bank + "_" + a._2)
    )
    s0_bank_write_en.zipWithIndex.map(a =>
      a._1.suggestName("s0_bank_write_en" + bank + "_" + a._2)
    )
    // s1: write data to entries
    val s1_bank_waddr_dec = s0_bank_waddr_dec.zip(s0_bank_write_en).map(w => RegEnable(w._1, w._2))
    val s1_bank_wen = RegNext(VecInit(s0_bank_write_en))
    val s1_wdata = io.wdata.zip(s0_bank_write_en).map(w => RegEnable(w._1, w._2))
    s1_bank_waddr_dec.zipWithIndex.map(a =>
      a._1.suggestName("s1_bank_waddr_dec" + bank + "_" + a._2)
    )
    s1_bank_wen.zipWithIndex.map(a =>
      a._1.suggestName("s1_bank_wen" + bank + "_" + a._2)
    )
    s1_wdata.zipWithIndex.map(a =>
      a._1.suggestName("s1_wdata" + bank + "_" + a._2)
    )

    // entry write
    for (entry <- 0 until numEntryPerBank) {
      // write ports
      val s1_entry_write_en_vec = s1_bank_wen.zip(s1_bank_waddr_dec).map(w => w._1 && w._2(entry))
      val s1_entry_write_en = VecInit(s1_entry_write_en_vec).asUInt.orR
      val s1_entry_write_data = Mux1H(s1_entry_write_en_vec, s1_wdata)
      when (s1_entry_write_en) {
        data(bank * numEntryPerBank + entry) := s1_entry_write_data
      }
      s1_entry_write_en_vec.zipWithIndex.map(a =>
        a._1.suggestName("s1_entry_write_en_vec" + bank + "_" + entry + "_" + a._2)
      )
      s1_entry_write_en.suggestName("s1_entry_write_en" + bank + "_" + entry)
      s1_entry_write_data.suggestName("s1_entry_write_data" + bank + "_" + entry)
    }
  }

  // content addressed match
  for (i <- 0 until StorePipelineWidth) {
    for (j <- 0 until numEntries) {
      io.violationMmask(i)(j) := io.violationMdata(i)(PAddrBits-1, DCacheWordOffset) === data(j)(PAddrBits-1, DCacheWordOffset)
    }
  }
  for (i <- 0 until LoadPipelineWidth) {
    for (j <- 0 until numEntries) {
      io.releaseMmask(i)(j) := io.releaseMdata(i)(PAddrBits-1, DCacheLineOffset) === data(j)(PAddrBits-1, DCacheLineOffset)
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

// load queue load mask module
class LQMaskModule(numEntries: Int, numRead: Int, numWrite: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, UInt(8.W)))
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, UInt(8.W)))
    // st-ld violation check wmask compare 
    val violationMdata = Input(Vec(StorePipelineWidth, UInt(8.W))) // input 8-bit wmask
    val violationMmask = Output(Vec(StorePipelineWidth, Vec(numEntries, Bool()))) // output wmask overlap vector
  })

  val data = Reg(Vec(numEntries, UInt(8.W)))

  // read ports
  for (i <- 0 until numRead) {
    io.rdata(i) := data(RegNext(io.raddr(i)))
  }

  // write ports
  val waddr_dec = io.waddr.map(a => UIntToOH(a))
  for (j <- 0 until numEntries) {
    val write_wen = io.wen.zip(waddr_dec).map(w => w._1 && w._2(j))
    when (VecInit(write_wen).asUInt.orR) {
      data(j) := Mux1H(write_wen, io.wdata)
    }
  }

  // st-ld violation check wmask compare 
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

// SQDataModule is a wrapper of 8 bit MaskedSyncDataModuleTemplates
// NOTE: SQDataModule is not used now
// 
// It also contains:
// * fwdMask, which is used to merge refill data and forwarded data
// * word index extracted from paddr, which is used to select data from refill data (a cacheline)
class LQDataModule(numEntries: Int, numRead: Int, numWrite: Int)(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle {
    // sync read
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, UInt(XLEN.W)))
    
    // address indexed write
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, UInt(XLEN.W)))
    // forward mask needs to be recorded to merge data
    val fwdMaskWdata = Input(Vec(numWrite, UInt(8.W)))
    // refillOffBits - wordOffBits bits in paddr need to be stored in LQDataModule for refilling
    val paddrWdata = Input(Vec(numWrite, UInt((PAddrBits).W)))
    
    // masked write
    val mwmask = Input(Vec(numEntries, Bool()))
    val refillData = Input(UInt(l1BusDataWidth.W))
  })

  val data8 = Seq.fill(8)(Module(new MaskedBankedSyncDataModuleTemplate(
    UInt(8.W), numEntries, numRead, numWrite, numMWrite = refillWords, numWBanks = LoadQueueNWriteBanks
  )))
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
    // TODO: optimize that
    when (io.wen(i)) {
      fwdMask(io.waddr(i)) := io.fwdMaskWdata(i)
    }
    when (io.wen(i)) {
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

// LoadQueueDataWrapper wraps:
// * load queue paddrModule
// * load queue maskModule
// and their interconnect
class LoadQueueDataWrapper(size: Int, wbNumWrite: Int)(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val paddr = new Bundle() {
      val wen = Vec(wbNumWrite, Input(Bool()))
      val waddr = Input(Vec(wbNumWrite, UInt(log2Up(size).W)))
      val wdata = Input(Vec(wbNumWrite, UInt(PAddrBits.W)))
    }
    val wb = new Bundle() {
      val wen = Vec(wbNumWrite, Input(Bool()))
      val waddr = Input(Vec(wbNumWrite, UInt(log2Up(size).W)))
      val wdata = Input(Vec(wbNumWrite, UInt(8.W)))
    }
    val uncache = new Bundle() {
      val raddr = Input(UInt(log2Up(size).W))
      val rdata = Output(new LQDataEntry)
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

    def wbWrite(channel: Int, waddr: UInt, wdata: UInt): Unit = {
      require(channel < wbNumWrite && wbNumWrite >= 0)
      // need extra "this.wb(channel).wen := true.B"
      this.wb.waddr(channel) := waddr
      this.wb.wdata(channel) := wdata
    }
  })

  // data module
  val paddrModule = Module(new LQPaddrModule(size, numRead = 1, numWrite = LoadPipelineWidth, numWBanks = LoadQueueNWriteBanks))
  val maskModule = Module(new LQMaskModule(size, numRead = 1, numWrite = LoadPipelineWidth))

  // read port for uncache
  paddrModule.io.raddr(0) := io.uncache.raddr
  maskModule.io.raddr(0) := io.uncache.raddr

  io.uncache.rdata.paddr := paddrModule.io.rdata(0)
  io.uncache.rdata.mask := maskModule.io.rdata(0)
  io.uncache.rdata.data := DontCare
  io.uncache.rdata.fwdMask := DontCare

  // write mask and paddr
  // write port 0 -> wbNumWrite-1
  (0 until wbNumWrite).map(i => {
    paddrModule.io.wen(i) := false.B
    maskModule.io.wen(i) := false.B

    maskModule.io.waddr(i) := io.wb.waddr(i)
    maskModule.io.wdata(i) := io.wb.wdata(i)

    when(io.wb.wen(i)){
      maskModule.io.wen(i) := true.B
    }

    paddrModule.io.wen(i) := io.paddr.wen(i)
    paddrModule.io.waddr(i) := io.paddr.waddr(i)
    paddrModule.io.wdata(i) := io.paddr.wdata(i)
  })

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

  // debug data read
  io.debug := DontCare
}
