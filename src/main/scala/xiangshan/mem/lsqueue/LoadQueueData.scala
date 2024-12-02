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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, MemoryOpConstants}
import xiangshan.mem._
import xiangshan.backend.rob.RobPtr
import utils._
import utility._

// Data module define
// These raw data modules are like SyncDataModuleTemplate, but support cam-like ops
abstract class LqRawDataModule[T <: Data] (gen: T, numEntries: Int, numRead: Int, numWrite: Int, numWBank: Int, numWDelay: Int, numCamPort: Int = 0)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val ren   = Input(Vec(numRead, Bool()))
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, gen))
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, gen))
    // violation cam: hit if addr is in the same cacheline
    val violationMdata = Input(Vec(numCamPort, gen)) // addr
    val violationMmask = Output(Vec(numCamPort, Vec(numEntries, Bool()))) // cam result mask
    // refill cam: hit if addr is in the same cacheline
    val releaseMdata = Input(Vec(numCamPort, gen))
    val releaseMmask = Output(Vec(numCamPort, Vec(numEntries, Bool())))  // cam result mask
    // release violation cam: hit if addr is in the same cacheline
    val releaseViolationMdata = Input(Vec(numCamPort, gen))
    val releaseViolationMmask = Output(Vec(numCamPort, Vec(numEntries, Bool()))) // cam result mask result
  })

  require(isPow2(numWBank), "write bank must be a power of two!")
  require(numWBank >= 2, "write bank must be greater than or equal to two!")
  require(numWDelay >= 1, "write delay must be greater than or equal to one!")
  require(numCamPort >= 0, "camport must be greater than or equal to zero!")
  require((numEntries % numWBank == 0), "numEntries must be divided by numWBank!")

  val numEntryPerBank = numEntries / numWBank

  val data = Reg(Vec(numEntries, gen))
  // read ports
  for (i <- 0 until numRead) {
    io.rdata(i) := RegEnable(data(io.raddr(i)), io.ren(i))
  }

  // write ports
  val writeAddrDec = io.waddr.map(a => UIntToOH(a))
  def selectBankMask(in: UInt, bank: Int): UInt = {
    in((bank + 1) * numEntryPerBank - 1, bank * numEntryPerBank)
  }
  for (bank <- 0 until numWBank) {
    // write ports
    // s0: write to bank level buffer
    val s0_bankWriteAddrDec = writeAddrDec.map(a => selectBankMask(a, bank))
    val s0_bankWriteEn = io.wen.zip(s0_bankWriteAddrDec).map(w => w._1 && w._2.orR)
    s0_bankWriteAddrDec.zipWithIndex.map(a =>
      a._1.suggestName("s0_bankWriteAddrDec" + bank + "_" + a._2)
    )
    s0_bankWriteEn.zipWithIndex.map(a =>
      a._1.suggestName("s0_bankWriteEn" + bank + "_" + a._2)
    )
    // sx: write data to entries
    val sx_bankWriteAddrDec_resp = (0 until numWrite).map(w => DelayNWithValid(s0_bankWriteAddrDec(w), io.wen(w), numWDelay - 1))
    val sx_bankWriteAddrDec = (0 until numWrite).map(w => sx_bankWriteAddrDec_resp(w)._2)
    val sx_bankWriteEn = s0_bankWriteEn.map(w => DelayN(w, numWDelay - 1))
     val sx_writeData_resp = (0 until numWrite).map(w => DelayNWithValid(io.wdata(w), io.wen(w), numWDelay - 1))
     val sx_writeData =  (0 until numWrite).map(w => sx_writeData_resp(w)._2)

    sx_bankWriteAddrDec.zipWithIndex.map(a =>
      a._1.suggestName("sx_bankWriteAddrDec" + bank + "_" + a._2)
    )
    sx_bankWriteEn.zipWithIndex.map(a =>
      a._1.suggestName("sx_bankWriteEn" + bank + "_" + a._2)
    )
    sx_writeData.zipWithIndex.map(a =>
      a._1.suggestName("sx_writeData" + bank + "_" + a._2)
    )

    // entry write
    for (entry <- 0 until numEntryPerBank) {
      // write ports
      val sx_entryWriteEnVec = sx_bankWriteEn.zip(sx_bankWriteAddrDec).map(w => w._1 && w._2(entry))
      val sx_entryWriteEn = VecInit(sx_entryWriteEnVec).asUInt.orR
      val sx_entryWriteData = Mux1H(sx_entryWriteEnVec, sx_writeData)
      when (sx_entryWriteEn) {
        data(bank * numEntryPerBank + entry) := sx_entryWriteData
      }
      sx_entryWriteEnVec.zipWithIndex.map(a =>
        a._1.suggestName("sx_entryWriteEnVec" + bank + "_" + entry + "_" + a._2)
      )
      sx_entryWriteEn.suggestName("sx_entryWriteEn" + bank + "_" + entry)
      sx_entryWriteData.suggestName("sx_entryWriteData" + bank + "_" + entry)
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

// Load queue physical address module
class LqPAddrModule[T <: UInt](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numWrite: Int,
  numWBank: Int,
  numWDelay: Int = 1,
  numCamPort: Int = 1)(implicit p: Parameters) extends LqRawDataModule(gen, numEntries, numRead, numWrite, numWBank, numWDelay, numCamPort)
  with HasDCacheParameters
{
  // content addressed match
  // 128-bits aligned
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      io.violationMmask(i)(j) := io.violationMdata(i)(PAddrBits-1, DCacheVWordOffset) === data(j)(PAddrBits-1, DCacheVWordOffset)
    }
  }

  // content addressed match
  // cacheline aligned
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      io.releaseViolationMmask(i)(j) := io.releaseViolationMdata(i)(PAddrBits-1, DCacheLineOffset) === data(j)(PAddrBits-1, DCacheLineOffset)
    }
  }

  // content addressed match
  // cacheline aligned
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      io.releaseMmask(i)(j) := io.releaseMdata(i)(PAddrBits-1, DCacheLineOffset) === data(j)(PAddrBits-1, DCacheLineOffset)
    }
  }
}

// Load queue data module
class LqVAddrModule[T <: UInt](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numWrite: Int,
  numWBank: Int,
  numWDelay: Int = 1,
  numCamPort: Int = 1)(implicit p: Parameters) extends LqRawDataModule(gen, numEntries, numRead, numWrite, numWBank, numWDelay, numCamPort)
  with HasDCacheParameters
{
  // content addressed match
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      io.violationMmask(i)(j) := io.violationMdata(i)(VAddrBits-1, DCacheVWordOffset) === data(j)(VAddrBits-1, DCacheVWordOffset)
    }
  }

  // content addressed match
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      io.releaseMmask(i)(j) := io.releaseMdata(i)(VAddrBits-1, DCacheLineOffset) === data(j)(VAddrBits-1, DCacheLineOffset)
    }
  }
}

// Load queue mask module
class LqMaskModule[T <: UInt](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numWrite: Int,
  numWBank: Int,
  numWDelay: Int = 1,
  numCamPort: Int = 1)(implicit p: Parameters) extends LqRawDataModule(gen, numEntries, numRead, numWrite, numWBank, numWDelay, numCamPort)
  with HasDCacheParameters
{
  // content addressed match
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      io.violationMmask(i)(j) := (io.violationMdata(i) & data(j)).orR
    }
  }
  // content addressed match
  // cacheline aligned
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      io.releaseViolationMmask(i)(j) := (io.releaseViolationMdata(i) & data(j)).orR
    }
  }

  // content addressed match
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      io.releaseMmask(i)(j) := (io.releaseMdata(i) & data(j)).orR
    }
  }
}

class LqVAddrSplitModule[T <: UInt](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numWrite: Int,
  numWBank: Int,
  numWDelay: Int = 1,
  numCamPort: Int = 1
  )(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
{
  val io = IO(new Bundle() {
    val clockGate_low = Input(Clock())
    val clockGate_high = Input(Clock())
    val ren   = Input(Vec(numRead, Bool()))
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, gen))
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, gen))
  })
  
  def clkGateEntrySize = 32
  val vaddrModule_low = Module(new LqVAddrModule(
    gen = UInt(VAddrBits.W),
    numEntries = clkGateEntrySize,
    numRead = numRead,
    numWrite = numWrite,
    numWBank = numWBank,
    numWDelay = numWDelay,
    numCamPort = numCamPort))
  vaddrModule_low.io := DontCare
  val vaddrModule_high = Module(new LqVAddrModule(
    gen = UInt(VAddrBits.W),
    numEntries = numEntries - clkGateEntrySize,
    numRead = numRead,
    numWrite = numWrite,
    numWBank = numWBank,
    numWDelay = numWDelay,
    numCamPort = numCamPort))
  vaddrModule_high.io := DontCare

  vaddrModule_low.clock := io.clockGate_low
  vaddrModule_high.clock := io.clockGate_high
 for(i <- 0 until numWrite) {
    when(io.waddr(i) < clkGateEntrySize.U) {
      vaddrModule_low.io.wen(i) := io.wen(i)
      vaddrModule_low.io.waddr(i) := io.waddr(i)
      vaddrModule_low.io.wdata(i) := io.wdata(i)
      vaddrModule_high.io.wen(i) := 0.U
    }.otherwise {
      vaddrModule_high.io.wen(i) := io.wen(i)
      vaddrModule_high.io.waddr(i) := io.waddr(i) - clkGateEntrySize.U
      vaddrModule_high.io.wdata(i) := io.wdata(i)
      vaddrModule_low.io.wen(i) := 0.U
    }
 }

 val raddr = (0 until numRead).map(i => {RegEnable(io.raddr(i), io.ren(i))})
 for(i <- 0 until numRead) {
    when(io.raddr(i) < clkGateEntrySize.U) {
      vaddrModule_low.io.ren(i) := io.ren(i)
      vaddrModule_low.io.raddr(i) := io.raddr(i)
    }.otherwise {
      vaddrModule_high.io.ren(i) := io.ren(i)
      vaddrModule_high.io.raddr(i) := io.raddr(i) - clkGateEntrySize.U
    }
  }
  for(i <- 0 until numRead) {
    when(raddr(i) < clkGateEntrySize.U) {
      io.rdata(i) := vaddrModule_low.io.rdata(i)
    }.otherwise {
      io.rdata(i) := vaddrModule_high.io.rdata(i)
    }
  }
 }

class LqPAddrSplitModule[T <: UInt](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numWrite: Int,
  numWBank: Int,
  numWDelay: Int = 1,
  numCamPort: Int = 1
  )(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
{
  val io = IO(new Bundle() {
    val clockGate_low = Input(Clock())
    val clockGate_high = Input(Clock())
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, gen))
    // violation cam: hit if addr is in the same cacheline
    val violationMdata = Input(Vec(numCamPort, gen)) // addr
    val violationMmask = Output(Vec(numCamPort, Vec(numEntries, Bool()))) // cam result mask
    // refill cam: hit if addr is in the same cacheline
    val releaseMdata = Input(Vec(numCamPort, gen))
    val releaseMmask = Output(Vec(numCamPort, Vec(numEntries, Bool())))  // cam result mask
    // release violation cam: hit if addr is in the same cacheline
    val releaseViolationMdata = Input(Vec(numCamPort, gen))
    val releaseViolationMmask = Output(Vec(numCamPort, Vec(numEntries, Bool()))) // cam result mask result
  })
  
  def clkGateEntrySize = 40
  val paddrModule_low = Module(new LqPAddrModule(
    gen = gen,
    numEntries = clkGateEntrySize,
    numRead = numRead,
    numWrite = numWrite,
    numWBank = numWBank,
    numWDelay = numWDelay,
    numCamPort = numCamPort))
  paddrModule_low.io := DontCare
  val paddrModule_high = Module(new LqPAddrModule(
    gen = gen,
    numEntries = numEntries - clkGateEntrySize,
    numRead = numRead,
    numWrite = numWrite,
    numWBank = numWBank,
    numWDelay = numWDelay,
    numCamPort = numCamPort))
  paddrModule_high.io := DontCare
  
  paddrModule_low.clock := io.clockGate_low
  paddrModule_high.clock := io.clockGate_high
 for(i <- 0 until numWrite) {
    when(io.waddr(i) < clkGateEntrySize.U) {
      paddrModule_low.io.wen(i) := io.wen(i)
      paddrModule_low.io.waddr(i) := io.waddr(i)
      paddrModule_low.io.wdata(i) := io.wdata(i)
      paddrModule_high.io.wen(i) := 0.U
    }.otherwise {
      paddrModule_high.io.wen(i) := io.wen(i)
      paddrModule_high.io.waddr(i) := io.waddr(i) - clkGateEntrySize.U
      paddrModule_high.io.wdata(i) := io.wdata(i)
      paddrModule_low.io.wen(i) := 0.U
    }
 }

 for(i <- 0 until numCamPort) {
  paddrModule_low.io.violationMdata(i) := io.violationMdata(i)
  paddrModule_high.io.violationMdata(i) := io.violationMdata(i)

  paddrModule_low.io.releaseMdata(i) := io.releaseMdata(i)
  paddrModule_high.io.releaseMdata(i) := io.releaseMdata(i)

  paddrModule_low.io.releaseViolationMdata(i) := io.releaseViolationMdata(i)
  paddrModule_high.io.releaseViolationMdata(i) := io.releaseViolationMdata(i)
 }

 for(i <- 0 until numCamPort) {
  for(j <- 0 until clkGateEntrySize) {
    io.violationMmask(i)(j) := paddrModule_low.io.violationMmask(i)(j)
  }
  for(j <- clkGateEntrySize until numEntries) {
    io.violationMmask(i)(j) := paddrModule_high.io.violationMmask(i)(j - clkGateEntrySize)
  }
 }
for(i <- 0 until numCamPort) {
  for(j <- 0 until clkGateEntrySize) {
    io.releaseViolationMmask(i)(j) := paddrModule_low.io.releaseViolationMmask(i)(j)
  }
  for(j <- clkGateEntrySize until numEntries) {
    io.releaseViolationMmask(i)(j) := paddrModule_high.io.releaseViolationMmask(i)(j - clkGateEntrySize)
  }
 }

for(i <- 0 until numCamPort) {
  for(j <- 0 until clkGateEntrySize) {
    io.releaseMmask(i)(j) := paddrModule_low.io.releaseMmask(i)(j)
  }
  for(j <- clkGateEntrySize until numEntries) {
    io.releaseMmask(i)(j) := paddrModule_high.io.releaseMmask(i)(j - clkGateEntrySize)
  }
 }
}

class LqMaskSplitModule[T <: UInt](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numWrite: Int,
  numWBank: Int,
  numWDelay: Int = 1,
  numCamPort: Int = 1
  )(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
{
  val io = IO(new Bundle() {
    val clockGate_low = Input(Clock())
    val clockGate_high = Input(Clock())
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, gen))
    // violation cam: hit if addr is in the same cacheline
    val violationMdata = Input(Vec(numCamPort, gen)) // addr
    val violationMmask = Output(Vec(numCamPort, Vec(numEntries, Bool()))) // cam result mask
  })
  
  def clkGateEntrySize = 40
  val maskModule_low = Module(new LqMaskModule(
    gen = gen,
    numEntries = clkGateEntrySize,
    numRead = numRead,
    numWrite = numWrite,
    numWBank = numWBank,
    numWDelay = numWDelay,
    numCamPort = numCamPort))
  maskModule_low.io := DontCare
  val maskModule_high = Module(new LqMaskModule(
    gen = gen,
    numEntries = numEntries - clkGateEntrySize,
    numRead = numRead,
    numWrite = numWrite,
    numWBank = numWBank,
    numWDelay = numWDelay,
    numCamPort = numCamPort))
  maskModule_high.io := DontCare
  
  maskModule_low.clock := io.clockGate_low
  maskModule_high.clock := io.clockGate_high
 for(i <- 0 until numWrite) {
    when(io.waddr(i) < clkGateEntrySize.U) {
      maskModule_low.io.wen(i) := io.wen(i)
      maskModule_low.io.waddr(i) := io.waddr(i)
      maskModule_low.io.wdata(i) := io.wdata(i)
      maskModule_high.io.wen(i) := 0.U
    }.otherwise {
      maskModule_high.io.wen(i) := io.wen(i)
      maskModule_high.io.waddr(i) := io.waddr(i) - clkGateEntrySize.U
      maskModule_high.io.wdata(i) := io.wdata(i)
      maskModule_low.io.wen(i) := 0.U
    }
 }

 for(i <- 0 until numCamPort) {
  maskModule_low.io.violationMdata(i) := io.violationMdata(i)
  maskModule_high.io.violationMdata(i) := io.violationMdata(i)
 }

 for(i <- 0 until numCamPort) {
  for(j <- 0 until clkGateEntrySize) {
    io.violationMmask(i)(j) := maskModule_low.io.violationMmask(i)(j)
  }
  for(j <- clkGateEntrySize until numEntries) {
    io.violationMmask(i)(j) := maskModule_high.io.violationMmask(i)(j - clkGateEntrySize)
  }
 }
}
