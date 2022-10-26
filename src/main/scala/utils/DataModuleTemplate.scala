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

package utils

import chisel3._
import chisel3.util._

class RawDataModuleTemplate[T <: Data](gen: T, numEntries: Int, numRead: Int, numWrite: Int, isSync: Boolean) extends Module {
  val io = IO(new Bundle {
    val rvec  = Vec(numRead,  Input(UInt(numEntries.W)))
    val rdata = Vec(numRead,  Output(gen))
    val wen   = Vec(numWrite, Input(Bool()))
    val wvec  = Vec(numWrite, Input(UInt(numEntries.W)))
    val wdata = Vec(numWrite, Input(gen))
  })

  val data = Reg(Vec(numEntries, gen))

  // read ports
  val rvec = if (isSync) RegNext(io.rvec) else io.rvec
  for (i <- 0 until numRead) {
    assert(PopCount(rvec(i)) <= 1.U)
    io.rdata(i) := Mux1H(rvec(i), data)
  }

  // write ports
  for (i <- 0 until numEntries) {
    val w = VecInit((0 until numWrite).map(j => io.wen(j) && io.wvec(j)(i)))
    assert(PopCount(w) <= 1.U, s"RawDatModule multi-write index:$i")
    when (w.asUInt.orR) {
      data(i) := Mux1H(w, io.wdata)
    }
  }
}


class SyncRawDataModuleTemplate[T <: Data](gen: T, numEntries: Int, numRead: Int, numWrite: Int) extends RawDataModuleTemplate(gen, numEntries, numRead, numWrite, true)
class AsyncRawDataModuleTemplate[T <: Data](gen: T, numEntries: Int, numRead: Int, numWrite: Int) extends RawDataModuleTemplate(gen, numEntries, numRead, numWrite, false)

class DataModuleTemplate[T <: Data](gen: T, numEntries: Int, numRead: Int, numWrite: Int, isSync: Boolean) extends Module {
  val io = IO(new Bundle {
    val raddr = Vec(numRead,  Input(UInt(log2Up(numEntries).W)))
    val rdata = Vec(numRead,  Output(gen))
    val wen   = Vec(numWrite, Input(Bool()))
    val waddr = Vec(numWrite, Input(UInt(log2Up(numEntries).W)))
    val wdata = Vec(numWrite, Input(gen))
  })

  val data = Mem(numEntries, gen)

  // read ports
  val raddr = if (isSync) (RegNext(io.raddr)) else io.raddr
  for (i <- 0 until numRead) {
    io.rdata(i) := data(raddr(i))
  }

  // below is the write ports (with priorities)
  for (i <- 0 until numWrite) {
    when (io.wen(i)) {
      data(io.waddr(i)) := io.wdata(i)
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

class SyncDataModuleTemplate[T <: Data](gen: T, numEntries: Int, numRead: Int, numWrite: Int) extends DataModuleTemplate(gen, numEntries, numRead, numWrite, true)
class AsyncDataModuleTemplate[T <: Data](gen: T, numEntries: Int, numRead: Int, numWrite: Int) extends DataModuleTemplate(gen, numEntries, numRead, numWrite, false)

class Folded1WDataModuleTemplate[T <: Data](gen: T, numEntries: Int, numRead: Int,
  isSync: Boolean, width: Int, hasResetEn: Boolean = true) extends Module {
  val io = IO(new Bundle {
    val raddr = Vec(numRead,  Input(UInt(log2Up(numEntries).W)))
    val rdata = Vec(numRead,  Output(gen))
    val wen   = Input(Bool())
    val waddr = Input(UInt(log2Up(numEntries).W))
    val wdata = Input(gen)
    val resetEn = if (hasResetEn) Some(Input(Bool())) else None
  })

  require(width > 0 && isPow2(width))
  require(numEntries % width == 0)

  val nRows = numEntries / width

  val data = Mem(nRows, Vec(width, gen))

  val doing_reset = RegInit(true.B)
  if (hasResetEn) {
    io.resetEn.map(en => when (en) { doing_reset := true.B })
  }
  val resetRow = RegInit(0.U(log2Ceil(nRows).W))
  resetRow := resetRow + doing_reset
  when (resetRow === (nRows-1).U) { doing_reset := false.B }

  val raddr = if (isSync) RegNext(io.raddr) else io.raddr

  for (i <- 0 until numRead) {
    val addr = raddr(i) >> log2Ceil(width)
    val idx = raddr(i)(log2Ceil(width)-1, 0)
    io.rdata(i) := Mux(doing_reset, 0.U.asTypeOf(gen), data(addr)(idx))
  }

  val waddr = io.waddr >> log2Ceil(width)
  val wmask = UIntToOH(io.waddr(log2Ceil(width)-1, 0))
  val wdata = VecInit(Seq.fill(width)(io.wdata))

  when(doing_reset) {
    data.write(resetRow, 0.U.asTypeOf(Vec(width, gen)))
  }.elsewhen(io.wen) {
    data.write(waddr, wdata, wmask.asBools)
  }
}