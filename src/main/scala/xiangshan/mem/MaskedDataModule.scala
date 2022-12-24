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
import xiangshan._
import utils._
import utility._
import xiangshan.cache._

class MaskedSyncDataModuleTemplate[T <: Data](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numWrite: Int,
  numMRead: Int = 0,
  numMWrite: Int = 0
) extends Module {
  val io = IO(new Bundle {
    // address indexed sync read
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, gen))
    // masked sync read (1H)
    val mrmask = Input(Vec(numMRead, Vec(numEntries, Bool())))
    val mrdata = Output(Vec(numMRead, gen))
    // address indexed write
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, gen))
    // masked write
    val mwmask = Input(Vec(numMWrite, Vec(numEntries, Bool())))
    val mwdata = Input(Vec(numMWrite, gen))
  })

  val data = Reg(Vec(numEntries, gen))

  // read ports
  for (i <- 0 until numRead) {
    io.rdata(i) := data(RegNext(io.raddr(i)))
  }

  // masked read ports
  for (i <- 0 until numMRead) {
    io.mrdata(i) := Mux1H(RegNext(io.mrmask(i)), data)
  }

  // write ports (with priorities)
  for (i <- 0 until numWrite) {
    when (io.wen(i)) {
      data(io.waddr(i)) := io.wdata(i)
    }
  }

  // masked write
  for (j <- 0 until numEntries) {
    val wen = VecInit((0 until numMWrite).map(i => io.mwmask(i)(j))).asUInt.orR
    when (wen) {
      data(j) := VecInit((0 until numMWrite).map(i => {
        Mux(io.mwmask(i)(j), io.mwdata(i), 0.U).asUInt
      })).reduce(_ | _)
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

class MaskedBankedSyncDataModuleTemplate[T <: Data](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numWrite: Int,
  numMRead: Int = 0,
  numMWrite: Int = 0,
  numWBanks: Int = 2
) extends Module {
  val io = IO(new Bundle {
    // address indexed sync read
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, gen))
    // masked sync read (1H)
    val mrmask = Input(Vec(numMRead, Vec(numEntries, Bool())))
    val mrdata = Output(Vec(numMRead, gen))
    // address indexed write
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, gen))
    // masked write
    val mwmask = Input(Vec(numMWrite, Vec(numEntries, Bool())))
    val mwdata = Input(Vec(numMWrite, gen))
  })

  require(isPow2(numWBanks))
  require(numWBanks >= 2)

  val numEntryPerBank = numEntries / numWBanks

  val data = Reg(Vec(numEntries, gen))

  // read ports
  for (i <- 0 until numRead) {
    val raddr_dec = RegNext(UIntToOH(io.raddr(i)))
    io.rdata(i) := Mux1H(raddr_dec, data)
  }

  // masked read ports
  for (i <- 0 until numMRead) {
    io.mrdata(i) := Mux1H(RegNext(io.mrmask(i)), data)
  }

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
    // masked write ports
    // s0: write to bank level buffer
    val s0_bank_mwmask = io.mwmask.map(a => selectBankMask(a.asUInt, bank))
    val s0_bank_mwrite_en = s0_bank_mwmask.map(w => w.orR)
    s0_bank_mwmask.zipWithIndex.map(a =>
      a._1.suggestName("s0_bank_mwmask" + bank + "_" + a._2)
    )
    s0_bank_mwrite_en.zipWithIndex.map(a =>
      a._1.suggestName("s0_bank_mwrite_en" + bank + "_" + a._2)
    )
    // s1: write data to entries
    val s1_bank_mwmask = s0_bank_mwmask.map(a => RegNext(a))
    val s1_mwdata = io.mwdata.zip(s0_bank_mwrite_en).map(w => RegEnable(w._1, w._2))
    s1_bank_mwmask.zipWithIndex.map(a =>
      a._1.suggestName("s1_bank_mwmask" + bank + "_" + a._2)
    )
    s1_mwdata.zipWithIndex.map(a =>
      a._1.suggestName("s1_mwdata" + bank + "_" + a._2)
    )

    // entry write
    for (entry <- 0 until numEntryPerBank) {
      // write ports
      val s1_entry_write_en_vec = s1_bank_wen.zip(s1_bank_waddr_dec).map(w => w._1 && w._2(entry))
      val s1_entry_write_en = VecInit(s1_entry_write_en_vec).asUInt.orR
      val s1_entry_write_data = Mux1H(s1_entry_write_en_vec, s1_wdata)
      // masked write ports
      val s1_bank_mwrite_en_vec = s1_bank_mwmask.map(_(entry))
      val s1_bank_mwrite_en = VecInit(s1_bank_mwrite_en_vec).asUInt.orR
      val s1_bank_mwrite_data = Mux1H(s1_bank_mwrite_en_vec, s1_mwdata)
      when (s1_entry_write_en || s1_bank_mwrite_en) {
        data(bank * numEntryPerBank + entry) := Mux1H(
          Seq(s1_entry_write_en, s1_bank_mwrite_en), 
          Seq(s1_entry_write_data, s1_bank_mwrite_data)
        )
      }
      s1_entry_write_en_vec.zipWithIndex.map(a =>
        a._1.suggestName("s1_entry_write_en_vec" + bank + "_" + entry + "_" + a._2)
      )
      s1_bank_mwrite_en_vec.zipWithIndex.map(a =>
        a._1.suggestName("s1_bank_mwrite_en_vec" + bank + "_" + entry + "_" + a._2)
      )
      s1_entry_write_en.suggestName("s1_entry_write_en" + bank + "_" + entry)
      s1_entry_write_data.suggestName("s1_entry_write_data" + bank + "_" + entry)
      s1_bank_mwrite_en.suggestName("s1_bank_mwrite_en" + bank + "_" + entry)
      s1_bank_mwrite_data.suggestName("s1_bank_mwrite_data" + bank + "_" + entry)
    }
  }
}
