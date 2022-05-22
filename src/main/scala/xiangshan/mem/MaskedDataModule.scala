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
import xiangshan.cache._

class MaskedSyncDataModuleTemplate[T <: Data](
  gen: T, numEntries: Int, numRead: Int, numWrite: Int, numMRead: Int = 0, numMWrite: Int = 0
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

  // write and masked write
  for (j <- 0 until numEntries) {
    val write_wen = (0 until numWrite).map(i => io.wen(i) && io.waddr(i) === j.U)
    val mwrite_wen = (0 until numMWrite).map(i => io.mwmask(i)(j))
    when (VecInit(mwrite_wen).asUInt.orR) {
      data(j) := Mux1H(mwrite_wen, io.mwdata)
    }.elsewhen (VecInit(write_wen).asUInt.orR) {
      data(j) := Mux1H(write_wen, io.wdata)
    }
  }

}
