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

package xiangshan.backend.regcache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.BackendParams

class RCReadPort(dataWidth: Int, addrWidth: Int) extends Bundle {
  val ren  = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
  val data = Output(UInt(dataWidth.W))
}

class RCWritePort(dataWidth: Int, addrWidth: Int, tagWidth: Int, debugEn: Boolean) extends Bundle {
  val wen  = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
  val data = Input(UInt(dataWidth.W))
  val tag  = OptionWrapper(debugEn, Input(UInt(tagWidth.W)))
}

class RegCacheDataModule
(
  name: String,
  numEntries: Int,
  numReadPorts: Int,
  numWritePorts: Int,
  dataWidth: Int,
  addrWidth: Int,
  tagWidth: Int,
)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val readPorts = Vec(numReadPorts, new RCReadPort(dataWidth, addrWidth))
    val writePorts = Vec(numWritePorts, new RCWritePort(dataWidth, addrWidth, tagWidth, backendParams.debugEn))
    val validInfo = Vec(numEntries, Output(Bool()))
  })

  println(s"[RegCache] $name: size: $numEntries, read: $numReadPorts, write: $numWritePorts")

  val v   = RegInit(VecInit(Seq.fill(numEntries)(false.B)))
  val mem = Reg(Vec(numEntries, UInt(dataWidth.W)))
  val tag = OptionWrapper(backendParams.debugEn, Reg(Vec(numEntries, UInt(tagWidth.W))))

  for ((r, i) <- io.readPorts.zipWithIndex) {
    r.data := mem(r.addr)
    when (r.ren) {
      assert(v(r.addr), s"$name readPorts $i read a invalid entry")
    }
  }

  val writePorts = io.writePorts
  for (i <- writePorts.indices) {
    if (i < writePorts.size-1) {
      val hasSameWrite = writePorts.drop(i + 1).map(w => w.wen && w.addr === writePorts(i).addr && writePorts(i).wen).reduce(_ || _)
      assert(!hasSameWrite, s"$name has two or more writePorts writing the same entry")
    }
  }
  for (i <- mem.indices) {
    val wenOH = VecInit(io.writePorts.map(w => w.wen && w.addr === i.U))
    val wData = Mux1H(wenOH, io.writePorts.map(_.data))
    when (wenOH.asUInt.orR) {
      v(i)   := true.B
      mem(i) := wData
    }
    if (backendParams.debugEn) {
      val wTag = Mux1H(wenOH, io.writePorts.map(_.tag.get))
      when (wenOH.asUInt.orR) {
        tag.get(i) := wTag
      }
    }
  }

  io.validInfo := v
}
