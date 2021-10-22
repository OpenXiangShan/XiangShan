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

package xiangshan.backend.pubs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._

class BrSliceEntry(tagWidth: Int)(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val tag = UInt(tagWidth.W)
  val pc_br = UInt(VAddrBits.W)
  override def cloneType: BrSliceEntry.this.type =
    new BrSliceEntry(tagWidth).asInstanceOf[this.type]
}

class BrSliceTable(implicit p: Parameters) extends XSModule {
  val brSliceTableSet = 128
  val brSliceTableWay = 8

  val setWidth = log2Up(brSliceTableSet)
  val tagWidth = 8
  def pcSetIdx(pc: UInt): UInt = pc(setWidth - 1, 0)
  def pcTag(pc: UInt): UInt = XORFold(pc(VAddrBits - 1, setWidth), tagWidth)

  val io = IO(new Bundle {
    val read = Vec(RenameWidth, new Bundle {
      val enable = Input(Bool())
      val address = Input(UInt(VAddrBits.W))
      val data = Output(new BrSliceEntry(tagWidth))
    })
    val write = Vec(2*RenameWidth, new Bundle() {
      val enable = Input(Bool())
      val address = Input(UInt(VAddrBits.W))
      val data = Input(new BrSliceEntry(tagWidth))
    })
  })

  val data = Reg(Vec(brSliceTableSet, Vec(brSliceTableWay, new BrSliceEntry(tagWidth))))
  val lru = ReplacementPolicy.fromString("setlru", brSliceTableWay, brSliceTableSet)

  for (i <- 0 until RenameWidth) {
    val row = data(pcSetIdx(io.read(i).address))
    val hitVec = VecInit(row.map(e => e.valid && e.tag === pcTag(io.read(i).address)))
    val hit = hitVec.asUInt.orR
    val entry = row(OHToUInt(hitVec))
    io.read(i).data := entry
    io.read(i).data.valid := entry.valid && hit
    when (hit && io.read(i).enable) {
      lru.access(pcSetIdx(io.read(i).address), OHToUInt(hitVec))
    }
  }

  for (i <- 0 until 2*RenameWidth) {
    val row = data(pcSetIdx(io.write(i).address))
    val hitVec = VecInit(row.map(e => e.valid && e.tag === pcTag(io.write(i).address)))
    val hit = hitVec.asUInt.orR
    val allocate = lru.way(pcSetIdx(io.write(i).address))
    when (hit) {
      row(OHToUInt(hitVec)) := io.write(i).data
    }.otherwise {
      row(allocate) := io.write(i).data
    }
  }
  when (reset.asBool()) {
    data.map(_.map(_.valid := false.B))
  }
}
