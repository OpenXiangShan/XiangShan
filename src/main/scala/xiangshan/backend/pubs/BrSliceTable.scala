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

class BrSliceEntry(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val pc_br = UInt(VAddrBits.W)
}

class BrSliceTable(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val read = Vec(RenameWidth, new Bundle {
      val address = Input(UInt(VAddrBits.W))
      val data = Output(new BrSliceEntry)
    })
    val write = Vec(2*RenameWidth, new Bundle() {
      val enable = Input(Bool())
      val address = Input(UInt(VAddrBits.W))
      val data = Input(new BrSliceEntry)
    })
  })
  val brSliceTableSize = 4096

  val data = Reg(Vec(brSliceTableSize, new BrSliceEntry()))

  def address_hash(address: UInt) = XORFold(address, log2Up(brSliceTableSize))
  io.read.foreach(r => r.data := data(address_hash(r.address)))

  io.write.foreach(w => when (w.enable) { data(address_hash(w.address)) := w.data })
  when (reset.asBool()) {
    data.map(_.valid := false.B)
  }
}
