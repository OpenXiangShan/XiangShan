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

class RCAgeTimerReadPort(addrWidth: Int) extends Bundle {
  val ren  = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
}

class RCAgeTimerWritePort(addrWidth: Int) extends Bundle {
  val wen  = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
}

class RegCacheAgeTimer
(
  numEntries: Int,
  numReadPorts: Int,
  numWritePorts: Int,
  addrWidth: Int,
)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val readPorts = Vec(numReadPorts, new RCAgeTimerReadPort(addrWidth))
    val writePorts = Vec(numWritePorts, new RCAgeTimerWritePort(addrWidth))
    val validInfo = Vec(numEntries, Input(Bool()))
    val ageInfo = Vec(numEntries, Vec(numEntries, Output(Bool())))
  })

  val ageTimer = RegInit(VecInit(Seq.fill(numEntries)(3.U(2.W))))
  val ageTimerNext = Seq.fill(numEntries)(Wire(UInt(2.W)))

  val hasReadReq = (0 until numEntries).map{ i => 
    io.readPorts.map(r => r.ren && r.addr === i.U).reduce(_ || _)
  }
  val hasWriteReq = (0 until numEntries).map{ i => 
    io.writePorts.map(w => w.wen && w.addr === i.U).reduce(_ || _)
  }

  for ((atNext, i) <- ageTimerNext.zipWithIndex) {
    when(hasWriteReq(i)) {
      atNext := 0.U
    }.elsewhen(hasReadReq(i)) {
      atNext := ageTimer(i)
    }.elsewhen(ageTimer(i) === 3.U) {
      atNext := 3.U
    }.otherwise {
      atNext := ageTimer(i) + 1.U
    }
    ageTimer(i) := atNext
  }

  def age_cmp_func(row: Int, col: Int): Bool = {
    if (row == col)
      true.B
    else if (row < col) {
      val res = Wire(Bool())
      when (io.validInfo(row) && !io.validInfo(col)) {
        res := false.B
      }.elsewhen (!io.validInfo(row) && io.validInfo(col)) {
        res := true.B
      }.otherwise {
        res := ageTimerNext(row) >= ageTimerNext(col)
      }
      res
    }
    else
      !age_cmp_func(col, row)
  }

  for (i <- 0 until numEntries) {
    for (j <- 0 until numEntries) {
      io.ageInfo(i)(j) := age_cmp_func(i, j)
    }
  }
}
