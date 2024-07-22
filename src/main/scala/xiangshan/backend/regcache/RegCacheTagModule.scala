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
import freechips.rocketchip.util.SeqBoolBitwiseOps

class RCTagTableReadPort(addrWidth: Int, tagWidth: Int)(implicit p: Parameters) extends XSBundle {
  val tag   = Input(UInt(tagWidth.W))
  val valid = Output(Bool())
  val addr  = Output(UInt(addrWidth.W))
}

class RCTagTableWritePort(addrWidth: Int, tagWidth: Int)(implicit p: Parameters) extends XSBundle {
  val wen   = Input(Bool())
  val addr  = Input(UInt(addrWidth.W))
  val tag   = Input(UInt(tagWidth.W))
  val loadDependency = Vec(LoadPipelineWidth, Input(UInt(LoadDependencyWidth.W)))
}

class RegCacheTagModule
(
  name: String,
  numEntries: Int,
  numReadPorts: Int,
  numWritePorts: Int,
  addrWidth: Int,
  tagWidth: Int,
)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val readPorts = Vec(numReadPorts, new RCTagTableReadPort(addrWidth, tagWidth))
    val writePorts = Vec(numWritePorts, new RCTagTableWritePort(addrWidth, tagWidth))

    val cancelVec = Vec(numEntries, Input(Bool()))

    val validVec = Vec(numEntries, Output(Bool()))
    val tagVec = Vec(numEntries, Output(UInt(tagWidth.W)))
    val loadDependencyVec = Vec(numEntries, Vec(LoadPipelineWidth, Output(UInt(LoadDependencyWidth.W))))
  })

  println(s"[RegCacheTagModule] $name: size: $numEntries, read: $numReadPorts, write: $numWritePorts")

  val v   = RegInit(VecInit(Seq.fill(numEntries)(false.B)))
  val tag = Reg(Vec(numEntries, UInt(tagWidth.W)))
  val loadDependency = Reg(Vec(numEntries, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))))

  for ((r, i) <- io.readPorts.zipWithIndex) {
    val matchOH = v.zip(tag).map(x => x._1 && x._2 === r.tag)
    r.valid := matchOH.orR
    r.addr  := OHToUInt(matchOH)
    assert(PopCount(matchOH) <= 1.U, s"$name readPorts $i has more than 1 matched entry")
  }

  val writePorts = io.writePorts
  for (i <- writePorts.indices) {
    if (i < writePorts.size-1) {
      val hasSameWrite = writePorts.drop(i + 1).map(w => w.wen && w.addr === writePorts(i).addr && writePorts(i).wen).reduce(_ || _)
      assert(!hasSameWrite, s"$name has two or more writePorts writing the same entry")
    }
  }

  for (i <- tag.indices) {
    val wenOH = VecInit(io.writePorts.map(w => w.wen && w.addr === i.U))
    val wTag = Mux1H(wenOH, io.writePorts.map(_.tag))
    val wLoadDependency = Mux1H(wenOH, io.writePorts.map(_.loadDependency))

    when (wenOH.asUInt.orR) {
      v(i)   := true.B
      tag(i) := wTag
      loadDependency(i) := wLoadDependency
    }
    .elsewhen (io.cancelVec(i)) {
      v(i)   := false.B
      loadDependency(i) := 0.U.asTypeOf(loadDependency(i))
    }
    .elsewhen (loadDependency(i).map(x => x.orR).reduce(_ || _)) {
      loadDependency(i) := loadDependency(i).map(l => l << 1)
    }
  }

  io.validVec := v
  io.tagVec := tag
  io.loadDependencyVec := loadDependency
}
